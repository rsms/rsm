// kernel-style memory allocator
// SPDX-License-Identifier: Apache-2.0

#include "rsmimpl.h"
#define ILIST_TEST_IMPL
#include "list.h"
#include "bits.h"
#include "mem.h"

#define PAGE_SIZE 4096u

// _SCRUB_BYTE defines a byte value that, upon allocating or freeing a region,
// memory is filled with (ie. memset(ptr,_SCRUB_BYTE,size).)
// This is useful for debugging memory management issues, like use after free, since
// memory managed by this allocator is not subject to host memory protection.
// Set to zero to disable scrubbing.
#define KMEM_ALLOC_SCRUB_BYTE 0xbb
#define KMEM_FREE_SCRUB_BYTE  0xaa

//———————————————————————————————————————————————————————————————————————————————————
// API

typedef struct kmem_ kmem_t;

void* nullable kmem_alloc(kmem_t*, usize size)
  __attribute__((malloc, alloc_size(2)));

void* nullable kmem_allocx(kmem_t*, usize* size_in_out, usize alignment) // returns actual size
  __attribute__((malloc, alloc_align(3)));

void* nullable kmem_alloc_aligned(kmem_t*, usize size, usize alignment)
  __attribute__((malloc, alloc_size(2), alloc_align(3)));

void kmem_free(kmem_t*, void* ptr);

// TODO
// void* nullable kmem_resize(void* oldptr, usize oldsize, usize newsize)
//   __attribute__((malloc, alloc_size(3)));

usize kmem_alloc_size(usize);


//———————————————————————————————————————————————————————————————————————————————————
// subheap allocator
//
// This implementation maintains multiple heaps (g_subheaps).
// There is initially just one heap, backed by BSS memory (g_init_mem).
// When no space is found for an allocation request in that heap, another
// heap is allocated and added to the subheaps list.
//


typedef struct {
  usize nchunks; // there are nchunks*CHUNK_SIZE bytes at data
  u8    data[0];
} allochead_t;

typedef struct {
  usize    chunk_cap; // total_chunks
  usize    chunk_len; // number of used (allocated) chunks
  u8*      chunks;
  bitset_t chunk_use; // a set bit means that chunk is in use; is allocated
} heap_t;

typedef struct {
  ilist_t list_entry;
  heap_t  allocator;
} subheap_t;

// typedef struct {
//   usize   size;
//   ilist_t free;
//   ilist_t used;
// } slabheap_t;

typedef struct kmem_ {
  rmm_t*  mm;
  RHMutex lock;
  ilist_t subheaps;
  //slabheap_t slabheaps[6] = {{16},{32},{64},{128},{256},{512}}; // TODO
  bool    expansion_in_progress;
} allocator_t;


static_assert(sizeof(subheap_t) <= PAGE_SIZE, "");


// CHUNK_SIZE: allocation chunk size, in bytes (must be a power of two)
// All allocations are at least CHUNK_SIZE large.
// All allocations are aligned to CHUNK_SIZE addresses.
#if UINTPTR_MAX < 0xffffffffffffffff
  #define CHUNK_SIZE 32u
#else
  #define CHUNK_SIZE 64u
#endif


// BEST_FIT_THRESHOLD: if the number of allocation chunks required are at least
// these many, use a "best fit" search instead of a "first fit" search.
// Used by kmem_alloc.
#define BEST_FIT_THRESHOLD 128u


// KMEM_TRACE: uncomment to enable logging a lot of info via dlog
#define KMEM_TRACE
//
#if defined(KMEM_TRACE) && defined(DEBUG)
  #define trace(fmt, args...) dlog("[kmem] " fmt, ##args)
#else
  #ifdef KMEM_TRACE
    #warning KMEM_TRACE has no effect unless DEBUG is enabled
    #undef KMEM_TRACE
  #endif
  #define trace(...) ((void)0)
#endif


// allochead_t* ALLOCHEAD(void* ptr) accesses the allochead_t of an allocation
#define ALLOCHEAD(ptr)  ((allochead_t*)( (((u8*)ptr) - sizeof(allochead_t)) ))


// heap_init initializes heap h with memory at p of size bytes
static void heap_init(heap_t* h, u8* p, usize size) {
  assert(size >= CHUNK_SIZE*2);
  // Top (low address; p) of the heap is memory we allocate.
  // Bottom (high address) of the heap contains a bitset index of chunk use.
  // The amount of space we need for the bitset depends on how much space is left
  // after allocating the bitset, so that makes this a little tricky.
  //
  //  p                                                         p+size
  //  ┣━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┯━━━━━━━━━┳━━━━━━━━━━━━━━━━━┫
  //  ┃ chunk 1 │ chunk 2 │ ...     │ chunk n ┃ bitset          ┃
  //  ┗━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━┷━━━━━━━━━╋━━━━━━━━━━━━━━━━━┛
  //                                        split
  //
  // We need to figure out the ideal "split"; where chunks end and bitset begin.
  // The bitset needs one bit per chunk and must be byte aligned.
  //
  // Begin by putting split at the end, leaving just one chunk for the bitset.
  // This is the highest split we can use, for the smallest size (CHUNK_SIZE*2).
  //

  // TODO: try binary search

  // ALT 2: exponential search
  const u8* p_end = p + size;
  usize chunk_cap = (size / CHUNK_SIZE) - 1;
  usize chunk_cap_sub = 1;
  u8* bitset_end = (p + (chunk_cap * CHUNK_SIZE)) + chunk_cap/8;
  while (bitset_end > p_end) {
    // dlog("p %p…%p, end bitset %p", p, p_end, bitset_end);
    chunk_cap -= chunk_cap_sub;
    chunk_cap_sub *= 2;
    bitset_end = (p + (chunk_cap * CHUNK_SIZE)) + chunk_cap/8;
  }
  // usize spill = (usize)((uintptr)p_end - (uintptr)bitset_end);
  // dlog("p %p…%p, split %p, end bitset %p, spill %zu",
  //   p, p_end, p + (chunk_cap * CHUNK_SIZE), bitset_end, spill);
  // assert((uintptr)bitset_end <= (uintptr)p_end);

  // // ALT 1: branchless approximation. Spills ~28 kiB for a 2 MiB memory size (~1.2%).
  // usize chunk_cap = size / (CHUNK_SIZE + 1);

  h->chunk_cap = chunk_cap;
  h->chunk_len = 0;
  h->chunks = p;
  bitset_init(&h->chunk_use, p + (chunk_cap * CHUNK_SIZE), chunk_cap);
}


// heap_contains returns true if h is the owner of allocation at ptr
// Note that ptr must be a pointer previously returned by heap_alloc;
// it must not be a pointer into an arbitrary address of the heap.
static bool heap_contains(const heap_t* h, void const* ptr) {
  const allochead_t* a = ALLOCHEAD(ptr);
  uintptr endaddr = (uintptr)h->chunks + (h->chunk_cap * CHUNK_SIZE);
  // in range [startaddr, endaddr) ?
  return ((uintptr)a >= (uintptr)h->chunks) & ((uintptr)ptr < endaddr);
}


#if DEBUG
static bool g_heap_alloc_force_best_fit = false;
#endif


// heap_alloc finds space in the heap h that is at least *sizep bytes.
// Returns NULL if there's no space, otherwise it returns a pointer to the allocated
// region and updates *sizep to the effective byte size of the region.
static void* nullable heap_alloc(heap_t* h, usize* sizep, usize alignment) {
  // our goal is to find enough chunks to hold sizeof(allochead_t)+(*sizep) bytes

  usize chunkalign = alignment / CHUNK_SIZE;
  usize size = *sizep + ((chunkalign * CHUNK_SIZE) / 2);
  usize chunkalign_nz = chunkalign + !chunkalign; // branchless (n ? n : 1)
  size = MAX(size, chunkalign_nz * CHUNK_SIZE);

  #define ALLOCHEAD_NCHUNKS 1
  static_assert(sizeof(allochead_t) <= CHUNK_SIZE*ALLOCHEAD_NCHUNKS, "");

  usize allochead_size = sizeof(allochead_t);
  if (chunkalign)
    allochead_size = ALLOCHEAD_NCHUNKS * CHUNK_SIZE;

  // calculate how many chunks we need, which includes one allochead_t
  usize needchunks = (size + allochead_size + CHUNK_SIZE - 1) / CHUNK_SIZE;
  //dlog("need %zu chunks (%zu B, chunkalign=%zu)", needchunks, size, chunkalign);

  // early exit if the number of available chunks are less than what's needed
  if (needchunks > h->chunk_cap - h->chunk_len)
    return NULL;

  // find a free range in the "chunks in use" bitset h->chunk_use
  usize ci = chunkalign_nz - ALLOCHEAD_NCHUNKS, clen;
  if (needchunks < BEST_FIT_THRESHOLD
    #if DEBUG
    && !g_heap_alloc_force_best_fit
    #endif
  ) {
    // first fit
    clen = bitset_find_unset_range(&h->chunk_use, &ci, needchunks, needchunks);
  } else {
    // best fit
    clen = bitset_find_best_fit(&h->chunk_use, &ci, needchunks);
  }

  if (clen == 0) // no space found
    return NULL;

  // align chunk start index and trim chunk length
  // TODO: clean this up
  if (chunkalign) {
    // assert(ci > 0);
    usize ci2 = ALIGN2(ci, chunkalign) - ALLOCHEAD_NCHUNKS;
    clen -= ci - ci2;
    ci = ci2;
  }

  // emplace allocation header and return allocation pointer past the header.
  void* p = h->chunks + (ci * CHUNK_SIZE);

  void* ptr = p + allochead_size;
  allochead_t* a = ptr - sizeof(allochead_t);
  a->nchunks = clen;
  *sizep = (clen*CHUNK_SIZE) - allochead_size;

  // update bitset to mark chunks as "in use"
  bitset_set_range(&h->chunk_use, ci, clen, true);

  h->chunk_len += clen;

  // fill allocated memory with scrub bytes, if enabled
  if (KMEM_ALLOC_SCRUB_BYTE)
    memset(ptr, KMEM_ALLOC_SCRUB_BYTE, *sizep);

  trace("[heap] allocating %p (%zu B) in %zu chunks [%zu…%zu)",
    ptr, *sizep, clen, ci, ci+clen);

  return ptr;
}


static void heap_free(heap_t* h, void* ptr) {
  assert(heap_contains(h, ptr));
  allochead_t* a = ALLOCHEAD(ptr);

  // calculate chunk index for the allocation
  uintptr ci = ((uintptr)a - (uintptr)h->chunks) / CHUNK_SIZE;

  assertf(bitset_get(&h->chunk_use, ci),
    "trying to free segment starting at %zu that is already free (ptr=%p)", ci, ptr);

  bitset_set_range(&h->chunk_use, ci, a->nchunks, false);

  assert(h->chunk_len >= a->nchunks);
  h->chunk_len -= a->nchunks;

  // fill freed memory with scrub bytes, if enabled
  if (KMEM_FREE_SCRUB_BYTE)
    memset(a, KMEM_FREE_SCRUB_BYTE, a->nchunks * CHUNK_SIZE);
}


#if DEBUG
  UNUSED static void heap_debug_dump_state(
    heap_t* h, void* nullable highlight_p, usize highlight_size)
  {
    if (h->chunk_len == 0) {
      fputs("(empty)\n", stderr);
      return;
    }
    // find last set bit in the bitmap
    const usize bucket_bits = 8 * sizeof(usize);
    const bitset_t bset = h->chunk_use;
    const usize last_bucket = bset.len / bucket_bits;
    const usize* buckets = (const usize*)bset.data;
    usize last_used_bit = 0;
    for (usize bucket = 0; bucket < last_bucket; bucket++) {
      if (buckets[bucket] == 0)
        continue;
      last_used_bit = (bucket * bucket_bits) + (usize)rsm_fls(buckets[bucket]);
    }
    usize trailing_bytes = (bset.len % bucket_bits) / 8;
    for (usize i = (bset.len / bucket_bits) * sizeof(usize); i < trailing_bytes; ++i) {
      if (((u8*)bset.data)[i])
        last_used_bit = i * 8;
    }

    assert(h->chunk_use.len > last_used_bit);
    fputs(
      "────┬──────────┬───────────"
      "────────────────────────────────────────────────────────────\n"
      "page│   address│ chunk use\n"
      "────┼──────────┼───────────"
      "────────────────────────────────────────────────────────────\n"
      , stderr);
    uintptr page_addr, chunk_idx;

    highlight_size += sizeof(allochead_t);
    uintptr highlight_start_addr = ALIGN2_FLOOR((uintptr)highlight_p, CHUNK_SIZE);
    uintptr highlight_end_addr = highlight_start_addr + ALIGN2(highlight_size, CHUNK_SIZE);

    for (chunk_idx = 0; chunk_idx < last_used_bit+1; chunk_idx++) {
      if ((chunk_idx % (PAGE_SIZE / CHUNK_SIZE)) == 0) {
        if (chunk_idx) fputc('\n', stderr);
        page_addr = (uintptr)h->chunks + (chunk_idx * CHUNK_SIZE);
        usize page_idx = (chunk_idx * CHUNK_SIZE) / PAGE_SIZE;
        fprintf(stderr, "%4zu│%10lx│%6zu ", page_idx, page_addr, chunk_idx);
      }
      uintptr addr = (uintptr)h->chunks + (chunk_idx * CHUNK_SIZE);
      if (highlight_start_addr <= addr && addr < highlight_end_addr) {
        fputs(bitset_get(&h->chunk_use, chunk_idx) ? "▓" : "_", stderr);
      } else {
        fputs(bitset_get(&h->chunk_use, chunk_idx) ? "░" : "_", stderr);
      }
    }
    fprintf(stderr,
      "\n···─┼────···───┼───···─────"
      "────────────────────────────────────────────────────────────\n"
      "%4zu│%10lx│%6zu END\n"
      "────┴──────────┴───────────"
      "────────────────────────────────────────────────────────────\n"
      ,
      ((h->chunk_cap * CHUNK_SIZE) / PAGE_SIZE) + 1,
      (uintptr)h->chunks + (h->chunk_cap * CHUNK_SIZE), // end address
      h->chunk_cap);
  }
#endif // DEBUG


inline static void subheap_init(subheap_t* sh, u8* base, usize size) {
  heap_init(&sh->allocator, base, size);
}

inline static void* nullable subheap_alloc(subheap_t* sh, usize* size, usize alignment) {
  return heap_alloc(&sh->allocator, size, alignment);
}

inline static bool subheap_try_free(subheap_t* sh, void* ptr) {
  if (!heap_contains(&sh->allocator, ptr))
    return false;
  heap_free(&sh->allocator, ptr);
  return true;
}


static void kmem_add_subheap(allocator_t* a, void* storage, usize size) {
  trace("add subheap %p (%zu B)", storage, size);
  assert(size > PAGE_SIZE);
  // place subheap struct in first page and give it the rest to use for its memory
  subheap_t* sh = storage;
  subheap_init(sh, storage + PAGE_SIZE, size - PAGE_SIZE);
  ilist_append(&a->subheaps, &sh->list_entry);
}


#if DEBUG
  UNUSED static void kmem_debug_dump_state(
    allocator_t* a, void* nullable highlight_p, usize highlight_size)
  {
    RHMutexLock(&a->lock);
    usize i = 0;
    ilist_for_each(lent, &a->subheaps) {
      subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
      heap_t* h = &sh->allocator;
      uintptr start_addr = (uintptr)h->chunks;
      uintptr end_addr = (uintptr)h->chunks + (h->chunk_cap * CHUNK_SIZE);
      uintptr end_addr_use = (uintptr)h->chunks + (h->chunk_len * CHUNK_SIZE);
      dlog("subheap %zu %zx…%zx %zu kiB (%zu kiB, %zu chunks in use)",
        i, start_addr, end_addr,
        (end_addr - start_addr) / 1024,
        (end_addr_use - start_addr) / 1024,
        h->chunk_len);
      heap_debug_dump_state(h, highlight_p, highlight_size);
    }
    RHMutexUnlock(&a->lock);
  }
#endif


// kmem_allocx attempts to allocate *size bytes.
// On success, *size is updated to its actual size.
void* nullable kmem_allocx(allocator_t* a, usize* size, usize alignment) {
  assertf(IS_POW2(alignment), "alignment %zu is not a power-of-two", alignment);
  assert(alignment <= PAGE_SIZE);

  void* ptr = NULL;

  RHMutexLock(&a->lock);
  assert(!a->expansion_in_progress);

  // TODO: g_slabheaps

  // attempt to allocate space in a subheap
  ilist_for_each(lent, &a->subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    if ((ptr = subheap_alloc(sh, size, alignment)))
      goto end;
  }

  panic("TODO expand memory");

end:
  RHMutexUnlock(&a->lock);
  return ptr;
}


void* nullable kmem_alloc(allocator_t* a, usize size) {
  return kmem_allocx(a, &size, 1);
}

void* nullable kmem_alloc_aligned(allocator_t* a, usize size, usize alignment) {
  return kmem_allocx(a, &size, alignment);
}


void kmem_free(allocator_t* a, void* ptr) {
  assertnotnull(ptr);

  RHMutexLock(&a->lock);
  assert(!a->expansion_in_progress);

  ilist_for_each(lent, &a->subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    if (subheap_try_free(sh, ptr))
      goto end;
  }

  safecheckf(0, "kmem_free: invalid address %p", ptr);
end:
  RHMutexUnlock(&a->lock);
}


usize kmem_alloc_size(usize size) {
  assert(size > 0);
  return ALIGN2(size + sizeof(allochead_t), CHUNK_SIZE) - sizeof(allochead_t);
}


#define ALLOCATOR_SIZE  ALIGN2(sizeof(allocator_t), PAGE_SIZE)


void* nullable allocator_create(rmm_t* mm, usize min_initmem) {
  usize nbyte = CEIL_POW2(ALLOCATOR_SIZE + min_initmem);
  dlog("allocating %zu pages (%zu kiB)", nbyte / PAGE_SIZE, nbyte / kiB);
  void* p = rmm_allocpages(mm, nbyte / PAGE_SIZE);
  if (!p)
    return NULL;

  allocator_t* a = p;
  memset(a, 0, sizeof(*a));
  a->mm = mm;
  ilist_init(&a->subheaps);
  if (!RHMutexInit(&a->lock))
    return NULL;

  // use the rest of the memory allocated for the allocator struct as a subheap
  // TODO: consider using this as a slabheap instead (when we have slabheaps)
  kmem_add_subheap(a, p + ALLOCATOR_SIZE, nbyte - ALLOCATOR_SIZE);

  return a;
}


void allocator_dispose(allocator_t* a) {
  // TODO: free subheaps (except the initial subheap at a+ALLOCATOR_SIZE)
  rmm_freepages(a->mm, a);
}


rerror kmem_init() {
  ilist_test();

  // create a memory manager
  rmm_t* mm = rmm_emplace(assertnotnull( osvmem_alloc(16 * MiB) ), 16 * MiB);

  allocator_t* a = assertnotnull( allocator_create(mm, 4 * MiB) );

  usize z = kmem_alloc_size(123);
  dlog("kmem_alloc_size(123) => %zu", z);

  void* p1, *p2, *p3, *p4, *p5;

  p1 = kmem_alloc(a, z - 3);
  dlog("kmem_alloc(%zu) => %p", z, p1);

  usize size2 = 100;
  p2 = kmem_allocx(a, &size2, 512);
  dlog("kmem_alloc_aligned(100,512) => %p (%p) %zu B",
    p2, (void*)ALIGN2((uintptr)p2,512), size2 );

  kmem_free(a, p1);
  kmem_free(a, p2);

  p3 = kmem_alloc(a, 800);
  dlog("kmem_alloc(800) => %p", p3);
  kmem_free(a, p3);

  p1 = kmem_alloc(a, CHUNK_SIZE*(BEST_FIT_THRESHOLD-2));
  p1 = kmem_alloc(a, CHUNK_SIZE);   // 0-2
  p2 = kmem_alloc(a, CHUNK_SIZE*3); // 2-6
  p3 = kmem_alloc(a, CHUNK_SIZE);   // 6-8
  p4 = kmem_alloc(a, CHUNK_SIZE);   // 8-10
  p5 = kmem_alloc(a, CHUNK_SIZE*3); // 10-14
  kmem_free(a, p2);
  kmem_free(a, p4);
  kmem_debug_dump_state(a, NULL, 0);
  // now, for a CHUNK_SIZE allocation,
  // the "best fit" allocation strategy should select chunks 8-10, and
  // the "first fit" allocation strategy should select chunks 2-4.

  g_heap_alloc_force_best_fit = true;
  dlog("best fit");
  p2 = kmem_alloc(a, CHUNK_SIZE);
  kmem_debug_dump_state(a, p2, CHUNK_SIZE);
  kmem_free(a, p2);

  g_heap_alloc_force_best_fit = false;
  dlog("first fit");
  p2 = kmem_alloc(a, CHUNK_SIZE);
  kmem_debug_dump_state(a, p2, CHUNK_SIZE);
  kmem_free(a, p2);


  kmem_free(a, p5);
  kmem_free(a, p3);
  kmem_free(a, p1);

  allocator_dispose(a);

  log("——————————————————");
  return rerr_canceled;
}

