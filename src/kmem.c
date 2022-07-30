// kernel-style memory allocator
// SPDX-License-Identifier: Apache-2.0

#include "rsmimpl.h"
#define ILIST_TEST_IMPL
#include "list.h"
#include "bits.h"
#include "mem.h"

#define PAGE_SIZE 4096u

//———————————————————————————————————————————————————————————————————————————————————
// API

typedef struct kmem_ kmem_t;

void* nullable kmem_alloc(kmem_t*, usize size)
  __attribute__((malloc, alloc_size(2)));

void* nullable kmem_allocx(kmem_t*, usize* size_in_out, usize alignment) // returns actual size
  __attribute__((malloc, alloc_align(3)));

void* nullable kmem_alloc_aligned(kmem_t*, usize size, usize alignment)
  __attribute__((malloc, alloc_size(2), alloc_align(3)));

void kmem_free(kmem_t*, void* ptr, usize size);

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

// KMEM_TRACE: uncomment to enable logging a lot of info via dlog
#define KMEM_TRACE

// _SCRUB_BYTE defines a byte value that, upon allocating or freeing a region,
// memory is filled with (ie. memset(ptr,_SCRUB_BYTE,size).)
// This is useful for debugging memory management issues, like use after free, since
// memory managed by this allocator is not subject to host memory protection.
// Set to zero to disable scrubbing.
#define KMEM_ALLOC_SCRUB_BYTE 0xbb
#define KMEM_FREE_SCRUB_BYTE  0xaa

// KMEM_ENABLE_SLABHEAPS: define to enable use of slabheaps; speeds up small allocations
#define KMEM_ENABLE_SLABHEAPS

#define SLABHEAP_BLOCK_SIZE  ((usize)(PAGE_SIZE * 16lu))
#define SLABHEAP_BLOCK_MASK  (~(SLABHEAP_BLOCK_SIZE - 1))
#define SLABHEAP_MIN_SIZE    sizeof(void*) /* must be pow2 */
#define SLABHEAP_MAX_BLOCKS  (SLABHEAP_BLOCK_SIZE / SLABHEAP_MIN_SIZE)
static_assert(IS_ALIGN2(SLABHEAP_BLOCK_SIZE, PAGE_SIZE), "");

// HEAP_MAX_ALIGN: maximum alignment factor that heap_alloc can handle
#define HEAP_MAX_ALIGN  XMAX(PAGE_SIZE, SLABHEAP_BLOCK_SIZE)
static_assert(IS_POW2(HEAP_MAX_ALIGN), "");


typedef struct {
  // There are nchunks*CHUNK_SIZE bytes at data.
  //   We only need 46 bits to represent the largest possible allocation
  //   i.e. bits_needed = log2(pow(2, addressable_bits) / CHUNK_SIZE)
  //   e.g. log2(pow(2,52) / 64) = 46
  //   So, we could use the other 18 bits for additional data at no extra cost:
  //   usize nchunks : 46;
  //   usize foo : 18;
  usize nchunks;
  u8    data[0];
} allocmeta_t;

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

typedef struct slabchunk_ {
  struct slabchunk_* nullable next;
} slabchunk_t;

typedef struct slabblock_ slabblock_t;
struct slabblock_ {
  slabblock_t* nullable next;    // next block in parent slabheap_t's list
  slabchunk_t*          recycle; // list of recycled chunks
  u32                   cap;     // total chunks at data
  u32                   len;     // unallocated chunks at data (<=SLABHEAP_MAX_BLOCKS)
};

typedef struct {
  usize                 size;   // chunk size
  slabblock_t* nullable usable; // list of blocks with free space
  slabblock_t* nullable full;   // list of blocks which chunks are all allocated
} slabheap_t;

typedef struct kmem_ {
  rmm_t*     mm;
  RHMutex    lock;
  ilist_t    subheaps;
  #ifdef KMEM_ENABLE_SLABHEAPS
  slabheap_t slabheaps[4];
  #endif
  bool       expansion_in_progress;
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


#if defined(KMEM_TRACE) && defined(DEBUG)
  #define trace(fmt, args...) dlog("[kmem] " fmt, ##args)
#else
  #ifdef KMEM_TRACE
    #warning KMEM_TRACE has no effect unless DEBUG is enabled
    #undef KMEM_TRACE
  #endif
  #define trace(...) ((void)0)
#endif


// allocmeta_t* ALLOCHEAD(void* ptr) accesses the allocmeta_t of an allocation
#define ALLOCHEAD(ptr) \
  ((allocmeta_t*)( ((uintptr)(ptr) - sizeof(allocmeta_t)) & CHUNK_MASK ))

#define CHUNK_MASK  (~((uintptr)CHUNK_SIZE - 1))


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
  const allocmeta_t* a = ALLOCHEAD(ptr);
  uintptr endaddr = (uintptr)h->chunks + (h->chunk_cap * CHUNK_SIZE);
  // in range [startaddr, endaddr) ?
  return ((uintptr)a >= (uintptr)h->chunks) & ((uintptr)ptr < endaddr);
}


// heap_alloc finds space in the heap h that is at least *sizep bytes.
// Returns NULL if there's no space, otherwise it returns a pointer to the allocated
// region and updates *sizep to the effective byte size of the region.
static void* nullable heap_alloc(heap_t* h, usize* sizep, usize alignment) {
  // Our goal is to find enough chunks to hold sizeof(allocmeta_t)+(*sizep) bytes.
  //
  // Alignment is a little tricky. A few considerations:
  // - Alignment is always a power-of-two (1 2 4 8 16 32 64 128 256 ...)
  // - Each chunk is aligned to CHUNK_SIZE address (but no other guarantees)
  // - The head of the first chunk will contain the allocmeta_t struct,
  //   so our starting address is offset by sizeof(allocmeta_t)
  // - allocmeta_t is just one address wide so most of the space of a chunk is
  //   free to return to the caller
  // - When alignment is less than CHUNK_SIZE, we should use the first chunk if
  //   we can, rather than just allocating a second one (else we waste space.)
  // - We need to know the number of chunks we need before we know the address
  //   of the first chunk, meaning that for large alignment constraints we need
  //   allocate more chunks than we actually need.
  //   *However* we have a trick up our sleves: since we know that chunk#0 is
  //   PAGE_SIZE aligned, we can figure out the chunk alignment needed and
  //   search chunks ranges starting at this alignment.
  //
  // Example of 16B alignment, where we add 8B padding to get a 16B aligned address:
  //
  //   ┌───────────────┬───────────────┬───────────────────────────────┬───────
  //   │byte           │    1 1 1 1 1 1│1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3
  //   │0 1 2 3 4 5 6 7│8 9 0 1 2 3 4 5│6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
  //   ├───────────────┼───────────────┼───────────────────────────────────────
  //   │allocmeta_t    │  padding      │ user data
  //   ├───────────────┼───────────────┼───────────────────────────────┬───────
  //   64 aligned      8 aligned       16 aligned                      32 aligned
  //
  dlog("—————————————————————————————————————");

  // align_rem: The remainder of alignment after "taking away" CHUNK_SIZE
  // align_big: The chunk-sized portion of alignment
  usize align_rem = alignment % CHUNK_SIZE;
  usize align_big = alignment - align_rem;

  // Since we will use align_rem with the ALIGN2 function which doesn't support
  // a value of zero, clamp align_rem to [1 (i.e. align_rem += align_rem == 0 ? 1 : 0)
  align_rem += !align_rem;

  // head_size: size needed for allocmeta AND padding
  const usize head_size = ALIGN2(sizeof(allocmeta_t), align_rem);

  // nchunks: the number of chunks we need.
  // We add an extra once since integer division rounds down but we need
  // the "ceiling", enough chunks to fit head_size and *sizep.
  usize nchunks = (head_size + (*sizep) + CHUNK_SIZE - 1) / CHUNK_SIZE;

  // nchunks_extra: In case the alignment factor is >=CHUNK_SIZE, we'll need
  // additional chunks so that we can adjust the address in case the first chunk's
  // address has a lesser alignment. Again, we divide to get the "ceiling".
  // Note that align_big is 0 if alignment < CHUNK_SIZE.
  usize nchunks_extra = (align_big + CHUNK_SIZE - 1) / CHUNK_SIZE;

  // chunks_align: alignment requirement of chunk range
  // usize chunks_align = (alignment + CHUNK_SIZE - 1) / CHUNK_SIZE;
  usize chunks_align = alignment / CHUNK_SIZE;

  dlog("alignment     %5zu", alignment);
  dlog("size          %5zu", *sizep);
  dlog("  align_rem   %5zu", align_rem);
  dlog("  align_big   %5zu", align_big);
  dlog("head_size     %5zu", head_size);
  dlog("nchunks       %5zu", nchunks);
  dlog("nchunks_extra %5zu", nchunks_extra);
  dlog("chunks_align  %5zu", chunks_align);

  // Now we will search for a free range in the "chunks in use" bitset h->chunk_use.

  // chunk_index is the chunk we start searching.
  // When the search completes, chunk_index holds the index of the first chunk
  // in the free range.
  usize chunk_index = chunks_align;
  if (chunks_align > 1) {
    chunk_index = chunks_align - 1;
    nchunks++;
  }

  // Before we go look for a range of free chunks,
  // exit early if the number of available chunks are less than what's needed
  if (h->chunk_cap - h->chunk_len < nchunks + chunk_index)
    return NULL;

  // chunk_len will contain the number of consecutive chunks found
  usize chunk_len;

  // search_stride is the step size used while searching for a free range of blocks.
  // "x+!x" has the effect of "if (x==0) x = 1".
  usize search_stride = chunks_align + !chunks_align;

  dlog("chunk_index   %5zu", chunk_index);
  if (nchunks < BEST_FIT_THRESHOLD) {
    dlog(">> bitset_find_first_fit");
    chunk_len = bitset_find_first_fit(
      &h->chunk_use, &chunk_index, nchunks, search_stride);
  } else {
    dlog(">> bitset_find_best_fit");
    chunk_len = bitset_find_best_fit(&h->chunk_use, &chunk_index, nchunks, search_stride);
  }

  dlog("chunk_index   %5zu", chunk_index);
  dlog("chunk_len     %5zu", chunk_len);

  // Give up if we didn't find a range of chunks large enough
  if (chunk_len == 0)
    return NULL;

  // We found a range of free chunks!
  // Update the bitset to mark the chunks as "in use"
  bitset_set_range(&h->chunk_use, chunk_index, chunk_len, true);

  // Increment total number of chunks "in use" in the heap
  h->chunk_len += chunk_len;

  // chunk1 is the address of the first chunk.
  void* chunk1 = h->chunks + (chunk_index * CHUNK_SIZE);
  assert(IS_ALIGN2((uintptr)chunk1, CHUNK_SIZE));
  dlog("chunk1     %p", chunk1);

  // Put the allocation header at the beginning of the first chunk
  allocmeta_t* header = chunk1;
  header->nchunks = chunk_len;

  // start_addr is the address returned to the caller, a pointer to usable memory
  uintptr start_addr = (
    chunks_align ? (uintptr)chunk1 + CHUNK_SIZE
                 : (uintptr)chunk1 + head_size );
  assertf(IS_ALIGN2(start_addr, alignment),
    "bug in %s (start_addr 0x%lx, alignment %zu)", __FUNCTION__, start_addr, alignment);

  // end_addr is the end of the address range
  uintptr end_addr = (uintptr)chunk1 + (chunk_len * CHUNK_SIZE);

  // Return back to the caller the actual usable size of the allocation
  usize req_size = *sizep;
  *sizep = (usize)(end_addr - start_addr);

  dlog("req. size     %5zu", req_size);
  dlog("usable size   %5zu", *sizep);
  assert(req_size <= *sizep);

  dlog("start_addr %p", (void*)start_addr);
  dlog("end_addr   %p", (void*)end_addr);

  // Make sure our ALLOCHEAD macro works as expected
  assert(ALLOCHEAD(start_addr) == header);

  // fill allocated memory with scrub bytes (if enabled)
  if (KMEM_ALLOC_SCRUB_BYTE)
    memset((void*)start_addr, KMEM_ALLOC_SCRUB_BYTE, *sizep);

  trace("[heap] allocating %p (%zu B) in %zu chunks [%zu…%zu)",
    (void*)start_addr, *sizep, chunk_len, chunk_index, chunk_index + chunk_len);
  dlog("—————————————————————————————————————");

  return (void*)start_addr;
}


static void heap_free(heap_t* h, void* ptr, usize size) {
  assert(heap_contains(h, ptr));

  allocmeta_t* a = ALLOCHEAD(size > CHUNK_SIZE ? ptr - CHUNK_SIZE : ptr);

  assertf(size <= a->nchunks * CHUNK_SIZE,
    "freeing %p of smaller size (%zu) than expected (%zu)",
    ptr, a->nchunks * CHUNK_SIZE, size);

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

    highlight_size += sizeof(allocmeta_t);
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


static bool kmem_add_subheap(allocator_t* a, void* storage, usize size) {
  assertf(IS_ALIGN2((uintptr)storage, PAGE_SIZE),
    "subheap storage %p not page aligned", storage);
  assert(size > PAGE_SIZE);

  subheap_t* sh;

  if (IS_ALIGN2((uintptr)storage, HEAP_MAX_ALIGN)) {
    // place subheap struct at the end to minimize spill
    uintptr tmp_addr = (uintptr)storage + size - sizeof(subheap_t);
    sh = (subheap_t*)ALIGN2_FLOOR(tmp_addr, _Alignof(subheap_t));
    size -= (usize)((uintptr)sh - (uintptr)storage);
  } else {
    // place subheap struct at the beginning and align storage
    sh = storage;
    storage = (void*)ALIGN2((uintptr)storage + sizeof(subheap_t), HEAP_MAX_ALIGN);
    usize size_diff = (usize)((uintptr)storage - (uintptr)sh);
    if (size_diff > 0)
      trace("forfeiting %zu kiB in subheap (HEAP_MAX_ALIGN alignment)", size_diff / kiB);
    if (check_sub_overflow(size, size_diff, &size) || size < PAGE_SIZE)
      return false; // not enough space
  }

  assertf(IS_ALIGN2((uintptr)storage, HEAP_MAX_ALIGN), "%p", storage);

  trace("add subheap %p (data %p, %zu B)", sh, storage, size);
  subheap_init(sh, storage, size);
  ilist_append(&a->subheaps, &sh->list_entry);
  return true;
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


static bool kmem_expand(allocator_t* a, usize minsize) {
  panic("TODO expand memory");
  return false;
}


static void* nullable kmem_heapalloc(allocator_t* a, usize* size, usize alignment) {
  // assertf(alignment <= PAGE_SIZE, "%zu", alignment);
  ilist_for_each(lent, &a->subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    void* ptr = subheap_alloc(sh, size, alignment);
    if (ptr)
      return ptr;
  }
  return NULL;
}


#ifdef KMEM_ENABLE_SLABHEAPS


static void* nullable slabheap_alloc(allocator_t* a, slabheap_t* sh) {
  slabblock_t* block = sh->usable;

  if UNLIKELY(block == NULL) {
    trace("[slabheap %zu] grow", sh->size);
    usize size = SLABHEAP_BLOCK_SIZE;
    static_assert(_Alignof(slabblock_t) <= SLABHEAP_MIN_SIZE, "");
    for (;;) {
      if LIKELY(( block = kmem_heapalloc(a, &size, SLABHEAP_BLOCK_SIZE) ))
        break;
      if UNLIKELY(!kmem_expand(a, size))
        return NULL;
    }
    trace("[slabheap %zu] allocated new block %p", sh->size, block);
    assertf((uintptr)block % SLABHEAP_BLOCK_SIZE == 0,
      "misaligned address %p returned by kmem_heapalloc", block);
    block->cap = SLABHEAP_BLOCK_SIZE / sh->size;
    block->len = 0;
    block->recycle = NULL;
    block->next = NULL;
    sh->usable = block; // set as "usable" list
  }

  // Try to recycle a chunk
  slabchunk_t* chunk = block->recycle;

  // No chunk to recycle; allocate a new one from the block
  if UNLIKELY(chunk == NULL) {
    assertf(block->cap - block->len > 0, "full block %p found on sh->usable!", block);
    uintptr data_addr = ALIGN2((uintptr)block + sizeof(slabblock_t), sh->size);
    assertf(data_addr % sh->size == 0, "misaligned data_addr %p", (void*)data_addr);
    chunk = (slabchunk_t*)( data_addr + (block->len * sh->size) );
    chunk->next = NULL; // for "block->recycle = chunk->next" later on
    block->len++;
  }

  // Dequeue the chunk from the block's recycle list (free list)
  // No matter if the chunk was the last one or the list was empty (branch above),
  // this has the same effect.
  block->recycle = chunk->next;

  // if the recycle list is empty and all chunks are allocated, the block is full
  // and we need to move the block to the sh->full list
  if (chunk->next == NULL && block->len == block->cap) {
    trace("[slabheap %zu] mark block %p as full", sh->size, block);
    sh->usable = block->next;
    block->next = sh->full;
    sh->full = block;
  }

  #ifdef KMEM_TRACE
  uintptr data_addr = ALIGN2((uintptr)block + sizeof(slabblock_t), sh->size);
  trace("[slabheap %zu] allocating chunk %zu %p from block %p",
    sh->size, ((uintptr)chunk - data_addr) / sh->size, chunk, block);
  #endif

  return chunk;
}


static void slabheap_free(allocator_t* a, slabheap_t* sh, void* ptr) {
  assertf( !((uintptr)ptr % sh->size), "invalid address %p (slab %zu)", ptr, sh->size);

  // fill freed memory with scrub bytes, if enabled
  if (KMEM_FREE_SCRUB_BYTE)
    memset(ptr, KMEM_FREE_SCRUB_BYTE, sh->size);

  slabblock_t* block = (slabblock_t*)((uintptr)ptr & SLABHEAP_BLOCK_MASK);

  bool block_full = block->recycle == NULL && block->len == block->cap;

  // add chunk to the block's recycle list
  slabchunk_t* chunk = ptr;
  chunk->next = block->recycle;
  block->recycle = chunk;

  #ifdef KMEM_TRACE
  uintptr data_addr = ALIGN2((uintptr)block + sizeof(slabblock_t), sh->size);
  trace("[slabheap %zu] freeing chunk %zu %p from block %p",
    sh->size, ((uintptr)ptr - data_addr) / sh->size, ptr, block);
  #endif

  // If the block was fully used, it no longer is and we need to
  // move it from the "full" list to the "usable" list.
  if (block_full) {
    trace("[slabheap %zu] mark block %p as usable", sh->size, block);
    sh->full = block->next;
    block->next = sh->usable;
    sh->usable = block;
  }
}


#endif // KMEM_ENABLE_SLABHEAPS


// kmem_allocx attempts to allocate *size bytes.
// On success, *size is updated to its actual size.
void* nullable kmem_allocx(allocator_t* a, usize* size, usize alignment) {
  assertf(IS_POW2(alignment), "alignment %zu is not a power-of-two", alignment);
  assertf(alignment <= PAGE_SIZE, "%zu", alignment);

  void* ptr = NULL;

  #ifdef KMEM_ENABLE_SLABHEAPS
    const usize slabsize = ALIGN2(*size, alignment);
  #endif

  RHMutexLock(&a->lock);
  assert(!a->expansion_in_progress);

retry:

  // Attempt to allocate space in a slabheap.
  // This succeeds for the common case of a small allocation size.
  #ifdef KMEM_ENABLE_SLABHEAPS
    for (usize i = 0; i < countof(a->slabheaps); i++) {
      if (slabsize <= a->slabheaps[i].size) {
        *size = a->slabheaps[i].size;
        ptr = slabheap_alloc(a, &a->slabheaps[i]);
        if UNLIKELY(ptr == NULL)
          goto expand;
        goto end;
      }
    }
  #endif

  // Attempt to allocate space in a subheap
  if LIKELY((ptr = kmem_heapalloc(a, size, alignment)))
    goto end;

#ifdef KMEM_ENABLE_SLABHEAPS
expand:
#endif

  if (kmem_expand(a, *size))
    goto retry;

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


void kmem_free(allocator_t* a, void* ptr, usize size) {
  assertnotnull(ptr);

  RHMutexLock(&a->lock);
  assert(!a->expansion_in_progress);

  #ifdef KMEM_ENABLE_SLABHEAPS
    for (usize i = 0; i < countof(a->slabheaps); i++) {
      if (size <= a->slabheaps[i].size) {
        slabheap_free(a, &a->slabheaps[i], ptr);
        goto end;
      }
    }
  #endif

  ilist_for_each(lent, &a->subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    if (!heap_contains(&sh->allocator, ptr))
      continue;
    heap_free(&sh->allocator, ptr, size);
    goto end;
  }

  safecheckf(0, "kmem_free: invalid address %p", ptr);
end:
  RHMutexUnlock(&a->lock);
}


usize kmem_alloc_size(usize size) {
  assert(size > 0);
  return ALIGN2(size + sizeof(allocmeta_t), CHUNK_SIZE) - sizeof(allocmeta_t);
}


void* nullable allocator_create(rmm_t* mm, usize min_initmem) {
  const usize allocator_size = ALIGN2(sizeof(allocator_t), _Alignof(allocator_t));

  // rmm requires page allocations in pow2 orders
  usize nbyte = CEIL_POW2(allocator_size + min_initmem);

  trace("create allocator with %zu pages (%zu kiB, %zu kiB usable)",
    nbyte / PAGE_SIZE, nbyte / kiB, (nbyte - allocator_size) / kiB);
  void* p = rmm_allocpages(mm, nbyte / PAGE_SIZE);
  if (!p)
    return NULL;

  // place the allocator at the end of the page range to increase the chances
  // of perfect alignment of the initial heap (which has HEAP_MAX_ALIGN alignment.)
  allocator_t* a = (void*)( ((uintptr)p + nbyte) - allocator_size );
  memset(a, 0, sizeof(*a));
  a->mm = mm;
  ilist_init(&a->subheaps);
  if (!RHMutexInit(&a->lock))
    return NULL;

  // initialize slab heaps, starting with size=sizeof(void*)
  // TODO: tune these sizes once we have some stats on usage.
  #ifdef KMEM_ENABLE_SLABHEAPS
    for (usize i = 0; i < countof(a->slabheaps); i++) {
      a->slabheaps[i].size = 1lu << (i + ILOG2(SLABHEAP_MIN_SIZE));
      a->slabheaps[i].usable = NULL;
      a->slabheaps[i].full = NULL;
      trace("init slabheaps[%zu] (%zu B)", i, a->slabheaps[i].size);
    }
  #endif

  // use the rest of the memory allocated for the allocator struct as a subheap
  // TODO: consider using this as a slabheap instead (when we have slabheaps)
  kmem_add_subheap(a, p, nbyte - allocator_size);

  return a;
}


void allocator_dispose(allocator_t* a) {
  // TODO: free subheaps (except the initial subheap which a is embedded into)
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

  // slabheap
  // void* ptrs[SLABHEAP_BLOCK_SIZE / 64];
  void* ptrs[4];
  for (usize i = 0; i < countof(ptrs); i++)
    ptrs[i] = kmem_alloc(a, 64);
  for (usize i = 0; i < countof(ptrs); i++)
    kmem_free(a, ptrs[i], 64);
  // push it over the limit
  p1 = kmem_alloc(a, 64);
  kmem_free(a, p1, 64);

  p1 = kmem_alloc(a, z - 3);
  dlog("kmem_alloc(%zu) => %p", z, p1);

  usize size2 = 100;
  p2 = kmem_allocx(a, &size2, 512);
  dlog("kmem_alloc_aligned(100,512) => %p (%p) %zu B",
    p2, (void*)ALIGN2((uintptr)p2,512), size2 );

  kmem_free(a, p1, z - 3);
  kmem_free(a, p2, size2);

  p3 = kmem_alloc(a, 800);
  dlog("kmem_alloc(800) => %p", p3);
  kmem_free(a, p3, 800);

  p1 = kmem_alloc(a, CHUNK_SIZE*(BEST_FIT_THRESHOLD-2));
  p1 = kmem_alloc(a, CHUNK_SIZE);   // 0-2
  p2 = kmem_alloc(a, CHUNK_SIZE*3); // 2-6
  p3 = kmem_alloc(a, CHUNK_SIZE);   // 6-8
  p4 = kmem_alloc(a, CHUNK_SIZE);   // 8-10
  p5 = kmem_alloc(a, CHUNK_SIZE*3); // 10-14
  kmem_free(a, p2, CHUNK_SIZE*3);
  kmem_free(a, p4, CHUNK_SIZE);
  kmem_debug_dump_state(a, NULL, 0);
  // now, for a CHUNK_SIZE allocation,
  // the "best fit" allocation strategy should select chunks 8-10, and
  // the "first fit" allocation strategy should select chunks 2-4.

  p2 = kmem_alloc(a, CHUNK_SIZE);
  kmem_debug_dump_state(a, p2, CHUNK_SIZE);
  kmem_free(a, p2, CHUNK_SIZE);

  kmem_free(a, p5, CHUNK_SIZE*3);
  kmem_free(a, p3, CHUNK_SIZE);
  kmem_free(a, p1, CHUNK_SIZE*(BEST_FIT_THRESHOLD-2));

  allocator_dispose(a);

  log("——————————————————");
  return rerr_canceled;
}

