// kernel-style memory allocator
// SPDX-License-Identifier: Apache-2.0

#include "rsmimpl.h"
#define ILIST_TEST_IMPL
#include "list.h"

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

void* nullable kmem_alloc(usize size)
  __attribute__((malloc, alloc_size(1)));

void* nullable kmem_allocx(usize* size_in_out, usize alignment) // returns actual size
  __attribute__((malloc, alloc_align(2)));

void* nullable kmem_alloc_aligned(usize size, usize alignment)
  __attribute__((malloc, alloc_size(1), alloc_align(2)));

void kmem_free(void* ptr);

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

// CHUNK_SIZE: allocation chunk size, in bytes (must be a power of two)
// All allocations are at least CHUNK_SIZE large.
// All allocations are aligned to CHUNK_SIZE addresses.
#if UINTPTR_MAX < 0xffffffffffffffff
  #define CHUNK_SIZE 32u
#else
  #define CHUNK_SIZE 64u
#endif

#define KMALLOC_INIT_SIZE (2u * MiB)
static_assert(KMALLOC_INIT_SIZE % PAGE_SIZE == 0, "");

// BEST_FIT_THRESHOLD: if the number of allocation chunks required are at least
// these many, use a "best fit" search instead of a "first fit" search.
// Used by kmem_alloc.
#define BEST_FIT_THRESHOLD 128u


typedef struct {
  usize nchunks; // there are nchunks*CHUNK_SIZE bytes at data
  u8    data[0];
} allochead_t;

typedef struct {
  u8*   data;
  usize len; // number of bits at data
} bitset_t;

typedef struct {
  usize    chunk_cap; // total_chunks
  usize    chunk_len; // number of used (allocated) chunks
  u8*      chunks;
  bitset_t chunk_use; // a set bit means that chunk is in use; is allocated
} heap_t;

typedef struct subheap subheap_t;
struct subheap {
  ilist_t list_entry;
  heap_t  allocator;
};
static_assert(sizeof(subheap_t) <= PAGE_SIZE, "");

// allochead_t* ALLOCHEAD(void* ptr) accesses the allochead_t of an allocation
#define ALLOCHEAD(ptr)  ((allochead_t*)( (((u8*)ptr) - sizeof(allochead_t)) ))

#define trace(fmt, args...) dlog("[kmalloc] " fmt, ##args)


// kmalloc global state
static u8 g_init_mem[KMALLOC_INIT_SIZE] __attribute__((aligned(PAGE_SIZE)));
static RHMutex g_lock;
static ilist_t g_subheaps = ILIST_INIT_HEAD(g_subheaps);
//static slabheap_t g_slabheaps[6] = {{16},{32},{64},{128},{256},{512}}; // TODO
static bool    g_expansion_in_progress = false;

static const u8 kBitsetMaskFirst[8] = { 0xFF, 0xFE, 0xFC, 0xF8, 0xF0, 0xE0, 0xC0, 0x80 };
static const u8 kBitsetMaskLast[8] = { 0x00, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F };


static void bitset_init(bitset_t* bset, u8* data, usize len) {
  bset->data = data;
  bset->len = len;
}


inline static bool bitset_get(const bitset_t* bset, usize index) {
  //assert(index < bset->len);
  return bset->data[index / 8] & (1lu << (index % 8));
}


static void bitset_set_range(bitset_t* bset, usize start, usize len, bool on) {
  assert(len > 0);
  assert(start+len <= bset->len);

  u8* first = &bset->data[start / 8];
  u8* last = &bset->data[(start + len) / 8];
  u8 mask = kBitsetMaskFirst[start % 8];

  if (first == last) {
    mask &= kBitsetMaskLast[(start + len) % 8];
    // branchless (*first = on ? (*first | mask) : (*first & ~mask))
    *first = COND_BYTE_MASK(*first, mask, on);
    return;
  }

  *first = COND_BYTE_MASK(*first, mask, on);

  mask = kBitsetMaskLast[(start + len) % 8];
  *last = COND_BYTE_MASK(*last, mask, on);

  first++;

  // set all bytes in between
  memset(first, 0xFF * (u8)on, last - first);
}


// bitset_find_unset_range searches for a contiguous region of unset bits.
//   start: #bit to start the search at.
//   minlen: minimum number of unset bytes needed
//   maxlen: maximum number of unset bytes to consider
// Returns >=minlen if a range was found (and updates startp.)
// Returns 0 if no range large enough was found (may still update startp.)
static usize bitset_find_unset_range(
  bitset_t* bset, usize* startp, usize minlen, usize maxlen)
{
  // First fit implementation
  //
  //           0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
  //         ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
  // bit     │ ╳ │ ╳ │ ╳ │ ╳ │   │   │ ╳ │   │   │   │   │   │ ╳ │   │   │
  //         ├───┴───┴───┴───┼───┴───┴───┴───┼───┴───┴───┴───┼───┴───┴───┘
  // bucket  │       ╳       │       /       │               │ trailing bits
  //         └───────────────┴───────────────┴───────────────┘
  //                 1               2               3
  //           ↑
  //         start
  //
  // We start by splitting up the bitmask into register-sized (bucket_bits) buckets.
  // Then we iterate over the buckets in order. If a bucket is MAXSIZE we know there
  // are no free bits. If a bucket is 0 we know there are bucket_bits free bits.
  // If a bucket is neither full nor empty, we scan its individual bits for a free
  // range. Finally, if bset->len-start is not aligned to bucket_bits, we scan the
  // "trailing bits" for a free range.

  usize init_start = *startp;

  assert(maxlen >= minlen);

  // We'll work with a register sized granule ("bucket") over the bitset
  usize bucket_bits = 8 * sizeof(usize); // bit size of one "bucket"
  usize* buckets = (usize*)bset->data;

  const usize start_bucket = (usize)(uintptr)*startp / bucket_bits;
  const usize end_bucket = bset->len / bucket_bits; // bucket index to stop before
  //dlog("start_bucket startp=%zu => %zu", *startp, start_bucket);

  u8 start_bitoffs = (u8)(*startp % bucket_bits); // bit index to start at

  usize freelen = 0; // current "free range" length

  for (usize bucket = start_bucket; bucket < end_bucket; bucket++) {
    //dlog("** bucket %zu %zx", bucket, buckets[bucket]);

    // if bucket is full
    if (buckets[bucket] == USIZE_MAX) {
      if (freelen >= minlen)
        goto found;
      freelen = 0; // reset start of free range
      start_bitoffs = 0; // reset start bit
      continue;
    }

    // if bucket is empty
    if (buckets[bucket] == 0lu) {
      if (freelen == 0)
        *startp = bucket * bucket_bits;
      freelen += bucket_bits - start_bitoffs;
      if (freelen >= maxlen) {
        //dlog("-> %zu", maxlen);
        *startp += start_bitoffs;
        return maxlen;
      }
      start_bitoffs = 0; // reset start bit
      continue;
    }

    // else: bucket has some free space; scan its bits
    usize bucket_val = buckets[bucket];
    u8 nbits = start_bitoffs; // number of bits we've looked at
    u32 tz = 0; // number of trailing zeroes
    bucket_val >>= nbits;
    start_bitoffs = 0;

    // visit each bit
    while (nbits < bucket_bits) {
      if (bucket_val == 0) {
        if (freelen == 0)
          *startp = bucket * bucket_bits + nbits;
        freelen += bucket_bits - nbits;
        nbits = bucket_bits;
      } else {
        tz = rsm_ctz(bucket_val);
        bucket_val >>= tz;

        if (freelen == 0)
          *startp = bucket * bucket_bits + nbits;
        freelen += tz;
        nbits += tz;

        if (freelen >= minlen)
          goto found;

        // Deleting trailing ones.
        u32 trailing_ones = rsm_ctz(~bucket_val);
        bucket_val >>= trailing_ones;
        nbits += trailing_ones;
        freelen = 0;
      }
    }
  }

  if (freelen >= minlen)
    goto found;

  usize ftb = (bset->len / bucket_bits) * bucket_bits; // first trailing #bit
  usize trailing_bits = bset->len % bucket_bits;
  for (usize i = 0; i < trailing_bits; ++i) {
    if (bitset_get(bset, ftb + i)) {
      freelen = 0; // reset freelen
      continue;
    }
    // bit is not set; this slot is free
    if (freelen == 0)
      *startp = ftb + i;
    freelen++;
    if (freelen >= minlen)
      goto found;
  }

  // no free range found that satisfies freelen >= minlen
  freelen = 0;
found:
  //dlog("-> MIN(%zu, %zu) = %zu", freelen, maxlen, MIN(freelen, maxlen));
  return MIN(freelen, maxlen);
}


// bitset_find_best_fit searches for the smallest hole that is >=minlen large
static usize bitset_find_best_fit(bitset_t* bset, usize* startp, usize minlen) {
  usize start = *startp;
  usize best_start = 0;
  usize best_len = bset->len + 1;
  usize found = 0;
  //dlog("bitset_find_best_fit minlen=%zu", minlen);
  for (;;) {
    usize rangelen = bitset_find_unset_range(bset, &start, minlen, best_len);
    //dlog(">> bitset_find_unset_range => start=%zu len=%zu", start, rangelen);
    if (best_len > rangelen || !found) {
      if (!rangelen)
        break;
      //dlog(">> new best range found: start=%zu len=%zu", start, rangelen);
      best_start = start;
      best_len = rangelen;
      found = USIZE_MAX;
    }
    start += rangelen;
  }
  *startp = best_start & found;
  return minlen & found;
}


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

  h->chunk_cap = chunk_cap;
  h->chunk_len = 0;
  h->chunks = p;
  bitset_init(&h->chunk_use, p + (h->chunk_cap * CHUNK_SIZE), h->chunk_cap);

  // // branchless approximation. Spills ~28 kiB for a 2 MiB memory size (~1.2%).
  // h->chunk_cap = size / (CHUNK_SIZE + 1);
  // h->chunk_len = 0;
  // h->chunks = p;
  // bitset_init(&h->chunk_use, p + (h->chunk_cap * CHUNK_SIZE), h->chunk_cap);
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

  dlog("[heap] allocating %p (%zu B) in %zu chunks [%zu…%zu)",
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


static void kmem_add_subheap(void* storage, usize size) {
  trace("add subheap %p (%zu B)", storage, size);
  assert(size >= PAGE_SIZE*2);
  // place subheap struct in first page and give it the rest to use for its memory
  subheap_t* sh = storage;
  subheap_init(sh, storage + PAGE_SIZE, size - PAGE_SIZE);
  ilist_append(&g_subheaps, &sh->list_entry);
}


#if DEBUG
  UNUSED static void kmem_debug_dump_state(
    void* nullable highlight_p, usize highlight_size)
  {
    RHMutexLock(&g_lock);
    usize i = 0;
    ilist_for_each(lent, &g_subheaps) {
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
    RHMutexUnlock(&g_lock);
  }
#endif


// kmem_allocx attempts to allocate *size bytes.
// On success, *size is updated to its actual size.
void* nullable kmem_allocx(usize* size, usize alignment) {
  assertf(IS_POW2(alignment), "alignment %zu is not a power-of-two", alignment);
  assert(alignment <= PAGE_SIZE);

  void* ptr = NULL;

  RHMutexLock(&g_lock);
  assert(!g_expansion_in_progress);

  // TODO: g_slabheaps

  // attempt to allocate space in a subheap
  ilist_for_each(lent, &g_subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    if ((ptr = subheap_alloc(sh, size, alignment)))
      goto end;
  }

  panic("TODO expand memory");

end:
  RHMutexUnlock(&g_lock);
  return ptr;
}


void* nullable kmem_alloc(usize size) {
  return kmem_allocx(&size, 1);
}

void* nullable kmem_alloc_aligned(usize size, usize alignment) {
  return kmem_allocx(&size, alignment);
}


void kmem_free(void* ptr) {
  assertnotnull(ptr);

  RHMutexLock(&g_lock);
  assert(!g_expansion_in_progress);

  ilist_for_each(lent, &g_subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    if (subheap_try_free(sh, ptr))
      goto end;
  }

  safecheckf(0, "kmem_free: invalid address %p", ptr);
end:
  RHMutexUnlock(&g_lock);
}


usize kmem_alloc_size(usize size) {
  assert(size > 0);
  return ALIGN2(size + sizeof(allochead_t), CHUNK_SIZE) - sizeof(allochead_t);
}


rerror kmem_init() {
  ilist_test();
  safecheckexpr( RHMutexInit(&g_lock) , true);

  memset(g_init_mem, 0, sizeof(g_init_mem));
  kmem_add_subheap(g_init_mem, sizeof(g_init_mem));

  assertf(
    (uintptr)g_init_mem == ALIGN2((uintptr)g_init_mem, PAGE_SIZE),
    "g_init_mem %p is not aligned to PAGE_SIZE (%u); expected %p",
    g_init_mem, PAGE_SIZE, (void*)ALIGN2((uintptr)g_init_mem, PAGE_SIZE));

  usize z = kmem_alloc_size(123);
  dlog("kmem_alloc_size(123) => %zu", z);

  void* p1, *p2, *p3, *p4, *p5;

  p1 = kmem_alloc(z - 3);
  dlog("kmem_alloc(%zu) => %p", z, p1);

  usize size2 = 100;
  p2 = kmem_allocx(&size2, 512);
  dlog("kmem_alloc_aligned(100,512) => %p (%p) %zu B",
    p2, (void*)ALIGN2((uintptr)p2,512), size2 );

  kmem_free(p1);
  kmem_free(p2);

  p3 = kmem_alloc(800);
  dlog("kmem_alloc(800) => %p", p3);
  kmem_free(p3);

  //                   1111
  //         01234567890123
  // chunks: ▒▒____▒▒__▒▒▒▒
  //         p1 p2 p3p4 p5
  p1 = kmem_alloc(CHUNK_SIZE*(BEST_FIT_THRESHOLD-2));
  p1 = kmem_alloc(CHUNK_SIZE);   // 0-2
  p2 = kmem_alloc(CHUNK_SIZE*3); // 2-6
  p3 = kmem_alloc(CHUNK_SIZE);   // 6-8
  p4 = kmem_alloc(CHUNK_SIZE);   // 8-10
  p5 = kmem_alloc(CHUNK_SIZE*3); // 10-14
  kmem_free(p2);
  kmem_free(p4);
  kmem_debug_dump_state(NULL, 0);
  // now, for a CHUNK_SIZE allocation,
  // the "best fit" allocation strategy should select chunks 8-10, and
  // the "first fit" allocation strategy should select chunks 2-4.
  g_heap_alloc_force_best_fit = true;
  p2 = kmem_alloc(CHUNK_SIZE);
  g_heap_alloc_force_best_fit = false;
  kmem_debug_dump_state(p2, CHUNK_SIZE);


  kmem_free(p5);
  kmem_free(p3);
  kmem_free(p2);
  kmem_free(p1);

  // rangealloc_t a = {0};

  // // allocate 1 MiB of memory pages
  // void* p = assertnotnull( osvmem_alloc(1024*1024) );

  log("——————————————————");
  return rerr_canceled;
}

