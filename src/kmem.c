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

void kmem_free(void* ptr, usize size);

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

#define trace(fmt, args...) dlog("[kmalloc] " fmt, ##args)

// CHUNK_SIZE: allocation chunk size, in bytes (must be a power of two)
#if UINTPTR_MAX < 0xffffffffffffffff
  #define CHUNK_SIZE 32u
#else
  #define CHUNK_SIZE 64u
#endif

#define KMALLOC_INIT_SIZE (2u * MiB)


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


// bitset_find_first_fit searches for the firts hole that is >=minlen large.
//   start: #bit to start the search at.
//   maxlen: search ranges up to this size
// Returns >=minlen if a range was found and updates start.
// Returns 0 if no range large enough was found (may still update start.)
static usize bitset_find_first_fit(
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

  assert(maxlen >= minlen);

  // We'll work with a register sized granule ("bucket") over the bitset
  usize bucket_bits = 8 * sizeof(usize); // bit size of one "bucket"
  usize* buckets = (usize*)bset->data;

  const usize start_bucket = *startp / bucket_bits; // bucket index to start with
  const usize end_bucket = bset->len / bucket_bits; // bucket index to stop before
  u8 start_bitoffs = (u8)(*startp % bucket_bits); // bit index to start at

  usize freelen = 0; // current "free range" length

  for (usize bucket = start_bucket; bucket < end_bucket; bucket++) {
    //dlog("bucket %zu %zx", bucket, buckets[bucket]);

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
      freelen += bucket_bits;
      if (freelen >= maxlen)
        return maxlen;
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
  return MIN(freelen, maxlen);
}


// bitset_find_best_fit searches the entire bitset to find the smallest hole
// that is >=minlen large
static isize bitset_find_best_fit(
  bitset_t* bset, usize* start, usize minlen, usize maxlen)
{
  panic("TODO bitset_find_best_fit");
  return -1;
}


static void heap_init(heap_t* h, u8* p, usize size) {
  h->chunk_cap = size / (CHUNK_SIZE + 1);
  h->chunk_len = 0;
  h->chunks = p;
  // put bitset at the end of memory region p
  bitset_init(&h->chunk_use, p + (h->chunk_cap * CHUNK_SIZE), h->chunk_cap);
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
  //
  // BEST_FIT_THRESHOLD: if the number of allocation chunks required are at least
  // these many, use a "best fit" search instead of a "first fit" search.
  #define BEST_FIT_THRESHOLD 128u
  usize ci = chunkalign_nz - ALLOCHEAD_NCHUNKS, clen;
  if (needchunks < BEST_FIT_THRESHOLD) {
    clen = bitset_find_first_fit(&h->chunk_use, &ci, needchunks, needchunks);
  } else {
    clen = bitset_find_best_fit(&h->chunk_use, &ci, needchunks, needchunks);
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


static void heap_free(heap_t* h, void* ptr, usize size) {
  (void)size; // silence "unused" warnings
  assert(heap_contains(h, ptr));
  allochead_t* a = ALLOCHEAD(ptr);

  assertf(size <= (a->nchunks * CHUNK_SIZE) - sizeof(allochead_t),
    "freeing memory %p that is smaller (%zu) than expected (%zu)",
    ptr, (a->nchunks * CHUNK_SIZE) - sizeof(allochead_t), size);

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


inline static void subheap_init(subheap_t* sh, u8* base, usize size) {
  heap_init(&sh->allocator, base, size);
}

inline static void* nullable subheap_alloc(subheap_t* sh, usize* size, usize alignment) {
  return heap_alloc(&sh->allocator, size, alignment);
}

inline static bool subheap_try_free(subheap_t* sh, void* ptr, usize size) {
  if (!heap_contains(&sh->allocator, ptr))
    return false;
  heap_free(&sh->allocator, ptr, size);
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


// kmem_allocx attempts to allocate *size bytes.
// On success, *size is updated to its actual size.
void* nullable kmem_allocx(usize* size, usize alignment) {
  assertf(IS_POW2(alignment), "alignment %zu is not a power-of-two", alignment);

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


void kmem_free(void* ptr, usize size) {
  assertnotnull(ptr);
  assert(size > 0);

  RHMutexLock(&g_lock);
  assert(!g_expansion_in_progress);

  ilist_for_each(lent, &g_subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    if (subheap_try_free(sh, ptr, size))
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

  void* p = kmem_alloc(z - 3);
  dlog("kmem_alloc(%zu) => %p", z, p);

  usize size2 = 100;
  void* p2 = kmem_allocx(&size2, 512);
  dlog("kmem_alloc_aligned(100,512) => %p (%p) %zu B",
    p2, (void*)ALIGN2((uintptr)p,512), size2 );

  kmem_free(p, z - 3);
  kmem_free(p2, size2);

  void* p3 = kmem_alloc(800);
  dlog("kmem_alloc(800) => %p", p3);

  // rangealloc_t a = {0};

  // // allocate 1 MiB of memory pages
  // void* p = assertnotnull( osvmem_alloc(1024*1024) );

  log("——————————————————");
  return rerr_canceled;
}

