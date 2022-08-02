// generic memory allocator
// SPDX-License-Identifier: Apache-2.0

#define ILIST_TEST_IMPL /* define ilist test in this translation unit */
#include "rsmimpl.h"
#include "list.h"
#include "bits.h"
#include "mem.h"

//
// This implements a generic heap allocator backed by a few small-size slabs
// and one or more subheaps. subheaps are in turn backed by pages from a
// memory manager (rmm_t).
//
// When no space is found for an allocation request that fits in a slab, a new slab is
// allocated from a subheap.
// When no space is found for an allocation request in a subheap, another
// subheap is allocated and added to the subheaps list.
//

// RMEM_TRACE: uncomment to enable logging a lot of info via dlog/
//#define RMEM_TRACE

// RMEM_RUN_TEST_ON_INIT: uncomment to run tests during exe init in DEBUG builds
#define RMEM_RUN_TEST_ON_INIT

// _SCRUB_BYTE defines a byte value that, upon allocating or freeing a region,
// memory is filled with (ie. memset(ptr,_SCRUB_BYTE,size).)
// This is useful for debugging memory management issues, like use after free, since
// memory managed by this allocator is not subject to host memory protection.
// Set to zero to disable scrubbing.
#if RSM_SAFE
  #ifndef RMEM_ALLOC_SCRUB_BYTE
    #define RMEM_ALLOC_SCRUB_BYTE 0xbb
  #endif
  #ifndef RMEM_FREE_SCRUB_BYTE
    #define RMEM_FREE_SCRUB_BYTE  0xaa
  #endif
#else
  #ifndef RMEM_ALLOC_SCRUB_BYTE
    #define RMEM_ALLOC_SCRUB_BYTE 0
  #endif
  #ifndef RMEM_FREE_SCRUB_BYTE
    #define RMEM_FREE_SCRUB_BYTE  0
  #endif
#endif

// RMEM_SLABHEAP_ENABLE: define to enable use of slabheaps; speeds up small allocations
#define RMEM_SLABHEAP_ENABLE

// RMEM_SLABHEAP_ENABLE_EAGER_ALLOC: define to allocate slab space up front
// Note: test_rmem assumes expects this to NOT be defined.
//#define RMEM_SLABHEAP_ENABLE_EAGER_ALLOC

// SLABHEAP_COUNT dictates the slabheap size classes in increasing pow2, starting
// with SLABHEAP_MIN_SIZE and ending with SLABHEAP_MAX_SIZE (inclusive.)
// E.g. SLABHEAP_MIN_SIZE=8 SLABHEAP_COUNT=4 means we'll have
// the following slabheaps: 8, 16, 32, 64  (bytes). SLABHEAP_COUNT=6 means we'll have
// the following slabheaps: 8, 16, 32, 64, 128, 256  (bytes). And so on.
#define SLABHEAP_COUNT       4
#define SLABHEAP_MIN_SIZE    sizeof(void*) /* must be pow2 */
#define SLABHEAP_MAX_ORDER   ((usize)(SLABHEAP_COUNT - 1) + ILOG2(SLABHEAP_MIN_SIZE))
#define SLABHEAP_MAX_SIZE    (1lu << SLABHEAP_MAX_ORDER)
#define SLABHEAP_BLOCK_SIZE  ((usize)(PAGE_SIZE * 16lu))
#define SLABHEAP_BLOCK_MASK  (~(SLABHEAP_BLOCK_SIZE - 1))
#define SLABHEAP_MAX_BLOCKS  (SLABHEAP_BLOCK_SIZE / SLABHEAP_MIN_SIZE)

static_assert(IS_ALIGN2(SLABHEAP_BLOCK_SIZE, PAGE_SIZE), "");
static_assert(SLABHEAP_COUNT > 0, "undef RMEM_SLABHEAP_ENABLE instead");

#define HEAP_MIN_SIZE  (CHUNK_SIZE*2)
#define HEAP_ALIGN     CHUNK_SIZE
// HEAP_MAX_ALIGN: maximum alignment factor that heap_alloc can handle
#define HEAP_MAX_ALIGN  XMAX(PAGE_SIZE, SLABHEAP_BLOCK_SIZE)
static_assert(IS_POW2(HEAP_MAX_ALIGN), "");

#ifdef RMEM_SLABHEAP_ENABLE
  #define HEAP_IDEAL_ALIGN  SLABHEAP_BLOCK_SIZE
#else
  #define HEAP_IDEAL_ALIGN  HEAP_ALIGN
#endif

#define SAFECHECK_VALID_REGION(region) \
  safecheckf(RMEM_IS_VALID(region), "invalid region " RMEM_FMT, RMEM_FMT_ARGS(region))

// debug_id
#ifdef RMEM_TRACE
  #define DEBUG_ID_FIELD(NAME)   usize NAME;
  #define DEBUG_ID_PARAM         , usize debug_id
  #define DEBUG_ID_ARG           , debug_id
  #define DEBUG_ID_GEN_ARG(gen)  , ((gen)++)
  #define DEBUG_ID_ASSIGN(dst)   ((dst) = debug_id)
  #define DEBUG_ID_INIT(ptr, FIELD, initval) ((ptr)->FIELD = (initval))
#else
  #define DEBUG_ID_FIELD(NAME)
  #define DEBUG_ID_PARAM
  #define DEBUG_ID_ARG
  #define DEBUG_ID_GEN_ARG(gen)
  #define DEBUG_ID_ASSIGN(dst)               ((void)0)
  #define DEBUG_ID_INIT(ptr, FIELD, initval) ((void)0)
#endif


typedef struct {
  usize    chunk_cap; // total_chunks
  usize    chunk_len; // number of used (allocated) chunks
  u8*      chunks;
  bitset_t chunk_use; // a set bit means that chunk is in use; is allocated
  DEBUG_ID_FIELD(debug_id)
} heap_t;

typedef struct {
  heap_t  heap; // should be first to make debugging heap_t <> subheap_t easier
  ilist_t list_entry;
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

typedef struct rmemalloc_ {
  RHMutex lock;
  ilist_t subheaps;

  rmm_t* nullable mm;
  void* nullable  mm_origin; // address of mm pages, if allocator was allocated in mm

  #ifdef RMEM_SLABHEAP_ENABLE
  slabheap_t slabheaps[SLABHEAP_COUNT];
  #endif

  DEBUG_ID_FIELD(next_heap_debug_id)
} rmemalloc_t;


static_assert(sizeof(subheap_t) <= PAGE_SIZE, "");


// CHUNK_SIZE: allocation chunk size, in bytes (must be a power of two)
// All subheap allocations are at least CHUNK_SIZE large.
// All subheap allocations are aligned to CHUNK_SIZE addresses.
#if UINTPTR_MAX < 0xffffffffffffffff
  #define CHUNK_SIZE 32u
#else
  #define CHUNK_SIZE 64u
#endif


// BEST_FIT_THRESHOLD: if the number of allocation chunks required are at least
// these many, use a "best fit" search instead of a "first fit" search.
// Used by rmem_alloc.
#define BEST_FIT_THRESHOLD 128u


#if defined(RMEM_TRACE) && defined(DEBUG)
  #define trace(fmt, args...) dlog("[rmem] " fmt, ##args)
#else
  #ifdef RMEM_TRACE
    #warning RMEM_TRACE has no effect unless DEBUG is enabled
    #undef RMEM_TRACE
  #endif
  #define trace(...) ((void)0)
#endif


#define CHUNK_MASK  (~((uintptr)CHUNK_SIZE - 1))


#if DEBUG
  UNUSED static void heap_debug_dump_state(
    heap_t* h, void* nullable highlight_p, usize highlight_size)
  {
    // if (h->chunk_len == 0) {
    //   fputs("(empty)\n", stderr);
    //   return;
    // }
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


// heap_init initializes heap h with memory at p of size bytes
static void heap_init(heap_t* h, u8* p, usize size  DEBUG_ID_PARAM) {
  assertf(size >= HEAP_MIN_SIZE, "%zu", size);
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
  // This is the highest split we can use, for the smallest size HEAP_MIN_SIZE.
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
  DEBUG_ID_ASSIGN(h->debug_id);
  trace("[heap %zu] init (chunk_cap %zu)", h->debug_id, chunk_cap);
  bitset_init(&h->chunk_use, p + (chunk_cap * CHUNK_SIZE), chunk_cap);
}


// heap_contains returns true if h is the owner of allocation at ptr
static bool heap_contains(const heap_t* h, void const* ptr, usize size) {
  uintptr max_addr = (uintptr)h->chunks + (h->chunk_cap * CHUNK_SIZE);
  return ((uintptr)ptr >= (uintptr)h->chunks) & ((uintptr)ptr + size <= max_addr);
}


inline static usize heap_avail(const heap_t* h) {
  return (h->chunk_cap - h->chunk_len) * CHUNK_SIZE;
}


inline static usize heap_cap(const heap_t* h) {
  return h->chunk_cap * CHUNK_SIZE;
}


// heap_alloc finds space in the heap h that is at least *sizep bytes.
// Returns NULL if there's no space, otherwise it returns a pointer to the allocated
// region and updates *sizep to the effective byte size of the region.
static void* nullable heap_alloc(heap_t* h, usize* sizep, usize alignment) {
  // nchunks: the number of chunks we need.
  // We add an extra once since integer division rounds down but we need
  // the "ceiling", enough chunks to fit meta_size and *sizep.
  usize nchunks = (*sizep + CHUNK_SIZE - 1) / CHUNK_SIZE;

  // chunks_align: alignment requirement of chunk range
  usize chunks_align = alignment / CHUNK_SIZE;
  chunks_align += !chunks_align; // branchless "if (x==0) x = 1"

  // dlog("size          %5zu", *sizep);
  // dlog("alignment     %5zu", alignment);
  // dlog("nchunks       %5zu", nchunks);
  // dlog("chunks_align  %5zu", chunks_align);

  // chunk_index is the chunk we start searching
  usize chunk_index = 0;

  // Before we go look for a range of free chunks,
  // exit early if the number of available chunks are less than what's needed
  if (h->chunk_cap - h->chunk_len < nchunks + chunk_index)
    return NULL;

  // chunk_len will contain the number of consecutive chunks found
  usize chunk_len;

  // Now we will search for a free range in the "chunks in use" bitset h->chunk_use
  //heap_debug_dump_state(h, NULL, 0);
  if (nchunks < BEST_FIT_THRESHOLD) {
    chunk_len = bitset_find_first_fit(&h->chunk_use, &chunk_index, nchunks, chunks_align);
  } else {
    chunk_len = bitset_find_best_fit(&h->chunk_use, &chunk_index, nchunks, chunks_align);
  }

  // Give up if we didn't find a range of chunks large enough
  if (chunk_len == 0)
    return NULL;

  // We found a range of free chunks!
  // Update the bitset to mark the chunks as "in use"
  bitset_set_range(&h->chunk_use, chunk_index, chunk_len, true);

  // Increment total number of chunks "in use" in the heap
  h->chunk_len += chunk_len;

  // chunk1 is the address of the first chunk.
  void* ptr = h->chunks + (chunk_index * CHUNK_SIZE);
  assertf(IS_ALIGN2((uintptr)ptr, alignment),
    "bug in %s (ptr %p, alignment %zu)", __FUNCTION__, ptr, alignment);

  // Return back to the caller the actual usable size of the allocation
  // dlog("req. size     %5zu", *sizep);
  // dlog("usable size   %5zu", chunk_len * CHUNK_SIZE);
  assert(chunk_len * CHUNK_SIZE >= *sizep);
  *sizep = chunk_len * CHUNK_SIZE;

  // fill allocated memory with scrub bytes (if enabled)
  if (RMEM_ALLOC_SCRUB_BYTE)
    memset(ptr, RMEM_ALLOC_SCRUB_BYTE, chunk_len * CHUNK_SIZE);

  trace("[heap %zu] allocating %p (%zu B) in %zu chunks [%zu…%zu)",
    h->debug_id, ptr, *sizep, chunk_len, chunk_index, chunk_index + chunk_len);

  return ptr;
}


static void heap_free(heap_t* h, void* ptr, usize size) {
  assert(heap_contains(h, ptr, size));

  // calculate chunk index for the allocation
  uintptr chunk_addr = (uintptr)ptr & CHUNK_MASK;
  uintptr chunk_index = (chunk_addr - (uintptr)h->chunks) / CHUNK_SIZE;
  usize chunk_len = size / CHUNK_SIZE;

  trace("[heap %zu] freeing " RMEM_FMT " in %zu chunks [%zu…%zu)",
    h->debug_id, RMEM_FMT_ARGS(RMEM(ptr, size)),
    chunk_len, chunk_index, chunk_index + chunk_len);

  assertf(bitset_get(&h->chunk_use, chunk_index),
    "trying to free already-free region starting at chunk %zu (ptr=%p)",
    chunk_index, ptr);

  bitset_set_range(&h->chunk_use, chunk_index, chunk_len, 0);

  assert(h->chunk_len >= chunk_len);
  h->chunk_len -= chunk_len;

  // fill freed memory with scrub bytes, if enabled
  if (RMEM_FREE_SCRUB_BYTE)
    memset(ptr, RMEM_FREE_SCRUB_BYTE, size);
}


inline static void subheap_init(subheap_t* sh, u8* base, usize size  DEBUG_ID_PARAM) {
  heap_init(&sh->heap, base, size  DEBUG_ID_ARG);
}

inline static void* nullable subheap_alloc(subheap_t* sh, usize* size, usize alignment) {
  return heap_alloc(&sh->heap, size, alignment);
}

inline static usize subheap_avail(const subheap_t* sh) {
  return heap_avail(&sh->heap);
}

inline static usize subheap_cap(const subheap_t* sh) {
  return heap_cap(&sh->heap);
}


static bool rmem_add_subheap(rmemalloc_t* a, void* storage, usize size) {
  if (size < ALIGN2(sizeof(subheap_t), _Alignof(subheap_t)) + HEAP_MIN_SIZE) {
    trace("[%s] size (%zu) too small", __FUNCTION__, size);
    return false;
  }

  subheap_t* sh;

  // If storage is aligned, place subheap struct at the end to minimize spill
  if (IS_ALIGN2((uintptr)storage, HEAP_MAX_ALIGN)) {
    uintptr end_addr = (uintptr)storage + size;
    sh = (subheap_t*)ALIGN2_FLOOR(end_addr - sizeof(subheap_t), _Alignof(subheap_t));
    if (check_sub_overflow((usize)(uintptr)sh, (usize)(uintptr)storage, &size)) {
      trace("not enough space at %p…%p for %lu B alignment while adding subheap",
        storage, (void*)end_addr, HEAP_MAX_ALIGN);
      return false;
    }
    assert((uintptr)storage + size <= (uintptr)sh);
  } else {
    // Otherwise we place the subheap struct at the beginning and align storage
    assert(IS_ALIGN2((uintptr)storage, _Alignof(subheap_t)));
    sh = storage;
    usize halign = (size - sizeof(subheap_t) >= HEAP_MAX_ALIGN) ?
      HEAP_MAX_ALIGN : HEAP_ALIGN;
    usize size_diff;
    void* newstorage = storage;
    usize newsize;
    for (;;) {
      trace("try storage in tail with %lu B alignment for new subheap", halign);
      newstorage = (subheap_t*)ALIGN2((uintptr)storage + sizeof(subheap_t), halign);
      size_diff = (usize)((uintptr)newstorage - (uintptr)sh);
      if (!check_sub_overflow(size, size_diff, &newsize))
        break;
      if (HEAP_MAX_ALIGN > HEAP_ALIGN && halign > HEAP_ALIGN) {
        // try with smaller alignment (slabheaps will fail to grow)
        halign = HEAP_ALIGN;
        continue;
      }
      // give up
      trace("not enough space at %p (%p…%p) for %lu B alignment while adding subheap",
        newstorage, storage, storage + size, halign);
      return false;
    }
    size = newsize;
    storage = newstorage;
    if (size_diff > 0)
      trace("forfeiting %zu kiB in subheap (%zu B alignment)", size_diff / kiB, halign);
  }

  if (size < HEAP_MIN_SIZE) {
    trace("size (%zu) too small after alignment while adding subheap", size);
    return false;
  }

  assertf(IS_ALIGN2((uintptr)storage, HEAP_ALIGN), "%p", storage);

  subheap_init(sh, storage, size  DEBUG_ID_GEN_ARG(a->next_heap_debug_id));
  ilist_append(&a->subheaps, &sh->list_entry);

  trace("added [heap %zu] owning %p…%p (%zu B, %.1f pages)",
    sh->heap.debug_id, storage, storage + size, size, (double)size / (double)PAGE_SIZE);

  return true;
}


#if DEBUG
  UNUSED static void rmem_debug_dump_state(
    rmemalloc_t* a, void* nullable highlight_p, usize highlight_size)
  {
    RHMutexLock(&a->lock);
    usize i = 0;
    ilist_for_each(lent, &a->subheaps) {
      subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
      heap_t* h = &sh->heap;
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


static void* nullable rmem_heapalloc(rmemalloc_t* a, usize* size, usize alignment) {
  ilist_for_each(lent, &a->subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    void* ptr = subheap_alloc(sh, size, alignment);
    if (ptr)
      return ptr;
  }
  return NULL;
}


static bool rmem_expand(rmemalloc_t* a, usize minsize);
static bool free_to_subheaps(rmemalloc_t* a, void* ptr, usize size);


#ifdef RMEM_SLABHEAP_ENABLE


static usize slabheap_avail(const slabheap_t* sh) {
  usize nbyte = 0;
  for (const slabblock_t* block = sh->usable; block; block = block->next) {
    nbyte += block->len * sh->size;
    for (const slabchunk_t* chunk = block->recycle; chunk; chunk = chunk->next)
      nbyte += sh->size;
  }
  return nbyte;
}


static usize slabheap_purge(rmemalloc_t* a, slabheap_t* sh) {
  usize nblocks_purged = 0;

  // scan the usable-blocks list for unused blocks and free them
  for (slabblock_t* block = sh->usable; block; ) {
    usize nchunk_allocated = block->len;

    for (slabchunk_t* chunk = block->recycle; chunk; chunk = chunk->next)
      nchunk_allocated--;

    slabblock_t* b = block;
    block = block->next;

    if (nchunk_allocated == 0) {
      trace("[slab %zu] purge block %p", sh->size, b);
      UNUSED bool ok = free_to_subheaps(a, b, SLABHEAP_BLOCK_SIZE);
      assert(ok);
      nblocks_purged++;
    }
  }

  return nblocks_purged * SLABHEAP_BLOCK_SIZE;
}


static slabblock_t* nullable slabheap_grow(rmemalloc_t* a, slabheap_t* sh) {
  trace("[slab %zu] grow", sh->size);
  assertnull(sh->usable);

  usize size = SLABHEAP_BLOCK_SIZE;
  static_assert(_Alignof(slabblock_t) <= SLABHEAP_MIN_SIZE, "");

  slabblock_t* block;
  for (usize attempt = 10; ; attempt++) {
    if LIKELY(( block = rmem_heapalloc(a, &size, SLABHEAP_BLOCK_SIZE) ))
      break;
    size = SLABHEAP_BLOCK_SIZE;
    trace("[slab %zu] rmem_heapalloc(%zu, %zu) failed; trying rmem_expand",
      sh->size, size, SLABHEAP_BLOCK_SIZE);
    if UNLIKELY(!rmem_expand(a, size) || attempt == 0)
      return NULL;
  }

  trace("[slab %zu] allocated backing block %p", sh->size, block);
  assertf((uintptr)block % SLABHEAP_BLOCK_SIZE == 0,
    "misaligned address %p returned by rmem_heapalloc", block);

  usize block_align = CEIL_POW2(MAX(sh->size, _Alignof(slabblock_t)));
  usize block_size = ALIGN2(sizeof(slabblock_t), block_align);
  block->cap = (SLABHEAP_BLOCK_SIZE - block_size) / sh->size;
  block->len = 0;
  block->recycle = NULL;
  block->next = NULL;
  sh->usable = block; // set as "usable" list
  return block;
}


static void* nullable slabheap_alloc(rmemalloc_t* a, slabheap_t* sh) {
  slabblock_t* block = sh->usable;

  // If there are no usable blocks, attempt to allocate a new one
  if UNLIKELY(block == NULL) {
    if (!(block = slabheap_grow(a, sh)))
      return NULL;
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
    trace("[slab %zu] mark block %p as full", sh->size, block);
    sh->usable = block->next;
    block->next = sh->full;
    sh->full = block;
  }

  #ifdef RMEM_TRACE
  uintptr data_addr = ALIGN2((uintptr)block + sizeof(slabblock_t), sh->size);
  trace("[slab %zu] allocating chunk %zu %p from block %p",
    sh->size, ((uintptr)chunk - data_addr) / sh->size, chunk, block);
  #endif

  return chunk;
}


static void slabheap_free(rmemalloc_t* a, slabheap_t* sh, void* ptr) {
  assertf( !((uintptr)ptr % sh->size), "invalid address %p (slab %zu)", ptr, sh->size);

  // fill freed memory with scrub bytes, if enabled
  if (RMEM_FREE_SCRUB_BYTE)
    memset(ptr, RMEM_FREE_SCRUB_BYTE, sh->size);

  slabblock_t* block = (slabblock_t*)((uintptr)ptr & SLABHEAP_BLOCK_MASK);

  bool block_full = block->recycle == NULL && block->len == block->cap;

  // add chunk to the block's recycle list
  slabchunk_t* chunk = ptr;
  chunk->next = block->recycle;
  block->recycle = chunk;

  #ifdef RMEM_TRACE
  uintptr data_addr = ALIGN2((uintptr)block + sizeof(slabblock_t), sh->size);
  trace("[slab %zu] freeing chunk %zu %p from block %p",
    sh->size, ((uintptr)ptr - data_addr) / sh->size, ptr, block);
  #endif

  // If the block was fully used, it no longer is and we need to
  // move it from the "full" list to the "usable" list.
  if (block_full) {
    trace("[slab %zu] mark block %p as usable", sh->size, block);
    sh->full = block->next;
    block->next = sh->usable;
    sh->usable = block;
  }
}


#endif // RMEM_SLABHEAP_ENABLE


// rmem_alloc_aligned attempts to allocate *size bytes.
// On success, *size is updated to its actual size.
rmem_t rmem_alloc_aligned(rmemalloc_t* a, usize size, usize alignment) {
  assertf(IS_POW2(alignment), "alignment %zu is not a power-of-two", alignment);
  assertf(alignment <= PAGE_SIZE, "%zu", alignment);

  if (size == 0)
    return (rmem_t){0};

  void* ptr = NULL;

  RHMutexLock(&a->lock);

  // Attempt to allocate space in a slabheap.
  // This succeeds for the common case of a small allocation size.
  #ifdef RMEM_SLABHEAP_ENABLE
    // overflow of slab_index is ok
    const usize slabsize = CEIL_POW2(ALIGN2(size, SLABHEAP_MIN_SIZE));
    usize slab_index = ILOG2(slabsize) - ILOG2(SLABHEAP_MIN_SIZE);
    //dlog("** %zu => slabsize %zu, slab_index %zu", size, slabsize, slab_index);
    if (slab_index < SLABHEAP_COUNT) {
      slabheap_t* slab = &a->slabheaps[slab_index];
      size = slab->size;
      if LIKELY((ptr = slabheap_alloc(a, slab)))
        goto end;
      // If we get here, that means we were unable to slabheap_grow,
      // which in turn means there's less than SLABHEAP_BLOCK_SIZE space available
      // from subheaps, including potential growth by requesting more pages from mm.
      // So what we do now is to fall back to subheap allocation for small "slab"
      // sized allocation. Essentially the same path as if slabheaps were disabled.
    }
  #endif

  // Attempt to allocate space in a subheap, retrying after successful expansion
  for (;;) {
    if LIKELY((ptr = rmem_heapalloc(a, &size, alignment)))
      break;
    if (!rmem_expand(a, size))
      break;
  }

end:
  RHMutexUnlock(&a->lock);
  trace("allocated region " RMEM_FMT, RMEM_FMT_ARGS(RMEM(ptr, size)));
  return (rmem_t){ .p=ptr, .size=size };
}


rmem_t rmem_must_alloc(rmemalloc_t* a, usize size) {
  rmem_t m = rmem_alloc(a, size);
  if UNLIKELY(!m.p)
    panic("out of memory");
  return m;
}


void rmem_must_resize(rmemalloc_t* a, rmem_t* m, usize newsize) {
  if (!rmem_resize(a, m, newsize))
    panic("out of memory");
}


rmem_t rmem_alloc_array(rmemalloc_t* a, usize count, usize elemsize, usize alignment) {
  if (check_mul_overflow(count, elemsize, &count))
    return (rmem_t){0};
  return rmem_alloc_aligned(a, count, alignment);
}


static bool free_to_subheaps(rmemalloc_t* a, void* ptr, usize size) {
  #ifdef RMEM_SLABHEAP_ENABLE
  assert(size > SLABHEAP_MAX_SIZE);
  #endif

  ilist_for_each(lent, &a->subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    if (heap_contains(&sh->heap, ptr, size)) {
      heap_free(&sh->heap, ptr, size);
      return true;
    }
  }
  return false;
}


void rmem_free(rmemalloc_t* a, rmem_t region) {
  SAFECHECK_VALID_REGION(region);

  RHMutexLock(&a->lock);

  #ifdef RMEM_SLABHEAP_ENABLE
    for (usize i = 0; i < SLABHEAP_COUNT; i++) {
      if (region.size <= a->slabheaps[i].size) {
        slabheap_free(a, &a->slabheaps[i], region.p);
        goto end;
      }
    }
  #endif

  if UNLIKELY(!free_to_subheaps(a, region.p, region.size))
    safecheckf(0, "rmem_free: invalid region " RMEM_FMT, RMEM_FMT_ARGS(region));

end:
  trace("freed region " RMEM_FMT, RMEM_FMT_ARGS(region));
  RHMutexUnlock(&a->lock);
}


bool rmem_resize(rmemalloc_t* a, rmem_t* region, usize newsize) {
  // Allow "resizing" zero region
  if (RMEM_IS_NULL(*region)) {
    *region = rmem_alloc(a, newsize);
    return region->p != NULL;
  }

  SAFECHECK_VALID_REGION(*region);

  // can't shrink to zero
  // TODO: should this be allowed as a weird way to free a region?
  safecheckf(newsize > 0, "attempted to resize %p to 0", region->p);

  // load old size and check for misaligned (invalid) region address
  usize oldsize = region->size;
  assertf(IS_ALIGN2(region->size, sizeof(void*)),
    "invalid size of memory region " RMEM_FMT, RMEM_FMT_ARGS(*region));

  // align newsize
  newsize = (
    #ifdef RMEM_SLABHEAP_ENABLE
      // slabs sizes are pow2
      (newsize <= SLABHEAP_MAX_SIZE) ? CEIL_POW2(newsize) :
    #endif
    ALIGN2(newsize, CHUNK_SIZE)
  );

  if (newsize == oldsize)
    return true;

  // The region could not be trivially resized; resort to actual resizing.
  trace("resizing %p  %zu -> %zu", region->p, oldsize, newsize);

  // FIXME replace this daft "allocate new, copy, free old" code
  usize alignment = MIN(CEIL_POW2(newsize), (usize)CEIL_POW2((uintptr)region->p));
  rmem_t new_region = rmem_alloc_aligned(a, newsize, alignment);
  if (!new_region.p)
    return false;

  // r1 |...........|
  // r2        |............|
  #define RMEM_IS_INTERSECTING(r1, r2) ( \
    (uintptr)(r1).p <= ((uintptr)(r2).p + (uintptr)(r2).size) && \
    (uintptr)(r2).p <= ((uintptr)(r1).p + (uintptr)(r1).size) \
  )

  assertf(!RMEM_IS_INTERSECTING(new_region, *region),
    "bug in rmem_alloc_aligned: allocated non-free memory."
    "\nThe following regions overlap:"
    "\n  new allocation  " RMEM_FMT
    "\n  non-free region " RMEM_FMT
    "\n",
    RMEM_FMT_ARGS(new_region),
    RMEM_FMT_ARGS(*region) );

  memcpy(new_region.p, region->p, MIN(oldsize, newsize));
  rmem_free(a, *region);
  *region = new_region;
  return true;

  // #ifdef RMEM_SLABHEAP_ENABLE
  //   if (oldsize <= SLABHEAP_MAX_SIZE) {
  //     panic("TODO slabheap resize");
  //   }
  // #endif
}


// rmem_expand attempts to expand the available memory to allocate.
// Returns true if expansion succeeded; the caller should try again to allocate.
static bool rmem_expand(rmemalloc_t* a, usize minsize) {

  // before we ask the memory manager for more pages, attempt to purge slabs
  #ifdef RMEM_SLABHEAP_ENABLE
    usize nbyte_purged = 0;
    for (usize i = 0; i < SLABHEAP_COUNT; i++) {
      nbyte_purged += slabheap_purge(a, &a->slabheaps[i]);
      if (nbyte_purged >= minsize)
        return true;
    }
  #endif

  // We were unable to purge enough slab space to satisfy minsize.
  // Let's try to allocate more pages from the memory manager.
  if (!a->mm)
    return false;

  // align minsize to page boundary
  #if DEBUG
    assert(!check_align_ceil_overflow(minsize, PAGE_SIZE, &minsize));
  #else
    minsize = ALIGN2(minsize, PAGE_SIZE);
  #endif

  // request twice as much memory as we need
  usize req_size; // req_size = try { minsize * 2 } on_overflow { minsize }
  if (check_mul_overflow(minsize, 2lu, &req_size))
    req_size = minsize;

  // Ideally we'd like to allocate memory in SLABHEAP_BLOCK_SIZE granules for
  // our slabs, which all require size _and_ alignment of SLABHEAP_BLOCK_SIZE
  // for their backing data.
  #ifdef RMEM_SLABHEAP_ENABLE
    usize req_size2;
    if (!check_align_ceil_overflow(req_size, SLABHEAP_BLOCK_SIZE, &req_size2)) {
      req_size = req_size2;
    } else {
      // overflowed; try using minsize instead
      if (!check_align_ceil_overflow(minsize, SLABHEAP_BLOCK_SIZE, &req_size2))
        req_size = req_size2;
      // else: that too overflowed; leave req_size aligned to page boundary
    }
  #endif

  usize req_npages = CEIL_POW2(req_size / PAGE_SIZE);
  usize min_npages = CEIL_POW2(minsize / PAGE_SIZE);
  trace("[expand] attempting to allocate %zu-%zu pages from mm", min_npages, req_npages);

  // try to allocate req_npages, going as low as min_npages if needed
  void* ptr = rmm_allocpages_min(a->mm, &req_npages, min_npages);
  if (!ptr) {
    trace("[expand] failed; mm was unable to find %zu contiguous pages", min_npages);
    return false;
  }

  trace("[expand] allocated %zu pages from mm: %p…%p(%zu)",
    req_npages, ptr, ptr + (req_npages * PAGE_SIZE), req_npages * PAGE_SIZE);

  if UNLIKELY(!rmem_add_subheap(a, ptr, req_npages * PAGE_SIZE)) {
    rmm_freepages(a->mm, ptr);
    return false;
  }

  return true;
}


usize rmem_alloc_size(usize size) {
  assert(size > 0);

  #ifdef RMEM_SLABHEAP_ENABLE
  if (size <= SLABHEAP_MAX_SIZE)
    return CEIL_POW2(size);
  #endif

  return ALIGN2(size, CHUNK_SIZE);
}


static rmemalloc_t* nullable rmem_allocator_init(
  rmemalloc_t*    a,
  void*           heap0p, usize heap0size, // initial heap storage
  rmm_t* nullable mm,
  void* nullable  mm_origin)
{
  assertf(((uintptr)a % (uintptr)_Alignof(rmemalloc_t)) == 0, "a %p misaligned", a);
  assertf(IS_ALIGN2((uintptr)heap0p, HEAP_ALIGN), "heap0p %p misaligned", heap0p);
  assertf(mm_origin == NULL || mm != NULL, "mm_origin without a mm");

  if (!RHMutexInit(&a->lock))
    return NULL;

  ilist_init(&a->subheaps);

  a->mm = mm;
  a->mm_origin = mm_origin;

  DEBUG_ID_INIT(a, next_heap_debug_id, 0);

  // initialize slab heaps, starting with size=sizeof(void*)
  // TODO: tune these sizes once we have some stats on usage.
  #ifdef RMEM_SLABHEAP_ENABLE
    for (usize i = 0; i < SLABHEAP_COUNT; i++) {
      a->slabheaps[i].size = 1lu << (i + ILOG2(SLABHEAP_MIN_SIZE));
      a->slabheaps[i].usable = NULL;
      a->slabheaps[i].full = NULL;
      trace("init slabheaps[%zu] (%zu B)", i, a->slabheaps[i].size);
    }
  #endif

  // initialize the first heap
  if UNLIKELY(!rmem_add_subheap(a, heap0p, heap0size)) {
    trace("failed to add initial subheap; not enough space and/or alignment too small");
    if (!mm)
      return NULL;
  } else {
    // allocate initial slab blocks up front, if enabled
    #if defined(RMEM_SLABHEAP_ENABLE) && defined(RMEM_SLABHEAP_ENABLE_EAGER_ALLOC)
      for (usize i = SLABHEAP_COUNT; i--; ) {
        if UNLIKELY(!slabheap_grow(a, &a->slabheaps[i])) {
          // We're out of memory, but don't do anything about it since we are
          // just optimistically allocating slab space here
          break;
        }
      }
    #endif
  }

  return a;
}


rmemalloc_t* nullable rmem_allocator_create(rmm_t* mm, usize req_heap0size) {
  // rmm requires page allocations in pow2 orders
  usize nbytes = sizeof(rmemalloc_t) + ALIGN_CEIL(req_heap0size, HEAP_ALIGN);
  usize npages = CEIL_POW2(ALIGN2(nbytes, PAGE_SIZE) / PAGE_SIZE);

  // minimum amount of memory pages we need
  const usize min_npages =
    ALIGN2(sizeof(rmemalloc_t) + HEAP_MIN_SIZE, PAGE_SIZE) / PAGE_SIZE;

  // try to allocate npages, going lower if needed, but no lower than min_npages
  void* p = rmm_allocpages_min(mm, &npages, min_npages);
  if (!p)
    return NULL;

  usize nbyte = npages * PAGE_SIZE;
  trace("creating allocator in %zu pages (%zu kiB, %.2f kiB usable)",
    npages,  nbyte / kiB,  (double)(nbyte - sizeof(rmemalloc_t)) / (double)kiB);

  // place the allocator at the end of the page range to increase the chances
  // of perfect alignment of the initial heap (which has HEAP_MAX_ALIGN alignment.)
  rmemalloc_t* a = (void*)( ((uintptr)p + nbyte) - sizeof(rmemalloc_t) );

  void* heap0p = p;
  usize heap0size = nbyte - sizeof(rmemalloc_t);
  void* mm_origin = p;

  return rmem_allocator_init(a, heap0p, heap0size, mm, mm_origin);
}


rmemalloc_t* nullable rmem_allocator_create_buf(
  rmm_t* nullable mm, void* buf, usize bufsize)
{
  if (bufsize < sizeof(rmemalloc_t) + sizeof(subheap_t))
    return NULL; // definitely too small

  // The following loop finds out...
  // - if it's better to put the allocator in the head or tail of buf
  // - what the largest alignment is that we can use (<=HEAP_IDEAL_ALIGN)
  const uintptr end_addr = (uintptr)buf + bufsize;

  usize halign = (bufsize - sizeof(rmemalloc_t) - sizeof(subheap_t) >= HEAP_MAX_ALIGN) ?
    HEAP_MAX_ALIGN : HEAP_ALIGN;

  uintptr head_a_addr, head_h_addr, head_h_size;
  uintptr tail_h_addr, tail_a_addr, tail_h_size;

  rmemalloc_t* a;
  void* heap0p = NULL;
  usize heap0size = 0;

  for (;;) {
    trace("[%s] try with halign %lu (size %zu B)", __FUNCTION__, halign, bufsize);

    head_a_addr = ALIGN2((uintptr)buf, _Alignof(rmemalloc_t));
    head_h_addr = ALIGN2(head_a_addr + sizeof(rmemalloc_t), halign);
    head_h_size = (end_addr >= head_h_addr) ?
      end_addr - head_h_addr : 0;

    tail_h_addr = ALIGN2((uintptr)buf, halign);
    tail_a_addr = ALIGN2_FLOOR(end_addr - sizeof(rmemalloc_t), _Alignof(rmemalloc_t));
    tail_h_size = (tail_a_addr > tail_h_addr) ?
      ALIGN2_FLOOR(tail_a_addr - tail_h_addr, CHUNK_SIZE) : 0;

    trace("[%s]   halign      %lu", __FUNCTION__, halign);
    trace("[%s]   head_a_addr 0x%lx", __FUNCTION__, head_a_addr);
    trace("[%s]   head_h_addr 0x%lx", __FUNCTION__, head_h_addr);
    trace("[%s]   head_h_size %lu", __FUNCTION__, head_h_size);
    trace("[%s]   tail_a_addr 0x%lx", __FUNCTION__, tail_a_addr);
    trace("[%s]   tail_h_addr 0x%lx", __FUNCTION__, tail_h_addr);
    trace("[%s]   tail_h_size %lu", __FUNCTION__, tail_h_size);

    if (tail_h_size == 0 && head_h_size == 0) {
      if (HEAP_IDEAL_ALIGN > HEAP_ALIGN && halign > HEAP_ALIGN) {
        halign = HEAP_ALIGN;
        continue;
      }
      trace("cannot create allocator in too-small buffer %p (%zu B)", buf, bufsize);
      return NULL;
    } else if (tail_h_size > head_h_size) {
      // go with tail placement
      a = (void*)tail_a_addr;
      heap0p = (void*)tail_h_addr;
      heap0size = (usize)tail_h_size;
      break;
    } else {
      // go with head placement
      a = (void*)head_a_addr;
      heap0p = (void*)head_h_addr;
      heap0size = (usize)head_h_size;
      break;
    }
  }

  return rmem_allocator_init(a, heap0p, heap0size, mm, NULL);
}


void rmem_allocator_free(rmemalloc_t* a) {
  // TODO: free slabheaps
  // TODO: free additional subheaps
  if (a->mm_origin)
    rmm_freepages(assertnotnull(a->mm), a->mm_origin);
}


usize rmem_avail(rmemalloc_t* a) {
  RHMutexLock(&a->lock);
  usize nbyte = 0;

  #ifdef RMEM_SLABHEAP_ENABLE
    for (usize i = 0; i < SLABHEAP_COUNT; i++) {
      nbyte += slabheap_avail(&a->slabheaps[i]);
    }
  #endif

  ilist_for_each(lent, &a->subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    nbyte += subheap_avail(sh);
  }

  RHMutexUnlock(&a->lock);
  return nbyte;
}


usize rmem_cap(rmemalloc_t* a) {
  RHMutexLock(&a->lock);

  usize nbyte = 0;

  // note: slabs are allocated in subheaps, so we don't need to count those
  ilist_for_each(lent, &a->subheaps) {
    subheap_t* sh = ilist_entry(lent, subheap_t, list_entry);
    nbyte += subheap_cap(sh);
  }

  RHMutexUnlock(&a->lock);
  return nbyte;
}


const char* rmem_scrubcheck(void* ptr, usize size) {
  if (RMEM_ALLOC_SCRUB_BYTE | RMEM_FREE_SCRUB_BYTE) {
    u8 scrub_bytes[128] ATTR_ALIGNED(sizeof(void*));

    size = MIN(size, sizeof(scrub_bytes));

    if (RMEM_ALLOC_SCRUB_BYTE) {
      memset(scrub_bytes, RMEM_ALLOC_SCRUB_BYTE, size);
      if (memcmp(ptr, scrub_bytes, size) == 0)
        return "uninit";
    }

    if (RMEM_FREE_SCRUB_BYTE) {
      memset(scrub_bytes, RMEM_FREE_SCRUB_BYTE, sizeof(scrub_bytes));
      if (memcmp(ptr, scrub_bytes, size) == 0)
        return "freed";
    }
  }

  return "ok";
}


#if defined(RMEM_RUN_TEST_ON_INIT) && DEBUG
static void test_rmem() {
  // verbose?
  //#define tlog dlog
  #ifndef tlog
    #ifdef RMEM_TRACE
      #define tlog dlog
    #else
      #define tlog(...) ((void)0)
    #endif
  #endif

  dlog("%s", __FUNCTION__);

  // test "not enough memory to create allocator"
  {
    usize allocator_size = ALIGN2(sizeof(rmemalloc_t), PAGE_SIZE);
    usize memsize = CEIL_POW2((allocator_size / PAGE_SIZE) + 8) * PAGE_SIZE;
    tlog("memsize %zu (%zu pages)", memsize, memsize / PAGE_SIZE);
    void* memp = assertnotnull( osvmem_alloc(memsize) );
    rmm_t* mm = assertnotnull( rmm_create(memp, memsize) );

    usize npages = rmm_avail_total(mm);
    while (npages--)
      assertnotnull( rmm_allocpages(mm, 1) );

    assertnull( rmem_allocator_create(mm, 0) );

    osvmem_free(memp, memsize);
    rmm_dispose(mm);
  }

  // test "not enough memory"
  {
    tlog("————————————————————————————————");
    usize allocator_size = ALIGN2(sizeof(rmemalloc_t), PAGE_SIZE);
    usize memsize = CEIL_POW2((allocator_size / PAGE_SIZE) + 8) * PAGE_SIZE;
    void* memp = assertnotnull( osvmem_alloc(memsize) );
    rmm_t* mm = assertnotnull( rmm_create(memp, memsize) );

    // allocate all pages but what's required for the allocator
    // (must allocate one at a time since rmm_allocpages needs pow2(count))
    usize npages = rmm_avail_total(mm) - (allocator_size / PAGE_SIZE);
    while (npages--)
      assertnotnull( rmm_allocpages(mm, 1) );

    rmemalloc_t* a = assertnotnull( rmem_allocator_create(mm, PAGE_SIZE) );

    // should have less than a page of memory, not enough for slabs or really
    // anything useful
    assertf(rmem_avail(a) < PAGE_SIZE, "rmem_avail(a) => %zu", rmem_avail(a));

    rmem_allocator_free(a);
    osvmem_free(memp, memsize);
    rmm_dispose(mm);
  }

  // test "out of memory -> expand by purging slab"
  {
    tlog("————————————————————————————————");
    usize memsize = (SLABHEAP_BLOCK_SIZE * 2) + PAGE_SIZE*3;
    void* memp = assertnotnull( osvmem_alloc(memsize) );
    rmm_t* mm = assertnotnull( rmm_create(memp, memsize) );

    // create an allocator
    rmemalloc_t* a = rmem_allocator_create(mm, rmm_avail_maxregion(mm) * PAGE_SIZE);
    assertnotnull(a);
    //tlog("rmem_cap / _avail  %zu / %zu", rmem_cap(a), rmem_avail(a));

    // allocate a small piece to trigger creation of a slab
    rmem_t mem = rmem_alloc(a, 1); assertnotnull(mem.p);
    rmem_free(a, mem);

    // should expand available memory
    mem = rmem_alloc(a, rmem_avail(a) + 1);
    assertnotnull(mem.p);

    rmem_allocator_free(a);
    rmm_dispose(mm);
    osvmem_free(memp, memsize);
  }

  // test "expand fails (out of memory) by requesting more pages from mm"
  {
    tlog("————————————————————————————————");
    usize memsize = (SLABHEAP_BLOCK_SIZE * 2) + PAGE_SIZE*3;
    void* memp = assertnotnull( osvmem_alloc(memsize) );
    rmm_t* mm = assertnotnull( rmm_create(memp, memsize) );

    // create an allocator
    rmemalloc_t* a = rmem_allocator_create(mm, rmm_avail_maxregion(mm) * PAGE_SIZE);
    assertnotnull(a);

    // should attempt to expand available memory, but mm should be out of memory
    assertnull( rmem_alloc(a, rmem_avail(a) + 1).p );

    rmem_allocator_free(a);
    rmm_dispose(mm);
    osvmem_free(memp, memsize);
  }

  // test "expand succeeds by requesting more pages from mm"
  {
    tlog("————————————————————————————————");
    usize memsize = (SLABHEAP_BLOCK_SIZE * 16) + PAGE_SIZE*3;
    void* memp = assertnotnull( osvmem_alloc(memsize) );
    rmm_t* mm = assertnotnull( rmm_create(memp, memsize) );

    // create an allocator
    rmemalloc_t* a = rmem_allocator_create(mm, SLABHEAP_BLOCK_SIZE * 2);
    assertnotnull(a);

    // should expand available memory by adding another subheap backed by mm
    rmem_t mem = rmem_alloc(a, rmem_avail(a) + 1);
    assertnotnull(mem.p);

    rmem_allocator_free(a);
    rmm_dispose(mm);
    osvmem_free(memp, memsize);
  }

  { // invalid regions
    rmem_t null_region = { .p=NULL, .size=8 };
    rmem_t empty_region = { .p=&null_region, .size=0 };
    rmem_t overflow_region = { .p=(void*)(~(uintptr)0)-16, .size=17 };

    assert(!RMEM_IS_VALID(null_region));
    assert(!RMEM_IS_VALID(empty_region));
    assert(!RMEM_IS_VALID(overflow_region));
    // freeing an invalid region panics (in safe mode only)
    // rmem_free(a, null_region);
  }

  { // rmem_alloc_size returns aligned allocation sizes
    usize heap_min_size = MAX(CEIL_POW2(SLABHEAP_MAX_SIZE + 1), CHUNK_SIZE);

    #ifdef RMEM_SLABHEAP_ENABLE
    assert( rmem_alloc_size(SLABHEAP_MIN_SIZE - 2) == SLABHEAP_MIN_SIZE );
    assert( rmem_alloc_size(SLABHEAP_MIN_SIZE + 2) == CEIL_POW2(SLABHEAP_MIN_SIZE + 1) );
    assert( rmem_alloc_size(SLABHEAP_MAX_SIZE - 2) == SLABHEAP_MAX_SIZE );
    assert( rmem_alloc_size(SLABHEAP_MAX_SIZE + 2) == heap_min_size );
    #endif

    assert( rmem_alloc_size(heap_min_size - 2) == heap_min_size );
    assert( rmem_alloc_size(heap_min_size + CHUNK_SIZE + 1)
            == heap_min_size + CHUNK_SIZE*2 );
  }

  // create a memory manager
  tlog("————————————————————————————————");
  usize memsize = 16 * MiB;
  rmm_t* mm = rmm_create(assertnotnull( osvmem_alloc(memsize) ), memsize);

  // create an allocator with ~4MiB initial memory
  rmemalloc_t* a = assertnotnull( rmem_allocator_create(mm, 4 * MiB) );

  { // resize a slab allocation
    rmem_t m, old;

    // canary data
    u8 expected_data[SLABHEAP_MIN_SIZE];
    memset(expected_data, 0xab, sizeof(expected_data));

    // initial allocation
    m = rmem_alloc(a, SLABHEAP_MIN_SIZE * 2); // 2nd order slab
    memcpy(m.p, expected_data, sizeof(expected_data));

    // shrink by 2B: noop (will update size)
    old = m;
    assert( rmem_resize(a, &m, (SLABHEAP_MIN_SIZE * 2) - 2 ) );
    assert( m.p == old.p );
    assert( m.size == SLABHEAP_MIN_SIZE * 2 );
    assert( memcmp(m.p, expected_data, sizeof(expected_data)) == 0 );

    // shrink
    old = m;
    assert( rmem_resize(a, &m, SLABHEAP_MIN_SIZE) );
    assert( m.size < old.size );
    assert( memcmp(m.p, expected_data, sizeof(expected_data)) == 0 );

    // grow
    old = m;
    assert( rmem_resize(a, &m, SLABHEAP_MIN_SIZE * 2) );
    assert( m.size > old.size );
    assert( memcmp(m.p, expected_data, sizeof(expected_data)) == 0 );

    rmem_free(a, m);
  }

  { // resize a subheap allocation
    rmem_t m, old;

    // canary data
    u8 expected_data[CHUNK_SIZE];
    memset(expected_data, 0xab, sizeof(expected_data));

    // initial allocation (force subheap use, instead of slabs)
    m = rmem_alloc(a, MAX(SLABHEAP_MAX_SIZE + 1, CHUNK_SIZE*2));
    memcpy(m.p, expected_data, sizeof(expected_data));

    // shrink (noop)
    old = m;
    assert( rmem_resize(a, &m, old.size - sizeof(void*)) );
    assert( m.p == old.p );
    assert( m.size == old.size );
    assert( memcmp(m.p, expected_data, sizeof(expected_data)) == 0 );

    // shrink
    old = m;
    assert( rmem_resize(a, &m, old.size - CHUNK_SIZE) );
    assert( m.size < old.size );
    assert( memcmp(m.p, expected_data, sizeof(expected_data)) == 0 );

    // grow
    old = m;
    assert( rmem_resize(a, &m, old.size + sizeof(void*)) );
    assert( m.size > old.size );
    assert( memcmp(m.p, expected_data, sizeof(expected_data)) == 0 );

    rmem_free(a, m);
  }


  rmem_t p1, p2, p3, p4, p5;

  // slabheap
  rmem_t regions[SLABHEAP_BLOCK_SIZE / 64];
  // rmem_t regions[4];
  for (usize i = 0; i < countof(regions); i++)
    regions[i] = rmem_alloc(a, 64);
  for (usize i = 0; i < countof(regions); i++)
    rmem_free(a, regions[i]);
  // push it over the limit
  p1 = rmem_alloc(a, 64);
  rmem_free(a, p1);

  p1 = rmem_alloc(a, 120);
  tlog("rmem_alloc(%u) => " RMEM_FMT, 120u, RMEM_FMT_ARGS(p1));

  usize req_size = 100;
  p2 = rmem_alloc_aligned(a, req_size, 512);
  tlog("rmem_alloc_aligned(%zu,512) => " RMEM_FMT " (expect %p)",
    req_size, RMEM_FMT_ARGS(p2), (void*)ALIGN2((uintptr)p2.p,512) );

  rmem_free(a, p1);
  rmem_free(a, p2);

  p3 = rmem_alloc(a, 800);
  tlog("rmem_alloc(800) => " RMEM_FMT, RMEM_FMT_ARGS(p3));
  rmem_free(a, p3);

  p1 = rmem_alloc(a, CHUNK_SIZE*(BEST_FIT_THRESHOLD-2));
  p1 = rmem_alloc(a, CHUNK_SIZE);   // 0-2
  p2 = rmem_alloc(a, CHUNK_SIZE*3); // 2-6
  p3 = rmem_alloc(a, CHUNK_SIZE);   // 6-8
  p4 = rmem_alloc(a, CHUNK_SIZE);   // 8-10
  p5 = rmem_alloc(a, CHUNK_SIZE*3); // 10-14
  rmem_free(a, p2);
  rmem_free(a, p4);
  // rmem_debug_dump_state(a, NULL, 0);
  // now, for a CHUNK_SIZE allocation,
  // the "best fit" allocation strategy should select chunks 8-10, and
  // the "first fit" allocation strategy should select chunks 2-4.

  p2 = rmem_alloc(a, CHUNK_SIZE);
  // rmem_debug_dump_state(a, p2, CHUNK_SIZE);
  rmem_free(a, p2);

  rmem_free(a, p5);
  rmem_free(a, p3);
  rmem_free(a, p1);

  rmem_allocator_free(a);
  rmm_dispose(mm);
  osvmem_free((void*)rmm_startaddr(mm), memsize);

  dlog("————————— END %s —————————", __FUNCTION__);
}
#endif // RMEM_RUN_TEST_ON_INIT


rerror init_rmem() {
  #if defined(RMEM_RUN_TEST_ON_INIT) && DEBUG
  test_rmem();
  #endif
  // currently nothing to initialize
  return 0;
}

