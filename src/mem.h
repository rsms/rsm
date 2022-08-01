// Memory API
// SPDX-License-Identifier: Apache-2.0
#pragma once
RSM_ASSUME_NONNULL_BEGIN

// PAGE_SIZE: size of one RSM virtual memory page, in bytes
#define PAGE_SIZE 4096u

static_assert(IS_POW2(PAGE_SIZE),       "PAGE_SIZE is not a power-of-two");
static_assert(PAGE_SIZE >= sizeof(u64), "PAGE_SIZE too small");
static_assert(PAGE_SIZE <= 65536,       "PAGE_SIZE too large");


//———————————————————————————————————————————————————————————————————————————————————————
// rmm_t - memory manager
// The memory manager owns and manages all of the host memory.
// Allocations are limited to pages (PAGE_SIZE).
// rmm_allocpages can only allocate a power-of-two number of pages per call.

typedef struct rmm_ rmm_t;

rmm_t* nullable rmm_create(void* memp, usize memsize);
void rmm_dispose(rmm_t*);
void* nullable rmm_allocpages(rmm_t*, usize npages);
void rmm_freepages(rmm_t* restrict mm, void* restrict ptr);
uintptr rmm_startaddr(const rmm_t*);
usize rmm_cap(const rmm_t* mm); // total capacity (number of pages)

// rmm_avail_total returns the total number of pages available to allocate
usize rmm_avail_total(rmm_t*);

// rmm_avail_maxregion returns the number of pages of the largest, free region
// of contiguous pages.
usize rmm_avail_maxregion(rmm_t*);

// rmm_allocpages_min performs a best-effort allocation; it attempts to allocate
// req_npages and if there's no contiguous region of that many pages, it tries to
// allocate one order less (req_npages << 1).
// min_npages is the smallest number of pages it will attempt to allocate.
// On success, req_npages is updated with the actual number of pages allocated.
void* nullable rmm_allocpages_min(rmm_t* mm, usize* req_npages, usize min_npages);

// DEPRECATED (use rmemalloc_t); TODO remove
inline static void* nullable rmm_allocpages_bytes(rmm_t* mm, usize minsize) {
  return rmm_allocpages(mm, ALIGN2(minsize, PAGE_SIZE) / PAGE_SIZE);
}


//———————————————————————————————————————————————————————————————————————————————————————
// rmemalloc_t - universal memory allocator
typedef struct rmemalloc_ rmemalloc_t;

// rmem2_t describes a memory region
typedef struct {
  void* nullable start;
  usize          size;
} rmem2_t;

// RMEM_FMT is used for printf formatting of a rmem2_t
#define RMEM_FMT "%p…%p(%zu)"
#define RMEM_FMT_ARGS(region) \
  (region).start, ((region).start + (region).size), (region).size

// TODO: throw away old shitty rmem allocator,
// replace all uses with this one and rename this one to rmem.

// kmem_allocator_create creates a new allocator that sources memory from mm.
// initsize is the desired initial memory and can be zero.
// initsize is rounded up to nearest min(CHUNK_SIZE, pow2(initsize)) size
// where CHUNK_SIZE = sizeof(void*) * 8. Since backing memory has alignment
// requirements, the actual memory available may be different than the requested
// initsize; use kmem_cap to get the actual capacity.
rmemalloc_t* nullable kmem_allocator_create(rmm_t* mm, usize initsize);

// kmem_allocator_free disposes of an allocator.
// The allocator is invalid after this call.
void kmem_allocator_free(rmemalloc_t*);

// kmem_alloc_aligned allocates size bytes with specific address alignment.
// The returned address will have a _minimum_ alignment of 'alignment'.
// Allocations less than CHUNK_SIZE are rounded up and aligned to nearest upper pow2.
// Eg. size=24 is rounded up to 32 and has a minimum of 32B alignment.
// Allocations >= CHUNK_SIZE are sized in CHUNK_SIZE steps with a minimum alignment
// of CHUNK_SIZE. Eg. size=130,alignment=16 returns 256 bytes with CHUNK_SIZE alignment.
// Returns .start==NULL if the allocator is out of memory.
rmem2_t kmem_alloc_aligned(rmemalloc_t*, usize size, usize alignment);

// kmem_alloc allocates size bytes with sizeof(void*) alignment.
// Returns .start==NULL if the allocator is out of memory.
inline static rmem2_t kmem_alloc(rmemalloc_t* m, usize size) {
  return kmem_alloc_aligned(m, size, 1);
}

// kmem_free frees a memory region allocated by the same allocator
void kmem_free(rmemalloc_t*, rmem2_t);

// kmem_resize grows or shrinks the size of an allocated memory region to newsize.
// If resizing fails, false is returned and the region is unchanged; it is still valid.
bool kmem_resize(rmemalloc_t*, rmem2_t*, usize newsize);

// kmem_alloc_size returns the effective size of an allocation of that size.
// E.g. kmem_alloc_size(size) == kmem_alloc(a, size).size
usize kmem_alloc_size(usize);

// kmem_avail returns the total number of bytes available to allocate
usize kmem_avail(rmemalloc_t*);

// kmem_cap returns the total number of bytes managed by the allocator
usize kmem_cap(rmemalloc_t*);


//———————————————————————————————————————————————————————————————————————————————————————
RSM_ASSUME_NONNULL_END
