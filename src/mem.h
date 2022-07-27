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
// mm - memory manager
// The memory manager owns and manages all of the host memory with a PAGE_SIZE granule.

typedef struct {
  void* _internal[8];
} rmm_t;

rerror rmm_init(rmm_t* restrict mm, void* restrict memp, usize memsize);
void* nullable rmm_allocpages(rmm_t*, usize npages);
void rmm_freepages(rmm_t* restrict mm, void* restrict ptr, usize npages);

inline static void* nullable rmm_allocpages_bytes(rmm_t* mm, usize minsize) {
  return rmm_allocpages(mm, ALIGN2(minsize, PAGE_SIZE) / PAGE_SIZE);
}
inline static void rmm_freepages_bytes(rmm_t* mm, void* ptr, usize minsize) {
  rmm_freepages(mm, ptr, ALIGN2(minsize, PAGE_SIZE) / PAGE_SIZE);
}


RSM_ASSUME_NONNULL_END
