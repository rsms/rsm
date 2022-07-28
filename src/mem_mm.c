// memory manager
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "list.h"
#include "bits.h"
#include "mem.h"

#define MAX_ORDER      20
#define MIN_ALLOC_SIZE PAGE_SIZE
#define OBJ_MAXSIZE    (1ul * GiB)
static_assert((1ul << 30) == (1ul * GiB), "");

typedef struct rmm_ {
  RHMutex lock;
  uintptr start_addr; // host address range start (PAGE_SIZE aligned, read-only)
  uintptr end_addr;   // host address range end (exclusive, read-only)
  usize   free_size;  // number of free bytes (i.e. available to allocate)
  u8*     bitsets[MAX_ORDER + 1];
  ilist_t freelists[MAX_ORDER + 1];
  usize   nalloc[MAX_ORDER + 1]; // number of allocations per order
} rmm_t;


// static usize buddy_order_max_blocks(usize region_len, int order) {
//   return region_len / ((uintptr)PAGE_SIZE << order);
// }


usize rmm_cap_bytes(const rmm_t* mm) {
  return (usize)(mm->end_addr - mm->start_addr);
}

usize rmm_free_bytes(const rmm_t* mm) {
  return mm->free_size;
}


rmm_t* nullable rmm_emplace(void* memp, usize memsize) {
  uintptr start = ALIGN2((uintptr)memp, _Alignof(rmm_t));
  uintptr end_addr = (uintptr)memp + memsize;

  if (start + sizeof(rmm_t) >= end_addr) // TODO: coalesce these checks
    return NULL;

  rmm_t* mm = (rmm_t*)start;

  start += sizeof(rmm_t);
  memsize -= sizeof(rmm_t);

  // number of entries per bitmap
  usize nchunks = (memsize / PAGE_SIZE) / 8; //dlog("nchunks %zu", nchunks);

  // memory we will use for our bitsets
  usize net_bitset_size = (nchunks * 2) + (MAX_ORDER * 2);

  for(int order = 0; order < MAX_ORDER; order++) {
    mm->bitsets[order] = (void*)start;
    memset((void*)start, 0, nchunks + 2);
    start += nchunks + 2;
    ilist_init(&mm->freelists[order]);
  }

  // dlog("bitset storage: %lx ... %lx (%zu B)",
  //   start, start + net_bitset_size, net_bitset_size);

  start = ALIGN2(start + net_bitset_size, PAGE_SIZE);
  memsize = ALIGN2_FLOOR(memsize - net_bitset_size, PAGE_SIZE);

  mm->start_addr = start;
  mm->end_addr = start + memsize;
  mm->free_size = (usize)(mm->end_addr - mm->start_addr);

  memset(&mm->nalloc, 0, sizeof(mm->nalloc));

  dlog("mm range: %lx…%lx (%.0f MB in %lu pages)",
    mm->start_addr, mm->end_addr,
    (double)(mm->end_addr - mm->start_addr)/1024.0/1024.0,
    (mm->end_addr - mm->start_addr) / PAGE_SIZE);

  // TODO test & think more closely about this.
  // I'm not at all confident this isn't buggy.
  int max_order = 0;
  usize npages = memsize / PAGE_SIZE;
  while (npages) {
    max_order++;
    npages >>= 1;
  }
  ilist_append(&mm->freelists[max_order], (ilist_t*)start);

  return mm;
}


static void* nullable rmm_allocpages1(rmm_t* mm, int order) {
  usize size = (usize)PAGE_SIZE << order;

  if (order >= MAX_ORDER)
    return NULL;

  ilist_t* block;
  ilist_t* freelist = &mm->freelists[order];

  if (ilist_is_empty(freelist)) {
    // No free blocks of requested order.
    // Allocate a block of the next order and split it to create two buddies.
    //dlog("split to create buddies");
    block = rmm_allocpages1(mm, order + 1);
    if UNLIKELY(block == NULL)
      return NULL;
    ilist_t* buddy = (void*)block + size;
    ilist_prepend(freelist, buddy);
  } else {
    block = mm->freelists[order].prev;
    assert(block != freelist);
    ilist_del(block);
  }

  // TODO: shift instead of division?
  usize bit = ((uintptr)block - (uintptr)(mm->start_addr)) / size;

  dlog("using %zu %s block %p (bit %zu)",
    size >= GiB ? size/GiB : size >= MiB ? size/MiB : size/kiB,
    size >= GiB ? "GiB" : size >= MiB ? "MiB" : "kiB",
    block, bit);

  assert(!bit_get(mm->bitsets[order], bit));
  bit_set(mm->bitsets[order], bit);
  mm->nalloc[order]++;

  return block;
}


void* nullable rmm_allocpages(rmm_t* mm, usize npages) {
  safecheckf(IS_POW2(npages), "can only allocate pow2(npages)");

  // order for npages (order as in size=PAGE_SIZE<<order)
  int order = 0;
  while (npages && !(npages & 1)) {
    order++;
    npages >>= 1;
  }

  RHMutexLock(&mm->lock);
  void* ptr = rmm_allocpages1(mm, order);
  if LIKELY(ptr != NULL)
    mm->free_size -= npages * PAGE_SIZE;
  RHMutexUnlock(&mm->lock);
  return ptr;
}


void rmm_freepages(rmm_t* mm, void* ptr, usize npages) {
  assert(IS_ALIGN2((uintptr)ptr, PAGE_SIZE));
  dlog("TODO");
}


static void rmm_test() {
  // since RSM runs as a regular host OS process, we get our host memory from the
  // host's virtual memory system via mmap, rather than physical memory as in a kernel.
  usize memsize = 2u * GiB;
  assert(memsize % mem_pagesize() == 0);
  void* memp = assertnotnull( osvmem_alloc(memsize) );

  rmm_t* mm = assertnotnull( rmm_emplace(memp, memsize) );
  dlog("rmm_cap_bytes()  %10zu", rmm_cap_bytes(mm));
  dlog("rmm_free_bytes() %10zu", rmm_free_bytes(mm));

  void* ptrs[16];
  for (usize i = 0; i < countof(ptrs); i++) {
    ptrs[i] = assertnotnull(rmm_allocpages(mm, 4));
    dlog("ptrs[%zu] %p", i, ptrs[i]);
  }
  // allocate an extra page to avoid triggering the tail decrement opt in rmm_freepages
  void* p2 = assertnotnull( rmm_allocpages(mm, 1) ); dlog("p2 %p", p2);

  // free in tip-tap order (0, 15, 2, 13, 4, 11, 6, 9, 8, 7, 10, 5, 12, 3, 14, 1)
  // this tests the "scan forward or backwards" branches
  for (usize i = 0; i < countof(ptrs); i++) {
    if (i % 2) {
      // dlog_recycle_state(mm);
      rmm_freepages(mm, ptrs[countof(ptrs) - i], 4);
      // dlog_recycle_state(mm);
    } else {
      rmm_freepages(mm, ptrs[i], 4);
    }
  }

  // dlog_recycle_state(mm);

  assertnotnull( rmm_allocpages(mm, 8) );

  rmm_freepages(mm, p2, 4);
  // dlog_recycle_state(mm);
  void* p3 = assertnotnull( rmm_allocpages(mm, 4) ); dlog("p3 %p", p3);

  dlog("rmm_cap_bytes()  %10zu", rmm_cap_bytes(mm));
  dlog("rmm_free_bytes() %10zu", rmm_free_bytes(mm));

  osvmem_free(memp, memsize);
}


rerror init_mm() {
  // check that PAGE_SIZE is an even multiple (or divisor) of host pagesize
  usize host_pagesize = mem_pagesize();
  if (host_pagesize % PAGE_SIZE && PAGE_SIZE % host_pagesize) {
    assertf(0, "PAGE_SIZE (%u) not a multiple of host page size (%zu)",
      PAGE_SIZE, host_pagesize);
    return rerr_invalid;
  }
  rmm_test();
  return rerr_canceled;
}
