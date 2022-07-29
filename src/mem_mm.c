// memory manager
// SPDX-License-Identifier: Apache-2.0
//
// This memory manager implements a binary buddy allocator, where a linear address range
// is arranged in sub-ranges half the size of larger sub-ranges.
// Blocks are managed per order of power of two (0 4096, 1 8192, 2 16384, ...)
//
// Here's an illustration of what the heirarchy logically looks like when we
// manage 64 kiB of memory. Blocks are considered "buddies" when they are split.
// The blocks below are filled with "Buddy #" when they are allocated and left
// empty when they are free. I.e. the second block of the 3rd order is free.
//
//  start of managed                                               end of managed
//   address range                                                 address range
//       ╷                                                               ╷
//  order┌───────────────────────────────────────────────────────────────┒
//    4  │                            Buddy 1                            ┃
//       ├───────────────────────────────┰───────────────────────────────┨
//    3  │            Buddy 1            ┃                               ┃
//       ├───────────────┰───────────────╂───────────────────────────────┨
//    2  │    Buddy 1    ┃    buddy 2    ┃                               ┃
//       ├───────┰───────╂───────┬───────┨                               ┃
//    1  │ Bud 1 ┃ Bud 2 ┃ Bud 1 │       ┃                               ┃
//       ├───┰───╂───────╂───┬───┼───────┨                               ┃
//    0  │ 1 ┃ 2 ┃       ┃ 1 │   │       ┃                               ┃
//       └───╂───┨       ┠───┴───┘       ┃                               ┃
//        4096  8192   16384           32768                           65536
//
// The following allocation has been made to get the blocks into the state shown above:
// - allocate 1 page  -> 1st block of order 0  (b1/b1/b1/b1/b1)
// - allocate 2 pages -> 2nd block of order 1  (b1/b1/b1/b2)
// - allocate 1 page  -> 2nd block of order 0  (b1/b1/b1/b1/b2)
// - allocate 1 page  -> 5th block of order 0  (b1/b1/b2/b1/b1)
//
// We use one free-list and one bitset per order. The free-lists contains free blocks
// and the bitsets denotes what blocks are free and which are buddies.
//
// Ideas for improvement
// - use just one bit per buddy (half the amount of bits in total)
//
#include "rsmimpl.h"
#include "list.h"
#include "bits.h"
#include "mem.h"

#define MAX_ORDER   19  /* 19=2GiB, 20=4GiB, 21=8GiB, 22=16GiB, ... */
#define OBJ_MAXSIZE (1ul * GiB)

// RMM_TRACE: uncomment to enable logging a lot of info via dlog
//#define RMM_TRACE

// RMM_RUN_TEST_ON_INIT: uncomment to run tests during init_mm
//#define RMM_RUN_TEST_ON_INIT


typedef struct rmm_ {
  RHMutex lock;
  uintptr start_addr; // host address range start (PAGE_SIZE aligned, read-only)
  uintptr end_addr;   // host address range end (exclusive, read-only)
  usize   free_size;  // number of free bytes (i.e. available to allocate)
  u8*     bitsets[MAX_ORDER + 1];
  ilist_t freelists[MAX_ORDER + 1];
  //usize nalloc[MAX_ORDER + 1]; // number of allocations per order
} rmm_t;


#if defined(RMM_TRACE) && defined(DEBUG)
  #define trace(fmt, args...) dlog("[mm] " fmt, ##args)
#else
  #ifdef RMM_TRACE
    #warning RMM_TRACE has no effect unless DEBUG is enabled
    #undef RMM_TRACE
  #endif
  #define trace(...) ((void)0)
#endif


usize rmm_cap_bytes(const rmm_t* mm) {
  return (usize)(mm->end_addr - mm->start_addr);
}

usize rmm_avail_bytes(const rmm_t* mm) {
  return mm->free_size;
}


#if DEBUG
UNUSED static void dlog_freelist(const rmm_t* mm, int order) {
  const ilist_t* head = &mm->freelists[order];
  trace("freelists[%d] %p", order, head);
  usize i = 0; // expect ents[0] first
  ilist_for_each(cur, head) {
    if (cur->prev == head && cur->next == head) {
      trace("  [%zu] %p (.prev HEAD, .next HEAD)", i, cur);
    } else if (cur->prev == head) {
      trace("  [%zu] %p (.prev HEAD, .next %p)", i, cur, cur->next);
    } else if (cur->next == head) {
      trace("  [%zu] %p (.prev %p, .next HEAD)", i, cur, cur->prev);
    } else {
      trace("  [%zu] %p (.prev %p, .next %p)", i, cur, cur->prev, cur->next);
    }
    i++;
  }
}
#endif


static uintptr rmm_allocpages1(rmm_t* mm, int order) {
  usize size = (usize)PAGE_SIZE << order;

  if (order >= MAX_ORDER)
    return UINTPTR_MAX;

  // try to dequeue a free block
  uintptr addr = (uintptr)ilist_pop(&mm->freelists[order]);

  if (addr) {
    addr -= mm->start_addr;
  } else {
    // No free blocks of requested order.
    // Allocate a block of the next order and split it to create two buddies.
    addr = rmm_allocpages1(mm, order + 1);
    if UNLIKELY(addr == UINTPTR_MAX)
      return UINTPTR_MAX;
    ilist_t* buddy1 = (ilist_t*)(addr + mm->start_addr);
    ilist_t* buddy2 = (void*)buddy1 + size;
    trace("ilist_append %p", buddy2);
    ilist_append(&mm->freelists[order], buddy2);
    dlog_freelist(mm, order);

    #if RMM_TRACE
    usize nextsize = (usize)PAGE_SIZE << (order + 1);
    trace("split block %d:%p (%zu %s) -> blocks %d:%p, %d:%p",
      order, (void*)addr,
      nextsize >= GiB ? nextsize/GiB : nextsize >= MiB ? nextsize/MiB : nextsize/kiB,
      nextsize >= GiB ? "GiB" : nextsize >= MiB ? "MiB" : "kiB",
      order + 1, (void*)addr,
      order + 1, (void*)addr + size);
    #endif
  }

  usize bit = addr / size;

  trace("using block %d:%p (%zu %s, 0x%lx, bit %zu)",
    order, (void*)addr,
    size >= GiB ? size/GiB : size >= MiB ? size/MiB : size/kiB,
    size >= GiB ? "GiB" : size >= MiB ? "MiB" : "kiB",
    addr + mm->start_addr, bit);

  assert(!bit_get(mm->bitsets[order], bit));
  bit_set(mm->bitsets[order], bit);
  //mm->nalloc[order]++;

  return addr;
}


void* nullable rmm_allocpages(rmm_t* mm, usize npages) {
  safecheckf(IS_POW2(npages), "can only allocate pow2(npages)");
  int order = 0;
  for (usize n = npages; n && !(n & 1); n >>= 1)
    order++;
  RHMutexLock(&mm->lock);
  uintptr addr = rmm_allocpages1(mm, order);
  if UNLIKELY(addr == UINTPTR_MAX) {
    addr = 0;
  } else {
    addr += mm->start_addr;
    mm->free_size -= npages * PAGE_SIZE;
  }
  RHMutexUnlock(&mm->lock);
  return (void*)addr;
}


static int rmm_freepages1(rmm_t* mm, uintptr addr, int order) {
  if (order >= MAX_ORDER)
    return -1;

  int bit = addr / ((uintptr)PAGE_SIZE << order);
  trace("rmm_freepages1 block %d:%p, bit %d", order, (void*)addr, bit);

  if (!bit_get(mm->bitsets[order], bit))
    return rmm_freepages1(mm, addr, order + 1);

  uintptr buddy_addr = addr ^ ((uintptr)PAGE_SIZE << order);
  int buddy_bit = buddy_addr / ((uintptr)PAGE_SIZE << order);
  bit_clear(mm->bitsets[order], bit); // no longer in use

  trace("  bit %d=0, buddy_bit %d=%d, buddy %p",
    bit, buddy_bit, bit_get(mm->bitsets[order], buddy_bit), (void*)buddy_addr);

  if (!bit_get(mm->bitsets[order], buddy_bit)) {
    // buddy is not in use -- merge
    trace("  merge buddies %p + %p", (void*)addr, (void*)buddy_addr);
    ilist_t* buddy = (void*)(buddy_addr + mm->start_addr);
    assertnotnull(buddy->next); assert(buddy->next != buddy);
    assertnotnull(buddy->prev); assert(buddy->prev != buddy);
    ilist_del(buddy);
    dlog_freelist(mm, order);
    rmm_freepages1(mm, buddy_addr > addr ? addr : buddy_addr, order + 1);
  } else {
    trace("  free block %p", (void*)addr);
    ilist_t* block = (void*)(addr + mm->start_addr);
    ilist_append(&mm->freelists[order], block);
    dlog_freelist(mm, order);
  }

  //mm->nalloc[order]--;
  return order;
}


void rmm_freepages(rmm_t* mm, void* ptr) {
  assert(IS_ALIGN2((uintptr)ptr, PAGE_SIZE));
  trace("rmm_freepages %p", ptr);
  RHMutexLock(&mm->lock);
  int order = rmm_freepages1(mm, (uintptr)ptr - mm->start_addr, 0);
  if (order >= 0)
    mm->free_size += (usize)PAGE_SIZE << order;
  RHMutexUnlock(&mm->lock);
}


rmm_t* nullable rmm_emplace(void* memp, usize memsize) {
  uintptr start = ALIGN2((uintptr)memp, _Alignof(rmm_t));
  uintptr end_addr = (uintptr)memp + memsize;

  if (start + sizeof(rmm_t) >= end_addr) // TODO: coalesce these checks
    return NULL;

  rmm_t* mm = (rmm_t*)start;
  //memset(&mm->nalloc, 0, sizeof(mm->nalloc));

  start += sizeof(rmm_t);
  memsize -= (usize)((start + sizeof(rmm_t)) - (uintptr)memp);

  // number of entries per bitmap
  usize nchunks = (memsize / PAGE_SIZE) / 8;

  // memory we will use for our bitsets
  usize net_bitset_size = (nchunks * 2) + (MAX_ORDER * 2);

  for (int order = 0; order < MAX_ORDER; order++) {
    mm->bitsets[order] = (void*)start;
    memset((void*)start, 0, nchunks + 2); // set all bits to 0
    start += nchunks + 2;
    nchunks /= 2;
    ilist_init(&mm->freelists[order]);
  }

  start = ALIGN2(start + net_bitset_size, PAGE_SIZE);
  memsize = ALIGN2_FLOOR(memsize - net_bitset_size, PAGE_SIZE);

  mm->start_addr = start;
  mm->end_addr = start + memsize;
  mm->free_size = (usize)(mm->end_addr - mm->start_addr);

  trace("mm using memory region %lx…%lx (%.0f MB in %lu pages)",
    mm->start_addr, mm->end_addr,
    (double)(mm->end_addr - mm->start_addr)/1024.0/1024.0,
    (mm->end_addr - mm->start_addr) / PAGE_SIZE);

  // Now we need to put initially-free blocks of memory into the free lists.
  // We'll do this by starting with the largest pow2(memsize),
  // then we put the rest pow2(memsize - (largest pow2(memsize))) and so on.
  usize npages_total = memsize / PAGE_SIZE;
  while (npages_total) {
    usize npages = FLOOR_POW2(npages_total);

    npages_total -= npages;
    int order = -1;
    for (usize n = npages; n; n >>= 1)
      order++;
    assert(order > -1);
    trace("initial free block %d:0x0 (%zu B)", order, (usize)PAGE_SIZE << order);

    ilist_append(&mm->freelists[order], (ilist_t*)start);
    //dlog_freelist(mm, order);

    // set buddy bit of (invalid, imaginary) "end buddy" of the largest block
    usize bit = (start - mm->start_addr) / ((usize)PAGE_SIZE << order);
    bit_set(mm->bitsets[order], bit + 1);

    start += (usize)PAGE_SIZE << order;
  }

  return mm;
}


#ifdef RMM_RUN_TEST_ON_INIT
static void rmm_test() {
  dlog("—————— %s begin ——————", __FUNCTION__);
  // since RSM runs as a regular host OS process, we get our host memory from the
  // host's virtual memory system via mmap, rather than physical memory as in a kernel.
  usize memsize = 10 * MiB;
  void* memp = assertnotnull( osvmem_alloc(memsize) );

  rmm_t* mm = assertnotnull( rmm_emplace(memp, memsize) );
  dlog("rmm_cap_bytes()   %10zu", rmm_cap_bytes(mm));
  dlog("rmm_avail_bytes() %10zu", rmm_avail_bytes(mm));

  void* p = assertnotnull( rmm_allocpages(mm, 4) );
  dlog("rmm_allocpages(4) => %p", p);
  rmm_freepages(mm, p);

  p = assertnotnull( rmm_allocpages(mm, 4) );
  dlog("rmm_allocpages(4) => %p", p);
  rmm_freepages(mm, p);

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
      rmm_freepages(mm, ptrs[countof(ptrs) - i]);
    } else {
      rmm_freepages(mm, ptrs[i]);
    }
  }

  rmm_freepages(mm, p2);

  dlog("rmm_cap_bytes()   %10zu", rmm_cap_bytes(mm));
  dlog("rmm_avail_bytes() %10zu", rmm_avail_bytes(mm));

  osvmem_free(memp, memsize);
  dlog("—————— %s end ——————", __FUNCTION__);
}
#endif // RMM_RUN_TEST_ON_INIT


rerror init_mm() {
  // check that PAGE_SIZE is an even multiple (or divisor) of host pagesize
  usize host_pagesize = mem_pagesize();
  if (host_pagesize % PAGE_SIZE && PAGE_SIZE % host_pagesize) {
    assertf(0, "PAGE_SIZE (%u) not a multiple of host page size (%zu)",
      PAGE_SIZE, host_pagesize);
    return rerr_invalid;
  }
  #ifdef RMM_RUN_TEST_ON_INIT
  rmm_test();
  #endif
  return 0;
}
