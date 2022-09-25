// memory manager
// SPDX-License-Identifier: Apache-2.0
//
// This memory manager implements a binary buddy allocator, where a linear
// address range is arranged in sub-ranges half the size of larger sub-ranges.
// Blocks are managed per order of power of two (0 4096, 1 8192, 2 16384, ...)
//
// Here's an illustration of what the hierarchy logically looks like when we
// manage 64 KiB of memory. Blocks are considered "buddies" when they are split.
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
// The following allocations has been made to get to the state shown above:
// - allocate 1 page  -> 1st block of order 0  (b1/b1/b1/b1/b1)
// - allocate 2 pages -> 2nd block of order 1  (b1/b1/b1/b2)
// - allocate 1 page  -> 2nd block of order 0  (b1/b1/b1/b1/b2)
// - allocate 1 page  -> 5th block of order 0  (b1/b1/b2/b1/b1)
//
// We use one free-list and one bitset per order. The free-lists contains free
// blocks and the bitsets denotes what blocks are free and which are buddies.
//
#include "rsmimpl.h"
#include "list.h"
#include "bits.h"
#include "thread.h"

// MAX_ORDER: the largest pow2 order of page size to use in our buddy tree.
// This value has almost no impact on the capacity.
// The size of bitset data varies very little with this value:
//   MAX_ORDER 12 vs 20 uses 262125 B vs 262167 B memory for bitsets, respectively.
// The size of mm_t also varies very little with this value:
//   MAX_ORDER 12 vs 20 uses 352 B vs 544 B memory for mm_t, respectively.
// These differences are so small that it doesn't change the page aligned usable
// memory range, meaning the amount of usable memory is the same for e.g. 12 and 20.
// If the value is small, there will be a few freelists with many entries each,
// if the value is large, there will be many freelists with a few entries each.
// The ideal value is large enough for the most commonly managed memory size.
// The largest possible value is log2(max_address/PAGE_SIZE).
#if UINTPTR_MAX >= 0xffffffffffffffff
  #define MAX_ORDER  20 /* 17=512M 18=1G 19=2G 20=4G ... (assuming PAGE_SIZE=4096) */
#else
  #define MAX_ORDER  19
#endif

// MAX_ORDER_NPAGES: number of pages that can fit into the largest order
#define MAX_ORDER_NPAGES  (1lu << MAX_ORDER)

// RMM_TRACE: define to enable logging a lot of info via dlog
//#define RMM_TRACE

// RMM_RUN_TEST_ON_INIT: define to run tests during init_mm
#define RMM_RUN_TEST_ON_INIT


typedef struct rmm_ {
  mutex_t lock;
  uintptr start_addr; // host address range start (PAGE_SIZE aligned, read-only)
  uintptr end_addr;   // host address range end (exclusive, read-only)
  usize   free_size;  // number of free bytes (i.e. available to allocate)
  u8*     bitsets[MAX_ORDER + 1];
  ilist_t freelists[MAX_ORDER + 1];
  bool    owns_host_vmmap; // true if rmm_dispose should call osvmem_free
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


static_assert(MAX_ORDER <= ILOG2(UINTPTR_MAX / PAGE_SIZE), "MAX_ORDER too large");


usize rmm_cap(const rmm_t* mm) {
  return (usize)(mm->end_addr - mm->start_addr) / PAGE_SIZE;
}

usize rmm_avail_total(rmm_t* mm) {
  mutex_lock(&mm->lock);
  usize npages = mm->free_size / PAGE_SIZE;
  mutex_unlock(&mm->lock);
  return npages;
}

usize rmm_avail_maxregion(rmm_t* mm) {
  usize npages = 0;
  mutex_lock(&mm->lock);
  for (int order = 0; order <= MAX_ORDER; order++) {
    usize n = ilist_count(&mm->freelists[order]) * (1lu << order);
    if (n > npages)
      npages = n;
  }
  mutex_unlock(&mm->lock);
  return npages;
}


#if RMM_TRACE
  UNUSED static void trace_freelist(const rmm_t* mm, int order) {
    const ilist_t* head = &mm->freelists[order];
    trace("  freelists[order=%d]", order);
    usize i = 0; // expect ents[0] first
    ilist_for_each(cur, head) {
      uintptr addr = (uintptr)cur - mm->start_addr;
      if (cur->prev == head && cur->next == head) {
        trace("    [%zu] 0x%lx (.prev HEAD, .next HEAD)", i, addr);
      } else if (cur->prev == head) {
        uintptr next = (uintptr)cur->next - mm->start_addr;
        trace("    [%zu] 0x%lx (.prev HEAD, .next 0x%lx)", i, addr, next);
      } else if (cur->next == head) {
        uintptr prev = (uintptr)cur->prev - mm->start_addr;
        trace("    [%zu] 0x%lx (.prev 0x%lx, .next HEAD)", i, addr, prev);
      } else {
        uintptr next = (uintptr)cur->next - mm->start_addr;
        uintptr prev = (uintptr)cur->prev - mm->start_addr;
        trace("    [%zu] 0x%lx (.prev 0x%lx, .next 0x%lx)", i, addr, prev, next);
      }
      i++;
    }
    if (i == 0)
      trace("    (empty)");
  }
#else
  #define trace_freelist(...) ((void)0)
#endif


static uintptr rmm_allocpages1(rmm_t* mm, int order) {
  usize blocksize = (usize)PAGE_SIZE << order;

  if (order > MAX_ORDER)
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
    ilist_t* buddy2 = (void*)buddy1 + blocksize;
    ilist_append(&mm->freelists[order], buddy2);
    trace_freelist(mm, order);

    #ifdef RMM_TRACE
    usize nextsize = (usize)PAGE_SIZE << (order + 1);
    trace("split block %d:0x%lx (%zu %s) -> blocks %d:%p, %d:%p",
      order, addr,
      nextsize >= GiB ? nextsize/GiB : nextsize >= MiB ? nextsize/MiB : nextsize/KiB,
      nextsize >= GiB ? "GiB" : nextsize >= MiB ? "MiB" : "KiB",
      order + 1, (void*)addr,
      order + 1, (void*)addr + blocksize);
    #endif
  }

  usize n = addr >> (PAGE_SIZE_BITS + order);

  trace("allocate block %d:0x%lx (%zu %s, 0x%lx, #%zu)",
    order, addr,
    blocksize >= GiB ? blocksize/GiB : blocksize >= MiB ? blocksize/MiB : blocksize/KiB,
    blocksize >= GiB ? "GiB" : blocksize >= MiB ? "MiB" : "KiB",
    addr + mm->start_addr, n);

  assert(!bit_get(mm->bitsets[order], n));
  bit_set(mm->bitsets[order], n);

  return addr;
}


void* nullable rmm_allocpages(rmm_t* mm, usize npages) {
  if (npages == 0)
    return 0;

  safecheckf(IS_POW2(npages), "can only allocate pow2(npages)");

  int order = 0;
  for (usize n = npages; n && !(n & 1); n >>= 1)
    order++;

  mutex_lock(&mm->lock);

  uintptr addr = rmm_allocpages1(mm, order);
  if UNLIKELY(addr == UINTPTR_MAX) {
    addr = 0;
  } else {
    addr += mm->start_addr;
    mm->free_size -= npages * PAGE_SIZE;
  }

  mutex_unlock(&mm->lock);
  return (void*)addr;
}


void* nullable rmm_allocpages_min(rmm_t* mm, usize* req_npages, usize min_npages) {
  usize npages = *req_npages;
  npages = CEIL_POW2(npages);
  if (min_npages == 0)
    min_npages = 1;
  for (;;) {
    void* p = rmm_allocpages(mm, npages);
    if (p) {
      *req_npages = npages;
      return p;
    }
    if (npages == min_npages)
      return NULL;
    npages >>= 1;
  }
}


static int rmm_freepages1(rmm_t* mm, uintptr addr, int order, usize npages) {
  if (order > MAX_ORDER)
    return -1;

  usize blocksize = (uintptr)PAGE_SIZE << order;
  // Instead of "n = addr / blocksize" we can do this: (since blocksize is pow2)
  usize n = addr >> (PAGE_SIZE_BITS + order);

  trace("rmm_freepages1 block %d:0x%lx (%lu pages) #%zu", order, addr, 1lu << order, n);

  if (!bit_get(mm->bitsets[order], n)) {
    // this block is not in use

    // check for "under free", for example:
    //   p = rmm_allocpages(mm, 4)  // allocates an order#2 block
    //   rmm_freepages(mm, p,   2)  // frees an order#1 block (2 != 8)
    assertf(npages > (usize)1lu << order,
      "0x%lx points to an allocation larger than %zu pages",
      addr + mm->start_addr, npages);

    // switch to block of twice the size
    return rmm_freepages1(mm, addr, order + 1, npages);
  }

  // check for "over free", for example:
  //   p = rmm_allocpages(mm, 2)
  //   rmm_freepages(mm, p,   4)  // 2 != 4
  assertf(npages <= (usize)1lu << order,
    "0x%lx (%zu pages) points to a smaller allocation (%zu pages)",
    addr + mm->start_addr, npages, (usize)1lu << order);

  // block is in use; mark it as unused by clearing its "in use" bit
  bit_clear(mm->bitsets[order], n);

  uintptr buddy_addr = addr ^ blocksize;
  usize buddy_n = buddy_addr >> (PAGE_SIZE_BITS + order);

  if (!bit_get(mm->bitsets[order], buddy_n)) {
    // buddy is not in use -- merge
    trace("  merge buddies 0x%lx + 0x%lx", addr, buddy_addr);
    ilist_t* buddy = (void*)(buddy_addr + mm->start_addr);
    assertnotnull(buddy->next); assert(buddy->next != buddy);
    assertnotnull(buddy->prev); assert(buddy->prev != buddy);
    ilist_del(buddy);
    trace_freelist(mm, order);
    rmm_freepages1(mm, buddy_addr > addr ? addr : buddy_addr, order + 1, npages);
  } else {
    trace("  free block %d:0x%lx", order, addr);
    ilist_t* block = (void*)(addr + mm->start_addr);
    ilist_append(&mm->freelists[order], block);
    trace_freelist(mm, order);
  }

  return order;
}


void rmm_freepages(rmm_t* mm, void* ptr, usize npages) {
  if (npages == 0)
    return;
  assert(IS_ALIGN2((uintptr)ptr, PAGE_SIZE));
  assertf(IS_POW2(npages), "npages %zu is not pow2", npages);
  uintptr addr = (uintptr)ptr - mm->start_addr;
  trace("rmm_freepages %p %zu (reladdr 0x%lx)", ptr, npages, addr);
  mutex_lock(&mm->lock);
  int order = rmm_freepages1(mm, addr, 0, npages);
  if (order >= 0)
    mm->free_size += (usize)PAGE_SIZE << order;
  mutex_unlock(&mm->lock);
}


rmm_t* nullable rmm_create(void* memp, usize memsize) {
  // Align the start address to our minimum requirement,
  // compute the end address and adjust memsize.
  uintptr start = ALIGN2((uintptr)memp, PAGE_SIZE);
  uintptr end = (uintptr)memp + memsize;
  memsize = (usize)(end - start);
  trace("total      %p … %p (%zu KiB)",
    (void*)start, (void*)end, memsize / KiB);

  // Place the mm struct at the end of memory to increase alignment efficiency,
  // assuming that in most cases start has a large alignment.
  // (The rmem allocator will allocate 64k-aligned chunks immediately, for its slabs.)
  //
  //   ┌───────────────────────────────┬──────────┬──────────┬──────────┬───────┐
  //   │ memory                        │ bitset 1 │ bitset … │ bitset N │ rmm_t │
  //   ├───────────────────────────────┼──────────┴──────────┴──────────┴───────┘
  // start                            end
  //
  rmm_t* mm = (rmm_t*)ALIGN2_FLOOR(end - sizeof(rmm_t), _Alignof(rmm_t));
  mutex_init(&mm->lock);
  mm->owns_host_vmmap = false;
  trace("mm at      %p … %p (%zu B)", mm, (void*)mm + sizeof(rmm_t), sizeof(rmm_t));

  // adjust memsize to the usable space at start (memsize = mm - start)
  if (check_sub_overflow((uintptr)mm, start, &memsize))
    goto out_of_memory;

  // number of entries per bitset
  const usize nchunks = memsize / PAGE_SIZE;

  // bset_nbytes is the size in bytes of bitset[0] (sans bset_extra_nbytes)
  const usize bset_nbytes = nchunks / 8;
  const usize bset_extra_nbytes = 2; // extra per bitset

  // BITSET_SIZE returns the byte size of bitset[order]
  #define BITSET_SIZE(order) ((bset_nbytes >> (order)) + bset_extra_nbytes)

  // (over) estimate memory needed for bitset data
  //usize bset_total_size = (nchunks * 2) + (MAX_ORDER * 2);

  // calculate total memory needed for bitsets
  usize bset_total_size = bset_nbytes + bset_extra_nbytes;
  for (u8 order = 1; order <= MAX_ORDER; order++)
    bset_total_size += BITSET_SIZE(order);

  // Adjust memsize to the usable space at start (memsize -= bset_total_size + PAGE_SIZE)
  // We need at least one page of free memory in addition to bitset storage.
  if (check_sub_overflow(memsize, bset_total_size + PAGE_SIZE, &memsize))
    goto out_of_memory;

  // calculate start of bitset data
  void* bitset_start = (void*)(uintptr)mm - bset_total_size;
  trace("bitsets at %p … %p (%zu B)",
    bitset_start, bitset_start + bset_total_size, bset_total_size);

  // Initialize each per-order bitset & freelist
  //
  // The smallest-order bitset holds the smallest block granule and has the most bits.
  // The next smallest-order bitset holds blocks twice the size; has 2/n bits.
  // The next smallest-order bitset holds blocks twice the size; has 2/n bits.
  // ...
  // The largest-order bitset holds blocks of the largest size and may have more than
  // one block, in case the total amount of memory is >=2x of MAX_ORDER.
  // (+2: this diagram assumes bset_extra_nbytes=2)
  //      ┌────────────────────────────────┬────────────────┬────────┬────┐
  //      │ order 0                        │ order 1        │ o2     │ o3 │
  //      ├────────────────────────────────┴────────────────┴────────┴────┘
  //      ↑           nbyte=16+2                  8+2          4+2    2+2
  // bitset_start
  for (u8 order = 0; order <= MAX_ORDER; order++) {
    ilist_init(&mm->freelists[order]);

    mm->bitsets[order] = bitset_start;
    usize size = BITSET_SIZE(order);
    memset(bitset_start, 0, size); // set all bits to 0
    bitset_start += size;
  }

  // align memsize to page boundary
  if UNLIKELY((memsize = ALIGN2_FLOOR(memsize, PAGE_SIZE)) == 0)
    goto out_of_memory;

  // set usable memory
  mm->start_addr = start;
  mm->end_addr = start + memsize;
  mm->free_size = memsize;

  trace("memory at  %p … %p (%zu KiB in %zu pages)",
    (void*)mm->start_addr,  (void*)mm->end_addr,  memsize / KiB,  memsize / PAGE_SIZE);
  trace("max buddy  %11zu KiB", ((usize)MAX_ORDER_NPAGES * PAGE_SIZE) / KiB);

  // Now we need to put initially-free blocks of memory into the free lists.
  // We'll do this by starting with the largest pow2(memsize),
  // then we put the rest pow2(memsize - (largest pow2(memsize))) and so on.
  usize npages_total = memsize / PAGE_SIZE;
  while (npages_total) {
    // Round npages_total down to nearest pow2, constrained by the top order page limit
    const usize npages = MIN(FLOOR_POW2(npages_total), MAX_ORDER_NPAGES);
    assert(npages > 0);
    npages_total -= npages;

    int order = -1;
    for (usize n = npages; n; n >>= 1)
      order++;
    assertf(order <= MAX_ORDER,
      "order (%d) > MAX_ORDER (%d) (npages %zu, MAX_ORDER_NPAGES %zu)",
      order, MAX_ORDER, npages, MAX_ORDER_NPAGES);

    // memory used by this block
    usize block_size = (usize)PAGE_SIZE << order;

    trace("initial free block %d:%p  %p … %p (%zu KiB)",
      order, (void*)(start - mm->start_addr),
      (void*)start, (void*)start + block_size,
      block_size / KiB);

    // add the block to its order's freelist
    ilist_append(&mm->freelists[order], (ilist_t*)start);
    //trace_freelist(mm, order);

    // calculate bit for the block
    usize bit = (start - mm->start_addr) / block_size;

    // Clear block bit to mark the block as "free".
    // This is needed even though we memset(0) the bitset earlier since we may end
    // up with up to three pages for the 0th order, and since we set the buddy bit
    // after clearing the block bit, the 2nd and 3rd page's bits would not be cleared
    // unless we do this.
    bit_clear(mm->bitsets[order], bit);

    // Set buddy bit of (invalid, imaginary) "end buddy" of the largest block
    bit_set(mm->bitsets[order], bit + 1);

    start += block_size;
  }

  #undef BITSET_SIZE

  return mm;

out_of_memory:
  dlog("[%s] not enough memory (%lu B)", __FUNCTION__, (end - (uintptr)memp));
  return NULL;
}


rmm_t* nullable rmm_create_host_vmmap(usize memsize) {
  void* ptr = osvmem_alloc(memsize);
  if (!ptr)
    return NULL;
  rmm_t* mm = rmm_create(ptr, memsize);
  if (!mm) {
    osvmem_free(ptr, memsize);
    return NULL;
  }
  mm->owns_host_vmmap = true;
  return mm;
}


void rmm_dispose(rmm_t* mm) {
  if (!mm->owns_host_vmmap)
    return;
  mutex_dispose(&mm->lock);
  void* ptr = (void*)rmm_startaddr(mm);
  usize size = ALIGN2(mm->end_addr - mm->start_addr, os_pagesize());
  if (!osvmem_free(ptr, size))
    dlog("rmm_dispose failed to release memory back to host with osvmem_free");
}


uintptr rmm_startaddr(const rmm_t* mm) {
  return mm->start_addr;
}


#if defined(RMM_RUN_TEST_ON_INIT) && DEBUG
static void test_rmm() {
  dlog("%s", __FUNCTION__);
  // since RSM runs as a regular host OS process, we get our host memory from the
  // host's virtual memory system via mmap, rather than physical memory as in a kernel.
  usize memsize = 10 * MiB;
  void* memp = assertnotnull( osvmem_alloc(memsize) );

  rmm_t* mm = assertnotnull( rmm_create(memp, memsize) );
  trace("rmm_cap()             %10zu", rmm_cap(mm));
  trace("rmm_avail_total()     %10zu", rmm_avail_total(mm));
  trace("rmm_avail_maxregion() %10zu", rmm_avail_maxregion(mm));

  assertnull(rmm_allocpages(mm, 0));

  void* p = assertnotnull( rmm_allocpages(mm, 4) );
  trace("rmm_allocpages(4) => %p", p);
  rmm_freepages(mm, p, 4);

  p = assertnotnull( rmm_allocpages(mm, 4) );
  trace("rmm_allocpages(4) => %p", p);
  rmm_freepages(mm, p, 4);

  void* ptrs[16];
  for (usize i = 0; i < countof(ptrs); i++) {
    ptrs[i] = assertnotnull(rmm_allocpages(mm, 4));
    trace("ptrs[%zu] %p", i, ptrs[i]);
  }
  // allocate an extra page to avoid triggering the tail decrement opt in rmm_freepages
  void* p2 = assertnotnull( rmm_allocpages(mm, 1) ); trace("p2 %p", p2);

  // free in tip-tap order (0, 15, 2, 13, 4, 11, 6, 9, 8, 7, 10, 5, 12, 3, 14, 1)
  // this tests the "scan forward or backwards" branches
  for (usize i = 0; i < countof(ptrs); i++) {
    if (i % 2) {
      rmm_freepages(mm, ptrs[countof(ptrs) - i], 4);
    } else {
      rmm_freepages(mm, ptrs[i], 4);
    }
  }

  rmm_freepages(mm, p2, 1);

  // test "under free" and "over free" (panics)
  //p = assertnotnull( rmm_allocpages(mm, 8) );
  //rmm_freepages(mm, p, 4);  // under free
  //rmm_freepages(mm, p, 16); // over free

  trace("rmm_cap()             %10zu", rmm_cap(mm));
  trace("rmm_avail_total()     %10zu", rmm_avail_total(mm));
  trace("rmm_avail_maxregion() %10zu", rmm_avail_maxregion(mm));

  rmm_dispose(mm);
  osvmem_free(memp, memsize);
  trace("—————— %s end ——————", __FUNCTION__);
}
#endif // RMM_RUN_TEST_ON_INIT


rerr_t init_mm() {
  // check that PAGE_SIZE is an even multiple (or divisor) of host pagesize
  usize host_pagesize = os_pagesize();
  if (host_pagesize % PAGE_SIZE && PAGE_SIZE % host_pagesize) {
    assertf(0, "PAGE_SIZE (%u) not a multiple of host page size (%zu)",
      PAGE_SIZE, host_pagesize);
    return rerr_invalid;
  }

  #if defined(RMM_RUN_TEST_ON_INIT) && DEBUG
  test_rmm();
  #endif

  return 0;
}
