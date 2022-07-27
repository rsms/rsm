// memory manager
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "list.h"
#include "mem.h"


typedef struct {
  struct ilist list;
} freepage_t;

typedef struct {
  uintptr      next_unused_addr; // next free page address
  uintptr      start_addr;       // host address range start (PAGE_SIZE aligned)
  uintptr      end_addr;         // host address range end (exclusive)
  struct ilist recycle;          // reclaimed pages
  RHMutex      lock;
} mm_t;

static_assert(sizeof(rmm_t) >= sizeof(mm_t), "");


rerror rmm_init(rmm_t* mmp, void* memp, usize memsize) {
  uintptr addr = ALIGN2((uintptr)memp, PAGE_SIZE);
  if (addr >= (uintptr)memp + memsize)
    return rerr_nomem;

  mm_t* mm = (mm_t*)mmp;
  mm->start_addr = addr;
  mm->end_addr = addr + memsize;
  mm->next_unused_addr = mm->start_addr;
  ilist_init(&mm->recycle);

  if (!RHMutexInit(&mm->lock))
    return rerr_invalid;

  dlog("mm range: %lx…%lx (%.0f MB in %lu pages)",
    mm->start_addr, mm->end_addr, (double)(mm->end_addr - mm->start_addr)/1024.0/1024.0,
    (mm->end_addr - mm->start_addr) / PAGE_SIZE);

  return 0;
}


void* nullable rmm_allocpages(rmm_t* mmp, usize npages) {
  mm_t* mm = (mm_t*)mmp;
  RHMutexLock(&mm->lock);

  // recycle a page range
  uintptr addr, endaddr = 0;
  usize nfound = 1;
  ilist_for_each_reverse(top, &mm->recycle) {
    if ((uintptr)top != endaddr)
      nfound = 0;
    nfound++;
    if (nfound == npages) {
      // found a consecutive range of npages
      dlog("recycling %zu pages %p ... %p",
        nfound, top, (void*)top + (npages * PAGE_SIZE));
      addr = (uintptr)top;
      // TODO: remove top + rest from list
      goto end;
    }
    endaddr = (uintptr)top - PAGE_SIZE;
  }

  // allocate from unused address space
  addr = mm->next_unused_addr;
  endaddr = addr + (npages * PAGE_SIZE);
  if UNLIKELY(endaddr > mm->end_addr) {
    addr = 0; // not enough memory available
  } else {
    mm->next_unused_addr = endaddr;
  }

end:
  RHMutexUnlock(&mm->lock);
  return (void*)addr;
}


void rmm_freepages(rmm_t* mmp, void* ptr, usize npages) {
  mm_t* mm = (mm_t*)mmp;

  assert(IS_ALIGN2((uintptr)ptr, PAGE_SIZE));

  RHMutexLock(&mm->lock);

  assertf((uintptr)ptr >= mm->start_addr, "outside address %p", ptr);
  //dlog("free %p ... %p (%zu pages)", ptr, ptr + (npages * PAGE_SIZE), npages);

  // in case the pages at ptr are at the tail of next_unused_addr,
  // simply decrement next_unused_addr instead of placing them on the recycle list.
  usize tailaddr;
  #if DEBUG
  assert(!check_sub_overflow(mm->next_unused_addr, npages * PAGE_SIZE, &tailaddr));
  #else
  tailaddr = mm->next_unused_addr - (npages * PAGE_SIZE);
  #endif
  if (tailaddr == (uintptr)ptr) {
    mm->next_unused_addr = tailaddr;
    goto end;
  }

  // add pages to recycle list (sorted)
  const uintptr max_recycle_addr = (uintptr)mm->recycle.prev;

  // if the list is empty or if the largest page address in the list is smaller than ptr
  // simply append the pages we are freeing to the end of the recycle list
  if (ilist_is_empty(&mm->recycle) || max_recycle_addr < (uintptr)ptr) {
    for (usize i = 0; i < npages; i++)
      ilist_append(&mm->recycle, (ilist_t*)(ptr + (i * PAGE_SIZE)));
    goto end;
  }

  // sorted insertion
  const uintptr min_recycle_addr = (uintptr)mm->recycle.next;
  const uintptr mid_recycle_addr = (max_recycle_addr + min_recycle_addr) / 2;
  ilist_t* smaller_entry;

  if (mid_recycle_addr > (uintptr)ptr) {
    // ptr is closer to the smallest page address in recycle
    // forward scan (small -> large)
    ilist_for_each(cur, &mm->recycle) {
      if ((uintptr)cur < (uintptr)ptr)
        continue;
      ilist_insert_before(cur, (ilist_t*)ptr);
      goto insert_rest;
    }
    UNREACHABLE;
  } else {
    // ptr is closer to the largest page address in recycle
    // reverse scan (small -> large)
    ilist_for_each_reverse(cur, &mm->recycle) {
      if ((uintptr)cur > (uintptr)ptr)
        continue;
      ilist_insert_after(cur, (ilist_t*)ptr);
      goto insert_rest;
    }
    UNREACHABLE;
  }

insert_rest:
  smaller_entry = (ilist_t*)ptr;
  for (usize i = 1; i < npages; i++) {
    ilist_t* ent = (ilist_t*)(ptr + (i * PAGE_SIZE));
    ilist_insert_after(smaller_entry, ent);
    smaller_entry = ent;
  }

end:
  RHMutexUnlock(&mm->lock);
}


static void dlog_recycle_state(rmm_t* mmp) {
  mm_t* mm = (mm_t*)mmp;
  usize i = 0;
  ilist_for_each(cur, &mm->recycle)
    dlog("%p [%zu]", cur, i++);
}


static void rmm_test() {
  // since RSM runs as a regular host OS process, we get our host memory from the
  // host's virtual memory system via mmap, rather than physical memory as in a kernel.
  usize memsize = 2u * GiB;
  assert(memsize % mem_pagesize() == 0);
  void* memp = assertnotnull( osvmem_alloc(memsize) );

  rmm_t mm;
  rmm_init(&mm, memp, memsize);

  void* ptrs[16];
  for (usize i = 0; i < countof(ptrs); i++) {
    ptrs[i] = assertnotnull(rmm_allocpages(&mm, 3));
    dlog("ptrs[%zu] %p", i, ptrs[i]);
  }
  // allocate an extra page to avoid triggering the tail decrement opt in rmm_freepages
  void* p2 = assertnotnull( rmm_allocpages(&mm, 1) ); dlog("p2 %p", p2);

  // free in tip-tap order (0, 15, 2, 13, 4, 11, 6, 9, 8, 7, 10, 5, 12, 3, 14, 1)
  // this tests the "scan forward or backwards" branches
  for (usize i = 0; i < countof(ptrs); i++) {
    if (i % 2) {
      // dlog_recycle_state(&mm);
      rmm_freepages(&mm, ptrs[countof(ptrs) - i], 3);
      // dlog_recycle_state(&mm);
    } else {
      rmm_freepages(&mm, ptrs[i], 3);
    }
  }

  dlog_recycle_state(&mm);

  assertnotnull( rmm_allocpages(&mm, 5) );

  rmm_freepages(&mm, p2, 3);
  // dlog_recycle_state(&mm);
  void* p3 = assertnotnull( rmm_allocpages(&mm, 3) ); dlog("p3 %p", p3);

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
  return 0;
}
