// virtual memory mapping
// See vmem.txt for in-depth documentation
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

#define vmtrace trace
#include "vm.h"

// VM_MAP_TRACE: define to enable logging a lot of info via dlog
//#define VM_MAP_TRACE


#if (defined(VM_MAP_TRACE) || defined(VM_TRACE)) && defined(DEBUG)
  #undef  VM_MAP_TRACE
  #define VM_MAP_TRACE
  #define trace(fmt, args...) dlog("[vm_map] " fmt, ##args)
#else
  #ifdef VM_MAP_TRACE
    #warning VM_MAP_TRACE has no effect unless DEBUG is enabled
    #undef VM_MAP_TRACE
  #endif
  #define trace(...) ((void)0)
#endif


vm_ptab_t nullable vm_ptab_alloc(rmm_t* mm) {
  // note: VM_PTAB_SIZE is always a multiple of PAGE_SIZE
  vm_ptab_t ptab = rmm_allocpages(mm, (usize)VM_PTAB_SIZE / PAGE_SIZE);
  if UNLIKELY(ptab == NULL)
    return NULL;
  memset(ptab, 0, VM_PTAB_SIZE);
  return ptab;
}


void vm_ptab_free(rmm_t* mm, vm_ptab_t ptab) {
  rmm_freepages(mm, ptab, (usize)VM_PTAB_SIZE / PAGE_SIZE);
}


rerr_t vm_map_init(vm_map_t* map, rmm_t* mm) {
  rerr_t err = rwmutex_init(&map->lock);
  if UNLIKELY(err)
    return err;
  vm_ptab_t ptab = vm_ptab_alloc(mm); // root page table
  if UNLIKELY(!ptab) {
    trace("failed to allocate root page table");
    return rerr_nomem;
  }
  trace("allocated root vm_ptab_t %p", ptab);
  map->root = ptab;
  map->mm = mm;
  map->min_free_vfn = 0;
  return 0;
}


static void vm_ptab_dispose(rmm_t* mm, vm_ptab_t ptab, u32 nuse, u32 level, u64 vfn) {
  assert(level < VM_PTAB_LEVELS);

  u64 npages = VM_PTAB_NPAGES(level+1);

  trace("vm_ptab_dispose L%u %012llx…%012llx 0x%llx nuse=%u",
    level+1, VM_VFN_VADDR(vfn), VM_VFN_VADDR(vfn + npages), npages, nuse);

  for (u32 i = 0; i < VM_PTAB_LEN && nuse != 0; i++) {
    bool isused = *(u64*)&ptab[i] != 0;
    nuse -= isused;

    if (level < VM_PTAB_LEVELS-1) {
      vm_table_t* subtable = &ptab[i].table;
      vm_ptab_t subptab = vm_table_ptab(subtable);
      if (subptab)
        vm_ptab_dispose(mm, subptab, subtable->nuse, level+1, vfn + npages*(u64)i);
    } else {
      vm_page_t* page = &ptab[i].page;
      if (page->purgeable && page->hfn) {
        void* haddr = (void*)vm_page_haddr(page);
        trace("free purgeable backing page %p", haddr);
        rmm_freepages(mm, haddr, 1);
      }
    }
  }

  vm_ptab_free(mm, ptab);
}


void vm_map_dispose(vm_map_t* map) {
  assert(!rwmutex_isrlocked(&map->lock)); // map should not be locked
  rwmutex_dispose(&map->lock);
  vm_ptab_dispose(map->mm, map->root, map->root_nuse, 0, 0);
}


static u64 alloc_backing_page(vm_map_t* map) {
  void* haddr = rmm_allocpages(map->mm, 1);
  if UNLIKELY(!haddr) {
    trace("FAILED to allocate backing page");
    panic("TODO: purge a least-recently-used page");
  }
  trace("allocated backing page %p", haddr);
  return (u64)haddr;
}


// vm_map_access returns the page table entry of a Virtual Frame Number
vm_page_t* nullable vm_map_access(vm_map_t* map, u64 vfn, bool isaccess) {
  assertf(vfn <= VM_VFN_MAX, "invalid VFN 0x%llx", vfn);
  u64 index_vfn = vfn << ((sizeof(vfn)*8) - VM_VFN_BITS);
  vm_ptab_t ptab = map->root;
  vm_page_t* page = NULL;

  vm_map_assert_rlocked(map);

  for (u32 level = 0; ; level++) {
    u32 index = (u32)(index_vfn >> (64 - VM_PTAB_BITS));

    if (level == VM_PTAB_LEVELS-1) {
      page = &ptab[index].page;
      trace("access page %012llx L%u[%u] (%s)",
        VM_VFN_VADDR(VM_BLOCK_VFN(vfn, level)), level, index,
        *(u64*)page != 0 ? "mapped" : "unused");

      if UNLIKELY(*(u64*)page == 0) {
        page = NULL;
        break;
      }

      // if there's no backing page, allocate one
      if (page->hfn == 0) {
        u64 haddr = alloc_backing_page(map);
        vm_page_set_haddr(page, haddr);
        page->purgeable = true;
      }
      break;
    }

    vm_table_t* subtable = &ptab[index].table;

    trace("access L%u %012llx L%u[%u] (nuse %u)",
      level+1, VM_VFN_VADDR(VM_BLOCK_VFN(vfn, level)), level, index, subtable->nuse);

    index_vfn <<= VM_PTAB_BITS;

    if UNLIKELY(*(u64*)subtable == 0) {
      subtable = NULL;
      break;
    }

    subtable->accessed |= isaccess;
    ptab = vm_table_ptab(subtable);
  }

  return page;
}
