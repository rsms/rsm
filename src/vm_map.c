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


#if DEBUG
  UNUSED static const char* vm_perm_str(vm_perm_t perm) {
    switch (perm) {
      case VM_PERM_R:  return "r-";
      case VM_PERM_W:  return "-w";
      case VM_PERM_RW: return "rw";
    }
    return "--";
  }
#endif


static vm_ptab_t nullable vm_ptab_create(rmm_t* mm) {
  // note: VM_PTAB_SIZE is always a multiple of PAGE_SIZE
  vm_ptab_t ptab = rmm_allocpages(mm, (usize)VM_PTAB_SIZE / PAGE_SIZE);
  if UNLIKELY(ptab == NULL)
    return NULL;
  memset(ptab, 0, VM_PTAB_SIZE);
  return ptab;
}


rerr_t vm_map_init(vm_map_t* map, rmm_t* mm) {
  rerr_t err = rwmutex_init(&map->lock);
  if UNLIKELY(err)
    return err;
  vm_ptab_t ptab = vm_ptab_create(mm); // root page table
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


static void vm_ptab_free(rmm_t* mm, vm_ptab_t ptab, u32 nuse, u32 level, u64 vfn) {
  assert(level < VM_PTAB_LEVELS);

  u64 npages = VM_PTAB_NPAGES(level+1);

  trace("vm_ptab_free L%u %012llx…%012llx 0x%llx nuse=%u",
    level+1, VM_VFN_VADDR(vfn), VM_VFN_VADDR(vfn + npages), npages, nuse);

  for (u32 i = 0; i < VM_PTAB_LEN && nuse != 0; i++) {
    vm_pte_t* pte = &ptab[i];
    bool isused = *(u64*)pte != 0;
    nuse -= isused;
    if (pte->outaddr == 0)
      continue;
    if (level < VM_PTAB_LEVELS-1) {
      vm_ptab_t sub_ptab = (vm_ptab_t)(uintptr)(pte->outaddr << PAGE_SIZE_BITS);
      vm_ptab_free(mm, sub_ptab, pte->nuse, level+1, vfn + npages*(u64)i);
    } else if (pte->purgeable) {
      void* haddr = (void*)(uintptr)(pte->outaddr << PAGE_SIZE_BITS);
      trace("free purgeable backing page %p", haddr);
      rmm_freepages(mm, haddr, 1);
    }
  }

  rmm_freepages(mm, ptab, (usize)VM_PTAB_SIZE / PAGE_SIZE);
}


void vm_map_dispose(vm_map_t* map) {
  assert(!rwmutex_isrlocked(&map->lock)); // map should not be locked
  rwmutex_dispose(&map->lock);
  vm_ptab_free(map->mm, map->root, map->root_nuse, 0, 0);
}


static u64 alloc_backing_page(vm_map_t* map) {
  void* haddr = rmm_allocpages(map->mm, 1);
  if UNLIKELY(!haddr) {
    trace("FAILED to allocate backing page");
    panic("TODO: purge a least-recently-used page");
  }
  trace("allocated backing page %p", haddr);
  u64 hfn = (u64)(uintptr)haddr >> PAGE_SIZE_BITS;
  return hfn;
}


// vm_map_access returns the page table entry of a Virtual Frame Number
vm_pte_t* nullable vm_map_access(vm_map_t* map, u64 vfn, bool isaccess) {
  assertf(vfn <= VM_VFN_MAX, "invalid VFN 0x%llx", vfn);
  u64 index_vfn = vfn << ((sizeof(vfn)*8) - VM_VFN_BITS);
  vm_ptab_t ptab = map->root;
  vm_pte_t* pte = NULL;

  vm_map_assert_rlocked(map);

  for (u32 level = 1; ; level++) {
    u32 index = (u32)(index_vfn >> (64 - VM_PTAB_BITS));
    pte = &ptab[index];

    trace("lookup vfn 0x%llx L%u ptab[%u]", vfn+1, level, index);

    if (level == VM_PTAB_LEVELS) {
      if UNLIKELY(*(u64*)pte == 0) {
        pte = NULL;
        break;
      }
      // if there's no backing page, allocate one
      if (pte->outaddr == 0) {
        pte->outaddr = alloc_backing_page(map);
        pte->purgeable = true;
      }
      break;
    }

    index_vfn <<= VM_PTAB_BITS;

    if UNLIKELY(*(u64*)pte == 0) {
      pte = NULL;
      break;
    }

    pte->accessed |= isaccess;
    ptab = (vm_ptab_t)(uintptr)(pte->outaddr << PAGE_SIZE_BITS);
  }

  return pte;
}


//—————————————————————————————————————————————————————————————————————————————————————
// vm_map_add


typedef struct {
  vm_map_t* map;
  uintptr   haddr;   // optional backing pages (count is same as vaddr)
  u64       npages;  // total number of pages to map
  u64       nmapped; // number of pages mapped so far
  vm_perm_t perm;    // permissions for mapped PTEs

  // fields only used in case of error
  u64    vaddr; // start address
  rerr_t err;
} addctx_t;


static u32 vm_map_add_visitor(
  vm_pte_t* table, u32 level, vm_ptab_t ptab, u32 index, u64 vfn, uintptr data)
{
  addctx_t* ctx = (addctx_t*)data;

  if (level < VM_PTAB_LEVELS - 1) {
    // ptab is a table of tables (i.e. ptab[index] is another table)
    vm_pte_t* pte = &ptab[index];

    trace("map L%u table %012llx…%012llx 0x%llx [%u] nuse=%u",
      level+1, VM_VFN_VADDR(vfn), VM_VFN_VADDR(vfn + VM_PTAB_NPAGES(level+1)) - 1,
      VM_PTAB_NPAGES(level+1), index, pte->nuse);

    // allocate table if needed
    if (*(u64*)pte == 0) {
      vm_ptab_t ptab2 = vm_ptab_create(ctx->map->mm);
      if UNLIKELY(!ptab2) {
        ctx->err = rerr_nomem;
        goto error;
      }
      pte->outaddr = (u64)(uintptr)ptab2 >> PAGE_SIZE_BITS;
      table->nuse++;
      //trace("allocated vm_ptab_t %p at ptab[%u]", ptab2, index);
    }

    return 1;
  }

  // ptab is a table of pages (i.e. ptab[index] is a page)

  u64 need_npages = ctx->npages - ctx->nmapped;
  const u32 end_index = (u32)MIN((u64)VM_PTAB_LEN, (u64)index + need_npages);

  uintptr haddr = ctx->haddr;
  u64 npages = (u64)(end_index - index);

  {
    UNUSED u64 vaddr = VM_VFN_VADDR(vfn);
    if (haddr) {
      trace("map %llu pages %012llx…%012llx %s => %lx…%lx",
        npages, vaddr, vaddr+(npages*PAGE_SIZE)-1, vm_perm_str(ctx->perm),
        haddr, haddr+(uintptr)(npages*PAGE_SIZE)-1);
    } else {
      trace("map %llu pages %012llx…%012llx %s",
        npages, vaddr, vaddr+(npages*PAGE_SIZE)-1, vm_perm_str(ctx->perm));
    }
  }

  for (u32 i = index; i < end_index; i++) {
    if UNLIKELY(*(u64*)&ptab[i]) {
      UNUSED u64 vaddr = VM_VFN_VADDR(vfn + (i - index));
      dlog("trying to map already-mapped page at vaddr 0x%llx", vaddr);
      ctx->err = rerr_exists;
      goto error;
    }

    *((u8*)&ptab[i]) = ctx->perm;  // sets all permission bits in one store
    vm_pte_set_outaddr(&ptab[i], (void*)haddr);
    haddr += PAGE_SIZE * !!haddr; // no-op in case haddr==0
  }

  ctx->nmapped += npages;
  ctx->haddr += (uintptr)npages * PAGE_SIZE * !!haddr;

  assertf((u64)table->nuse + npages <= VM_PTAB_LEN,
    "overflow %u + %u", table->nuse, (u32)npages);
  table->nuse += (u32)npages;

  // we are done if we have mapped all npages
  if (ctx->nmapped >= ctx->npages) {
    assertf(ctx->nmapped == ctx->npages,
      "mapped too many pages %llu > %llu", ctx->nmapped, ctx->npages);
    return 0;
  }

  return (u32)npages;

error:
  { // unmap what was mapped so far
    rerr_t err = vm_map_del(ctx->map, ctx->vaddr, ctx->nmapped);
    assertf(err == 0, "vm_map_del(%012llx, %llu) => %s",
      ctx->vaddr, ctx->nmapped, rerr_str(err));
  }
  return 0;
}


rerr_t vm_map_add(vm_map_t* map, u64* vaddrp, uintptr haddr, u64 npages, vm_perm_t perm) {
  if (npages == 0)
    return 0;

  u64 vaddr = *vaddrp;

  assertf(VM_ADDR_MIN <= vaddr && vaddr <= VM_ADDR_MAX, "invalid vaddr 0x%llx", vaddr);
  assertf(IS_ALIGN2((uintptr)haddr, PAGE_SIZE), "haddr 0x%lx not page aligned", haddr);
  assert(perm != 0); // pte "is mapped" checks relies on vm_pte_t not being zero
  vm_map_assert_locked(map);

  if (VM_ADDR_MIN > vaddr || vaddr > VM_ADDR_MAX ||
      perm == 0 ||
      !IS_ALIGN2((uintptr)haddr, PAGE_SIZE)
  ) {
    return rerr_invalid;
  }

  addctx_t ctx = {
    .map    = map,
    .vaddr  = vaddr,
    .haddr  = haddr,
    .npages = npages,
    .perm   = perm,
  };

  vm_map_iter(map, vaddr, &vm_map_add_visitor, (uintptr)&ctx);

  assert(ctx.nmapped <= ctx.npages);
  if (ctx.nmapped < ctx.npages)
    assertf(ctx.err != 0, "failed to map all pages but did not set error");
  return ctx.err;
}



//—————————————————————————————————————————————————————————————————————————————————————
// vm_map_del


typedef struct {
  u64    npages; // remaining number of pages to unmap
  u64    vaddr;  // start address
  rerr_t err;
} delctx_t;


static u32 vm_map_del_visitor(
  vm_pte_t* table, u32 level, vm_ptab_t ptab, u32 index, u64 vfn, uintptr data)
{
  delctx_t* ctx = (delctx_t*)data;

  if (level < VM_PTAB_LEVELS - 1) {
    // ptab is a table of tables (i.e. ptab[index] is another table)
    // traverse table
    vm_pte_t* pte = &ptab[index];

    trace("unmap L%u table %012llx…%012llx 0x%llx [%u] nuse=%u",
      level+1, VM_VFN_VADDR(vfn), VM_VFN_VADDR(vfn + VM_PTAB_NPAGES(level+1)) - 1,
      VM_PTAB_NPAGES(level+1), index, pte->nuse);

    if UNLIKELY(*(u64*)pte == 0) {
      dlog("vm_map_del vaddr %llx not mapped", VM_VFN_VADDR(vfn));
      goto err_not_found;
    }

    return 1;
  }

  // ptab is a table of pages (i.e. ptab[index] is a page)

  u32 end_index = (u32)MIN((u64)VM_PTAB_LEN, (u64)index + ctx->npages);
  u32 npages = end_index - index;

  trace("unmap %u pages %012llx…%012llx",
    npages, VM_VFN_VADDR(vfn), VM_VFN_VADDR(vfn)+((end_index - index)*PAGE_SIZE)-1);

  if UNLIKELY(table->nuse < npages) {
    dlog("table has fewer pages mapped than expected (%u < %u)", table->nuse, npages);
    goto err_not_found;
  }

  #if DEBUG
    for (u32 i = index; i < end_index; i++)
      assertf(*(u64*)&ptab[i] != 0, "page %llx not mapped", VM_VFN_VADDR(vfn));
  #endif

  // NOTE: a possible optimization here is to just delete the entire table when
  // table->nuse==npages, skipping the memset stage.

  assert(npages <= table->nuse);
  table->nuse -= npages;
  // NOTE: consider freeing unused table when table->nuse==0,
  // which is recursive (i.e. at that point we would also decrement parent->nuse
  // and if that drops to zero, free that one too and so on.)

  // pave entries at ptab[index:end_index]
  memset(&ptab[index], 0, sizeof(ptab[0]) * (usize)npages);

  assert((u64)npages <= ctx->npages);
  ctx->npages -= (u64)npages;
  if (ctx->npages == 0)
    return 0;

  return npages;

err_not_found:
  ctx->err = rerr_not_found;
  return 0;
}


rerr_t vm_map_del(vm_map_t* map, u64 vaddr, u64 npages) {
  if (npages == 0)
    return 0;

  vm_map_assert_locked(map);

  if (VM_ADDR_MIN > vaddr || vaddr > VM_ADDR_MAX) {
    assertf(0, "invalid vaddr 0x%llx", vaddr);
    return rerr_invalid;
  }

  delctx_t ctx = {
    .vaddr  = vaddr,
    .npages = npages,
  };

  vm_map_iter(map, vaddr, &vm_map_del_visitor, (uintptr)&ctx);

  return ctx.err;
}
