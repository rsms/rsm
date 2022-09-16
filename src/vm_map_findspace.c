// virtual memory mapping, finding free space
// See vmem.txt for in-depth documentation
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

#define vmtrace trace
#include "vm.h"

// VM_MAP_FINDSPACE_TRACE: define to enable logging a lot of info via dlog
//#define VM_MAP_FINDSPACE_TRACE


#if (defined(VM_MAP_FINDSPACE_TRACE) || defined(VM_MAP_TRACE) || defined(VM_TRACE)) && \
    defined(DEBUG)
  #undef  VM_MAP_FINDSPACE_TRACE
  #define VM_MAP_FINDSPACE_TRACE
  #define trace(fmt, args...) dlog("[vm_map] " fmt, ##args)
#else
  #ifdef VM_MAP_FINDSPACE_TRACE
    #warning VM_MAP_FINDSPACE_TRACE has no effect unless DEBUG is enabled
    #undef VM_MAP_FINDSPACE_TRACE
  #endif
  #define trace(...) ((void)0)
#endif


typedef struct {
  vm_ptab_t ptab;
  u32       nuse;
} findspace_nuse_t;

typedef struct {
  u64              want_npages;  // npages passed to vm_map_findspace
  u64              found_npages; // number of free consecutive pages found so far
  u64              start_vaddr;  // address of first free page
  findspace_nuse_t nuse[VM_PTAB_LEVELS];
} findspace_t;


static u32 vm_map_findspace_visitor(
  vm_pte_t* parent, u32 level, vm_ptab_t ptab, u32 index, u64 vfn, uintptr data)
{
  findspace_t* ctx = (findspace_t*)data;
  findspace_nuse_t* curr_nuse = &ctx->nuse[level];
  u32 advance;

  if (curr_nuse->ptab != ptab) {
    curr_nuse->ptab = ptab;
    curr_nuse->nuse = 0;
  }

  // visitor for free entries lives in separate function
  if (*(u64*)&ptab[index]) {
    // entry ptab[index] is not free
    ctx->start_vaddr = 0;
    ctx->found_npages = 0;
    // skip past immediate used entries (terminal level only)
    u32 i = index + 1;
    if (level == VM_PTAB_LEVELS-1) {
      while (i < VM_PTAB_LEN && *(u64*)&ptab[i])
        i++;
    }
    advance = i - index;
    curr_nuse->nuse += advance;
    return advance;
  }

  // entry ptab[index] is free, unused

  if (curr_nuse->nuse == parent->nuse) {
    // remainder of ptab entires are free (we have passed all used entries)
    advance = VM_PTAB_LEN - index;
  } else {
    // scan forward to the next used entry, or end of ptab
    u64 need_npages = ctx->want_npages - ctx->found_npages;
    const u32 end_index = (u32)MIN(
      (u64)VM_PTAB_LEN,
      (u64)index + IDIV_CEIL(need_npages, VM_PTAB_NPAGES(level+1)) );
    u32 i = index;
    while (i < end_index && *(u64*)&ptab[i] == 0)
      i++;
    advance = i - index;
  }

  if (ctx->start_vaddr == 0)
    ctx->start_vaddr = VM_VFN_VADDR(vfn);

  ctx->found_npages += VM_PTAB_NPAGES(level+1) * (u64)advance;

  // return 0 if found_npages>=want_npages, else return advance
  return advance * (ctx->found_npages < ctx->want_npages);
}


rerr_t vm_map_findspace(vm_map_t* map, u64* vaddrp, u64 npages) {
  dlog("————————————————— %s —————————————————", __FUNCTION__);
  vm_map_assert_rlocked(map);

  // *vaddrp holds the minimum desired address
  u64 vaddr = *vaddrp;
  if (vaddr < VM_ADDR_MIN)
    vaddr = VM_ADDR_MIN;

  // is vaddr+npages beyond end of address range?
  if UNLIKELY(
      npages > VM_ADDR_MAX/PAGE_SIZE ||
      vaddr/PAGE_SIZE > VM_ADDR_MAX/PAGE_SIZE - npages)
  {
    return rerr_nomem;
  }

  // adjust start to minimum free VFN
  if (VM_VFN(vaddr) < map->min_free_vfn)
    vaddr = VM_VFN_VADDR(map->min_free_vfn);

  dlog("vm_map_iter(start_vaddr=%llx, want_npages=%llu)", vaddr, npages);
  findspace_t ctx = { .want_npages = npages };
  vm_map_iter(map, vaddr, &vm_map_findspace_visitor, (uintptr)&ctx);

  if (ctx.found_npages < npages)
    return rerr_nomem;

  dlog("vm_map_findspace\n"
    "  expected min %10llu pages at >=%012llx\n"
    "  found        %10llu pages at   %012llx…%012llx\n",
    npages, VM_PAGE_ADDR(0xfacedeba4eef),
    ctx.found_npages,
    ctx.start_vaddr, ctx.start_vaddr + ((ctx.found_npages-1) * PAGE_SIZE) );

  *vaddrp = ctx.start_vaddr;
  return 0;
}
