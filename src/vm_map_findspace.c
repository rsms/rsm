// virtual memory mapping, finding free space
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#define vmtrace trace
#include "vm.h"

// VM_MAP_FINDSPACE_TRACE: define to enable logging a lot of info via dlog
#define VM_MAP_FINDSPACE_TRACE


#if (defined(VM_MAP_FINDSPACE_TRACE) || defined(VM_MAP_TRACE) || defined(VM_TRACE)) && \
    defined(DEBUG)
  #undef  VM_MAP_FINDSPACE_TRACE
  #define VM_MAP_FINDSPACE_TRACE
  #define trace(fmt, args...) dlog("[vm_map] " fmt, ##args)
  static void trace_table(vm_table_t* table, u32 level, u64 vfn) {
    u64 block_vfn = VM_BLOCK_VFN(vfn, level);
    u64 vfn_offset = vfn > block_vfn ? vfn - block_vfn : 0;
    trace("L%u at %012llx is %s (offset 0x%llx, nuse %u)",
      level+1, VM_VFN_VADDR(block_vfn),
      ( (*(u64*)table == 0)          ? "empty" :
        (table->nuse == VM_PTAB_LEN) ? "full" :
                                     "partial"
      ),
      vfn_offset << PAGE_SIZE_BITS, table->nuse);
  }
#else
  #ifdef VM_MAP_FINDSPACE_TRACE
    #warning VM_MAP_FINDSPACE_TRACE has no effect unless DEBUG is enabled
    #undef VM_MAP_FINDSPACE_TRACE
  #endif
  #define trace(...)       ((void)0)
  #define trace_table(...) ((void)0)
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


static void visit_page_table(vm_table_t* table, u64 vfn, findspace_t* ctx) {
  vm_ptab_t ptab = vm_table_ptab(table);
  u32 end_index = vm_ptab_page_end_index(vfn);
  u32 free_index = 0; // start of range of free pages

  for (u32 i = 0; i < end_index; i++) {
    //dlog("page %012llx %s", VM_VFN_VADDR(vfn + i), *(u64*)&ptab[i] ? "used" : "free");

    if (*(u64*)&ptab[i] == 0) // page is free
      continue;

    // check if we found enough free pages already
    u64 found_npages = (u64)(end_index - free_index);
    if (ctx->found_npages + found_npages >= ctx->want_npages) {
      ctx->found_npages += found_npages;
      return;
    }

    // fast forward to the next free page
    while (i < end_index && *(u64*)&ptab[i])
      i++;

    ctx->found_npages = 0;
    free_index = i;

    if (i == end_index) {
      // all remaining pages are used
      ctx->start_vaddr = 0;
      return;
    }

    ctx->start_vaddr = VM_VFN_VADDR(vfn + i);
  }

  // when we get here we found at least one free page in the tail of the table
  assert((end_index - free_index) > 0);
  ctx->found_npages += (u64)(end_index - free_index);
}


static u32 visit_table(
  vm_table_t* table, u32 level, vm_ptab_t ptab, u32 index, u64 vfn, uintptr data)
{
  findspace_t* ctx = (findspace_t*)data;

  findspace_nuse_t* curr_nuse = &ctx->nuse[level];
  if (curr_nuse->ptab != ptab) {
    curr_nuse->ptab = ptab;
    curr_nuse->nuse = 0;
  }

  u64 block_vfn_mask = VM_BLOCK_VFN_MASK(level);
  u64 block_npages = VM_PTAB_NPAGES(level+1);
  u32 i = index;

  u64 block_vfn = vfn & block_vfn_mask;
  u64 vfnoffs = (vfn - block_vfn) * (vfn > block_vfn);

  for (;i < VM_PTAB_LEN; i++, vfn = (vfn & block_vfn_mask) + block_npages, vfnoffs = 0) {
    assertf(level < VM_PTAB_LEVELS-1, "unexpected page; expected a table");
    vm_table_t* subtable = &ptab[i].table;
    trace_table(subtable, level, vfn);

    // full table
    if (subtable->nuse == VM_PTAB_LEN) {
      ctx->start_vaddr = 0;
      ctx->found_npages = 0;
      continue;
    }

    // unused table
    if (*(u64*)subtable == 0) {
      ctx->found_npages += block_npages - vfnoffs;
      if (ctx->start_vaddr == 0)
        ctx->start_vaddr = VM_VFN_VADDR(vfn);
      if (ctx->found_npages >= ctx->want_npages)
        return 0;
      continue;
    }

    // Table of tables needs to be traversed.
    // Break out of the loop and advance iterator.
    if (level < VM_PTAB_LEVELS-2) {
      if (i == index) i++; // guarantee advance
      break;
    }

    // table of pages (second to last level)
    visit_page_table(subtable, vfn, ctx);
    if (ctx->found_npages >= ctx->want_npages)
      return 0;
  }

  // advance iterator by the number of entries we scanned
  return i - index;
}


rerr_t vm_map_findspace(vm_map_t* map, u64* vaddrp, u64 npages) {
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

  trace("vm_map_iter(start_vaddr=%llx, want_npages=%llu)", vaddr, npages);
  findspace_t ctx = { .want_npages = npages };
  vm_map_iter(map, vaddr, &visit_table, (uintptr)&ctx);

  if (ctx.found_npages < npages)
    return rerr_nomem;

  trace("vm_map_findspace\n"
    "  expected min %10llu pages at >=%012llx\n"
    "  found        %10llu pages at   %012llx…%012llx\n",
    npages, VM_PAGE_ADDR(vaddr),
    ctx.found_npages,
    ctx.start_vaddr, ctx.start_vaddr + ((ctx.found_npages-1) * PAGE_SIZE) );

  assert(vaddr <= ctx.start_vaddr);
  *vaddrp = ctx.start_vaddr;
  return 0;
}
