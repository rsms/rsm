// virtual memory mapping
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#define vmtrace trace
#include "vm.h"

// VM_MAP_ADD_TRACE: define to enable logging a lot of info via dlog
//#define VM_MAP_ADD_TRACE

#if (defined(VM_MAP_ADD_TRACE) || defined(VM_MAP_TRACE) || defined(VM_TRACE)) && \
    defined(DEBUG)
  #undef  VM_MAP_ADD_TRACE
  #define VM_MAP_ADD_TRACE
  #define trace(fmt, args...) dlog("[vm_map_add] " fmt, ##args)
  static void trace_table(vm_table_t* table, u32 level, u64 vfn) {
    u64 block_vfn = VM_BLOCK_VFN(vfn, level);
    u64 vfn_offset = vfn > block_vfn ? vfn - block_vfn : 0;
    trace("L%u %012llx is %s (offset 0x%llx, nuse %u)",
      level+1, VM_VFN_VADDR(block_vfn),
      ( (table->nuse == 0)           ? "empty" :
        (table->nuse == VM_PTAB_LEN) ? "full" :
                                       "partial"
      ),
      vfn_offset << PAGE_SIZE_BITS, table->nuse);
  }
#else
  #ifdef VM_MAP_ADD_TRACE
    #warning VM_MAP_ADD_TRACE has no effect unless DEBUG is enabled
    #undef VM_MAP_ADD_TRACE
  #endif
  #define trace(...)       ((void)0)
  #define trace_table(...) ((void)0)
#endif


typedef struct {
  vm_map_t* map;
  u64       haddr;         // optional backing pages (count is same as vaddr)
  u64       need_npages;   // total number of pages to map
  u64       mapped_npages; // number of pages mapped so far
  vm_perm_t perm;          // permissions for mapped PTEs

  // fields only used in case of error
  u64    vaddr; // start address
  rerr_t err;
} addctx_t;


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


static u32 end_with_error(addctx_t* ctx, rerr_t err) {
  trace("revert what was mapped so far");
  ctx->err = err;
  UNUSED rerr_t err2 = vm_map_del(ctx->map, ctx->vaddr, ctx->mapped_npages);
  assertf(!err2, "vm_map_del(%012llx, %llu) => %s",
    ctx->vaddr, ctx->mapped_npages, rerr_str(err2));
  return 0;
}


#if DEBUG
  static void assert_nuse_integrity(vm_table_t* table, u32 level, u64 vfn) {
    u32 actual_nuse = 0;
    vm_ptab_t ptab = vm_table_ptab(table);
    for (u32 index = 0; index < vm_ptab_page_end_index(vfn); index++)
      actual_nuse += (*(u64*)&ptab[index] != 0);
    assertf(actual_nuse == table->nuse,
      "incorrect table.nuse=%u L%u %012llx (actual nuse %u)",
      table->nuse, level, VM_VFN_VADDR(vfn), actual_nuse);
  }
#else
  #define assert_nuse_integrity(...)
#endif


static rerr_t map_pages(vm_table_t* table, vm_ptab_t ptab, u64 vfn, addctx_t* ctx) {
  u32 index = vm_vfn_ptab_index(vfn, VM_PTAB_LEVELS-1);
  u64 end_index_need = (ctx->need_npages - ctx->mapped_npages) + (u64)index;

  u32 end_index = vm_ptab_page_end_index(vfn);
  if (end_index_need < (u64)end_index)
    end_index = (u32)end_index_need;

  u32 npages = end_index - index;
  u64 haddr = ctx->haddr;

  // fail if the table has less free space than requested
  if UNLIKELY(table->nuse > VM_PTAB_LEN - npages) {
    #if DEBUG
    for (u32 i = index; i < end_index; i++) {
      if UNLIKELY(*(u64*)&ptab[i] != 0) {
        dlog("vaddr %012llx already mapped", VM_VFN_VADDR(vfn + i));
        break;
      }
    }
    assert_nuse_integrity(table, VM_PTAB_LEVELS-1, vfn);
    #endif
    return rerr_exists;
  }

  assert(table->nuse + npages <= VM_PTAB_LEN);

  #ifdef VM_MAP_ADD_TRACE
    u64 vaddr = VM_VFN_VADDR(vfn);
    if (haddr) {
      trace("map %u pages L%u[%u:%u] %012llx…%012llx %s => %llx…%llx",
        npages, VM_PTAB_LEVELS-1, index, end_index,
        vaddr, vaddr+((u64)npages*PAGE_SIZE)-PAGE_SIZE, vm_perm_str(ctx->perm),
        haddr, haddr+((u64)npages*PAGE_SIZE)-PAGE_SIZE);
    } else {
      trace("map %u pages L%u[%u:%u] %012llx…%012llx %s",
        npages, VM_PTAB_LEVELS-1, index, end_index,
        vaddr, vaddr+((u64)npages*PAGE_SIZE)-PAGE_SIZE, vm_perm_str(ctx->perm));
    }
  #endif

  for (u32 i = index; i < end_index; i++) {
    if UNLIKELY(*(u64*)&ptab[i] != 0) {
      dlog("vaddr %012llx already mapped", VM_VFN_VADDR(vfn + i));
      ctx->mapped_npages += (u64)(i - index); // for end_with_error & vm_map_del
      return rerr_exists;
    }

    // sets all permission bits in one store
    *(u8*)&ptab[i].page = ctx->perm;
    // backing page address (haddr may be 0)
    vm_page_set_haddr(&ptab[i].page, haddr);
    haddr += PAGE_SIZE * !!haddr; // no-op in case haddr==0
  }

  ctx->mapped_npages += (u64)npages;
  ctx->haddr += (u64)npages * PAGE_SIZE * !!haddr;

  assert_no_add_overflow(table->nuse, npages);
  assert(table->nuse + npages <= VM_PTAB_LEN);
  table->nuse += npages;

  return 0;
}


static rerr_t map_new_table(
  vm_table_t* table, u32 level, vm_ptab_t ptab, u32 index, u64 vfn, addctx_t* ctx)
{
  assert(level < VM_PTAB_LEVELS-1);
  assertf(index == vm_vfn_ptab_index(vfn, level),
    "%u == %u", index, vm_vfn_ptab_index(vfn, level));

  vm_table_t* subtable = &ptab[index].table;

  vm_ptab_t newptab = vm_ptab_alloc(ctx->map->mm);
  if UNLIKELY(!newptab)
    return rerr_nomem;

  vm_table_set_ptab(subtable, newptab);
  table->nuse++;

  // trace("allocated L%u vm_ptab_t %p at L%u[%u] ptab#%p",
  //   level+1, newptab, level, index, ptab);

  // map pages
  if (level == VM_PTAB_LEVELS-2)
    return map_pages(subtable, newptab, vfn, ctx);

  // create & map sub tables
  level++;
  u64 block_vfn_mask = VM_BLOCK_VFN_MASK(level);
  u64 block_npages = VM_PTAB_NPAGES(level+1);
  index = vm_vfn_ptab_index(vfn, level);
  for (;;) {
    trace_table(subtable, level, vfn);
    rerr_t err = map_new_table(subtable, level, newptab, index, vfn, ctx);
    if (err || ctx->mapped_npages >= ctx->need_npages)
      return err;
    index++;
    vfn = (vfn & block_vfn_mask) + block_npages;
  }
}


static u32 visit_table(
  vm_table_t* table, u32 level, vm_ptab_t ptab, u32 index, u64 vfn, uintptr data)
{
  addctx_t* ctx = (addctx_t*)data;

  u64 block_vfn_mask = VM_BLOCK_VFN_MASK(level);
  u64 block_npages = VM_PTAB_NPAGES(level+1);
  u32 i = index;
  rerr_t err;

  for (;i < VM_PTAB_LEN; i++, vfn = (vfn & block_vfn_mask) + block_npages) {
    assert(level < VM_PTAB_LEVELS-1);
    vm_table_t* subtable = &ptab[i].table;
    trace_table(subtable, level, vfn);

    if (*(u64*)subtable == 0) {
      // missing table
      if UNLIKELY(err = map_new_table(table, level, ptab, i, vfn, ctx))
        return end_with_error(ctx, err);
    } else {
      // table of tables needs to be traversed
      if (level < VM_PTAB_LEVELS-2) {
        i++;
        break;
      }

      // page table
      if UNLIKELY(err = map_pages(subtable, vm_table_ptab(subtable), vfn, ctx))
        return end_with_error(ctx, err);
    }

    if (ctx->mapped_npages >= ctx->need_npages) {
      trace("done");
      return 0;
    }
  }

  trace("advance %u", i - index);
  return i - index;
}


rerr_t vm_map_add(vm_map_t* map, u64 vaddr, uintptr haddr1, u64 npages, vm_perm_t perm) {
  if (npages == 0)
    return 0;

  vaddr = VM_PAGE_ADDR(vaddr);
  u64 haddr = VM_PAGE_ADDR((u64)haddr1);

  vm_map_assert_locked(map);

  if ((VM_ADDR_MIN > vaddr) | (vaddr > VM_ADDR_MAX) | (perm == 0)) {
    assertf(VM_ADDR_MIN <= vaddr && vaddr <= VM_ADDR_MAX, "invalid vaddr 0x%llx", vaddr);
    assertf(perm != 0, "zero permissions");
    return rerr_invalid;
  }

  if (haddr) {
    trace("map %llu pages at %012llx…%012llx %s => %llx…%llx",
      npages, vaddr, vaddr + (npages-1)*PAGE_SIZE, vm_perm_str(perm),
      haddr, haddr + (npages-1)*PAGE_SIZE );
  } else {
    trace("map %llu pages at %012llx…%012llx %s",
      npages, vaddr, vaddr + (npages-1)*PAGE_SIZE, vm_perm_str(perm) );
  }

  addctx_t ctx = {
    .map         = map,
    .vaddr       = vaddr,
    .haddr       = haddr,
    .need_npages = npages,
    .perm        = perm,
  };

  vm_map_iter(map, vaddr, &visit_table, (uintptr)&ctx);

  assert(ctx.mapped_npages <= ctx.need_npages);
  if (ctx.mapped_npages < ctx.need_npages)
    assertf(ctx.err != 0, "failed to map all pages but did not set error");
  return ctx.err;
}
