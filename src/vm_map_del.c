// virtual memory mapping
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#define vmtrace trace
#include "vm.h"

// VM_MAP_DEL_TRACE: define to enable logging a lot of info via dlog
//#define VM_MAP_DEL_TRACE

#if (defined(VM_MAP_DEL_TRACE) || defined(VM_MAP_TRACE) || defined(VM_TRACE)) && \
    defined(DEBUG)
  #undef  VM_MAP_DEL_TRACE
  #define VM_MAP_DEL_TRACE
  #define trace(fmt, args...) dlog("[vm_map_del] " fmt, ##args)
  static void trace_table(vm_table_t* table, u32 level, u64 vfn) {
    u64 block_vfn = VM_BLOCK_VFN(vfn, level);
    u64 vfn_offset = vfn > block_vfn ? vfn - block_vfn : 0;
    trace("L%u %012llx is %s (offset 0x%llx, nuse %u)",
      level+1, VM_VFN_VADDR(block_vfn),
      ( (*(u64*)table == 0)          ? "empty" :
        (table->nuse == VM_PTAB_LEN) ? "full" :
                                     "partial"
      ),
      vfn_offset << PAGE_SIZE_BITS, table->nuse);
  }
#else
  #ifdef VM_MAP_DEL_TRACE
    #warning VM_MAP_DEL_TRACE has no effect unless DEBUG is enabled
    #undef VM_MAP_DEL_TRACE
  #endif
  #define trace(...)       ((void)0)
  #define trace_table(...) ((void)0)
#endif


typedef struct {
  u64    npages; // remaining number of pages to unmap
  u64    vaddr;  // start address
  rerr_t err;
} delctx_t;


static rerr_t unmap_pages(vm_table_t* table, vm_ptab_t ptab, u64 vfn, delctx_t* ctx) {
  u32 index = vm_vfn_ptab_index(vfn, VM_PTAB_LEVELS-1);
  u64 end_index_need = ctx->npages + (u64)index;

  u32 end_index = vm_ptab_page_end_index(vfn);
  if (end_index_need < (u64)end_index)
    end_index = (u32)end_index_need;

  u32 npages = end_index - index;

  #ifdef VM_MAP_DEL_TRACE
    u64 vaddr = VM_VFN_VADDR(vfn);
    trace("unmap %u pages L%u[%u:%u] %012llx…%012llx",
      npages, VM_PTAB_LEVELS-1, index, end_index,
      vaddr, vaddr+((u64)npages*PAGE_SIZE)-PAGE_SIZE);
  #endif

  #if DEBUG
    for (u32 i = index; i < end_index; i++) {
      if (*(u64*)&ptab[i] == 0)
        dlog("[vm_map_del] page %012llx is not mapped", VM_VFN_VADDR(vfn + i));
    }
  #endif

  // pave entries at ptab[index:end_index]
  memset(&ptab[index], 0, sizeof(ptab[0]) * (usize)npages);

  if UNLIKELY(table->nuse < npages) {
    table->nuse = 0;
    return rerr_not_found;
  }
  table->nuse -= npages;

  assert(ctx->npages >= (u64)npages);
  ctx->npages -= (u64)npages;

  return 0;
}


static u32 visit_table(
  vm_table_t* table, u32 level, vm_ptab_t ptab, u32 index, u64 vfn, uintptr data)
{
  delctx_t* ctx = (delctx_t*)data;

  u64 block_vfn_mask = VM_BLOCK_VFN_MASK(level);
  u64 block_npages = VM_PTAB_NPAGES(level+1);
  u32 i = index;

  for (;i < VM_PTAB_LEN; i++, vfn = (vfn & block_vfn_mask) + block_npages) {
    assert(level < VM_PTAB_LEVELS-1);
    vm_table_t* subtable = &ptab[i].table;
    trace_table(subtable, level, vfn);

    if UNLIKELY(*(u64*)subtable == 0) {
      dlog("[vm_map_del] vaddr %llx not mapped", VM_VFN_VADDR(vfn));
      ctx->err = rerr_not_found;
      return 0;
    }

    // table of tables needs to be traversed
    if (level < VM_PTAB_LEVELS-2) {
      i++;
      break;
    }

    // page table
    rerr_t err = unmap_pages(subtable, vm_table_ptab(subtable), vfn, ctx);
    if UNLIKELY(err) {
      ctx->err = err;
      return 0;
    }
    if (ctx->npages == 0) {
      trace("done");
      return 0;
    }
  }

  trace("advance %u", i - index);
  return i - index;
}


rerr_t vm_map_del(vm_map_t* map, u64 vaddr, u64 npages) {
  if (npages == 0)
    return 0;

  vaddr = VM_PAGE_ADDR(vaddr);

  vm_map_assert_locked(map);

  if ((VM_ADDR_MIN > vaddr) | (vaddr > VM_ADDR_MAX)) {
    assertf(0, "invalid vaddr 0x%llx", vaddr);
    return rerr_invalid;
  }

  trace("unmap %llu pages at %012llx…%012llx",
    npages, vaddr, vaddr + (npages-1)*PAGE_SIZE);

  delctx_t ctx = {
    .vaddr  = vaddr,
    .npages = npages,
  };

  vm_map_iter(map, vaddr, &visit_table, (uintptr)&ctx);

  return ctx.err;
}
