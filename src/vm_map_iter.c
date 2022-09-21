// virtual memory mapping iteration
// See vmem.txt for in-depth documentation
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

#define vmtrace trace
#include "vm.h"

// VM_MAP_ITER_TRACE: define to enable logging a lot of info via dlog
//#define VM_MAP_ITER_TRACE


#if (defined(VM_MAP_ITER_TRACE) || defined(VM_MAP_TRACE) || defined(VM_TRACE)) && \
    defined(DEBUG) && !defined(RSM_NO_LIBC)
  #undef  VM_MAP_ITER_TRACE
  #define VM_MAP_ITER_TRACE
  #define trace(fmt, args...) log("\e[1;33m▍[vm_map_iter]\e[0m " fmt, ##args)
  #define tracex(fmt, args...) trace("%*s" fmt, (int)level*2, "", ##args)
  #define trace_iter(args...) _trace_iter(args, __FUNCTION__)
  static void _trace_iter(u64 vfn, u32 level, vm_ptab_t ptab, u32 index, const char* fn) {
    u64 npages = VM_PTAB_NPAGES(level+1);
    if (level == VM_PTAB_LEVELS-1) {
      log("\e[1;33m▍%*s%s page\e[39m %012llx L%u[%u] %s\e[0m",
        (int)level*2, "", fn,
        VM_VFN_VADDR(vfn), level, index, (*(u64*)&ptab[index] ? "in use" : "free"));
    } else {
      u64 last_vaddr = VM_VFN_VADDR(vfn + npages - 1);
      log("\e[1;33m▍%*s%s L%u\e[39m %012llx…%012llx 0x%llx L%u[%u] nuse=%u\e[0m",
        (int)level*2, "", fn,
        level+1, VM_VFN_VADDR(vfn), last_vaddr, npages, level, index,
        ptab[index].table.nuse);
    }
  }
#else
  #ifdef VM_MAP_ITER_TRACE
    #if !defined(DEBUG)
      #warning VM_MAP_ITER_TRACE has no effect unless DEBUG is enabled
    #elif defined(RSM_NO_LIBC)
      #warning VM_MAP_ITER_TRACE has no effect when RSM_NO_LIBC is defined
    #endif
    #undef VM_MAP_ITER_TRACE
  #endif
  #define trace(...) ((void)0)
  #define tracex(...) ((void)0)
  #define trace_iter(...) ((void)0)
#endif



#if DEBUG
  #define fmtbitsval(value) ({ \
    __typeof__(value) val__ = (value); \
    fmtbits(&val__, sizeof(val__)*8); \
  })
  UNUSED static const char* fmtbits(const void* bits, usize len) {
    static char bufs[2][128];
    static int nbuf = 0;
    char* buf = bufs[nbuf++ % 2];
    assert(len <= sizeof(bufs[0]) - 1);
    buf += sizeof(bufs[0]) - 1 - len;
    memset(buf, '0', len);
    buf[len] = 0;
    for (usize i = 0; len--; i++)
      buf[i] = '0' + !!( ((u8*)bits)[len / 8] & (1lu << (len % 8)) );
    return buf;
  }
#endif


// vm_map_iter0 iterates over all entries in ptab
// returns true if caller should keep going
static bool vm_map_iter0(
  vm_map_iter_f callback,   // user callback
  uintptr       userdata,   // user data for callback
  vm_table_t*   parent,
  u64           vfn,        // parent VFN
  u32           level)      // level of page table (root = 0)
{
  u64 npages = VM_PTAB_NPAGES(level+1);
  vm_ptab_t ptab = vm_table_ptab(parent);

  u32 end_index = VM_PTAB_LEN;

  // when we are visiting the last table of pages, there is one too many pages at the
  // end as a result from VFN being offset by (VM_ADDR_MIN/PAGE_SIZE).
  if (VM_ADDR_MIN > 0) {
    // branchless version of
    //   u64 end_vfn = vfn + VM_PTAB_NPAGES(level) - 1;
    //   if (level == VM_PTAB_LEVELS-1 && end_vfn == VM_VFN_MAX + 1)
    //     end_index--;
    end_index -= (
      (level == VM_PTAB_LEVELS-1) &
      (vfn + VM_PTAB_NPAGES(level) - 1 == VM_VFN_MAX + 1) );
  }

  for (u32 index = 0; index < end_index;) {
    trace_iter(vfn, level, ptab, index);

    u32 index_incr = callback(parent, level, ptab, index, vfn, userdata);
    if (index_incr == 0)
      return false;

    // visit sub table
    vm_table_t* subtable = &ptab[index].table;
    if ((level < VM_PTAB_LEVELS-1) & (*(u64*)subtable != 0)) {
      if (!vm_map_iter0(callback, userdata, subtable, vfn, level+1))
        return false;
    }

    assert_no_add_overflow(index, index_incr);
    index += index_incr;
    vfn += (u64)index_incr * npages;
    // visit next table
  }

  return true;
}


// vm_map_iter1 iterates over pages in ptab, starting at start_vfn.
// returns true if caller should keep going
static bool vm_map_iter1(
  vm_map_iter_f callback,   // user callback
  uintptr       userdata,   // user data for callback
  u64           start_vfn,  // minimum VFN to start calling the callback for
  u64           index_vfn,
  vm_table_t*   parent,
  u32           level)      // level of page table (root = 0)
{
  u32 index = (u32)(index_vfn >> (64 - VM_PTAB_BITS));

  // VFN 0xfacedeadbeef
  //     0000000000000000000000000000 111110101 100111011 011110101 011011010
  //                                  L1        L2        L3        L4
  // #ifdef VM_MAP_ITER_TRACE
  //   tracex("start_vfn 0x%09llx (vaddr %012llx)",start_vfn,VM_VFN_VADDR(start_vfn));
  //   tracex("          %s", fmtbitsval(start_vfn));
  //   tracex("index     %-3u%*s",
  //     index,
  //     64-(VM_VFN_BITS - VM_PTAB_BITS - (level * VM_PTAB_BITS))-3,
  //     fmtbits(&index, VM_PTAB_BITS));
  // #endif

  u64 npages = VM_PTAB_NPAGES(level+1); // i.e. L1=0x8000000, L2=0x40000, L3=0x200, L4=1
  u64 vfn = VM_BLOCK_VFN(start_vfn, level);
  assertf(vfn + npages > start_vfn, "block ends before index (index_vfn bug)");
  vm_ptab_t ptab = vm_table_ptab(parent);

  trace_iter(vfn, level, ptab, index);

  u32 index_incr = callback(parent, level, ptab, index, start_vfn, userdata);
  if (index_incr == 0)
    return false;

  // visit sub table
  vm_table_t* subtable = &ptab[index].table;
  if ((level < VM_PTAB_LEVELS-1) & (*(u64*)subtable != 0)) {
    index_vfn <<= VM_PTAB_BITS;
    if (!vm_map_iter1(callback, userdata, start_vfn, index_vfn, subtable, level+1))
      return false;
  }

  assert_no_add_overflow(index, index_incr);
  index += index_incr;
  vfn += (u64)index_incr * npages;

  u32 end_index = VM_PTAB_LEN;

  // when we are visiting the last table of pages, there is one too many pages at the
  // end as a result from VFN being offset by (VM_ADDR_MIN/PAGE_SIZE).
  if (VM_ADDR_MIN > 0) {
    // branchless version of
    //   u64 end_vfn = vfn + VM_PTAB_NPAGES(level) - 1;
    //   if (level == VM_PTAB_LEVELS-1 && end_vfn == VM_VFN_MAX + 1)
    //     end_index--;
    end_index -= (
      (level == VM_PTAB_LEVELS-1) &
      (vfn + VM_PTAB_NPAGES(level) - 1 == VM_VFN_MAX + 1) );
  }

  while (index < end_index) {
    trace_iter(vfn, level, ptab, index);

    u32 index_incr = callback(parent, level, ptab, index, vfn, userdata);
    if (index_incr == 0)
      return false;

    // visit sub table
    vm_table_t* subtable = &ptab[index].table;
    if ((level < VM_PTAB_LEVELS-1) & (*(u64*)subtable != 0)) {
      if (!vm_map_iter0(callback, userdata, subtable, vfn, level+1))
        return false;
    }

    assert_no_add_overflow(index, index_incr);
    index += index_incr;
    vfn += (u64)index_incr * npages;
  }

  return true;
}


void vm_map_iter(vm_map_t* map, u64 start_vaddr, vm_map_iter_f* fn, uintptr data) {
  assertf(start_vaddr >= VM_ADDR_MIN && VM_ADDR_MAX >= start_vaddr,
    "invalid vaddr 0x%llx", start_vaddr);

  trace("iter start 0x%llx (VFN %09llx)", start_vaddr, VM_VFN(start_vaddr));

  vm_table_t parent = { .nuse = map->root_nuse };
  vm_table_set_ptab(&parent, map->root);

  u64 vfn = VM_VFN(start_vaddr);
  if (vfn == 0) {
    vm_map_iter0(fn, data, &parent, 0, 0);
  } else {
    u64 index_vfn = vfn << ((sizeof(vfn)*8) - VM_VFN_BITS);
    vm_map_iter1(fn, data, vfn, index_vfn, &parent, 0);
  }

  map->root_nuse = parent.nuse;
}
