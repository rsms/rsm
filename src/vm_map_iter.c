// virtual memory mapping iteration
// See vmem.txt for in-depth documentation
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

#define vmtrace trace
#include "vm.h"

// VM_MAP_ITER_TRACE: define to enable logging a lot of info via dlog
//#define VM_MAP_ITER_TRACE


#if (defined(VM_MAP_ITER_TRACE) || defined(VM_MAP_TRACE) || defined(VM_TRACE)) && \
    defined(DEBUG)
  #undef  VM_MAP_ITER_TRACE
  #define VM_MAP_ITER_TRACE
  #define trace(fmt, args...) dlog("[vm_map] " fmt, ##args)
#else
  #ifdef VM_MAP_ITER_TRACE
    #warning VM_MAP_ITER_TRACE has no effect unless DEBUG is enabled
    #undef VM_MAP_ITER_TRACE
  #endif
  #define trace(...) ((void)0)
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
  vm_pte_t*     parent,
  u32           level)      // level of page table (root = 0)
{
  #undef tracex
  #define tracex(fmt, args...) trace("%*s" fmt, (int)level*2, "", ##args)

  u32 bits = (VM_PTAB_BITS*(VM_PTAB_LEVELS-1)) - (level * VM_PTAB_BITS);
  u64 vfn_mask = ~0llu ^ ((1llu << bits)-1);
  u64 npages = VM_PTAB_NPAGES(level+1);
  vm_ptab_t ptab = vm_pte_outaddr(parent);

  for (u32 index = 0; index < VM_PTAB_LEN;) {
    u64 vfn = ((u64)index * npages);

    #ifdef VM_MAP_ITER_TRACE
      {
        vm_pte_t* pte = &ptab[index];
        if (level == VM_PTAB_LEVELS-1) {
          tracex("iter_visit page 0x%012llx [%u] %s",
            VM_VFN_VADDR(vfn), index, (*(u64*)pte ? "mapped" : "free"));
        } else {
          u64 last_vaddr = VM_VFN_VADDR(((vfn + npages) & vfn_mask)) - 1;
          if (level == 0) {
            tracex("iter_visit table L1 %012llx…%012llx 0x%llx [%u] nuse=%u",
              VM_VFN_VADDR(vfn), last_vaddr, npages, index, pte->nuse);
          } else {
            tracex("iter_visit table L%u %012llx…%012llx 0x%llx [%u] nuse=%u",
              level+1, VM_VFN_VADDR(vfn), last_vaddr, npages, index, pte->nuse);
          }
        }
      }
    #endif

    u32 index_incr = callback(parent, level, ptab, index, vfn, userdata);
    assert_no_add_overflow(index, index_incr);
    if (index_incr == 0)
      return false;

    vm_pte_t* pte = &ptab[index];
    index += index_incr;

    if ((level < VM_PTAB_LEVELS-1) & (*(u64*)pte != 0)) {
      // visit sub table
      if (!vm_map_iter0(callback, userdata, pte, level+1))
        return false;
    }

    // visit next table
  } // while (index < VM_PTAB_LEN)

  return true;
}


// vm_map_iter1 iterates over pages in ptab, starting at start_vfn.
// returns true if caller should keep going
static bool vm_map_iter1(
  vm_map_iter_f callback,   // user callback
  uintptr       userdata,   // user data for callback
  u64           start_vfn,  // minimum VFN to start calling the callback for
  u64           index_vfn,
  vm_pte_t*     parent,
  u32           level)      // level of page table (root = 0)
{
  #undef tracex
  #define tracex(fmt, args...) trace("%*s" fmt, (int)level*2, "", ##args)

  u32 start_index = (u32)(index_vfn >> (64 - VM_PTAB_BITS));

  // VFN 0xfacedeadbeef
  //     0000000000000000000000000000 111110101 100111011 011110101 011011010
  //                                  L1        L2        L3        L4
  // #ifdef VM_MAP_ITER_TRACE
  //   tracex("start_vfn   0x%09llx (vaddr %012llx)",start_vfn,VM_VFN_VADDR(start_vfn));
  //   tracex("            %s", fmtbitsval(start_vfn));
  //   tracex("start_index %-3u%*s",
  //     start_index,
  //     64-(VM_VFN_BITS - VM_PTAB_BITS - (level * VM_PTAB_BITS))-3,
  //     fmtbits(&start_index, VM_PTAB_BITS));
  // #endif

  // bits is the inverse count, i.e. L1=27, L2=18, L3=9, L4=0
  u32 bits = (VM_PTAB_BITS*(VM_PTAB_LEVELS-1)) - (level * VM_PTAB_BITS);
  u64 vfn_mask = ~0llu ^ ((1llu << bits)-1);
  u64 npages = VM_PTAB_NPAGES(level+1); // i.e. L1=0x8000000, L2=0x40000, L3=0x200, L4=1
  vm_ptab_t ptab = vm_pte_outaddr(parent);

  for (u32 index = start_index; index < VM_PTAB_LEN;) {
    vm_pte_t* pte = &ptab[index];

    u64 vfn = (start_vfn & vfn_mask) + ((u64)(index - start_index) * npages);

    #ifdef VM_MAP_ITER_TRACE
      if (level == VM_PTAB_LEVELS-1) {
        tracex("iter_visit page 0x%012llx [%u] %s",
          VM_VFN_VADDR(vfn), index, (*(u64*)pte ? "mapped" : "free"));
      } else {
        u64 last_vaddr = VM_VFN_VADDR(((vfn + npages) & vfn_mask)) - 1;
        if (level == 0) {
          tracex("iter_visit table L1 %012llx…%012llx 0x%llx [%u] nuse=%u",
            VM_VFN_VADDR(vfn), last_vaddr, npages, index, pte->nuse);
        } else {
          tracex("iter_visit table L%u %012llx…%012llx 0x%llx [%u] nuse=%u",
            level+1, VM_VFN_VADDR(vfn), last_vaddr, npages, index, pte->nuse);
        }
      }
    #endif

    bool end = (level == VM_PTAB_LEVELS-1);

    if ((end & (vfn < start_vfn)) | ((!end) & (vfn + npages < start_vfn))) {
      // vfn (or end of table's vfn) is less than start_vfn
      index++;
      continue;
    }

    u32 index_incr = callback(parent, level, ptab, index, vfn, userdata);
    assert_no_add_overflow(index, index_incr);
    if (index_incr == 0)
      return false;

    // reload pte in case callback created it
    pte = &ptab[index];

    if (!end & (*(u64*)pte != 0)) {
      // visit sub table
      bool cont;
      if (index == start_index) {
        u64 ivfn = index_vfn << VM_PTAB_BITS;
        cont = vm_map_iter1(callback, userdata, start_vfn, ivfn, pte, level+1);
      } else {
        cont = vm_map_iter0(callback, userdata, pte, level+1);
      }
      if (!cont)
        return false;
    }

    index += index_incr;
    // visit next table
  } // while (index < VM_PTAB_LEN)

  return npages;
}


void vm_map_iter(vm_map_t* map, u64 start_vaddr, vm_map_iter_f* fn, uintptr data) {
  assertf(start_vaddr >= VM_ADDR_MIN && VM_ADDR_MAX >= start_vaddr,
    "invalid vaddr 0x%llx", start_vaddr);

  u64 vfn = VM_VFN(start_vaddr);

  trace("iter start 0x%llx (VFN 0x%llx)", start_vaddr, vfn);

  vm_pte_t parent = { .nuse = map->root_nuse };
  vm_pte_set_outaddr(&parent, map->root);

  if (vfn == 0) {
    vm_map_iter0(fn, data, &parent, 0);
  } else {
    u64 index_vfn = vfn << ((sizeof(vfn)*8) - VM_VFN_BITS);
    vm_map_iter1(fn, data, vfn, index_vfn, &parent, 0);
  }

  map->root_nuse = parent.nuse;
}
