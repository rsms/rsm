// virtual memory
// See vmem.txt for in-depth documentation
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "mem.h"

// constants
//   PT_BITS: bits per page table; a divisor of VFN_BITS.
//   PT_BITS should be a value that makes each pagetable be one host page in size:
//     (2^PT_BITS)*sizeof(mpte_t) = pagesize
//     e.g. (2^8=256)*8 = 2048
//          (2^9=512)*8 = 4096
//          (2^11=2048)*8 = 16384
#define VADDR_BITS        48u /* 256 TiB addressable space */
#define VADDR_OFFS_BITS   12u /* bits needed for page offset =ILOG2(PAGE_SIZE) */
#define VFN_BITS          36u /* bits needed for VFN =VADDR_BITS-VADDR_OFFS_BITS */
#define PT_LEVELS          4u /* number of page-table levels */
#define PT_BITS            9u /* =ILOG2(PAGE_SIZE/sizeof(mpte_t)) */
#define PT_LEN           512u /* number of PTEs in a page table =(1lu << PT_BITS) */

// mpte_t printf formatting
#define PTE_FMT          "(0x%llx)"
#define PTE_FMTARGS(pte) (pte).outaddr


//———————————————————————————————————————————————————————————————————————————————————————

// mpte_t - page table entry
typedef struct {
#if RSM_LITTLE_ENDIAN
  // bool  execute     : 1; // can execute code in this page
  // bool  read        : 1; // can read from this page
  // bool  write       : 1; // can write to this page
  // bool  uncacheable : 1; // can not be cached in TLB
  // bool  accessed    : 1; // has been accessed
  // bool  dirty       : 1; // has been written to
  // usize type        : 3; // type of page
  // usize reserved    : 3;
  // u64   outaddr     : 52;
  usize reserved : 12;
  u64   outaddr  : 52;
#else
  #error TODO
#endif
} mpte_t;
static_assert(sizeof(mpte_t) == sizeof(u64), "");

// mptab_t - virtual memory page table
typedef mpte_t* mptab_t;

// mpagedir_t - page directory
typedef struct {
  rmm_t*  mm;
  mptab_t root;
} mpagedir_t;


// getbits returns the (right adjusted) n-bit field of x that begins at position p.
// We assume that bit position 0 is at the right end and that n and p
// are sensible positive values.
// For example, getbits(x, 4, 3) returns the three bits in bit
// positions 4, 3 and 2, right adjusted.
// [from K&R, 2nd Ed., pg. 49: get n bits from position p]
inline static u64 getbits(u64 x, u32 p, u32 n) {
  return (x >> (p+1-n)) & ~(~0llu << n);
}

// [UNUSED] vfn_to_vaddr returns the effective address of VFN + offset
// inline static u64 vfn_to_vaddr(u64 vfn, u64 offset) {
//   return (vfn << VADDR_OFFS_BITS) + offset;
// }

// vaddr_to_vfn returns the VFN of a virtual address
// VFN is the lower VFN_BITS bits.
// e.g. 0xdeadbeef 11011110101011011011111011101111 / 4096
//    = 0xdeadb    11011110101011011011
inline static u64 vaddr_to_vfn(u64 vaddr) {
  return vaddr >> VADDR_OFFS_BITS;
}

// Page offset of a virtual address is the upper VADDR_OFFS_BITS bits
// e.g. 0xdeadbeef 11011110101011011011111011101111 & 4095
//    = 0xeef                           11011101111
inline static u64 vaddr_offs(u64 vaddr) {
  return (vaddr & (PAGE_SIZE - 1));
}


static mpte_t mpte_make(u64 outaddr) {
  mpte_t pte = {0};
  pte.outaddr = outaddr;
  return pte;
}


static mptab_t nullable mptab_create(rmm_t* mm) {
  usize nbyte = PT_LEN * sizeof(mpte_t);
  mptab_t ptab = rmm_allocpages_bytes(mm, nbyte);
  if UNLIKELY(ptab == NULL)
    return NULL;
  memset(ptab, 0, nbyte);
  return ptab;
}


static mpagedir_t* nullable mpagedir_create(rmm_t* mm) {
  mptab_t root_ptab = mptab_create(mm);
  if UNLIKELY(!root_ptab) {
    dlog("failed to allocate root page table");
    return NULL;
  }
  // FIXME whole page allocated!
  mpagedir_t* pagedir = rmm_allocpages_bytes(mm, sizeof(mpagedir_t));
  if UNLIKELY(!pagedir)
    return NULL;
  pagedir->mm = mm;
  pagedir->root = root_ptab;
  return pagedir;
}


static mpte_t mpagedir_alloc_hpage(mpagedir_t* pagedir, mpte_t* pte) {
  uintptr haddr = (uintptr)rmm_allocpages(pagedir->mm, 1);
  uintptr hpage_addr = haddr >> VADDR_OFFS_BITS;
  if UNLIKELY(hpage_addr == 0) {
    dlog("FAILED to allocate host page");
    // TODO: swap out an older page
  }
  *pte = mpte_make(hpage_addr);
  return *pte;
}


// mpagedir_lookup_hfn returns the Host Frame Number for a Virtual Frame Number
static mpte_t mpagedir_lookup_pte(mpagedir_t* pagedir, u64 vfn) {
  u32 bits = 0;
  u64 masked_vfn = vfn;
  mptab_t ptab = pagedir->root;
  u8 level = 1;
  // TODO: RW thread lock

  for (;;) {
    u64 index = getbits(masked_vfn, VFN_BITS - (1+bits), PT_BITS);
    mpte_t pte = ptab[index];

    dlog(
      "lookup vfn 0x%llx L %u; index %llu = getbits(0x%llx, %u-(1+%u), %u)",
      vfn, level, index, masked_vfn, VFN_BITS, bits, PT_BITS);

    if (level == PT_LEVELS) {
      if UNLIKELY(*(u64*)&pte == 0) {
        dlog("first access to page vfn=0x%llx", vfn);
        return mpagedir_alloc_hpage(pagedir, &ptab[index]);
      }
      return pte;
    }

    bits += PT_BITS;
    masked_vfn = getbits(masked_vfn, VFN_BITS - (1+bits), VFN_BITS - bits);
    level++;

    if (*(u64*)&pte) {
      ptab = (mptab_t)(uintptr)(pte.outaddr << VADDR_OFFS_BITS);
      continue;
    }

    mptab_t ptab2 = mptab_create(pagedir->mm);
    assertf(IS_ALIGN2((u64)(uintptr)ptab2, PAGE_SIZE),
      "mptab_create did not allocate mptab_t on a page boundary (0x%lx/%u)",
      (uintptr)ptab2, PAGE_SIZE);
    u64 ptab2_addr = ((u64)(uintptr)ptab2) >> VADDR_OFFS_BITS;
    ptab[index] = mpte_make(ptab2_addr);
    ptab = ptab2;
    dlog("allocated L%u ptab at [%llu] %p", level, index, ptab);
    if UNLIKELY(!ptab)
      return (mpte_t){0};
  }
}

// // mpagedir_translate_vfn returns the host address for a virtual address
// static uintptr mpagedir_translate_vfn(mpagedir_t* pagedir, u64 vaddr) {
//   u64 vfn = vaddr_to_vfn(vaddr);
//   mpte_t pte = mpagedir_lookup_pte(pagedir, vfn);
//   uintptr host_page = (uintptr)(pte.outaddr << VADDR_OFFS_BITS);
//   return host_page + (uintptr)vaddr_offs(vaddr);
// }

static void vmem_test() {

  dlog("host pagesize:   %5u", (u32)mem_pagesize());
  dlog("PAGE_SIZE:       %5u", PAGE_SIZE);
  dlog("VADDR_BITS:      %5u", VADDR_BITS);
  dlog("VADDR_OFFS_BITS: %5u", VADDR_OFFS_BITS);
  dlog("VFN_BITS:        %5u", VFN_BITS);
  dlog("PT_LEVELS:       %5u", PT_LEVELS);
  dlog("PT_BITS:         %5u", PT_BITS);

  // create a memory manager
  usize memsize = 16 * MiB;
  rmm_t* mm = rmm_create(assertnotnull( osvmem_alloc(memsize) ), memsize);

  // create a page directory, with a reference to the MM
  mpagedir_t* pagedir = assertnotnull( mpagedir_create(mm) );

  { u64 vaddr = 0xdeadbeef;
    u64 vfn = vaddr_to_vfn(vaddr);
    dlog("—— mpagedir_lookup(addr 0x%llx, vfn 0x%llx) ——", vaddr, vfn);
    mpte_t pte = mpagedir_lookup_pte(pagedir, vfn);
    uintptr hpage = (uintptr)(pte.outaddr << VADDR_OFFS_BITS);
    uintptr haddr = hpage + (uintptr)vaddr_offs(vaddr);
    // dlog("=> PTE" PTE_FMT ", host page 0x%lx, host address 0x%lx",
    //   PTE_FMTARGS(pte), hpage, haddr);
    dlog("vaddr 0x%llx => host address 0x%lx (page 0x%lx)", vaddr, haddr, hpage);
  }

  osvmem_free((void*)rmm_startaddr(mm), memsize);
  rmm_dispose(mm);
}


rerror init_vmem() {
  vmem_test();
  return rerr_canceled;
}
