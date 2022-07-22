// virtual memory
// See vmem.txt for in-depth documentation
// SPDX-License-Identifier: Apache-2.0

#include "rsmimpl.h"

// constants
//   PT_BITS: bits per page table; a divisor of VFN_BITS.
//   PT_BITS should be a value that makes each pagetable be one host page in size:
//     (2^PT_BITS)*sizeof(mpte_t) = pagesize
//     e.g. (2^8=256)*8 = 2048
//          (2^9=512)*8 = 4096
//          (2^11=2048)*8 = 16384
#define PAGE_SIZE       4096u /* size of one virtual memory page, in bytes */
#define VADDR_BITS        48u /* 256 TiB addressable space */
#define VADDR_OFFS_BITS   12u /* bits needed for page offset =ILOG2(PAGE_SIZE) */
#define VFN_BITS          36u /* bits needed for VFN =VADDR_BITS-VADDR_OFFS_BITS */
#define PT_LEVELS          4u /* number of page-table levels */
#define PT_BITS            9u /* =ILOG2(PAGE_SIZE/sizeof(mpte_t)) */
#define PT_LEN           512u /* number of PTEs in a page table =(1lu << PT_BITS) */

#define MiB 0x100000   /* 1024*1024 */
#define GiB 0x40000000 /* 1024*1024*1024 */

// mpte_t printf formatting
#define PTE_FMT          "(0x%llx)"
#define PTE_FMTARGS(pte) (pte).outaddr

static_assert(IS_POW2(PAGE_SIZE), "PAGE_SIZE is not a power-of-two");
static_assert(PAGE_SIZE >= sizeof(u64), "PAGE_SIZE too small");
static_assert(PAGE_SIZE <= 65536, "PAGE_SIZE too large");


// mhost_t - host memory map
typedef struct {
  uintptr start; // host address range start
  uintptr end;   // host address range end (exclusive)
  uintptr curr;  // next free address (= allocated bytes)
} mhost_t;

// mpte_t - page table entry
typedef struct {
#if RSM_LITTLE_ENDIAN
  u64 reserved : 12;
  u64 outaddr : 52;
#else
  u64 outaddr : 52;
  u64 reserved : 12;
#endif
} mpte_t;

// mptab_t - virtual memory page table
typedef mpte_t* mptab_t;

// mpagedir_t - page directory
typedef struct {
  mhost_t* mhost;
  mptab_t  root;
} mpagedir_t;


// kHostPageSize is set by vmem_init to the host page size
static u32 kHostPageSize;


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


static void* nullable mhost_alloc(mhost_t* mhost, usize size, usize align) {
  // FIXME this is an incredibly dumb allocator
  assert(IS_POW2(align));
  uintptr addr = ALIGN2(mhost->curr, (uintptr)align);
  uintptr endaddr = addr + (uintptr)size;
  if (endaddr > mhost->end)
    return NULL; // not enough memory
  mhost->curr = endaddr;
  return (void*)addr;
}


static mpte_t mpte_make(u64 outaddr) {
  mpte_t pte = {0};
  pte.outaddr = outaddr;
  return pte;
}


static mptab_t nullable mptab_create(mhost_t* mhost) {
  usize nbyte = PT_LEN * sizeof(mpte_t);
  mptab_t ptab = mhost_alloc(mhost, nbyte, (usize)PAGE_SIZE);
  if UNLIKELY(ptab == NULL)
    return NULL;
  memset(ptab, 0, nbyte);
  return ptab;
}


static mpagedir_t* nullable mpagedir_create(mhost_t* mhost) {
  mptab_t root_ptab = mptab_create(mhost);
  if UNLIKELY(!root_ptab) {
    dlog("failed to allocate root page table");
    return NULL;
  }
  mpagedir_t* pagedir = mhost_alloc(mhost, sizeof(mpagedir_t), _Alignof(mpagedir_t));
  if UNLIKELY(!pagedir)
    return NULL;
  pagedir->mhost = mhost;
  pagedir->root = root_ptab;
  return pagedir;
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
      if LIKELY(*(u64*)&pte != 0)
        return pte;
      dlog("first access to page %llu", vfn);
      u64 host_page_addr = 0x104008000 >> VADDR_OFFS_BITS; // FIXME TODO
      pte = mpte_make(host_page_addr);
      ptab[index] = pte;
      return pte;
    }

    bits += PT_BITS;
    masked_vfn = getbits(masked_vfn, VFN_BITS - (1+bits), VFN_BITS - bits);
    level++;

    if (*(u64*)&pte) {
      ptab = (mptab_t)(uintptr)(pte.outaddr << VADDR_OFFS_BITS);
      continue;
    }

    mptab_t ptab2 = mptab_create(pagedir->mhost);
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
  dlog("kHostPageSize:   %5u", kHostPageSize);
  dlog("PAGE_SIZE:       %5u", PAGE_SIZE);
  dlog("VADDR_BITS:      %5u", VADDR_BITS);
  dlog("VADDR_OFFS_BITS: %5u", VADDR_OFFS_BITS);
  dlog("PT_LEVELS:       %5u", PT_LEVELS);
  dlog("PT_BITS:         %5u", PT_BITS);

  // allocate host memory
  usize host_vmsize = 2u * GiB;
  assert(host_vmsize % kHostPageSize == 0);
  void* host_vmptr = assertnotnull( osvmem_alloc(host_vmsize) );
  mhost_t mhost = {
    .start = (uintptr)host_vmptr,
    .end   = (uintptr)host_vmptr + (uintptr)host_vmsize,
    .curr  = (uintptr)host_vmptr,
  };
  dlog("host memory: %lx…%lx (%.0f MB in %lu pages)",
    mhost.start, mhost.end, (double)(mhost.end - mhost.start)/1024.0/1024.0,
    (mhost.end - mhost.start) / kHostPageSize);

  // create a page directory
  mpagedir_t* pagedir = assertnotnull( mpagedir_create(&mhost) );

  { u64 vaddr = 0xdeadbeef;
    u64 vfn = vaddr_to_vfn(vaddr);
    dlog("—— mpagedir_lookup(addr 0x%llx, vfn 0x%llx) ——", vaddr, vfn);
    mpte_t pte = mpagedir_lookup_pte(pagedir, vfn);
    uintptr host_page = (uintptr)(pte.outaddr << VADDR_OFFS_BITS);
    uintptr host_addr = host_page + (uintptr)vaddr_offs(vaddr);
    // dlog("=> PTE" PTE_FMT ", host page 0x%lx, host address 0x%lx",
    //   PTE_FMTARGS(pte), host_page, host_addr);
    dlog("vaddr 0x%llx => host address 0x%lx (page 0x%lx)", vaddr, host_page, host_addr);
  }


  osvmem_free((void*)mhost.start, (usize)(mhost.end - mhost.start));
}


rerror vmem_init() {
  // check that PAGE_SIZE is an even multiple (or divisor) of host pagesize
  kHostPageSize = (u32)mem_pagesize();
  if (kHostPageSize % PAGE_SIZE && PAGE_SIZE % kHostPageSize) {
    assertf(0, "PAGE_SIZE (%u) not a multiple of host page size (%u)",
      PAGE_SIZE, kHostPageSize);
    return rerr_invalid;
  }

  vmem_test();
  return rerr_canceled;
}
