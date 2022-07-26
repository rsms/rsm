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

// mpte_t printf formatting
#define PTE_FMT          "(0x%llx)"
#define PTE_FMTARGS(pte) (pte).outaddr

static_assert(IS_POW2(PAGE_SIZE),       "PAGE_SIZE is not a power-of-two");
static_assert(PAGE_SIZE >= sizeof(u64), "PAGE_SIZE too small");
static_assert(PAGE_SIZE <= 65536,       "PAGE_SIZE too large");

//———————————————————————————————————————————————————————————————————————————————————————
// mm - memory manager
// The memory manager owns and manages all of the host memory.
// It hands it out to requestors as pages (not arbitrary byte ranges.)

static struct {
  uintptr start; // host address range start (PAGE_SIZE aligned)
  uintptr end;   // host address range end (exclusive)
  uintptr curr;  // next free address (PAGE_SIZE aligned)
} g_mm;


// kHostPageSize is set by vmem_init to the host page size
static u32 kHostPageSize;


rerror mm_init() {
  // since RSM runs as a regular host OS process, we get our host memory from the
  // host's virtual memory system via mmap.

  // check that PAGE_SIZE is an even multiple (or divisor) of host pagesize
  kHostPageSize = (u32)mem_pagesize();
  if (kHostPageSize % PAGE_SIZE && PAGE_SIZE % kHostPageSize) {
    assertf(0, "PAGE_SIZE (%u) not a multiple of host page size (%u)",
      PAGE_SIZE, kHostPageSize);
    return rerr_invalid;
  }

  usize host_vmsize = 2u * GiB;
  assert(host_vmsize % kHostPageSize == 0);
  void* host_vmptr = assertnotnull( osvmem_alloc(host_vmsize) );

  g_mm.start = (uintptr)host_vmptr;
  g_mm.end   = (uintptr)host_vmptr + (uintptr)host_vmsize;
  g_mm.curr  = (uintptr)host_vmptr;

  assert(IS_ALIGN2(g_mm.start, PAGE_SIZE));

  dlog("mm range: %lx…%lx (%.0f MB in %lu pages)",
    g_mm.start, g_mm.end, (double)(g_mm.end - g_mm.start)/1024.0/1024.0,
    (g_mm.end - g_mm.start) / kHostPageSize);

  // deinit:
  // osvmem_free((void*)g_mm.start, (usize)(g_mm.end - g_mm.start));

  return 0;
}


void* nullable mm_allocpages(usize npages) {
  uintptr addr = g_mm.curr;
  uintptr endaddr = addr + (npages * PAGE_SIZE);
  if (endaddr > g_mm.end)
    return NULL; // not enough memory
  g_mm.curr = endaddr;
  return (void*)addr;
}


__attribute__((malloc))
void* nullable mm_alloc(usize size) {
  return mm_allocpages(ALIGN2(size, PAGE_SIZE) / PAGE_SIZE);
}


void mm_free(void* ptr) {
  // TODO: implement mm_free
}


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


static mptab_t nullable mptab_create() {
  usize nbyte = PT_LEN * sizeof(mpte_t);
  mptab_t ptab = mm_alloc(nbyte);
  if UNLIKELY(ptab == NULL)
    return NULL;
  memset(ptab, 0, nbyte);
  return ptab;
}


static mpagedir_t* nullable mpagedir_create() {
  mptab_t root_ptab = mptab_create();
  if UNLIKELY(!root_ptab) {
    dlog("failed to allocate root page table");
    return NULL;
  }
  mpagedir_t* pagedir = mm_alloc(sizeof(mpagedir_t)); // FIXME whole page allocated!
  if UNLIKELY(!pagedir)
    return NULL;
  pagedir->root = root_ptab;
  return pagedir;
}


static mpte_t mpagedir_alloc_hpage(mpagedir_t* pagedir, mpte_t* pte) {
  uintptr haddr = (uintptr)mm_allocpages(1);
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

    mptab_t ptab2 = mptab_create();
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
  dlog("VFN_BITS:        %5u", VFN_BITS);
  dlog("PT_LEVELS:       %5u", PT_LEVELS);
  dlog("PT_BITS:         %5u", PT_BITS);

  // create a page directory
  mpagedir_t* pagedir = assertnotnull( mpagedir_create() );

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
}


rerror vmem_init() {
  rerror err = mm_init();
  if (err)
    return err;

  vmem_test();
  return rerr_canceled;
}
