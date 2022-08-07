// virtual memory
// See vmem.txt for in-depth documentation
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "vm.h"

// VM_RUN_TEST_ON_INIT: define to run tests during exe init in DEBUG builds
#define VM_RUN_TEST_ON_INIT

// VM_TRACE: define to enable logging a lot of info via dlog
#define VM_TRACE

// VFN_BITS: bits needed for VFN (VM_ADDR_BITS-PAGE_SIZE_BITS)
#define VFN_BITS  (VM_ADDR_BITS - PAGE_SIZE_BITS)


#if defined(VM_TRACE) && defined(DEBUG)
  #define trace(fmt, args...) dlog("[vm] " fmt, ##args)
#else
  #ifdef VM_TRACE
    #warning VM_TRACE has no effect unless DEBUG is enabled
    #undef VM_TRACE
  #endif
  #define trace(...) ((void)0)
#endif


static_assert(ALIGN2_X(VM_PTAB_SIZE, PAGE_SIZE) == VM_PTAB_SIZE,
  "VM_PTAB_SIZE not page aligned");

// sanity check; VM_PTAB_SIZE should end up being exactly one page size
static_assert(VM_PTAB_SIZE == (VM_PTAB_LEN * sizeof(vm_pte_t)), "");

static_assert(sizeof(vm_pte_t) == sizeof(u64), "vm_pte_t too large");


// vm_pte_t printf formatting
#define PTE_FMT          "(0x%llx)"
#define PTE_FMTARGS(pte) (pte).outaddr


// getbits returns the (right adjusted) n-bit field of x that begins at position p.
// We assume that bit position 0 is at the right end and that n and p
// are sensible positive values.
// For example, getbits(x, 4, 3) returns the three bits in bit
// positions 4, 3 and 2, right adjusted.
// [from K&R, 2nd Ed., pg. 49: get n bits from position p]
inline static u64 getbits(u64 x, u32 p, u32 n) {
  return (x >> (p+1-n)) & ~(~0llu << n);
}


static vm_pte_t vm_pte_make(u64 outaddr) {
  return (vm_pte_t){ .outaddr = outaddr };
}


static vm_ptab_t nullable vm_ptab_create(rmm_t* mm) {
  // note: VM_PTAB_SIZE is always a multiple of PAGE_SIZE
  vm_ptab_t ptab = rmm_allocpages(mm, VM_PTAB_SIZE / PAGE_SIZE);
  if UNLIKELY(ptab == NULL)
    return NULL;
  #ifdef VM_ZERO_PAGES
  memset(ptab, 0, VM_PTAB_SIZE);
  #endif
  return ptab;
}


static void vm_ptab_free(rmm_t* mm, vm_ptab_t ptab) {
  rmm_freepages(mm, ptab);
}


bool vm_pagedir_init(vm_pagedir_t* pagedir, rmm_t* mm) {
  vm_ptab_t ptab = vm_ptab_create(mm); // root page table
  if UNLIKELY(!ptab) {
    trace("failed to allocate root page table");
    return false;
  }
  trace("allocated L%u page table %p +0x%lx", 1, ptab, VM_PTAB_SIZE);
  pagedir->root = ptab;
  pagedir->mm = mm;
  return true;
}


void vm_pagedir_dispose(vm_pagedir_t* pagedir) {
  vm_ptab_free(pagedir->mm, pagedir->root);
  dlog("TODO: free all PTEs");
}


static vm_pagedir_t* nullable vm_pagedir_create(rmm_t* mm) {
  // FIXME whole page allocated!
  static_assert(sizeof(vm_pagedir_t) < PAGE_SIZE, "");
  vm_pagedir_t* pagedir = rmm_allocpages(mm, 1);
  if (pagedir) {
    if (vm_pagedir_init(pagedir, mm))
      return pagedir;
    rmm_freepages(mm, pagedir);
  }
  return NULL;
}


static vm_pte_t vm_pagedir_alloc_backing_page(vm_pagedir_t* pagedir, vm_pte_t* pte) {
  uintptr haddr = (uintptr)rmm_allocpages(pagedir->mm, 1);
  uintptr hpage_addr = haddr >> PAGE_SIZE_BITS;
  if UNLIKELY(hpage_addr == 0) {
    trace("FAILED to allocate backing page");
    panic("TODO: purge a least recently-used page");
  }
  trace("allocated backing page %p", (void*)haddr);
  *pte = vm_pte_make(hpage_addr);
  return *pte;
}


// vm_pagedir_lookup_hfn returns the Host Frame Number for a Virtual Frame Number
static vm_pte_t vm_pagedir_lookup_pte(vm_pagedir_t* pagedir, u64 vfn) {
  assertf(vfn > 0, "invalid VFN 0x0 (vm address likely less than VM_ADDR_MIN)");
  vfn--; // VM_ADDR_MIN
  u32 bits = 0;
  u64 masked_vfn = vfn;
  vm_ptab_t ptab = pagedir->root;
  u8 level = 1;
  // TODO: RW thread lock

  for (;;) {
    u64 index = getbits(masked_vfn, VFN_BITS - (1+bits), VM_PTAB_BITS);
    vm_pte_t pte = ptab[index];

    trace(
      "lookup vfn 0x%llx L %u; index %llu = getbits(0x%llx, %u-(1+%u), %u)",
      vfn+1, level, index, masked_vfn, VFN_BITS, bits, VM_PTAB_BITS);

    if (level == VM_PTAB_LEVELS) {
      if UNLIKELY(*(u64*)&pte == 0) {
        trace("first access to page vfn=0x%llx", vfn+1);
        return vm_pagedir_alloc_backing_page(pagedir, &ptab[index]);
      }
      return pte;
    }

    bits += VM_PTAB_BITS;
    masked_vfn = getbits(masked_vfn, VFN_BITS - (1+bits), VFN_BITS - bits);
    level++;

    if (*(u64*)&pte) {
      ptab = (vm_ptab_t)(uintptr)(pte.outaddr << PAGE_SIZE_BITS);
      continue;
    }

    // allocate a new page table
    vm_ptab_t ptab2 = vm_ptab_create(pagedir->mm);
    if UNLIKELY(!ptab2) {
      // out of backing memory; try to free some up memory up by purging unused tables
      panic("TODO: purge unused page tables (except for the root)");
    }

    assertf(IS_ALIGN2((u64)(uintptr)ptab2, PAGE_SIZE),
      "ptab_create did not allocate vm_ptab_t on a page boundary (0x%lx/%u)",
      (uintptr)ptab2, PAGE_SIZE);

    u64 ptab2_addr = ((u64)(uintptr)ptab2) >> PAGE_SIZE_BITS;
    ptab[index] = vm_pte_make(ptab2_addr);
    ptab = ptab2;

    trace("allocated L%u page table %p +0x%lx at [%llu]",
      level, ptab, VM_PTAB_SIZE, index);

    if UNLIKELY(!ptab)
      return (vm_pte_t){0};
  }
}

// // vm_pagedir_translate_vfn returns the host address for a virtual address
// static uintptr vm_pagedir_translate_vfn(vm_pagedir_t* pagedir, u64 vaddr) {
//   u64 vfn = vaddr_to_vfn(vaddr);
//   vm_pte_t pte = vm_pagedir_lookup_pte(pagedir, vfn);
//   uintptr host_page = (uintptr)(pte.outaddr << PAGE_SIZE_BITS);
//   return host_page + (uintptr)vaddr_offs(vaddr);
// }


#ifdef VM_TRACE
  UNUSED static const char* debug_fmt_bits(const void* bits, usize len) {
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


void vm_cache_init(vm_cache_t* cache) {
  memset(cache, 0xff, sizeof(vm_cache_t));
}


void vm_cache_invalidate(vm_cache_t* cache) {
  memset(cache, 0xff, sizeof(vm_cache_t));
}


uintptr vm_cache_lookup(vm_cache_t* cache, u64 vaddr) {
  u64 vfn = VM_VFN(vaddr); // vaddr_to_vfn()

  // calculate hash index; the bottom VM_CACHE_INDEX_BITS bits of the VFN.
  // e.g. 0xdeadbeef => VFN 0xdeadb => hash index 0xdb (219)
  usize index = (usize)(vfn & VM_CACHE_INDEX_VFN_MASK);

  // load the hash table entry
  vm_cache_ent_t* entry = &cache->entries[index];

  // check tag value
  u64 is_valid = entry->tag == (vaddr & VM_CACHE_TAG_MASK(1));

  // return the host address (or 0x0 if tag is invalid)
  return (entry->haddr + VM_ADDR_OFFSET(vaddr)) * is_valid;
}


static uintptr vm_cache_add(vm_cache_t* cache, u64 vaddr, uintptr haddr) {
  assertf(IS_ALIGN2(haddr, PAGE_SIZE), "haddr is not a page address %p", (void*)haddr);
  vm_cache_ent_t* entry = VM_CACHE_ENTRY(cache, vaddr);
  entry->haddr = haddr;
  entry->tag = VM_PAGE_ADDR(vaddr);
  return haddr;
}


uintptr _vm_cache_miss(vm_cache_t* cache, vm_pagedir_t* pagedir, u64 vaddr, vm_op_t op) {
  trace("%s 0x%llx op=0x%x", __FUNCTION__, vaddr, op);

  // check validity
  if UNLIKELY(VM_ADDR_MIN > vaddr || vaddr > VM_ADDR_MAX) {
    panic("invalid address 0x%llx (out of range)", vaddr);
    return 0;
  }

  // check alignment
  if UNLIKELY(!IS_ALIGN2(vaddr, VM_OP_ALIGNMENT(op))) {
    const char* opname = VM_OP_TYPE(op) == VM_OP_LOAD ? "load from" : "store to";
    panic("misaligned %uB %s 0x%llx", VM_OP_ALIGNMENT(op), opname, vaddr);
  }

  u64 vfn = VM_VFN(vaddr);
  vm_pte_t pte = vm_pagedir_lookup_pte(pagedir, vfn);
  uintptr haddr = (uintptr)(pte.outaddr << PAGE_SIZE_BITS);
  trace("%s 0x%llx -> %p", __FUNCTION__, vaddr, (void*)haddr);

  if UNLIKELY(haddr == 0) {
    trace("invalid address 0x%llx (vm_pagedir_lookup_pte failed)", vaddr);
    return 0;
  }

  return vm_cache_add(cache, vaddr, haddr);
}


#if defined(VM_RUN_TEST_ON_INIT) && DEBUG
static void test_vm() {
  dlog("%s", __FUNCTION__);
  dlog("host pagesize:     %5u", (u32)os_pagesize());
  dlog("PAGE_SIZE:         %5u", PAGE_SIZE);
  dlog("PAGE_SIZE_BITS:    %5u", PAGE_SIZE_BITS);
  dlog("VM_ADDR_BITS:      %5u", VM_ADDR_BITS);
  dlog("VM_ADDR_MIN…MAX:   0x%llx … 0x%llx", VM_ADDR_MIN, VM_ADDR_MAX);
  dlog("VFN_BITS:       %5u", VFN_BITS);
  dlog("VM_PTAB_LEVELS:    %5u", VM_PTAB_LEVELS);
  dlog("VM_PTAB_BITS:      %5u", VM_PTAB_BITS);

  // create a memory manager
  usize memsize = 4 * MiB;
  rmm_t* mm = assertnotnull( rmm_create_host_vmmap(memsize) );

  // create a page directory with memory manager
  vm_pagedir_t* pagedir = assertnotnull( vm_pagedir_create(mm) );

  // creat a translation cache
  vm_cache_t* cache = assertnotnull( rmm_allocpages(mm,
    ALIGN_CEIL(sizeof(vm_cache_t), PAGE_SIZE) / PAGE_SIZE) );
  vm_cache_init(cache);

  // { u64 vaddr = 0xdeadbeef;
  //   u64 vfn = vaddr_to_vfn(vaddr);
  //   dlog("—— vm_pagedir_lookup(addr 0x%llx, vfn 0x%llx) ——", vaddr, vfn);
  //   vm_pte_t pte = vm_pagedir_lookup_pte(pagedir, vfn);
  //   uintptr hpage = (uintptr)(pte.outaddr << PAGE_SIZE_BITS);
  //   uintptr haddr = hpage + (uintptr)vaddr_offs(vaddr);
  //   // dlog("=> PTE" PTE_FMT ", host page 0x%lx, host address 0x%lx",
  //   //   PTE_FMTARGS(pte), hpage, haddr);
  //   dlog("vaddr 0x%llx => host address 0x%lx (page 0x%lx)", vaddr, haddr, hpage);
  // }

  // // look up address in cache
  // { u64 vaddr = 0xdeadbeef;
  //   uintptr haddr = vm_cache_lookup(cache, vaddr);
  //   dlog("vm_cache_lookup(0x%llx) => %p", vaddr, (void*)haddr);
  //   vm_cache_add(cache, vaddr, 0x1044f0ee0);
  //   haddr = vm_cache_lookup(cache, vaddr);
  //   dlog("vm_cache_lookup(0x%llx) => %p", vaddr, (void*)haddr);
  //   vm_cache_del(cache, vaddr);
  // }


  { // do full real lookup with cache
    u64 vaddr = 0xdeadbee4;
    u32 value = 12345;
    dlog("VM_STORE(u32, 0x%llx, %u)", vaddr, value);
    VM_STORE(u32, cache, pagedir, vaddr, value);
    value = VM_LOAD(u32, cache, pagedir, vaddr);
    dlog("VM_LOAD(u32, 0x%llx) => %u", vaddr, value);
    value = VM_LOAD(u32, cache, pagedir, vaddr);
    dlog("VM_LOAD(u32, 0x%llx) => %u", vaddr, value);

    // loading an invalid address
    //VM_LOAD(u64, cache, pagedir, 0xffffffffffffffffllu);

    // loading the same address, which is 4B aligned, with a type of stronger
    // alignment will cause a cache miss and subsequent alignment error.
    // ——WIP—— (see vm_translate)
    //value = (u64)VM_LOAD(u64, cache, pagedir, vaddr);
  }


  // // allocate all pages (should panic just shy of rmm_avail_total pages)
  // for (u64 vaddr = VM_ADDR_MIN; vaddr <= VM_ADDR_MAX; vaddr += PAGE_SIZE) {
  //   u64 vfn = vaddr_to_vfn(vaddr);
  //   vm_pte_t pte = vm_pagedir_lookup_pte(pagedir, vfn);
  // }

  rmm_dispose(mm);
}
#endif // VM_RUN_TEST_ON_INIT


rerr_t init_vmem() {
  #if defined(VM_RUN_TEST_ON_INIT) && DEBUG
  test_vm();
  // return rerr_canceled;
  #endif

  return 0;
}
