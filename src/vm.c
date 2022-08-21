// virtual memory
// See vmem.txt for in-depth documentation
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "vm.h"

// VM_RUN_TEST_ON_INIT: define to run tests during exe init in DEBUG builds
#define VM_RUN_TEST_ON_INIT

// VM_TRACE: define to enable logging a lot of info via dlog
//#define VM_TRACE

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


#if DEBUG
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

  UNUSED static const char* vm_perm_str(vm_perm_t perm) {
    switch (perm) {
      case VM_PERM_R:           return "r--";
      case VM_PERM_W:           return "-w-";
      case VM_PERM_X:           return "--x";
      case VM_PERM_RW:          return "rw-";
      case VM_PERM_RX:          return "r-x";
      case VM_PERM_W|VM_PERM_X: return "-wx";
      case VM_PERM_RWX:         return "rwx";
    }
    return "---";
  }
#endif


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


rerr_t vm_pagedir_init(vm_pagedir_t* pagedir, rmm_t* mm) {
  if (!RHMutexInit(&pagedir->lock))
    return rerr_not_supported;
  vm_ptab_t ptab = vm_ptab_create(mm); // root page table
  if UNLIKELY(!ptab) {
    trace("failed to allocate root page table");
    return rerr_nomem;
  }
  trace("allocated L%u page table %p +0x%lx", 1, ptab, VM_PTAB_SIZE);
  pagedir->root = ptab;
  pagedir->mm = mm;
  return 0;
}


void vm_pagedir_dispose(vm_pagedir_t* pagedir) {
  vm_ptab_free(pagedir->mm, pagedir->root);
  dlog("TODO: free all PTEs");
}


#if DEBUG
  static vm_pagedir_t* nullable vm_pagedir_create(rmm_t* mm) {
    // FIXME whole page allocated!
    static_assert(sizeof(vm_pagedir_t) < PAGE_SIZE, "");
    vm_pagedir_t* pagedir = rmm_allocpages(mm, 1);
    if (!pagedir)
      return NULL;
    if (vm_pagedir_init(pagedir, mm)) {
      rmm_freepages(mm, pagedir);
      return NULL;
    }
    return pagedir;
  }
#endif


// returns pte on success or NULL if out of memory
static vm_pte_t* nullable alloc_backing_page(vm_pagedir_t* pagedir, vm_pte_t* pte) {
  void* haddr = rmm_allocpages(pagedir->mm, 1);
  if UNLIKELY(!haddr) {
    trace("FAILED to allocate backing page");
    panic("TODO: purge a least-recently-used page");
  }

  trace("allocated backing page %p", haddr);

  u64 hfn = (u64)(uintptr)haddr >> PAGE_SIZE_BITS;
  pte->outaddr = hfn;

  return hfn ? pte : NULL;
}


// vm_pagedir_access returns the page table entry of a Virtual Frame Number
static vm_pte_t* nullable vm_pagedir_access(
  vm_pagedir_t* pagedir, u64 vfn, bool isaccess)
{
  assertf(vfn <= VM_VFN_MAX, "invalid VFN 0x%llx", vfn);
  u32 bits = 0;
  u64 masked_vfn = vfn;
  vm_ptab_t ptab = pagedir->root;
  u8 level = 1;

  vm_pte_t* pte = NULL;
  RHMutexLock(&pagedir->lock);

  for (;;) {
    u64 index = getbits(masked_vfn, VFN_BITS - (1+bits), VM_PTAB_BITS);
    pte = &ptab[index];

    trace(
      "lookup vfn 0x%llx L %u; index %llu = getbits(0x%llx, %u-(1+%u), %u)",
      vfn+1, level, index, masked_vfn, VFN_BITS, bits, VM_PTAB_BITS);

    if (level == VM_PTAB_LEVELS) {
      if UNLIKELY(*(u64*)pte == 0) {
        pte = NULL;
        break;
      }
      // if there's no backing page, allocate one
      if (pte->outaddr == 0)
        pte = alloc_backing_page(pagedir, pte);
      break;
    }

    bits += VM_PTAB_BITS;
    masked_vfn = getbits(masked_vfn, VFN_BITS - (1+bits), VFN_BITS - bits);
    level++;

    if UNLIKELY(*(u64*)pte == 0) {
      pte = NULL;
      break;
    }

    pte->accessed |= isaccess;
    ptab = (vm_ptab_t)(uintptr)(pte->outaddr << PAGE_SIZE_BITS);
  }

  RHMutexUnlock(&pagedir->lock);
  return pte;
}


rerr_t vm_unmap(vm_pagedir_t* pagedir, u64 vaddr, usize npages) {
  // TODO: rewrite this function like vm_map (based on logic in vm_pagedir_access)
  u64 vfn = VM_VFN(vaddr);

  trace("unmap 0x%llx (vfn 0x%llx, %zu pages)", VM_PAGE_ADDR(vaddr), vfn, npages);

  for (u64 vfn_end = vfn + npages; vfn < vfn_end; vfn++) {
    vm_pte_t* pte = vm_pagedir_access(pagedir, vfn, /*is_access*/false);
    if UNLIKELY(!pte)
      return rerr_not_found;
    *pte = (vm_pte_t){0};
  }

  return 0;
}


static vm_ptab_t nullable new_ptab(vm_pagedir_t* pagedir, vm_ptab_t parent, u64 index) {
  vm_ptab_t ptab = vm_ptab_create(pagedir->mm);
  if UNLIKELY(!ptab) {
    // out of backing memory
    // TODO: try to free some memory by purging unused tables
    dlog("TODO: purge unused page tables (except for the root)");
    return NULL;
  }

  u64 ptab_hfn = ((u64)(uintptr)ptab) >> PAGE_SIZE_BITS;
  parent[index] = (vm_pte_t){ .outaddr = ptab_hfn };

  trace("allocated page table %p (HFN 0x%llx) +0x%lx at parent[%llu]",
    ptab, ptab_hfn, VM_PTAB_SIZE, index);

  assertf(IS_ALIGN2((u64)(uintptr)ptab, PAGE_SIZE),
    "ptab_create did not allocate vm_ptab_t on a page boundary (0x%lx/%u)",
    (uintptr)ptab, PAGE_SIZE);

  return ptab;
}


rerr_t vm_map(
  vm_pagedir_t* pagedir, uintptr haddr, u64 vaddr, usize npages, vm_perm_t perm)
{
  assertf(IS_ALIGN2((uintptr)haddr, PAGE_SIZE), "haddr 0x%lx not page aligned", haddr);
  assertf(vaddr >= VM_ADDR_MIN && VM_ADDR_MAX >= vaddr, "invalid vaddr 0x%llx", vaddr);
  assert(npages > 0);

  rerr_t err = 0;
  u64 vfn = VM_VFN(vaddr);
  u64 vfn_end = vfn + npages;

  vm_pte_t* pte = NULL;
  RHMutexLock(&pagedir->lock);

  while (vfn < vfn_end) {
    u32 bits = 0;
    u64 masked_vfn = vfn;
    vm_ptab_t ptab = pagedir->root;
    u8 level = 1;
    u64 index;

  visit_next_ptab:
    index = getbits(masked_vfn, VFN_BITS - (1+bits), VM_PTAB_BITS);
    pte = &ptab[index];

    if (level < VM_PTAB_LEVELS) {
      // page table branch
      bits += VM_PTAB_BITS;
      masked_vfn = getbits(masked_vfn, VFN_BITS - (1+bits), VFN_BITS - bits);
      level++;
      if UNLIKELY(*(u64*)pte == 0) {
        // allocate new table
        if UNLIKELY( !(ptab = new_ptab(pagedir, ptab, index)) ) {
          err = rerr_nomem;
          goto end;
        }
      } else {
        // traverse existing table
        assert(pte->outaddr != 0);
        ptab = (vm_ptab_t)(uintptr)(pte->outaddr << PAGE_SIZE_BITS);
      }
      goto visit_next_ptab;
    }

    // page table leaf entry
    assert(npages > 0);
    for (;;) {
      trace("map 0x%llx => 0x%lx %s (vfn 0x%llx, ptab[%llu])",
        VM_VFN_VADDR(vfn), haddr, vm_perm_str(perm), vfn, index);

      if UNLIKELY(*(u64*)pte) {
        dlog("trying to map already-mapped page at vaddr 0x%llx", VM_VFN_VADDR(vfn));
        err = rerr_exists;
        goto end;
      }

      pte->outaddr = (u64)( (uintptr)haddr >> PAGE_SIZE_BITS );
      *((u8*)pte) = *((u8*)pte) | perm;

      index++;
      vfn++;
      haddr += PAGE_SIZE * !!haddr; // no-op in case haddr==0

      if (vfn == vfn_end || index == VM_PTAB_LEN)
        break;

      pte = &ptab[index];
    }
    // either we reached end of this table or we are done mapping all pages

  } // while (vfn < vfn_end)
end:
  if (err && vfn > VM_VFN(vaddr)) {
    dlog("TODO unmap what was partially mapped");
  }
  RHMutexUnlock(&pagedir->lock);
  return err;
}


// // vm_pagedir_translate translates a virtual address to its corresponding host address
// uintptr vm_pagedir_translate(vm_pagedir_t* pagedir, u64 vaddr) {
//   vm_pte_t* pte = vm_pagedir_access(pagedir, VM_VFN(vaddr), NULL);
//   uintptr host_page_addr = (uintptr)(pte->outaddr << PAGE_SIZE_BITS);
//   return host_page_addr + (uintptr)VM_ADDR_OFFSET(vaddr);
// }


void vm_cache_init(vm_cache_t* cache) {
  memset(cache, 0xff, sizeof(vm_cache_t));
}


void vm_cache_invalidate(vm_cache_t* cache, u64 vaddr, usize npages) {
  assert(vaddr >= VM_ADDR_MIN);
  usize index = (usize)VM_CACHE_INDEX(vaddr);
  safecheckf(index + npages <= VM_CACHE_LEN, "out of bounds %zu+%zu", index, npages);
  trace("cache invalidate entries[%zu:%zu]", index, index + npages);
  memset(&cache->entries[index], 0xff, npages*sizeof(cache->entries[0]));
}


#if DEBUG
  // vm_cache_lookup looks up the host page address for a virtual address.
  // Returns the host address, or 0 if the virtual page is not present in the cache.
  static uintptr vm_cache_lookup(vm_cache_t* cache, u64 vaddr, u64 alignment) {
    u64 index = VM_CACHE_INDEX(vaddr);
    u64 actual_tag = cache->entries[index].tag;
    u64 expected_tag = vaddr & (VM_ADDR_PAGE_MASK ^ (alignment - 1llu));
    u64 is_valid = actual_tag == expected_tag;
    return (uintptr)(cache->entries[index].haddr_diff + vaddr) * is_valid;
  }
#endif


// returns vm_cache_ent_t.haddr_diff
static u64 vm_cache_add(vm_cache_t* cache, u64 vpaddr, uintptr hpaddr) {
  assertf(IS_ALIGN2(vpaddr, PAGE_SIZE), "vpaddr not a page address 0x%llx", vpaddr);
  assertf(IS_ALIGN2(hpaddr, PAGE_SIZE), "hpaddr not a page address %p", (void*)hpaddr);

  vm_cache_ent_t* entry = VM_CACHE_ENTRY(cache, vpaddr);
  entry->haddr_diff = (u64)hpaddr - vpaddr;
  entry->tag = vpaddr;

  trace("%s 0x%llx => {.haddr_diff=0x%llx, .tag=0x%llx}",
    __FUNCTION__, vpaddr, entry->haddr_diff, entry->tag);

  return entry->haddr_diff;
}


// returns vm_cache_ent_t.haddr_diff
u64 _vm_cache_miss(vm_cache_t* cache, vm_pagedir_t* pagedir, u64 vaddr, vm_op_t op) {
  trace("%s 0x%llx op=0x%x", __FUNCTION__, vaddr, op);

  // check validity
  if UNLIKELY(VM_ADDR_MIN > vaddr || vaddr > VM_ADDR_MAX) {
    panic("invalid address 0x%llx (out of range)", vaddr); // FIXME
    return 0;
  }

  // check alignment
  if UNLIKELY(!IS_ALIGN2(vaddr, VM_OP_ALIGNMENT(op))) {
    const char* opname = VM_OP_TYPE(op) == VM_OP_LOAD ? "load from" : "store to";
    panic("misaligned %uB %s 0x%llx", VM_OP_ALIGNMENT(op), opname, vaddr); // FIXME
  }

  // get page table entry for the virtual page address (lookup via VFN)
  vm_pte_t* pte = vm_pagedir_access(pagedir, VM_VFN(vaddr), /*is_access*/true);

  // check if the lookup failed
  if UNLIKELY(!pte) {
    panic("invalid address 0x%llx (not mapped)", vaddr);
    return 0;
  }

  // check permissions
  vm_perm_t wantperm = 0;
  wantperm |= (VM_OP_TYPE(op) == VM_OP_LOAD) * VM_PERM_R;
  wantperm |= (VM_OP_TYPE(op) == VM_OP_STORE) * VM_PERM_W;
  vm_perm_t hasperm = VM_PTE_PERM(pte);
  if UNLIKELY(!VM_PERM_CHECK(hasperm, wantperm)) {
    trace("wantperm %s not in hasperm %s", vm_perm_str(wantperm), vm_perm_str(hasperm));
    if (VM_OP_TYPE(op) == VM_PERM_R)
      panic("store to read-protected address 0x%llx", vaddr);
    panic("store to read-only address 0x%llx", vaddr);
    return 0;
  }

  pte->accessed = true;
  pte->written |= (VM_OP_TYPE(op) == VM_OP_STORE);

  // calculate page addresses
  uintptr hpaddr = (uintptr)(pte->outaddr << PAGE_SIZE_BITS);
  u64 vpaddr = VM_PAGE_ADDR(vaddr);

  trace("%s 0x%llx -> %p", __FUNCTION__, vaddr, (void*)hpaddr);

  if (pte->uncacheable)
    return (u64)hpaddr - vpaddr; // vm_cache_ent_t.haddr_diff

  return vm_cache_add(cache, vpaddr, hpaddr);
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

  { // test the "store host page address diff in cache" logic
    const struct { u64 vaddr, hpage; } tests[] = {
      { 0xdeadbee4, 0x1065f0000 },
      { 0x1065f0000, 0xdeadbee4 },
    };
    for (usize i = 0; i < countof(tests); i++) {
      u64 vaddr = tests[i].vaddr;
      u64 hpage = tests[i].hpage;
      u64 vpage = vaddr & VM_ADDR_PAGE_MASK; // VM_PAGE_ADDR(vaddr)
      u64 diff = hpage - vpage;
      u64 haddr = diff + vaddr;
      u64 haddr_expected = hpage + VM_ADDR_OFFSET(vaddr);

      // log("vaddr  0x%-10llx %s", vaddr, fmtbits(&vaddr,64));
      // log("vpage  0x%-10llx %s", vpage, fmtbits(&vpage,64));
      // log("hpage  0x%-10llx %s", hpage, fmtbits(&hpage,64));
      // log("diff   0x%-10llx %s",  diff, fmtbits(&diff,64));
      // log("haddr  0x%-10llx %s", haddr, fmtbits(&haddr,64));
      // log("haddr′ 0x%-10llx %s", haddr_expected, fmtbits(&haddr_expected,64));

      assert(haddr == haddr_expected);
    }
  }

  // create a memory manager
  usize memsize = 4 * MiB;
  rmm_t* mm = assertnotnull( rmm_create_host_vmmap(memsize) );

  // create a page directory with memory manager
  vm_pagedir_t* pagedir = assertnotnull( vm_pagedir_create(mm) );

  // creat translation caches
  vm_cache_t* cache_rw = assertnotnull( rmm_allocpages(mm,
    ALIGN_CEIL(sizeof(vm_cache_t), PAGE_SIZE) / PAGE_SIZE) );
  vm_cache_init(cache_rw);

  vm_cache_t* cache_r = assertnotnull( rmm_allocpages(mm,
    ALIGN_CEIL(sizeof(vm_cache_t), PAGE_SIZE) / PAGE_SIZE) );
  vm_cache_init(cache_r);

  // { u64 vaddr = 0xdeadbeef;
  //   u64 vfn = vaddr_to_vfn(vaddr);
  //   dlog("—— vm_pagedir_access(addr 0x%llx, vfn 0x%llx) ——", vaddr, vfn);
  //   vm_pte_t pte = vm_pagedir_access(pagedir, vfn);
  //   uintptr hpage = (uintptr)(pte.outaddr << PAGE_SIZE_BITS);
  //   uintptr haddr = hpage + (uintptr)vaddr_offs(vaddr);
  //   // dlog("=> PTE" PTE_FMT ", host page 0x%lx, host address 0x%lx",
  //   //   PTE_FMTARGS(pte), hpage, haddr);
  //   dlog("vaddr 0x%llx => host address 0x%lx (page 0x%lx)", vaddr, haddr, hpage);
  // }

  // make sure cache lookups work
  { u64 vaddr = 0xdeadbeef;
    assert(vm_cache_lookup(cache_rw, vaddr, 1) == 0);
    vm_cache_add(cache_rw, VM_PAGE_ADDR(vaddr), 0x1044f000);
    uintptr haddr = vm_cache_lookup(cache_rw, vaddr, 1);
    //dlog("vm_cache_lookup(0x%llx) => %p", vaddr, (void*)haddr);
    assert(haddr == 0x1044feef);
    vm_cache_invalidate_one(cache_rw, vaddr);
    assert(vm_cache_lookup(cache_rw, vaddr, 1) == 0);
  }


  { // perform full real memory operations with virtual memory
    u64 vaddr = 0xdeadbee4;
    u32 value = 12345;
    usize npages = 8;
    vm_perm_t perm = VM_PERM_R | VM_PERM_W;

    void* haddr = assertnotnull( rmm_allocpages(mm, npages) );
    rerr_t err = vm_map(pagedir, (uintptr)haddr, vaddr, npages, perm);
    assertf(err == 0, "%s", rerr_str(err));

    dlog("VM_STORE(u32, 0x%llx, %u)", vaddr, value);
    VM_STORE(u32, cache_rw, pagedir, vaddr, value);
    value = VM_LOAD(u32, cache_rw, pagedir, vaddr);
    dlog("VM_LOAD(u32, 0x%llx) => %u", vaddr, value);
    value = VM_LOAD(u32, cache_rw, pagedir, vaddr);
    dlog("VM_LOAD(u32, 0x%llx) => %u", vaddr, value);

    // loading an invalid address
    //VM_LOAD(u64, cache_rw, pagedir, 0xffffffffffffffffllu); // error

    // loading the same address, which is 4B aligned, with a type of stronger
    // alignment will cause a cache miss and subsequent alignment error.
    //value = (u64)VM_LOAD(u64, cache, pagedir, vaddr); // error

    // reading from a page that is not mapped fails
    vm_unmap(pagedir, vaddr, npages);
    vm_cache_invalidate(cache_rw, vaddr, npages);
    //VM_LOAD(u32, cache_rw, pagedir, vaddr); // error

    // writing to a read-only page fails
    err = vm_map(pagedir, 0lu, vaddr, npages, VM_PERM_R);
    assertf(err == 0, "%s", rerr_str(err));
    VM_LOAD(u32, cache_r, pagedir, vaddr); // ok
    //VM_STORE(u32, cache_rw, pagedir, vaddr, value); // error

    rmm_freepages(mm, haddr);
  }

  // // allocate all pages (should panic just shy of rmm_avail_total pages)
  // for (u64 vaddr = VM_ADDR_MIN; vaddr <= VM_ADDR_MAX; vaddr += PAGE_SIZE) {
  //   u64 vfn = vaddr_to_vfn(vaddr);
  //   vm_pte_t pte = vm_pagedir_access(pagedir, vfn);
  // }

  rmm_dispose(mm);
  dlog("—— end %s", __FUNCTION__);
}
#endif // VM_RUN_TEST_ON_INIT


rerr_t init_vmem() {
  #if defined(VM_RUN_TEST_ON_INIT) && DEBUG
  test_vm();
  #endif

  return 0;
}
