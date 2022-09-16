// virtual memory
// See vmem.txt for in-depth documentation
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

#define vmtrace trace
#include "vm.h"

// VM_RUN_TEST_ON_INIT: define to run tests during exe init in DEBUG builds
#define VM_RUN_TEST_ON_INIT

// VM_TRACE: define to enable logging a lot of info via dlog
//#define VM_TRACE


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

  UNUSED static const char* vm_perm_str(vm_perm_t perm) {
    switch (perm) {
      case VM_PERM_R:  return "r-";
      case VM_PERM_W:  return "-w";
      case VM_PERM_RW: return "rw";
    }
    return "--";
  }
#endif


void vm_cache_init(vm_cache_t* cache) {
  memset(cache, 0xff, sizeof(vm_cache_t));
}


void vm_cache_invalidate_all(vm_cache_t* cache) {
  trace("cache %p invalidate all entries", cache);
  memset(cache->entries, 0xff, sizeof(cache->entries[0]) * VM_CACHE_LEN);
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
  assertf(IS_ALIGN2(vpaddr, PAGE_SIZE),
    "vpaddr not a page address 0x%llx", vpaddr);
  assertf(IS_ALIGN2(hpaddr, PAGE_SIZE),
    "hpaddr not a page address %p", (void*)hpaddr);

  vm_cache_ent_t* entry = VM_CACHE_ENTRY(cache, vpaddr);
  entry->haddr_diff = (u64)hpaddr - vpaddr;
  entry->tag = vpaddr;

  trace("%s 0x%llx => {.haddr_diff=0x%llx, .tag=0x%llx}",
    __FUNCTION__, vpaddr, entry->haddr_diff, entry->tag);

  return entry->haddr_diff;
}


// returns vm_cache_ent_t.haddr_diff
u64 _vm_cache_miss(vm_cache_t* cache, vm_map_t* map, u64 vaddr, vm_op_t op) {
  trace("%s 0x%llx op=0x%x", __FUNCTION__, vaddr, op);

  // check validity
  if UNLIKELY(VM_ADDR_MIN > vaddr || vaddr > VM_ADDR_MAX) {
    panic("invalid address 0x%llx (out of range)", vaddr); // FIXME
    return 0;
  }

  // check alignment
  if UNLIKELY(!IS_ALIGN2(vaddr, VM_OP_ALIGNMENT(op))) {
    const char* opname = VM_OP_TYPE(op) == VM_OP_LOAD ? "load from" : "store to";
    panic("misaligned %uB %s 0x%llx", VM_OP_ALIGNMENT(op), opname, vaddr);// FIXME
  }

  // get page table entry for the virtual page address (lookup via VFN)
  assert(!rwmutex_islocked(&map->lock));
  vm_map_rlock(map);
  vm_pte_t* pte = vm_map_access(map, VM_VFN(vaddr), /*is_access*/true);
  vm_map_runlock(map);

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


//————————————————————————————————————————————————————————————————————————————————————
#if defined(VM_RUN_TEST_ON_INIT) && DEBUG


static vm_map_t* nullable vm_map_create(rmm_t* mm) {
  // FIXME whole page allocated!
  static_assert(sizeof(vm_map_t) < PAGE_SIZE, "");
  vm_map_t* map = rmm_allocpages(mm, 1);
  if (!map)
    return NULL;
  if (vm_map_init(map, mm)) {
    rmm_freepages(mm, map);
    return NULL;
  }
  return map;
}


static void test_vm() {
  dlog("%s", __FUNCTION__);
  dlog("host pagesize:     %5u", (u32)os_pagesize());
  dlog("PAGE_SIZE:         %5u", PAGE_SIZE);
  dlog("PAGE_SIZE_BITS:    %5u", PAGE_SIZE_BITS);
  dlog("VM_ADDR_BITS:      %5u", VM_ADDR_BITS);
  dlog("VM_ADDR_MIN…MAX:   0x%llx … 0x%llx", VM_ADDR_MIN, VM_ADDR_MAX);
  dlog("VM_VFN_BITS:       %5u", VM_VFN_BITS);
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
  vm_map_t* map = assertnotnull( vm_map_create(mm) );

  // creat translation caches
  vm_cache_t* cache_rw = assertnotnull( rmm_allocpages(mm,
    ALIGN_CEIL(sizeof(vm_cache_t), PAGE_SIZE) / PAGE_SIZE) );
  vm_cache_init(cache_rw);

  vm_cache_t* cache_r = assertnotnull( rmm_allocpages(mm,
    ALIGN_CEIL(sizeof(vm_cache_t), PAGE_SIZE) / PAGE_SIZE) );
  vm_cache_init(cache_r);

  // #define PTE_FMT          "(0x%llx)"
  // #define PTE_FMTARGS(pte) ((pte).outaddr)
  // { u64 vaddr = 0xdeadbeef;
  //   u64 vfn = vaddr_to_vfn(vaddr);
  //   dlog("—— vm_map_access(addr 0x%llx, vfn 0x%llx) ——", vaddr, vfn);
  //   vm_pte_t pte = vm_map_access(map, vfn);
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
    vm_map_lock(map);
    rerr_t err = vm_map_add(map, &vaddr, (uintptr)haddr, npages, perm);
    vm_map_unlock(map);
    assertf(err == 0, "%s", rerr_str(err));

    dlog("VM_STORE(u32, 0x%llx, %u)", vaddr, value);
    VM_STORE(u32, cache_rw, map, vaddr, value);
    value = VM_LOAD(u32, cache_rw, map, vaddr);
    dlog("VM_LOAD(u32, 0x%llx) => %u", vaddr, value);
    value = VM_LOAD(u32, cache_rw, map, vaddr);
    dlog("VM_LOAD(u32, 0x%llx) => %u", vaddr, value);

    // loading an invalid address
    //VM_LOAD(u64, cache_rw, map, 0xffffffffffffffffllu); // error

    // loading the same address, which is 4B aligned, with a type of stronger
    // alignment will cause a cache miss and subsequent alignment error.
    //value = (u64)VM_LOAD(u64, cache, map, vaddr); // error

    // reading from a page that is not mapped fails
    vm_map_lock(map);
    vm_map_del(map, vaddr, npages);
    vm_map_unlock(map);
    vm_cache_invalidate_all(cache_rw);
    //VM_LOAD(u32, cache_rw, map, vaddr); // error

    // writing to a read-only page fails
    vm_map_lock(map);
    err = vm_map_add(map, &vaddr, 0lu, npages, VM_PERM_R);
    vm_map_unlock(map);
    assertf(err == 0, "%s", rerr_str(err));
    value = VM_LOAD(u32, cache_r, map, vaddr); // ok
    //VM_STORE(u32, cache_rw, map, vaddr, value); // error

    rmm_freepages(mm, haddr);
  }

  // // allocate all pages (should panic just shy of rmm_avail_total pages)
  // for (u64 vaddr = VM_ADDR_MIN; vaddr <= VM_ADDR_MAX; vaddr += PAGE_SIZE) {
  //   u64 vfn = vaddr_to_vfn(vaddr);
  //   vm_pte_t pte = vm_map_access(map, vfn);
  // }

  vm_map_dispose(map);

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
