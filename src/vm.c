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
#define PTE_FMTARGS(pte) ((pte).outaddr)


// getbits returns the (right adjusted) n-bit field of x that begins at position p.
// We assume that bit position 0 is at the right end and that n and p
// are sensible positive values.
// For example, getbits(x, 4, 3) returns the three bits in bit
// positions 4, 3 and 2, right adjusted.
// [from K&R, 2nd Ed., pg. 49: get n bits from position p]
inline static u64 getbits(u64 x, u32 p, u32 n) {
  return (x >> (p + 1u - n)) & ~(~0llu << n);
}


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


static vm_ptab_t nullable vm_ptab_create(rmm_t* mm) {
  // note: VM_PTAB_SIZE is always a multiple of PAGE_SIZE
  vm_ptab_t ptab = rmm_allocpages(mm, VM_PTAB_SIZE / PAGE_SIZE);
  if UNLIKELY(ptab == NULL)
    return NULL;
  memset(ptab, 0, VM_PTAB_SIZE);
  return ptab;
}


static void vm_ptab_free(rmm_t* mm, vm_ptab_t ptab) {
  rmm_freepages(mm, ptab);
}


// void vm_map_assert_locked(vm_map_t*)
#define vm_map_assert_locked(m)  assertf(rwmutex_islocked(&(m)->lock), "map locked")
#define vm_map_assert_rlocked(m) assertf(rwmutex_isrlocked(&(m)->lock), "map rlocked")


rerr_t vm_map_init(vm_map_t* map, rmm_t* mm) {
  rerr_t err = rwmutex_init(&map->lock);
  if UNLIKELY(err)
    return err;
  vm_ptab_t ptab = vm_ptab_create(mm); // root page table
  if UNLIKELY(!ptab) {
    trace("failed to allocate root page table");
    return rerr_nomem;
  }
  trace("allocated L%u page table %p +0x%lx", 1, ptab, VM_PTAB_SIZE);
  map->root = ptab;
  map->mm = mm;
  map->min_free_vfn = 0;
  return 0;
}


void vm_map_dispose(vm_map_t* map) {
  assert(!rwmutex_isrlocked(&map->lock)); // map should not be locked
  rwmutex_dispose(&map->lock);
  vm_ptab_free(map->mm, map->root);
  dlog("TODO: free all PTEs");
}


#if DEBUG
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
#endif


// returns pte on success or NULL if out of memory
static vm_pte_t* nullable alloc_backing_page(vm_map_t* map, vm_pte_t* pte) {
  void* haddr = rmm_allocpages(map->mm, 1);
  if UNLIKELY(!haddr) {
    trace("FAILED to allocate backing page");
    panic("TODO: purge a least-recently-used page");
  }

  trace("allocated backing page %p", haddr);

  u64 hfn = (u64)(uintptr)haddr >> PAGE_SIZE_BITS;
  pte->outaddr = hfn;

  return hfn ? pte : NULL;
}


// vm_map_access returns the page table entry of a Virtual Frame Number
static vm_pte_t* nullable vm_map_access(vm_map_t* map, u64 vfn, bool isaccess) {
  assertf(vfn <= VM_VFN_MAX, "invalid VFN 0x%llx", vfn);
  u32 bits = 0;
  u64 masked_vfn = vfn;
  vm_ptab_t ptab = map->root;
  u8 level = 1;
  vm_pte_t* pte = NULL;

  vm_map_assert_rlocked(map);

  for (;;) {
    usize index = (usize)getbits(masked_vfn, VFN_BITS - (1+bits), VM_PTAB_BITS);
    pte = &ptab[index];

    trace(
      "lookup vfn 0x%llx L%u; index %zu = getbits(0x%llx, %u-(1+%u), %u)",
      vfn+1, level, index, masked_vfn, VFN_BITS, bits, VM_PTAB_BITS);

    if (level == VM_PTAB_LEVELS) {
      if UNLIKELY(*(u64*)pte == 0) {
        pte = NULL;
        break;
      }
      // if there's no backing page, allocate one
      if (pte->outaddr == 0)
        pte = alloc_backing_page(map, pte);
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

  return pte;
}


rerr_t vm_map_del(vm_map_t* map, u64 vaddr, usize npages) {
  // TODO: rewrite this function like vm_map (based on logic in vm_map_access)
  // TODO: decrement pte->nuse of branches
  u64 vfn = VM_VFN(vaddr);
  u64 vfn_end = vfn + npages;

  trace("unmap 0x%llx (vfn 0x%llx, %zu pages)",
    VM_PAGE_ADDR(vaddr), vfn, npages);

  vm_map_assert_locked(map);

  for (; vfn < vfn_end; vfn++) {
    vm_pte_t* pte = vm_map_access(map, vfn, /*is_access*/false);
    if UNLIKELY(!pte)
      return rerr_not_found;
    *pte = (vm_pte_t){0};
  }

  if (vfn < map->min_free_vfn)
    map->min_free_vfn = vfn;

  return 0;
}


// vm_map_iter_f is the visitor callback type
// - nuse is the vm_pte_t.nuse field for ptab
// - level is the level of ptab (root is level 0)
// - index is the index of the current pte in ptab
// - vfn is the VFN of the current pte in ptab
// - data is whatever value was provided to vm_map_iter
// Return number of indices to advance. Return 0 to stop iteration.
typedef u32(vm_map_iter_f)(
  vm_ptab_t ptab, u32 nuse, u32 level, u32 index, u64 vfn, uintptr data);


// vm_map_ffwd fast-forwards to page table containing start_vfn
// static bool vm_map_ffwd ...


// vm_map_iter0 iterates over all entries in ptab
// returns true if caller should keep going
static bool vm_map_iter0(
  vm_map_iter_f callback,   // user callback
  uintptr       userdata,   // user data for callback
  vm_ptab_t     ptab,       // page table at level
  u32           ptab_nuse,  // vm_pte_t.nuse field for ptab
  u32           level)      // level of page table (root = 0)
{
  #undef dlogx
  #define dlogx(fmt, args...) log("%*s" fmt, (int)level*2, "", ##args)

  dlogx("*** vm_map_iter0");

  u32 bits = (VM_PTAB_BITS*(VM_PTAB_LEVELS-1)) - (level * VM_PTAB_BITS);
  u64 npages = 1llu << bits; // i.e. L1=0x8000000, L2=0x40000, L3=0x200, L4=1
  u64 vfn_mask = ~0llu ^ ((1llu << bits)-1);

  for (u32 index = 0; index < VM_PTAB_LEN;) {
    vm_pte_t* pte = &ptab[index];
    u64 vfn = ((u64)index * npages);

    if (level == VM_PTAB_LEVELS-1) {
      dlogx("iter_visit page 0x%09llx (parent[%u])", VM_VFN_VADDR(vfn), index);
    } else {
      u64 last_vfn = ((vfn + npages) & vfn_mask) - 1;
      if (level == 0) {
        dlogx("iter_visit block L1 0x%09llx…0x%09llx 0x%llx [%u] nuse=%u",
          VM_VFN_VADDR(vfn), VM_VFN_VADDR(last_vfn),
          npages, index, pte->nuse);
      } else {
        dlogx("iter_visit block L%u 0x%09llx…0x%09llx 0x%llx [%u] nuse=%u",
          level+1, VM_VFN_VADDR(vfn), VM_VFN_VADDR(last_vfn),
          npages, index, pte->nuse);
      }
    }

    bool is_leaf_level = level == VM_PTAB_LEVELS-1;
    if (is_leaf_level) { log("STOP"); abort(); } // XXX
    if (is_leaf_level | (*(u64*)pte == 0)) {
      // either we reached the last level or encountered an unused sub table
      u32 index_incr = callback(ptab, ptab_nuse, level, index, vfn, userdata);
      assert_no_add_overflow(index, index_incr);
      index += index_incr;
      if (index_incr == 0)
        return false;
      continue; // visit next table
    }

    // visit sub table
    vm_ptab_t child_ptab = (vm_ptab_t)(uintptr)(pte->outaddr << PAGE_SIZE_BITS);
    if (!vm_map_iter0(callback, userdata, child_ptab, pte->nuse, level + 1))
      return false;
    index++;
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
  vm_ptab_t     ptab,       // page table at level
  u32           ptab_nuse,  // vm_pte_t.nuse field for ptab
  u32           level)      // level of page table (root = 0)
{
  #undef dlogx
  #define dlogx(fmt, args...) log("%*s" fmt, (int)level*2, "", ##args)

  u32 start_index = (u32)(index_vfn >> (64 - VM_PTAB_BITS));

  // VFN 0xfacedeadbeef
  //     0000000000000000000000000000 111110101 100111011 011110101 011011010
  //                                  L1        L2        L3        L4
  dlogx("start_vfn   0x%09llx (vaddr %012llx)", start_vfn, VM_VFN_VADDR(start_vfn));
  dlogx("            %s", fmtbitsval(start_vfn));
  dlogx("start_index %-3u%*s",
    start_index,
    64-(VFN_BITS - VM_PTAB_BITS - (level * VM_PTAB_BITS))-3,
    fmtbits(&start_index, VM_PTAB_BITS));

  // bits is the inverse count, i.e. L1=27, L2=18, L3=9, L4=0
  u32 bits = (VM_PTAB_BITS*(VM_PTAB_LEVELS-1)) - (level * VM_PTAB_BITS);
  u64 vfn_mask = ~0llu ^ ((1llu << bits)-1);
  u64 npages = VM_PTAB_NPAGES(level+1); // i.e. L1=0x8000000, L2=0x40000, L3=0x200, L4=1

  for (u32 index = start_index; index < VM_PTAB_LEN;) {
    vm_pte_t* pte = &ptab[index];

    u64 vfn = (start_vfn & vfn_mask) + ((u64)(index - start_index) * npages);

    if (level == VM_PTAB_LEVELS-1) {
      dlogx("iter_visit page 0x%09llx [%u] %s",
        VM_VFN_VADDR(vfn), index, (*(u64*)pte ? "mapped" : "free"));
    } else {
      u64 last_vfn = ((vfn + npages) & vfn_mask) - 1;
      if (level == 0) {
        dlogx("iter_visit block L1 0x%09llx…0x%09llx 0x%llx [%u] nuse=%u",
          VM_VFN_VADDR(vfn), VM_VFN_VADDR(last_vfn),
          npages, index, pte->nuse);
      } else {
        dlogx("iter_visit block L%u 0x%09llx…0x%09llx 0x%llx [%u] nuse=%u",
          level+1, VM_VFN_VADDR(vfn), VM_VFN_VADDR(last_vfn),
          npages, index, pte->nuse);
      }
    }

    bool end = (level == VM_PTAB_LEVELS-1) | (*(u64*)pte == 0);
    if ((end & (vfn < start_vfn)) | ((!end) & (vfn + npages < start_vfn))) {
      // vfn (or end of block's vfn) is less than start_vfn
      index++;
      continue;
    }

    u32 index_incr = callback(ptab, ptab_nuse, level, index, vfn, userdata);
    assert_no_add_overflow(index, index_incr);
    if (index_incr == 0)
      return false;

    if (!end) {
      // visit sub table
      vm_ptab_t child_ptab = (vm_ptab_t)(uintptr)(pte->outaddr << PAGE_SIZE_BITS);
      bool cont;
      if (index == start_index) {
        u64 ivfn = index_vfn << VM_PTAB_BITS;
        cont = vm_map_iter1(
          callback, userdata, start_vfn, ivfn, child_ptab, pte->nuse, level+1);
      } else {
        cont = vm_map_iter0(callback, userdata, child_ptab, pte->nuse, level+1);
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
  dlog("iter start 0x%llx (VFN 0x%llx)", start_vaddr, vfn);
  if (vfn == 0)
    vm_map_iter0(fn, data, map->root, map->root_nuse, 0);
  u64 index_vfn = vfn << ((sizeof(vfn)*8) - VFN_BITS);
  vm_map_iter1(fn, data, vfn, index_vfn, map->root, map->root_nuse, 0);
}


typedef struct {
  vm_ptab_t ptab;
  u32       nuse;
} findspace_nuse_t;

typedef struct {
  u64 want_npages;  // npages passed to vm_map_findspace
  u64 found_npages; // number of free consecutive pages found so far
  u64 start_vaddr;  // address of first free page

  // fields for tracking nuse
  // vm_ptab_t curr_ptab;
  // u32       curr_nuse;

  findspace_nuse_t nuse[VM_PTAB_LEVELS];
} findspace_t;


// FINDSPACE_SKIP_SMALL_HOLES_OPT: define to enable optimization.
// I haven't verified that this is actually faster, but it seems like a good idea.
//#define FINDSPACE_SKIP_SMALL_HOLES_OPT


#ifdef FINDSPACE_SKIP_SMALL_HOLES_OPT
  static u32 findspace_restfree(vm_ptab_t ptab, u32 index, u64 vfn, findspace_t* ctx) {
    // find last used entry
    u32 next_index = VM_PTAB_LEN - 1;
    for (;;) {
      if (*(u64*)&ptab[next_index]) {
        if (next_index == VM_PTAB_LEN - 1) {
          // last entry of ptab is used; skip entire ptab
          ctx->start_vaddr = 0;
          ctx->found_npages = 0;
          return VM_PTAB_LEN;
        }
        next_index++;
        break;
      }
      next_index--;
      assertf(next_index > index, "invalid nuse count of ptab %p", ptab);
    }
    u32 skipped_npages = next_index - index;
    u32 free_npages = VM_PTAB_LEN - next_index;
    ctx->start_vaddr = VM_VFN_VADDR(vfn + skipped_npages);
    ctx->found_npages = free_npages;
    // dlog("skip past %u unusable pages (region too small)", skipped_npages);
    // dlog("use free tail: %llx (%u)", VM_VFN_VADDR(vfn + skipped_npages), free_npages);
    return VM_PTAB_LEN;
  }
#endif // FINDSPACE_SKIP_SMALL_HOLES_OPT


static u32 vm_map_findspace_visitor(
  vm_ptab_t ptab, u32 ptab_nuse, u32 level, u32 index, u64 vfn, uintptr data)
{
  findspace_t* ctx = (findspace_t*)data;

  findspace_nuse_t* curr_nuse = &ctx->nuse[level];
  if (curr_nuse->ptab != ptab) {
    curr_nuse->ptab = ptab;
    curr_nuse->nuse = 0;
  }

  // visitor for free entries lives in separate function
  if (*(u64*)&ptab[index]) {
    // entry ptab[index] is not free
    ctx->start_vaddr = 0;
    ctx->found_npages = 0;
    // skip past immediate used entries (terminal level only)
    u32 i = index + 1;
    if (level == VM_PTAB_LEVELS-1) {
      while (i < VM_PTAB_LEN && *(u64*)&ptab[i])
        i++;
    }
    u32 advance = i - index;
    curr_nuse->nuse += advance; // keep track of nuse for findspace_restfree
    return advance;
  }

  // entry ptab[index] is free, unused

  #ifdef FINDSPACE_SKIP_SMALL_HOLES_OPT
    // Optimization: If the current terminal ptab has used entries which
    // we have not yet encountered, and the remaining free entries of ptab
    // are less than what we need, skip to the last region of free entries.
    if (level == VM_PTAB_LEVELS-1 && curr_nuse->nuse < ptab_nuse) {
      u32 rem_free = (VM_PTAB_LEN - index) - (ptab_nuse - curr_nuse->nuse);
      if (rem_free < ctx->want_npages - ctx->found_npages) {
        // There are less free pages available than we need, which means that we know
        // that this ptab will not contain a range large enough to satisfy
        // ctx.want_npages.
        return findspace_restfree(ptab, index, vfn, ctx);
      }
    }
  #endif // FINDSPACE_SKIP_SMALL_HOLES_OPT

  u32 advance = 1;

  if (curr_nuse->nuse == ptab_nuse) {
    // remainder of ptab entires are free (we have passed all used entries)
    advance = VM_PTAB_LEN - index;
  } else {
    // scan forward to the next used entry, or end of ptab
    u64 need_npages = ctx->want_npages - ctx->found_npages;
    const u32 end_index = (u32)MIN(
      (u64)VM_PTAB_LEN,
      (u64)index + IDIV_CEIL(need_npages, VM_PTAB_NPAGES(level+1)) );
    u32 i = index;
    while (i < end_index && *(u64*)&ptab[i] == 0)
      i++;
    advance = i - index;
  }

  if (ctx->start_vaddr == 0)
    ctx->start_vaddr = VM_VFN_VADDR(vfn);

  ctx->found_npages += VM_PTAB_NPAGES(level+1) * (u64)advance;

  // return 0 if found_npages>=want_npages, else return advance
  return advance * (ctx->found_npages < ctx->want_npages);
}


rerr_t vm_map_findspace(vm_map_t* map, u64* vaddrp, u64 npages) {
  dlog("————————————————— %s —————————————————", __FUNCTION__);
  vm_map_assert_rlocked(map);

  // *vaddrp holds the minimum desired address
  u64 vaddr = *vaddrp;
  if (vaddr < VM_ADDR_MIN)
    vaddr = VM_ADDR_MIN;

  // is vaddr+npages beyond end of address range?
  if (npages > VM_ADDR_MAX/PAGE_SIZE ||
      vaddr/PAGE_SIZE > VM_ADDR_MAX/PAGE_SIZE - npages)
  {
    return rerr_nomem;
  }

  // adjust start to minimum free VFN
  if (VM_VFN(vaddr) < map->min_free_vfn)
    vaddr = VM_VFN_VADDR(map->min_free_vfn);

  dlog("vm_map_iter(start_vaddr=%llx, want_npages=%llu)", vaddr, npages);
  findspace_t ctx = { .want_npages = npages };
  vm_map_iter(map, vaddr, &vm_map_findspace_visitor, (uintptr)&ctx);

  if (ctx.found_npages < npages) {
    panic("LOLCAT did not find a large-enough region of pages");
    return rerr_nomem;
  }

  panic(
    "LOLCAT found %10llu pages: %012llx…%012llx\n"
    "                              expect: %012llx\n",
    ctx.found_npages, ctx.start_vaddr,
    ctx.start_vaddr + ((ctx.found_npages-1) * PAGE_SIZE),
    VM_PAGE_ADDR(0xfacedeba4eef));

  *vaddrp = ctx.start_vaddr;
  return 0;
}


static vm_ptab_t nullable new_ptab(vm_map_t* map, vm_ptab_t parent, usize index) {
  vm_ptab_t ptab = vm_ptab_create(map->mm);
  if UNLIKELY(!ptab) {
    // out of backing memory
    // TODO: try to free some memory by purging unused tables
    dlog("TODO: purge unused page tables (except for the root)");
    return NULL;
  }

  u64 ptab_hfn = ((u64)(uintptr)ptab) >> PAGE_SIZE_BITS;
  parent[index] = (vm_pte_t){ .outaddr = ptab_hfn };

  trace("allocated page table %p (HFN 0x%llx) +0x%lx at parent[%zu]",
    ptab, ptab_hfn, VM_PTAB_SIZE, index);

  assertf(IS_ALIGN2((u64)(uintptr)ptab, PAGE_SIZE),
    "ptab_create did not allocate vm_ptab_t on a page boundary (0x%lx/%u)",
    (uintptr)ptab, PAGE_SIZE);

  return ptab;
}


rerr_t vm_map_add(
  vm_map_t* map, u64* vaddrp, uintptr haddr, usize npages, vm_perm_t perm)
{
  if (npages == 0)
    return 0;

  u64 vaddr = *vaddrp;

  assertf(vaddr >= VM_ADDR_MIN && VM_ADDR_MAX >= vaddr, "invalid vaddr 0x%llx", vaddr);
  assertf(IS_ALIGN2((uintptr)haddr, PAGE_SIZE), "haddr 0x%lx not page aligned", haddr);
  vm_map_assert_locked(map);

  rerr_t err = 0;
  u64 vfn = VM_VFN(vaddr);
  const u64 vfn_end = vfn + npages;

  vm_pte_t* pte = NULL;

  while (vfn < vfn_end) {
    u32 bits = 0;
    u64 masked_vfn = vfn;
    vm_ptab_t ptab = map->root;
    u8 level = 1;
    usize index;

  visit_next_ptab:
    index = (usize)getbits(masked_vfn, VFN_BITS - (1+bits), VM_PTAB_BITS);

    pte = &ptab[index];

    if (level < VM_PTAB_LEVELS) {
      // page table branch
      bits += VM_PTAB_BITS;
      masked_vfn = getbits(masked_vfn, VFN_BITS - (1+bits), VFN_BITS - bits);
      level++;
      if UNLIKELY(*(u64*)pte == 0) {
        // allocate new table
        if UNLIKELY( !(ptab = new_ptab(map, ptab, index)) ) {
          err = rerr_nomem;
          goto error;
        }
      } else {
        // traverse existing table
        assert(pte->outaddr != 0);
        ptab = (vm_ptab_t)(uintptr)(pte->outaddr << PAGE_SIZE_BITS);
      }
      pte->nuse++;
      goto visit_next_ptab;
    }

    // page table leaf entry
    assert(npages > 0);
    for (;;) {
      trace("map 0x%llx => 0x%lx %s (vfn 0x%llx, ptab[%zu])",
        VM_VFN_VADDR(vfn), haddr, vm_perm_str(perm), vfn, index);

      if UNLIKELY(*(u64*)pte) {
        dlog("trying to map already-mapped page at vaddr 0x%llx",
          VM_VFN_VADDR(vfn));
        err = rerr_exists;
        goto error;
      }

      pte->outaddr = (u64)( (uintptr)haddr >> PAGE_SIZE_BITS );
      assert(perm != 0); // pte "is mapped" checks relies on pte not being zero
      *((u8*)pte) = *((u8*)pte) | perm;

      index++;
      vfn++;
      haddr += PAGE_SIZE * !!haddr; // no-op in case haddr==0

      if (vfn == vfn_end || index == VM_PTAB_LEN)
        break;

      pte = &ptab[index];
    }
    // either we reached end of this table
    // or we are done mapping all pages

  } // while (vfn < vfn_end)

  if (VM_VFN(vaddr) == map->min_free_vfn)
    map->min_free_vfn = vfn_end;

  return 0;

error:
  assert(err);
  if (vfn > VM_VFN(vaddr)) {
    dlog("TODO unmap what was partially mapped");
    // note: also decrement pte->nuse of branches
  }
  return err;
}


// // vm_map_translate translates a virtual address to its corresponding host address
// uintptr vm_map_translate(vm_map_t* map, u64 vaddr) {
//   vm_pte_t* pte = vm_map_access(map, VM_VFN(vaddr), NULL);
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


#if defined(VM_RUN_TEST_ON_INIT) && DEBUG
static void test_vm() {
  dlog("%s", __FUNCTION__);
  dlog("host pagesize:     %5u", (u32)os_pagesize());
  dlog("PAGE_SIZE:         %5u", PAGE_SIZE);
  dlog("PAGE_SIZE_BITS:    %5u", PAGE_SIZE_BITS);
  dlog("VM_ADDR_BITS:      %5u", VM_ADDR_BITS);
  dlog("VM_ADDR_MIN…MAX:   0x%llx … 0x%llx", VM_ADDR_MIN, VM_ADDR_MAX);
  dlog("VFN_BITS:          %5u", VFN_BITS);
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
    vm_cache_invalidate(cache_rw, vaddr, npages);
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
