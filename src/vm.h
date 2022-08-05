// virtual memory (internal)
// SPDX-License-Identifier: Apache-2.0
#pragma once
RSM_ASSUME_NONNULL_BEGIN

// VM_ZERO_PAGES: define to zero memory pages before use
#define VM_ZERO_PAGES

// virtual memory address space configuration
// VM_ADDR_BITS: addressable space (effectively: (1<<VM_ADDR_BITS)-VM_ADDR_MIN)
// VM_ADDR_OFFS_BITS: bits needed for address in-page offset (address without VFN)
// VM_ADDR_MIN: start of vm address space; first valid address
// VM_ADDR_MAX: last valid vm address (0xffffffffffff0000 @ 4096 PS & 48bit AS)
// VM_VFN_BITS: bits needed for VFN (VM_ADDR_BITS-VM_ADDR_OFFS_BITS)
#define VM_ADDR_BITS       48u /* 256 TiB */
#define VM_ADDR_OFFS_BITS  PAGE_SIZE_BITS
#define VM_ADDR_MIN        (1llu << VM_ADDR_OFFS_BITS)
#define VM_ADDR_MAX        ((1llu << VM_ADDR_BITS) - 1llu)
#define VM_VFN_BITS        (VM_ADDR_BITS - VM_ADDR_OFFS_BITS)

// page table constants
//   VM_PTAB_BITS: bits per page table; a divisor of VM_VFN_BITS.
//   VM_PTAB_BITS should be a value that makes each pagetable be one host page in size:
//     (2^VM_PTAB_BITS)*sizeof(vm_pte_t) = pagesize
//     e.g. (2^8=256)*8 = 2048
//          (2^9=512)*8 = 4096
//          (2^11=2048)*8 = 16384
#define VM_PTAB_LEVELS  4u /* number of page-table levels */
#define VM_PTAB_BITS    9u /* =ILOG2(PAGE_SIZE/sizeof(vm_pte_t)) */
#define VM_PTAB_LEN     ((usize)(1lu << VM_PTAB_BITS)) /* number of PTEs in a table */
#define VM_PTAB_SIZE    ((usize)PAGE_SIZE) /* byte size of one page table */

// cache constants
#define VM_CACHE_INDEX_BITS  8lu /* = ILOG2(VM_CACHE_LEN) */
#define VM_CACHE_INDEX_MASK  ((1llu << VM_CACHE_INDEX_BITS) - 1llu)
#define VM_CACHE_LEN         ((usize)(1lu << VM_CACHE_INDEX_BITS))
#define VM_CACHE_TAG_BITS    ((VM_ADDR_BITS - PAGE_SIZE_BITS) - VM_CACHE_INDEX_BITS)
#define VM_CACHE_DEL_TAG     ((1llu << VM_CACHE_TAG_BITS) + 1)

// vm_pte_t - page table entry
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

  // usize nuse      : VM_PTAB_BITS; // number of sub-entries in use
  // usize _reserved : 3;

  usize _reserved : 12;
  u64   outaddr   : 52;
#else
  #error TODO
#endif
} vm_pte_t;

// vm_ptab_t - virtual memory page table
typedef vm_pte_t* vm_ptab_t;

// vm_pagedir_t is a page directory, managing mapping between virtual and host pages
typedef struct {
  rmm_t*    mm;
  vm_ptab_t root; // L0
} vm_pagedir_t;

// vm_cache_ent_t is the type of vm_cache_t entries
typedef struct {
  uintptr haddr; // address of host page
  u64     tag;   // virtual address tag
} vm_cache_ent_t;

// vm_cache_t is a translation cache (aka TLB; Translation Lookaside Buffer.)
// Maps virtual page addresses to host page addresses. Not thread safe.
typedef struct {
  vm_cache_ent_t entries[VM_CACHE_LEN];
} vm_cache_t;


// VM_VFN(u64 vaddr) calculates the page address of a virtual address
// aka Virtual Frame Number. It's the address's bits without VM_ADDR_OFFS_BITS.
// e.g. 0xdeadbeef 11011110101011011011111011101111 / PAGE_SIZE(4096)
//    = 0xdeadb    11011110101011011011
#define VM_VFN(vaddr)  ( (u64)(vaddr) >> VM_ADDR_OFFS_BITS )

// VM_ADDR_OFFSET(u64 vaddr) calculates the page offset of an address.
// It's the upper VM_ADDR_OFFS_BITS bits of a virtual address.
// e.g. 0xdeadbeef 11011110101011011011111011101111 & (PAGE_SIZE(4096) - 1)
//    = 0xeef                          111011101111
#define VM_ADDR_OFFSET(vaddr)  ( (vaddr) & (u64)(PAGE_SIZE - 1) )

// VM_CACHE_VFN_TAG(u64 vfn) calculates the tag for a cache entry for a VFN
#define VM_CACHE_VFN_TAG(vfn)  ((u64)(vfn) >> VM_CACHE_INDEX_BITS)

// VM_CACHE_VFN_INDEX(u64 vfn) calculates the cache table index for a VFN
#define VM_CACHE_VFN_INDEX(vfn)  ((u64)(vfn) & VM_CACHE_INDEX_MASK)

// vm_cache_ent_t* VM_CACHE_ENTRY(vm_cache_t cache, u64 vfn) accesses the cache
// entry for a VFN.
#define VM_CACHE_ENTRY(cache, vfn)  (&(cache)->entries[VM_CACHE_VFN_INDEX(vfn)])


// VM_STORE performs a store to host memory using a virtual address.
// E.g. VM_STORE(u32, vm_cache, vm_pagedir, 0xdeadbee0, 1234)
// This becomes 10 x86_64 instructions, or 10 arm64/armv8-1 instructions (clang-14 -O3).
// See https://godbolt.org/z/esT98Y9Yd
#define VM_STORE(type, vm_cache, pagedir, vaddr, value) { \
  assertf(IS_ALIGN2(vaddr, _Alignof(type)), "misaligned store 0x%llx", vaddr); \
  uintptr haddr__ = vm_translate((vm_cache), (pagedir), (vaddr)); \
  trace("VM_STORE %s %p", #type, (void*)haddr__); \
  *(type*)haddr__ = (value); \
}

// VM_LOAD performs a load from host memory using a virtual address.
// E.g. u32 value = VM_LOAD(u32, vm_cache, vm_pagedir, 0xdeadbee0);
#define VM_LOAD(type, vm_cache, pagedir, vaddr) ({ \
  assertf(IS_ALIGN2(vaddr, _Alignof(type)), "misaligned load 0x%llx", vaddr); \
  uintptr haddr__ = vm_translate((vm_cache), (pagedir), (vaddr)); \
  trace("VM_LOAD %s %p", #type, (void*)haddr__); \
  *(type*)haddr__; \
})


// vm_pagedir_init initializes a new vm_pagedir_t, sourcing backing memory from mm.
// Returns false if allocating the root table in mm failed.
bool vm_pagedir_init(vm_pagedir_t*, rmm_t* mm);

// vm_pagedir_dispose frees up resources of a page directory,
// which becomes invalid after this call.
void vm_pagedir_dispose(vm_pagedir_t*);


// vm_cache_init initializes a vm_cache_t
void vm_cache_init(vm_cache_t*);

// vm_cache_invalidate invalidates the entire cache
void vm_cache_invalidate(vm_cache_t*);

// vm_cache_invalidate_one invalidates one page.
// vaddr can be a canonical address or a page address.
ALWAYS_INLINE void vm_cache_invalidate_one(vm_cache_t* cache, u64 vaddr) {
  // VM_CACHE_DEL_TAG is a value larger than max possible lookup tag value
  VM_CACHE_ENTRY(cache, vaddr)->tag = VM_CACHE_DEL_TAG;
}

// vm_translate converts a virtual address to its corresponding host address
static uintptr vm_translate(vm_cache_t* trc, vm_pagedir_t* pagedir, u64 vaddr);

// vm_cache_lookup looks up the host page address for a virtual address.
// Returns 0 if the virtual page is not present in the cache.
uintptr vm_cache_lookup(vm_cache_t*, u64 vaddr);

// vm_cache_populate resolves vaddr in the page directory and adds it to the cache.
// Returns the resolved host page address.
uintptr vm_cache_populate(vm_cache_t*, vm_pagedir_t*, u64 vaddr);


//————————————————————————————————————————————————————————————————————————————————————
// inline implementations

ALWAYS_INLINE static uintptr vm_translate(
  vm_cache_t* trc, vm_pagedir_t* pagedir, u64 vaddr)
{
  vm_cache_ent_t* entry = VM_CACHE_ENTRY(trc, vaddr);
  return ( // get host page address
    UNLIKELY(entry->tag != VM_CACHE_VFN_TAG(VM_VFN(vaddr))) ?
      vm_cache_populate(trc, pagedir, vaddr) :
      entry->haddr
  ) + (vaddr & (u64)(PAGE_SIZE - 1)); // add offset
}


RSM_ASSUME_NONNULL_END
