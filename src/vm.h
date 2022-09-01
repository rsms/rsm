// virtual memory (internal)
// SPDX-License-Identifier: Apache-2.0
#pragma once
#include "thread.h"
RSM_ASSUME_NONNULL_BEGIN

// virtual memory address space configuration
// VM_ADDR_BITS: addressable space (effectively: (1<<VM_ADDR_BITS)-VM_ADDR_MIN)
// Note that the bits needed for address in-page offset is PAGE_SIZE_BITS.
// VM_ADDR_MIN: start of vm address space; first valid address
// VM_ADDR_MAX: last valid vm address (0xffffffffffff0000 @ 4096 PS & 48bit AS)
// VM_ADDR_OFFS_MASK: AND with vaddr to get its offset within a page
// VM_ADDR_PAGE_MASK: AND with vaddr to get the virtual page address
// e.g. 1111111111111111111111111111111111111111111111111111000000000000
#define VM_ADDR_BITS       48u /* 256 TiB */
#define VM_ADDR_MIN        ((u64)( 1llu << PAGE_SIZE_BITS ))
#define VM_ADDR_MAX        ((u64)( (1llu << VM_ADDR_BITS) - 1llu ))
#define VM_ADDR_OFFS_MASK  ((u64)( PAGE_SIZE - 1llu ))
#define VM_ADDR_PAGE_MASK  ((u64)( ~0llu ^ ((u64)PAGE_SIZE - 1llu) ))

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
// VM_CACHE_INDEX_BITS: number of bottom bits of a VFN to use for indexing
// VM_CACHE_INDEX_VFN_MASK: AND with a VFN to get the corresponding cache index
// VM_CACHE_LEN: number of entries in a cache
// VM_CACHE_DEL_TAG: value larger than any valid tag
#define VM_CACHE_INDEX_BITS      8lu /* == ILOG2(VM_CACHE_LEN) */
#define VM_CACHE_INDEX_VFN_MASK  ((1llu << VM_CACHE_INDEX_BITS) - 1llu)
#define VM_CACHE_LEN             ((usize)(1lu << VM_CACHE_INDEX_BITS))
#define VM_CACHE_DEL_TAG         (~0llu)

// VM_CACHE_TAG_MASK is a neat trick we use to create a bitmask used for the
// "tag" value of a vm cache entry to both verify that the entry's address
// is correct and also check the address alignment.
//
// This is possible because we create a mask with bits set for the page address,
// and also set bits for the alignment order.
// When the tag is XORed in vm_translate's check, we get a non-zero value if
// the address has the wrong alignment.
//
// When we perform an operation on virtual memory we need to do the following:
//
//   1. Load the cache table entry for the corresponding page address.
//      This is a "lossy" process; the cache table only uses a few bits of the
//      page address as an index; the entry might be for a different page.
//   2. Check if the entry is indeed for this page address (cache miss if not.)
//   3. Check that the address is within valid range (VM_ADDR_BITS.)
//   4. Check the address's alignment (alignment violation if not.)
//   5. Load the host page address from the cache entry.
//   6. Calculate the canonical host address by adding the virtual address's
//      offset (bottom PAGE_SIZE_BITS) to the host page address.
//   7. Perform the memory operation (e.g. load, store etc) on the host address.
//
// Let's have a look at some examples.
// This is how we construct the mask for checking tags:
//
//          page address          alignment bits
//   ┌─~~~────────┴──────────┐         ┌┴┐
//   11 … 11111111111111111111000000000111
//                            └─────┬────┘
//                             page offset
//
// For example, with PAGE_SIZE_BITS=12:
//   …TAG_MASK(1)  = 1111111111111111111111111111111111111111111111111111000000000000
//   …TAG_MASK(2)  = 1111111111111111111111111111111111111111111111111111000000000001
//   …TAG_MASK(4)  = 1111111111111111111111111111111111111111111111111111000000000011
//   …TAG_MASK(8)  = 1111111111111111111111111111111111111111111111111111000000000111
//   …TAG_MASK(16) = 1111111111111111111111111111111111111111111111111111000000001111
//   ... and so on.
//   Max alignment value the cache can handle is PAGE_SIZE/2.
//
// Since we need to do all of this for _every single memory operation_ it needs
// to be very efficient. We can't just sprinkle if...else checks around.
//
// So what we do is this trick by comparing the tag of the cache entry;
// we simply AND the virtual address with the TAG_MASK for the operation's alignment
// requirement and then compare the result with the cache entry's tag:
// if they are equal we know that it is both a valid cache entry for this page
// _and_ that the address's alignment is correct.
// Since we don't mask out the bottom (alignment-1) bits, if the address
// has invalid alignment, the bottom bits will be set in the resulting tag and the
// check will fail (and we take a slow path to figure out what's going on.)
//
// The first time we access an address we get a cache miss which leads us to
// call _vm_cache_miss to perform full translation in the page directory,
// check for alignment violation, and finally
// creating a cache entry for the VFN (0xD) with the resolved host page address
// and set the cache entry's tag to the virtual page address:
//
//      │    vaddr = 0xD022
//      │          = 0000000000000000000000000000000000000000000000001101000000100010
//      │      tag = PAGE_ADDRESS(addr) = 0xD000
//      │          = 0000000000000000000000000000000000000000000000001101000000000000
//      │    cache[VM_VFN(addr) & VM_CACHE_INDEX_VFN_MASK].tag = tag
// We can then finish by performing the memory operation on the host address:
//      │    haddr = hpaddr + VM_ADDR_OFFSET(vaddr)
//
// Now we LOAD from address 0xD022 again, with 2B alignment:
//
//   1. build the expected tag by masking it with TAG_MASK(alignment=2):
//      │    vaddr = 0000000000000000000000000000000000000000000000001101000000100010
//      │  mask(2) = 1111111111111111111111111111111111111111111111111111000000000001
//      │      tag = addr & mask = 0xD000
//      │          = 0000000000000000000000000000000000000000000000001101000000000000
//   2. compare the expected tag to that of the cache entry tag:
//      │      vfn = VM_VFN(addr)
//      │          = 0000000000000000000000000000000000000000000000000000000000001101
//      │   hpaddr = entry.haddr
//      │   if (tag != cache[vfn & VM_CACHE_INDEX_VFN_MASK].tag) call _vm_cache_miss
//   3. perform the memory operation on the host address:
//      │    haddr = hpaddr + VM_ADDR_OFFSET(vaddr)
//   Note that if the tag check fail, we call _vm_cache_miss
//
// Detecting misaligned addresses
// Now we LOAD from address 0xD022 again, but this time with 4B alignment.
// Note that the address is not 4B aligned so this should "fail":
//   1. Build the expected tag by masking it with TAG_MASK(alignment=2):
//      │    vaddr = 0xD022
//      │          = 0000000000000000000000000000000000000000000000001101000000100010
//      │  mask(4) = 1111111111111111111111111111111111111111111111111111000000000011
//      │                                                4B alignment test bits →  ━━
//      │      tag = addr & mask = 0xD002
//      │          = 0000000000000000000000000000000000000000000000001101000000000010
//      │                                                          Note set bit →  ━
//   2. Compare the expected tag to that of the cache entry tag.
//      The tag will not match cache[vfn].tag and we will call _vm_cache_miss
//      which in turn will:
//      - check for alignment violation (raise an alignment violation error.)
//      - resolve host page address (hpaddr) in the page directory.
//      - update the cache entry (set haddr & tag as described earlier.)
//
// So, we can implement the cache lookup in only a few (~10) instructions,
// with a conditional branch and a call to _vm_cache_miss.
//
#define VM_CACHE_TAG_MASK(align)  ( VM_ADDR_PAGE_MASK ^ ((u64)(align) - 1llu) )

// u64 VM_VFN(u64 vaddr) calculates the page frame number of a virtual address,
// aka Virtual Frame Number.
// The VFN is the top (64-PAGE_SIZE_BITS) bits of the address minus VM_ADDR_MIN.
// The result is undefined if vaddr < VM_ADDR_MIN.
// e.g. 0xdeadbeef 11011110101011011011111011101111 / PAGE_SIZE(4096)
//    = 0xdeadb    11011110101011011011
#define VM_VFN(vaddr)  ( ((u64)(vaddr) >> PAGE_SIZE_BITS) - (VM_ADDR_MIN/PAGE_SIZE) )

// VM_VFN_MAX is the largest valid VFN
#define VM_VFN_MAX  VM_VFN(VM_ADDR_MAX)

// u64 VM_VFN_VADDR(u64 vfn) calculates the virtual page address for vfn
#define VM_VFN_VADDR(vfn)  ( ((u64)(vfn) + (VM_ADDR_MIN/PAGE_SIZE)) << PAGE_SIZE_BITS )

// u64 VM_PAGE_ADDR(u64 vaddr) calculates the page address of a virtual address
// e.g. 0xdeadbeef 11011110101011011011111011101111 & (PAGE_SIZE(4096) - 1)
//    = 0xdeadb000 11011110101011011011000000000000
#define VM_PAGE_ADDR(vaddr) ((u64)(vaddr) & VM_ADDR_PAGE_MASK)

// VM_ADDR_OFFSET(u64 vaddr) calculates the page offset of an address.
// It's the upper PAGE_SIZE_BITS bits of a virtual address.
// e.g. 0xdeadbeef 11011110101011011011111011101111 & (PAGE_SIZE(4096) - 1)
//    = 0xeef                          111011101111
#define VM_ADDR_OFFSET(vaddr)  ( (vaddr) & VM_ADDR_OFFS_MASK )

// u64 VM_CACHE_INDEX(u64) returns the index in vm_cache_t.entries for vaddr.
// vaddr must be >= VM_ADDR_MIN or the result is undefined.
#define VM_CACHE_INDEX(vaddr)  ( VM_VFN(vaddr) & VM_CACHE_INDEX_VFN_MASK )

// vm_cache_ent_t* VM_CACHE_ENTRY(vm_cache_t cache, u64 vaddr)
// accesses the cache entry for a virtual address's page.
// vaddr can be either a page address or a canonical address (offset is ignored.)
// #define VM_CACHE_INDEX_MASK
#define VM_CACHE_ENTRY(cache, vaddr)  ( &(cache)->entries[VM_CACHE_INDEX(vaddr)] )

// vm_perm_t VM_PTE_PERM(vm_pte_t) returns vm_perm_t of a PTE
#define VM_PTE_PERM(pte)  ( (*(vm_perm_t*)(pte)) & ((vm_perm_t)VM_PERM_ALL) )

// bool VM_PERM_CHECK(vm_perm_t, vm_perm_t) returns true if hasperm contains all
// permissions of wantperm.
#define VM_PERM_CHECK(hasperm, wantperm) \
  ( ( ((hasperm) & (wantperm)) ^ (wantperm) ) == 0 )


// vm_perm_t: page permissions
typedef u8 vm_perm_t;
enum vm_perm {
  // note: values should match permission bit positions of vm_pte_t
  VM_PERM_NONE = 0,
  VM_PERM_R    = 1 << 0, // can read from this page
  VM_PERM_W    = 1 << 1, // can write to this page
  VM_PERM_X    = 1 << 2, // can execute RSM code in this page

  // convenience values
  VM_PERM_RW  = VM_PERM_R | VM_PERM_W,
  VM_PERM_RX  = VM_PERM_R | VM_PERM_X,
  VM_PERM_RWX = VM_PERM_R | VM_PERM_W | VM_PERM_X,

  VM_PERM_ALL = VM_PERM_RWX,
};

// vm_pte_t - page table entry
typedef struct {
#if RSM_LITTLE_ENDIAN
  // note: permission bit positions should match vm_perm_t
  bool  read        : 1; // can read from this page
  bool  write       : 1; // can write to this page
  bool  execute     : 1; // can execute code in this page
  bool  uncacheable : 1; // can not be cached
  bool  accessed    : 1; // has been accessed
  bool  written     : 1; // has been written to
  u64   type        : 3; // type of page
  u64   _reserved   : 3;

  // usize nuse      : VM_PTAB_BITS; // number of sub-entries in use
  // usize _reserved : 3;

  // usize _reserved : 12;

  // outaddr is the host frame number (hfn=haddr>>PAGE_SIZE_BITS).
  // For branches (page tables) this is the address of the next table or PTE.
  // For leafs (PTEs) this is the mapped host page address.
  u64 outaddr : 52;
#else
  #error TODO
#endif
} vm_pte_t;

// vm_ptab_t - virtual memory page table
typedef vm_pte_t* vm_ptab_t;

// vm_pagedir_t is a page directory, managing mapping between virtual and host pages
typedef struct {
  rmm_t*    mm;
  mutex_t   lock;
  vm_ptab_t root; // L0
} vm_pagedir_t;

// vm_cache_ent_t is the type of vm_cache_t entries
typedef struct {
  u64 haddr_diff; // diff between host page address & virtual page address
  u64 tag;        // virtual address tag
} vm_cache_ent_t;

// vm_cache_t is a translation cache (aka TLB; Translation Lookaside Buffer.)
// Maps virtual page addresses to host page addresses. Not thread safe.
typedef struct {
  vm_cache_ent_t entries[VM_CACHE_LEN];
} vm_cache_t;

// vm_op_t communicates a memory operation with optional alignment value
typedef u32 vm_op_t;
enum vm_op {
  VM_OP_STORE     = 0,
  VM_OP_STORE_1   = VM_OP_STORE + 1,
  VM_OP_STORE_2   = VM_OP_STORE + 2,
  VM_OP_STORE_4   = VM_OP_STORE + 4,
  VM_OP_STORE_8   = VM_OP_STORE + 8,
  VM_OP_STORE_16  = VM_OP_STORE + 16,
  VM_OP_STORE_32  = VM_OP_STORE + 32,
  VM_OP_STORE_64  = VM_OP_STORE + 64,
  VM_OP_STORE_128 = VM_OP_STORE + 128,

  VM_OP_LOAD     = 0x100,
  VM_OP_LOAD_1   = VM_OP_LOAD + 1,
  VM_OP_LOAD_2   = VM_OP_LOAD + 2,
  VM_OP_LOAD_4   = VM_OP_LOAD + 4,
  VM_OP_LOAD_8   = VM_OP_LOAD + 8,
  VM_OP_LOAD_16  = VM_OP_LOAD + 16,
  VM_OP_LOAD_32  = VM_OP_LOAD + 32,
  VM_OP_LOAD_64  = VM_OP_LOAD + 64,
  VM_OP_LOAD_128 = VM_OP_LOAD + 128,
};
#define VM_OP_ALIGNMENT(op) ( (op) & 0xff ) /* e.g. 2, 4, 8 ... */
#define VM_OP_TYPE(op)      ( (op) & (~0u << 8) ) /* e.g. VM_OP_LOAD, VM_OP_STORE */


// vm_pagedir_init initializes a new vm_pagedir_t, sourcing backing memory from mm.
// Returns false if allocating the root table in mm failed.
rerr_t vm_pagedir_init(vm_pagedir_t*, rmm_t* mm);

// vm_pagedir_dispose frees up resources of a page directory,
// which becomes invalid after this call.
void vm_pagedir_dispose(vm_pagedir_t*);

// vm_map maps a range of host pages, starting at haddr, to a range of virtual pages.
// If haddr is 0, backing pages are allocated as needed on first access.
// haddr's address must be aligned to PAGE_SIZE.
// vaddr does not need to be page aligned; its top PAGE_SIZE_BITS bits are ignored.
rerr_t vm_map(vm_pagedir_t*, uintptr haddr, u64 vaddr, usize npages, vm_perm_t);

// vm_unmap deallocates a range of virtual pages
// Caller using a cache should call vm_cache_invalidate
rerr_t vm_unmap(vm_pagedir_t*, u64 vaddr, usize npages);


// vm_cache_init initializes a vm_cache_t
void vm_cache_init(vm_cache_t*);

// vm_cache_invalidate invalidates npages starting at vaddr.
// To invalidate the entire cache, pass vaddr=VM_ADDR_MIN, npages=VM_CACHE_LEN.
void vm_cache_invalidate(vm_cache_t*, u64 vaddr, usize npages);

// vm_cache_invalidate_one invalidates the entry for the provided address's page.
// Note that this does not verify if the VM_CACHE_ENTRY is for the correct page.
// vaddr can be a canonical address or a page address.
ALWAYS_INLINE void vm_cache_invalidate_one(vm_cache_t* cache, u64 vaddr) {
  // VM_CACHE_DEL_TAG is a value larger than max possible lookup tag value
  VM_CACHE_ENTRY(cache, vaddr)->tag = VM_CACHE_DEL_TAG;
}

// _vm_cache_miss is called by vm_translate to resolve vaddr using a page directory.
// The address is checked for alignment according to vm_op_t.
// Finally, the entry is added to to the cache.
// Returns vm_cache_ent_t.haddr_diff
u64 _vm_cache_miss(vm_cache_t*, vm_pagedir_t*, u64 vaddr, vm_op_t);

// void vmtrace(const char* fmt, ...)
#ifndef vmtrace
  #define vmtrace(...) ((void)0)
#endif

// VM_STORE performs a store to host memory using a virtual address.
// E.g. VM_STORE(u32, vm_cache, vm_pagedir, 0xdeadbee0, 1234)
// This becomes 10 x86_64 instructions, or 10 arm64/armv8-1 instructions (clang-14 -O3).
// See https://godbolt.org/z/esT98Y9Yd
#define VM_STORE(type, vm_cache, pagedir, vaddr, value) { \
  vmtrace("VM_STORE %s (align %lu) 0x%llx", #type, _Alignof(type), vaddr); \
  *(type*)vm_translate( \
    (vm_cache), (pagedir), (vaddr), _Alignof(type), VM_OP_STORE + _Alignof(type) \
  ) = (value); \
}

// VM_LOAD performs a load from host memory using a virtual address.
// E.g. u32 value = VM_LOAD(u32, vm_cache, vm_pagedir, 0xdeadbee0);
#define VM_LOAD(type, vm_cache, pagedir, vaddr) ( \
  vmtrace("VM_LOAD %s (align %lu) 0x%llx", #type, _Alignof(type), vaddr), \
  *(type*)vm_translate( \
    (vm_cache), (pagedir), (vaddr), _Alignof(type), VM_OP_LOAD + _Alignof(type) \
  ) \
)

// uintptr VM_TRANSLATE() translates a virtual address to a host address
#define VM_TRANSLATE(vm_cache, pagedir, vaddr, align) \
  vm_translate((vm_cache), (pagedir), (vaddr), (align), VM_OP_LOAD + (u32)(align))

// vm_translate converts a virtual address to its corresponding host address
ALWAYS_INLINE static uintptr vm_translate(
  vm_cache_t* cache, vm_pagedir_t* pagedir, u64 vaddr, u64 align, vm_op_t op)
{
  assert(vaddr >= VM_ADDR_MIN);
  u64 index = VM_CACHE_INDEX(vaddr);
  u64 actual_tag = cache->entries[index].tag;
  u64 expected_tag = vaddr & (VM_ADDR_PAGE_MASK ^ (align - 1llu));
  u64 haddr_diff = UNLIKELY( actual_tag != expected_tag ) ?
    _vm_cache_miss(cache, pagedir, vaddr, op) :
    cache->entries[index].haddr_diff;
  return (uintptr)(haddr_diff + vaddr);
}


RSM_ASSUME_NONNULL_END
