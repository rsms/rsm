// calling VM_STORE(u32, cache, pagedir, vaddr, value)
// See https://godbolt.org/z/x11EKveod
// 
// inline static uintptr vm_translate(
//   vm_cache_t*   cache   [rdi],
//   vm_pagedir_t* pagedir [rsi],
//   u64           vaddr   [rdx],
//   u64           align,
//   vm_op_t       op)
// {
  mov     rbx, rdx // copy vaddr to long-lived (caller-owned) register rbx
//   u64 index = ((u64)(vaddr) >> PAGE_SIZE_BITS) & VM_CACHE_INDEX_VFN_MASK;
  mov     eax, ebx  // copy most significant 32 bits of vaddr to eax (index)
  shr     eax, 12   // index = index >> PAGE_SIZE_BITS
  movzx   eax, al   // copy bottom 8 bits of index and zero extend the value
//
//   u64 tag = vaddr & (VM_ADDR_PAGE_MASK ^ (align - 1llu));
  mov     rcx, rdx // copy vaddr to rcx
  and     rcx, -4093
//
//   return ( // get host page address
//     UNLIKELY( cache->entries[index].tag != tag ) ?
  shl     rax, 4
  cmp     qword ptr [rdi + rax + 8], rcx
  jne     .cache_miss
//
//       _vm_cache_miss(cache, pagedir, vaddr, op) :
//
//       cache->entries[index].haddr
  mov     rax, qword ptr [rdi + rax]  // load cache entry's haddr value to rax
.b1:
//
//   ) + VM_ADDR_OFFSET(vaddr);
  and     ebx, 4095  // offset = vaddr & VM_ADDR_OFFS_MASK
// }

// VM_STORE
//   *(u32*)haddr = v;
  mov     dword ptr [rax + rbx], ebp  // store 4B from stack (ebp) to haddr+offset

//       _vm_cache_miss(cache, pagedir, vaddr, op) :
.cache_miss:
  mov     rdx, rbx  // copy address offset to rdx
  mov     ecx, 4
  call    _vm_cache_miss
  jmp     .b1

