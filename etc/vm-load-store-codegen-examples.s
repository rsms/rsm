// __attribute__((noinline))
// void example_store(vm_cache_t* cache, vm_pagedir_t* pagedir, u64 vaddr, u32 v) {
//   VM_STORE(u32, cache, pagedir, vaddr, v);
// }

// —————————————————————————————————————————————————————————————————————————————————————
// armv8-a/arm64 clang-14 -O3
example_store:
  // function preamble
  stp     x29, x30, [sp, #-32]!           // 16-byte Folded Spill
  stp     x20, x19, [sp, #16]             // 16-byte Folded Spill
  mov     x29, sp
  mov     x20, x2
  mov     w19, w3

  // VM_STORE → vm_translate
  //   UNLIKELY(entry->tag != VM_CACHE_VFN_TAG(VM_VFN(vaddr)))
  add     x8, x0, w20, uxtb #4
  ldr     x8, [x8, #8]
  cmp     x8, x2, lsr #20
  b.ne    .cache_miss

  // VM_STORE → vm_translate
  //   vm_cache_ent_t* entry = VM_CACHE_ENTRY(trc, vaddr);
  and     x8, x20, #0xff
  lsl     x8, x8, #4

  // VM_STORE → vm_translate
  //   entry->haddr
  ldr     x0, [x0, x8]

  // VM_STORE → vm_translate
  //   + (vaddr & (u64)(PAGE_SIZE - 1)); // add offset
  and     x8, x20, #0xfff

  // VM_STORE
  //   *(type*)haddr__
  str     w19, [x0, x8]

  // function postamble
  ldp     x20, x19, [sp, #16]             // 16-byte Folded Reload
  ldp     x29, x30, [sp], #32             // 16-byte Folded Reload
  ret

.cache_miss:
  // VM_STORE → vm_translate
  //   vm_cache_populate(trc, pagedir, vaddr)
  mov     x2, x20
  bl      vm_cache_populate

  // VM_STORE → vm_translate
  //   + (vaddr & (u64)(PAGE_SIZE - 1)); // add offset
  and     x8, x20, #0xfff

  // VM_STORE
  //   *(type*)haddr__
  str     w19, [x0, x8]

  // function postamble
  ldp     x20, x19, [sp, #16]             // 16-byte Folded Reload
  ldp     x29, x30, [sp], #32             // 16-byte Folded Reload
  ret

// —————————————————————————————————————————————————————————————————————————————————————
// x86_64 clang-14 -O3
example_store:
  // function preamble
  push    rbp
  push    rbx
  push    rax
  mov     ebp, ecx
  mov     rbx, rdx

  // VM_STORE → vm_translate
  //   vm_cache_ent_t* entry = VM_CACHE_ENTRY(trc, vaddr);
  movzx   eax, bl

  // VM_STORE → vm_translate
  //   UNLIKELY(entry->tag != VM_CACHE_VFN_TAG(VM_VFN(vaddr)))
  shl     rax, 4
  mov     rcx, rdx
  shr     rcx, 20
  cmp     qword ptr [rdi + rax + 8], rcx
  jne     .cache_miss

  // VM_STORE → vm_translate
  //   entry->haddr
  mov     rax, qword ptr [rdi + rax]

.finalize:
  // VM_STORE → vm_translate
  //   + (vaddr & (u64)(PAGE_SIZE - 1)); // add offset
  and     ebx, 4095

  // VM_STORE
  //   *(type*)haddr__
  mov     dword ptr [rax + rbx], ebp

  // function postamble
  add     rsp, 8
  pop     rbx
  pop     rbp
  ret

.cache_miss:
  mov     rdx, rbx
  call    vm_cache_populate
  jmp     .finalize
