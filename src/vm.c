// virtual machine
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

typedef struct vmstate vmstate;
struct vmstate {
  usize inlen;   // number of instructions at inv
  usize memsize;
  union { usize datasize, stacktop; };  // aka
  union { usize heapbase, stackbase; }; // aka
};

// vm interperter functions signature
#define VMPARAMS vmstate* vs, u64* iregs, rinstr* inv, void* membase, usize pc
#define VMARGS   vs, iregs, inv, membase, pc

#if defined(DEBUG) && !defined(RSM_NO_LIBC)
  #include <stdio.h>
  static void logstate_header() {
    fprintf(stderr, "\e[2m");
    for (int i = 0; i < 6; i++)
      fprintf(stderr, "  " REG_FMTNAME_PAT, REG_FMTNAME(i));
    fprintf(stderr, "  │  PC  INSTRUCTION\e[22m\n");
  }
  static void logstate(VMPARAMS) {
    for (int i = 0; i < 6; i++)
      fprintf(stderr, REG_FMTVAL_PAT("%4llx"), REG_FMTVAL(i, iregs[i]));
    char buf[128];
    rsm_fmtinstr(buf, sizeof(buf), inv[pc]);
    fprintf(stderr, "  │ %3ld  %s\n", pc, buf);
  }
#else
  #define logstate_header(...) ((void)0)
  #define logstate(...) ((void)0)
#endif

// constants
#define STK_ALIGN    8          // stack alignment
#define MAIN_RET_PC  USIZE_MAX  // special PC value representing the main return address

// accessor macros
#define SP  iregs[31]
#define PC  pc

#define RA  iregs[ar]
#define RB  iregs[RSM_GET_B(in)]
#define RC  iregs[RSM_GET_C(in)]
#define RD  iregs[RSM_GET_D(in)]

#define RAu ( RSM_GET_Ai(in) ? (u64)RSM_GET_Au(in) : iregs[RSM_GET_Au(in)] )
#define RBu ( RSM_GET_Bi(in) ? (u64)RSM_GET_Bu(in) : iregs[RSM_GET_Bu(in)] )
#define RCu ( RSM_GET_Ci(in) ? (u64)RSM_GET_Cu(in) : iregs[RSM_GET_Cu(in)] )
#define RDu ( RSM_GET_Di(in) ? (u64)RSM_GET_Du(in) : iregs[RSM_GET_Du(in)] )

#define RAs ((i64)( RSM_GET_Ai(in) ? RSM_GET_As(in) : iregs[RSM_GET_Au(in)] ))
#define RBs ((i64)( RSM_GET_Bi(in) ? RSM_GET_Bs(in) : iregs[RSM_GET_Bu(in)] ))
#define RCs ((i64)( RSM_GET_Ci(in) ? RSM_GET_Cs(in) : iregs[RSM_GET_Cu(in)] ))
#define RDs ((i64)( RSM_GET_Di(in) ? RSM_GET_Ds(in) : iregs[RSM_GET_Du(in)] ))

// runtime error checking & reporting
#if RSM_SAFE
typedef u32 vmerror;
enum vmerror {
  VM_E_UNALIGNED_STORE = 1,
  VM_E_UNALIGNED_LOAD,
  VM_E_STACK_OVERFLOW,
  VM_E_OOB_LOAD,
  VM_E_OOB_STORE,
} RSM_END_ENUM(vmerror)

static void vmerr(VMPARAMS, vmerror err, u64 arg1, u64 arg2) {
  char buf[2048];
  abuf s1 = abuf_make(buf, sizeof(buf)); abuf* s = &s1;
  pc--; // undo the increment to make pc point to the violating instruction
  #define S(ERR, fmt, args...) case ERR: abuf_fmt(s, fmt, ##args); break;
  switch ((enum vmerror)err) {
    S(VM_E_UNALIGNED_STORE, "unaligned memory store %llx (align %llu B)", arg1, arg2)
    S(VM_E_UNALIGNED_LOAD,  "unaligned memory load %llx (align %llu B)", arg1, arg2)
    S(VM_E_STACK_OVERFLOW,  "stack overflow %llx (align %llu B)", arg1, arg2)
    S(VM_E_OOB_LOAD,        "memory load out of bounds %llx (align %llu B)", arg1, arg2)
    S(VM_E_OOB_STORE,       "memory store out of bounds %llx (align %llu B)", arg1, arg2)
  }
  #undef S
  abuf_c(s, '\n');

  abuf_fmt(s, "  %08lx  ", PC);
  fmtinstr(s, inv[pc]);
  abuf_c(s, '\n');

  abuf_str(s, "Register state:");

  for (u32 i = 0, endi = 31; i < endi; i++) {
    if (i % 8 == 0) {
      usize len = s->len;
      abuf_fmt(s, "\n  R%u…%u", i, MIN(i+7, endi-1));
      abuf_fill(s, ' ', 10 - (s->len - len - 1));
    }
    abuf_fmt(s, " %8llx", iregs[i]);
  }
  usize stacktop = ALIGN2(vs->datasize, STK_ALIGN);
  usize stacksize = vs->stackbase - stacktop;
  abuf_fmt(s, "\n  SP     %8llx", SP);

  usize heapsize = vs->memsize - vs->heapbase;
  abuf_fmt(s, "\nMemory: (%lu B)", vs->memsize);
  abuf_fmt(s, "\n  data         0...%-8lx %10lu B", vs->datasize, vs->datasize);
  abuf_fmt(s, "\n  stack %8lx...%-8lx %10lu B", stacktop, vs->heapbase, stacksize);
  abuf_fmt(s, "\n  heap  %8lx...%-8lx %10lu B", vs->heapbase, vs->memsize, heapsize);

  abuf_terminate(s);
  log("%s", buf);
  abort();
}

#define __vmerr_NARGS_X(a,b,c,d,...) d
#define __vmerr_NARGS(...) __vmerr_NARGS_X(__VA_ARGS__,3,2,1,0,)
#define __vmerr_CONCAT_X(a,b) a##b
#define __vmerr_CONCAT(a,b) __vmerr_CONCAT_X(a,b)
#define __vmerr_DISP(a,...) __vmerr_CONCAT(a,__vmerr_NARGS(__VA_ARGS__))(__VA_ARGS__)
#define __vmerr1(err)     err, 0, 0
#define __vmerr2(err,a)   err, a, 0
#define __vmerr3(err,a,b) err, a, b

#define check(cond, ...) if UNLIKELY(!(cond)) vmerr(VMARGS, __vmerr_DISP(__vmerr,__VA_ARGS__))
#else
#define check(cond, ...) ((void)0)
#endif // RSM_SAFE

static void scall(VMPARAMS, u8 ar, rinstr in) {
  // dlog("scall #%llu arg1 0x%llx", RA, RBs);
  iregs[0] = 0; // result = no error
  switch (RA) {
    case 1: putc(RBs, stdout); break;
    default: iregs[0] = 1; // error; invalid scall
  }
}

static u64 load(VMPARAMS, u64 addr, usize nbits) {
  check(IS_ALIGN2(addr, nbits), VM_E_UNALIGNED_LOAD, addr, nbits);
  check(addr+nbits <= vs->memsize, VM_E_OOB_LOAD, addr, nbits);
  return *(u64*)(membase + addr);
}

static void store(VMPARAMS, u64 addr, usize nbits, u64 val) {
  check(IS_ALIGN2(addr, nbits), VM_E_UNALIGNED_STORE, addr, nbits);
  check(addr+nbits <= vs->memsize, VM_E_OOB_STORE, addr, nbits);
  *(u64*)(membase + addr) = val;
}

static void push(VMPARAMS, usize nbits, u64 val) {
  usize addr = SP;
  check(addr >= nbits && addr - nbits >= vs->stacktop, VM_E_STACK_OVERFLOW, addr);
  addr -= nbits;
  SP = addr;
  store(VMARGS, addr, nbits, val);
}

static u64 pop(VMPARAMS, usize nbits) {
  usize addr = SP;
  check(USIZE_MAX - addr >= nbits && addr + nbits <= vs->stackbase, VM_E_STACK_OVERFLOW, addr);
  SP = addr + nbits;
  return load(VMARGS, addr, nbits);
}

static usize call(VMPARAMS) {
  push(VMARGS, 8, (u64)PC); // save return address on stack
  rinstr in = inv[pc - 1];  // for RAu
  return (usize)RAu;        // replacement PC
}

static void vmexec(VMPARAMS) {
  #if 1 // use label jump table
    static const void* jumptab[RSM_OP_COUNT] = {
      #define _(name, ...) &&_op_##name,
      RSM_FOREACH_OP(_)
      #undef _
    };
    #define DISPATCH goto *jumptab[RSM_GET_OP(in)];
    #define NEXT     continue;
    #define E(OP)    _op_##OP:
  #else // use switch
    #define DISPATCH switch (RSM_GET_OP(in))
    #define NEXT     break;
    #define E(OP)    case rop_##OP:
  #endif

  for (;;) {
    assertf(pc < vs->inlen, "pc overrun %lu", pc);
    logstate(VMARGS);

    rinstr in = inv[pc++];
    u8 ar = RSM_GET_A(in);

    DISPATCH {

    E(COPY)  RA = RBu; NEXT
    E(LOAD)  RA = load(VMARGS, (u64)((i64)RB + RCs), 8); NEXT
    E(STORE) store(VMARGS, (u64)((i64)RB + RCs), 8, RA); NEXT

    E(ADD)   RA = RB + RCu; NEXT
    E(SUB)   RA = RB - RCu; NEXT
    E(MUL)   RA = RB * RCu; NEXT
    E(DIV)   RA = RB / RCu; NEXT
    E(MOD)   RA = RB % RCu; NEXT
    E(AND)   RA = RB & RCu; NEXT
    E(OR)    RA = RB | RCu; NEXT
    E(XOR)   RA = RB ^ RCu; NEXT
    E(SHL)   RA = RB << RCu; NEXT
    E(SHRS)  RA = (u64)((i64)RB >> RCs); NEXT
    E(SHRU)  RA = RB >> RCu; NEXT

    E(CMPEQ) RA = RB == RCu; NEXT
    E(CMPLT) RA = RB < RCu; NEXT
    E(CMPGT) RA = RB > RCu; NEXT

    E(BRZ)  if (RA == 0) pc = ((isize)pc + (isize)RBs); NEXT
    E(BRNZ) if (RA != 0) pc = ((isize)pc + (isize)RBs); NEXT

    E(SCALL) scall(VMARGS, ar, in); NEXT
    E(CALL)  pc = call(VMARGS); NEXT
    E(TCALL) pc = (usize)RAu; NEXT
    E(RET) {
      pc = (usize)pop(VMARGS, 8); // load return address from stack
      // TODO: instead of MAIN_RET_PC, append coro(end) or yield(end) or exit instr
      // to end of inv and setup main return to that address.
      if (pc == MAIN_RET_PC) return;
      NEXT
    }
  } // DISPATCH
  } // loop
}

void rsm_vmexec(u64* iregs, rinstr* inv, usize inlen, void* membase, usize memsize) {
  // memory layout:
  //    ┌─────────────┬─────────────┬───────────···
  //    │ data        │     ← stack │ heap →
  //    ├─────────────┼─────────────┼───────────···
  // membase      datasize      heapbase
  //              stacktop      stackbase

  // make sure memory is aligned to most stringent alignment of data (TODO: read ROM)
  uintptr ma = ALIGN2((uintptr)membase, 8);
  if UNLIKELY(ma != (uintptr)membase) {
    uintptr diff = ma - (uintptr)membase;
    memsize = diff > memsize ? 0 : memsize - diff;
    membase = (void*)ma;
    dlog("adjusting membase+%lu memsize-%lu (address alignment)", (usize)diff, (usize)diff);
  }
  usize datasize = 0; // TODO: read from ROM
  usize stacktop  = ALIGN2(datasize, STK_ALIGN);
  usize stackbase = stacktop + ALIGN2_FLOOR(MIN(memsize/2, 4096), STK_ALIGN);
  dlog("memsize %zu B, stack{top,base} %zu...%zu", memsize, stacktop, stackbase);

  vmstate vs = {
    .inlen    = inlen,
    .memsize  = memsize,
    .datasize = datasize,
    .heapbase = stackbase,
  };

  // initialize stack pointer and push main return address on stack
  SP = (u64)stackbase;
  push(&vs, iregs, inv, membase, 0, 8, MAIN_RET_PC);

  logstate_header();
  return vmexec(&vs, iregs, inv, membase, 0);
}
