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

#define RA  iregs[ar] // u64
#define RB  iregs[br] // u64
#define RC  iregs[RSM_GET_C(in)] // u64
#define RD  iregs[RSM_GET_D(in)] // u64

#define RAu RSM_GET_Au(in) // u32
#define RBu RSM_GET_Bu(in) // u32
#define RCu RSM_GET_Cu(in) // u32
#define RDu RSM_GET_Du(in) // u32

#define RAs RSM_GET_As(in) // i32
#define RBs RSM_GET_Bs(in) // i32
#define RCs RSM_GET_Cs(in) // i32
#define RDs RSM_GET_Ds(in) // i32

#define RAru ( RSM_GET_i(in) ? (u64)RSM_GET_Au(in) : iregs[RSM_GET_Au(in)] )
#define RBru ( RSM_GET_i(in) ? (u64)RSM_GET_Bu(in) : iregs[RSM_GET_Bu(in)] )
#define RCru ( RSM_GET_i(in) ? (u64)RSM_GET_Cu(in) : iregs[RSM_GET_Cu(in)] )
#define RDru ( RSM_GET_i(in) ? (u64)RSM_GET_Du(in) : iregs[RSM_GET_Du(in)] )

#define RArs ((i64)( RSM_GET_i(in) ? RSM_GET_As(in) : iregs[RSM_GET_Au(in)] ))
#define RBrs ((i64)( RSM_GET_i(in) ? RSM_GET_Bs(in) : iregs[RSM_GET_Bu(in)] ))
#define RCrs ((i64)( RSM_GET_i(in) ? RSM_GET_Cs(in) : iregs[RSM_GET_Cu(in)] ))
#define RDrs ((i64)( RSM_GET_i(in) ? RSM_GET_Ds(in) : iregs[RSM_GET_Du(in)] ))

// runtime error checking & reporting
#if RSM_SAFE
typedef u32 vmerror;
enum vmerror {
  VM_E_UNALIGNED_STORE = 1,
  VM_E_UNALIGNED_LOAD,
  VM_E_STACK_OVERFLOW,
  VM_E_OOB_LOAD,
  VM_E_OOB_STORE,
  VM_E_OPNOI,
  VM_E_SHIFT_EXP,
} RSM_END_ENUM(vmerror)

static void _vmerr(VMPARAMS, vmerror err, u64 arg1, u64 arg2) {
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
    S(VM_E_OPNOI,           "op %s does not accept immediate value", rop_name(arg1))
    S(VM_E_SHIFT_EXP,       "shift exponent %llu is too large", arg1)
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
#define vmerr(...) _vmerr(VMARGS, __vmerr_DISP(__vmerr,__VA_ARGS__))
#define check(cond, ...) if UNLIKELY(!(cond)) vmerr(__VA_ARGS__)
#else
  #define check(cond, ...) ((void)0)
#endif // RSM_SAFE

#define check_loadstore(addr, size, ealign, eoob) ({ \
  check(IS_ALIGN2(addr, size), ealign, addr, size); \
  check(addr + size <= vs->memsize, eoob, addr, size); \
})

#define check_shift(exponent) check((exponent) < 64, VM_E_SHIFT_EXP, exponent)

static void scall(VMPARAMS, u8 ar, rinstr in) {
  // dlog("scall #%llu arg1 0x%llx", RA, RBrs);
  // iregs[0] = 0; // result = no error
  // switch (RA) {
  //   case 1: putc(RBrs, stdout); break;
  //   default: iregs[0] = 1; // error; invalid scall
  // }
}

// u64 LOAD(TYPE, usize addr)
#define LOAD(TYPE, addr) ({ \
  usize a__ = (usize)(addr); TYPE t__; \
  check_loadstore(a__, sizeof(TYPE), VM_E_UNALIGNED_LOAD, VM_E_OOB_LOAD); \
  void* p = membase + a__; \
  u64 val = (u64)*_Generic(t__, u64:(u64*)p, u32:(u32*)p, u16:(u16*)p, u8:(u8*)p ); \
  /*dlog("load  %s mem[0x%lx] => 0x%llx", #TYPE, a__, val);*/ \
  val; \
})

// void STORE(TYPE, usize addr, u64 value)
#define STORE(TYPE, addr, value) ({ \
  usize a__ = (usize)(addr); TYPE v__ = (TYPE)(value); \
  check_loadstore(a__, sizeof(TYPE), VM_E_UNALIGNED_STORE, VM_E_OOB_STORE); \
  void* p = membase + a__; \
  /*dlog("store %s mem[0x%lx] <= 0x%llx", #TYPE, a__, (u64)v__);*/ \
  *_Generic(v__, u64:(u64*)p, u32:(u32*)p, u16:(u16*)p, u8:(u8*)p ) = v__; \
})

static void push(VMPARAMS, usize size, u64 val) {
  usize addr = SP;
  check(addr >= size && addr - size >= vs->stacktop, VM_E_STACK_OVERFLOW, addr);
  addr -= size;
  SP = addr;
  STORE(u64, addr, val);
}

static u64 pop(VMPARAMS, usize size) {
  usize addr = SP;
  check(USIZE_MAX-addr >= size && addr+size <= vs->stackbase, VM_E_STACK_OVERFLOW, addr);
  SP = addr + size;
  return LOAD(u64, addr);
}

isize write(int fd, const void* buf, usize nbyte); // libc

static void vmexec(VMPARAMS) {
  // This is the interpreter loop.
  // It executes instructions until the entry function returns or an error occurs.
  //
  // First, we define how we will map an instruction to its corresponding handler code.
  // There are two options: using a label jump table or using a switch statement.
  #if 1 // use label jump table
    // Instructions are indexed together with the i (immediate) flag bit.
    // Indexed interleaved on r/i handlers like this:
    //   ...
    //   [28]=&&_op_FOO      op with register or imm (register handler)
    //   [29]=&&_op_FOO_i    op with register or imm (imm handler)
    //   [30]=&&_op_BAR      op with register
    //    --                 (hole, since BAR does not accept imm)
    //   [32]=&&_op_CAT      op with register or imm (register handler)
    //   [33]=&&_op_CAT_i    op with register or imm (imm handler)
    //   ...
    static const void* jumptab[(RSM_OP_COUNT << 1) | 1] = {
      #define L_r(OP)      [(rop_##OP << 1)]=&&_op_##OP,
      #define L_i(OP)      [(rop_##OP << 1)|1]=&&_op_##OP##_i,
      #define L__          L_r
      #define L_A          L_r
      #define L_AB         L_r
      #define L_ABC        L_r
      #define L_ABCD       L_r
      #define L_Au(OP)     L_r(OP) L_i(OP)
      #define L_ABu(OP)    L_r(OP) L_i(OP)
      #define L_ABCu(OP)   L_r(OP) L_i(OP)
      #define L_ABCDu(OP)  L_r(OP) L_i(OP)
      #define L_As(OP)     L_r(OP) L_i(OP)
      #define L_ABs(OP)    L_r(OP) L_i(OP)
      #define L_ABCs(OP)   L_r(OP) L_i(OP)
      #define L_ABCDs(OP)  L_r(OP) L_i(OP)
      #define _(OP, enc, ...) L_##enc(OP)
      RSM_FOREACH_OP(_)
      #undef _
    };
    #define DISPATCH \
      u32 ji = (RSM_GET_OP(in) << 1) | RSM_GET_i(in); \
      assertf(jumptab[ji], "\"%s\" i=%d", rop_name(RSM_GET_OP(in)), RSM_GET_i(in)); \
      goto *jumptab[ji];
    #define NEXT     continue;
    #define R(OP)    _op_##OP:
    #define I(OP)    _op_##OP##_i:
    #define DEFAULT  /* check done in DISPATCH */
  #else // use switch
    #define DISPATCH switch ((RSM_GET_OP(in) << 1) | RSM_GET_i(in))
    #define NEXT     break;
    #define R(OP)    case (rop_##OP << 1):
    #define I(OP)    case (rop_##OP << 1)|1:
    #ifdef DEBUG
      #define DEFAULT default: panic("\"%s\" i=%d", rop_name(RSM_GET_OP(in)), RSM_GET_i(in));
    #else
      #define DEFAULT
    #endif
  #endif

  // instruction feed loop
  for (;;) {
    // load the next instruction and advance program counter
    assertf(pc < vs->inlen, "pc overrun %lu", pc); logstate(VMARGS);
    rinstr in = inv[pc++];
    // preload arguments A and B as most instructions need it
    u8 ar = RSM_GET_A(in);
    u8 br = RSM_GET_B(in);

    // depending on RSM_GET_OP(in) & RSM_GET_i(in), jump to a handler below
    DISPATCH {

    // operation handlers -- each op should have a do_OP(lastarg) macro defined here.
    // Ops that use the i(mmediate) flag will have their "do" macro used twice (reg & imm)
    // These macros are used like this: "case op_and_i: do_OP(lastarg); break;"
    #define do_COPY(B) RA = B

    #define do_LOAD(C)   RA = LOAD(u64, (usize)((i64)RB+(i64)C))
    #define do_LOAD4U(C) RA = LOAD(u32, (usize)((i64)RB+(i64)C)) // zero-extend i32 to i64
    #define do_LOAD4S(C) RA = LOAD(u32, (usize)((i64)RB+(i64)C)) // sign-extend i32 to i64
    #define do_LOAD2U(C) RA = LOAD(u16, (usize)((i64)RB+(i64)C)) // zero-extend i16 to i64
    #define do_LOAD2S(C) RA = LOAD(u16, (usize)((i64)RB+(i64)C)) // sign-extend i16 to i64
    #define do_LOAD1U(C) RA = LOAD(u8,  (usize)((i64)RB+(i64)C)) // zero-extend i8 to i64
    #define do_LOAD1S(C) RA = LOAD(u8,  (usize)((i64)RB+(i64)C)) // sign-extend i8 to i64

    #define do_STORE(C)  STORE(u64, (usize)((i64)RB+(i64)C), RA)
    #define do_STORE4(C) STORE(u32, (usize)((i64)RB+(i64)C), RA) // wrap i64 to i32
    #define do_STORE2(C) STORE(u16, (usize)((i64)RB+(i64)C), RA) // wrap i64 to i16
    #define do_STORE1(C) STORE(u8,  (usize)((i64)RB+(i64)C), RA) // wrap i64 to i8

    #define do_PUSH(A)  push(VMARGS, 8, A)
    #define do_POP(A)   A = pop(VMARGS, 8)

    #define do_ADD(C)  RA = RB + C
    #define do_SUB(C)  RA = RB - C
    #define do_MUL(C)  RA = RB * C
    #define do_DIV(C)  RA = RB / C
    #define do_MOD(C)  RA = RB % C
    #define do_AND(C)  RA = RB & C
    #define do_OR(C)   RA = RB | C
    #define do_XOR(C)  RA = RB ^ C
    #define do_SHL(C)  check_shift(C); RA = RB << C
    #define do_SHRS(C) check_shift(C); RA = (u64)((i64)RB >> C)
    #define do_SHRU(C) check_shift(C); RA = RB >> C

    #define do_EQ(C)   RA = RB == C
    #define do_NEQ(C)  RA = RB != C
    #define do_LTU(C)  RA = RB < C
    #define do_LTS(C)  RA = (i64)RB < (i64)C
    #define do_LTEU(C) RA = RB <= C
    #define do_LTES(C) RA = (i64)RB <=(i64)C
    #define do_GTU(C)  RA = RB > C
    #define do_GTS(C)  RA = (i64)RB > (i64)C
    #define do_GTEU(C) RA = RB >= C
    #define do_GTES(C) RA = (i64)RB >= (i64)C

    #define do_BR(B)    if (RA)      pc = (isize)((i64)pc + (i64)B)
    #define do_BRZ(B)   if (RA == 0) pc = (isize)((i64)pc + (i64)B)
    #define do_BRLT(C)  if (RA < RB) pc = (isize)((i64)pc + (i64)C)

    #define do_JUMP(A)  pc = (usize)A
    #define do_SCALL(A) scall(VMARGS, A, in)
    #define do_CALL(A)  push(VMARGS, 8, (u64)PC); pc = (usize)A

    #define do_WRITE(D) RA = (u64)write(D, membase + RB, RC) // (addr=RB size=RC fd=D)

    #define do_RET() { \
      pc = (usize)pop(VMARGS, 8); /* load return address from stack */ \
      /* TODO: instead of MAIN_RET_PC, append coro(end) or yield(end) or exit instr */ \
      /* to end of inv and setup main return to that address. */ \
      if (pc == MAIN_RET_PC) return; \
    }

    // generators for handler labels (or case statements if a switch is used)
    #define CASE__(OP)     R(OP) do_##OP(); NEXT
    #define CASE_A(OP)     R(OP) do_##OP(RA); NEXT
    #define CASE_AB(OP)    R(OP) do_##OP(RB); NEXT
    #define CASE_ABC(OP)   R(OP) do_##OP(RC); NEXT
    #define CASE_ABCD(OP)  R(OP) do_##OP(RD); NEXT
    #define CASE_Au(OP)    R(OP) do_##OP(RA); NEXT   I(OP) do_##OP(RAu); NEXT
    #define CASE_ABu(OP)   R(OP) do_##OP(RB); NEXT   I(OP) do_##OP(RBu); NEXT
    #define CASE_ABCu(OP)  R(OP) do_##OP(RC); NEXT   I(OP) do_##OP(RCu); NEXT
    #define CASE_ABCDu(OP) R(OP) do_##OP(RD); NEXT   I(OP) do_##OP(RDu); NEXT
    #define CASE_As(OP)    R(OP) do_##OP(RA); NEXT   I(OP) do_##OP(RAs); NEXT
    #define CASE_ABs(OP)   R(OP) do_##OP(RB); NEXT   I(OP) do_##OP(RBs); NEXT
    #define CASE_ABCs(OP)  R(OP) do_##OP(RC); NEXT   I(OP) do_##OP(RCs); NEXT
    #define CASE_ABCDs(OP) R(OP) do_##OP(RD); NEXT   I(OP) do_##OP(RDs); NEXT
    #define _(OP, enc, ...) CASE_##enc(OP)
    RSM_FOREACH_OP(_)
    #undef _

    DEFAULT
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
  usize datasize  = 64; // TODO: read from ROM
  usize stacktop  = ALIGN2(datasize, STK_ALIGN);
  usize stackbase = stacktop + ALIGN2_FLOOR(MIN((memsize-datasize)/2, 4096), STK_ALIGN);

  dlog(
    "Memory layout: (%.3f MB total)\n"
    "   ┌─────────────────────┬────────────────────┬───────────────────────┐\n"
    "   │ data %12lu B │ %8lu B ← stack │ heap → %12lu B │\n"
    "   ├─────────────────────┼────────────────────┼───────────────────────┘\n"
    "   0                   0x%-8lx           0x%-8lx\n",
    (double)memsize/1024.0/1024.0,
    datasize, /*stacksize=*/stackbase - stacktop, /*heapsize=*/memsize - stackbase,
    stacktop, stackbase
  );

  vmstate vs = {
    .inlen    = inlen,
    .memsize  = memsize,
    .datasize = datasize,
    .heapbase = stackbase,
  };

  // initialize stack pointer and push main return address on stack
  SP = (u64)stackbase;
  push(&vs, iregs, inv, membase, 0, 8, MAIN_RET_PC);

  // initialize data (TODO: copy from ROM)
  memset(membase, 0, datasize);
  memcpy(membase, "hello\n", 6);

  logstate_header();
  return vmexec(&vs, iregs, inv, membase, 0);
}
