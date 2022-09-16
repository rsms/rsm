// execution engine
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "abuf.h"
#include "thread.h"
#include "sched.h"
#include "syscall.h"

#define TRACE_MEMORY // define to dlog LOAD and STORE operations

// MAIN_RET_PC: special PC value representing the main return address
#define MAIN_RET_PC  USIZE_MAX

// stackbase for a task
#define TASK_STACKBASE(t) ((void*)ALIGN2_FLOOR((uintptr)(t),STK_ALIGN))

// EXEC_PARAMS: parameters of an exec function
// EXEC_ARGS: arguments inside a exec function, for calling another function
#define EXEC_PARAMS T* t, u64* iregs, const rin_t* inv, usize pc
#define EXEC_ARGS   t, iregs, inv, pc

//———————————————————————————————————————————————————————————————————————————————————
// exec_logstate

#if defined(DEBUG) && !defined(RSM_NO_LIBC)
  #include <stdio.h>
  static void exec_logstate_header() {
    flockfile(stderr);
    fprintf(stderr, "\e[2m");
    for (int i = 0; i < 6; i++)
      fprintf(stderr, "      " REG_FMTNAME_PAT, REG_FMTNAME(i));
    fprintf(stderr, "           \e[9%cmSP\e[39m  │  PC  INSTRUCTION\e[22m\n",
      REG_FMTCOLORC(RSM_MAX_REG));
    funlockfile(stderr);
  }
  static void exec_logstate(EXEC_PARAMS) {
    flockfile(stderr);
    for (int i = 0; i < 6; i++)
      fprintf(stderr, REG_FMTVAL_PAT("%8llx"), REG_FMTVAL(i, iregs[i]));
    char buf[128];
    rsm_fmtinstr(buf, sizeof(buf), inv[pc], NULL, RSM_FMT_COLOR);
    fprintf(stderr, REG_FMTVAL_PAT("%13llx") "  │ %3ld  %s\n",
      REG_FMTVAL(RSM_MAX_REG, iregs[RSM_MAX_REG]), pc, buf);
    funlockfile(stderr);
  }
  #ifdef TRACE_MEMORY
    #if defined(SCHED_TRACE)
      #define tracemem(fmt, args...) \
        _schedtrace(2, "[interp/mem]", fmt " \e[2m(%s:%d)\e[0m", \
          ##args, __FILE__, __LINE__)
    #else
      #define tracemem dlog
    #endif
  #else
    #define tracemem(...) ((void)0)
  #endif
#else
  #define exec_logstate(...)        ((void)0)
  #define exec_logstate_header(...) ((void)0)
  #define tracemem(...)        ((void)0)
#endif


//———————————————————————————————————————————————————————————————————————————————————
// runtime error checking & reporting
typedef u32 execerr_t;
enum execerr_t {
  EX_E_UNALIGNED_STORE = 1,
  EX_E_UNALIGNED_ACCESS,
  EX_E_UNALIGNED_STACK,
  EX_E_STACK_OVERFLOW,
  EX_E_OOB_LOAD,
  EX_E_OOB_STORE,
  EX_E_OOB_PC,
  EX_E_OPNOI,
  EX_E_SHIFT_EXP,
} RSM_END_ENUM(execerr_t)
#if RSM_SAFE
  static void _execerr(EXEC_PARAMS, execerr_t err, u64 a1, u64 a2) {
    char buf[2048];
    abuf_t s1 = abuf_make(buf, sizeof(buf)); abuf_t* b = &s1;
    pc--; // undo the increment to make pc point to the violating instruction
    #define _(ERR, fmt, args...) case ERR: abuf_fmt(b, fmt, ##args); break;
    switch ((enum execerr_t)err) {
      _(EX_E_UNALIGNED_STORE, "unaligned memory store %llx (align %llu B)", a1, a2)
      _(EX_E_UNALIGNED_ACCESS,"unaligned memory access %llx (align %llu B)", a1, a2)
      _(EX_E_UNALIGNED_STACK, "unaligned stack pointer SP=%llx (align %lu B)", a1, STK_ALIGN)
      _(EX_E_STACK_OVERFLOW,  "stack overflow %llx (%llu)", a1, a2)
      _(EX_E_OOB_LOAD,        "memory load out of bounds %llx (align %llu B)", a1, a2)
      _(EX_E_OOB_STORE,       "memory store out of bounds %llx (align %llu B)", a1, a2)
      _(EX_E_OOB_PC,          "PC out of bounds %llx", a1)
      _(EX_E_OPNOI,           "op %s does not accept immediate value", rop_name(a1))
      _(EX_E_SHIFT_EXP,       "shift exponent %llu is too large", a1)
    }
    #undef _
    abuf_c(b, '\n');

    abuf_fmt(b, "  %08lx  ", pc);
    fmtinstr(b, inv[pc], RSM_FMT_COLOR);
    abuf_c(b, '\n');

    abuf_str(b, "Register state:");
    for (u32 i = 0, endi = RSM_MAX_REG; i < endi; i++) {
      if (i % 8 == 0) {
        usize len = b->len;
        abuf_fmt(b, "\n  R%u…%u", i, MIN(i+7, endi-1));
        abuf_fill(b, ' ', 10 - (b->len - len - 1));
      }
      abuf_fmt(b, " %8llx", iregs[i]);
    }

    abuf_terminate(b);
    log("%s", buf);
    abort();
  }
  #define __execerr_NARGS_X(a,b,c,d,...) d
  #define __execerr_NARGS(...) __execerr_NARGS_X(__VA_ARGS__,3,2,1,0,)
  #define __execerr_CONCAT_X(a,b) a##b
  #define __execerr_CONCAT(a,b) __execerr_CONCAT_X(a,b)
  #define __execerr_DISP(a,...) \
    __execerr_CONCAT(a,__execerr_NARGS(__VA_ARGS__))(__VA_ARGS__)
  #define __execerr1(err)     err, 0, 0
  #define __execerr2(err,a)   err, a, 0
  #define __execerr3(err,a,b) err, a, b
  #define execerr(...) _execerr(EXEC_ARGS, __execerr_DISP(__execerr,__VA_ARGS__))
  #define check(cond, ...) if UNLIKELY(!(cond)) execerr(__VA_ARGS__)
#else
  #define check(cond, ...) ((void)0)
#endif // RSM_SAFE

#define check_shift(exponent) check((exponent) < 64, EX_E_SHIFT_EXP, exponent)


// END runtime error checking & reporting
//———————————————————————————————————————————————————————————————————————————————————
// register accessor macros

#define SP  iregs[RSM_MAX_REG]  // R31
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

//———————————————————————————————————————————————————————————————————————————————————
// memory operations

// inline u64 MLOAD(TYPE, u64 addr)
#define MLOAD(TYPE, vaddr) ({ \
  u64 vaddr__ = (vaddr); \
  u64 value__ = VM_LOAD( \
    TYPE, m_vm_cache((t)->m, VM_PERM_RW), &(t)->m->s->vm_map, vaddr__); \
  tracemem("LOAD %s 0x%llx (align %lu) => 0x%llx (host 0x%lx)", \
    #TYPE, vaddr__, _Alignof(TYPE), value__, \
    VM_TRANSLATE(m_vm_cache((t)->m, VM_PERM_RW), &(t)->m->s->vm_map, \
      vaddr__, _Alignof(TYPE))); \
  value__; \
})

// inline void MSTORE(TYPE, u64 addr, u64 value)
#define MSTORE(TYPE, vaddr, value) { \
  u64 vaddr__ = (vaddr); \
  u64 value__ = (value); \
  tracemem("STORE %s 0x%llx (align %lu) => 0x%llx (host 0x%lx)", \
    #TYPE, value__, _Alignof(TYPE), vaddr__, \
    VM_TRANSLATE(m_vm_cache((t)->m, VM_PERM_RW), &(t)->m->s->vm_map, \
      vaddr__, _Alignof(TYPE))); \
  VM_STORE( \
    TYPE, m_vm_cache((t)->m, VM_PERM_RW), &(t)->m->s->vm_map, vaddr__, value__); \
}

static void mcopy(EXEC_PARAMS, u64 dstaddr, u64 srcaddr, u64 size) {
  panic("NOT IMPLEMENTED"); // TODO virtual memory
}

static i64 mcmp(EXEC_PARAMS, u64 xaddr, u64 yaddr, u64 size) {
  panic("NOT IMPLEMENTED"); // TODO virtual memory
}

static u64 copyv(EXEC_PARAMS, u64 n) {
  // A = instr[PC+1] + instr[PC+2]; PC+=2
  check(pc+n < t->instrc, EX_E_OOB_PC, (u64)pc);
  if (n == 1) return (u64)inv[pc];
  assert(n == 2);
  return (((u64)inv[pc]) << 32) | (u64)inv[pc+1];
}

// —————————— stack operations

inline static void push(EXEC_PARAMS, u64 size, u64 value) {
  u64 vaddr = SP;
  check(vaddr >= t->stack_lo + size, EX_E_STACK_OVERFLOW, vaddr);
  vaddr -= size;
  SP = vaddr;
  MSTORE(u64, vaddr, value);
}

inline static u64 pop(EXEC_PARAMS, u64 size) {
  u64 vaddr = SP;
  check(vaddr + size >= t->stack_lo, EX_E_STACK_OVERFLOW, vaddr);
  SP = vaddr + size;
  return MLOAD(u64, vaddr);
}

static void push_PC(EXEC_PARAMS) {
  // save PC on stack
  check(IS_ALIGN2(SP, STK_ALIGN), EX_E_UNALIGNED_STACK, SP, STK_ALIGN);
  push(EXEC_ARGS, 8, (u64)pc);
}


#define STK_SPLIT_LINK_SIZE  32u
static_assert(IS_ALIGN2(STK_SPLIT_LINK_SIZE, STK_ALIGN), "");


static i64 stkmem_grow(EXEC_PARAMS, i64 delta) {
  u64 sp = SP;
  u64 newsp = sp - (u64)delta;

  check(IS_ALIGN2(sp, STK_ALIGN), EX_E_UNALIGNED_STACK, sp, STK_ALIGN);
  check(IS_ALIGN2(delta, STK_ALIGN), EX_E_UNALIGNED_STORE, delta, STK_ALIGN);
  vm_map_t* vm_map = &t->m->s->vm_map;

  // Allocate new stack and split the stack
  u64 newsize = ALIGN2((u64)delta, PAGE_SIZE)*2;
  rerr_t err;
  vm_map_lock(vm_map);

  u64 stack_lo = 0;
  u64 npages = newsize/PAGE_SIZE;

  // Note: we could attempt to allocate a new stack just before the current one.
  // In practice it will always fail for the main task.
  // stack_lo = ALIGN2_FLOOR(sp - newsize, PAGE_SIZE);
  // npages = t->stack_lo - stack_lo;

  // find a free region of virtual memory
  err = vm_map_findspace(vm_map, &stack_lo, npages);
  if UNLIKELY(err) {
    if (err == rerr_nomem)
      panic("out of memory white trying to grow stack");
    safecheckf(err==0, "vm_map_findspace: %s", rerr_str(err));
  }
  dlog("vm_map found free space at %012llx", stack_lo);

  err = vm_map_add(vm_map, &stack_lo, 0, newsize/PAGE_SIZE, VM_PERM_RW);
  vm_map_unlock(vm_map);
  safecheckf(err==0, "vm_map %s", rerr_str(err));

  // new stack pointer.
  // reserve space on new stack for saving link to previous stack
  newsp = (stack_lo + newsize) - STK_SPLIT_LINK_SIZE;

  // save previous stack range
  vm_cache_t* vm_cache = m_vm_cache(t->m, VM_PERM_RW);
  u64* newstack = (void*)VM_TRANSLATE(vm_cache, vm_map, newsp, STK_ALIGN);
  newstack[0] = sp;          // newsp
  newstack[1] = t->stack_hi; // newsp+8
  newstack[2] = t->stack_lo; // newsp+16

  // update task with current stack limits (always inside page boundary)
  t->stack_lo = stack_lo;
  t->stack_hi = newsp;
  t->nsplitstack++;

  tracemem("splitstack add %012llx-%012llx (%llu KiB)",
    stack_lo, newsp+STK_SPLIT_LINK_SIZE, newsize/KiB);

  // return new stack pointer, offset by storage of previous stack range
  assert(newsp - delta >= stack_lo); // or our calculations above are incorrect
  return newsp - delta;
}

static i64 stkmem_shrink(EXEC_PARAMS, i64 delta) {
  // end of a split stack -- unmap it & unlink
  u64 sp = SP + (u64)-delta;

  // unmap current stack being retired
  vm_map_t* vm_map = &t->m->s->vm_map;
  usize stacksize = (t->stack_hi + STK_SPLIT_LINK_SIZE) - t->stack_lo;
  assert(IS_ALIGN2(stacksize, PAGE_SIZE));
  vm_map_lock(vm_map);
  UNUSED rerr_t err = vm_map_del(vm_map, t->stack_lo, stacksize/PAGE_SIZE);
  vm_map_unlock(vm_map);
  safecheckf(err==0, "splitstack vm_unmap %llx: %s", t->stack_lo, rerr_str(err));

  tracemem("splitstack del %012llx-%012llx (%zu KiB)",
    t->stack_lo, t->stack_hi + STK_SPLIT_LINK_SIZE, stacksize/KiB);

  // load range of parent stack (always inside page boundary)
  vm_cache_t* vm_cache = m_vm_cache(t->m, VM_PERM_RW);
  u64* stack = (void*)VM_TRANSLATE(vm_cache, vm_map, sp, STK_ALIGN);
  u64 newsp = stack[0];   // newsp
  t->stack_hi = stack[1]; // newsp+8
  t->stack_lo = stack[2]; // newsp+16

  t->nsplitstack--;

  return newsp;
}

static i64 stkmem_alloc(EXEC_PARAMS, i64 delta) {
  u64 sp = SP;
  u64 newsp;
  if (check_sub_overflow(sp, (u64)delta, &newsp) || UNLIKELY(newsp < t->stack_lo)) {
    // Not enough space in current stack
    return stkmem_grow(EXEC_ARGS, delta);
  }
  return newsp;
}

static i64 stkmem_free(EXEC_PARAMS, i64 delta) {
  u64 sp = SP;
  u64 newsp;
  if (check_add_overflow(sp, (u64)-delta, &newsp) || UNLIKELY(newsp > t->stack_hi)) {
    dlog("unbalanced stkmem calls (overflow from stkmem %lld)", delta);
    execerr(EX_E_STACK_OVERFLOW, sp, (u64)-delta);
    return sp;
  }
  if UNLIKELY(newsp == t->stack_hi && t->nsplitstack > 0) {
    // end of a split stack -- unmap it & unlink
    return stkmem_shrink(EXEC_ARGS, delta);
  }
  return newsp;
}

// stkmem returns new SP with delta added
static i64 stkmem(EXEC_PARAMS, i64 delta) {
  if (delta < 0)
    return stkmem_free(EXEC_ARGS, delta);
  return stkmem_alloc(EXEC_ARGS, delta);
}

// —————————— I/O

// libc prototypes
isize write(int fd, const void* buf, usize nbyte);
isize read(int fd, void* buf, usize nbyte);

static u64 _write(EXEC_PARAMS, u64 fd, u64 addr, u64 size) {
  // RA = write srcaddr=RB size=R(C) fd=Du
  panic("NOT IMPLEMENTED"); // TODO virtual memory
  // return (u64)write((int)fd, src, (usize)size);
}

static u64 _read(EXEC_PARAMS, u64 fd, u64 addr, u64 size) {
  // RA = read dstaddr=RB size=R(C) fd=Du
  panic("NOT IMPLEMENTED"); // TODO virtual memory
  // return (u64)read((int)fd, dst, (usize)size);
}

// —————————— syscall

// Returns true if execution should continue,
// returns false if execution should stop (scheduler may later resume the task.)
#undef _syscall
static bool _syscall(T* t, u64* iregs, usize pc, u32 syscall_op) {
  switch ((enum syscall_op)syscall_op) {

  case SC_EXIT:
    dlog("TODO exit program");
    task_exit(t);
    return false;

  case SC_TEXIT:
    task_exit(t);
    return false;

  case SC_SLEEP: {
    u64 nsec = iregs[0];
    if (nsec == 0)
      return true; // no-op
    enter_syscall(t);
    dlog("sleeping for %llu ns", nsec);
    u64 remaining = rsm_nanosleep(nsec);
    dlog("rsm_nanosleep() => remaining %llu", remaining);
    iregs[0] = remaining;
    return exit_syscall(t, /*priority*/0);
  }

  }
  panic("NOT IMPLEMENTED syscall %u", syscall_op);
  return true;
}

// —————————— arithmetic

static void on_overflow(T* t, usize pc, rop_t op, i64 x, i64 y, i64* dst) {
  char opch = '?';
  const char* typename = "i64";

  switch (op) {
    case rop_ADDS: opch = '+';
      // revert so we can print the initial values
      if (x == *dst) {
        __builtin_sub_overflow(x, y, &x);
      } else if (y == *dst) {
        __builtin_sub_overflow(x, y, &y);
      }
      break;
    case rop_SUBS: opch = '-';
      // revert so we can print the initial values
      if (x == *dst) {
        __builtin_add_overflow(x, y, &x);
      } else if (y == *dst) {
        __builtin_add_overflow(x, y, &y);
      }
      break;
    case rop_MULS: opch = '*';
      // can't revert multiplication overflow
      if (x == *dst || y == *dst)
        panic("signed integer overflow: multiplication overflows %s", typename);
      break;
  }

  // TODO raise error in scheduler/current task
  panic("signed integer overflow: %lld %c %lld overflows %s", x, opch, y, typename);
}

// void CHECK_OVERFLOW(name OP, name T, T x, T y, T* dstptr)
//   OP: add, sub, mul
//   T: i8, i16, i32, i64
#if __has_builtin(__builtin_add_overflow)
  #define CHECK_OVERFLOW(OP, T, x, y, dstptr) ( \
    UNLIKELY(__builtin_##OP##_overflow((T)(x), (T)(y), (T*)(dstptr))) ? \
      on_overflow(t, pc, RSM_GET_OP(in), (i64)(x), (i64)(y), (i64*)(dstptr)) : \
      ((void)0) \
  )
#else
  #define _CHECK_OVERFLOW_add(x, y, dstptr) (*(dstptr) = (x) + (y))
  #define _CHECK_OVERFLOW_sub(x, y, dstptr) (*(dstptr) = (x) - (y))
  #define _CHECK_OVERFLOW_mul(x, y, dstptr) (*(dstptr) = (x) * (y))
  #define CHECK_OVERFLOW(OP, T, x, y, dstptr) \
    _CHECK_OVERFLOW_##OP((T)(x), (T)(y), (T*)(dstptr))
#endif

// —————————— interpreter


#ifdef INTERPRET_USE_JUMPTABLE
  static const void* jumptab[(RSM_OP_COUNT << 1) | 1];
  static bool jumptab_init = false;
#endif


usize rsched_eval(EXEC_PARAMS) {
  // This is the interpreter loop.
  // It executes instructions until a syscall parks the task (or an error occurs.)
  //
  // First, we define how we will map an instruction to its corresponding handler code.
  // There are two options: using a label jump table or a switch statement.
  #ifdef INTERPRET_USE_JUMPTABLE // use label jump table
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
    if UNLIKELY(!jumptab_init) {
      jumptab_init = true;
      #define L_r(OP)      jumptab[rop_##OP << 1]     = &&_op_##OP;
      #define L_i(OP)      jumptab[(rop_##OP << 1)|1] = &&_op_##OP##_i;
      #define L__          L_r
      #define L_A          L_r
      #define L_AB         L_r
      #define L_ABv        L_i
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
      #undef L_r
      #undef L_i
      #undef L__
      #undef L_A
      #undef L_AB
      #undef L_ABv
      #undef L_ABC
      #undef L_ABCD
      #undef L_Au
      #undef L_ABu
      #undef L_ABCu
      #undef L_ABCDu
      #undef L_As
      #undef L_ABs
      #undef L_ABCs
      #undef L_ABCDs
    }
    #define DISPATCH \
      u32 ji = (RSM_GET_OP(in) << 1) | RSM_GET_i(in); \
      assertf(jumptab[ji], "\"%s\" i=%d", rop_name(RSM_GET_OP(in)), RSM_GET_i(in)); \
      goto *jumptab[ji];
    #define NEXT         continue;
    #define CASE_R(OP) _op_##OP:
    #define CASE_I(OP) _op_##OP##_i:
    #define DEFAULT  /* check done in DISPATCH */
  #else // use switch
    #define DISPATCH     switch ((RSM_GET_OP(in) << 1) | RSM_GET_i(in))
    #define NEXT         break;
    #define CASE_R(OP) case (rop_##OP << 1):
    #define CASE_I(OP) case (rop_##OP << 1)|1:
    #ifdef DEBUG
      #define DEFAULT \
        default: panic("\"%s\" i=%d", rop_name(RSM_GET_OP(in)), RSM_GET_i(in));
    #else
      #define DEFAULT
    #endif
  #endif

  exec_logstate_header();

  // instruction feed loop
  for (;;) {
    // load the next instruction and advance program counter
    assertf(pc < t->instrc, "pc overrun %lu", pc); exec_logstate(EXEC_ARGS);
    rin_t in = inv[pc++];
    // preload arguments A and B as most instructions need it
    u8 ar = RSM_GET_A(in);
    u8 br = RSM_GET_B(in);

    // depending on RSM_GET_OP(in) & RSM_GET_i(in), jump to a handler below
    DISPATCH {

    //———————————————————————————————————————————————————————————————————————————————————
    // operation handlers -- each op should have a do_OP(lastarg) macro defined here.
    // Ops that use the i(mmediate) flag will have their "do" macro used twice. reg & imm
    // These macros are used like this: "case op_and_i: do_OP(lastarg); break;"
    //———————————————————————————————————————————————————————————————————————————————————

    #define do_COPY(B)  RA = B
    #define do_COPYV(B) RA = copyv(EXEC_ARGS, B); pc += B;

    #define do_LOAD(C)   RA = MLOAD(u64, (u64)((i64)RB+(i64)C))
    #define do_LOAD4U(C) RA = MLOAD(u32, (u64)((i64)RB+(i64)C)) // zero-extend i32 to i64
    #define do_LOAD4S(C) RA = MLOAD(u32, (u64)((i64)RB+(i64)C)) // sign-extend i32 to i64
    #define do_LOAD2U(C) RA = MLOAD(u16, (u64)((i64)RB+(i64)C)) // zero-extend i16 to i64
    #define do_LOAD2S(C) RA = MLOAD(u16, (u64)((i64)RB+(i64)C)) // sign-extend i16 to i64
    #define do_LOAD1U(C) RA = MLOAD(u8,  (u64)((i64)RB+(i64)C)) // zero-extend i8 to i64
    #define do_LOAD1S(C) RA = MLOAD(u8,  (u64)((i64)RB+(i64)C)) // sign-extend i8 to i64

    #define do_STORE(C)  MSTORE(u64, (u64)((i64)RB+(i64)C), RA)
    #define do_STORE4(C) MSTORE(u32, (u64)((i64)RB+(i64)C), RA) // wrap i64 to i32
    #define do_STORE2(C) MSTORE(u16, (u64)((i64)RB+(i64)C), RA) // wrap i64 to i16
    #define do_STORE1(C) MSTORE(u8 , (u64)((i64)RB+(i64)C), RA) // wrap i64 to i8

    #define do_PUSH(A)  push(EXEC_ARGS, 8, A)
    #define do_POP(A)   A = pop(EXEC_ARGS, 8)

    #define do_ADD(C)  RA = RB + C
    #define do_SUB(C)  RA = RB - C
    #define do_MUL(C)  RA = RB * C
    #define do_ADDS(C) CHECK_OVERFLOW(add, i64, RB, C, &RA);
    #define do_SUBS(C) CHECK_OVERFLOW(sub, i64, RB, C, &RA);
    #define do_MULS(C) CHECK_OVERFLOW(mul, i64, RB, C, &RA);
    // TODO: if we keep ...S overflow-safe ops, expand to i32, i16 and i8 (e.g. do_ADDS4)
    #define do_DIV(C)  RA = RB / C
    #define do_MOD(C)  RA = RB % C
    #define do_AND(C)  RA = RB & C
    #define do_OR(C)   RA = RB | C
    #define do_XOR(C)  RA = RB ^ C
    #define do_SHL(C)  check_shift(C); RA = RB << C
    #define do_SHRS(C) check_shift(C); RA = (u64)((i64)RB >> C)
    #define do_SHRU(C) check_shift(C); RA = RB >> C
    #define do_BINV(B) RA = ~B
    #define do_NOT(B)  RA = !B

    #define do_EQ(C)   RA = RB == C
    #define do_NEQ(C)  RA = RB != C
    #define do_LTU(C)  RA = RB <  C
    #define do_LTEU(C) RA = RB <= C
    #define do_GTU(C)  RA = RB >  C
    #define do_GTEU(C) RA = RB >= C
    #define do_LTS(C)  RA = (i64)RB <  (i64)C
    #define do_LTES(C) RA = (i64)RB <= (i64)C
    #define do_GTS(C)  RA = (i64)RB >  (i64)C
    #define do_GTES(C) RA = (i64)RB >= (i64)C

    #define do_IF(B)   if (RA)      pc = (isize)((i64)pc + (i64)B)
    #define do_IFZ(B)  if (RA == 0) pc = (isize)((i64)pc + (i64)B)

    #define do_JUMP(A)  pc = (usize)A
    #define do_CALL(A)  push_PC(EXEC_ARGS); pc = (usize)A;

    #define do_TSPAWN(A)  iregs[0] = task_spawn(t, A, iregs);
    #define do_SYSCALL(A) if (!_syscall(t, iregs, pc, A)) return pc;
    #define do_WRITE(D)   RA = _write(EXEC_ARGS, D, RB, RC) // addr=RB size=RC fd=D
    #define do_READ(D)    RA = _read(EXEC_ARGS, D, RB, RC) // addr=RB size=RC fd=D
    #define do_MCOPY(C)   mcopy(EXEC_ARGS, RA, RB, C)
    #define do_MCMP(D)    RA = (u64)mcmp(EXEC_ARGS, RB, RC, D)
    #define do_STKMEM(A)  SP = stkmem(EXEC_ARGS, A);

    #define do_RET() pc = (usize)pop(EXEC_ARGS, 8) // load return address from stack

    //———————————————————————————————————————————————————————————————————————————————————
    // generators for handler labels (or case statements if a switch is used)
    #define CASE__(OP)     CASE_R(OP) do_##OP(); NEXT
    #define CASE_A(OP)     CASE_R(OP) do_##OP(RA); NEXT
    #define CASE_AB(OP)    CASE_R(OP) do_##OP(RB); NEXT
    #define CASE_ABC(OP)   CASE_R(OP) do_##OP(RC); NEXT
    #define CASE_ABCD(OP)  CASE_R(OP) do_##OP(RD); NEXT
    #define CASE_Au(OP)    CASE_R(OP) do_##OP(RA); NEXT  CASE_I(OP) do_##OP(RAu); NEXT
    #define CASE_ABv(OP)   CASE_I(OP) do_##OP(RBu); NEXT
    #define CASE_ABu(OP)   CASE_R(OP) do_##OP(RB); NEXT  CASE_I(OP) do_##OP(RBu); NEXT
    #define CASE_ABCu(OP)  CASE_R(OP) do_##OP(RC); NEXT  CASE_I(OP) do_##OP(RCu); NEXT
    #define CASE_ABCDu(OP) CASE_R(OP) do_##OP(RD); NEXT  CASE_I(OP) do_##OP(RDu); NEXT
    #define CASE_As(OP)    CASE_R(OP) do_##OP(RA); NEXT  CASE_I(OP) do_##OP(RAs); NEXT
    #define CASE_ABs(OP)   CASE_R(OP) do_##OP(RB); NEXT  CASE_I(OP) do_##OP(RBs); NEXT
    #define CASE_ABCs(OP)  CASE_R(OP) do_##OP(RC); NEXT  CASE_I(OP) do_##OP(RCs); NEXT
    #define CASE_ABCDs(OP) CASE_R(OP) do_##OP(RD); NEXT  CASE_I(OP) do_##OP(RDs); NEXT
    #define _(OP, enc, ...) CASE_##enc(OP)
    RSM_FOREACH_OP(_)
    #undef _

    DEFAULT
  } // DISPATCH
  } // loop

  #undef DISPATCH
  #undef NEXT
  #undef CASE_R
  #undef CASE_I
  #undef DEFAULT
}

