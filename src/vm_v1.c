// virtual machine
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "abuf.h"

//#define DEBUG_VM_LOG_LOADSTORE // define to dlog LOAD and STORE operations

// virtual memory segmentation (for mapped devices)
// M_SEG_COUNT: max mapped devices = M_SEG_COUNT-2 (must be pow2)
// M_SEG_SIZE:  size of each segment, in bytes (must be pow2)
// (ideal M_SEG_SIZE = (UINTPTR_MAX / M_SEG_COUNT) = ALIGN2(UINTPTR_MAX / M_SEG_COUNT))
#if UINTPTR_MAX >= 0xFFFFFFFFFFFFFFFFu
  #define M_SEG_COUNT 0x10000lu
  #define M_SEG_SIZE  0x100000000000lu // 17TB
#elif UINTPTR_MAX >= 0xFFFFFFFFu
  #define M_SEG_COUNT 8lu
  #define M_SEG_SIZE  0x10000000lu  // 256MB
#endif
static_assert(IS_POW2(M_SEG_COUNT), "M_SEG_COUNT is not pow2");
static_assert(IS_POW2(M_SEG_SIZE), "M_SEG_SIZE is not pow2");
static_assert(U64_MAX/M_SEG_COUNT > M_SEG_SIZE, "too many segments / M_SEG_SIZE too large");

// vmstate: internal vm state
typedef struct {
  rvm          pub;      // public API
  rmemalloc_t* memalloc; // memory allocator
  usize        inlen;    // number of instructions at inv
  union { usize datasize, stacktop; };  // aka
  union { usize heapbase, stackbase; }; // aka
  // memory segments
  void* mbase[M_SEG_COUNT]; // [0]=RAM, [N]=devN ... [M_SEG_COUNT-1]=invalid
  usize msize[M_SEG_COUNT];
} vmstate;
static_assert(offsetof(vmstate,pub) == 0, "vmstate.pub is not first member");

// vm interperter functions signature
#define VMPARAMS vmstate* vs, u64* iregs, const rinstr* inv, usize pc
#define VMARGS   vs, iregs, inv, pc

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
    rsm_fmtinstr(buf, sizeof(buf), inv[pc], NULL, RSM_FMT_COLOR);
    fprintf(stderr, "  │ %3ld  %s\n", pc, buf);
  }
  #ifdef DEBUG_VM_LOG_LOADSTORE
    #define log_loadstore dlog
  #else
    #define log_loadstore(...) ((void)0)
  #endif
#else
  #define logstate(...)        ((void)0)
  #define logstate_header(...) ((void)0)
  #define log_loadstore(...)   ((void)0)
#endif

// constants
#define STK_ALIGN    8           // stack alignment (== sizeof(u64))
#define STK_MIN      2048        // minium stack size (TODO: consider making part of ROM)
#define STK_MAX      (1024*1024) // maximum stack size
#define MAIN_RET_PC  USIZE_MAX   // special PC value representing the main return address

static_assert(STK_MIN % STK_ALIGN == 0, "STK_MIN not aligned to STK_ALIGN");

// accessor macros
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

// runtime error checking & reporting
#if RSM_SAFE
typedef u32 vmerror;
enum vmerror {
  VM_E_UNALIGNED_STORE = 1,
  VM_E_UNALIGNED_ACCESS,
  VM_E_UNALIGNED_STACK,
  VM_E_STACK_OVERFLOW,
  VM_E_OOB_LOAD,
  VM_E_OOB_STORE,
  VM_E_OOB_PC,
  VM_E_OPNOI,
  VM_E_SHIFT_EXP,
} RSM_END_ENUM(vmerror)

static void _vmerr(VMPARAMS, vmerror err, u64 arg1, u64 arg2) {
  char buf[2048];
  abuf_t s1 = abuf_make(buf, sizeof(buf)); abuf_t* s = &s1;
  pc--; // undo the increment to make pc point to the violating instruction
  #define S(ERR, fmt, args...) case ERR: abuf_fmt(s, fmt, ##args); break;
  switch ((enum vmerror)err) {
    S(VM_E_UNALIGNED_STORE, "unaligned memory store %llx (align %llu B)", arg1, arg2)
    S(VM_E_UNALIGNED_ACCESS,"unaligned memory access %llx (align %llu B)", arg1, arg2)
    S(VM_E_UNALIGNED_STACK, "unaligned stack pointer SP=%llx (align %d B)", arg1, STK_ALIGN)
    S(VM_E_STACK_OVERFLOW,  "stack overflow %llx (align %llu B)", arg1, arg2)
    S(VM_E_OOB_LOAD,        "memory load out of bounds %llx (align %llu B)", arg1, arg2)
    S(VM_E_OOB_STORE,       "memory store out of bounds %llx (align %llu B)", arg1, arg2)
    S(VM_E_OOB_PC,          "PC out of bounds %llx", arg1)
    S(VM_E_OPNOI,           "op %s does not accept immediate value", rop_name(arg1))
    S(VM_E_SHIFT_EXP,       "shift exponent %llu is too large", arg1)
  }
  #undef S
  abuf_c(s, '\n');

  abuf_fmt(s, "  %08lx  ", PC);
  fmtinstr(s, inv[pc], RSM_FMT_COLOR);
  abuf_c(s, '\n');

  abuf_str(s, "Register state:");

  for (u32 i = 0, endi = RSM_MAX_REG; i < endi; i++) {
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

  usize heapsize = vs->msize[0] - vs->heapbase;
  abuf_fmt(s, "\nMemory: (%lu B)", vs->msize[0]);
  abuf_fmt(s, "\n  data         0...%-8lx %10lu B", vs->datasize, vs->datasize);
  abuf_fmt(s, "\n  stack %8lx...%-8lx %10lu B", stacktop, vs->heapbase, stacksize);
  abuf_fmt(s, "\n  heap  %8lx...%-8lx %10lu B", vs->heapbase, vs->msize[0], heapsize);

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

#define check_loadstore(addr, align, ealign, eoob) { \
  check(IS_ALIGN2(addr, align), ealign, addr, align); \
  check(addr <= endaddr(VMARGS, addr), eoob, addr, align); \
}

#define check_shift(exponent) check((exponent) < 64, VM_E_SHIFT_EXP, exponent)

#if RSM_SAFE
  // endaddr returns the fist invalid address for the segment addr is apart of.
  // (In other words: it returns the last valid address + 1.)
  inline static u64 endaddr(VMPARAMS, u64 addr) {
    usize index = MIN((usize)(addr / M_SEG_SIZE), M_SEG_COUNT-1);
    return index*M_SEG_SIZE + vs->msize[index];
  }
#endif

// mbase_index translates a vm address to a host address's index in vs->mbase
inline static usize mbase_index(VMPARAMS, u64 addr) {
  usize index = (usize)(addr / M_SEG_SIZE);
  check(index < M_SEG_COUNT, VM_E_OOB_STORE, addr);
  return index;
}

// hostaddr translates a vm address to a host address
inline static void* hostaddr(VMPARAMS, u64 addr) {
  usize index = mbase_index(VMARGS, addr);
  addr -= index * M_SEG_SIZE;
  return vs->mbase[index] + addr;
}

inline static void* hostaddr_check_access(VMPARAMS, u64 align, u64 addr) {
  check_loadstore(addr, align, VM_E_UNALIGNED_ACCESS, VM_E_OOB_LOAD);
  return hostaddr(VMARGS, addr);
}

// inline u64 LOAD(TYPE, u64 addr)
#define LOAD(TYPE, addr) ({ u64 a__=(addr); \
  u64 v__ = *(TYPE*)hostaddr_check_access(VMARGS, sizeof(TYPE), a__); \
  log_loadstore("LOAD  %s mem[0x%llx] => 0x%llx", #TYPE, a__, v__); \
  v__; \
})

// inline void STORE(TYPE, u64 addr, u64 value)
#define STORE(TYPE, addr, value) { u64 a__=(addr), v__=(value); \
  log_loadstore("STORE %s mem[0x%llx] <= 0x%llx", #TYPE, a__, v__); \
  check_loadstore(a__, sizeof(TYPE), VM_E_UNALIGNED_STORE, VM_E_OOB_STORE); \
  usize index = mbase_index(VMARGS, a__); \
  *(TYPE*)(vs->mbase[index] + a__ - index*M_SEG_SIZE) = v__; \
}

// inline void STORE_RAM(TYPE, u64 addr, u64 value)
#define STORE_RAM(TYPE, addr, value) { u64 a__=(addr), v__=(value);  \
  log_loadstore("STORE %s mem[0x%llx] <= 0x%llx", #TYPE, a__, v__); \
  assert(a__ < M_SEG_SIZE); \
  check_loadstore(a__, sizeof(TYPE), VM_E_UNALIGNED_STORE, VM_E_OOB_STORE); \
  *(u64*)(vs->mbase[0] + (uintptr)a__) = v__; \
}

// libc
isize write(int fd, const void* buf, usize nbyte);
isize read(int fd, void* buf, usize nbyte);

static u64 _write(VMPARAMS, u64 fd, u64 addr, u64 size) {
  // RA = write srcaddr=RB size=R(C) fd=Du
  void* src = hostaddr_check_access(VMARGS, 1, addr);
  return (u64)write((int)fd, src, (usize)size);
}

static u64 _read(VMPARAMS, u64 fd, u64 addr, u64 size) {
  // RA = read dstaddr=RB size=R(C) fd=Du
  void* dst = hostaddr_check_access(VMARGS, 1, addr);
  return (u64)read((int)fd, dst, (usize)size);
}

//————————————————————————————————————————————————————————————————————————————————————————
// BEGIN memory-mapped devices experiment
typedef struct rvm_dev_t rvm_dev_t;
struct rvm_dev_t {
  u32 id;
  rerror(*open)(rvm_dev_t*, void** mp, usize* msizep, u32 flags);
  rerror(*close)(rvm_dev_t*, void* m, usize msize);
  rerror(*refresh)(rvm_dev_t*, void* m, usize msize, u32 flags);
};

static void* testdev_mem[32/sizeof(void*)];
static rerror tesdev_open(rvm_dev_t* dev, void** mp, usize* msizep, u32 flags) {
  *mp = testdev_mem;
  *msizep = sizeof(testdev_mem);
  return 0;
}
static rerror tesdev_close(rvm_dev_t* dev, void* m, usize msize) {
  return 0;
}
static rerror testdev_refresh(rvm_dev_t* dev, void* m, usize msize, u32 flags) {
  char buf[(sizeof(testdev_mem)*3) + 1];
  abuf_t s = abuf_make(buf, sizeof(buf));
  abuf_repr(&s, testdev_mem, sizeof(testdev_mem));
  abuf_terminate(&s);
  dlog("[testdev_refresh] device memory contents:\n\"%s\"", buf);
  return 0;
}
static rvm_dev_t testdev = {
  .id      = 1,
  .open    = &tesdev_open,
  .close   = &tesdev_close,
  .refresh = &testdev_refresh,
};

static u64 dev_open(VMPARAMS, u64 devid) {
  dlog("dev_open #%llu", devid);
  if (devid < 1 || devid > M_SEG_COUNT-1)
    return 0;
  testdev.id = (u32)devid;
  rerror err = testdev.open(&testdev, &vs->mbase[devid], &vs->msize[devid], 0);
  if (err) {
    log("devopen [%u] failed: %s", testdev.id, rerror_str(err));
    return 0;
  }
  u64 addr = devid * M_SEG_SIZE;
  dlog("[dev_open] addr 0x%llx, membase %p", addr, vs->mbase[devid]);
  return addr;
}
// END memory-mapped devices experiment
//————————————————————————————————————————————————————————————————————————————————————————

static void scall(VMPARAMS, u8 ar, rinstr in) {
  dlog("scall not implemented");
}

inline static void push(VMPARAMS, u64 size, u64 val) {
  u64 addr = SP;
  check(addr >= size && addr - size >= vs->stacktop, VM_E_STACK_OVERFLOW, addr);
  addr -= size;
  SP = addr;
  STORE_RAM(u64, addr, val);
}

inline static u64 pop(VMPARAMS, usize size) {
  usize addr = SP;
  check(USIZE_MAX-addr >= size && addr+size <= vs->stackbase, VM_E_STACK_OVERFLOW, addr);
  SP = addr + size;
  return LOAD(u64, addr);
}

static void push_PC(VMPARAMS) {
  // save PC on stack
  check(IS_ALIGN2(SP, STK_ALIGN), VM_E_UNALIGNED_STACK, SP, STK_ALIGN);
  push(VMARGS, 8, (u64)pc);
}

static u64 copyv(VMPARAMS, u64 n) {
  // A = instr[PC+1] + instr[PC+2]; PC+=2
  check(pc+n < vs->inlen, VM_E_OOB_PC, (u64)pc);
  if (n == 1) return (u64)inv[pc];
  assert(n == 2);
  return (((u64)inv[pc]) << 32) | (u64)inv[pc+1];
}

static void mcopy(VMPARAMS, u64 dstaddr, u64 srcaddr, u64 size) {
  void* dst = hostaddr_check_access(VMARGS, 1, dstaddr);
  void* src = hostaddr_check_access(VMARGS, 1, srcaddr);
  memcpy(dst, src, (usize)size);
}

static i64 mcmp(VMPARAMS, u64 xaddr, u64 yaddr, u64 size) {
  void* x = hostaddr_check_access(VMARGS, 1, xaddr);
  void* y = hostaddr_check_access(VMARGS, 1, yaddr);
  return (i64)memcmp(x, y, (usize)size);
}

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

    //———————————————————————————————————————————————————————————————————————————————————
    // operation handlers -- each op should have a do_OP(lastarg) macro defined here.
    // Ops that use the i(mmediate) flag will have their "do" macro used twice. reg & imm
    // These macros are used like this: "case op_and_i: do_OP(lastarg); break;"
    //———————————————————————————————————————————————————————————————————————————————————

    #define do_COPY(B)  RA = B
    #define do_COPYV(B) RA = copyv(VMARGS, B); pc += B;

    #define do_LOAD(C)   RA = LOAD(u64, (u64)((i64)RB+(i64)C))
    #define do_LOAD4U(C) RA = LOAD(u32, (u64)((i64)RB+(i64)C)) // zero-extend i32 to i64
    #define do_LOAD4S(C) RA = LOAD(u32, (u64)((i64)RB+(i64)C)) // sign-extend i32 to i64
    #define do_LOAD2U(C) RA = LOAD(u16, (u64)((i64)RB+(i64)C)) // zero-extend i16 to i64
    #define do_LOAD2S(C) RA = LOAD(u16, (u64)((i64)RB+(i64)C)) // sign-extend i16 to i64
    #define do_LOAD1U(C) RA = LOAD(u8,  (u64)((i64)RB+(i64)C)) // zero-extend i8 to i64
    #define do_LOAD1S(C) RA = LOAD(u8,  (u64)((i64)RB+(i64)C)) // sign-extend i8 to i64

    #define do_STORE(C)  STORE(u64, (u64)((i64)RB+(i64)C), RA)
    #define do_STORE4(C) STORE(u32, (u64)((i64)RB+(i64)C), RA) // wrap i64 to i32
    #define do_STORE2(C) STORE(u16, (u64)((i64)RB+(i64)C), RA) // wrap i64 to i16
    #define do_STORE1(C) STORE(u8 , (u64)((i64)RB+(i64)C), RA) // wrap i64 to i8

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
    #define do_BINV(B) RA = ~B

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
    #define do_CALL(A)  push_PC(VMARGS); pc = (usize)A;
    #define do_SCALL(A) scall(VMARGS, A, in)

    #define do_WRITE(D)   RA = _write(VMARGS, D, RB, RC) // addr=RB size=RC fd=D
    #define do_READ(D)    RA = _read(VMARGS, D, RB, RC) // addr=RB size=RC fd=D
    #define do_DEVOPEN(B) RA = dev_open(VMARGS, B)
    #define do_MCOPY(C)   mcopy(VMARGS, RA, RB, C)
    #define do_MCMP(D)    RA = (u64)mcmp(VMARGS, RB, RC, D)

    #define do_RET() { \
      pc = (usize)pop(VMARGS, 8); /* load return address from stack */ \
      /* TODO: instead of MAIN_RET_PC, append coro(end) or yield(end) or exit instr */ \
      /* to end of inv and setup main return to that address. */ \
      if (pc == MAIN_RET_PC) return; \
    }

    //———————————————————————————————————————————————————————————————————————————————————
    // generators for handler labels (or case statements if a switch is used)
    #define CASE__(OP)     R(OP) do_##OP(); NEXT
    #define CASE_A(OP)     R(OP) do_##OP(RA); NEXT
    #define CASE_AB(OP)    R(OP) do_##OP(RB); NEXT
    #define CASE_ABC(OP)   R(OP) do_##OP(RC); NEXT
    #define CASE_ABCD(OP)  R(OP) do_##OP(RD); NEXT
    #define CASE_Au(OP)    R(OP) do_##OP(RA); NEXT   I(OP) do_##OP(RAu); NEXT
    #define CASE_ABv(OP)   I(OP) do_##OP(RBu); NEXT
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

#if DEBUG
static void log_memory(rrom_t* rom, vmstate* vs) {
  void* membase = vs->mbase[0];
  usize memsize = (usize)vs->msize[0];
  usize stacksize = vs->stackbase - vs->stacktop;
  usize heapsize = memsize - vs->stackbase;
  dlog(
    "Memory layout: (%.3f MB total)\n"
    "     ┌─────────────────────┬────────────────────┬───────────────────────┐\n"
    "segm │ data %12lu B │ %8lu B ← stack │ heap → %12lu B │\n"
    "     ├─────────────────────┼────────────────────┼───────────────────────┘\n"
    "addr 0             %8lx│%-8lx           %-8lx",
    (double)memsize/1024.0/1024.0,
    rom->datasize, stacksize, heapsize,
    rom->datasize, vs->stacktop, vs->stackbase
  );
  if (rom->datasize > 0) {
    char buf[1024];
    abuf_t s = abuf_make(buf, sizeof(buf));
    if (rom->datasize > sizeof(buf)/3) {
      abuf_reprhex(&s, membase, sizeof(buf)/3 - strlen("…"));
      abuf_str(&s, "…");
    } else {
      abuf_reprhex(&s, membase, rom->datasize);
    }
    abuf_terminate(&s);
    dlog("Initial data contents:\n%s", buf);
  }
}
#endif // DEBUG


static rerror vm_loadrom(rrom_t* rom) {
  if (rom->code)
    return 0; // already loaded
  rerror err = rsm_loadrom(rom);
  if (err == 0 && (rom->code == NULL || rom->codelen == 0))
    err = rerr_invalid; // ROM without (or with empty) code section
  return err;
}


rerror rsm_vmexec(rvm* vm, rrom_t* rom) {
  rerror err = vm_loadrom(rom);
  if (err)
    return err;

  // memory layout:
  //    ┌─────────────┬─────────────┬───────────···
  //    │ data        │     ← stack │ heap →
  //    ├─────────────┼─────────────┼───────────···
  // rambase      datasize      heapbase
  //              stacktop      stackbase
  //
  // make sure rambase is aligned to most stringent alignment of data
  uintptr ma = ALIGN2((uintptr)vm->rambase, MAX((uintptr)rom->dataalign, STK_ALIGN));
  if UNLIKELY(ma != (uintptr)vm->rambase) {
    uintptr diff = ma - (uintptr)vm->rambase;
    vm->ramsize = diff > vm->ramsize ? 0 : vm->ramsize - diff;
    vm->rambase = (void*)ma;
    dlog("adjusting rambase+%lu ramsize-%lu (address alignment)", (usize)diff, (usize)diff);
  }

  // check if we have enough memory for ROM data
  if UNLIKELY(vm->ramsize < STK_MIN || rom->datasize > vm->ramsize - STK_MIN)
    return rerr_nomem;

  // limit RAM memory size to M_SEG_SIZE
  if (vm->ramsize > M_SEG_SIZE) {
    dlog("memory size capped at %zu B", M_SEG_SIZE);
    vm->ramsize = M_SEG_SIZE;
  }

  // calculate stackbase and check if we have enough memory
  usize stackbase; {
    usize stacksize = MAX(STK_MIN, MIN(STK_MAX, (vm->ramsize - rom->datasize)/2));
    usize stacktop = ALIGN2(rom->datasize, STK_ALIGN);
    stackbase = stacktop + ALIGN2_FLOOR(stacksize, STK_ALIGN);
    // note: At this point stacksize is no longer logically correct.
    //       The correct value is now: stacksize = stackbase - stacktop
    if UNLIKELY(stackbase > vm->ramsize)
      return rerr_nomem; // not enough memory for STK_MIN
  }

  // initialize vm state
  vmstate vs = {
    .inlen     = rom->codelen,
    .datasize  = rom->datasize, // aka stacktop
    .stackbase = stackbase,     // aka heapbase
    .mbase = {vm->rambase},
    .msize = {vm->ramsize},
  };

  // initialize registers
  u64* iregs = vm->iregs;
  SP = (u64)stackbase;       // stack pointer
  iregs[8] = (u64)stackbase; // R8 = heapstart
  iregs[9] = (u64)vm->ramsize;   // R9 = heapend

  // push main return address on stack
  push(&vs, iregs, rom->code, 0, 8, MAIN_RET_PC);

  // initialize global data by copying from ROM
  if (rom->datasize > 0)
    memcpy(vm->rambase, rom->data, rom->datasize);

  #if DEBUG
    log_memory(rom, &vs);
    logstate_header();
  #endif

  vmexec(&vs, iregs, rom->code, 0);

  if (vs.mbase[testdev.id]) {
    rerror err = testdev.refresh(&testdev, vs.mbase[testdev.id], vs.msize[testdev.id], 0);
    rerror err2 = testdev.close(&testdev, vs.mbase[testdev.id], vs.msize[testdev.id]);
    return err ? err : err2;
  }

  return 0;
}
