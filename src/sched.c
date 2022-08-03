// virtual machine
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "abuf.h"
#include "thread.h"

//#define DEBUG_VM_LOG_LOADSTORE // define to dlog LOAD and STORE operations

// stack constants
#define STK_ALIGN    8           // stack alignment (== sizeof(u64))
#define STK_MIN      2048        // minium stack size (TODO: consider making part of ROM)
#define STK_MAX      (1024*1024) // maximum stack size
static_assert(STK_MIN % STK_ALIGN == 0, "STK_MIN not aligned to STK_ALIGN");

// MAIN_RET_PC: special PC value representing the main return address
#define MAIN_RET_PC  USIZE_MAX

//———————————————————————————————————————————————————————————————————————————————————
// vm v2

// SCHED_TRACE: when defined, verbose log tracing on stderr is enabled.
// The value is used as a prefix for log messages.
#define SCHED_TRACE "\e[1m▍\e[0m "

// S_MAXPROCS is the upper limit of concurrent P's; the effective CPU parallelism limit.
// There are no fundamental restrictions on the value.
// S_MAXPROCS pointers are allocated in S.allp[S_MAXPROCS].
#define S_MAXPROCS 256

// P_RUNQSIZE is the size of P.runq. Must be power-of-two (2^N)
#define P_RUNQSIZE 256 // 256 is the value Go 1.16 uses

// - stack per coroutine
// - shared heap
// - ROM data is shared (like a second heap)
// - TODO: ROM instructions assume its global data starts at address 0x0
//         Either a "linker" needs to rewrite all data addresses when loading a ROM,
//         or we need memory translation per ROM (ie. depending on instruction origin.)
//
// Main scheduling concepts:
// - T for Task, a coroutine task
// - M for Machine, an OS thread
// - P for Processor, an execution resource required to execute a T
// - S for Scheduler
// M must have an associated P to execute T, however
// a M can be blocked or in a syscall without an associated P.
typedef struct S S;
typedef struct T T;
typedef struct M M;
typedef struct P P;

typedef u8 TStatus;
enum TStatus {
  // T_IDLE: task was just allocated and has not yet been initialized
  T_IDLE = 0,

  // T_RUNNABLE: task is on a run queue.
  // It is not currently executing user code.
  // The stack is not owned.
  T_RUNNABLE, // 1

  // T_RUNNING: task may execute user code.
  // The stack is owned by this task.
  // It is not on a run queue.
  // It is assigned an M and a P (t.m and t.m.p are valid).
  T_RUNNING, // 2

  // T_SYSCALL: task is executing a system call.
  // It is not executing user code.
  // The stack is owned by this task.
  // It is not on a run queue.
  // It is assigned an M.
  T_SYSCALL, // 3

  // T_WAITING: task is blocked in the runtime.
  // It is not executing user code.
  // It is not on a run queue, but should be recorded somewhere
  // (e.g., a channel wait queue) so it can be ready()d when necessary.
  // The stack is not owned *except* that a channel operation may read or
  // write parts of the stack under the appropriate channel
  // lock. Otherwise, it is not safe to access the stack after a
  // task enters T_WAITING (e.g., it may get moved).
  T_WAITING, // 4

  // T_DEAD: task is unused.
  // It may be just exited, on a free list, or just being initialized.
  // It is not executing user code.
  // It may or may not have a stack allocated.
  // The T and its stack (if any) are owned by the M that is exiting the T
  // or that obtained the T from the free list.
  T_DEAD, // 5
};

typedef u8 PStatus;
enum PStatus {
  P_IDLE = 0,
  P_RUNNING, // Only this P is allowed to change from P_RUNNING
  P_SYSCALL,
  P_DEAD,
};

// tctx: task execution context used for task switching, stored on stack
typedef struct {
  double fregs[RSM_NREGS - RSM_NTMPREGS];
  u64    iregs[RSM_NREGS - RSM_NTMPREGS - 1]; // does not include SP
} __attribute__((__aligned__(STK_ALIGN))) tctx;

struct T {
  u64         id;
  S*          s;         // parent scheduler
  M* nullable m;         // attached M
  T* nullable parent;    // task that spawned this task
  T* nullable schedlink; // next task to be scheduled

  usize        pc;     // program counter; next instruction = iv[pc]
  usize        instrc; // instruction count
  const rin_t* instrv; // instruction array
  const void*  rodata; // global read-only data (from ROM)

  void*   sp;       // saved SP register value used by m_switchtask
  uintptr stacktop; // end of stack (lowest valid stack address)

  u64              waitsince; // approx time when the T became blocked
  _Atomic(TStatus) status;
};

struct M {
  u32         id;
  T           t0;       // task with scheduling stack (m_start entry)
  S*          s;        // parent scheduler
  P* nullable p;        // attached p for executing Ts (NULL if not executing)
  T* nullable currt;    // current running task
  u32         locks;    // number of logical locks held by Ts to this M
  bool        spinning; // m is out of work and is actively looking for work
  bool        blocked;  // m is blocked on a note

  // vm register values
  u64    iregs[RSM_NREGS];
  double fregs[RSM_NREGS];
};

struct P {
  u32         id;        // corresponds to offset in s.allp
  u32         schedtick; // incremented on every scheduler call
  PStatus     status;
  bool        preempt; // this P should be enter the scheduler ASAP
  S*          s;       // parent scheduler
  M* nullable m;       // associated m (NULL when P is idle)

  // Queue of runnable tasks. Accessed without lock.
  _Atomic(u32) runqhead;
  _Atomic(u32) runqtail;
  T*           runq[P_RUNQSIZE];
  // runnext, if non-NULL, is a runnable T that was ready'd by
  // the current T and should be run next instead of what's in
  // runq if there's time remaining in the running T's time
  // slice. It will inherit the time left in the current time
  // slice. If a set of coroutines is locked in a
  // communicate-and-wait pattern, this schedules that set as a
  // unit and eliminates the (potentially large) scheduling
  // latency that otherwise arises from adding the ready'd
  // coroutines to the end of the run queue.
  _Atomic(T*) runnext;
};

// typedef struct mpage mpage;
// struct mpage {
//   mpage* nullable next;
//   mpage* nullable prev;
// };

struct S {
  rvm_t        pub;       // public API
  rmemalloc_t* memalloc;  // memory allocator
  M            m0;        // main M (bound to the OS thread which rvm_main is called on)
  T*           t0;        // main task on m0
  u32          maxmcount; // limit number of M's created (ie. OS thread limit)

  _Atomic(u64) tidgen; // T.id generator
  _Atomic(u32) midgen; // M.id generator

  P*           allp[S_MAXPROCS]; // all live P's (managed by s_procresize)
  _Atomic(u32) nprocs;           // max active Ps (also num valid P's in allp)

  // TODO: paged virtual memory implementation
  void* heapbase; // heap memory base address
  usize heapsize; // heap memory size

  // allt holds all live T's
  struct {
    mtx_t        lock;
    _Atomic(T**) ptr; // atomic for reading; lock used for writing
    _Atomic(u32) len; // atomic for reading; lock used for writing
    u32          cap; // capacity of ptr array
  } allt;

  // // vmem holds virtual memory data
  // struct {
  //   mpage* freephead;
  //   mpage* usedphead;
  // } vmem;
};

// stackbase for a task
#define TASK_STACKBASE(t) ((void*)ALIGN2_FLOOR((uintptr)(t),STK_ALIGN))


// t_get() and _g_t is thread-local storage of the current task on current OS thread
static _Thread_local T* _g_t = NULL;
static ALWAYS_INLINE T* t_get() { return _g_t; }


// vm interperter functions signature
#define VMPARAMS T* t, u64* iregs, const rin_t* inv, usize pc
#define VMARGS   t, iregs, inv, pc


// trace(const char* fmt, ...) -- debug tracing
#if defined(SCHED_TRACE) && !defined(RSM_NO_LIBC)
  #include <stdio.h>
  static void _vmtrace(const char* fmt, ...) {
    T* _t_ = t_get();
    FILE* fp = stderr;
    flockfile(fp);
    const char* prefix = SCHED_TRACE;
    fwrite(prefix, strlen(prefix), 1, fp);
    if (_t_ != NULL) {
      char T_color = (char)('1' + (_t_->id % 6));
      if (_t_->m != NULL) {
        char M_color = (char)('1' + (_t_->m->id % 6));
        if (_t_->m->p != NULL) {
          char P_color = (char)('1' + (_t_->m->p->id % 6));
          fprintf(fp, "\e[1;3%cmM%u\e[0m \e[1;3%cmP%u\e[0m \e[1;3%cmT%-2llu\e[0m ",
            M_color, _t_->m->id,
            P_color, _t_->m->p->id,
            T_color, _t_->id);
        } else {
          fprintf(fp, "\e[1;3%cmM%u\e[0m \e[1;2mP-\e[0m \e[1;3%cmT%-2llu\e[0m ",
            M_color, _t_->m->id,
            T_color, _t_->id);
        }
      } else {
        fprintf(fp, "\e[1;2mM-\e[0m \e[1;3%cmT%-2llu\e[0m ",
          T_color, _t_->id);
      }
    } else {
      fprintf(fp, "\e[1;2mM- T-\e[0m ");
    }
    va_list ap;
    va_start(ap, fmt);
    vfprintf(fp, fmt, ap);
    va_end(ap);
    funlockfile(fp);
  }
  #define trace(fmt, ...) \
    _vmtrace("\e[1;36m%-15s\e[39m " fmt "\e[0m\n", __FUNCTION__, ##__VA_ARGS__)
#else
  #define trace(...) do{}while(0)
#endif


#if defined(DEBUG) && !defined(RSM_NO_LIBC)
  #include <stdio.h>
  static void vmexec_logstate_header() {
    fprintf(stderr, "\e[2m");
    for (int i = 0; i < 6; i++) {
      fprintf(stderr, "  " REG_FMTNAME_PAT, REG_FMTNAME(i));
    }
    fprintf(stderr, "    \e[9%cmSP\e[39m  │  PC  INSTRUCTION\e[22m\n",
      REG_FMTCOLORC(RSM_MAX_REG));
  }
  static void vmexec_logstate(VMPARAMS) {
    for (int i = 0; i < 6; i++) {
      fprintf(stderr, REG_FMTVAL_PAT("%4llx"), REG_FMTVAL(i, iregs[i]));
    }
    char buf[128];
    rsm_fmtinstr(buf, sizeof(buf), inv[pc], NULL, RSM_FMT_COLOR);
    fprintf(stderr, REG_FMTVAL_PAT("%6llx") "  │ %3ld  %s\n",
      REG_FMTVAL(RSM_MAX_REG, iregs[RSM_MAX_REG]), pc, buf);
  }
  #ifdef DEBUG_VM_LOG_LOADSTORE
    #define vmexec_logmemop dlog
  #else
    #define vmexec_logmemop(...) ((void)0)
  #endif
#else
  #define vmexec_logstate(...)        ((void)0)
  #define vmexec_logstate_header(...) ((void)0)
  #define vmexec_logmemop(...)        ((void)0)
#endif


//———————————————————————————————————————————————————————————————————————————————————
// runtime error checking & reporting
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
#if RSM_SAFE
  static void _vmerr(VMPARAMS, vmerror err, u64 arg1, u64 arg2) {
    char buf[2048];
    abuf_t s1 = abuf_make(buf, sizeof(buf)); abuf_t* b = &s1;
    pc--; // undo the increment to make pc point to the violating instruction
    #define _(ERR, fmt, args...) case ERR: abuf_fmt(b, fmt, ##args); break;
    switch ((enum vmerror)err) {
      _(VM_E_UNALIGNED_STORE, "unaligned memory store %llx (align %llu B)", arg1, arg2)
      _(VM_E_UNALIGNED_ACCESS,"unaligned memory access %llx (align %llu B)", arg1, arg2)
      _(VM_E_UNALIGNED_STACK, "unaligned stack pointer SP=%llx (align %d B)", arg1, STK_ALIGN)
      _(VM_E_STACK_OVERFLOW,  "stack overflow %llx (align %llu B)", arg1, arg2)
      _(VM_E_OOB_LOAD,        "memory load out of bounds %llx (align %llu B)", arg1, arg2)
      _(VM_E_OOB_STORE,       "memory store out of bounds %llx (align %llu B)", arg1, arg2)
      _(VM_E_OOB_PC,          "PC out of bounds %llx", arg1)
      _(VM_E_OPNOI,           "op %s does not accept immediate value", rop_name(arg1))
      _(VM_E_SHIFT_EXP,       "shift exponent %llu is too large", arg1)
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
    return addr + t->s->heapsize;
  }
#endif

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


// hostaddr translates a vm address to a host address
inline static void* hostaddr(VMPARAMS, u64 addr) {
  // TODO FIXME virtual address table (or something more reliable)
  // [TMP XXX] for now, assume lower addresses refer to global data
  if (addr < 0xffff) {
    //dlog("get host address of ROM global 0x%llu", addr);
    return (void*)t->rodata + (uintptr)addr;
  }
  return (void*)(uintptr)addr;
}

inline static void* hostaddr_check_access(VMPARAMS, u64 align, u64 addr) {
  check_loadstore(addr, align, VM_E_UNALIGNED_ACCESS, VM_E_OOB_LOAD);
  return hostaddr(VMARGS, addr);
}

// inline u64 MLOAD(TYPE, u64 addr)
#define MLOAD(TYPE, addr) ({ \
  u64 a__=(addr); \
  u64 v__ = *(TYPE*)hostaddr_check_access(VMARGS, sizeof(TYPE), a__); \
  vmexec_logmemop("LOAD  %s mem[0x%llx] => 0x%llx", #TYPE, a__, v__); \
  v__; \
})

// inline void MSTORE(TYPE, u64 addr, u64 value)
#define MSTORE(TYPE, addr, value) { \
  u64 a__=(addr), v__=(value); \
  vmexec_logmemop("STORE %s mem[0x%llx] <= 0x%llx", #TYPE, a__, v__); \
  check_loadstore(a__, sizeof(TYPE), VM_E_UNALIGNED_STORE, VM_E_OOB_STORE); \
  void* addr__ = hostaddr(VMARGS, a__); \
  *(TYPE*)addr__ = v__; \
}

static u64 copyv(VMPARAMS, u64 n) {
  // A = instr[PC+1] + instr[PC+2]; PC+=2
  check(pc+n < t->instrc, VM_E_OOB_PC, (u64)pc);
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

// —————————— vm I/O

// libc prototypes
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

// —————————— vm syscall

static void scall(VMPARAMS, u8 ar, rin_t in) {
  dlog("scall not implemented");
}

static u64 dev_open(VMPARAMS, u64 devid) {
  // DEPRECATED (memory mapped devices via memory segmentation)
  return 0;
}

// —————————— vm stack operations

inline static void push(VMPARAMS, u64 size, u64 val) {
  u64 addr = SP;
  check(addr >= size && addr - size >= t->stacktop, VM_E_STACK_OVERFLOW, addr);
  addr -= size;
  SP = addr;
  MSTORE(u64, addr, val);
}

inline static u64 pop(VMPARAMS, usize size) {
  usize addr = SP;
  check(
    USIZE_MAX-addr >= size && (uintptr)addr+size <= (uintptr)TASK_STACKBASE(t),
    VM_E_STACK_OVERFLOW, addr);
  SP = addr + size;
  return MLOAD(u64, addr);
}

static void push_PC(VMPARAMS) {
  // save PC on stack
  check(IS_ALIGN2(SP, STK_ALIGN), VM_E_UNALIGNED_STACK, SP, STK_ALIGN);
  push(VMARGS, 8, (u64)pc);
}

// —————————— vm interpreter

static void t_vmexec(VMPARAMS) {
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

  vmexec_logstate_header();

  // instruction feed loop
  for (;;) {
    // load the next instruction and advance program counter
    assertf(pc < t->instrc, "pc overrun %lu", pc); vmexec_logstate(VMARGS);
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
    #define do_COPYV(B) RA = copyv(VMARGS, B); pc += B;

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


static inline void m_acquire(M* m) { m->locks++; }
static inline void m_release(M* m) { m->locks--; }


// static void t_free(T* task) {
//   usize memsize = (usize)(((uintptr)task + (uintptr)sizeof(T)) - task->stacktop);
//   rmem_free(task->s->memalloc, (void*)task->stacktop, memsize);
// }


// t_readstatus returns the value of t->status.
// On many archs, including x86, this is just a plain load.
static inline TStatus t_readstatus(T* t) {
  return AtomicLoad(&t->status, memory_order_relaxed);
}

// t_setstatus updates t->status using using an atomic store.
// This is only used when setting up new or recycled Ts.
// To change T.status for an existing T, use t_casstatus instead.
static void t_setstatus(T* t, TStatus newval) {
  AtomicStore(&t->status, newval, memory_order_relaxed);
}


// t_casstatus updates t->status using compare-and-swap.
// It blocks until it succeeds (until t->status==oldval.)
static void t_casstatus(T* t, TStatus oldval, TStatus newval) {
  assert(oldval != newval);

  // use a temporary variable for oldval since AtomicCAS will store the current value
  // to it on failure, but we want to swap values only when the current value is
  // explicitly oldval.
  TStatus oldvaltmp = oldval;

  // // See https://golang.org/cl/21503 for justification of the yield delay.
  // const u64 yieldDelay = 5 * 1000;
  // u64 nextYield = 0;

  for (int i = 0; !AtomicCASRelaxed(&t->status, &oldvaltmp, newval); i++) {
    // Note: on failure, when we get here, oldval has been updated; we compare it
    assertf(!(oldval == T_WAITING && oldvaltmp == T_RUNNABLE),
           "waiting for T_WAITING but is T_RUNNABLE");
    oldvaltmp = oldval; // restore oldvaltmp for next attempt

    // TODO:
    // if (i == 0)
    //   nextYield = nanotime() + yieldDelay;
    //
    // if (nanotime() < nextYield) {
    //   for x := 0; x < 10 && t.status != oldval; x++ {
    //     procyield(1);
    //   }
    // } else {
    //   osyield();
    //   nextYield = nanotime() + yieldDelay/2;
    // }
    //
    // temporary solution until the above has been researched & implemented
    YIELD_THREAD();
  }
}


// Put t and a batch of work from local runnable queue on global queue.
// Executed only by the owner P.
static bool p_runqputslow(P* p, T* t, u32 head, u32 tail) {
  panic("TODO");
  return false;
}


// p_runqput tries to put t on the local runnable queue.
// If runnext if false, runqput adds T to the tail of the runnable queue.
// If runnext is true, runqput puts T in the p.runnext slot.
// If the run queue is full, runnext puts T on the global queue.
// Executed only by the owner P.
static void p_runqput(P* p, T* t, bool runnext) {
  // if (randomizeScheduler && runnext && (fastrand() % 2) == 0)
  //   runnext = false;
  T* tp = t;

  if (runnext) {
    // puts t in the p.runnext slot.
    T* oldnext = p->runnext;
    while (!AtomicCASAcqRel(&p->runnext, &oldnext, t)) {
      // Note that when AtomicCAS fails, it performs a loads of the current value
      // into oldnext, thus we can simply loop here without having to explicitly
      // load oldnext=p->runnext.
    }
    trace("set p.runnext = T%llu", tp->id);
    if (oldnext == NULL)
      return;
    // Kick the old runnext out to the regular run queue
    tp = oldnext;
  }

  while (1) {
    // load-acquire, sync with consumers
    u32 head = AtomicLoadAcq(&p->runqhead);
    u32 tail = p->runqtail;
    if (tail - head < P_RUNQSIZE) {
      trace("set p.runq[%u] = T%llu", tail % P_RUNQSIZE, tp->id);
      p->runq[tail % P_RUNQSIZE] = tp;
      // store memory_order_release makes the item available for consumption
      AtomicStoreRel(&p->runqtail, tail + 1);
      return;
    }
    // Put t and move half of the locally scheduled runnables to global runq
    if (p_runqputslow(p, tp, head, tail))
      return;
    // the queue is not full, now the put above must succeed. retry...
  }
}


// Get T from local runnable queue.
// If inheritTime is true, T should inherit the remaining time in the current time slice.
// Otherwise, it should start a new time slice.
// Executed only by the owner P.
static T* p_runqget(P* p, bool* inheritTime) {
  // If there's a runnext, it's the next G to run.
  while (1) {
    T* next = p->runnext;
    if (next == NULL)
      break;
    if (AtomicCASAcqRel(&p->runnext, &next, NULL)) {
      *inheritTime = true;
      return next;
    }
  }

  trace("no runnext; trying dequeue p->runq");
  *inheritTime = false;

  while (1) {
    u32 head = AtomicLoadAcq(&p->runqhead); // load-acquire, sync with consumers
    u32 tail = p->runqtail;
    if (tail == head)
      return NULL;
    // trace("loop2 tail != head; load p->runq[%u]", head % P_RUNQSIZE);
    T* tp = p->runq[head % P_RUNQSIZE];
    // trace("loop2 tp => %p", tp);
    // trace("loop2 tp => T#%llu", tp->id);
    if (AtomicCASRel(&p->runqhead, &head, head + 1)) // cas-release, commits consume
      return tp;
    trace("CAS failure; retry");
  }
}


static rerr_t s_allt_add(S* s, T* t) {
  assert(t->s == s);
  assert(t_readstatus(t) != T_IDLE);

  mtx_lock(&s->allt.lock);

  if (s->allt.len == s->allt.cap) {
    rmem_t m = { s->allt.ptr, (usize)s->allt.cap * sizeof(void*) };
    if UNLIKELY(!rmem_resize(s->memalloc, &m, (s->allt.cap + 64) * sizeof(void*))) {
      mtx_unlock(&s->allt.lock);
      return rerr_nomem;
    }
    s->allt.cap = m.size / sizeof(void*);
    AtomicStore(&s->allt.ptr, m.p, memory_order_release);
  }

  trace("set s.allt[%u] = T%llu", s->allt.len, t->id);
  s->allt.ptr[s->allt.len++] = t;
  AtomicStore(&s->allt.len, s->allt.len, memory_order_release);

  mtx_unlock(&s->allt.lock);
  return 0;
}


// TCTX_SIZE: bytes needed on stack to store a task's persistent register values
#define TCTX_SIZE ( \
  ((RSM_NREGS - RSM_NTMPREGS) * sizeof(u64)) + \
  ((RSM_NREGS - RSM_NTMPREGS) * sizeof(double)) )


// tctx layout:
//   [byte 0] F{RSM_NTMPREGS} F{RSM_NTMPREGS+1} F{...} F{RSM_NREGS-1} ...
//        ... R{RSM_NTMPREGS} R{RSM_NTMPREGS+1} R{...} R{RSM_NREGS-2}

static void m_switchtask(M* m, T* nullable t) {
  if (m->currt) {
    // save context
    panic("TODO: save context");
  }
  m->currt = t;
  if (t) {
    // restore context
    // tctx is stored on the task stack at address SP-sizeof(tctx)
    tctx* ctx = t->sp;
    for (usize i = 0; i < countof(ctx->fregs); i++)
      m->fregs[i + RSM_NTMPREGS] = ctx->fregs[i];
    for (usize i = 0; i < countof(ctx->iregs); i++)
      m->iregs[i + RSM_NTMPREGS] = ctx->iregs[i];
    u64 sp = (u64)(uintptr)t->sp;
    dlog("TODO: translate vm stack address to host address");
    m->iregs[RSM_MAX_REG] = sp;
  }
}


static T* nullable s_alloctask(S* s, usize stacksize) {
  // task memory layout:
  //    ┌───────────┬───────────┐
  //    │   ← stack │ T struct  │
  //    ├───────────┼───────────┘
  // stacktop   stackbase
  //            initial SP

  usize alignment = MAX(_Alignof(T), STK_ALIGN);
  stacksize = ALIGN2(stacksize, alignment);

  rmem_t stacktop = rmem_alloc_aligned(s->memalloc, stacksize + sizeof(T), alignment);
  if UNLIKELY(!stacktop.p)
    return NULL;
  memset(stacktop.p, 0, stacktop.size);
  assertf(IS_ALIGN2((uintptr)stacktop.p, (uintptr)alignment), "bug in rmem_alloc");

  T* t = (T*)((uintptr)stacktop.p + stacktop.size - sizeof(T));
  t->s = s;
  t->stacktop = (uintptr)stacktop.p;
  t->sp = TASK_STACKBASE(t);

  // initialize tctx
  tctx* ctx = (tctx*)(t->sp - sizeof(tctx));
  ctx->iregs[RSM_MAX_REG - RSM_NTMPREGS - 1] = (u64)(uintptr)t; // CTX

  // dlog("s_alloctask stack %p … %p (%lu B)",
  //   stacktop.p, t->sp, (usize)((uintptr)t->sp - (uintptr)stacktop.p));

  return t;
}


// s_spawn creates a new T running fn with argsize bytes of arguments.
// stacksize is a requested minimum number of bytes to allocate for its stack. A stacksize
// of 0 means to allocate a stack of default standard size.
// Put it on the queue of T's waiting to run.
// The compiler turns a go statement into a call to this.
static rerr_t s_spawn(S* s, const rin_t* iv, usize ic, const void* rodata, usize pc) {
  T* t = t_get();
  M* m = t->m;
  rerr_t err = 0;

  // disable preemption
  m_acquire(m);

  // create main task
  T* newtask = s_alloctask(s, STK_MIN);
  if (!newtask) {
    err = rerr_nomem;
    goto onerr;
  }

  // setup program instruction array, program counter and task ID
  newtask->pc = pc;
  newtask->instrc = ic;
  newtask->instrv = iv;
  newtask->rodata = rodata;
  newtask->id = AtomicAdd(&s->tidgen, 1, memory_order_acquire);

  // add the task to the scheduler
  t_setstatus(newtask, T_DEAD);
  if ((err = s_allt_add(s, newtask)))
    goto onerr;

  // set status to runnable
  t_setstatus(newtask, T_RUNNABLE);

  // get P associated with current M
  P* p = assertnotnull(m->p);

  // re-enable preemption
  m_release(m);

  // add newtask to run queue
  p_runqput(p, newtask, /*runnext*/true);

  // TODO // wake P (unless we are spawning the main task)
  // if (s->mainstarted)
  //   p_wake();

  return 0;

onerr:
  m_release(m); // re-enable preemption
  return err;
}


static rerr_t m_init(M* m, u64 id) {
  m->id = id;

  // configure main task
  m->t0.m = m;
  m->t0.status = T_RUNNING;
  m->t0.id = AtomicAdd(&m->s->tidgen, 1, memory_order_acquire);

  return 0;
}


// schedule performs one pass of scheduling: find a runnable coroutine and execute it.
// Never returns.
static void schedule() {
  T* parent_t = t_get();
  M* m = parent_t->m;
  P* p = m->p;

  p->preempt = false;
  bool inheritTime = false;

  trace("try p_runqget");
  T* t = p_runqget(p, &inheritTime);
  // We can see t != NULL here even if the M is spinning,
  // if checkTimers added a local goroutine via goready.
  if (t)
    trace("p_runqget found T%llu", t->id);

  // This thread is going to run a coroutine and is not spinning anymore,
  // so if it was marked as spinning we need to reset it now and potentially
  // start a new spinning M.
  if (m->spinning)
    panic("TODO: m_resetspinning(m)");

  // TODO: lockedm
  // if (t->lockedm != NULL) {
  //   // Hands off own p to the locked m, then blocks waiting for a new p
  //   panic("TODO: startlockedm(t) ; goto top");
  // }

  // save any current task's state and restore t's state, setting m->currt
  m_switchtask(m, t);

  // Assign t->m before entering T_RUNNING so running Ts have an M
  t->m = m;
  t_casstatus(t, T_RUNNABLE, T_RUNNING);
  t->waitsince = 0;
  t->parent = parent_t;

  p->schedtick += (u32)inheritTime;

  _g_t = t;

  t_vmexec(t, m->iregs, t->instrv, t->pc);
}


// m_start is the entry-point for new M's.
// Basically the "OS thread main function."
// M doesn't have a P yet.
NOINLINE static rerr_t m_start(M* m) {
  assert(t_get() == &m->t0);
  // TODO: loop
  schedule();
  return 0;
}


static void p_init(P* p, u32 id) {
  p->id = id;
  p->status = P_IDLE;
}


static void p_acquire(P* p, M* m) {
  // associates P with the current M
  assertf(p->m == NULL, "P in use with other M");
  assertf(m->p == NULL, "M in use with other P");
  assert(p->status == P_IDLE);
  m->p = p;
  p->m = m;
  p->status = P_RUNNING;
}

// static void p_release(P* p) {
//   // disassociates P from its M
//   assertnotnull(p->m);
//   p->m->p = NULL;
//   p->m = NULL;
//   p->status = P_IDLE;
// }


static rerr_t s_init(S* s) {
  // clear allt struct
  memset(&s->allt, 0, sizeof(s->allt));
  mtx_init(&s->allt.lock, mtx_plain);

  // initialize main M (id=0) on current OS thread
  s->midgen = 1;
  s->maxmcount = 1000;
  s->m0.s = s;
  rerr_t err = m_init(&s->m0, 0);
  if UNLIKELY(err)
    return err;

  // set "current task" to M's root task
  _g_t = &s->m0.t0;

  // create processors
  u32 nprocs = 2;
  assert(nprocs <= S_MAXPROCS);
  s->nprocs = nprocs;
  trace("s.nprocs=%u", nprocs);
  for (u32 pid = 0; pid < nprocs; pid++) {
    P* p = rmem_alloct(s->memalloc, P);
    if (!p) // TODO: free other P's we allocated
      return rerr_nomem;
    memset(p, 0, sizeof(P));
    p_init(p, pid);
    s->allp[pid] = p;
  }

  // associate P0 with M0
  P* p = s->allp[0];
  p->status = P_IDLE;
  p_acquire(p, &s->m0); // associate P and M (p->m=m, m->p=p, p.status=P_RUNNING)

  return err;
}


rerr_t rvm_main(rvm_t* vm, rrom_t* rom) {
  S* s = (S*)vm;

  // initialize the scheduler
  rerr_t err = s_init(s);
  if (err)
    return err;

  // load ROM
  if (!rom->code) {
    rerr_t err = rsm_loadrom(rom);
    if (err == 0 && (rom->code == NULL || rom->codelen == 0))
      err = rerr_invalid; // ROM without (or with empty) code section
    return err;
  }

  // create main task
  err = s_spawn(s, rom->code, rom->codelen, rom->data, /*pc=*/0);
  if (err)
    return err;

  // save pointer to main task
  s->t0 = s->m0.p->runnext;

  // enter scheduler loop in M0
  return m_start(&s->m0);
}


rvm_t* nullable rvm_create(rmemalloc_t* ma) {
  S* s = rmem_alloct(ma, S);
  if (!s)
    return NULL;
  memset(s, 0, sizeof(S));

  // allocate heap
  // TODO: vmem
  rmem_t heapmem = rmem_alloc(ma, 1024 * PAGE_SIZE); // 4MB @ 4k page size
  if UNLIKELY(!heapmem.p) {
    rmem_free(ma, RMEM(s, sizeof(S)));
    return NULL;
  }
  s->heapbase = heapmem.p;
  s->heapsize = heapmem.size;
  s->memalloc = ma;
  return (rvm_t*)s;
}


void rvm_dispose(rvm_t* vm) {
  S* s = (S*)vm;
  rmem_free(s->memalloc, RMEM(s->heapbase, s->heapsize));
  rmem_freet(s->memalloc, s);
}

