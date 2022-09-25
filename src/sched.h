// process scheduler
// SPDX-License-Identifier: Apache-2.0
#pragma once
#include "vm.h"
RSM_ASSUME_NONNULL_BEGIN

// SCHED_TRACE: when defined, verbose log tracing on stderr is enabled.
#if !defined(SCHED_TRACE) && DEBUG
  #define SCHED_TRACE 1
#endif
// The value decides granularity of logging:
// 0 (or undefined) - tracing disabled
// 1  - key aspects of scheduling
// 2  - detailed aspects of scheduling
// >2 - very detailed trace

// stack constants
#define STK_ALIGN    8lu                // stack alignment
#define STK_MIN      ((usize)PAGE_SIZE) // min backing memory allocated for stack
#define STK_DEFAULT  1lu * MiB          // default stack size (virtual)

// INTERPRET_USE_JUMPTABLE: define to enable use of jump table in the interpreter.
// Requires that the compiler supports taking the address of labels, ie. "&&label".
#define INTERPRET_USE_JUMPTABLE

// S_MAXPROCS is the upper limit of concurrent P's; the effective CPU parallelism limit.
// There are no fundamental restrictions on the value. Must be pow2.
#define S_MAXPROCS  256
static_assert(IS_POW2_X(S_MAXPROCS), "");

// P_RUNQSIZE is the size of P.runq. Must be power-of-two (2^N)
#define P_RUNQSIZE  256 // 256 is the value Go 1.16 uses
static_assert(IS_POW2_X(P_RUNQSIZE), "");

// Main scheduling concepts:
// - T for Task, a coroutine task
// - M for Machine, an OS thread
// - P for Processor, an execution resource required to execute a T
// M must have an associated P to execute T, however
// a M can be blocked or in a syscall without an associated P.
typedef struct rsched_ rsched_t;
typedef struct T T; // Task, a coroutine task
typedef struct M M; // Machine, an OS thread
typedef struct P P; // Processor, an execution resource required to execute a T
typedef u8 tstatus_t; // Task status
typedef u8 pstatus_t; // Processor status

// mnote_t: sleep and wakeup on one-time events
typedef struct {
  // key holds:
  // a) NULL when unused.
  // b) pointer to some M.
  // c) opaque value indicating locked state.
  _Atomic(uintptr) key; // must be initialized to 0
} mnote_t;

typedef struct {
  T* nullable head;
  u32         len;
} tlist_t;

typedef struct {
  _Atomic(usize) bits[S_MAXPROCS / sizeof(usize) / 8];
} pbitset_t;

struct T {
  u64         id;
  T* nullable parent;    // task that spawned this task
  T* nullable schedlink; // next task to be scheduled (used by runq, freet)
  M* nullable m;         // attached M

  usize        pc;     // program counter; next instruction = iv[pc]
  usize        instrc; // instruction count
  const rin_t* instrv; // instruction array

  u64 sp;       // saved SP register value used by m_switchtask
  u64 stack_lo; // top of stack (lowest valid stack address)
  u64 stack_hi; // bottom of stack (highest valid stack address)

  u64                waitsince; // approx time when the T became blocked
  _Atomic(tstatus_t) status;
  u32                nsplitstack; // number of stack splits
};

struct M {
  rsched_t*   s;        // parent scheduler
  P* nullable p;        // attached p for executing Ts (NULL if not executing)
  P* nullable oldp;     // P that was attached before executing a syscall
  P* nullable nextp;    // P to attach
  T* nullable currt;    // current running task
  M* nullable nextm;    // next M in list (for s.idlem and s.freem)
  u32         id;
  u32         locks;    // number of logical locks held by Ts to this M
  bool        spinning; // m is out of work and is actively looking for work
  bool        blocked;  // m is blocked on a note
  mnote_t     park;     // park notification
  sema_t      parksema; // park notification semaphore

  // register values
  u64    iregs[RSM_NREGS];
  double fregs[RSM_NREGS];

  // virtual memory cache for read-only and read-write pages.
  // index is offset by 1, since "no permissions" is never cached.
  vm_cache_t vmcache[VM_PERM_MAX]; // 0=r, 1=w, 2=rw
};

struct P {
  u32                id;        // corresponds to offset in s.allp
  u32                schedtick; // incremented on every scheduler call
  _Atomic(pstatus_t) status;    // status
  bool               preempt;   // this P should enter scheduling ASAP
  rsched_t*          s;         // parent scheduler
  M* nullable        m;         // associated m (NULL when P is idle)
  P* nullable        nextp;     // next P in list (for s.idlep)

  // runq -- queue of runnable tasks, accessed without lock
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
  _Atomic(T* nullable) runnext;

  // freet is a list of unused T's (status==T_DEAD)
  tlist_t freet;

  // timers
  _Atomic(u32) ntimers; // Number of timers in P's heap (writes uses timers_lock)
  // timers_lock is the lock for timers.
  // We normally access the timers while running on this P,
  // but the scheduler can also do it from a different P.
  mutex_t timers_lock;
  // timer0_when is the "when" field of the first entry on the timer heap.
  // This is updated using atomic functions. 0 if the timer heap is empty.
  _Atomic(u64) timer0_when;
  // timer_modified_earliest is the earliest known next_when field of a timer with
  // TIMER_MODIFIED_EARLIER status. Because the timer may have been modified again,
  // there need not be any timer with this value.
  // This is updated using atomic functions.
  // This is 0 if there are no TIMER_MODIFIED_EARLIER timers.
  _Atomic(u64) timer_modified_earliest;
};

struct rsched_ {
  rmachine_t*  machine;      // host machine
  _Atomic(u64) tidgen;       // T.id generator
  mutex_t      lock;         // protects access to idlem, idlep, allp, runq
  vm_map_t     vm_map;       // virtual memory page directory
  bool         main_started; // true when main task has started

  // M's
  M* nullable  idlem;        // idle m's waiting for work
  _Atomic(u32) nidlem;       // number of idle m's waiting for work
  u32          nidlemlocked; // number of locked m's waiting for work
  _Atomic(i32) nmspinning;   // number of spinning M's
  _Atomic(u32) midgen;       // M.id generator
  u32          maxmcount;    // limit number of M's created (ie. OS thread limit)
  u64          nmfreed;      // cumulative number of freed m's
  M* nullable  freem;        // list of M's to be freed when their m.exited is set

  // P's
  P*           allp[S_MAXPROCS];     // all live P's (managed by sched_procresize)
  _Atomic(u32) nprocs;               // max active Ps (num valid P's in allp; maxprocs)
  P* nullable  idlep;                // list of idle P's
  _Atomic(u32) nidlep;               // (note: writes use lock)
  u32          stealord[S_MAXPROCS]; // steal order of P's in allp
  u32          stealordlen;          // entries at stealord

  // idlep_mask is a bitmask of Ps in PIdle list, one bit per P in allp.
  // Reads and writes must be atomic. Length may change at safe points.
  //
  // Each P must update only its own bit. In order to maintain
  // consistency, a P going idle must set the idle mask simultaneously with
  // updates to the idle P list under the sched.lock, otherwise a racing
  // s_idlep_get may clear the mask before s_idlep_put sets the mask,
  // corrupting the bitmap.
  pbitset_t idlep_mask;

  // timerp_mask is a bitmask of P's that may have a timer, one bit per P in allp.
  // Reads and writes must be atomic. Length may change at safe points.
  pbitset_t timerp_mask;

  // exec_lock serializes exec and clone to avoid bugs or unspecified behaviour
  // around exec'ing while creating/disposing threads.
  rwmutex_t exec_lock;

  // allocm_lock is locked for read when creating new Ms in allocm and their
  // addition to allm. Thus acquiring this lock for write blocks the
  // creation of new Ms.
  rwmutex_t allocm_lock;

  // allt holds all T's (any status, including T_DEAD)
  struct {
    rwmutex_t    lock;
    _Atomic(T**) ptr;
    _Atomic(u32) len;
    u32          cap;
  } allt;

  // global run queue (when a task is resumed without a P.)
  // T's are linked via T.schedlink
  struct {
    _Atomic(T*)  head;
    _Atomic(T*)  tail;
    _Atomic(u32) len;
  } runq;

  // freet is a list of unused T's (status==T_DEAD)
  tlist_t freet;
  mutex_t freet_lock;

  M m0; // main M (bound to the OS thread which rvm_main is called on)
  P p0; // first P
};


// tctx_t: task execution context used for task switching, stored on stack
typedef struct {
  double fregs[RSM_NREGS - RSM_NTMPREGS];
  u64    iregs[RSM_NREGS - RSM_NTMPREGS - 1]; // does not include SP
} tctx_t;


enum tstatus {
  // T_IDLE: task was just allocated and has not yet been initialized
  T_IDLE = 0,

  // T_RUNNABLE: task is on a run queue.
  // It is not currently executing user code.
  // The stack is not owned.
  T_RUNNABLE,

  // T_RUNNING: task may execute user code.
  // The stack is owned by this task.
  // It is not on a run queue.
  // It is assigned an M and a P (t.m and t.m.p are valid).
  T_RUNNING,

  // T_SYSCALL: task is executing a system call.
  // It is not executing user code.
  // The stack is owned by this task.
  // It is not on a run queue.
  // It is assigned an M.
  T_SYSCALL,

  // T_DEAD: task is unused.
  // It may be just exited, on a free list, or just being initialized.
  // It is not executing user code.
  // It may or may not have a stack allocated.
  // The T and its stack (if any) are owned by the M that is exiting the T
  // or that obtained the T from the free list.
  T_DEAD,
};


rerr_t rsched_init(rsched_t* s, rmachine_t* machine);
void rsched_dispose(rsched_t* s);

rerr_t rsched_execrom(rsched_t* s, rrom_t* rom);

// return pc
usize rsched_eval(T* t, u64* iregs, const rin_t* inv, usize pc);

// task_spawn spawns a new task executing code at pc newtask_pc.
// t is the task that calls spawns, not the task being spawned.
// args hold the initial arguments (R0...R7) for the new task.
// Returns the new spawned task's ID, or <0 on error (value is a rerr_t).
i64 task_spawn(T* t, usize newtask_pc, const u64 args[RSM_NARGREGS]);

// task_exit is called by the interpreter when a task's main function exits
void task_exit(T*);

// enter_syscall releases the P associated with the task,
// making that P available for use by other tasks waiting to run.
// After this call the task can not be executed until exit_syscall is called.
void enter_syscall(T*);

// exit_syscall attempts to associate an available P with the task.
// priority>0 means "important".
// Returns true if the task should resume execution immediately,
// otherwise (when false is returend), the task has been put on a run queue
// and the caller should not continue execution of the task.
bool exit_syscall(T*, int priority);

// m_spawn_osthread creates & starts an OS thread, calling mainf on the new thread.
// Returns the OS-specific thread ID, or 0 on failure.
uintptr m_spawn_osthread(M* m, rerr_t(*mainf)(M*));

// m_vm_cache accesses the vm cache for perm on M
inline static vm_cache_t* m_vm_cache(M* m, vm_perm_t perm) {
  assertf(perm > 0 && (perm-1) < (vm_perm_t)countof(m->vmcache), "%u", perm);
  return &m->vmcache[perm-1];
}


// schedtrace1(const char* fmt, ...) -- debug tracing level 1
// schedtrace2(const char* fmt, ...) -- debug tracing level 2
// schedtrace3(const char* fmt, ...) -- debug tracing level 3
#if defined(SCHED_TRACE) && !SCHED_TRACE
  #undef SCHED_TRACE
#endif
#if defined(SCHED_TRACE) && !defined(RSM_NO_LIBC)
  void _schedtrace(int level, const char* nullable fn, const char* fmt, ...)
    ATTR_FORMAT(printf, 3, 4);
  #define schedtrace1(fmt, args...) _schedtrace(1, __FUNCTION__, fmt, ##args)
  #if SCHED_TRACE > 1
    #define schedtrace2(fmt, args...) _schedtrace(2, __FUNCTION__, fmt, ##args)
  #else
    #define schedtrace2(...) ((void)0)
  #endif
  #if SCHED_TRACE > 2
    #define schedtrace3(fmt, args...) _schedtrace(3, __FUNCTION__, fmt, ##args)
  #else
    #define schedtrace3(...) ((void)0)
  #endif
#else
  #define schedtrace1(...) ((void)0)
  #define schedtrace2(...) ((void)0)
  #define schedtrace3(...) ((void)0)
#endif


RSM_ASSUME_NONNULL_END
