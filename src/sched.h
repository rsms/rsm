// process scheduler
// SPDX-License-Identifier: Apache-2.0
#pragma once
#include "vm.h"
RSM_ASSUME_NONNULL_BEGIN

// EXE_TOP_ADDR is the end of the virtual memory-address space,
// where exeinfo_t ends.
#define EXE_TOP_ADDR  ALIGN2_FLOOR_X(VM_ADDR_MAX, 8)

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
typedef struct M M; // Machine, an OS thread (TODO: rename. C; CPU or Core)
typedef struct P P; // Processor, an execution resource required to execute a T
typedef u8 tstatus_t; // Task status
typedef u8 PStatus; // Processor status

struct T {
  u64         id;
  M* nullable m;         // attached M
  T* nullable parent;    // task that spawned this task
  T* nullable schedlink; // next task to be scheduled

  usize        pc;     // program counter; next instruction = iv[pc]
  usize        instrc; // instruction count
  const rin_t* instrv; // instruction array
  const void*  rodata; // global read-only data (from ROM)

  u64 sp;       // saved SP register value used by m_switchtask
  u64 stacktop; // end of stack (lowest valid stack address)

  u64                waitsince; // approx time when the T became blocked
  _Atomic(tstatus_t) status;
};

struct M {
  u32         id;
  rsched_t*   s;        // parent scheduler
  P* nullable p;        // attached p for executing Ts (NULL if not executing)
  T* nullable currt;    // current running task
  u32         locks;    // number of logical locks held by Ts to this M
  bool        spinning; // m is out of work and is actively looking for work
  bool        blocked;  // m is blocked on a note

  // register values
  u64    iregs[RSM_NREGS];
  double fregs[RSM_NREGS];

  // virtual memory cache for read-only and read-write pages
  vm_cache_t vm_cache[3]; // 0=r, 1=w, 2=wr
};

struct P {
  u32         id;        // corresponds to offset in s.allp
  u32         schedtick; // incremented on every scheduler call
  PStatus     status;
  bool        preempt;   // this P should be enter the scheduler ASAP
  rsched_t*   s;         // parent scheduler
  M* nullable m;         // associated m (NULL when P is idle)
  P* nullable nextp;     // next P in list (for rsched_t.pidle)

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
  _Atomic(T* nullable) runnext;

  // timers
  _Atomic(u32) ntimers; // Number of timers in P's heap (writes uses timers_lock)
  // timers_lock is the lock for timers.
  // We normally access the timers while running on this P,
  // but the scheduler can also do it from a different P.
  RHMutex timers_lock;
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

typedef struct {
  _Atomic(usize) bits[S_MAXPROCS / sizeof(usize) / 8];
} P_bitset_t;

struct rsched_ {
  rmachine_t*  machine;   // host machine
  u32          maxmcount; // limit number of M's created (ie. OS thread limit)

  _Atomic(u64) tidgen; // T.id generator
  _Atomic(u32) midgen; // M.id generator

  RHMutex lock; // protects access to allp, pidle and runq

  P*           allp[S_MAXPROCS]; // all live P's (managed by sched_procresize)
  _Atomic(u32) nprocs;           // max active Ps (num valid P's in allp; maxprocs)
  P* nullable  pidle;            // list of idle P's
  _Atomic(u32) npidle;           // (note: writes use lock)
  _Atomic(i32) nmspinning;       // number of spinning M's

  // pidle_mask is a bitmask of Ps in PIdle list, one bit per P in allp.
  // Reads and writes must be atomic. Length may change at safe points.
  //
  // Each P must update only its own bit. In order to maintain
  // consistency, a P going idle must set the idle mask simultaneously with
  // updates to the idle P list under the sched.lock, otherwise a racing
  // s_pidle_get may clear the mask before s_pidle_put sets the mask,
  // corrupting the bitmap.
  P_bitset_t pidle_mask;

  // timerp_mask is a bitmask of P's that may have a timer, one bit per P in allp.
  // Reads and writes must be atomic. Length may change at safe points.
  P_bitset_t timerp_mask;

  // allt holds all live T's
  struct {
    mtx_t        lock;
    _Atomic(T**) ptr; // atomic for reading; lock used for writing
    _Atomic(u32) len; // atomic for reading; lock used for writing
    u32          cap; // capacity of ptr array
  } allt;

  // global run queue (when a task is resumed without a P)
  // T's are linked via T.schedlink
  struct {
    _Atomic(T*)  head;
    _Atomic(T*)  tail;
    _Atomic(u32) len;
  } runq;

  vm_pagedir_t vm_pagedir; // virtual memory page directory

  M m0; // main M (bound to the OS thread which rvm_main is called on)
  P p0; // first P
};


// exeinfo_t is mapped at the end of the virtual memory-address space,
// at the bottom of the stack. It describes the host system.
// Thus, in a program's main function, exeinfo_t is at SP.
typedef struct {
  u64 heap_vaddr; // start of heap, end of data (SP+0)
} exeinfo_t;


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

// TODO:
// change the concepts here
// - "suspend" (alt "park"): takes a task out of running state (disassoc. M & P from T.)
//   Must be explicitly resumed with a call to "resume" (alt "unpark")
// - "release_p" (alt "syscall_begin"): releases a task's P.
//   Task is expected to not need a P until a call to "retain_p" (alt "syscall_end").
//   "retain_p" would return to signal if the task can immediately continue execution
//   (ie it was assigned a P and has is the next task scheduled for P),
//   or if no P was available and the task was put on a run queue.
//   In the latter case, T might have been added to either the run queue of P
//   or to the global (scheduler) run queue.

void task_park(T* t, tstatus_t);
// void task_unpark(T* t, tstatus_t);

// task_disable releases the P associated with the task,
// making that P available for use by other tasks waiting to run.
// After this call the task can not be executed until task_enable is called.
void task_disable(T*);

// task_enable attempts to associate an available P with the task.
// priority>0 means "important".
// Returns true if the task should resume execution immediately,
// otherwise (when false is returend), the task has been put on a run queue
// and the caller should not continue execution of the task.
bool task_enable(T*, int priority);


// SCHED_TRACE: when defined, verbose log tracing on stderr is enabled.
// The value is used as a prefix for log messages.
#if defined(SCHED_TRACE) && !SCHED_TRACE
  #undef SCHED_TRACE
#elif DEBUG
  #define SCHED_TRACE "\e[1m▍\e[0m "
#endif

// schedtrace(const char* fmt, ...) -- debug tracing
#if defined(SCHED_TRACE) && !defined(RSM_NO_LIBC)
  void _schedtrace(const char* fmt, ...);
  #define schedtrace(fmt, ...) \
    _schedtrace("\e[1;36m%-15s\e[39m " fmt "\e[0m\n", __FUNCTION__, ##__VA_ARGS__)
#else
  #define schedtrace(...) do{}while(0)
#endif


RSM_ASSUME_NONNULL_END
