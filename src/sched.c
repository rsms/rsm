// virtual machine
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "abuf.h"
#include "thread.h"
#include "sched.h"
#include "machine.h"

// tctx
// #define TCTX_ALIGN       PAGE_SIZE
// #define TCTX_ALIGN       CEIL_POW2_X(sizeof(tctx_t))
#define TCTX_ALIGN       _Alignof(tctx_t)
#define TCTX_ALIGN_BITS  ILOG2(TCTX_ALIGN)

// u64 TCTX_VADDR(u64 sp) calculates the address of tctx given a stack pointer
#define TCTX_VADDR(sp)  ALIGN2_FLOOR((u64)(sp) - (u64)sizeof(tctx_t), TCTX_ALIGN)

// stack constants
#define STK_ALIGN    8lu       // stack alignment
#define STK_MIN      2048lu    // minium stack size (backing memory always allocated)
#define STK_DEFAULT  1lu * MiB // default stack size (virtual)
static_assert(STK_MIN >= ALIGN2_X(sizeof(tctx_t), TCTX_ALIGN), "STK_MIN too small");
static_assert(STK_MIN % STK_ALIGN == 0, "STK_MIN not aligned to STK_ALIGN");
static_assert(STK_DEFAULT % STK_ALIGN == 0, "STK_DEFAULT not aligned to STK_ALIGN");

// HEAP_ALIGN: alignment of heap memory address
#define HEAP_ALIGN   8

// HEAP_INITSIZE: minimum initial heap size. Must be multiple of PAGE_SIZE.
#define HEAP_INITSIZE  (1lu * MiB)
static_assert(IS_ALIGN2(HEAP_INITSIZE, PAGE_SIZE), "");

// MAIN_RET_PC: special PC value representing the main return address
#define MAIN_RET_PC  USIZE_MAX

// EXEINFO_NPAGES pages used for exeinfo_t
#define EXEINFO_NPAGES  IDIV_CEIL_X(sizeof(exeinfo_t), PAGE_SIZE)

// STACK_VADDR is the virtual address of the base of the main stack
#define STACK_VADDR \
  ALIGN2_FLOOR_X( \
    EXE_TOP_ADDR - sizeof(exeinfo_t), MAX_X(_Alignof(exeinfo_t), STK_ALIGN) )

// DATA_VADDR is the virtual address of the data section
#define DATA_VADDR  VM_ADDR_MIN

// PROLOGUE_LEN: number of instructions in the main prologue.
#define PROLOGUE_LEN 1

// PROLOGUE_PC is the instruction address of the prologue
#define PROLOGUE_PC  0

//———————————————————————————————————————————————————————————————————————————————————
// vm v2
//
// Memory thoughts:
// - stack per coroutine
// - shared heap
// - ROM data is shared (like a second heap)
// - TODO: ROM instructions assume its global data starts at address 0x0
//         Either a "linker" needs to rewrite all data addresses when loading a ROM,
//         or we need memory translation per ROM (ie. depending on instruction origin.)


enum PStatus {
  P_IDLE = 0,
  P_RUNNING, // Only this P is allowed to change from P_RUNNING
  P_SYSCALL,
  P_DEAD,
};


// stackbase for a task
#define TASK_STACKBASE(t) ((void*)ALIGN2_FLOOR((uintptr)(t),STK_ALIGN))


#define trace schedtrace

#if defined(SCHED_TRACE) && !defined(RSM_NO_LIBC)
  #include <stdio.h>

  // thread-local storage of the current M on current OS thread, for tracing
  static _Thread_local M* _g_current_m = NULL;
  #define TRACE_SET_CURRENT_M(m)  do{ _g_current_m = (m); }while(0)

  // "Mn Pn Tn  function  message"
  void _schedtrace(const char* fmt, ...) {
    M* m = assertnotnull(_g_current_m);
    T* t = m->currt;
    P* p = m->p;
    FILE* fp = stderr;
    flockfile(fp);
    const char* prefix = SCHED_TRACE;
    fwrite(prefix, strlen(prefix), 1, fp);
    char color = (char)('1' + (m->id % 6));
    fprintf(fp, "\e[1;3%cmM%u\e[0m ", color, m->id);
    if (p) {
      color = (char)('1' + (p->id % 6));
      fprintf(fp, "\e[1;3%cmP%u\e[0m ", color, p->id);
    } else {
      fprintf(fp, "\e[1;2mP-\e[0m ");
    }
    if (t) {
      color = (char)('1' + (t->id % 6));
      fprintf(fp, "\e[1;3%cmT%-2llu\e[0m ", color, t->id);
    } else {
      fprintf(fp, "\e[1;2mT- \e[0m ");
    }
    va_list ap;
    va_start(ap, fmt);
    vfprintf(fp, fmt, ap);
    va_end(ap);
    funlockfile(fp);
  }
#endif


#if DEBUG
UNUSED static const char* tstatus_str(tstatus_t status) {
  switch ((enum tstatus)status) {
    case T_IDLE:     return "T_IDLE";
    case T_RUNNABLE: return "T_RUNNABLE";
    case T_RUNNING:  return "T_RUNNING";
    case T_SYSCALL:  return "T_SYSCALL";
    case T_DEAD:     return "T_DEAD";
  }
  return "?";
}
#endif


// _Atomic(usize)* pbits_ptr(P_bitset_t* bs, u32 bit)
#define pbits_ptr(bs, bit) \
  ( &(bs)->bits[(bit) / sizeof(usize)] )

// pbits_isset returns true if bit is set
static bool pbits_isset(P_bitset_t* bs, u32 bit) {
  usize mask = (usize)1lu << (bit % sizeof(usize));
  return (AtomicLoad(pbits_ptr(bs, bit), memory_order_relaxed) & mask) != 0;
}

// pbits_set sets bit (to 1)
static void pbits_set(P_bitset_t* bs, u32 bit) {
  usize mask = (usize)1lu << (bit % sizeof(usize));
  AtomicOr(pbits_ptr(bs, bit), mask, memory_order_release);
}

// pbits_clear clears bit (sets it to 0)
static void pbits_clear(P_bitset_t* bs, u32 bit) {
  usize mask = (usize)1lu << (bit % sizeof(usize));
  AtomicAnd(pbits_ptr(bs, bit), ~mask, memory_order_release);
}


static inline void m_acquire(M* m) { m->locks++; }
static inline void m_release(M* m) { m->locks--; }


// static void t_free(T* task) {
//   usize memsize = (usize)(((uintptr)task + (uintptr)sizeof(T)) - task->stacktop);
//   rmem_free(task->s->memalloc, (void*)task->stacktop, memsize);
// }


// t_readstatus returns the value of t->status.
// On many archs, including x86, this is just a plain load.
static inline tstatus_t t_readstatus(T* t) {
  return AtomicLoad(&t->status, memory_order_relaxed);
}

// t_setstatus updates t->status using using an atomic store.
// This is only used when setting up new or recycled Ts.
// To change T.status for an existing T, use t_casstatus instead.
static void t_setstatus(T* t, tstatus_t newval) {
  AtomicStore(&t->status, newval, memory_order_relaxed);
}


// t_casstatus updates t->status using compare-and-swap.
// It blocks until it succeeds (until t->status==oldval.)
static void t_casstatus(T* t, tstatus_t oldval, tstatus_t newval) {
  assert(oldval != newval);

  // use a temporary variable for oldval since AtomicCAS will store the current value
  // to it on failure, but we want to swap values only when the current value is
  // explicitly oldval.
  tstatus_t oldvaltmp = oldval;

  // // See https://golang.org/cl/21503 for justification of the yield delay.
  // const u64 yieldDelay = 5 * 1000;
  // u64 nextYield = 0;

  for (int i = 0; !AtomicCASRelaxed(&t->status, &oldvaltmp, newval); i++) {
    // Note: on failure, when we get here, oldval has been updated.
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
static bool p_runq_put_slow(P* p, T* t, u32 head, u32 tail) {
  panic("TODO");
  return false;
}


// p_runq_put tries to put t on the local runnable queue.
// If runnext if false, runqput adds T to the tail of the runnable queue.
// If runnext is true, runqput puts T in the p.runnext slot.
// If the run queue is full, runnext puts T on the global queue.
// Executed only by the owner P.
static void p_runq_put(P* p, T* t, bool runnext) {
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
    if (p_runq_put_slow(p, tp, head, tail))
      return;
    // the queue is not full, now the put above must succeed. retry...
  }
}


// Get T from local runnable queue.
// If inherit_time is true, T should inherit the remaining time in the current time slice.
// Otherwise, it should start a new time slice.
// Executed only by the owner P.
static T* nullable p_runq_get(P* p, bool* inherit_time) {
  // If there's a runnext, it's the next G to run.
  while (1) {
    T* next = p->runnext;
    if (next == NULL)
      break;
    if (AtomicCASAcqRel(&p->runnext, &next, NULL)) {
      *inherit_time = true;
      return next;
    }
  }

  trace("no runnext; trying dequeue p->runq");
  *inherit_time = false;

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


static rerr_t sched_allt_add(rsched_t* s, T* t) {
  assert(t_readstatus(t) != T_IDLE);

  mtx_lock(&s->allt.lock);

  if (s->allt.len == s->allt.cap) {
    rmem_t mem = { s->allt.ptr, (usize)s->allt.cap * sizeof(void*) };
    bool ok = rmem_resize(s->machine->malloc, &mem, (s->allt.cap + 64) * sizeof(void*));
    if UNLIKELY(!ok) {
      mtx_unlock(&s->allt.lock);
      return rerr_nomem;
    }
    s->allt.cap = mem.size / sizeof(void*);
    AtomicStore(&s->allt.ptr, mem.p, memory_order_release);
  }

  trace("set s.allt[%u] = T%llu", s->allt.len, t->id);
  s->allt.ptr[s->allt.len++] = t;
  AtomicStore(&s->allt.len, s->allt.len, memory_order_release);

  mtx_unlock(&s->allt.lock);
  return 0;
}


// tctx_vaddr calculates the address of a tctx_t given a stack pointer
static u64 tctx_vaddr(u64 sp) {
  u64 vaddr = ALIGN2_FLOOR((u64)(sp) - (u64)sizeof(tctx_t), TCTX_ALIGN);

  if LIKELY(VM_PAGE_ADDR(vaddr) == VM_PAGE_ADDR(vaddr + sizeof(tctx_t)))
    return vaddr;

  // the ctx address spans pages; store it at the end of the first page
  return ALIGN2_FLOOR(vaddr + sizeof(tctx_t), PAGE_SIZE) - sizeof(tctx_t);
}


static tctx_t* nullable m_get_tctx(M* m, T* t) {
  // A suspended task's tctx is stored on the task's stack, just above SP
  u64 ctx_vaddr = tctx_vaddr(t->sp);
  assertf(ctx_vaddr >= t->stacktop, "%llx, %llx", ctx_vaddr, t->stacktop);

  // get backing memory
  vm_cache_t* vm_cache = &m->vm_cache[VM_PERM_RW];
  uintptr haddr = VM_TRANSLATE(vm_cache, &m->s->vm_pagedir, ctx_vaddr, TCTX_ALIGN);

  //dlog("load ctx from stack at vaddr 0x%llx (haddr %p)", ctx_vaddr, (void*)haddr);
  return (tctx_t*)haddr;
}


static void t_save_ctx(T* t) {
  M* m = assertnotnull(t->m);
  tctx_t* ctx = assertnotnull(m_get_tctx(m, t));
  for (usize i = 0; i < countof(ctx->fregs); i++)
    ctx->fregs[i] = m->fregs[i + RSM_NTMPREGS];
  for (usize i = 0; i < countof(ctx->iregs); i++)
    ctx->iregs[i] = m->iregs[i + RSM_NTMPREGS];
  t->sp = m->iregs[RSM_MAX_REG];
}


static void t_restore_ctx(T* t) {
  M* m = assertnotnull(t->m);
  tctx_t* ctx = assertnotnull(m_get_tctx(m, t));
  // tctx_t layout:
  //   [byte 0] F{RSM_NTMPREGS} F{RSM_NTMPREGS+1} F{...} F{RSM_NREGS-1} ...
  //        ... R{RSM_NTMPREGS} R{RSM_NTMPREGS+1} R{...} R{RSM_NREGS-2}
  for (usize i = 0; i < countof(ctx->fregs); i++)
    m->fregs[i + RSM_NTMPREGS] = ctx->fregs[i];
  for (usize i = 0; i < countof(ctx->iregs); i++)
    m->iregs[i + RSM_NTMPREGS] = ctx->iregs[i];
  m->iregs[RSM_MAX_REG] = t->sp;
}


// m_switchtask configures m to execute task t.
// It first saves the current task's state, then restores the state of t.
static void m_switchtask(M* m, T* nullable t) {
  if (t) trace("-> T%llu", t->id);
  else   trace("-> T-");

  assert(t != m->currt);

  if (m->currt) {
    t_save_ctx(m->currt);
    m->currt->m = NULL;
  }

  m->currt = t;

  if (t) {
    t->m = m;
    t_restore_ctx(t);
  }
}


static T* nullable task_create(M* m, u64 stack_vaddr, usize stacksize) {

  // TODO: move this T-struct layout into rsched_loadrom so that T is before exeinfo_t;
  // then we might not need CTX, since t is always at aligned(VM_ADDR_MAX-sizeof(T)).

  // task memory layout:
  //
  //               ┌─────────────┬─ stacktop
  //               │             │
  //               ~      ↑      ~
  //               │    stack    │← tctx_t on top when task is idle
  //               ├─────────────┼─ stackbase (CTX and initial SP)
  //               │  T struct   │
  //  stack_vaddr ─┴─────────────┘
  //
  assert(IS_ALIGN2(stack_vaddr, STK_ALIGN));
  vm_cache_t* vm_cache = &m->vm_cache[VM_PERM_RW];
  void* stackptr =
    (void*)VM_TRANSLATE(vm_cache, &m->s->vm_pagedir, stack_vaddr, STK_ALIGN);
  assertf(stackptr != NULL, "stack memory not mapped for 0x%llx", stack_vaddr);

  //dlog("stack 0x%llx => haddr %p ... %p", stack_vaddr, stackptr - stacksize, stackptr);

  // space for T, below stack, with strongest alignment
  usize tsize = ALIGN2(sizeof(T), MAX(_Alignof(T), STK_ALIGN));

  T* t = stackptr - tsize;
  t->stacktop = stack_vaddr - stacksize;
  t->sp = stack_vaddr - (u64)tsize - sizeof(u64); // current stack pointer

  // push final return value to stack
  static_assert(_Alignof(T) <= STK_ALIGN, "assuming u64 alignment of stack");
  u64* sp = (void*)t - sizeof(u64);
  *sp = (u64)PROLOGUE_PC; // instruction offset of prologue

  // initialize tctx
  tctx_t* ctx = (tctx_t*)ALIGN2_FLOOR((uintptr)sp - sizeof(tctx_t), TCTX_ALIGN);
  ctx->iregs[RSM_MAX_REG - RSM_NTMPREGS - 1] = (u64)(uintptr)t; // CTX reg

  //dlog("store ctx to stack at vaddr 0x%llx (haddr %p)", tctx_vaddr(t->sp), ctx);

  trace("T@%p+%zu stack 0x%llx-0x%llx", t, tsize, t->stacktop, stack_vaddr);

  return t;
}


// m_spawn creates a new T running fn with argsize bytes of arguments.
// Put it on the queue of T's waiting to run.
// The compiler turns a go statement into a call to this.
static rerr_t m_spawn(
  M* m, const rin_t* iv, usize ic, u64 stack_vaddr, usize stacksize, usize pc)
{
  rerr_t err = 0;

  // disable preemption
  m_acquire(m);

  // create main task
  T* newtask = task_create(m, stack_vaddr, stacksize);
  if (!newtask) {
    err = rerr_nomem;
    goto onerr;
  }

  // setup program instruction array, program counter and task ID
  newtask->pc = pc;
  newtask->instrc = ic;
  newtask->instrv = iv;
  //newtask->rodata = rodata; // TODO vm
  newtask->id = AtomicAdd(&m->s->tidgen, 1, memory_order_acquire);

  // add the task to the scheduler
  t_setstatus(newtask, T_DEAD);
  if ((err = sched_allt_add(m->s, newtask)))
    goto onerr;

  // set status to runnable
  t_setstatus(newtask, T_RUNNABLE);

  // get P associated with current M
  P* p = assertnotnull(m->p);

  // re-enable preemption
  m_release(m);

  // add newtask to run queue
  p_runq_put(p, newtask, /*runnext*/true);

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

  // virtual memory caches
  for (usize i = 0; i < countof(m->vm_cache); i++)
    vm_cache_init(&m->vm_cache[i]);

  return 0;
}


static void p_init(P* p, rsched_t* s, u32 id) {
  memset(p, 0, sizeof(P));
  p->id = id;
  p->status = P_IDLE;
  p->s = s;
  RHMutexInit(&p->timers_lock);
}


// static void p_dispose(P* p) {
//   RHMutexDispose(&p->timers_lock);
// }


// p_acquire associates P with M
static void p_acquire(P* p, M* m) {
  trace("P%u", p->id);
  assertf(p->m == NULL, "P in use with other M");
  assertf(m->p == NULL, "M in use with other P");
  assert(p->status == P_IDLE);
  m->p = p;
  p->m = m;
  p->status = P_RUNNING;
}

// p_release disassociates P from its current M
static void p_release(P* p) {
  trace("");
  assertf(p->status == P_RUNNING, "%u", p->status);
  M* m = assertnotnull(p->m);
  m->p = NULL;
  p->m = NULL;
  p->status = P_IDLE;
}


// p_runqempty returns true if p has no Ts on its local run queue
static bool p_runqempty(P* p) {
  // return p->runqhead == p->runqtail && p->runnext == 0; //< unlocked impl

  // Defend against a race where
  // 1) p has T1 in runqnext but runqhead == runqtail,
  // 2) p_runq_put on p kicks T1 to the runq, 3) runqget on p empties runqnext.
  // Simply observing that runqhead == runqtail and then observing that runqnext == NULL
  // does not mean the queue is empty.
  while (1) {
    u32 head = AtomicLoadAcq(&p->runqhead);
    u32 tail = AtomicLoadAcq(&p->runqtail);
    T* runnext = AtomicLoadAcq(&p->runnext);
    if (tail == AtomicLoadAcq(&p->runqtail)) {
      return head == tail && runnext == NULL;
    }
  }
}


// p_update_timerp_mask clears P's timer mask if it has no timers.
//
// Ideally, the timer mask would be kept immediately consistent on any timer
// operations. Unfortunately, updating a shared global data structure in the
// timer hot path adds too much overhead in applications frequently switching
// between no timers and some timers.
//
// As a compromise, the timer mask is updated only on pidleget / pidleput. A
// running P (returned by pidleget) may add a timer at any time, so its mask
// must be set. An idle P (passed to pidleput) cannot add new timers while
// idle, so if it has no timers at that time, its mask may be cleared.
//
// Thus, we get the following effects on timer-stealing in findrunnable:
//
// * Idle Ps with no timers when they go idle are never checked in findrunnable
//   (for work- or timer-stealing; this is the ideal case).
// * Running Ps must always be checked.
// * Idle Ps whose timers are stolen must continue to be checked until they run
//   again, even after timer expiration.
//
// When the P starts running again, the mask should be set, as a timer may be
// added at any time.
//
// TODO(prattmic): Additional targeted updates may improve the above cases.
// e.g., updating the mask when stealing a timer.
static void p_update_timerp_mask(P* p) {
  if (AtomicLoad(&p->ntimers, memory_order_relaxed) > 0)
    return;
  // Looks like there are no timers, however another P may transiently
  // decrement ntimers when handling a timer_modified timer in p_check_timers.
  RHMutexLock(&p->timers_lock);
  if (AtomicLoadAcq(&p->ntimers) == 0)
    pbits_clear(&p->s->timerp_mask, p->id);
  RHMutexUnlock(&p->timers_lock);
}


// s_pidle_get tries to get a P from s.pidle list.
// s.lock must be held.
static P* nullable s_pidle_get(rsched_t* s) {
  P* p = s->pidle;

  if (!p) {
    trace("P- (no idle P)");
    return NULL;
  }

  pbits_set(&s->timerp_mask, p->id);
  pbits_clear(&s->pidle_mask, p->id);
  s->pidle = p->nextp; // dequeue
  AtomicSub(&s->npidle, 1, memory_order_release);

  trace("P%u", p->id);
  return p;
}


// s_pidle_put puts P on s.pidle list.
// s.lock must be held.
static void s_pidle_put(rsched_t* s, P* p) {
  trace("P%u", p->id);
  assert(p_runqempty(p) /* trying to put P to sleep with runnable Ts */);
  p_update_timerp_mask(p);
  pbits_set(&s->pidle_mask, p->id);
  p->nextp = s->pidle;
  s->pidle = p;
  AtomicAdd(&s->npidle, 1, memory_order_release);
}


// s_runq_prepend puts T in front of the global runnable queue, to be run ASAP.
// s.lock must be held.
static void s_runq_prepend(rsched_t* s, T* t) {
  assertnull(t->schedlink);
  t->schedlink = AtomicLoadAcq(&s->runq.head);
  AtomicStoreRel(&s->runq.head, t);
  if (AtomicLoadAcq(&s->runq.tail) == NULL)
    AtomicStoreRel(&s->runq.tail, t);
  AtomicAdd(&s->runq.len, 1, memory_order_release);
}


// s_runq_append adds T to the end of the global runnable queue.
// s.lock must be held.
static void s_runq_append(rsched_t* s, T* t) {
  assertnull(t->schedlink);
  T* tail = AtomicLoadAcq(&s->runq.head);
  if (tail) {
    tail->schedlink = t;
  } else {
    AtomicStoreRel(&s->runq.head, t);
  }
  AtomicStoreRel(&s->runq.tail, t);
  AtomicAdd(&s->runq.len, 1, memory_order_release);
}


static T* s_runq_pop(rsched_t* s) {
  T* t = assertnotnull(AtomicLoadAcq(&s->runq.head));
  AtomicStoreRel(&s->runq.head, t->schedlink);
  if (t->schedlink == NULL)
    AtomicStoreRel(&s->runq.head, NULL);
  return t;
}


// s_runq_get attempts to dequeue a batch of T's from the global runnable queue.
// s.lock must be held.
// If max=0, move up to runq_len/s->nprocs extra tasks to p.runq.
static T* s_runq_get(rsched_t* s, P* nullable p, u32 max) {
  u32 runq_len = AtomicLoadAcq(&s->runq.len);
  if (runq_len == 0)
    return NULL;

  // determine amount of Ts to get
  u32 n = MIN(runq_len, runq_len/s->nprocs + 1);
  if (max > 0 && n > max)
    n = max;
  // limit number of Ts we take to half of P runq size
  if (n > P_RUNQSIZE / 2) // ok, P_RUNQSIZE is 2^N
    n = P_RUNQSIZE / 2;

  AtomicSub(&s->runq.len, n, memory_order_release);

  // Take top T, to be returned
  T* t = s_runq_pop(s);

  // Move n Ts from top of s->runq to end of p->runq
  if (p) while (--n > 0) {
    T* t = s_runq_pop(s);
    p_runq_put(p, t, /*next=*/false);
  }

  return t;
}


static void m_droptask(M* m) {
  trace("");
  T* t = assertnotnull(m->currt);
  assert(t->m == m);
  t->m = NULL;
  m->currt = NULL;
}


static void m_park(M* m) {
  trace("TODO");
}


// m_steal_work attempts to steal work from other P's.
// Marks m as "spinning".
static inline T* nullable m_steal_work(
  M* m, bool* inherit_time, bool* ran_timer, u64* poll_untilp)
{
  if (!m->spinning) {
    trace("marking M%u spinning", m->id);
    m->spinning = true;
    AtomicAdd(&m->s->nmspinning, 1, memory_order_release);
  }

  trace("TODO");
  // P* p = assertnotnull(m->p);
  // for (int i = 0, steal_attempts = 4; i < steal_attempts; i++) {
  //   // ...
  // }

  u64 poll_until = 0; // TODO

  // Earlier timer to wait for
  if (poll_until != 0 && (*poll_untilp == 0 || poll_until < *poll_untilp))
    *poll_untilp = poll_until;

  return NULL;
}


// p_wake_time looks at P's timers and returns the time when we should wake up
// the I/O poller. It returns 0 if there are no timers.
static u64 p_wake_time(P* p) {
  u64 next = AtomicLoadAcq(&p->timer0_when);
  u64 next_adj = AtomicLoadAcq(&p->timer_modified_earliest);
  if (next == 0 || (next_adj != 0 && next_adj < next))
    next = next_adj;
  return next;
}


static i64 s_check_timers(rsched_t* s, u32 nprocs, u64 poll_until) {
  for (u32 id = 0; id < nprocs; id++) {
    if (!pbits_isset(&s->timerp_mask, id))
      continue;
    P* p = s->allp[id];
    u64 w = p_wake_time(p);
    if (w != 0 && (poll_until == 0 || w < poll_until))
      poll_until = w;
  }
  return poll_until;
}


// s_find_runnable_p tries to find a P with non-empty runq
static P* nullable s_find_runnable_p(rsched_t* s, u32 nprocs) {
  for (u32 id = 0; id < nprocs; id++) {
    P* p = s->allp[id];
    if (pbits_isset(&s->pidle_mask, id) || p_runqempty(p))
      continue;
    RHMutexLock(&s->lock);
    p = s_pidle_get(s);
    RHMutexUnlock(&s->lock);
    return p; // ok to return NULL (don't bother checking other P's)
  }
  return NULL;
}


// s_findrunnable finds a runnable coroutine to execute.
// Blocks until a task is found.
// 1. Tries to dequeue from p.runq
// 2. Tries to dequeue from global s.runq
// 3. Tries to steal from other P's
// 4. Polls I/O (e.g. network, file, etc)
// 5. Retries by starting over
static T* nullable m_findrunnable(M* m, bool* inherit_time) {
  rsched_t* s = assertnotnull(m->s);
  P* p;
  T* t;

top:
  p = assertnotnull(m->p);

  // check for expired timers
  u64 now = 0;
  u64 poll_until = 0;
  // TODO: now = p_check_timers(p, &poll_until)

  // try to dequeue a ready-to-run task from p.runq
  trace("try p.runq");
  t = p_runq_get(p, inherit_time);
  // We can see t != NULL here even if the M is spinning,
  // if check_timers added a local goroutine via goready.
  if (t) {
    trace("found T%llu on p.runq", t->id);
    return t;
  }

  // global runq
  trace("try s.runq");
  if (AtomicLoad(&s->runq.len, memory_order_relaxed)) {
    RHMutexLock(&s->lock);
    t = s_runq_get(s, p, 0); // 0 means "move all to P"
    RHMutexUnlock(&s->lock);
    if (t) {
      trace("found T%llu on s.runq", t->id);
      *inherit_time = false;
      return t;
    }
  }

  // Check for I/O events
  //TODO: I/O (e.g. netpoll)

  // Steal work from other P's
  //
  // If number of spinning M's >= number of busy P's, block (i.e. don't steal work).
  // This is necessary to prevent excessive CPU consumption
  // when MAXPROCS>1 but the program parallelism is low.
  i32 nmspinning = AtomicLoadAcq(&s->nmspinning);
  i32 npbusy = (i32)( s->nprocs - AtomicLoadAcq(&s->npidle) );
  if (m->spinning || 2*nmspinning < npbusy) {
    if (!m->spinning) {
      m->spinning = true;
      AtomicAdd(&s->nmspinning, 1, memory_order_release);
    }

    bool ran_timer = false;
    t = m_steal_work(m, inherit_time, &ran_timer, &poll_until);
    if (t)
      return t;
    // Running a timer may have made some task ready, so try again
    if (ran_timer)
      goto top; // retry while loop
  }

  // if we get here, there's probably no work
  trace("out of work");

  // Release P for other M's to use.
  RHMutexLock(&s->lock);
  // Before we drop our P, make a snapshot of nprocs
  u32 nprocs = s->nprocs;
  // We might just find a task on the global s.runq, but probably not.
  if (AtomicLoad(&s->runq.len, memory_order_relaxed) > 0) {
    t = s_runq_get(s, p, 0); // 0 means "move all to P"
    assertnotnull(t); // lock held; runq.len not able to change
    RHMutexUnlock(&s->lock);
    trace("found T%llu in s.runq", t->id);
    *inherit_time = false;
    return t;
  }
  p_release(p); // disassociate P from M
  s_pidle_put(s, p);
  RHMutexUnlock(&s->lock);

  // Delicate dance: thread transitions from spinning to non-spinning state,
  // potentially concurrently with submission of new tasks.
  // We must decrement nmspinning first and then check all per-P queues again.
  // If we do it the other way around, another thread can submit a task after
  // we've checked all run queues but before we decrement nmspinning;
  // as a result nobody will unpark a thread to run the task.
  // If we discover new work below, we need to restore m.spinning as a signal
  // for m_reset_spinning to unpark a new worker thread (because there can be more
  // than one starving task). However, if after discovering new work
  // we also observe no idle Ps, it is OK to just park the current thread:
  // the system is fully loaded so no spinning threads are required.
  bool m_was_spinning = m->spinning;
  if (m_was_spinning) {
    m->spinning = false;
    UNUSED i32 nmspinning = AtomicSub(&s->nmspinning, 1, memory_order_release);
    assertf(nmspinning > 0, "negative nmspinning");

    // check all runqueues once again (returns P with non-empty runq)
    p = s_find_runnable_p(s, nprocs);
    if (p != NULL) {
      p_acquire(p, m);
      m->spinning = true;
      AtomicAdd(&s->nmspinning, 1, memory_order_release);
      trace("found P%u with non-empty runq; retrying", p->id);
      goto top;
    }

    // Finally, check for timer creation or expiry concurrently with
    // transitioning from spinning to non-spinning.
    //
    // Note that we cannot use p_check_timers here because it requires an active P.
    poll_until = s_check_timers(s, nprocs, poll_until);
  }

  // TODO ...
  // // Poll I/O until next timer
  // if ( (iopoll_inited() && iopoll_nwaiters() > 0 || poll_until != 0) &&
  //      atomic.Xchg64(&sched.lastpoll, 0) != 0)
  // {
  //   // TODO ...
  //   // atomic.Store64(&sched.pollUntil, uint64(pollUntil))
  // }

  // TODO

  return NULL;
}


// m_schedule performs scheduling: in a loop, it finds a runnable task and executes it.
// Returns when all run queues are empty.
static void m_schedule(M* m) {
  rsched_t* s = m->s;
  for (;;) {
    trace("");

    P* p = m->p; // may be NULL
    T* t = NULL;
    bool inherit_time = false;

    // Check the global runnable queue once in a while to ensure fairness.
    // Otherwise two tasks can completely occupy the local runqueue by constantly
    // respawning each other.
    if (p && p->schedtick % 61 == 0 &&
        AtomicLoad(&s->runq.len, memory_order_relaxed) > 0)
    {
      RHMutexLock(&s->lock);
      t = s_runq_get(s, p, 1);
      RHMutexUnlock(&s->lock);
      if (t) trace("random runq steal of T%llu", t->id);
    }

    // if M does not have an associated P, wait for a P to become available
    if (!p) {
      RHMutexLock(&s->lock);
      p = s_pidle_get(s);
      RHMutexUnlock(&s->lock);
      if (!p)
        panic("TODO wait for a P to become available");
      p_acquire(p, m);
    }

    p->preempt = false;

    // m_findrunnable blocks until work is available or all tasks have exited
    if (!t) {
      t = m_findrunnable(m, &inherit_time);
      if (!t)
        return;
    }

    // This thread is going to run a coroutine and is not spinning anymore,
    // so if it was marked as spinning we need to reset it now and potentially
    // start a new spinning M.
    if (m->spinning)
      panic("TODO: m_resetspinning(m)");

    // save any current task's (m->currt) state and restore state of t (sets m->currt)
    m_switchtask(m, t);

    // Assign t->m before entering T_RUNNING so running Ts have an M
    t_casstatus(t, T_RUNNABLE, T_RUNNING);
    t->waitsince = 0;

    p->schedtick += (u32)inherit_time;

    // execute task
    trace("eval");
    t->pc = rsched_eval(t, m->iregs, t->instrv, t->pc);
    // returns here when the task has been parked with task_park
  }
}


// task_disable is called when a task is about to do something that will lock up its M,
// releasing its P for other tasks to use.
void task_disable(T* t) {
  trace("");

  assert(t->status == T_RUNNING);
  t_setstatus(t, T_SYSCALL);

  M* m = assertnotnull(t->m);
  P* p = assertnotnull(m->p);

  // release the P so it can be used by other tasks waiting to run
  p_release(p); // disassociate P from M
  // put P on list of idle P's
  rsched_t* s = p->s;
  RHMutexLock(&s->lock);
  s_pidle_put(s, p);
  RHMutexUnlock(&s->lock);
}


bool task_enable(T* t, int priority) {
  trace("");

  M* m = assertnotnull(t->m);
  assertnull(m->p);

  // mark task as runnable
  assert(t->status != T_RUNNING);

  // try to get an idle P, immediately resuming execution
  #ifdef SCHED_OPTIMIZE
    RHMutexLock(&s->lock);
    P* p = s_pidle_get(m->s);
    RHMutexUnlock(&s->lock);
    if (p) {
      t_set_current(t);
      t_setstatus(t, T_RUNNING);
      p_acquire(p, m);
      return true;
    }
  #endif

  m_switchtask(m, NULL);
  t_setstatus(t, T_RUNNABLE);

  // no available P; put task on global run queue
  if (priority > 0) {
    s_runq_prepend(m->s, t);
  } else {
    // m_droptask(m); // release M to allow other waiting tasks to run
    s_runq_append(m->s, t);
  }

  return false;
}


// task_park is called by the execution engine when a task is to be suspended.
// "reason" tells us why the task is being suspended.
void task_park(T* t, tstatus_t reason) {
  trace("%s", tstatus_str(reason));
  assert(t->m->currt == t);

  #if RSM_SAFE
    tstatus_t status = t_readstatus(t);
    safecheckf(status == T_RUNNING, "status=%u", status);
  #endif

  switch (reason) {

  case T_DEAD:
    // task exited
    t_setstatus(t, T_DEAD);
    t->parent = NULL;
    break;

  case T_SYSCALL:
    // task will be waiting for a syscall
    t_setstatus(t, T_SYSCALL);
    break;

  default: panic("unexpected task status %u", reason);
  }

  if (reason == T_DEAD) {
    m_droptask(t->m);
    m_park(t->m);
  }
}


// static void task_unpark(T* t, tstatus_t reason) {
//   trace("%s", tstatus_str(reason));
//   t_setstatus(t, T_RUNNABLE);
//   P* p = assertnotnull(assertnotnull(t->m)->p);
//   //m_droptask(t->m);
//   p_runq_put(p, t, /*next*/false);
// }


// m_start is the entry-point for new M's.
// Basically the "OS thread main function."
// M doesn't have a P yet.
NOINLINE static rerr_t m_start(M* m) {
  m_schedule(m);
  return 0;
}


rerr_t rsched_init(rsched_t* s, rmachine_t* machine) {
  memset(s, 0, sizeof(rsched_t));

  s->machine = machine;

  if (!RHMutexInit(&s->lock))
    return rerr_canceled;

  // allt
  if (mtx_init(&s->allt.lock, mtx_plain) != 0)
    return rerr_canceled;

  // virtual memory page directory
  rerr_t err = vm_pagedir_init(&s->vm_pagedir, machine->mm);
  if (err)
    return err;

  // initialize main M (id=0) on current OS thread
  s->midgen = 1;
  s->maxmcount = 1000;
  s->m0.s = s;
  err = m_init(&s->m0, 0);
  if UNLIKELY(err)
    goto error;

  TRACE_SET_CURRENT_M(&s->m0);

  // create processors
  u32 nprocs = 2;
  assertf(nprocs > 0 && nprocs <= S_MAXPROCS, "%u", nprocs);
  s->nprocs = nprocs;
  trace("s.nprocs=%u", nprocs);
  p_init(&s->p0, s, 0);
  s->allp[0] = &s->p0;
  for (u32 p_id = 1; p_id < nprocs; p_id++) {
    P* p = rmem_alloct(s->machine->malloc, P);
    if (!p) {
      // TODO: free other P's we allocated
      err = rerr_nomem;
      goto error;
    }
    p_init(p, s, p_id);
    s->allp[p_id] = p;
    p->nextp = s->pidle;
    s->pidle = p;
  }

  // associate P0 with M0
  P* p = s->allp[0];
  p->status = P_IDLE;
  p_acquire(p, &s->m0); // associate P and M (p->m=m, m->p=p, p.status=P_RUNNING)

  return 0;
error:
  vm_pagedir_dispose(&s->vm_pagedir);
  return err;
}


void rsched_dispose(rsched_t* s) {
  RHMutexDispose(&s->lock);
  mtx_destroy(&s->allt.lock);
  vm_pagedir_dispose(&s->vm_pagedir);
}


static void exeinfo_init(exeinfo_t* exeinfo, u64 heap_vaddr) {
  assertf(IS_ALIGN2((uintptr)exeinfo, _Alignof(exeinfo_t)), "%p", exeinfo);
  exeinfo->heap_vaddr = heap_vaddr;
}


// rsched_alloc_basemem allocates base memory; backing pages for rom code and data
static rmem_t rsched_alloc_basemem(rsched_t* s, rrom_t* rom) {
  // Minimum size needed for uncompressed ROM image.
  // Plus one extra page for rom's code & data alignment.
  usize minromsize = rromimg_loadsize(rom->img, rom->imgsize) + (PAGE_SIZE - 1);

  // calculate how many pages we need (allocation must be pow2 order)
  usize npages = CEIL_POW2(
    ( (minromsize + PAGE_SIZE-1) / PAGE_SIZE ) + // code & data
    ( (STK_MIN + PAGE_SIZE-1) / PAGE_SIZE ) + // minimum stack (fail early)
    EXEINFO_NPAGES // exeinfo_t
  );

  // allocate pages
  void* p = rmm_allocpages(s->machine->mm, npages);
  rmem_t basemem = RMEM(p, npages*PAGE_SIZE);

  dlog("basemem " RMEM_FMT " %zu pages", RMEM_FMT_ARGS(basemem), npages);
  return basemem;
}


#if DEBUG
static void dlog_memory_map(
  const void* code, usize codesize,
  const void* data, usize datasize,
  const void* heap, usize heapsize,
  const void* stack, usize stacksize)
{
  dlog("memory map:");
  dlog("  %-7s %12s  %-12s              %8s","SEGMENT","HADDR","VADDR","SIZE");
  #define SEG(name, haddr, vaddr, size) { \
    uintptr haddr__ = (uintptr)(haddr); \
    u64 vaddr__ = (u64)(vaddr), vaddr_end__; \
    if ( check_add_overflow(vaddr__, (u64)(size), &vaddr_end__) || \
         vaddr_end__ >= EXE_TOP_ADDR ) \
    { \
      vaddr_end__ = vaddr__; \
      vaddr__ -= (u64)(size); \
      haddr__ -= (uintptr)(size); \
    } \
    if (vaddr__ == 0) { \
      dlog("  %-7s %12lx  (not mapped)              %8zu", \
      (name), haddr__, (size)); \
    } else { \
      dlog("  %-7s %12lx  %012llx-%012llx %8zu", \
        (name), haddr__, vaddr__, vaddr_end__, (size)); \
    } \
  }

  u64 heap_vaddr = VM_ADDR_MIN + (u64)(uintptr)(heap - data);

  SEG("code",    code,  0,           codesize);
  SEG("data",    data,  DATA_VADDR,  datasize);
  SEG("heap",    heap,  heap_vaddr,  heapsize);
  SEG("stack",   stack, STACK_VADDR, stacksize);
  SEG("exeinfo", stack, STACK_VADDR, sizeof(exeinfo_t));
  #undef SEG
}
#else
  #define dlog_memory_map(...) ((void)0)
#endif


// rsched_loadrom loads a rom image into backing memory pages at basemem.
// When called, *stacksizep is the requested stack size. On return the actual stack size.
static rerr_t rsched_loadrom(rsched_t* s, rrom_t* rom, rmem_t basemem, usize* stacksizep) {
  // 1. load rom image into base memory:
  //   basemem
  //   ├────────┬──────────┬──────────────────────┬─────────────────────┐
  //   │ header │ code     │ data                 │ ignored_section ... │
  //   ├────────┴──────────┴──────────────────────┴─────────────────────┘
  //   page 1
  //
  // 2. move code and data sections to page boundaries,
  //    overwriting header and other ROM sections:
  //   ┌────────────────┬─────────────────────────────────┐
  //   │ code           │ data                            │
  //   ├────────────────┼────────────────┬────────────────┘
  //   page 1           page 2           page 3
  //                    vm start
  //
  rerr_t err = rsm_loadrom(rom, basemem);
  if (err)
    return err;

  // this implementation currently requires the CODE section
  // to appear before the DATA section
  if UNLIKELY(rom->data != NULL && (const void*)rom->code > rom->data) {
    // TODO: support rom with DATA section before CODE section
    dlog("unsupported ROM layout: DATA section before CODE section");
    return rerr_not_supported;
  }

  // prologue code
  // must fit in ROM header so that we can guarantee space in basemem
  // and alignment of data. This constraint can be lifted by adding logic to
  // rsched_alloc_basemem and add to codesize below as needed.
  static_assert(PROLOGUE_LEN*sizeof(rin_t) <= sizeof(rromimg_t),
    "prologue larger than rom header");
  const rin_t prologue[PROLOGUE_LEN] = {
    RSM_MAKE_Au(rop_SCALL, 0), // syscall(SC_EXIT)
  };
  static_assert(PROLOGUE_PC == 0, "");
  memcpy(basemem.p, prologue, sizeof(prologue));

  // move code section to front
  usize codesize = rom->codelen * sizeof(rin_t);
  memmove(basemem.p + sizeof(prologue), rom->code, codesize);

  // rsm_loadrom verifies that rom->dataalign<=RSM_ROM_ALIGN and we're
  // making the assumption here that PAGE_SIZE as least as large,
  // since we're aligning data to page boundary.
  static_assert(RSM_ROM_ALIGN <= PAGE_SIZE, "");

  // move data section to after code, aligned to page boundary
  void* data = basemem.p + ALIGN2(codesize, PAGE_SIZE);
  memmove(data, rom->data, rom->datasize);

  // virtual memory layout
  //   ┌────────────┬─────────┬─────────── ··· ────────────┬───────────┐
  //   │ code       │ data    │ heap →             ← stack │ exeinfo_t │
  //   ├────────────┼─────────┴─────────── ··· ────────────┴───────────┤
  //   not mapped   0x1000                                             0xffffffffffff
  //                VM_ADDR_MIN                                        VM_ADDR_MAX

  // stacksize (size of stack; space for exeinfo later subtracted)
  usize stacksize = EXEINFO_NPAGES * PAGE_SIZE;

  // remaining data space is used for backing memory of the initial heap
  void* heap = (void*)ALIGN2((uintptr)data + rom->datasize, HEAP_ALIGN);
  usize heapsize;
  void* heapend = basemem.p + (basemem.size - stacksize);
  if (heap >= heapend) {
    heap = heapend;
    heapsize = 0;
  } else {
    heapsize = (usize)(uintptr)(heapend - heap);
    if (heapsize > PAGE_SIZE) {
      // when we have extra backing pages, map them to initial stack instead of heap
      usize diff = ALIGN2_FLOOR(heapsize, PAGE_SIZE);
      stacksize += diff;
      heapsize -= diff;
    }
  }

  // Set logical size of stack (virtual memory, not backing memory.)
  // Note that *stacksizep is the stack size requested by the caller.
  stacksize = MAX(stacksize, *stacksizep);

  // stack starts at the highest virtual address and is mapped to the last
  // stacksize/PAGE_SIZE host backing pages.
  void* stack = basemem.p + basemem.size;

  // reserve space on stack for exeinfo_t & initialize it
  usize exeinfo_size = ALIGN2(sizeof(exeinfo_t), MAX(_Alignof(exeinfo_t), STK_ALIGN));
  stack -= exeinfo_size;
  stacksize -= exeinfo_size;
  u64 heap_vaddr = VM_ADDR_MIN + (u64)(uintptr)(heap - data);
  exeinfo_init(stack, heap_vaddr);
  assertf(IS_ALIGN2((uintptr)stack, STK_ALIGN), "%p", stack);
  assertf(stacksize >= STK_MIN, "stacksize(%zu) < STK_MIN(%lu)", stacksize, STK_MIN);

  dlog_memory_map(
    basemem.p, codesize,
    data, rom->datasize,
    heap, MAX(heapsize, HEAP_INITSIZE),
    stack, stacksize);

  // map bottom vm pages (data & heap) in vm page directory
  uintptr haddr = (uintptr)data;
  usize data_nbyte = ALIGN2(rom->datasize, HEAP_ALIGN) + heapsize;
  assertf(IS_ALIGN2(data_nbyte, PAGE_SIZE), "%zu", data_nbyte);
  usize bottom_npages = data_nbyte / PAGE_SIZE;
  err = vm_map(&s->vm_pagedir, haddr, DATA_VADDR, bottom_npages, VM_PERM_RW);
  if UNLIKELY(err) {
    dlog("vm_map failed: %s", rerr_str(err));
    return rerr_mfault;
  }

  // map remaining heap pages WITHOUT backing memory
  if (heapsize < HEAP_INITSIZE) {
    usize npages = (HEAP_INITSIZE - heapsize) / PAGE_SIZE;
    err = vm_map(&s->vm_pagedir, 0, DATA_VADDR + data_nbyte, npages, VM_PERM_RW);
    if UNLIKELY(err) {
      dlog("vm_map failed: %s", rerr_str(err));
      vm_unmap(&s->vm_pagedir, DATA_VADDR, bottom_npages);
      return rerr_mfault;
    }
    bottom_npages += npages;
  }

  // remaining backing memory at haddr
  usize hi_nbyte = (usize)(uintptr)(heapend - data); // remaining haddr
  assertf(IS_ALIGN2(hi_nbyte, PAGE_SIZE), "%zu", hi_nbyte);

  // map the low-address pages of the stack WITHOUT backing memory
  usize lo_nbyte = (stacksize + exeinfo_size) - hi_nbyte;
  u64 lo_vaddr = STACK_VADDR - stacksize;
  usize lo_npages = lo_nbyte / PAGE_SIZE;
  //dlog("map %zu pages: 0x%llx (lazy)", lo_npages, lo_vaddr);
  err = vm_map(&s->vm_pagedir, 0, lo_vaddr, lo_npages, VM_PERM_RW);
  if UNLIKELY(err) {
    dlog("vm_map failed: %s", rerr_str(err));
    vm_unmap(&s->vm_pagedir, DATA_VADDR, bottom_npages);
    return rerr_mfault;
  }

  // map the high-address pages of the stack WITH backing memory
  u64 vaddr = lo_vaddr + lo_nbyte;
  assertf(IS_ALIGN2(hi_nbyte, PAGE_SIZE), "%zu", hi_nbyte);
  //dlog("map %zu pages: 0x%llx => %p", hi_nbyte/PAGE_SIZE, vaddr, (void*)haddr);
  err = vm_map(&s->vm_pagedir, haddr, vaddr, hi_nbyte/PAGE_SIZE, VM_PERM_RW);
  if UNLIKELY(err) {
    dlog("vm_map failed: %s", rerr_str(err));
    vm_unmap(&s->vm_pagedir, lo_vaddr, lo_npages);
    vm_unmap(&s->vm_pagedir, DATA_VADDR, bottom_npages);
    return rerr_mfault;
  }

  *stacksizep = stacksize;

  return 0;
}


static rerr_t rsched_unloadrom(rsched_t* s, rrom_t* rom) {
  dlog("TODO %s: vm_unmap", __FUNCTION__);
  return 0;
}


rerr_t rsched_execrom(rsched_t* s, rrom_t* rom) {
  // allocate base memory; pages for rom code and data.
  // We will load the rom image into this space and move code & data to page boundaries.
  rmem_t basemem = rsched_alloc_basemem(s, rom);
  if (basemem.p == NULL)
    return rerr_nomem;

  // load rom into initial backing pages
  usize stacksize = STK_DEFAULT;
  rerr_t err = rsched_loadrom(s, rom, basemem, &stacksize);
  if (err)
    goto end;

  // create main task
  void* code = basemem.p;
  u64 pc = PROLOGUE_LEN; // TODO: use instruction address of "main" function
  err = m_spawn(&s->m0, code, rom->codelen + PROLOGUE_LEN, STACK_VADDR, stacksize, pc);
  if (err)
    goto end;

  // enter scheduler loop in M0
  err = m_start(&s->m0);

  // finish
  rsched_unloadrom(s, rom);

end:
  rmm_freepages(s->machine->mm, basemem.p);
  return err;
}

