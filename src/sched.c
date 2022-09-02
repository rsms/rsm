// scheduler
// SPDX-License-Identifier: Apache-2.0
//
// Inspired by the Go and Erlang schedulers.
// References:
//
//  "Scalable Go Scheduler Design Doc"
//  https://docs.google.com/document/u/1/d/1TTj4T2JO42uD5ID9e89oa0sLKhJYD0Y_kqxDv3I3XMw/
//
//  "Analysis of the Go runtime scheduler" (Columbia University, 2012)
//  http://www.cs.columbia.edu/~aho/cs6998/reports/12-12-11_DeshpandeSponslerWeiss_GO.pdf
//
//  "Inside the Erlang VM" (Kenneth Lundin, Ericsson AB, 2008)
//
#include "rsmimpl.h"
#include "abuf.h"
#include "thread.h"
#include "sched.h"
#include "machine.h"
#include "hash.h"
#include "syscall.h"

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

// EPILOGUE_LEN: number of instructions in the main epilogue.
#define EPILOGUE_LEN 1

// void s_assert_locked(rsched_t*)
#define s_assert_locked(s)  assertf(mutex_islocked(&(s)->lock), "s locked")


enum PStatus {
  P_IDLE = 0,
  P_RUNNING, // Only this P is allowed to change from P_RUNNING
  P_SYSCALL,
  P_DEAD,
};

// stackbase for a task
#define TASK_STACKBASE(t) ((void*)ALIGN2_FLOOR((uintptr)(t),STK_ALIGN))


#define trace  schedtrace1
#define trace2 schedtrace2
#define trace3 schedtrace3

#if defined(SCHED_TRACE) && !defined(RSM_NO_LIBC)
  #include <stdio.h>

  // thread-local storage of the current M on current OS thread, for tracing
  static _Thread_local M* _g_current_m = NULL;
  #define TRACE_SET_CURRENT_M(m)  do{ _g_current_m = (m); }while(0)

  // "Mn Pn Tn  function  message"
  void _schedtrace(int level, const char* fn, const char* fmt, ...) {
    M* m = assertnotnull(_g_current_m);
    T* t = m->currt;
    P* p = m->p;
    FILE* fp = stderr;
    flockfile(fp);
    const char* prefix = "\e[1m▍\e[0m ";
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
    fprintf(fp, "\e[1;36m%-15s\e[39m ", fn);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(fp, fmt, ap);
    va_end(ap);
    fprintf(fp, "\e[0m\n");
    funlockfile(fp);
  }
#else
  #define TRACE_SET_CURRENT_M(m)  ((void)0)
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


// _Atomic(usize)* pbits_ptr(pbitset_t* bs, u32 bit)
#define pbits_ptr(bs, bit) \
  ( &(bs)->bits[(bit) / sizeof(usize)] )

// pbits_isset returns true if bit is set
static bool pbits_isset(pbitset_t* bs, u32 bit) {
  usize mask = (usize)1lu << (bit % sizeof(usize));
  return (AtomicLoad(pbits_ptr(bs, bit), memory_order_relaxed) & mask) != 0;
}

// pbits_set sets bit (to 1)
static void pbits_set(pbitset_t* bs, u32 bit) {
  usize mask = (usize)1lu << (bit % sizeof(usize));
  AtomicOr(pbits_ptr(bs, bit), mask, memory_order_release);
}

// pbits_clear clears bit (sets it to 0)
static void pbits_clear(pbitset_t* bs, u32 bit) {
  usize mask = (usize)1lu << (bit % sizeof(usize));
  AtomicAnd(pbits_ptr(bs, bit), ~mask, memory_order_release);
}




#define MNOTE_LOCKED  ((uintptr)1lu)


static void mnote_clear(mnote_t* n) {
  n->key = 0;
}


// note_wakeup notifies callers to note_sleep
static void mnote_wakeup(mnote_t* n) {
  uintptr key = AtomicLoad(&n->key, memory_order_relaxed);
  while (!AtomicCASRelaxed(&n->key, &key, MNOTE_LOCKED)) {
    // note: AtomicCAS loads current val of n.key into key on failure
  }
  if (key <= MNOTE_LOCKED) {
    assertf(key == 0, "double %s", __FUNCTION__);
    // Nothing was waiting. Done.
    return;
  }
  sema_signal(&((M*)key)->parksema, 1);
}


static void mnote_sleep(mnote_t* n, M* m) {
  uintptr key = 0;
  if (!AtomicCASRelaxed(&n->key, &key, (uintptr)&m->parksema)) {
    // note_wakeup called already
    // note: AtomicCAS loads current val of n.key into key on failure
    assert(key == MNOTE_LOCKED);
    return;
  }
  m->blocked = true;
  sema_wait(&m->parksema, -1);
  m->blocked = false;
}



static inline void m_acquire(M* m) { m->locks++; }
static inline void m_release(M* m) { m->locks--; }


// static void t_free(T* task) {
//   usize memsize = (usize)(((uintptr)task + (uintptr)sizeof(T)) - task->stacktop);
//   rmem_free(task->s->memalloc, (void*)task->stacktop, memsize);
// }

// void assert_tstatus(const T* t, tstatus_t expect_status)
// void assert_not_tstatus(const T* t, tstatus_t anything_but_status)
#if DEBUG
  #define assert_tstatus(t, expect_status) do{ \
    tstatus_t status__ = (expect_status); \
    assertf(t_status(t) == status__, "%s == %s", \
      tstatus_str(t->status), tstatus_str(status__)); \
  }while(0)
  #define assert_not_tstatus(t, anything_but_status) do{ \
    tstatus_t status__ = (anything_but_status); \
    assertf(t_status(t) != status__, "%s != %s", \
      tstatus_str(t->status), tstatus_str(status__)); \
  }while(0)
#else
  #define assert_tstatus(...) ((void)0)
  #define assert_not_tstatus(...) ((void)0)
#endif


// t_status returns the value of t->status.
// On many archs, including x86, this is just a plain load.
static inline tstatus_t t_status(T* t) {
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
    thread_yield();
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
  // if (RANDOMIZE_SCHEDULER && runnext && (fastrand() % 2) == 0)
  //   runnext = false;

  if (runnext) {
    // puts t in the p.runnext slot.
    T* oldnext = p->runnext;
    while (!AtomicCASAcqRel(&p->runnext, &oldnext, t)) {
      // Note that when AtomicCAS fails, it performs a loads of the current value
      // into oldnext, thus we can simply loop here without having to explicitly
      // load oldnext=p->runnext.
    }
    trace3("set p.runnext = T%llu", t->id);
    if (oldnext == NULL)
      return;
    // Kick the old runnext out to the regular run queue
    t = oldnext;
  }

  while (1) {
    // load-acquire, sync with consumers
    u32 head = AtomicLoadAcq(&p->runqhead);
    u32 tail = p->runqtail;
    if (tail - head < P_RUNQSIZE) {
      trace3("set p.runq[%u] = T%llu", tail % P_RUNQSIZE, t->id);
      p->runq[tail % P_RUNQSIZE] = t;
      // store memory_order_release makes the item available for consumption
      AtomicStoreRel(&p->runqtail, tail + 1);
      return;
    }
    // Put t and move half of the locally scheduled runnables to global runq
    if (p_runq_put_slow(p, t, head, tail))
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

  //trace("no runnext; trying dequeue p->runq");
  *inherit_time = false;

  while (1) {
    u32 head = AtomicLoadAcq(&p->runqhead); // load-acquire, sync with consumers
    u32 tail = p->runqtail;
    if (tail == head)
      return NULL;
    // trace3("loop2 tail != head; load p->runq[%u]", head % P_RUNQSIZE);
    T* t = p->runq[head % P_RUNQSIZE];
    // trace3("loop2 t => %p", t);
    // trace3("loop2 t => T%llu", t->id);
    if (AtomicCASRel(&p->runqhead, &head, head + 1)) // cas-release, commits consume
      return t;
    trace3("CAS failure; retry");
  }
}


// p_runq_grab grabs a batch of tasks from P's runnable queue into dst_runq.
// dst_runq is a ring buffer starting at dst_head.
// Returns number of grabbed tasks.
static u32 p_runq_grab(P* p, T* dst_runq[P_RUNQSIZE], u32 dst_head, bool steal_runnext) {
  trace3("source P%u (steal_runnext=%u)", p->id, steal_runnext);
  while (1) {
    // load-acquire, synchronize with other consumers
    u32 h = AtomicLoadAcq(&p->runqhead);
    // load-acquire, synchronize with the producer
    u32 t = AtomicLoadAcq(&p->runqtail);
    u32 n = t - h;
    n = n - n/2;
    if (n == 0) {
      if (steal_runnext) {
        // Try to steal from p.runnext.
        T* next = p->runnext;
        if (next != 0) {
          // ensure that p isn't about to run the T we are about to steal.
          // The important use case here is when the T running on p readies another T
          // and then almost immediately blocks.
          if (p->status == P_RUNNING)
            thread_yield();
          if (!AtomicCASRel(&p->runnext, &next, NULL))
            continue;
          dst_runq[dst_head % P_RUNQSIZE] = next;
          return 1;
        }
      }
      return 0;
    }
    if (n > (u32)(P_RUNQSIZE / 2)) // read inconsistent h and t
      continue;
    for (u32 i = 0; i < n; i++) {
      T* t = p->runq[(h + i) % P_RUNQSIZE];
      dst_runq[(dst_head + i) % P_RUNQSIZE] = t;
    }
    if (AtomicCASRel(&p->runqhead, &h, h + n)) // cas-release, commits consume
      return n;
  }
}


// p_runq_steal steals half of the tasks from src_p.runq, putting them on p.runq.
// Returns one of the stolen tasks, or NULL if failed.
static T* nullable p_runq_steal(P* p, P* src_p, bool steal_runnext) {
  u32 tail = AtomicLoad(&p->runqtail, memory_order_relaxed);
  u32 n = p_runq_grab(src_p, p->runq, tail, steal_runnext);
  if (n == 0) {
    trace3("could not grab any tasks");
    return NULL;
  }
  trace3("grabbed %u tasks from P%u", n, src_p->id);
  n--;
  T* t = p->runq[(tail + n) % P_RUNQSIZE];
  if (n == 0) // just one task
    return t;
  UNUSED u32 h = AtomicLoadAcq(&p->runqhead); // load-acquire, sync with consumers
  assertf(tail - h + n >= P_RUNQSIZE, "runq overflow");
  AtomicStoreRel(&p->runqtail, tail+n); // store-release, make available for consumption
  return t;
}


static rerr_t s_allt_add(rsched_t* s, T* t) {
  assert_not_tstatus(t, T_IDLE);

  rwmutex_lock(&s->allt.lock);

  if (s->allt.len == s->allt.cap) {
    rmem_t mem = { s->allt.ptr, (usize)s->allt.cap * sizeof(void*) };
    bool ok = rmem_resize(s->machine->malloc, &mem, (s->allt.cap + 64) * sizeof(void*));
    if UNLIKELY(!ok) {
      rwmutex_unlock(&s->allt.lock);
      return rerr_nomem;
    }
    s->allt.cap = mem.size / sizeof(void*);
    AtomicStore(&s->allt.ptr, mem.p, memory_order_release);
  }

  trace3("add s.allt[%u] = T%llu", s->allt.len, t->id);
  s->allt.ptr[s->allt.len++] = t;
  AtomicStore(&s->allt.len, s->allt.len, memory_order_release);

  rwmutex_unlock(&s->allt.lock);
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
  assertf(ctx_vaddr >= t->stack_lo, "%llx, %llx", ctx_vaddr, t->stack_lo);

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


// m_dropt removes the association between M and the current task m->currt
static void m_dropt(M* m) {
  assertnotnull(m->currt);
  t_save_ctx(m->currt);
  m->currt->m = NULL;
  m->currt = NULL;
}


static void s_startm(rsched_t* s, P* nullable p, bool spinning);


// s_wakep tries to add one more P to execute T's.
// Called when a T is made runnable (m_spawn.)
static void s_wakep(rsched_t* s) {
  if (AtomicLoad(&s->nidlep, memory_order_relaxed) == 0)
    return;

  // be conservative about spinning threads
  i32 zero = 0;
  i32 nmspinning = AtomicLoad(&s->nmspinning, memory_order_relaxed);
  if (nmspinning != 0 || !AtomicCASRelaxed(&s->nmspinning, &zero, 1)) {
    trace2("idle P's are spinning");
    return;
  }

  // start a new M
  s_startm(s, NULL, /*spinning*/true);
}


// m_switchtask configures m to execute task t.
// It first saves the current task's state, then restores the state of t.
static void m_switchtask(M* m, T* nullable t) {
  if (t) trace("-> T%llu", t->id);
  else   trace("-> T-");

  assert(t != m->currt);

  if (m->currt)
    m_dropt(m);

  if (t) {
    m->currt = t;
    t->m = m;
    t_restore_ctx(t);
  }
}


static T* nullable task_create(M* m, u64 stack_vaddr, usize stacksize, usize instrc) {

  // TODO: move this T-struct layout into rsched_loadrom so that T is before exeinfo_t;
  // then we might not need CTX, since t is always at aligned(VM_ADDR_MAX-sizeof(T)).

  // task memory layout:
  //
  //               ┌─────────────┬─ stack_lo
  //               │             │
  //               ~      ↑      ~
  //               │    stack    │← tctx_t on top when task is idle
  //               ├─────────────┼─ stack_hi (CTX and initial SP)
  //               │  T struct   │
  //  stack_vaddr ─┴─────────────┘
  //
  assert(IS_ALIGN2(stack_vaddr, STK_ALIGN));
  vm_cache_t* vm_cache = &m->vm_cache[VM_PERM_RW];
  void* stackptr =
    (void*)VM_TRANSLATE(vm_cache, &m->s->vm_pagedir, stack_vaddr, STK_ALIGN);
  assertf(stackptr != NULL, "stack memory not mapped for 0x%llx", stack_vaddr);

  //dlog("stack 0x%llx => haddr %p ... %p",
  //  stack_vaddr, stackptr - stacksize, stackptr);

  // space for T, below stack, with strongest alignment
  usize tsize = ALIGN2(sizeof(T), MAX(_Alignof(T), STK_ALIGN));

  T* t = stackptr - tsize;
  t->stack_lo = stack_vaddr - stacksize;
  t->stack_hi = stack_vaddr - (u64)tsize - sizeof(u64);
  t->sp = t->stack_hi; // current stack pointer

  // push final return value to stack
  static_assert(_Alignof(T) <= STK_ALIGN, "assuming u64 alignment of stack");
  u64* sp = (void*)t - sizeof(u64);
  *sp = (u64)(instrc - EPILOGUE_LEN); // instruction offset of epilogue

  // initialize tctx
  tctx_t* ctx = (tctx_t*)ALIGN2_FLOOR((uintptr)sp - sizeof(tctx_t), TCTX_ALIGN);
  ctx->iregs[RSM_MAX_REG - RSM_NTMPREGS - 1] = (u64)(uintptr)t; // CTX reg

  //dlog("store ctx to stack at vaddr 0x%llx (haddr %p)", tctx_vaddr(t->sp), ctx);

  trace2("T@%p+%zu stack 0x%llx-0x%llx", t, tsize, t->stack_lo, t->stack_hi);

  return t;
}


// m_spawn creates a new T running fn with argsize bytes of arguments.
// Put it on the queue of T's waiting to run.
static T* nullable m_spawn(
  M* m,
  const rin_t* instrv, usize instrc, usize pc,
  u64 stack_vaddr, usize stacksize,
  rerr_t* errp)
{
  rerr_t err = 0;

  // disable preemption
  m_acquire(m);

  // create main task
  T* newt = task_create(m, stack_vaddr, stacksize, instrc);
  if (!newt) {
    err = rerr_nomem;
    goto onerr;
  }

  // setup program instruction array, program counter and task ID
  newt->pc = pc;
  newt->instrc = instrc;
  newt->instrv = instrv;
  //newt->rodata = rodata; // TODO vm
  newt->id = AtomicAdd(&m->s->tidgen, 1, memory_order_acquire);

  // limit IDs to 0..I64_MAX
  if (newt->id > I64_MAX) {
    // atomically: if tidgen==newt->id+1 then tidgen=1.
    // Note that we start over at 1 instead of 0 since 0 is used for the main task.
    newt->id++; // expected value
    AtomicCASRel(&m->s->tidgen, &newt->id, 1);
    newt->id = AtomicAdd(&m->s->tidgen, 1, memory_order_acquire);
    assert(newt->id <= I64_MAX);
  }

  // add the task to the scheduler
  t_setstatus(newt, T_DEAD);
  if ((err = s_allt_add(m->s, newt)))
    goto onerr;

  // set status to runnable
  t_setstatus(newt, T_RUNNABLE);

  // get P associated with current M
  P* p = assertnotnull(m->p);

  // re-enable preemption
  m_release(m);

  // add newt to run queue
  p_runq_put(p, newt, /*runnext*/true);

  // wake P (unless we are spawning the main task)
  if (m->s->main_started)
    s_wakep(m->s);

  *errp = 0;
  return newt;

onerr:
  m_release(m); // re-enable preemption
  *errp = err;
  return NULL;
}


i64 task_spawn(T* t, usize newtask_pc, const u64 args[RSM_NARGREGS]) {
  M* m = assertnotnull(t->m);

  // new task will run same code as its parent task
  const rin_t* instrv = t->instrv;
  usize instrc = t->instrc;

  // stack
  // TODO: allocate virtual memory for stack
  u64 stack_vaddr = STACK_VADDR - PAGE_SIZE * 2; // FIXME
  usize stacksize = PAGE_SIZE; // FIXME

  // TODO: copy args
  //memcpy(args)

  rerr_t err;
  T* newt = m_spawn(m, instrv, instrc, newtask_pc, stack_vaddr, stacksize, &err);
  if (!newt)
    return (i64)err;
  trace("-> T%llu (pc %lu)", newt->id, newtask_pc);
  return (i64)newt->id;
}


static void p_init(P* p, rsched_t* s, u32 id) {
  memset(p, 0, sizeof(P));
  p->id = id;
  p->status = P_IDLE;
  p->s = s;
  safecheckx(mutex_init(&p->timers_lock) == 0);
}


// static void p_dispose(P* p) {
//   mutex_dispose(&p->timers_lock);
// }


// p_acquire_m associates P with M
static void p_acquire_m(P* p, M* m) {
  trace2("P%u", p->id);
  assertf(p->m == NULL, "P%u in use with other M%u", p->id, p->m->id);
  assertf(m->p == NULL, "M%u in use with other P%u", m->id, m->p->id);
  assert(p->status == P_IDLE);
  m->p = p;
  p->m = m;
  AtomicStoreRel(&p->status, P_RUNNING);
}

// p_release_m disassociates P from its current M
static void p_release_m(P* p) {
  trace2("");
  assertf(p->status == P_RUNNING, "%u", p->status);
  M* m = assertnotnull(p->m);
  m->p = NULL;
  p->m = NULL;
  AtomicStoreRel(&p->status, P_IDLE);
}


// p_runq_isempty returns true if p has no Ts on its local run queue
static bool p_runq_isempty(P* p) {
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
// As a compromise, the timer mask is updated only on s_idlep_get / idlep_put.
// A running P (returned by s_idlep_get) may add a timer at any time, so its mask
// must be set. An idle P (passed to idlep_put) cannot add new timers while
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
  mutex_lock(&p->timers_lock);
  if (AtomicLoadAcq(&p->ntimers) == 0)
    pbits_clear(&p->s->timerp_mask, p->id);
  mutex_unlock(&p->timers_lock);
}


// s_idlep_get tries to get a P from s.idlep list.
// s.lock must be held.
static P* nullable s_idlep_get(rsched_t* s) {
  s_assert_locked(s);
  P* p = s->idlep;
  if (!p) {
    trace2("P- (no idle P)");
    return NULL;
  }
  pbits_set(&s->timerp_mask, p->id);
  pbits_clear(&s->idlep_mask, p->id);
  s->idlep = p->nextp; // dequeue
  AtomicSub(&s->nidlep, 1, memory_order_release);
  trace2("P%u", p->id);
  return p;
}

// s_idlem_get tries to get a P from s.idlem list.
// s.lock must be held.
static M* nullable s_idlem_get(rsched_t* s) {
  s_assert_locked(s);
  M* m = s->idlem;
  if (!m) {
    trace2("M- (no idle M)");
    return NULL;
  }
  s->idlem = m->nextm;
  AtomicSub(&s->nidlem, 1, memory_order_relaxed);
  trace2("M%u", m->id);
  return m;
}


// idlep_put puts P on s.idlep list.
// p.s.lock must be held.
static void idlep_put(P* p) {
  trace2("P%u", p->id);
  s_assert_locked(p->s);
  assert(p_runq_isempty(p) /* trying to put P to sleep with runnable Ts */);
  p_update_timerp_mask(p);
  pbits_set(&p->s->idlep_mask, p->id);
  // TODO: atomic ops on s.idlep?
  p->nextp = p->s->idlep;
  p->s->idlep = p;
  AtomicAdd(&p->s->nidlep, 1, memory_order_release);
}


// idlem_put puts M on s.idlem list.
// m.s.lock must be held.
static void idlem_put(M* m) {
  trace2("M%u", m->id);
  s_assert_locked(m->s);
  assertnull(m->p);
  // TODO: atomic ops on s.idlem?
  m->nextm = m->s->idlem;
  m->s->idlem = m;
  AtomicAdd(&m->s->nidlem, 1, memory_order_release);
}


// s_runq_prepend puts T in front of the global runnable queue, to be run ASAP.
// s.lock must be held.
static void s_runq_prepend(rsched_t* s, T* t) {
  s_assert_locked(s);
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
  s_assert_locked(s);
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


// s_runq_pop is a helper for s_runq_get
static T* s_runq_pop(rsched_t* s) {
  s_assert_locked(s);
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
  s_assert_locked(s);
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


static bool tlist_empty(const tlist_t* l) {
  return l->head == NULL;
}

static void tlist_push(tlist_t* l, T* t) {
  t->schedlink = l->head;
  l->head = t;
  l->len++;
}

static T* nullable tlist_pop(tlist_t* l) {
  T* t = l->head;
  if (t) {
    l->head = t->schedlink;
    l->len--;
  }
  return t;
}


// When p.freet.len reaches P_FREET_WATERMARK_HIGH, move T's to global s.freet
// so that there are P_FREET_WATERMARK_LOW T's left in p.freet.
#define P_FREET_WATERMARK_HIGH  64u
#define P_FREET_WATERMARK_LOW   32u


// p_freet_add adds T to P's freet list.
// Moves tasks to global s.freet if p.freet is full.
static void p_freet_add(P* p, T* t) {
  assert_tstatus(t, T_DEAD);

  u64 stacksize = t->stack_hi - t->stack_lo;
  trace3("T%llu -> P%u.freet (stacksize %llu)", t->id, p->id, stacksize);

  tlist_push(&p->freet, t);

  if (p->freet.len < P_FREET_WATERMARK_HIGH)
    return;

  // move some T's from local p.freet list to global s.freet list
  static_assert(P_FREET_WATERMARK_LOW > 0, "");
  static_assert(P_FREET_WATERMARK_LOW <= P_FREET_WATERMARK_HIGH, "");

  while (p->freet.len >= P_FREET_WATERMARK_LOW) {
    T* t = tlist_pop(&p->freet);
  }
}


// handoff_p hands off P from syscall or locked M
static void handoff_p(P* p) {
  // we must start an M in any situation where s_findrunnable would return a T to run.

  // if there are tasks waiting to run, start P on an M
  if (!p_runq_isempty(p) || p->s->runq.len != 0) {
    s_startm(p->s, p, /*spinning*/false);
    return;
  }

  dlog("TODO");
}


static rerr_t m_schedule(M* m);


static void m_droptask(M* m) {
  trace2("");
  assertnotnull(m->currt);
  assert(m->currt->m == m);

  m->currt->m = NULL;
  m->currt = NULL;
}


static void m_park(M* m) {
  trace2("");
  assertnotnull(m);
  mnote_sleep(&m->park, m);
  mnote_clear(&m->park);
}


// Stops execution of the current m until new work is available.
// Returns with acquired P. [TODO]
static void m_stop(M* m) {
  trace2("M%u", m->id);
  assertf(m->p == NULL, "M%u still associated with P%u", m->id, m->p->id);

  mutex_lock(&m->s->lock);
  idlem_put(m);
  mutex_unlock(&m->s->lock);

  m_park(m);

  // TODO: nextp
  // p_acquire_m(m->nextp, m);
  // m->nextp = NULL;
}


static void s_check_deadlock(rsched_t* s) {
  dlog("TODO checkdead");
}


static rerr_t m_exit_main(M* m) {
  trace("");

  P* p = m->p;
  if (p) {
    p_release_m(p);
    handoff_p(p);
  }

  mutex_lock(&m->s->lock);
  m->s->nmfreed++;
  s_check_deadlock(m->s);
  mutex_unlock(&m->s->lock);

  return 0;
}


// m_exit tears down and exits the current thread
static rerr_t m_exit(M* m) {
  if (m == &m->s->m0)
    return m_exit_main(m);

  trace("TODO");
  // TODO

  return 0;
}


// m_start is the entry-point for new M's, the thread main function.
// M doesn't have a P yet.
NOINLINE static rerr_t m_start(M* m) {
  TRACE_SET_CURRENT_M(m);

  // Associate the assigned P with M (unless M is the main thread)
  // Note: m->nextp is set by p_newm
  if (m != &m->s->m0) {
    assertnotnull(m->nextp);
    p_acquire_m(m->nextp, m);
    m->nextp = NULL;
  }

  MUSTTAIL return m_schedule(m);
}


static void m_dispose(M* m) {
  sema_dispose(&m->parksema);
}


static rerr_t m_init(M* m, rsched_t* s, u64 id) {
  m->id = id;
  m->s = s;
  mnote_clear(&m->park);
  safecheckx(sema_init(&m->parksema, 0) == 0);

  // virtual memory caches
  for (usize i = 0; i < countof(m->vm_cache); i++)
    vm_cache_init(&m->vm_cache[i]);

  return 0;
}


// p_allocm allocates a new M, not associated with any thread.
// Can use P for allocation context if needed.
static M* nullable p_allocm(P* p) {
  rsched_t* s = p->s;
  rwmutex_rlock(&s->allocm_lock);

  if (s->freem)
    trace("TODO: release freem list");

  M* m = rmem_alloct(s->machine->malloc, M);
  if UNLIKELY(!m) {
    trace("failed to allocate new M");
  } else {
    u64 id = AtomicAdd(&s->midgen, 1, memory_order_acquire);
    memset(m, 0, sizeof(*m));
    rerr_t err = m_init(m, s, id);
    if UNLIKELY(err) {
      rmem_freet(s->machine->malloc, m);
      trace("failed to initialize new M (rerr %d)", err);
    }
  }

  rwmutex_runlock(&s->allocm_lock);
  return m;
}


// p_newm allocates a new M and associates it with P (sets p->m)
static rerr_t p_newm(P* p, bool start_spinning) {
  assertnull(p->m);

  M* m = p_allocm(p);
  if UNLIKELY(m == NULL)
    return rerr_nomem;

  // assign P to M, which m_start will use with p_acquire_m in its OS thread
  assertnull(m->nextp);
  m->nextp = p;
  m->spinning = start_spinning;

  trace2("M%u", m->id);

  rwmutex_rlock(&p->s->exec_lock);
  m_spawn_osthread(m, m_start);
  rwmutex_runlock(&p->s->exec_lock);
  return 0;
}


// s_startm schedules some M to run P, creating an M if necessary.
// If p==NULL, tries to get an idle P, if no idle P's does nothing.
static void s_startm(rsched_t* s, P* nullable p, bool spinning) {
  trace2("");

  mutex_lock(&s->lock);

  if (!p && !(p = s_idlep_get(s))) {
    trace2("no idle P's");
    mutex_unlock(&s->lock);
    if (spinning) {
      // caller incremented nmspinning, but there are no idle P's,
      // so it's okay to just undo the increment and give up.
      UNUSED u32 v = AtomicSub(&s->nmspinning, 1, memory_order_release) - 1;
      assertf(v != 0xFFFFFFFF, "nmspinning decrement does not match increment");
    }
  }

  // try to acquire an idle M
  M* m = s_idlem_get(s);

  mutex_unlock(&s->lock);

  if (!m) {
    // No M is available, we must release s.lock and call p_newm.
    // However, we already own a P to assign to the M.
    //
    // Once s.lock is released, another T (e.g., in a syscall),
    // could find no idle P while checkdead finds a runnable T but
    // no running M's because this new M hasn't started yet, thus
    // throwing in an apparent deadlock.
    //
    // Avoid this situation by pre-allocating the ID for the new M,
    // thus marking it as 'running' before we drop s.lock. This
    // new M will eventually run the scheduler to execute any
    // queued T's.
    p_newm(p, spinning);
    return;
  }

  assert(!m->spinning);
  assertf(m->nextp == 0, "M should not have a P");
  assertf(spinning && !p_runq_isempty(p), "P should not have runnable tasks");

  m->spinning = spinning;
  m->nextp = p;
  mnote_wakeup(&m->park);
}


// stealresult_t is the result from m_steal_work
typedef struct {
  T* nullable t;
  u64         now;
  u64         poll_until;
  bool        new_work;
} stealresult_t;

typedef struct {
  u32 _i, _len, _inc;
  u32 index;
} randord_it_t;


static u32 gcd(u32 a, u32 b) {
  while (b != 0) {
    // a, b = b, a%b
    u32 tmp = a % b;
    a = b;
    b = tmp;
  }
  return a;
}


static u32 randord_init(u32* coprimev, u32 nindices) {
  u32 coprimec = 0;
  for (u32 i = 1; i <= nindices; i++) {
    if (gcd(i, nindices) == 1)
      coprimev[coprimec++] = i;
  }
  return coprimec;
}


static randord_it_t randord_it(
  const u32* coprimev, u32 coprimec, u32 nindices, u32 seed)
{
  return (randord_it_t){
    ._i   = 0,
    ._len = coprimec,
    ._inc = coprimev[seed % nindices],
    .index = seed % coprimec,
  };
}

inline static bool randord_it_next(randord_it_t* it) {
  if (it->_i == it->_len)
    return false;
  it->_i++;
  it->index = (it->index + it->_inc) % it->_len;
  return true;
}


// m_steal_work attempts to steal work from other P's.
// Marks m as "spinning".
static stealresult_t m_steal_work(M* m, bool* inherit_time) {
  stealresult_t res = {0};
  *inherit_time = false;

  P* p = assertnotnull(m->p);
  rsched_t* s = m->s;
  u32 steal_attempts = MIN(4u, s->nprocs);

  for (u32 i = 0; i < steal_attempts; i++) {
    trace3("attempt %u of %u", i+1, steal_attempts);

    bool is_final_attempt = i == steal_attempts-1;

    // pick victim P's at random
    randord_it_t it = randord_it(s->stealord, s->stealordlen, s->nprocs, fastrand());
    while (randord_it_next(&it)) {
      P* p2 = s->allp[it.index];
      if (p2 == p)
        continue;
      trace3("victim P%u", p2->id);

      if (is_final_attempt && pbits_isset(&s->timerp_mask, it.index)) {
        // Steal timers from p2. This call to p_check_timers is the only place where
        // we might hold a lock on a different P's timers. We do this once on the
        // last pass before checking runnext because stealing from the other P's runnext
        // should be the last resort, so if there are timers to steal do that first.
        //
        // We only check timers on one of the stealing iterations because the time
        // stored in now doesn't change in this loop and checking the timers for each P
        // more than once with the same value of now is probably a waste of time.
        trace("TODO p_check_timers");
      }

      // don't bother to attempt to steal if p2 is idle
      if (pbits_isset(&s->idlep_mask, it.index))
        continue;

      // steal half of p2's runq
      T* t = p_runq_steal(p, p2, is_final_attempt);
      if (t) {
        trace2("T%llu (stolen from P%u)", t->id, p2->id);
        res.t = t;
        goto end;
      }

    }
  }

  trace2("nothing found");

end:
  return res;
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
    if (pbits_isset(&s->idlep_mask, id) || p_runq_isempty(p))
      continue;
    mutex_lock(&s->lock);
    p = s_idlep_get(s);
    mutex_unlock(&s->lock);
    return p; // ok to return NULL (don't bother checking other P's)
  }
  return NULL;
}


static void m_resetspinning(M* m) {
  trace2("M%u", m->id);
  assert(m->spinning);
  m->spinning = false;
  UNUSED i32 nmspinning = AtomicSub(&m->s->nmspinning, 1, memory_order_release);
  assertf(nmspinning > 0, "negative nmspinning %d", m->s->nmspinning);
  // M wakeup policy is deliberately somewhat conservative, so check if we
  // need to wakeup another P here
  s_wakep(m->s);
}


// s_findrunnable finds a runnable task to execute.
// Blocks until a task is found, or (returns NULL) if all tasks have exited.
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
  trace3("try p.runq");
  t = p_runq_get(p, inherit_time);
  if (t) {
    trace3("found T%llu on p.runq", t->id);
    return t;
  }

  // global runq
  trace3("try s.runq");
  if (AtomicLoad(&s->runq.len, memory_order_relaxed)) {
    mutex_lock(&s->lock);
    t = s_runq_get(s, p, 0); // 0 means "move all to P"
    mutex_unlock(&s->lock);
    if (t) {
      trace3("found T%llu on s.runq", t->id);
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
  i32 npbusy = (i32)( s->nprocs - AtomicLoadAcq(&s->nidlep) );
  if (m->spinning || 2*nmspinning < npbusy) {
    if (!m->spinning) {
      m->spinning = true;
      AtomicAdd(&s->nmspinning, 1, memory_order_release);
    }
    stealresult_t r = m_steal_work(m, inherit_time);
    if (r.t)
      return r.t;
    now = r.now;
    // Running a timer may have made some task ready, so try again
    if (r.new_work)
      goto top; // retry while loop
    if (r.poll_until && (poll_until == 0 || r.poll_until < poll_until))
      poll_until = r.poll_until;
  }

  // if we get here, there's probably no work
  trace2("out of work");

  // Release P for other M's to use.
  mutex_lock(&s->lock);
  // Before we drop our P, make a snapshot of nprocs
  u32 nprocs = s->nprocs;
  // We might just find a task on the global s.runq, but probably not.
  if (AtomicLoad(&s->runq.len, memory_order_relaxed) > 0) {
    t = s_runq_get(s, p, 0); // 0 means "move all to P"
    assertnotnull(t); // lock held; runq.len not able to change
    mutex_unlock(&s->lock);
    trace3("found T%llu in s.runq", t->id);
    *inherit_time = false;
    return t;
  }
  p_release_m(p); // disassociate P from M
  idlep_put(p);
  mutex_unlock(&s->lock);

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
      p_acquire_m(p, m);
      m->spinning = true;
      AtomicAdd(&s->nmspinning, 1, memory_order_release);
      trace3("found P%u with non-empty runq; retrying", p->id);
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

  // If we get here, either some tasks are waiting for work or all tasks are dead.

  // check if there are any live tasks
  bool has_live_tasks = false;
  rwmutex_rlock(&s->allt.lock);
  for (u32 i = 0, len = AtomicLoad(&s->allt.len, memory_order_relaxed); i < len; i++) {
    if (s->allt.ptr[i]->status != T_DEAD) {
      has_live_tasks = true;
      break;
    }
  }
  rwmutex_runlock(&s->allt.lock);

  // look again if there are live tasks
  if (has_live_tasks) {
    // stop the M
    m_stop(m);
    // M was woken up
    goto top;
  }

  // no tasks left
  trace("no live tasks");
  return NULL;
}


// m_schedule performs scheduling: in a loop, it finds a runnable task and executes it.
// Returns when all run queues are empty.
static rerr_t m_schedule(M* m) {
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
      mutex_lock(&s->lock);
      t = s_runq_get(s, p, 1);
      mutex_unlock(&s->lock);
      if (t) trace2("random runq steal of T%llu", t->id);
    }

    // if M does not have an associated P, wait for a P to become available
    if (!p) {
      mutex_lock(&s->lock);
      p = s_idlep_get(s);
      mutex_unlock(&s->lock);
      if (!p)
        panic("TODO wait for a P to become available");
      p_acquire_m(p, m);
    }

    p->preempt = false;

    // m_findrunnable blocks until work is available or all tasks have exited
    if (!t) {
      t = m_findrunnable(m, &inherit_time);
      if (!t)
        return m_exit(m);
    }

    // This thread is going to run a task and is not spinning anymore,
    // so if it was marked as spinning we need to reset it now and potentially
    // start a new spinning M.
    if (m->spinning)
      m_resetspinning(m);

    // save any current task's (m->currt) state and restore state of t (sets m->currt)
    m_switchtask(m, t);

    // Assign t->m before entering T_RUNNING so running Ts have an M
    t_casstatus(t, T_RUNNABLE, T_RUNNING);
    t->waitsince = 0;

    p->schedtick += (u32)inherit_time;

    // execute task
    trace3("eval (pc %lu)", t->pc);
    t->pc = rsched_eval(t, m->iregs, t->instrv, t->pc);
    // returns here when the task has been parked with task_park
  }
}


// enter_syscall is called when a task is about to enter a syscall, locking up its M.
// M's P is released and made available for other tasks to use.
// A matching call to exit_syscall resumes the task after the syscall has completed.
void enter_syscall(T* t) {
  trace("");

  t_casstatus(t, T_RUNNING, T_SYSCALL);

  M* m = assertnotnull(t->m);
  P* p = assertnotnull(m->p);

  // save pointer to P attached to M for exit_syscall
  m->oldp = p;

  // release the P so it can be used by other tasks waiting to run
  p_release_m(p); // disassociate P from M
  AtomicStoreRel(&p->status, P_SYSCALL);
}


// The task t exited its system call.
// Arrange for it to run on a processor again.
bool exit_syscall(T* t, int priority) {
  assert_tstatus(t, T_SYSCALL);

  M* m = assertnotnull(t->m);
  assertnull(m->p);

  // Try to re-acquire the P that M used when entering the syscall
  P* p = m->oldp;
  m->oldp = NULL;
  pstatus_t expect_pstatus = P_SYSCALL;
  if (p && AtomicCASAcqRel(&p->status, &expect_pstatus, P_IDLE)) {
    // There's a processor for us, so we can run.
    // See "if (p)" branch further ahead.
    trace("re-acquired oldp P%u", p->id);
  } else {
    // try to get an idle P
    mutex_lock(&m->s->lock);
    p = s_idlep_get(m->s);
    mutex_unlock(&m->s->lock);
    if (p)
      trace("acquired idle P%u", p->id);
  }
  if (p) {
    // we have a processor; continue execution
    p_acquire_m(p, m);
    t_casstatus(t, T_SYSCALL, T_RUNNING);
    return true;
  }

  // slow path; no available P's
  trace("no available P");

  // unlink T from M
  m_dropt(m);
  t_casstatus(t, T_SYSCALL, T_RUNNABLE);

  // put task on global run queue
  mutex_lock(&m->s->lock);
  if (priority > 0) {
    s_runq_prepend(m->s, t);
  } else {
    // m_droptask(m); // release M to allow other waiting tasks to run
    s_runq_append(m->s, t);
  }
  mutex_unlock(&m->s->lock);

  // suspend M and return false to make caller stop execution
  m_stop(m);
  return false;
}


// task_exit is called by the execution engine when a task exits.
void task_exit(T* t) {
  trace("");
  assert(t->m->currt == t);

  t_setstatus(t, T_DEAD);
  t->waitsince = 0;
  if (t->parent) {
    dlog("TODO: [SC] wake up waiting parent");
    t->parent = NULL;
  }

  M* m = t->m;
  P* p = m->p;

  m_droptask(m);  // disassociate T from M

  // put T on P's freet list
  p_freet_add(p, t);

  // return to m_schedule
}


// task_park takes a task out of running state (disassoc. M & P from T.)
// Must be explicitly resumed with a call to task_unpark
//
// task_park is called by the execution engine when a task is to be suspended.
// "reason" tells us why the task is being suspended.
void task_park(T* t, tstatus_t reason) {
  panic("TODO");
}

// static void task_unpark(T* t, tstatus_t reason) {
//   trace("%s", tstatus_str(reason));
//   t_setstatus(t, T_RUNNABLE);
//   P* p = assertnotnull(assertnotnull(t->m)->p);
//   //m_droptask(t->m);
//   p_runq_put(p, t, /*next*/false);
// }


rerr_t rsched_init(rsched_t* s, rmachine_t* machine) {
  rerr_t err;
  memset(s, 0, sizeof(rsched_t));

  s->machine = machine;

  if ((err = mutex_init(&s->lock))) return err;
  if ((err = rwmutex_init(&s->exec_lock))) return err;
  if ((err = rwmutex_init(&s->allocm_lock))) return err;
  if ((err = rwmutex_init(&s->allt.lock))) return err;
  if ((err = mutex_init(&s->freet_lock))) return err;

  // virtual memory page directory
  if ((err = vm_pagedir_init(&s->vm_pagedir, machine->mm)))
    return err;

  // initialize main M (id=0) on current OS thread
  s->midgen = 1;
  s->maxmcount = 1000;
  err = m_init(&s->m0, s, 0);
  if UNLIKELY(err)
    goto error;

  TRACE_SET_CURRENT_M(&s->m0);

  // create processors
  u32 nprocs = 2;
  assertf(nprocs > 0 && nprocs <= S_MAXPROCS, "%u", nprocs);
  s->nprocs = nprocs;
  trace3("s.nprocs=%u", nprocs);
  p_init(&s->p0, s, 0);
  s->allp[0] = &s->p0;
  s->nidlep = nprocs - 1;
  for (u32 p_id = 1; p_id < nprocs; p_id++) {
    P* p = rmem_alloct(s->machine->malloc, P);
    if (!p) {
      // TODO: free other P's we allocated
      err = rerr_nomem;
      goto error;
    }
    p_init(p, s, p_id);
    s->allp[p_id] = p;
    // add p to idlep list
    p->nextp = s->idlep;
    s->idlep = p;
    // flag p as being idle
    pbits_set(&s->idlep_mask, p_id);
  }

  // randomize steal order
  s->stealordlen = randord_init(s->stealord, nprocs);

  // associate P0 with M0
  P* p = s->allp[0];
  p->status = P_IDLE;
  p_acquire_m(p, &s->m0); // associate P and M (p->m=m, m->p=p, p.status=P_RUNNING)

  return 0;
error:
  vm_pagedir_dispose(&s->vm_pagedir);
  return err;
}


void rsched_dispose(rsched_t* s) {
  mutex_dispose(&s->lock);
  rwmutex_dispose(&s->exec_lock);
  rwmutex_dispose(&s->allocm_lock);
  rwmutex_dispose(&s->allt.lock);
  mutex_dispose(&s->freet_lock);
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
static rerr_t rsched_loadrom(
  rsched_t* s, rrom_t* rom, rmem_t basemem, usize* stacksizep)
{
  // 1. load rom image into base memory:
  //   basemem
  //   ├────────┬──────────┬──────────────────────┬─────────────────────┐
  //   │ header │ code     │ data                 │ ignored_section ... │
  //   ├────────┴──────────┴──────────────────────┴─────────────────────┘
  //   page 1
  //
  // 2. move code and data sections to page boundaries,
  //    overwriting header and other ROM sections:
  //   ┌──────┬──────────┬─────────────────────────────────┐
  //   │ code │ epilogue │ data                            │
  //   ├──────┴──────────┼────────────────┬────────────────┘
  //   page 1            page 2           page 3
  //                     vm start
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

  // move code section to front
  usize codesize = rom->codelen * sizeof(rin_t);
  memmove(basemem.p, rom->code, codesize);

  // epilogue code
  // must fit in ROM header so that we can guarantee space in basemem
  // and alignment of data. This constraint can be lifted by adding logic to
  // rsched_alloc_basemem and add to codesize as needed.
  static_assert(EPILOGUE_LEN*sizeof(rin_t) <= sizeof(rromimg_t),
    "epilogue larger than rom header");
  const rin_t epilogue[EPILOGUE_LEN] = {
    RSM_MAKE_Au(rop_SYSCALL, SC_TEXIT),
  };
  memcpy(basemem.p + codesize, epilogue, sizeof(epilogue));

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

  // spawn main task
  const rin_t* instrv = basemem.p;
  usize instrc = rom->codelen + EPILOGUE_LEN;
  u64 pc = 0; // TODO: use instruction address of "main" function
  T* t = m_spawn(&s->m0, instrv, instrc, pc, STACK_VADDR, stacksize, &err);
  if (!t)
    goto end;
  s->main_started = true;

  // enter scheduler loop in M0
  err = m_start(&s->m0);

  // finish
  rsched_unloadrom(s, rom);

end:
  rmm_freepages(s->machine->mm, basemem.p);
  return err;
}

