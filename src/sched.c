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
#define STK_ALIGN    8lu       // stack alignment (== sizeof(u64))
#define STK_MIN      2048lu    // minium stack size
#define STK_MAX      1lu * MiB // maximum stack size
static_assert(STK_MIN % STK_ALIGN == 0, "STK_MIN not aligned to STK_ALIGN");
static_assert(STK_MIN >= ALIGN2_X(sizeof(tctx_t), TCTX_ALIGN), "STK_MIN too small");

#define HEAP_ALIGN   8  // alignment of heap memory address

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

enum PStatus {
  P_IDLE = 0,
  P_RUNNING, // Only this P is allowed to change from P_RUNNING
  P_SYSCALL,
  P_DEAD,
};


// stackbase for a task
#define TASK_STACKBASE(t) ((void*)ALIGN2_FLOOR((uintptr)(t),STK_ALIGN))


_Thread_local T* _g_t = NULL; // current task on current OS thread


#define trace schedtrace

#if defined(SCHED_TRACE) && !defined(RSM_NO_LIBC)
  #include <stdio.h>
  void _schedtrace(const char* fmt, ...) {
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
#endif


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


// m_switchtask configures m to execute task t.
// It first saves the current task's state, then restores the state of t.
static void m_switchtask(M* m, T* nullable t) {
  if (t) trace("-> T%llu", t->id);
  else   trace("-> T-");

  assert(t != m->currt);

  if (m->currt) {
    // save context
    panic("TODO: save context");
  }

  m->currt = t;
  if (!t) {
    dlog("TODO");
    return;
  }

  // restore context

  // A suspended task's tctx is stored on the task's stack, just above SP
  u64 ctx_vaddr = tctx_vaddr(t->sp);
  assertf(ctx_vaddr >= t->stacktop, "%llx, %llx", ctx_vaddr, t->stacktop);

  // get backing memory
  vm_cache_t* vm_cache = &m->vm_cache[VM_PERM_RW];
  tctx_t* ctx =
    (tctx_t*)VM_TRANSLATE(vm_cache, &m->s->vm_pagedir, ctx_vaddr, TCTX_ALIGN);

  dlog("load ctx from stack at vaddr 0x%llx", ctx_vaddr);


  assertnotnull(ctx);


  // tctx_t layout:
  //   [byte 0] F{RSM_NTMPREGS} F{RSM_NTMPREGS+1} F{...} F{RSM_NREGS-1} ...
  //        ... R{RSM_NTMPREGS} R{RSM_NTMPREGS+1} R{...} R{RSM_NREGS-2}

  for (usize i = 0; i < countof(ctx->fregs); i++)
    m->fregs[i + RSM_NTMPREGS] = ctx->fregs[i];

  for (usize i = 0; i < countof(ctx->iregs); i++)
    m->iregs[i + RSM_NTMPREGS] = ctx->iregs[i];

  m->iregs[RSM_MAX_REG] = t->sp;
}


static T* nullable task_create(M* m, u64 stack_vaddr, usize stacksize) {
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
  void* stackptr = (void*)VM_TRANSLATE(vm_cache, &m->s->vm_pagedir, stack_vaddr, STK_ALIGN);
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
  *sp = (u64)MAIN_RET_PC; // TODO: actual return address

  // initialize tctx
  tctx_t* ctx = (tctx_t*)ALIGN2_FLOOR((uintptr)sp - sizeof(tctx_t), TCTX_ALIGN);
  ctx->iregs[RSM_MAX_REG - RSM_NTMPREGS - 1] = (u64)(uintptr)t; // CTX reg

  dlog("store ctx to stack at vaddr 0x%llx (haddr %p)", tctx_vaddr(t->sp), ctx);

  trace("T@%p+%zu stack 0x%llx-0x%llx", t, tsize, t->stacktop, stack_vaddr);

  return t;
}


// sched_spawn creates a new T running fn with argsize bytes of arguments.
// Put it on the queue of T's waiting to run.
// The compiler turns a go statement into a call to this.
static rerr_t sched_spawn(
  rsched_t* s, const rin_t* iv, usize ic, u64 stack_vaddr, usize stacksize, usize pc)
{
  T* t = t_get();
  M* m = t->m;
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
  newtask->id = AtomicAdd(&s->tidgen, 1, memory_order_acquire);

  // add the task to the scheduler
  t_setstatus(newtask, T_DEAD);
  if ((err = sched_allt_add(s, newtask)))
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

  // virtual memory caches
  for (usize i = 0; i < countof(m->vm_cache); i++)
    vm_cache_init(&m->vm_cache[i]);

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

  rsched_exec(t, m->iregs, t->instrv, t->pc);
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
  memset(p, 0, sizeof(P));
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


rerr_t rsched_init(rsched_t* s, rmachine_t* machine) {
  memset(s, 0, sizeof(rsched_t));
  s->machine = machine;

  // virtual memory page directory
  rerr_t err = vm_pagedir_init(&s->vm_pagedir, machine->mm);
  if (err)
    return err;

  // allt
  mtx_init(&s->allt.lock, mtx_plain);

  // initialize main M (id=0) on current OS thread
  s->midgen = 1;
  s->maxmcount = 1000;
  s->m0.s = s;
  err = m_init(&s->m0, 0);
  if UNLIKELY(err)
    goto error;

  // set "current task" to M's root task
  _g_t = &s->m0.t0;

  // create processors
  u32 nprocs = 2;
  assertf(nprocs > 0 && nprocs <= S_MAXPROCS, "%u", nprocs);
  s->nprocs = nprocs;
  trace("s.nprocs=%u", nprocs);
  p_init(&s->p0, 0);
  s->allp[0] = &s->p0;
  for (u32 p_id = 1; p_id < nprocs; p_id++) {
    P* p = rmem_alloct(s->machine->malloc, P);
    if (!p) {
      // TODO: free other P's we allocated
      err = rerr_nomem;
      goto error;
    }
    p_init(p, p_id);
    s->allp[p_id] = p;
  }

  // associate P0 with M0
  P* p = s->allp[0];
  p->status = P_IDLE;
  p_acquire(p, &s->m0); // associate P and M (p->m=m, m->p=p, p.status=P_RUNNING)

  // allocate heap  TODO: remove; use vm instead
  rmem_t heapmem = rmem_alloc(machine->malloc, 4lu * MiB);
  if UNLIKELY(!heapmem.p) {
    err = rerr_nomem;
    goto error;
  }
  s->heapbase = heapmem.p;
  s->heapsize = heapmem.size;

  return 0;
error:
  vm_pagedir_dispose(&s->vm_pagedir);
  return err;
}


void rsched_dispose(rsched_t* s) {
  vm_pagedir_dispose(&s->vm_pagedir);
  rmem_free(s->machine->malloc, RMEM(s->heapbase, s->heapsize));
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
  dlog("  %-7s %12s  %-12s              %6s","SEGMENT","HADDR","VADDR","SIZE");
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
      dlog("  %-7s %12lx  (not mapped)              %6zu", \
      (name), haddr__, (size)); \
    } else { \
      dlog("  %-7s %12lx  %012llx-%012llx %6zu", \
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


// rsched_loadrom loads a rom image into backing memory pages at basemem
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
  if UNLIKELY((const void*)rom->code > rom->data) {
    // TODO: support rom with DATA section before CODE section
    dlog("unsupported ROM layout: DATA section before CODE section");
    return rerr_not_supported;
  }

  // move code section to front
  usize codesize = rom->codelen * sizeof(rin_t);
  memmove(basemem.p, rom->code, codesize);

  // rsm_loadrom verifies that rom->dataalign<=RSM_ROM_ALIGN and we're
  // making the assumption here that PAGE_SIZE as least as large,
  // since we're aligning data to page boundary.
  static_assert(RSM_ROM_ALIGN <= PAGE_SIZE, "");

  // move data section to after code, aligned to page boundary
  void* data = basemem.p + ALIGN2(codesize, PAGE_SIZE);
  memmove(data, rom->data, rom->datasize);

  // virtual memory layout
  //   ┌──────┬─────────── ··· ────────────┬───────────┐
  //   │ data │ heap →             ← stack │ exeinfo_t │
  //   ├──────┴─────────── ··· ────────────┴───────────┤
  //   0x1000                                          0xffffffffffff
  //   VM_ADDR_MIN                                     VM_ADDR_MAX

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
    heap, heapsize,
    stack, stacksize);

  // map bottom vm pages (data & heap) in vm page directory
  uintptr haddr = (uintptr)data;
  usize nbyte = ALIGN2(rom->datasize, HEAP_ALIGN) + heapsize;
  usize bottom_npages = nbyte / PAGE_SIZE;
  assertf(IS_ALIGN2(nbyte, PAGE_SIZE), "%zu", nbyte);
  if (( err = vm_map(&s->vm_pagedir, haddr, DATA_VADDR, bottom_npages, VM_PERM_RW) )) {
    dlog("vm_map failed: %s", rerr_str(err));
    return rerr_mfault;
  }

  // map top vm pages (stack & exeinfo)
  u64 vaddr = STACK_VADDR - stacksize;
  haddr = (uintptr)stack - stacksize;
  nbyte = (usize)(uintptr)(heapend - data);
  assertf(IS_ALIGN2(nbyte, PAGE_SIZE), "%zu", nbyte);
  if (( err = vm_map(&s->vm_pagedir, haddr, vaddr, nbyte/PAGE_SIZE, VM_PERM_RW) )) {
    dlog("vm_map failed: %s", rerr_str(err));
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
  usize stacksize;
  rerr_t err = rsched_loadrom(s, rom, basemem, &stacksize);
  if (err)
    goto end;

  // create main task
  void* code = basemem.p;
  err = sched_spawn(s, code, rom->codelen, STACK_VADDR, stacksize, /*pc=*/0);
  if (err)
    goto end;

  // set pointer to main task
  s->t0 = s->m0.p->runnext;

  // enter scheduler loop in M0
  err = m_start(&s->m0);

  // finish
  rsched_unloadrom(s, rom);

end:
  rmm_freepages(s->machine->mm, basemem.p);
  return err;
}

