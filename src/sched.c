// virtual machine
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "abuf.h"
#include "thread.h"
#include "sched.h"
#include "machine.h"


// stack constants
#define STK_ALIGN    8           // stack alignment (== sizeof(u64))
#define STK_MIN      2048        // minium stack size (TODO: consider making part of ROM)
#define STK_MAX      (1024*1024) // maximum stack size
static_assert(STK_MIN % STK_ALIGN == 0, "STK_MIN not aligned to STK_ALIGN");

#define HEAP_ALIGN   8  // alignment of heap memory address

// MAIN_RET_PC: special PC value representing the main return address
#define MAIN_RET_PC  USIZE_MAX

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

// tctx_t: task execution context used for task switching, stored on stack
typedef struct {
  double fregs[RSM_NREGS - RSM_NTMPREGS];
  u64    iregs[RSM_NREGS - RSM_NTMPREGS - 1]; // does not include SP
} __attribute__((__aligned__(STK_ALIGN))) tctx_t;


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
  assert(t->s == s);
  assert(t_readstatus(t) != T_IDLE);

  mtx_lock(&s->allt.lock);

  if (s->allt.len == s->allt.cap) {
    rmem_t m = { s->allt.ptr, (usize)s->allt.cap * sizeof(void*) };
    if UNLIKELY(!rmem_resize(s->machine->malloc, &m, (s->allt.cap + 64) * sizeof(void*))) {
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


// tctx_t layout:
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
    // tctx is stored on the task stack at address SP-sizeof(tctx_t)
    tctx_t* ctx = t->sp;
    for (usize i = 0; i < countof(ctx->fregs); i++)
      m->fregs[i + RSM_NTMPREGS] = ctx->fregs[i];
    for (usize i = 0; i < countof(ctx->iregs); i++)
      m->iregs[i + RSM_NTMPREGS] = ctx->iregs[i];
    u64 sp = (u64)(uintptr)t->sp;
    dlog("TODO: translate vm stack address to host address");
    m->iregs[RSM_MAX_REG] = sp;
  }
}


// stacksize is a requested minimum number of bytes to allocate for its stack
static T* nullable sched_alloctask(rsched_t* s, usize stacksize) {
  // task memory layout:
  //    ┌───────────┬──────────┐
  //    │   ← stack │ T struct │
  //    ├───────────┼──────────┘
  // stacktop   stackbase
  //            initial SP

  usize alignment = MAX(_Alignof(T), STK_ALIGN);
  stacksize = ALIGN2(stacksize, alignment);

  rmem_t stacktop =
    rmem_alloc_aligned(s->machine->malloc, stacksize + sizeof(T), alignment);
  if UNLIKELY(!stacktop.p)
    return NULL;
  memset(stacktop.p, 0, stacktop.size);
  assertf(IS_ALIGN2((uintptr)stacktop.p, (uintptr)alignment), "bug in rmem_alloc");

  T* t = (T*)((uintptr)stacktop.p + stacktop.size - sizeof(T));
  t->s = s;
  t->stacktop = (uintptr)stacktop.p;
  t->sp = TASK_STACKBASE(t);

  // initialize tctx
  tctx_t* ctx = (tctx_t*)(t->sp - sizeof(tctx_t));
  ctx->iregs[RSM_MAX_REG - RSM_NTMPREGS - 1] = (u64)(uintptr)t; // CTX

  // dlog("s_alloctask stack %p … %p (%lu B)",
  //   stacktop.p, t->sp, (usize)((uintptr)t->sp - (uintptr)stacktop.p));

  return t;
}


// sched_spawn creates a new T running fn with argsize bytes of arguments.
// Put it on the queue of T's waiting to run.
// The compiler turns a go statement into a call to this.
static rerr_t sched_spawn(
  rsched_t* s, const rin_t* iv, usize ic, u64 stack_vaddr, usize pc)
{
  T* t = t_get();
  M* m = t->m;
  rerr_t err = 0;

  // disable preemption
  m_acquire(m);

  // create main task
  T* newtask = sched_alloctask(s, STK_MIN);
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


static u64 rsched_vmlayout(rsched_t* s, rrom_t* rom) {
  // virtual memory layout
  //    ┌─────────┬──────── ··· ─────────┬───────────┐
  //    │ data    │ heap →       ← stack │ heap_addr │
  //    ├─────────┴──────── ··· ─────────┴───────────┤
  //  0x1000                                  0xffffffffffff
  //
  static_assert(IS_ALIGN2(EXE_BASE_ADDR, PAGE_SIZE), "");
  u64 data_vaddr = EXE_BASE_ADDR;

  // copy data to base address
  u64 offs = 0;
  usize size = rom->datasize;
  for (u64 npages = (rom->datasize + PAGE_SIZE - 1)/PAGE_SIZE; npages--;) {
    uintptr haddr = vm_pagedir_translate(&s->vm_pagedir, data_vaddr + offs);
    if (haddr == 0)
      return 0;
    assertf(IS_ALIGN2(haddr, PAGE_SIZE), "%p", (void*)haddr);
    // TODO: set r & w flags for the vm page entry
    usize copysize = size;
    if (copysize > PAGE_SIZE)
      copysize = PAGE_SIZE;
    memcpy((void*)haddr, rom->data + offs, copysize);
    size -= copysize;
    offs += PAGE_SIZE;
  }

  // top part of address space
  // TODO: env, argv etc
  u64 top_addr = ALIGN2_FLOOR(EXE_TOP_ADDR, 8);

  // store heap_addr
  u64 heap_vaddr = ALIGN2(data_vaddr + rom->datasize, HEAP_ALIGN);
  top_addr -= 8;
  uintptr haddr = vm_pagedir_translate(&s->vm_pagedir, top_addr);
  if (haddr == 0)
    return 0;
  *(u64*)haddr = heap_vaddr;

  // stack starts where initial args end
  u64 stack_vaddr = ALIGN2_FLOOR(top_addr, STK_ALIGN);

  dlog("data_vaddr:  0x%llx", data_vaddr);
  dlog("heap_vaddr:  0x%llx", heap_vaddr);
  dlog("stack_vaddr: 0x%llx", stack_vaddr);

  return stack_vaddr;
}


rerr_t rsched_execrom(rsched_t* s, rrom_t* rom) {
  // make sure ROM is loaded
  if (!rom->code) {
    rerr_t err = rsm_loadrom(rom);
    if (err)
      return err;
  }
  if (rom->code == NULL || rom->codelen == 0)
    return rerr_invalid; // ROM without (or with empty) code section

  // layout virtual memory
  u64 stack_vaddr = rsched_vmlayout(s, rom);
  if (stack_vaddr == 0)
    return rerr_nomem;

  // create main task
  rerr_t err = sched_spawn(s, rom->code, rom->codelen, stack_vaddr, /*pc=*/0);
  if (err)
    return err;

  // save pointer to main task
  s->t0 = s->m0.p->runnext;

  // enter scheduler loop in M0
  return m_start(&s->m0);
}

