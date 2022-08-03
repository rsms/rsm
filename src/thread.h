// OS threads and atomic memory operations
// SPDX-License-Identifier: Apache-2.0
/*
  typedef int (*thrd_start_t)(void*);

  void YIELD_THREAD()
    yields for other threads to be scheduled on the current CPU by the OS
  void YIELD_CPU()
    yields for other work on a CPU core

  int    thrd_create(thrd_t *thr, thrd_start_t func, void *arg);
  void   thrd_exit(int res);
  int    thrd_join(thrd_t thr, int *res);
  int    thrd_detach(thrd_t thr);
  thrd_t thrd_current(void);
  int    thrd_equal(thrd_t a, thrd_t b);
  int    thrd_sleep(const struct timespec *ts_in, struct timespec *rem_out);
  void   thrd_yield(void);

  int    mtx_init(mtx_t *mtx, int type);
  void   mtx_destroy(mtx_t *mtx);
  int    mtx_lock(mtx_t *mtx);
  int    mtx_trylock(mtx_t *mtx);
  int    mtx_timedlock(mtx_t *mtx, const struct timespec *ts);
  int    mtx_unlock(mtx_t *mtx);

  int    cnd_init(cnd_t *cond);
  void   cnd_destroy(cnd_t *cond);
  int    cnd_signal(cnd_t *cond);
  int    cnd_broadcast(cnd_t *cond);
  int    cnd_wait(cnd_t *cond, mtx_t *mtx);
  int    cnd_timedwait(cnd_t *cond, mtx_t *mtx, const struct timespec *ts);

  int    tss_create(tss_t *key, tss_dtor_t dtor);
  void   tss_delete(tss_t key);
  int    tss_set(tss_t key, void *val);
  void*  tss_get(tss_t key);

*/
#pragma once

#if !defined(__STDC_NO_ATOMICS__)
  #include <stdatomic.h>

  #define AtomicLoad(x, order) atomic_load_explicit((x), (order))
  #define AtomicLoadAcq(x)     atomic_load_explicit((x), memory_order_acquire)

  #define AtomicStore(x, v, order) atomic_store_explicit((x), (v), (order))
  #define AtomicStoreRel(x, v)     atomic_store_explicit((x), (v), memory_order_release)

  // note: these operations return the previous value; _before_ applying the operation
  #define AtomicAdd(x, n, order) atomic_fetch_add_explicit((x), (n), (order))
  #define AtomicSub(x, n, order) atomic_fetch_sub_explicit((x), (n), (order))
  #define AtomicOr(x, n, order)  atomic_fetch_or_explicit((x), (n), (order))
  #define AtomicAnd(x, n, order) atomic_fetch_and_explicit((x), (n), (order))
  #define AtomicXor(x, n, order) atomic_fetch_xor_explicit((x), (n), (order))

  // Compare And Swap
  #define AtomicCAS(p, oldval, newval, order_succ, order_fail) \
    atomic_compare_exchange_strong_explicit( \
      (p), (oldval), (newval), (order_succ), (order_fail))

  #define AtomicCASRel(p, oldval, newval) \
    AtomicCAS((p), (oldval), (newval), memory_order_release, memory_order_relaxed)

  #define AtomicCASAcqRel(p, oldval, newval) \
    AtomicCAS((p), (oldval), (newval), memory_order_acq_rel, memory_order_relaxed)

  // The weak forms of AtomicCAS is allowed to fail spuriously, that is,
  // act as if *obj != *expected even if they are equal. When a compare-and-exchange
  // is in a loop, the weak version will yield better performance on some platforms.
  #define AtomicCASWeak(p, oldval, newval, order_succ, order_fail) \
    atomic_compare_exchange_weak_explicit( \
      (p), (oldval), (newval), (order_succ), (order_fail))

  #define AtomicCASRelaxed(p, oldval, newval) \
    AtomicCASWeak((p), (oldval), (newval), memory_order_relaxed, memory_order_relaxed)

  #define AtomicCASWeakAcq(p, oldval, newval) \
    AtomicCASWeak((p), (oldval), (newval), memory_order_acquire, memory_order_relaxed)

  #define AtomicCASWeakRelAcq(p, oldval, newval) \
    AtomicCASWeak((p), (oldval), (newval), memory_order_release, memory_order_acquire)

  #define AtomicExchange(p, desired_next_value, order) \
    atomic_exchange_explicit((p), (desired_next_value), (order))

#else
  #error "TODO: STDC_NO_ATOMICS"
#endif


// YIELD_THREAD() yields for other threads to be scheduled on the current CPU by the OS
#if (defined(WIN32) || defined(_WIN32))
  #include <windows.h>
  #define YIELD_THREAD() ((void)0)
#elif defined(RSM_NO_LIBC)
  #define YIELD_THREAD() ((void)0)
#else
  #include <sched.h>
  #define YIELD_THREAD() sched_yield() // equivalent to thrd_yield
#endif


// YIELD_CPU() yields for other work on a CPU core
#if defined(__i386) || defined(__i386__) || defined(__x86_64__)
  #define YIELD_CPU() __asm__ __volatile__("pause")
#elif defined(__arm__) || defined(__arm64__) || defined(__aarch64__)
  #define YIELD_CPU() __asm__ __volatile__("yield")
#elif defined(mips) || defined(__mips__) || defined(MIPS) || defined(_MIPS_) || defined(__mips64)
  #if defined(_ABI64) && (_MIPS_SIM == _ABI64)
    #define YIELD_CPU() __asm__ __volatile__("pause")
  #else
    // comment from WebKit source:
    //   The MIPS32 docs state that the PAUSE instruction is a no-op on older
    //   architectures (first added in MIPS32r2). To avoid assembler errors when
    //   targeting pre-r2, we must encode the instruction manually.
    #define YIELD_CPU() __asm__ __volatile__(".word 0x00000140")
  #endif
#elif (defined(WIN32) || defined(_WIN32))
  #include <immintrin.h>
  #define YIELD_CPU() _mm_pause()
#elif defined(RSM_NO_LIBC)
  #define YIELD_CPU() ((void)0)
#else
  // GCC & clang intrinsic
  #define YIELD_CPU() __builtin_ia32_pause()
#endif


// C11 threads API (with pthread shim)
#if defined(RSM_NO_LIBC)
  #error "TODO: impl thread RSM_NO_LIBC"
#elif defined(__STDC_NO_THREADS__) && __STDC_NO_THREADS__
  #include "thread_pthread.h"
#else
  #include <threads.h>
#endif


// RSema is a portable semaphore; a thin layer over the OS's semaphore implementation.
#if defined(_WIN32) || defined(__MACH__)
  typedef uintptr RSema; // intptr instead of void* to improve compiler diagnostics
#elif defined(__unix__)
  ASSUME_NONNULL_END
  #include <semaphore.h>
  ASSUME_NONNULL_BEGIN
  typedef sem_t RSema;
#endif /* RSema */


RSM_ASSUME_NONNULL_BEGIN


bool RSemaInit(RSema*, u32 initcount); // returns false if system impl failed (rare)
void RSemaDispose(RSema*);
bool RSemaWait(RSema*);    // wait for a signal
bool RSemaTryWait(RSema*); // try acquire a signal; return false instead of blocking
bool RSemaTimedWait(RSema*, u64 timeout_usecs);
bool RSemaSignal(RSema*, u32 count /*must be >0*/);


// RLSema is a "light-weight" semaphore which is more efficient than RSema under
// high-contention condition, by avoiding syscalls.
// Waiting when there's already a signal available is extremely cheap and involves
// no syscalls. If there's no signal the implementation will retry by spinning for
// a short while before eventually falling back to RSema.
typedef struct RLSema {
  _Atomic(isize) count;
  RSema          sema;
} RLSema;

bool RLSemaInit(RLSema*, u32 initcount); // returns false if system impl failed (rare)
void RLSemaDispose(RLSema*);
bool RLSemaWait(RLSema*);
bool RLSemaTryWait(RLSema*);
bool RLSemaTimedWait(RLSema*, u64 timeout_usecs);
void RLSemaSignal(RLSema*, u32 count /*must be >0*/);
usize RLSemaApproxAvail(RLSema*);


// RWMutex is a read-write mutex.
// There can be many concurrent readers but only one writer.
// While no write lock is held, up to 16777214 read locks may be held.
// While a write lock is held no read locks or other write locks can be held.
typedef struct RWMutex {
  mtx_t        w; // writer lock
  _Atomic(u32) r; // reader count
} RWMutex;
static int  RWMutexInit(RWMutex* m, int wtype);
static void RWMutexDispose(RWMutex* m);
int RWMutexRLock(RWMutex* m);     // acquire read-only lock (blocks until acquired)
int RWMutexTryRLock(RWMutex* m);  // attempt to acquire read-only lock (non-blocking)
int RWMutexRUnlock(RWMutex* m);   // release read-only lock
int RWMutexLock(RWMutex* m);      // acquire read+write lock (blocks until acquired)
int RWMutexTryLock(RWMutex* m);   // attempt to acquire read+write lock (non-blocking)
int RWMutexUnlock(RWMutex* m);    // release read+write lock


// RHMutex is a mutex that will spin for a short while and then block
typedef struct RHMutex {
  _Atomic(bool) flag;
  _Atomic(i32)  nwait;
  RSema         sema;
} RHMutex;
static bool RHMutexInit(RHMutex* m); // returns false if system failed to init semaphore
static void RHMutexDispose(RHMutex* m);
static void RHMutexLock(RHMutex* m);
static void RHMutexUnlock(RHMutex* m);
static bool RHMutexIsLocked(RHMutex* m);

//———————————————————————————————————————————————
// inline impl

static inline int RWMutexInit(RWMutex* m, int wtype) {
  assertf(wtype != mtx_timed, "mtx_timed not supported");
  m->r = 0;
  return mtx_init(&m->w, wtype);
}
static inline void RWMutexDispose(RWMutex* m) { mtx_destroy(&m->w); }

inline static bool RHMutexInit(RHMutex* m) {
  m->flag = false;
  m->nwait = 0;
  return RSemaInit(&m->sema, 0);
}

inline static void RHMutexDispose(RHMutex* m) {
  RSemaDispose(&m->sema);
}

void _RHMutexLock(RHMutex* m);

inline static void RHMutexLock(RHMutex* m) {
  if UNLIKELY(AtomicExchange(&m->flag, true, memory_order_acquire))
    _RHMutexLock(m); // already locked -- slow path
}

inline static bool RHMutexIsLocked(RHMutex* m) {
  return AtomicLoad(&m->flag, memory_order_acquire);
}

inline static void RHMutexUnlock(RHMutex* m) {
  AtomicExchange(&m->flag, false, memory_order_seq_cst);
  if UNLIKELY(AtomicLoad(&m->nwait, memory_order_seq_cst) != 0) {
    // at least one thread waiting on a semaphore signal -- wake one thread
    RSemaSignal(&m->sema, 1); // TODO: should we check the return value?
  }
}

RSM_ASSUME_NONNULL_END
