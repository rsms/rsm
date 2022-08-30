// threads and atomic memory operations
// SPDX-License-Identifier: Apache-2.0
#pragma once

// select thread API
#if defined(__STDC_NO_THREADS__) && __STDC_NO_THREADS__
  #if defined(RSM_NO_LIBC)
    #define RSM_THREAD_NONE
    #warning TODO
  #else
    #define RSM_THREAD_PTHREAD
    #include <pthread.h>
  #endif
#else
  #define RSM_THREAD_C11
  #include <threads.h>
#endif

// select semaphore API
#if defined(_WIN32) || defined(__MACH__)
  typedef uintptr sema_t;
  #define RSM_SEMAPHORE_PTR
#elif defined(__unix__)
  #include <semaphore.h>
  #define RSM_SEMAPHORE_POSIX
#else
  #warning TODO
#endif

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


RSM_ASSUME_NONNULL_BEGIN


// sema_t is a thin layer over the OS's semaphore implementation
#ifdef RSM_SEMAPHORE_POSIX
  typedef sem_t sema_t;
#else
  typedef uintptr sema_t;
#endif
rerr_t sema_init(sema_t*, u32 initcount);
void sema_dispose(sema_t*);
void sema_signal(sema_t*, u32 count); // count must be >0
// sema_wait waits for a signal.
// If timeout_nsec < 0, wait indefinitely.
// If timeout_nsec >= 0, try to acquire m's semaphore for at most timeout_nsec.
// Returns true if a signal was received.
bool sema_wait(sema_t*, i64 timeout_nsec);


// RHMutex is a regular mutex optimized for unlikely contention.
// In the uncontended case, as fast as spin locks (just a few instructions),
// but on the contention path it sleep in the kernel.
typedef struct RHMutex {
  sema_t        sema;
  _Atomic(i32)  nwait;
  _Atomic(bool) flag;
} RHMutex;
static rerr_t RHMutexInit(RHMutex* m);
static void RHMutexDispose(RHMutex* m);
static void RHMutexLock(RHMutex* m);
static void RHMutexUnlock(RHMutex* m);
static bool RHMutexIsLocked(RHMutex* m);


// mutex_t is a regular mutex
#ifdef RSM_THREAD_PTHREAD
  typedef pthread_mutex_t mutex_t;
#elif defined(RSM_THREAD_C11)
  typedef mtx_t mutex_t;
#endif
typedef enum { MUTEX_PLAIN, MUTEX_RECURSIVE } mutexflag_t;
rerr_t mutex_init(mutex_t*, mutexflag_t);
void mutex_dispose(mutex_t*);
static void mutex_lock(mutex_t*);
static bool mutex_trylock(mutex_t*);
static void mutex_unlock(mutex_t*);


// RWMutex is a read-write mutex.
// There can be many concurrent readers but only one writer.
// While no write lock is held, up to 16777214 read locks may be held.
// While a write lock is held no read locks or other write locks can be held.
typedef struct RWMutex {
  mutex_t      w; // writer lock
  _Atomic(u32) r; // reader count
} RWMutex;
static rerr_t RWMutexInit(RWMutex* m, mutexflag_t);
static void RWMutexDispose(RWMutex* m);
void RWMutexRLock(RWMutex* m);     // acquire read-only lock (blocks until acquired)
bool RWMutexTryRLock(RWMutex* m);  // attempt to acquire read-only lock (non-blocking)
void RWMutexRUnlock(RWMutex* m);   // release read-only lock
void RWMutexLock(RWMutex* m);      // acquire read+write lock (blocks until acquired)
bool RWMutexTryLock(RWMutex* m);   // attempt to acquire read+write lock (non-blocking)
void RWMutexUnlock(RWMutex* m);    // release read+write lock


//———————————————————————————————————————————————————————————————————————————————————————
// inline impl, mutex_t
#ifdef RSM_THREAD_C11
  inline static void mutex_lock(mutex_t* mu) {
    safecheckxf(mtx_lock(mu) == 0, "mutex_lock");
  }
  inline static void mutex_unlock(mutex_t* mu) {
    safecheckxf(mtx_unlock(mu) == 0, "mutex_unlock");
  }
  inline static bool mutex_trylock(mutex_t* mu) {
    return mtx_trylock(mu) == 0;
  }
#elif defined(RSM_THREAD_PTHREAD)
  inline static void mutex_lock(mutex_t* mu) {
    safecheckxf(pthread_mutex_lock(mu) == 0, "mutex_lock");
  }
  inline static void mutex_unlock(mutex_t* mu) {
    safecheckxf(pthread_mutex_unlock(mu) == 0, "mutex_unlock");
  }
  inline static bool mutex_trylock(mutex_t* mu) {
    return pthread_mutex_trylock(mu) == 0;
  }
#else
  #error TODO
#endif


//———————————————————————————————————————————————————————————————————————————————————————
// inline impl, RWMutex

static inline rerr_t RWMutexInit(RWMutex* m, mutexflag_t flags) {
  //assertf((flags & MUTEX_TIMED) == 0, "MUTEX_TIMED not supported");
  m->r = 0;
  return mutex_init(&m->w, flags);
}
static inline void RWMutexDispose(RWMutex* m) { mutex_dispose(&m->w); }

//———————————————————————————————————————————————————————————————————————————————————————
// inline impl, RHMutex

inline static rerr_t RHMutexInit(RHMutex* m) {
  m->flag = false;
  m->nwait = 0;
  return sema_init(&m->sema, 0);
}

inline static void RHMutexDispose(RHMutex* m) {
  sema_dispose(&m->sema);
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
    sema_signal(&m->sema, 1); // TODO: should we check the return value?
  }
}

//———————————————————————————————————————————————————————————————————————————————————————
RSM_ASSUME_NONNULL_END
