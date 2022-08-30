#include "rsmimpl.h"
#include "thread.h"

#ifndef RSM_SEMAPHORE_POSIX
  #if defined(_WIN32)
    #include <windows.h>
    #undef min
    #undef max
  #elif defined(__MACH__)
    #undef panic // mach/mach.h defines a function called panic()
    #include <mach/mach.h>
    // redefine panic
    #define panic(fmt, ...) _panic(__FILE__, __LINE__, __FUNCTION__, fmt, ##__VA_ARGS__)
  #else
    #error Unsupported platform
  #endif
#endif

#ifdef RSM_THREAD_PTHREAD
  #include <errno.h>
#endif

#define USECS_IN_1_SEC 1000000
#define NSECS_IN_1_SEC 1000000000

RSM_ASSUME_NONNULL_BEGIN



//———————————————————————————————————————————————————————————————————————————————————
#if defined(RSM_THREAD_C11)
rerr_t mutex_init(mutex_t* mu, mutexflag_t flags) {
  return mutex_init(mu, mutex_plain) ? rerr_nomem : 0;
}
void mutex_dispose(mutex_t* mu) {
  mutex_destroy(mu);
}

//———————————————————————————————————————————————————————————————————————————————————
#elif defined(RSM_THREAD_PTHREAD)


rerr_t mutex_init(mutex_t* mu, mutexflag_t flags) {
  if (!flags)
    return pthread_mutex_init(mu, NULL) == 0 ? 0 : rerr_nomem;
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  // if (flags & MUTEX_TIMED)
  //   pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_TIMED_NP);
  if (flags & MUTEX_RECURSIVE)
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  int err = pthread_mutex_init(mu, &attr);
  pthread_mutexattr_destroy(&attr);
  return err ? rerr_nomem : 0;
}

void mutex_dispose(mutex_t* mu) {
  int err = pthread_mutex_destroy(mu);
  if (err == EBUSY) {
    dlog("warning: mutex_dispose called on locked mutex");
  } else {
    safecheckf(!err, "mutex_dispose (%d)", err);
  }
}

//———————————————————————————————————————————————————————————————————————————————————
#elif !defined(RSM_THREAD_C11)
  #error TODO implementation
#endif

//———————————————————————————————————————————————————————————————————————————————————
// RWMutex

// RWMUTEX_WATERMARK: this is a watermark value for RWMutex.r
//   RWMutex.r == 0                -- no read or write locks
//   RWMutex.r <  RWMUTEX_WATERMARK  -- RWMutex.r read locks
//   RWMutex.r >= RWMUTEX_WATERMARK  -- write lock held
// RWMutex_rlock optimistically increments RWMutex.r thus the value of RWMutex.r
// may exceed RWMUTEX_WATERMARK for brief periods of time while a RWMutex_rlock fails.
const u32 RWMUTEX_WATERMARK = 0xffffff;

void RWMutexRLock(RWMutex* m) {
  while (1) {
    u32 r = AtomicAdd(&m->r, 1, memory_order_acquire);
    if (r < RWMUTEX_WATERMARK)
      return;
    // there's a write lock; revert addition and await write lock
    AtomicSub(&m->r, 1, memory_order_release);
    mutex_lock(&m->w);
    mutex_unlock(&m->w);
    // try read lock again
  }
}

bool RWMutexTryRLock(RWMutex* m) {
  u32 r = AtomicAdd(&m->r, 1, memory_order_acquire);
  if (r < RWMUTEX_WATERMARK)
    return true;
  // there's a write lock; revert addition and await write lock
  AtomicSub(&m->r, 1, memory_order_release);
  return false;
}

void RWMutexRUnlock(RWMutex* m) {
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->r);
    safecheckf(prevr != 0, "no read lock held");
    if (prevr < RWMUTEX_WATERMARK) {
      AtomicSub(&m->r, 1, memory_order_release);
      return;
    }
    // await write lock
    mutex_lock(&m->w);
    mutex_unlock(&m->w);
  }
}

void RWMutexLock(RWMutex* m) {
  int retry = 0;
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->r);
    if (prevr == 0 && AtomicCASWeakRelAcq(&m->r, &prevr, RWMUTEX_WATERMARK)) {
      // no read locks; acquire write lock
      return mutex_lock(&m->w);
    }
    // spin
    if (retry++ == 100) {
      retry = 0;
      YIELD_THREAD();
    }
  }
}

bool RWMutexTryLock(RWMutex* m) {
  u32 prevr = AtomicLoadAcq(&m->r);
  if (prevr == 0 && AtomicCASWeakRelAcq(&m->r, &prevr, RWMUTEX_WATERMARK)) {
    // no read locks; acquire write lock
    return mutex_trylock(&m->w);
  }
  // read-locked
  return false;
}


void RWMutexUnlock(RWMutex* m) {
  int retry = 0;
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->r);
    if (prevr >= RWMUTEX_WATERMARK &&
        AtomicCASWeakRelAcq(&m->r, &prevr, prevr - RWMUTEX_WATERMARK))
    {
      return mutex_unlock(&m->w);
    }
    safecheckf(prevr >= RWMUTEX_WATERMARK, "no write lock held");
    // spin
    if (retry++ == 100) {
      retry = 0;
      YIELD_THREAD();
    }
  }
}

//———————————————————————————————————————————————————————————————————————————————————
// RHMutex

// The value of kYieldProcessorTries is cargo culted from TCMalloc, Windows
// critical section defaults, WebKit, etc.
#define kYieldProcessorTries 1000


void _RHMutexLock(RHMutex* m) {
  while (1) {
    if (!AtomicExchange(&m->flag, true, memory_order_acquire))
      break;
    usize n = kYieldProcessorTries;
    while (AtomicLoad(&m->flag, memory_order_relaxed)) {
      if (--n == 0) {
        AtomicAdd(&m->nwait, 1, memory_order_relaxed);
        while (AtomicLoad(&m->flag, memory_order_relaxed))
          sema_wait(&m->sema, -1);
        AtomicSub(&m->nwait, 1, memory_order_relaxed);
      } else {
        // avoid starvation on hyper-threaded CPUs
        YIELD_CPU();
      }
    }
  }
}


//———————————————————————————————————————————————————————————————————————————————————
// BEGIN LIGHTWEIGHT SEMAPHORE IMPLEMENTATION
//
// This implementation is based on of Jeff Preshing's "lightweight semaphore"
// https://github.com/preshing/cpp11-on-multicore/blob/master/common/sema.h
// zlib license:
//
// Copyright (c) 2015 Jeff Preshing
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//  claim that you wrote the original software. If you use this software
//  in a product, an acknowledgement in the product documentation would be
//  appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//  misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

#if defined(_WIN32) && !defined(RSM_SEMAPHORE_POSIX)

rerr_t sema_init(sema_t* sp, u32 initcount) {
  assert(initcount <= 0x7fffffff);
  *sp = (sema_t)CreateSemaphoreW(NULL, (int)initcount, 0x7fffffff, NULL);
  return *sp ? 0 : rerr_nomem;
}

void sema_dispose(sema_t* sp) {
  CloseHandle((HANDLE)*sp);
}

bool sema_wait(sema_t* sp, i64 timeout_nsec) {
  DWORD dwMilliseconds;
  if (timeout_nsec < 0) {
    dwMilliseconds = ~(DWORD)0ul; // ULONG_MAX, INFINITE
  } else {
    dwMilliseconds = (DWORD)(timeout_nsec / 1000000ll);
  }
  return WaitForSingleObject((HANDLE)*sp, dwMilliseconds) == 0;
}

void sema_signal(sema_t* sp, u32 count) {
  assert(count > 0);
  safecheckxf(ReleaseSemaphore((HANDLE)*sp, count, NULL) != 0, "sema_signal");
}

//---------------------------------------------------------------------------------------------
#elif defined(__MACH__) && !defined(RSM_SEMAPHORE_POSIX)
// Can't use POSIX semaphores due to
// https://web.archive.org/web/20140109214515/
// http://lists.apple.com/archives/darwin-kernel/2009/Apr/msg00010.html
// https://opensource.apple.com/source/xnu/xnu-7195.81.3/osfmk/mach/semaphore.h.auto.html

rerr_t sema_init(sema_t* sp, u32 initcount) {
  assert(initcount <= 0x7fffffff);
  kern_return_t rc = semaphore_create(
    mach_task_self(), (semaphore_t*)sp, SYNC_POLICY_FIFO, (int)initcount);
  return rc == KERN_SUCCESS ? 0 : rerr_nomem;
}

void sema_dispose(sema_t* sp) {
  semaphore_destroy(mach_task_self(), *(semaphore_t*)sp);
}

bool sema_wait(sema_t* sp, i64 timeout_nsec) {
  semaphore_t s = *(semaphore_t*)sp;
  mach_timespec_t ts;

  if (timeout_nsec == 0) {
    ts.tv_sec  = 0;
    ts.tv_nsec = 0;
    return semaphore_timedwait(s, ts) == KERN_SUCCESS;
  }

  if (timeout_nsec < 0) while (1) {
    kern_return_t rc = semaphore_wait(s);
    if (rc != KERN_ABORTED)
      return rc == KERN_SUCCESS;
  }

  // timeout_nsec > 0
  // Note:
  // semaphore_wait_deadline was introduced in macOS 10.6
  // semaphore_timedwait was introduced in macOS 10.10
  // https://developer.apple.com/library/prerelease/mac/documentation/General/
  //   Reference/APIDiffsMacOSX10_10SeedDiff/modules/Darwin.html
  while (1) {
    ts.tv_sec  = (u32)(timeout_nsec / 1000000000ll);
    ts.tv_nsec = (int)(timeout_nsec % 1000000000ll);
    u64 start = nanotime();
    kern_return_t rc = semaphore_timedwait(s, ts);
    // note: rc==KERN_OPERATION_TIMED_OUT on timeout
    if (rc != KERN_ABORTED)
      return rc == KERN_SUCCESS;
    // interrupted
    // subtract time already waited and retry
    i64 diff = (i64)(nanotime() - start);
    if (diff == 0) {
      // avoid infinite loop
      timeout_nsec--;
    } else if (diff > timeout_nsec) {
      timeout_nsec = 0;
    } else {
      timeout_nsec -= diff;
    }
  }
}

void sema_signal(sema_t* sp, u32 count) {
  semaphore_t s = *(semaphore_t*)sp;
  assert(count > 0);
  do {
    safecheckxf(semaphore_signal(s) == KERN_SUCCESS, "sema_signal");
  } while (--count);
}

//---------------------------------------------------------------------------------------------
#elif defined(RSM_SEMAPHORE_POSIX)

// TODO: implementation based on futex (for Linux and OpenBSD).
// See "__TBB_USE_FUTEX" of oneTBB

rerr_t sema_init(sema_t* sp, u32 initcount) {
  int err = sem_init((sem_t*)sp, 0, initcount);
  if (err)
    return (errno == EINVAL) ? rerr_overflow : rerr_errno(err);
  return 0;
}

void sema_dispose(sema_t* sp) {
  sem_destroy((sem_t*)sp);
}

int sem_timedwait(sem_t *restrict sem,
           const struct timespec *restrict abstime) {
  panic("x");
}

bool sema_wait(sema_t* sp, i64 timeout_nsec) {
  sem_t* s = (sem_t*)sp;
  int rc;
  if (timeout_nsec == 0) {
    do { rc = sem_trywait(s); } while (rc == -1 && errno == EINTR);
  } else if (timeout_nsec < 0) {
    do { rc = sem_wait(s); } while (rc == -1 && errno == EINTR);
  } else {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += (time_t)(timeout_nsec / 1000000000ll);
    ts.tv_nsec += (long)(timeout_nsec % 1000000000ll);
    // sem_timedwait bombs if you have more than 1000000000 in tv_nsec
    // so we have to clean things up before passing it in
    if (ts.tv_nsec >= 1000000000) {
      ts.tv_nsec -= 1000000000;
      ts.tv_sec++;
    }
    do { rc = sem_timedwait(s, &ts); } while (rc == -1 && errno == EINTR);
  }
  return rc == 0;
}

void sema_signal(sema_t* sp, u32 count) {
  assert(count > 0);
  do {
    safecheckxf(sem_post((sem_t*)sp) != -1, "sema_signal");
  } while (--count);
}

#endif /* system */
// end of RSema implementations
//——————————————————————————————————————————————————————————————————————————————————————
#if 0
// RLSema

// RLSema is a "lightweight" semaphore which is more efficient than RSema under
// high-contention conditions.
// Waiting when there's already a signal available is very cheap (no syscalls.)
// If there's no signal the implementation will retry by spinning for a short while
// before eventually falling back to RSema.
typedef struct RLSema {
  _Atomic(isize) count;
  RSema          sema;
} RLSema;
rerr_t RLSemaInit(RLSema*, u32 initcount);
void RLSemaDispose(RLSema*);
bool RLSemaWait(RLSema*);
bool RLSemaTryWait(RLSema*);
bool RLSemaTimedWait(RLSema*, u64 timeout_usecs);
void RLSemaSignal(RLSema*, u32 count /*must be >0*/);
usize RLSemaApproxAvail(RLSema*);


// LSEMA_MAX_SPINS is the upper limit of how many times to retry a CAS while spinning.
// After LSEMA_MAX_SPINS CAS attempts has failed (not gotten a signal), the implementation
// falls back on calling sema_wait.
//
// The number 10000 has been choosen by looking at contention between a few threads competing
// for signal & wait on macOS 10.15 x86_64. In most observed cases two threads with zero overhead
// racing to wait usually spends around 200–3000 loop cycles before succeeding. (clang -O0)
//
#define LSEMA_MAX_SPINS 10000


static bool _RLSemaWaitPartialSpin(RLSema* s, u64 timeout_usecs) {
  isize oldCount;
  int spin = LSEMA_MAX_SPINS;
  while (--spin >= 0) {
    oldCount = AtomicLoad(&s->count, memory_order_relaxed);
    if (oldCount > 0 && AtomicCASAcqRel(&s->count, &oldCount, oldCount - 1))
      return true;
    // Prevent the compiler from collapsing the loop
    // [rsms]: Is this really needed? Find out. I think both clang and gcc will avoid
    //         messing with loops that contain atomic ops,
    #if defined(__STDC_NO_ATOMICS__)
      __asm__ volatile("" ::: "memory");
    #else
      atomic_signal_fence(memory_order_acquire);
    #endif
  }
  oldCount = AtomicSub(&s->count, 1, memory_order_acquire);
  if (oldCount > 0)
    return true;
  if (timeout_usecs == 0) {
    if (sema_wait(&s->sema))
      return true;
  }
  if (timeout_usecs > 0 && RSemaTimedWait(&s->sema, timeout_usecs))
    return true;
  // At this point, we've timed out waiting for the semaphore, but the
  // count is still decremented indicating we may still be waiting on
  // it. So we have to re-adjust the count, but only if the semaphore
  // wasn't signaled enough times for us too since then. If it was, we
  // need to release the semaphore too.
  while (1) {
    oldCount = AtomicLoadAcq(&s->count);
    if (oldCount >= 0 && RSemaTryWait(&s->sema))
      return true;
    if (oldCount < 0 && AtomicCASRel(&s->count, &oldCount, oldCount + 1))
      return false;
  }
}


rerr_t RLSemaInit(RLSema* s, u32 initcount) {
  s->count = initcount;
  return sema_init(&s->sema, initcount);
}

void RLSemaDispose(RLSema* s) {
  sema_dispose(&s->sema);
}

bool RLSemaWait(RLSema* s) {
  return RLSemaTryWait(s) || _RLSemaWaitPartialSpin(s, 0);
}

bool RLSemaTryWait(RLSema* s) {
  isize oldCount = AtomicLoadAcq(&s->count);
  while (oldCount > 0) {
    if (AtomicCASWeakAcq(&s->count, &oldCount, oldCount - 1))
      return true;
  }
  return false;
}

bool RLSemaTimedWait(RLSema* s, u64 timeout_usecs) {
  return RLSemaTryWait(s) || _RLSemaWaitPartialSpin(s, timeout_usecs);
}

void RLSemaSignal(RLSema* s, u32 count) {
  assert(count > 0);
  isize oldCount = AtomicAdd(&s->count, (isize)count, memory_order_release);
  isize toRelease = -oldCount < count ? -oldCount : (isize)count;
  if (toRelease > 0)
    RSemaSignal(&s->sema, (u32)toRelease);
}

usize RLSemaApproxAvail(RLSema* s) {
  isize count = AtomicLoadAcq(&s->count);
  return count > 0 ? (usize)(count) : 0;
}

#endif
// END LIGHTWEIGHT SEMAPHORE IMPLEMENTATION
//———————————————————————————————————————————————————————————————————————————————————

RSM_ASSUME_NONNULL_END
