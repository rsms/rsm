#include "rsmimpl.h"

//#define USE_UNIX_SEMA

#if defined(_WIN32) && !defined(USE_UNIX_SEMA)
  #include <windows.h>
  #undef min
  #undef max
#elif defined(__MACH__) && !defined(USE_UNIX_SEMA)
  #undef panic // mach/mach.h defines a function called panic()
  #include <mach/mach.h>
  // redefine panic
  #define panic(fmt, ...) _panic(__FILE__, __LINE__, __FUNCTION__, fmt, ##__VA_ARGS__)
#elif defined(__unix__) || defined(USE_UNIX_SEMA)
  #include <semaphore.h>
#else
  #error Unsupported platform
#endif

#define USECS_IN_1_SEC 1000000
#define NSECS_IN_1_SEC 1000000000

RSM_ASSUME_NONNULL_BEGIN


//———————————————————————————————————————————————————————————————————————————————————
// RWMutex

// MTX_W_WATERMARK: this is a watermark value for RWMutex.r
//   RWMutex.r == 0                -- no read or write locks
//   RWMutex.r <  MTX_W_WATERMARK  -- RWMutex.r read locks
//   RWMutex.r >= MTX_W_WATERMARK  -- write lock held
// RWMutex_rlock optimistically increments RWMutex.r thus the value of RWMutex.r
// may exceed MTX_W_WATERMARK for brief periods of time while a RWMutex_rlock fails.
const u32 MTX_W_WATERMARK = 0xffffff;

int RWMutexRLock(RWMutex* m) {
  while (1) {
    u32 r = AtomicAdd(&m->r, 1, memory_order_acquire);
    if (r < MTX_W_WATERMARK)
      return thrd_success;
    // there's a write lock; revert addition and await write lock
    AtomicSub(&m->r, 1, memory_order_release);
    int status = mtx_lock(&m->w);
    if (status != thrd_success)
      return status;
    mtx_unlock(&m->w);
    // try read lock again
  }
}

int RWMutexTryRLock(RWMutex* m) {
  while (1) {
    u32 r = AtomicAdd(&m->r, 1, memory_order_acquire);
    if (r < MTX_W_WATERMARK)
      return thrd_success;
    // there's a write lock; revert addition and await write lock
    AtomicSub(&m->r, 1, memory_order_release);
    return thrd_busy;
  }
}

int RWMutexRUnlock(RWMutex* m) {
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->r);
    if (prevr == 0)
      return thrd_error; // not holding a read lock!
    if (prevr < MTX_W_WATERMARK) {
      AtomicSub(&m->r, 1, memory_order_release);
      return thrd_success;
    }
    // await write lock
    int status = mtx_lock(&m->w);
    if (status != thrd_success)
      return status;
    mtx_unlock(&m->w);
  }
}

int RWMutexLock(RWMutex* m) {
  int retry = 0;
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->r);
    if (prevr == 0 && AtomicCASWeakRelAcq(&m->r, &prevr, MTX_W_WATERMARK)) {
      // no read locks; acquire write lock
      return mtx_lock(&m->w);
    }
    // spin
    if (retry++ == 100) {
      retry = 0;
      YIELD_THREAD();
    }
  }
}

int RWMutexTryLock(RWMutex* m) {
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->r);
    if (prevr == 0 && AtomicCASWeakRelAcq(&m->r, &prevr, MTX_W_WATERMARK)) {
      // no read locks; acquire write lock
      return mtx_trylock(&m->w);
    }
    return thrd_busy;
  }
}


int RWMutexUnlock(RWMutex* m) {
  int retry = 0;
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->r);
    if (prevr >= MTX_W_WATERMARK &&
        AtomicCASWeakRelAcq(&m->r, &prevr, prevr - MTX_W_WATERMARK))
    {
      return mtx_unlock(&m->w);
    }
    if (prevr < MTX_W_WATERMARK)
      return thrd_error; // not holding a write lock!
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


void _RHMutexWait(RHMutex* m) {
  while (1) {
    if (!AtomicExchange(&m->flag, true, memory_order_acquire))
      break;
    usize n = kYieldProcessorTries;
    while (AtomicLoad(&m->flag, memory_order_relaxed)) {
      if (--n == 0) {
        AtomicAdd(&m->nwait, 1, memory_order_relaxed);
        while (AtomicLoad(&m->flag, memory_order_relaxed))
          RSemaWait(&m->sema);
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

#if defined(_WIN32) && !defined(USE_UNIX_SEMA)

bool RSemaInit(RSema* sp, u32 initcount) {
  assert(initcount <= 0x7fffffff);
  *sp = (RSema)CreateSemaphoreW(NULL, (int)initcount, 0x7fffffff, NULL);
  return *sp != NULL;
}

void RSemaDispose(RSema* sp) {
  CloseHandle(*sp);
}

bool RSemaWait(RSema* sp) {
  const unsigned long infinite = 0xffffffff;
  return WaitForSingleObject(*sp, infinite) == 0;
}

bool RSemaTryWait(RSema* sp) {
  return WaitForSingleObject(*sp, 0) == 0;
}

bool RSemaTimedWait(RSema* sp, u64 timeout_usecs) {
  return WaitForSingleObject(*sp, (unsigned long)(timeout_usecs / 1000)) == 0;
}

bool RSemaSignal(RSema* sp, u32 count) {
  assert(count > 0);
  // while (!ReleaseSemaphore(*sp, count, NULL)) {
  // }
  return ReleaseSemaphore(*sp, count, NULL);
}

//---------------------------------------------------------------------------------------------
#elif defined(__MACH__) && !defined(USE_UNIX_SEMA)
// Can't use POSIX semaphores due to
// https://web.archive.org/web/20140109214515/
// http://lists.apple.com/archives/darwin-kernel/2009/Apr/msg00010.html

bool RSemaInit(RSema* sp, u32 initcount) {
  assert(initcount <= 0x7fffffff);
  kern_return_t rc =
    semaphore_create(mach_task_self(), (semaphore_t*)sp, SYNC_POLICY_FIFO, (int)initcount);
  return rc == KERN_SUCCESS;
}

void RSemaDispose(RSema* sp) {
  semaphore_destroy(mach_task_self(), *(semaphore_t*)sp);
}

bool RSemaWait(RSema* sp) {
  semaphore_t s = *(semaphore_t*)sp;
  while (1) {
    kern_return_t rc = semaphore_wait(s);
    if (rc != KERN_ABORTED)
      return rc == KERN_SUCCESS;
  }
}

bool RSemaTryWait(RSema* sp) {
  return RSemaTimedWait(sp, 0);
}

bool RSemaTimedWait(RSema* sp, u64 timeout_usecs) {
  mach_timespec_t ts;
  ts.tv_sec = (u32)(timeout_usecs / USECS_IN_1_SEC);
  ts.tv_nsec = (int)((timeout_usecs % USECS_IN_1_SEC) * 1000);
  // Note:
  // semaphore_wait_deadline was introduced in macOS 10.6
  // semaphore_timedwait was introduced in macOS 10.10
  // https://developer.apple.com/library/prerelease/mac/documentation/General/Reference/
  //   APIDiffsMacOSX10_10SeedDiff/modules/Darwin.html
  semaphore_t s = *(semaphore_t*)sp;
  while (1) {
    kern_return_t rc = semaphore_timedwait(s, ts);
    if (rc != KERN_ABORTED)
      return rc == KERN_SUCCESS;
    // TODO: update ts; subtract time already waited and retry (loop).
    // For now, let's just return with an error:
    return false;
  }
}

bool RSemaSignal(RSema* sp, u32 count) {
  assert(count > 0);
  semaphore_t s = *(semaphore_t*)sp;
  kern_return_t rc = 0; // KERN_SUCCESS
  while (count-- > 0) {
    rc += semaphore_signal(s); // == ...
    // auto rc1 = semaphore_signal(s);
    // if (rc1 != KERN_SUCCESS) {
    //   rc = rc1;
    // }
  }
  return rc == KERN_SUCCESS;
}

//---------------------------------------------------------------------------------------------
#elif defined(__unix__) || defined(USE_UNIX_SEMA)

// TODO: implementation based on futex (for Linux and OpenBSD). See "__TBB_USE_FUTEX" of oneTBB

bool RSemaInit(RSema* sp, u32 initcount) {
  return sem_init((sem_t*)sp, 0, initcount) == 0;
}

void RSemaDispose(RSema* sp) {
  sem_destroy((sem_t*)sp);
}

bool RSemaWait(RSema* sp) {
  // http://stackoverflow.com/questions/2013181/gdb-causes-sem-wait-to-fail-with-eintr-error
  int rc;
  do {
    rc = sem_wait((sem_t*)sp);
  } while (rc == -1 && errno == EINTR);
  return rc == 0;
}

bool RSemaTryWait(RSema* sp) {
  int rc;
  do {
    rc = sem_trywait((sem_t*)sp);
  } while (rc == -1 && errno == EINTR);
  return rc == 0;
}

bool RSemaTimedWait(RSema* sp, u64 timeout_usecs) {
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  ts.tv_sec += (time_t)(timeout_usecs / USECS_IN_1_SEC);
  ts.tv_nsec += (long)(timeout_usecs % USECS_IN_1_SEC) * 1000;
  // sem_timedwait bombs if you have more than 1e9 in tv_nsec
  // so we have to clean things up before passing it in
  if (ts.tv_nsec >= NSECS_IN_1_SEC) {
    ts.tv_nsec -= NSECS_IN_1_SEC;
    ++ts.tv_sec;
  }
  int rc;
  do {
    rc = sem_timedwait((sem_t*)sp, &ts);
  } while (rc == -1 && errno == EINTR);
  return rc == 0;
}

bool RSemaSignal(RSema* sp, u32 count) {
  assert(count > 0);
  while (count-- > 0) {
    while (sem_post((sem_t*)sp) == -1) {
      return false;
    }
  }
  return true;
}

//---------------------------------------------------------------------------------------------
#endif /* system */
// end of RSema implementations

//---------------------------------------------------------------------------------------------
// RLSema

// LSEMA_MAX_SPINS is the upper limit of how many times to retry a CAS while spinning.
// After LSEMA_MAX_SPINS CAS attempts has failed (not gotten a signal), the implementation
// falls back on calling RSemaWait.
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
    if (RSemaWait(&s->sema))
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


bool RLSemaInit(RLSema* s, u32 initcount) {
  s->count = initcount;
  return RSemaInit(&s->sema, initcount);
}

void RLSemaDispose(RLSema* s) {
  RSemaDispose(&s->sema);
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

// END LIGHTWEIGHT SEMAPHORE IMPLEMENTATION
//———————————————————————————————————————————————————————————————————————————————————

RSM_ASSUME_NONNULL_END
