#include "rsmimpl.h"
#include "thread.h"
#include "hash.h"

#ifndef RSM_SEMAPHORE_POSIX
  #if defined(_WIN32)
    #include <windows.h>
  #elif defined(__MACH__)
    #undef panic // mach/mach.h defines a function called panic()
    #include <mach/mach.h>
    // redefine panic
    #define panic(fmt, ...) _panic(__FILE__, __LINE__, __FUNCTION__, fmt, ##__VA_ARGS__)
  #else
    #error Unsupported platform
  #endif
#endif

#if defined(RSM_THREAD_PTHREAD) || defined(RSM_SEMAPHORE_POSIX)
  #include <errno.h>
#endif

RSM_ASSUME_NONNULL_BEGIN


//———————————————————————————————————————————————————————————————————————————————————
// mutex_t
//———————————————————————————————————————————————————————————————————————————————————
#if defined(RSM_THREAD_C11)

rerr_t mutex_init(mutex_t* mu) {
  mu->w = 0;
  return mtx_init(&mu->m, mtx_plain) ? rerr_nomem : 0;
}

void mutex_dispose(mutex_t* mu) {
  #if DEBUG
  if (mutex_islocked(mu))
    dlog("warning: mutex_dispose called on locked mutex");
  #endif
  mtx_destroy(&mu->m);
}

//———————————————————————————————————————————————————————————————————————————————————
#elif defined(RSM_THREAD_PTHREAD)

rerr_t mutex_init(mutex_t* mu) {
  mu->w = 0;
  return pthread_mutex_init(&mu->m, NULL) == 0 ? 0 : rerr_nomem;
}

void mutex_dispose(mutex_t* mu) {
  int err = pthread_mutex_destroy(&mu->m);
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
// rwmutex_t

// RWMUTEX_WATERMARK: this is a watermark value for mutex.r
//   mutex.r == 0                 -- no read or write locks
//   mutex.r <  RWMUTEX_WATERMARK -- mutex.r read locks
//   mutex.r >= RWMUTEX_WATERMARK -- write lock held
// rwmutex_rlock optimistically increments mutex.r thus the value of mutex.r
// may exceed RWMUTEX_WATERMARK for brief periods of time while a rwmutex_rlock fails.
const u32 RWMUTEX_WATERMARK = 0xffffff;


rerr_t rwmutex_init(rwmutex_t* mu) {
  mu->m.r = 0;
  return mutex_init(&mu->m);
}

void rwmutex_dispose(rwmutex_t* mu) {
  mutex_dispose(&mu->m);
}

void rwmutex_rlock(rwmutex_t* m) {
  while (1) {
    u32 r = AtomicAdd(&m->m.r, 1, memory_order_acquire);
    if (r < RWMUTEX_WATERMARK)
      return;
    // there's a write lock; revert addition and await write lock
    AtomicSub(&m->m.r, 1, memory_order_release);
    mutex_lock(&m->m);
    mutex_unlock(&m->m);
    // try read lock again
  }
}

bool rwmutex_tryrlock(rwmutex_t* m) {
  u32 r = AtomicAdd(&m->m.r, 1, memory_order_acquire);
  if (r < RWMUTEX_WATERMARK)
    return true;
  // there's a write lock; revert addition and await write lock
  AtomicSub(&m->m.r, 1, memory_order_release);
  return false;
}

void rwmutex_runlock(rwmutex_t* m) {
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->m.r);
    safecheckf(prevr != 0, "no read lock held");
    if (prevr < RWMUTEX_WATERMARK) {
      AtomicSub(&m->m.r, 1, memory_order_release);
      return;
    }
    // await write lock
    mutex_lock(&m->m);
    mutex_unlock(&m->m);
  }
}

void rwmutex_lock(rwmutex_t* m) {
  int retry = 0;
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->m.r);
    if (prevr == 0 && AtomicCASWeakRelAcq(&m->m.r, &prevr, RWMUTEX_WATERMARK)) {
      // no read locks; acquire write lock
      return mutex_lock(&m->m);
    }
    // spin while there are read locks
    if (retry++ == 100) {
      retry = 0;
      thread_yield();
    }
  }
}

bool rwmutex_trylock(rwmutex_t* m) {
  u32 prevr = AtomicLoadAcq(&m->m.r);
  if (prevr == 0 && AtomicCASWeakRelAcq(&m->m.r, &prevr, RWMUTEX_WATERMARK)) {
    // no read locks; acquire write lock
    return mutex_trylock(&m->m);
  }
  // read-locked
  return false;
}


void rwmutex_unlock(rwmutex_t* m) {
  int retry = 0;
  while (1) {
    u32 prevr = AtomicLoadAcq(&m->m.r);
    if (prevr >= RWMUTEX_WATERMARK &&
        AtomicCASWeakRelAcq(&m->m.r, &prevr, prevr - RWMUTEX_WATERMARK))
    {
      return mutex_unlock(&m->m);
    }
    safecheckf(prevr >= RWMUTEX_WATERMARK, "no write lock held");
    // spin
    if (retry++ == 100) {
      retry = 0;
      thread_yield();
    }
  }
}


//———————————————————————————————————————————————————————————————————————————————————
// sema_t
//———————————————————————————————————————————————————————————————————————————————————
#if defined(RSM_SEMAPHORE_POSIX)

rerr_t sema_init(sema_t* sp, u32 initcount) {
  int err = sem_init((sem_t*)sp, 0, initcount);
  if (err)
    return (errno == EINVAL) ? rerr_overflow : rerr_errno(err);
  return 0;
}

void sema_dispose(sema_t* sp) {
  sem_destroy((sem_t*)sp);
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

//———————————————————————————————————————————————————————————————————————————————————
#elif defined(_WIN32)
#warning "This was dry coded based on MSDN. Needs testing"

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

//———————————————————————————————————————————————————————————————————————————————————
#elif defined(__MACH__)
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

#endif
//——————————————————————————————————————————————————————————————————————————————————————

#if 0

typedef struct {
  mutex_t*  mu;
  pthread_t t;
} test_thread_t;

static void* nullable test_thread(test_thread_t* t) {
  mutex_lock(t->mu);
  //dlog("test_thread %p", t->t);
  rsm_nanosleep(fastrand() % 1000000);
  mutex_unlock(t->mu);
  return NULL;
}

__attribute__((constructor)) static void test() {
  test_thread_t threads[8];

  mutex_t mu;
  mutex_init(&mu);

  for (usize i = 0; i < countof(threads); i++) {
    test_thread_t* t = &threads[i];
    t->mu = &mu;
    int err = pthread_create(
      &t->t, NULL, (void*nullable(*_Nonnull)(void*))test_thread, t);
    assert(err == 0);
  }

  for (usize i = 0; i < countof(threads); i++) {
    test_thread_t* t = &threads[i];
    void* result = NULL;
    int err = pthread_join(t->t, &result);
    assert(err == 0);
  }
}

#endif

RSM_ASSUME_NONNULL_END
