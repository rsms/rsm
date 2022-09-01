// scheduler: 
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "sched.h"


// TODO: different thread APIs
#if !defined(RSM_NO_LIBC)
  #include <pthread.h>
  #include <signal.h>
  #include <errno.h>
// #elif !__STDC_NO_THREADS__
//   #include <threads.h>
#else
  #error TODO
#endif


uintptr m_spawn_osthread(M* m, rerr_t(*mainf)(M*)) {
  pthread_attr_t attr;
  int err = pthread_attr_init(&attr);
  if (err != 0) {
    dlog("pthread_attr_init failed (err %d)", err);
    return 0;
  }

  // // find out OS stack size
  // uintptr stacksize = 0;
  // if ((err = pthread_attr_getstacksize(&attr, &stacksize)) != 0) {
  //   dlog("pthread_attr_getstacksize failed (err %d)", err);
  //   goto error;
  // }
  // dlog("OS thread stack size: %zu B", stacksize);
  // //m->t0.stack.hi = stacksize; // for m_start

  // Tell the pthread library we won't join with this thread and that
  // the system should reclaim the thread storage upon exit.
  if ((err = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED)) != 0) {
    dlog("pthread_attr_setdetachstate failed (err %d)", err);
    goto error;
  }

  // disable signal delivery of all signals while we create the thread
  sigset_t sigset_prev;
  sigset_t sigset_all; // saved signal mask
  memset(&sigset_all, 0xff, sizeof(sigset_t));
  if (sigprocmask(SIG_SETMASK, &sigset_all, &sigset_prev) != 0) {
    dlog("sigprocmask failed (errno %d)", errno);
    goto error;
  }

  // create the thread, executing mainf
  DIAGNOSTIC_IGNORE_PUSH("-Wcast-function-type")
  pthread_t t;
  err = pthread_create(&t, &attr, (void*nullable(*_Nonnull)(void*nullable))mainf, m);
  DIAGNOSTIC_IGNORE_POP()

  // restore signal mask
  sigprocmask(SIG_SETMASK, &sigset_prev, NULL);

  if (err != 0) {
    dlog("pthread_create failed (err %d)", err);
    goto error;
  }

  return (uintptr)t;

error:
  pthread_attr_destroy(&attr);
  return 0;
}
