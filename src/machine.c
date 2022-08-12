// virtual machine
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "machine.h"


rmachine_t* nullable rmachine_create(rmm_t* mm) {
  rmemalloc_t* malloc = rmem_allocator_create(mm, 4 * MiB);
  if (!malloc)
    return NULL;

  rmachine_t* m;

  dlog("sizeof(rmachine_t) %zu", sizeof(rmachine_t));

  // if (sizeof(rmachine_t) > PAGE_SIZE/2) {
  //   usize npages = (sizeof(rmachine_t) + PAGE_SIZE-1) / PAGE_SIZE;
  //   m = rmm_allocpages(mm, npages);
  // } else {
  m = rmem_alloct(malloc, rmachine_t);
  // }

  if (!m) {
    rmem_allocator_free(malloc);
    return NULL;
  }

  m->mm = mm;
  m->malloc = malloc;

  rerr_t err = rsched_init(&m->sched, m);
  if (err) {
    log("rsched_init failed: %s", rerr_str(err));
    rmem_allocator_free(malloc);
    return NULL;
  }

  return m;
}


rerr_t rmachine_execrom(rmachine_t* m, rrom_t* rom) {
  return rsched_execrom(&m->sched, rom);
}


void rmachine_dispose(rmachine_t* m) {
  rsched_dispose(&m->sched);
  rmem_allocator_free(m->malloc);
}

