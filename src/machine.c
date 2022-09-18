// virtual machine
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "machine.h"


rmachine_t* nullable rmachine_create(rmm_t* mm) {
  rmemalloc_t* malloc = rmem_allocator_create(mm, 4 * MiB);
  if (!malloc)
    return NULL;

  // note that we must allocate pages in pow2 orders
  usize npages = CEIL_POW2((sizeof(rmachine_t) + PAGE_SIZE-1) / PAGE_SIZE);
  u16 tailmemcap = (u16)(npages * PAGE_SIZE) - sizeof(rmachine_t);

  rmachine_t* m = rmm_allocpages(mm, npages);
  if UNLIKELY(!m) {
    log("rmm_allocpages(%zu) failed", npages);
    goto error;
  }

  dlog("%s allocating %zu pages " RMEM_FMT " rmachine_t=%zu",
    __FUNCTION__, npages, RMEM_FMT_ARGS(RMEM(m, npages*PAGE_SIZE)), sizeof(rmachine_t));

  m->mm = mm;
  m->malloc = malloc;
  m->tailmemcap = tailmemcap;

  rerr_t err = rsched_init(&m->sched, m);
  if UNLIKELY(err) {
    log("rsched_init failed: %s", rerr_str(err));
    goto error;
  }

  return m;

error:
  rmem_allocator_free(malloc);
  if (m)
    rmm_freepages(mm, m, npages);
  return NULL;
}


rerr_t rmachine_execrom(rmachine_t* m, rrom_t* rom) {
  return rsched_execrom(&m->sched, rom);
}


void rmachine_dispose(rmachine_t* m) {
  rsched_dispose(&m->sched);
  rmem_allocator_free(m->malloc);
}

