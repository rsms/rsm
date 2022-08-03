// misc memory functions
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

bool rmem_align(rmem_t* region, usize alignment) {
  assertf(IS_POW2(alignment), "%zu", alignment);
  assert(RMEM_IS_VALID(*region));
  uintptr addr = ALIGN2((uintptr)region->p, alignment);
  usize size_reduction = (usize)(addr - (uintptr)region->p);
  if (check_sub_overflow(region->size, size_reduction, &region->size))
    return false;
  region->p = (void*)addr;
  return true;
}
