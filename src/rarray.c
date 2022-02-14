#include "rsm.h"

#define RXARRAY_INITCAP 4

void rarray_grow(rarray* a, rmem* m) {
  u32 newcap = RXARRAY_INITCAP;
  usize nbytes = (usize)newcap * sizeof(void*);

  // double existing capacity
  if (a->cap) {
    if (check_mul_overflow(a->cap, 2u, &newcap)) {
      if (a->cap >= U32_MAX-1)
        rmem_fail();
      newcap = U32_MAX;
    }
    if (check_mul_overflow((usize)newcap, sizeof(void*), &nbytes))
      rmem_fail();
  }

  void* p2 = rmem_alloc(m, nbytes);
  if (a->v)
    memcpy(p2, a->v, a->len*sizeof(void*));
  a->v = p2;
  a->cap = newcap;
}
