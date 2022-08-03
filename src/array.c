// dynamically sized array
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "array.h"

void _rarray_remove(rarray* a, u32 elemsize, u32 start, u32 len) {
  if (len == 0)
    return;
  safecheckf(start+len <= a->len, "end=%u > len=%u", start+len, a->len);
  if (start+len < a->len) {
    void* dst = a->v + elemsize*start;
    void* src = dst + elemsize*len;
    memmove(dst, src, elemsize*(a->len - start - len));
  }
  a->len -= len;
}

bool rarray_grow(rarray* a, rmemalloc_t* ma, u32 elemsize, u32 addl) {
  u32 newcap = a->cap ? (u32)MIN((u64)a->cap * 2, U32_MAX) : MAX(addl, 4);
  usize newsize;
  if (check_mul_overflow((usize)newcap, (usize)elemsize, &newsize))
    return false;
  rmem_t m = { a->v, (usize)(a->cap * elemsize) };
  if UNLIKELY(!rmem_resize(ma, &m, newsize))
    return false;
  assertf(m.size/(usize)elemsize >= newcap, "bug in rmem_resize %u", a->cap * elemsize);
  a->v = m.p;
  a->cap = (u32)(m.size / (usize)elemsize);
  return true;
}

bool _rarray_reserve(rarray* a, rmemalloc_t* ma, u32 elemsize, u32 addl) {
  u32 len;
  if (check_add_overflow(a->len, addl, &len))
    return false;
  if (len >= a->cap && UNLIKELY(!rarray_grow(a, ma, elemsize, addl)))
    return false;
  return true;
}

void _arotatemem(u32 stride, void* v, u32 first, u32 mid, u32 last) {
  assert(first <= mid); // if equal (zero length), do nothing
  assert(mid < last);
  usize tmp[16]; assert(sizeof(u32) <= sizeof(tmp));
  u32 next = mid;
  while (first != next) {
    // swap
    memcpy(tmp, v + first*stride, stride); // tmp = v[first]
    memcpy(v + first*stride, v + next*stride, stride); // v[first] = v[next]
    memcpy(v + next*stride, tmp, stride); // v[next] = tmp
    first++;
    next++;
    if (next == last) {
      next = mid;
    } else if (first == mid) {
      mid = next;
    }
  }
}

#define DEF_AROTATE(NAME, T)                                   \
  void NAME(T* const v, u32 first, u32 mid, u32 last) { \
    assert(first <= mid);                                      \
    assert(mid < last);                                        \
    u32 next = mid;                                            \
    while (first != next) {                                    \
      T tmp = v[first]; v[first++] = v[next]; v[next++] = tmp; \
      if (next == last) next = mid;                            \
      else if (first == mid) mid = next;                       \
    }                                                          \
  }

DEF_AROTATE(_arotate32, u32)
DEF_AROTATE(_arotate64, u64)
