// dynamically sized array
// SPDX-License-Identifier: Apache-2.0
#pragma once
RSM_ASSUME_NONNULL_BEGIN

typedef struct rarray {
  u8* nullable v; // u8 so we get -Wincompatible-pointer-types if we access .v directly
  u32 len, cap;
} rarray;

#define rarray_at(T, a, index)             (((T*)(a)->v) + (index))
#define rarray_at_safe(T, a, i)            ({safecheck((i)<(a)->len);rarray_at(T,(a),(i));})
#define rarray_push(T, a, m)               ((T*)_rarray_push((a),(m),sizeof(T)))
#define rarray_remove(T, a, start, len)    _rarray_remove((a),sizeof(T),(start),(len))
#define rarray_move(T, a, dst, start, end) _array_move(sizeof(T),(a)->v,(dst),(start),(end))
#define rarray_reserve(T, a, m, addl)      _rarray_reserve((a),(m),sizeof(T),(addl))
#define rarray_free(T, a, ma) ( \
  (a)->v ? rmem_free( (ma), RMEM((a)->v, (usize)(a)->cap * sizeof(T)) ) : ((void)0) \
)

bool rarray_grow(rarray* a, rmemalloc_t*, u32 elemsize, u32 addl);
bool _rarray_reserve(rarray* a, rmemalloc_t*, u32 elemsize, u32 addl);
void _rarray_remove(rarray* a, u32 elemsize, u32 start, u32 len);

inline static void* nullable _rarray_push(rarray* a, rmemalloc_t* ma, u32 elemsize) {
  if (a->len == a->cap && UNLIKELY(!rarray_grow(a, ma, elemsize, 1)))
    return NULL;
  return a->v + elemsize*(a->len++);
}

// _array_move moves the chunk [src,src+len) to index dst. For example:
//   _array_move(z, v, 5, 1, 1+2) = [1  2 3  4 5|6 7 8] ⟹ [1 4 5  2 3  6 7 8]
//   _array_move(z, v, 1, 4, 4+2) = [1|2 3 4  5 6  7 8] ⟹ [1  5 6  2 3 4 7 8]
#define _array_move(elemsize, v, dst, start, end) (                                 \
  (elemsize) == 4 ? _AMOVE_ROTATE(_arotate32,(dst),(start),(end),(u32* const)(v)) : \
  (elemsize) == 8 ? _AMOVE_ROTATE(_arotate64,(dst),(start),(end),(u64* const)(v)) : \
                    _AMOVE_ROTATE(_arotatemem,(dst),(start),(end),(elemsize),(v)) )
#define _AMOVE_ROTATE(f, dst, start, end, args...) (     \
  ((start)==(dst)||(start)==(end)) ? ((void)0) :         \
  ((start) > (dst)) ? (f)(args, (dst), (start), (end)) : \
  (f)(args, (start), (end), (dst)) )

// arotate rotates the order of v in the range [first,last) in such a way
// that the element pointed to by "mid" becomes the new "first" element.
// Assumes first <= mid < last.
#define arotate(elemsize, v, first, mid, last) (                          \
  (elemsize) == 4 ? _arotate32((u32* const)(v), (first), (mid), (last)) : \
  (elemsize) == 8 ? _arotate64((u64* const)(v), (first), (mid), (last)) : \
  _arotatemem((elemsize), (v), (first), (mid), (last)) )
void _arotatemem(u32 stride, void* v, u32 first, u32 mid, u32 last);
void _arotate32(u32* const v, u32 first, u32 mid, u32 last);
void _arotate64(u64* const v, u32 first, u32 mid, u32 last);

RSM_ASSUME_NONNULL_END
