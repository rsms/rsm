// buffer-backed memory allocator
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"


#define RMEM_ALIGN sizeof(void*) // allocation alignment (must be pow2)

// buffer-backed allocator, used for both rmem_mkvmalloc and rmem_mkbufalloc
typedef struct bufalloc bufalloc;
struct bufalloc {
  void* buf; // memory buffer
  usize len; // number of bytes used up in the memory buffer
  usize cap; // memory buffer size in bytes
  bool  ismmap;
};

static_assert(sizeof(bufalloc) == RMEM_MK_MIN, "");

inline static bool ba_istail(bufalloc* a, void* p, usize size) {
  return a->buf + a->len == p + size;
}

inline static usize ba_avail(bufalloc* a) { // available capacity
  return a->cap - a->len;
}

// main allocator implementation
//   ba_alloc(s, NULL,       0, newsize) = new allocation
//   ba_alloc(s,    p, oldsize, newsize) = resize allocation
//   ba_alloc(s,    p, oldsize,       0) = free allocation
static void* nullable ba_alloc(void* state, void* nullable p, usize oldsize, usize newsize) {
  bufalloc* a = state;
  if UNLIKELY(p != NULL) {
    if LIKELY(newsize == 0) {
      // free -- ba_alloc(s,p,>0,0)
      if (ba_istail(a, p, oldsize))
        a->len -= oldsize;
      return NULL; // ignored by caller
    }
    // resize -- ba_alloc(s,p,>0,>0)
    assert(oldsize > 0);
    if UNLIKELY(newsize <= oldsize) {
      // shrink
      if (ba_istail(a, p, oldsize))
        a->len -= oldsize - newsize;
      return p;
    }
    // grow
    if (ba_istail(a, p, oldsize)) {
      // extend
      if UNLIKELY(ba_avail(a) < newsize - oldsize)
        return NULL; // out of memory
      a->len += newsize - oldsize;
      return p;
    }
    // relocate
    void* p2 = ba_alloc(state, NULL, 0, newsize);
    if UNLIKELY(p2 == NULL)
      return NULL;
    return memcpy(p2, p, oldsize);
  }
  // new -- ba_alloc(s,NULL,0,>0)
  assert(oldsize == 0);
  assert(newsize > 0);
  if UNLIKELY(ba_avail(a) < newsize) {
    return NULL; // out of memory
  }
  void* p2 = a->buf + a->len;
  a->len += newsize;
  return p2;
}

static rmem ba_make(void* ap, void* buf, usize size, bool ismmap) {
  bufalloc* a = ap;
  a->buf = buf;
  a->cap = size;
  a->len = 0;
  a->ismmap = ismmap;
  return (rmem){&ba_alloc, a};
}

static void* rmem_align(void* buf, usize* sizep) {
  uintptr addr = ALIGN2((uintptr)buf, RMEM_ALIGN);
  if LIKELY(addr == (uintptr)buf)
    return buf;
  usize offs = (usize)(addr - (uintptr)buf);
  assertf(offs >= *sizep, "unaligned address with too small size");
  *sizep = (offs > *sizep) ? 0 : *sizep - offs;
  return (void*)addr;
}

rmem rmem_initbufalloc(void* astate[4], void* buf, usize size) {
  assert(ALIGN2((uintptr)astate, RMEM_ALIGN) == (uintptr)astate); // must be aligned
  buf = rmem_align(buf, &size);
  return ba_make((void*)astate, buf, size, false);
}

rmem rmem_mkbufalloc(void* buf, usize size) {
  buf = rmem_align(buf, &size);
  assert(size >= RMEM_MK_MIN);
  return ba_make(buf, buf + sizeof(bufalloc), size - sizeof(bufalloc), false);
}

rmem rmem_mkvmalloc(usize size) {
  usize pagesize = mem_pagesize();
  if (size == 0) { // "I just want a huge allocation"
    size = U32_MAX + (pagesize - (U32_MAX % pagesize));
    for (;;) {
      void* buf = vmem_alloc(size);
      if LIKELY(buf != NULL)
        return ba_make(buf, buf + sizeof(bufalloc), size - sizeof(bufalloc), true);
      if (size <= 0xffff) // we weren't able to allocate 16kB; give up
        return (rmem){0,0};
      // try again with half the size
      size >>= 1;
    }
  }
  // caller requested minimum size; round up to pagesize
  assert(RMEM_MK_MIN < pagesize); // we assume pagesize is (much) larger than RMEM_MK_MIN
  assert(ALIGN2(pagesize, RMEM_ALIGN) == pagesize); // we assume pagesize is RMEM_ALIGNed
  usize rem = size % pagesize;
  if (rem)
    size += pagesize - rem;
  void* buf = vmem_alloc(size);
  if UNLIKELY(buf == NULL)
    return (rmem){0,0};
  return ba_make(buf, buf + sizeof(bufalloc), size - sizeof(bufalloc), true);
}

void rmem_freealloc(rmem m) {
  bufalloc* a = m.state;
  if (a->ismmap) {
    void* buf = a->buf;
    usize cap = a->cap;
    #if DEBUG
    memset(a, 0, sizeof(*a));
    #endif
    vmem_free(buf, cap);
    return;
  }
  #if DEBUG
  memset(a, 0, sizeof(*a));
  #endif
}
