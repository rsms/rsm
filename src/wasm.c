// WASM API
// SPDX-License-Identifier: Apache-2.0
#if defined(__wasm__) && !defined(__wasi__)
#include "rsmimpl.h"

#define EXPORT __attribute__((visibility("default")))
#define IMPORT __attribute__((visibility("default")))

// WASM imports (provided by rsm.js)
IMPORT void log1(const char* s, usize len);

// WASM memory layout
//        0        __data_end    __heap_base
//        ┌─────────────┼─────────────┼───────────···
//        │ data        │ stack       │ heap
//        └─────────────┴─────────────┴───────────···
extern u8 __heap_base, __data_end; // symbols provided by wasm-ld

// WASM instance state
static rmem  mem;                    // the one memory allocator, owning the wasm heap
static u64   iregs[RSM_NREGS] = {0}; // register state
static char* tmpbuf;                 // for temporary formatting etc
static usize tmpbufcap;
struct { // wasm_compile result
  usize   c;
  rinstr* v;
  const char* nullable errmsg;
} cresult;

// winit is called by rsm.js to initialize the WASM instance state
EXPORT void winit(usize memsize) {
  void* heap = &__heap_base;
  usize heapsize = (memsize < (usize)heap) ? 0 : memsize - (usize)heap;

  #if DEBUG
  // we haven't allocated tmpbuf yet, so use stack space for early log messages
  char tmpbufstk[512];
  tmpbufcap = sizeof(tmpbufstk);
  tmpbuf = tmpbufstk;
  void* stackbase = &__data_end;
  log("data: %zu B, stack: %p-%p (%zu B), heap %p (%zu B)",
    (usize)stackbase, stackbase, heap, (usize)(heap - stackbase), heap, heapsize);
  #endif

  // first chunk of heap used for tmpbuf
  tmpbufcap = (heapsize > 4096*4) ? 4096 : 512;
  tmpbuf = heap;
  heap += tmpbufcap;
  heapsize -= tmpbufcap;

  // rest of heap is owned by our one allocator
  mem = rmem_mkbufalloc(heap, heapsize);
}

void logv(const char* _Nonnull format, va_list ap) {
  int n = vsnprintf(tmpbuf, tmpbufcap, format, ap);
  if (n < 0) n = 0;
  log1(tmpbuf, MIN((usize)n, tmpbufcap));
}

EXPORT void* wmalloc(usize nbyte) { return rmem_alloc(mem, nbyte); }
EXPORT void* wmresize(void* p, usize newsize, usize oldsize) {
  return rmem_resize(mem, p, newsize, oldsize); }
EXPORT void wmfree(void* p, usize size) { return rmem_free(mem, p, size); }

static bool diaghandler(const rdiag* d, void* _) {
  cresult.errmsg = d->msg;
  return d->code != 1; // stop on error
}

EXPORT void* wcompile(const char* srcname, const char* srcdata, usize srclen) {
  rcomp comp = {
    .mem         = mem,
    .srcdata     = srcdata,
    .srclen      = srclen,
    .srcname     = srcname,
    .diaghandler = &diaghandler,
  };
  cresult.errmsg = NULL;
  cresult.c = rsm_compile(&comp, mem, &cresult.v);
  return &cresult;
}

EXPORT usize wfmtprog(char* buf, usize bufcap, rinstr* nullable inv, u32 inc) {
  return rsm_fmtprog(buf, bufcap, inv, inc);
}

EXPORT void wvmexec(u64* iregs, rinstr* inv, u32 inc) {
  return rsm_vmexec(iregs, inv, inc);
}

EXPORT u64* wvmiregs() {
  return iregs;
}

#endif // __wasm__
