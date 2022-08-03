// WASM API
// SPDX-License-Identifier: Apache-2.0
#if defined(__wasm__) && !defined(__wasi__)
#include "rsmimpl.h"


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


// WASM imports (provided by rsm.js)
WASM_IMPORT void wasm_log(const char* s, usize len);


// implementation of logv
void logv(const char* _Nonnull format, va_list ap) {
  int n = vsnprintf(tmpbuf, tmpbufcap, format, ap);
  if (n < 0) n = 0;
  wasm_log(tmpbuf, MIN((usize)n, tmpbufcap));
}


// implementation of libc-compatible write()
isize write(int fd, const void* buf, usize nbyte) {
  return rerr_not_supported;
}


// implementation of libc-compatible write()
isize read(int fd, void* buf, usize nbyte) {
  return rerr_not_supported;
}


// memory allocator interface exported to WASM host
WASM_EXPORT void* wmalloc(usize nbyte) { return rmem_alloc(mem, nbyte); }
WASM_EXPORT void* wmresize(void* p, usize newsize, usize oldsize) {
  return rmem_resize(mem, p, newsize, oldsize); }
WASM_EXPORT void wmfree(void* p, usize size) { return rmem_free(mem, p, size); }


// winit is called by rsm.js to initialize the WASM instance state
WASM_EXPORT bool winit(usize memsize) {
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

  // initialize RSM library
  return rsm_init();
}


//———————————————————————————————————————————————————————————————————————————————————
#ifndef RSM_NO_ASM


struct { // compilation result
  // IF THIS CHANGES, UPDATE rsm.js
  rromimg*             rom_img;
  usize                rom_imgsize;
  const char* nullable errmsg;
} cresult;


static bool wdiaghandler(const rdiag* d, void* _) {
  cresult.errmsg = d->msg;
  return d->code != 1; // stop on error
}


WASM_EXPORT void* wcompile(const char* srcname, const char* srcdata, usize srclen) {
  rasm a = {
    .mem = mem,
    .diaghandler = wdiaghandler,
    .srcname = srcname,
    .srcdata = srcdata,
    .srclen = srclen,
  };

  // reset result data
  cresult.errmsg = NULL;
  if (cresult.rom_img)
    rmem_free(mem, cresult.rom_img, cresult.rom_imgsize);
  cresult.rom_img = NULL;
  cresult.rom_imgsize = 0;

  // parse assembly source
  rnode* mod = rasm_parse(&a);
  if UNLIKELY(mod == NULL) {
    cresult.errmsg = "failed to allocate memory for parser";
    return &cresult;
  }
  if UNLIKELY(a.errcount)
    goto end;

  // build ROM
  rrom rom = {0};
  rerr_t err = rasm_gen(&a, mod, mem, &rom);
  if (err) {
    if (cresult.errmsg == NULL)
      cresult.errmsg = rerr_str(err);
    goto end;
  }

  cresult.rom_img = rom.img;
  cresult.rom_imgsize = rom.imgsize;

end:
  rasm_free_rnode(&a, mod);
  return &cresult;
}


#endif // !defined(RSM_NO_ASM)
//———————————————————————————————————————————————————————————————————————————————————


WASM_EXPORT isize wfmtprog(char* buf, usize bufcap, rromimg* rom_img, usize rom_imgsize) {
  // WASM_EXPORT usize wfmtprog(char* buf, usize bufcap, rinstr* nullable ip, u32 ilen)
  if (!rom_img || rom_imgsize == 0)
    return 0;
  rrom rom = { .img=rom_img, .imgsize=rom_imgsize };
  rerr_t err = rsm_loadrom(&rom);
  if (err)
    return -1;
  return rsm_fmtprog(buf, bufcap, rom.code, rom.codelen, /*rfmtflag*/0);
}


WASM_EXPORT rerr_t wvmexec(u64* iregs, rromimg* rom_img, usize rom_imgsize) {
  // allocate instance memory
  static usize memsize = 1024*1024;
  void* membase = rmem_alloc(mem, memsize);
  if (!membase)
    return rerr_nomem;

  // load ROM
  rrom rom = { .img=rom_img, .imgsize=rom_imgsize };
  rerr_t err = rsm_loadrom(&rom);

  // run
  if (!err)
    err = rsm_vmexec(&rom, iregs, membase, memsize);

  // free instance memory
  rmem_free(mem, membase, memsize);
  return err;
}


WASM_EXPORT u64* wvmiregs() {
  return iregs;
}

#endif // __wasm__
