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
static rmemalloc_t* mem;             // the one memory allocator, owning the wasm heap
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
WASM_EXPORT void* wmalloc(usize nbyte) { return rmem_alloc(mem, nbyte).p; }
WASM_EXPORT void* wmresize(void* p, usize newsize, usize oldsize) {
  rmem_t m = RMEM(p, oldsize);
  return rmem_resize(mem, &m, newsize) ? m.p : NULL;
}
WASM_EXPORT void wmfree(void* p, usize size) { return rmem_free(mem, RMEM(p, size)); }


// winit is called by rsm.js to initialize the WASM instance state
WASM_EXPORT bool winit(usize memsize) {
  u8* heap = &__heap_base;
  usize heapsize = (memsize < (usize)heap) ? 0 : memsize - (usize)heap;

  #if DEBUG
  // we haven't allocated tmpbuf yet, so use stack space for early log messages
  char tmpbufstk[512];
  tmpbufcap = sizeof(tmpbufstk);
  tmpbuf = tmpbufstk;
  u8* stackbase = &__data_end;
  log("data: %zu B, stack: %p-%p (%zu B), heap %p (%zu B)",
    (usize)stackbase, stackbase, heap, (usize)(heap - stackbase), heap, heapsize);
  #endif

  // first chunk of heap used for tmpbuf
  tmpbufcap = (heapsize > 4096*4) ? 4096 : 512;
  tmpbuf = (char*)heap;
  heap += tmpbufcap;
  heapsize -= tmpbufcap;

  // rest of heap is owned by our one allocator
  mem = rmem_allocator_create_buf(NULL, heap, heapsize);
  if (!mem)
    return false;

  // initialize RSM library
  return rsm_init();
}


//———————————————————————————————————————————————————————————————————————————————————
#ifndef RSM_NO_ASM


struct { // compilation result
  // IF THIS CHANGES, UPDATE rsm.js
  rromimg_t*           rom_img;
  usize                rom_imgsize;
  usize                rom_imgmemsize;
  const char* nullable errmsg;
} cresult;


static bool wdiaghandler(const rdiag_t* d, void* _) {
  cresult.errmsg = d->msg;
  return d->code != 1; // stop on error
}


WASM_EXPORT void* wcompile(const char* srcname, const char* srcdata, usize srclen) {
  rasm_t a = {
    .memalloc = mem,
    .diaghandler = wdiaghandler,
    .srcname = srcname,
    .srcdata = srcdata,
    .srclen = srclen,
  };

  // reset result data
  cresult.errmsg = NULL;
  if (cresult.rom_img)
    rmem_free(mem, RMEM(cresult.rom_img, cresult.rom_imgmemsize));
  cresult.rom_img = NULL;
  cresult.rom_imgsize = 0;
  cresult.rom_imgmemsize = 0;

  // parse assembly source
  rnode_t* mod = rasm_parse(&a);
  if UNLIKELY(mod == NULL) {
    cresult.errmsg = "failed to allocate memory for parser";
    return &cresult;
  }
  if UNLIKELY(a.errcount)
    goto end;

  // build ROM
  rrom_t rom = {0};
  rerr_t err = rasm_gen(&a, mod, &rom);
  if (err) {
    if (cresult.errmsg == NULL)
      cresult.errmsg = rerr_str(err);
    goto end;
  }

  cresult.rom_img = rom.img;
  cresult.rom_imgsize = rom.imgsize;
  cresult.rom_imgmemsize = rom.imgmemsize;

end:
  rasm_free_rnode(&a, mod);
  return &cresult;
}


#endif // !defined(RSM_NO_ASM)
//———————————————————————————————————————————————————————————————————————————————————


static rerr_t wloadrom(rrom_t* rom, rmem_t* loadmem) {
  *loadmem = RMEM(NULL, 0);
  if ((rom->img->flags & RROM_LZ4) != 0) {
    usize dstsize = rromimg_loadsize(rom->img, rom->imgsize);
    if (dstsize == USIZE_MAX)
      return rerr_invalid;
    *loadmem = rmem_alloc_aligned(mem, dstsize, RSM_ROM_ALIGN);
    if (loadmem->p == NULL)
      return rerr_nomem;
  }
  rerr_t err = rsm_loadrom(rom, *loadmem);
  if (err && loadmem->p) {
    rmem_free(mem, *loadmem);
    *loadmem = RMEM(NULL, 0);
  }
  return err;
}


WASM_EXPORT isize wfmtprog(char* buf, usize bufcap, rromimg_t* rom_img, usize rom_imgsize) {
  // WASM_EXPORT usize wfmtprog(char* buf, usize bufcap, rinstr* nullable ip, u32 ilen)
  if (!rom_img || rom_imgsize == 0)
    return 0;
  rrom_t rom = { .img=rom_img, .imgsize=rom_imgsize };
  rmem_t loadmem;
  rerr_t err = wloadrom(&rom, &loadmem);
  if (err)
    return -1;
  isize n = (isize)rsm_fmtprog(buf, bufcap, rom.code, rom.codelen, /*rfmtflag*/0);
  if (loadmem.p)
    rmem_free(mem, loadmem);
  return n;
}


WASM_EXPORT rerr_t wvmexec(u64* iregs, rromimg_t* rom_img, usize rom_imgsize) {
  // allocate instance memory
  static usize memsize = 1024*1024;
  rmem_t vmmem = rmem_alloc(mem, memsize);
  if (!vmmem.p)
    return rerr_nomem;

  // load ROM
  rrom_t rom = { .img=rom_img, .imgsize=rom_imgsize };
  rmem_t loadmem;
  rerr_t err = wloadrom(&rom, &loadmem);

  // run
  if (!err) {
    rvm_t vm = {
      .rambase = vmmem.p,
      .ramsize = vmmem.size,
    };
    memcpy(vm.iregs, iregs, sizeof(vm.iregs));
    err = rsm_vmexec(&vm, &rom, mem);
    memcpy(iregs, vm.iregs, sizeof(vm.iregs));
  }

  // free instance memory
  if (loadmem.p)
    rmem_free(mem, loadmem);
  rmem_free(mem, vmmem);
  return err;
}


WASM_EXPORT u64* wvmiregs() {
  return iregs;
}

#endif // __wasm__
