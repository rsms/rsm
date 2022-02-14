#include "rsm.h"

#ifdef R_WITH_LIBC
  #include <stdlib.h> // calloc, free
  #include <sys/mman.h> // mmap

  #ifdef _WIN32
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
    #define MEM_PAGESIZE malloc_getpagesize
  #elif defined(malloc_getpagesize)
    #define MEM_PAGESIZE malloc_getpagesize
  #else
    #include <unistd.h>
    #ifdef _SC_PAGESIZE  /* some SVR4 systems omit an underscore */
      #ifndef _SC_PAGE_SIZE
        #define _SC_PAGE_SIZE _SC_PAGESIZE
      #endif
    #endif
    #ifdef _SC_PAGE_SIZE
      #define MEM_PAGESIZE sysconf(_SC_PAGE_SIZE)
    #elif defined(BSD) || defined(DGUX) || defined(R_HAVE_GETPAGESIZE)
      extern size_t getpagesize();
      #define MEM_PAGESIZE getpagesize()
    #else
      #include <sys/param.h>
      #ifdef EXEC_PAGESIZE
        #define MEM_PAGESIZE EXEC_PAGESIZE
      #elif defined(NBPG)
        #ifndef CLSIZE
          #define MEM_PAGESIZE NBPG
        #else
          #define MEM_PAGESIZE (NBPG * CLSIZE)
        #endif
      #elif defined(NBPC)
          #define MEM_PAGESIZE NBPC
      #elif defined(PAGESIZE)
        #define MEM_PAGESIZE PAGESIZE
      #endif
    #endif
    #include <sys/types.h>
    #include <sys/mman.h>
    #include <sys/resource.h>
    #if defined(__MACH__) && defined(__APPLE__)
      #include <mach/vm_statistics.h>
      #include <mach/vm_prot.h>
    #endif
    #ifndef MAP_ANON
      #define MAP_ANON MAP_ANONYMOUS
    #endif
    #define HAS_MMAP
    #define HAS_MMAP_ZEROED_PAGES  // TODO only define when we know this is true
  #endif // _WIN32
#endif // R_WITH_LIBC
#ifndef MEM_PAGESIZE
  #define MEM_PAGESIZE ((usize)4096U)
#endif


enum rmemflag {
  rmem_f_vm = 1 << 0, // m->buf is part of the system's virtual memory
};


static rmem* rmem_make_inbuf(void* buf, usize size, u32 flags) {
  assert(size >= RMEM_MIN);
  rmem* m = buf;
  m->buf = buf;
  m->cap = size;
  m->len = rmem_align_size(sizeof(*m));
  m->flags = flags;
  return m;
}

rmem* rmem_make(void* buf, usize size) {
  uintptr addr = rmem_align_addr(buf);
  if UNLIKELY(addr != (uintptr)buf) {
    if (addr - (uintptr)buf > size) {
      size = 0;
    } else {
      size -= addr - (uintptr)buf;
    }
    buf = (void*)addr;
  }
  return rmem_make_inbuf(buf, size, 0);
}

rmem* nullable rmem_makevm(usize size) {
  #ifdef HAS_MMAP
    if (size < RMEM_MIN)
      size = RMEM_MIN;

    // round up size to nearest page size
    usize pagesize = MEM_PAGESIZE;
    usize rem = size % pagesize;
    if (rem)
      size += pagesize - rem;

    void* buf = rmem_vmalloc(size);
    if UNLIKELY(buf == NULL)
      return NULL;

    return rmem_make_inbuf(buf, size, rmem_f_vm);
  #else
    return NULL;
  #endif
}

void rmem_release(rmem* m) {
  if (m->flags & 1) {
    void* buf = m->buf;
    usize cap = m->cap;
    #if DEBUG
    memset(m, 0, sizeof(*m));
    #endif
    rmem_vmfree(buf, cap);
    return;
  }
  #if DEBUG
  memset(m, 0, sizeof(*m));
  #endif
}

void* _rmem_allocz(rmem* m, usize size) {
  usize avail = m->cap - m->len;
  if UNLIKELY(avail < size)
    rmem_fail();
  void* p = m->buf + m->len;
  m->len += size;
  return p;
}

void* rmem_alloczv(rmem* m, usize elemsize, usize count) {
  usize nbytes;
  if (check_mul_overflow(elemsize, count, &nbytes))
    panic("allocation too large");
  return rmem_allocz(m, nbytes);
}


void* nullable rmem_resize(rmem* m, void* p, usize newsize) {
  if (newsize == 0) {
    rmem_free(p);
    return NULL;
  }
  // TODO
  return p;
}


usize rmem_vmpagesize() {
  return MEM_PAGESIZE;
}


void* nullable rmem_vmalloc(usize nbytes) {
  #ifndef HAS_MMAP
    return NULL;
  #else
    if (nbytes == 0)
      return NULL;

    #if defined(DEBUG) && defined(HAS_MPROTECT)
      usize nbytes2;
      if (check_add_overflow(nbytes, MEM_PAGESIZE, &nbytes2)) {
        // nbytes too large
        nbytes2 = 0;
      } else {
        nbytes += MEM_PAGESIZE;
      }
    #endif

    #if defined(__MACH__) && defined(__APPLE__) && defined(VM_PROT_DEFAULT)
      // vm_map_entry_is_reusable uses VM_PROT_DEFAULT as a condition for page reuse.
      // See http://fxr.watson.org/fxr/source/osfmk/vm/vm_map.c?v=xnu-2050.18.24#L10705
      int mmapprot = VM_PROT_DEFAULT;
    #else
      int mmapprot = PROT_READ | PROT_WRITE;
    #endif

    int mmapflags = MAP_PRIVATE | MAP_ANON
      #ifdef MAP_NOCACHE
      | MAP_NOCACHE // don't cache pages for this mapping
      #endif
      #ifdef MAP_NORESERVE
      | MAP_NORESERVE // don't reserve needed swap area
      #endif
    ;

    #if defined(__MACH__) && defined(__APPLE__) && defined(VM_FLAGS_PURGABLE)
      int fd = VM_FLAGS_PURGABLE; // Create a purgable VM object for new VM region
    #else
      int fd = -1;
    #endif

    void* ptr = mmap(0, nbytes, mmapprot, mmapflags, fd, 0);
    if UNLIKELY(ptr == MAP_FAILED)
      return NULL;

    // Pretty much all OSes return zeroed anonymous memory pages.
    // Do a little sample in debug mode just to be sure.
    #if defined(HAS_MMAP_ZEROED_PAGES) && defined(DEBUG)
    const u8 zero[128] = {0};
    assert(MEM_PAGESIZE >= 128);
    assert(memcmp(ptr, zero, sizeof(zero)) == 0); // or: got random data from mmap!
    #endif

    // protect the last page from access to cause a crash on out of bounds access
    #if defined(DEBUG) && defined(HAS_MPROTECT)
      if (nbytes2 != 0) {
        const usize pagesize = MEM_PAGESIZE;
        assert(nbytes > pagesize);
        void* protPagePtr = ptr;
        protPagePtr = &((u8*)ptr)[nbytes - pagesize];
        int status = mprotect(protPagePtr, pagesize, PROT_NONE);
        if LIKELY(status == 0) {
          *nbytes = nbytes - pagesize;
        } else {
          dlog("mprotect failed");
        }
      }
    #endif

    return ptr;
  #endif // HAS_MMAP
}


bool rmem_vmfree(void* ptr, usize nbytes) {
  #ifdef HAS_MMAP
    return munmap(ptr, nbytes) == 0;
  #else
    return false;
  #endif
}


void* rmem_dup(rmem* m, const void* p, usize len) {
  void* p2 = rmem_alloc(m, len);
  memcpy(p2, p, len);
  return p2;
}
