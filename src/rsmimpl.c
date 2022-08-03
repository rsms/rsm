// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "hash.h"

#ifndef RSM_NO_LIBC
  #include <stdio.h>
  #include <fcntl.h>
  #include <errno.h>
  #include <unistd.h>
  #include <stdlib.h> // free
  #include <execinfo.h> // backtrace* (for _panic)
  #include <sys/stat.h>
  #include <sys/mman.h> // mmap
  #include <string.h> // strerror

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
    #ifndef MAP_ANONYMOUS
      #ifdef MAP_ANON
        #define MAP_ANONYMOUS MAP_ANON
      #else
        #define MAP_ANONYMOUS 0
      #endif
    #endif
    // MAP_NORESERVE flag says "don't reserve needed swap area"
    #ifndef MAP_NORESERVE
      #define MAP_NORESERVE 0
    #endif
    #define HAS_MMAP
  #endif // _WIN32
#endif // RSM_NO_LIBC
#ifndef MEM_PAGESIZE
  // fallback value
  #define MEM_PAGESIZE ((usize)4096U)
#endif


static_assert(RSM_POPCOUNT_X(0u) == 0, "");
static_assert(RSM_POPCOUNT_X(1u) == 1, "");
static_assert(RSM_POPCOUNT_X(10u) == 2, "");
static_assert(RSM_POPCOUNT_X(31u) == 5, "");
static_assert(RSM_POPCOUNT_X(32u) == 1, "");
static_assert(RSM_POPCOUNT_X(33u) == 2, "");
static_assert(RSM_POPCOUNT_X(0xFFFFFFFFu) == 32, "");

static_assert(RSM_POPCOUNT_X(0xFFFFFFFFFFFFFFFFllu) == 64, "");
static_assert(RSM_POPCOUNT_X(0xFFFFFFFFFFFFllu) == 48, "");
static_assert(RSM_POPCOUNT_X(0xFFFFFFFFFFFFllu + 1) == 1, "");
static_assert(RSM_POPCOUNT_X(0xFFFFFFFFFFFFllu + 10) == 3, "");
static_assert(RSM_POPCOUNT_X(0xFFFFFFFFFFFFllu + 31) == 5, "");
static_assert(RSM_POPCOUNT_X(0xFFFFFFFFFFFFllu + 32) == 6, "");
static_assert(RSM_POPCOUNT_X(0xFFFFFFFFFFFFllu + 33) == 2, "");


static_assert(RSM_CLZ_X(0u) == 32, "");
static_assert(RSM_CLZ_X(1u) == 31, "");
static_assert(RSM_CLZ_X(0xFFFFFFFFu) == 0, "");
static_assert(RSM_CLZ_X(10u) == 28, "");
static_assert(RSM_CLZ_X(31u) == 27, "");
static_assert(RSM_CLZ_X(32u) == 26, "");
static_assert(RSM_CLZ_X(33u) == 26, "");

static_assert(RSM_CLZ_X(0llu) == 64, "");
static_assert(RSM_CLZ_X(1llu) == 63, "");
static_assert(RSM_CLZ_X(0xFFFFFFFFFFFFFFFFllu) == 0, "");
static_assert(RSM_CLZ_X(10llu) == 60, "");
static_assert(RSM_CLZ_X(31llu) == 59, "");
static_assert(RSM_CLZ_X(32llu) == 58, "");
static_assert(RSM_CLZ_X(33llu) == 58, "");


static_assert(RSM_FLS_X(0) == 0, "0");
static_assert(RSM_FLS_X(1) == 1, "1");
static_assert(RSM_FLS_X(2) == 2, "10");
static_assert(RSM_FLS_X(3) == 2, "11");
static_assert(RSM_FLS_X(128) == 8, "10000000");


static_assert(CEIL_POW2_X(0xFFFFFFFFu) == 0xFFFFFFFFu, "");
static_assert(CEIL_POW2_X(1u) == 1, "");
static_assert(CEIL_POW2_X(10u) == 16, "");
static_assert(CEIL_POW2_X(31u) == 32, "");
static_assert(CEIL_POW2_X(32u) == 32, "");
static_assert(CEIL_POW2_X(33u) == 64, "");
static_assert(CEIL_POW2_X(64u) == 64, "");
static_assert(CEIL_POW2_X(100u) == 128, "");
static_assert(CEIL_POW2_X(128u) == 128, "");
static_assert(CEIL_POW2_X(200u) == 256, "");
static_assert(CEIL_POW2_X(256u) == 256, "");
static_assert(CEIL_POW2_X(300u) == 512, "");
static_assert(CEIL_POW2_X(400u) == 512, "");
static_assert(CEIL_POW2_X(500u) == 512, "");
static_assert(CEIL_POW2_X(512u) == 512, "");
static_assert(CEIL_POW2_X(600u) == 1024, "");

static_assert(CEIL_POW2_X(0xFFFFFFFFFFFFFFFFllu) == 0xFFFFFFFFFFFFFFFFllu, "");
static_assert(CEIL_POW2_X(1llu) == 1, "");
static_assert(CEIL_POW2_X(10llu) == 16, "");
static_assert(CEIL_POW2_X(31llu) == 32, "");
static_assert(CEIL_POW2_X(32llu) == 32, "");
static_assert(CEIL_POW2_X(33llu) == 64, "");
static_assert(CEIL_POW2_X(64llu) == 64, "");
static_assert(CEIL_POW2_X(100llu) == 128, "");
static_assert(CEIL_POW2_X(128llu) == 128, "");
static_assert(CEIL_POW2_X(200llu) == 256, "");
static_assert(CEIL_POW2_X(256llu) == 256, "");
static_assert(CEIL_POW2_X(300llu) == 512, "");
static_assert(CEIL_POW2_X(400llu) == 512, "");
static_assert(CEIL_POW2_X(500llu) == 512, "");
static_assert(CEIL_POW2_X(512llu) == 512, "");
static_assert(CEIL_POW2_X(600llu) == 1024, "");


static_assert(FLOOR_POW2_X(0xFFFFFFFFu) == 0xFFFFFFFF, "");
static_assert(FLOOR_POW2_X(0xFFFFFFFFu-1) == 0x80000000, "");
static_assert(FLOOR_POW2_X(0u) == 1, "");
static_assert(FLOOR_POW2_X(1u) == 1, "");
static_assert(FLOOR_POW2_X(10u) == 8, "");
static_assert(FLOOR_POW2_X(31u) == 16, "");
static_assert(FLOOR_POW2_X(32u) == 32, "");
static_assert(FLOOR_POW2_X(33u) == 32, "");
static_assert(FLOOR_POW2_X(64u) == 64, "");
static_assert(FLOOR_POW2_X(100u) == 64, "");
static_assert(FLOOR_POW2_X(128u) == 128, "");
static_assert(FLOOR_POW2_X(200u) == 128, "");
static_assert(FLOOR_POW2_X(256u) == 256, "");
static_assert(FLOOR_POW2_X(300u) == 256, "");
static_assert(FLOOR_POW2_X(400u) == 256, "");
static_assert(FLOOR_POW2_X(500u) == 256, "");
static_assert(FLOOR_POW2_X(512u) == 512, "");
static_assert(FLOOR_POW2_X(600u) == 512, "");

static_assert(FLOOR_POW2_X(0xFFFFFFFFFFFFFFFFllu) == 0xFFFFFFFFFFFFFFFF, "");
static_assert(FLOOR_POW2_X(0xFFFFFFFFFFFFFFFFllu-1) == 0x8000000000000000, "");
static_assert(FLOOR_POW2_X(0llu) == 1, "");
static_assert(FLOOR_POW2_X(1llu) == 1, "");
static_assert(FLOOR_POW2_X(10llu) == 8, "");
static_assert(FLOOR_POW2_X(31llu) == 16, "");
static_assert(FLOOR_POW2_X(32llu) == 32, "");
static_assert(FLOOR_POW2_X(33llu) == 32, "");
static_assert(FLOOR_POW2_X(64llu) == 64, "");
static_assert(FLOOR_POW2_X(100llu) == 64, "");
static_assert(FLOOR_POW2_X(128llu) == 128, "");
static_assert(FLOOR_POW2_X(200llu) == 128, "");
static_assert(FLOOR_POW2_X(256llu) == 256, "");
static_assert(FLOOR_POW2_X(300llu) == 256, "");
static_assert(FLOOR_POW2_X(400llu) == 256, "");
static_assert(FLOOR_POW2_X(500llu) == 256, "");
static_assert(FLOOR_POW2_X(512llu) == 512, "");
static_assert(FLOOR_POW2_X(600llu) == 512, "");


static_assert(IS_POW2(PAGE_SIZE),       "PAGE_SIZE is not a power-of-two");
static_assert(PAGE_SIZE >= sizeof(u64), "PAGE_SIZE too small");
static_assert(PAGE_SIZE <= 65536,       "PAGE_SIZE too large");


char abuf_zeroc = 0;


u32 _rsm_floor_pow2_32(u32 x) {
  x += !x;
  if ( x == ~0u )
    return ~0u;
  if ((x) > ((x) << 1))
    return (~0u >> 1) + 1;
  return 1u << (rsm_fls(x) - 1);
}

u64 _rsm_floor_pow2_64(u64 x) {
  x += !x;
  if ( x == ~0llu )
    return ~0llu;
  if ((x) > ((x) << 1))
    return (~0llu >> 1) + 1;
  return 1llu << (rsm_fls(x) - 1);
}


const char* rerr_str(rerr_t e) {
  switch ((enum rerr_)e) {
  case rerr_ok:            return "(no error)";
  case rerr_invalid:       return "invalid data or argument";
  case rerr_sys_op:        return "invalid syscall op or syscall op data";
  case rerr_badfd:         return "invalid file descriptor";
  case rerr_bad_name:      return "invalid or misformed name";
  case rerr_not_found:     return "resource not found";
  case rerr_name_too_long: return "name too long";
  case rerr_canceled:      return "operation canceled";
  case rerr_not_supported: return "not supported";
  case rerr_exists:        return "already exists";
  case rerr_end:           return "end of resource";
  case rerr_access:        return "permission denied";
  case rerr_nomem:         return "cannot allocate memory";
  case rerr_mfault:        return "bad memory address";
  case rerr_overflow:      return "value too large";
  }
  return "(unknown error)";
}

rerr_t rerr_errno(int e) {
  switch (e) {
    case 0: return 0;
  #ifndef RSM_NO_LIBC
    case EACCES: return rerr_access;
    case EEXIST: return rerr_exists;
    case ENOENT: return rerr_not_found;
    case EBADF:  return rerr_badfd;
  #endif
    default: return rerr_invalid;
  }
}


const char* rop_name(rop_t op) {
  switch (op) {
    #define _(name, enc, res, asmname, ...) case rop_##name: return asmname;
    RSM_FOREACH_OP(_)
    #undef _
  }
  return "?";
}


rerr_t mmapfile(const char* filename, rmem_t* data_out) {
  #ifdef RSM_NO_LIBC
    return rerr_not_supported;
  #else
    int fd = open(filename, O_RDONLY);
    if (fd < 0)
      return rerr_errno(errno);

    struct stat st;
    if (fstat(fd, &st) != 0) {
      rerr_t err = rerr_errno(errno);
      close(fd);
      return err;
    }

    void* p = mmap(0, (usize)st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    if (p == MAP_FAILED)
      return rerr_nomem;

    data_out->p = p;
    data_out->size = (usize)st.st_size;
    return 0;
  #endif
}

void unmapfile(rmem_t m) {
  #ifndef RSM_NO_LIBC
    munmap(m.p, m.size);
  #endif
}

rerr_t read_stdin_data(rmemalloc_t* ma, usize maxlen, rmem_t* data_out) {
  #ifdef RSM_NO_LIBC
    return rerr_not_supported;
  #else
    if (isatty(0))
      return rerr_badfd;
    usize len = 0;
    rmem_t dst = rmem_alloc(ma, PAGE_SIZE);
    if (!dst.p)
      return rerr_nomem;
    for (;;) {
      isize n = read(0, dst.p + len, dst.size - len);
      if (n < 0) {
        rmem_free(ma, dst);
        return rerr_errno(errno);
      }
      len += (usize)n;
      if ((usize)n < dst.size - len)
        break;
      if (!rmem_resize(ma, &dst, dst.size * 2)) {
        rmem_free(ma, dst);
        return rerr_nomem;
      }
    }
    *data_out = dst;
    return 0;
  #endif
}

rerr_t writefile(const char* filename, u32 mode, const void* data, usize size) {
  #ifdef RSM_NO_LIBC
    return rerr_not_supported;
  #else
    assert(size <= ISIZE_MAX);
    int fd = open(filename, O_WRONLY|O_CREAT|O_TRUNC, 0777);
    if (fd < 0)
      return rerr_errno(errno);
    rerr_t err = 0;
    while (size) {
      isize n = write(fd, data, size);
      if (n < (isize)size) {
        err = n < 0 ? rerr_errno(errno) : rerr_canceled;
        break;
      }
      data += n;
      size -= (usize)n;
    }
    close(fd);
    return err;
  #endif
}

static char* strrevn(char* s, usize len) {
  for (usize i = 0, j = len - 1; i < j; i++, j--) {
    char tmp = s[i];
    s[i] = s[j];
    s[j] = tmp;
  }
  return s;
}

usize stru64(char buf[64], u64 v, u32 base) {
  static const char* chars =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  base = MAX(2, MIN(base, 62));
  char* p = buf;
  do {
    *p++ = chars[v % base];
    v /= base;
  } while (v);
  usize len = (usize)(uintptr)(p - buf);
  p--;
  strrevn(buf, len);
  return len;
}

rerr_t parseu64(const char* src, usize srclen, int base, u64* result, u64 cutoff) {
  assert(base >= 2 && base <= 36);
  const char* s = src;
  const char* end = src + srclen;
  u64 acc = 0;
  u64 cutlim = cutoff % base;
  cutoff /= base;
  int any = 0;
  for (char c = *s; s != end; c = *++s) {
    if (isdigit(c)) {
      c -= '0';
    } else if (isupper(c)) {
      c -= 'A' - 10;
    } else if (islower(c)) {
      c -= 'a' - 10;
    } else {
      return rerr_invalid;
    }
    if (c >= base)
      return rerr_invalid;
    if (any < 0 || acc > cutoff || (acc == cutoff && (u64)c > cutlim)) {
      any = -1;
    } else {
      any = 1;
      acc *= base;
      acc += c;
    }
  }
  if (any < 0 || // more digits than what fits in acc
      any == 0)
  {
    return rerr_overflow;
  }
  *result = acc;
  return 0;
}

// // logbin is a little debug/development function which logs a number
// // in binary, unsigned decimal and signed decimal.
// void logbin32(u32 v) {
//   char buf[32];
//   usize n = stru64(buf, v, 2);
//   log(
//     "\e[2mbit   3322222222221111111111          \e[22m\n"
//     "\e[2m      10987654321098765432109876543210\e[22m\n"
//     "\e[2mbin   %.*s\e[22m%.*s\n"
//     "\e[2mdec u \e[22m%u (0x%x)\n"
//     "\e[2mdec s \e[22m%d",
//     (int)(32-n), "00000000000000000000000000000000",
//     (int)n, buf,
//     v, v,
//     (i32)v);
// }

// void logbin64(u64 v) {
//   char buf[64];
//   usize n = stru64(buf, v, 2);
//   log(
//     "\e[2mbit   666655555555554444444444333333333322222222221111111111          \e[22m\n"
//     "\e[2m      3210987654321098765432109876543210987654321098765432109876543210\e[22m\n"
//     "\e[2mbin   %.*s\e[22m%.*s\n"
//     "\e[2mdec u \e[22m%llu (0x%llx)\n"
//     "\e[2mdec s \e[22m%lld",
//     (int)(64-n), "0000000000000000000000000000000000000000000000000000000000000000",
//     (int)n, buf,
//     v, v,
//     (i64)v);
// }


noreturn void _panic(const char* file, int line, const char* fun, const char* fmt, ...) {
  #ifdef RSM_NO_LIBC
    log("panic");
  #else
    FILE* fp = stderr;
    flockfile(fp);
    fprintf(fp, "\npanic: ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(fp, fmt, ap);
    va_end(ap);
    fprintf(fp, " in %s at %s:%d\n", fun, file, line);
    void* buf[32];
    int framecount = backtrace(buf, countof(buf));
    if (framecount > 1) {
      char** strs = backtrace_symbols(buf, framecount);
      if (strs != NULL) {
        for (int i = 1; i < framecount; ++i) {
          fwrite(strs[i], strlen(strs[i]), 1, fp);
          fputc('\n', fp);
        }
        free(strs);
      } else {
        fflush(fp);
        backtrace_symbols_fd(buf, framecount, fileno(fp));
      }
    }
    funlockfile(fp);
    fflush(fp);
    fsync(STDERR_FILENO);
  #endif
  abort();
}


usize mem_pagesize() {
  return (usize)MEM_PAGESIZE;
}


void* nullable osvmem_alloc(usize nbytes) {
  #ifndef HAS_MMAP
    return NULL;
  #else
    if (nbytes == 0) {
      dlog("mmap failed: zero size requested");
      return NULL;
    }

    int protection = PROT_READ | PROT_WRITE;
    int flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;

    void* ptr = mmap(0, nbytes, protection, flags, -1, 0);

    if UNLIKELY(ptr == MAP_FAILED | ptr == NULL) {
      dlog("mmap failed with errno %d %s", errno, strerror(errno));
      return NULL;
    }

    return ptr;
  #endif
}


bool osvmem_free(void* ptr, usize nbytes) {
  #ifdef HAS_MMAP
    return munmap(ptr, nbytes) == 0;
  #else
    return false;
  #endif
}



/* --- BEGIN musl code, licensed as followed (MIT) ---
Copyright © 2005-2020 Rich Felker, et al.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. */

#if !HAS_LIBC_BUILTIN(__builtin_memset)
  void* memset(void* dst, int c, usize n) {
    u8* s = dst;
    usize k;

    // Fill head and tail with minimal branching. Each conditional ensures that all
    // the subsequently used offsets are well-defined and in the dst region.

    if (!n) return dst;
    s[0] = c;
    s[n-1] = c;
    if (n <= 2) return dst;
    s[1] = c;
    s[2] = c;
    s[n-2] = c;
    s[n-3] = c;
    if (n <= 6) return dst;
    s[3] = c;
    s[n-4] = c;
    if (n <= 8) return dst;

    // Advance pointer to align it at a 4-byte boundary, and truncate n to a multiple of 4.
    // The previous code already took care of any head/tail that get cut off by the alignment.
    k = -(uintptr)s & 3;
    s += k;
    n -= k;
    n &= -4;

    for (; n; n--, s++)
      *s = c;
    return dst;
  }
#endif

#if !HAS_LIBC_BUILTIN(__builtin_memcpy)
  void* memcpy(void* restrict dst, const void* restrict src, usize n) {
    u8* d = dst;
    const u8* s = src;
    for (; n; n--)
      *d++ = *s++;
    return dst;
  }
#endif

#if !HAS_LIBC_BUILTIN(__builtin_memmove)
void* memmove(void* dest, const void* src, usize n) {
  char *d = dest;
  const char *s = src;
  if (d==s) return d;
  if ((uintptr)s-(uintptr)d-n <= -2*n) return memcpy(d, s, n);
  if (d<s) {
    for (; n; n--) *d++ = *s++;
  } else {
    while (n) n--, d[n] = s[n];
  }
  return dest;
}
#endif

#if !HAS_LIBC_BUILTIN(__builtin_memcmp)
  int memcmp(const void* a, const void* b, usize n) {
    const u8* l = a, *r = b;
    for (; n && *l == *r; n--, l++, r++) {}
    return n ? *l - *r : 0;
  }
#endif

#if !HAS_LIBC_BUILTIN(__builtin_strcmp)
  int strcmp(const char* l, const char* r) {
    for (; *l==*r && *l; l++, r++);
    return *(unsigned char *)l - *(unsigned char *)r;
  }
#endif

#if !HAS_LIBC_BUILTIN(__builtin_strlen)
  usize strlen(const char* s) {
    const char* p = s;
    for (; *s; s++);
    return (usize)(uintptr)(s - p);
  }
#endif


// --- END musl code ---
// --- resume original rsm code ---

// one-time initialization of global state
rerr_t init_time();
rerr_t init_mm();
rerr_t init_rmem();
rerr_t init_vmem();
rerr_t init_asmparse();

bool rsm_init() {
  static bool y = false; if (y) return true; y = true;
  rerr_t err;
  const char* err_what = "?";
  #define CHECK_ERR(expr, what) err = (expr); if (err) { err_what = (what); goto error; }

  // time
  CHECK_ERR(init_time(), "init_time");
  u64 sec, nsec;
  CHECK_ERR(unixtime((i64*)&sec, &nsec), "unixtime");

  // PRNG
  fastrand_seed(nsec);

  // memory manager
  CHECK_ERR(init_mm(), "init_mm");

  // memory allocator
  CHECK_ERR(init_rmem(), "init_rmem");

  // virtual memory manager
  CHECK_ERR(init_vmem(), "init_vmem");

  // assembly parser
  #ifndef RSM_NO_ASM
    CHECK_ERR(init_asmparse(), "init_asmparse");
  #endif

  return true;
error:
  log("rsm_init error: %s (%s)", rerr_str(err), err_what);
  return false;
}

