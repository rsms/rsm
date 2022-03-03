// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

#ifndef RSM_NO_LIBC
  #include <stdio.h>
  #include <fcntl.h>
  #include <errno.h>
  #include <unistd.h>
  #include <stdlib.h> // free
  #include <execinfo.h> // backtrace* (for _panic)
  #include <sys/stat.h>
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
  #endif // _WIN32
#endif // RSM_NO_LIBC
#ifndef MEM_PAGESIZE
  // fallback value (should match wasm32)
  #define MEM_PAGESIZE ((usize)4096U)
#endif


char abuf_zeroc = 0;


const char* rerror_str(rerror e) {
  switch ((enum rerror)e) {
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


#ifndef RSM_NO_LIBC
  static rerror rerror_errno(int e) {
    switch (e) {
      case 0: return 0;
      case EACCES: return rerr_access;
      case EEXIST: return rerr_exists;
      case ENOENT: return rerr_not_found;
      case EBADF:  return rerr_badfd;
      default: return rerr_invalid;
    }
  }
#endif


rerror mmapfile(const char* filename, void** p_put, usize* len_out) {
  #ifdef RSM_NO_LIBC
    return rerr_not_supported;
  #else
    int fd = open(filename, O_RDONLY);
    if (fd < 0)
      return rerror_errno(errno);

    struct stat st;
    if (fstat(fd, &st) != 0) {
      rerror err = rerror_errno(errno);
      close(fd);
      return err;
    }
    *len_out = (usize)st.st_size;

    void* p = mmap(0, (usize)st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    if (p == MAP_FAILED)
      return rerr_nomem;

    *p_put = p;
    return 0;
  #endif
}

void unmapfile(void* p, usize len) {
  #ifndef RSM_NO_LIBC
    munmap(p, len);
  #endif
}

rerror read_stdin_data(rmem m, usize maxlen, void** p_put, usize* len_out) {
  *len_out = 0;
  #ifdef RSM_NO_LIBC
    return rerr_not_supported;
  #else
    if (isatty(0))
      return rerr_badfd;
    usize cap = 4096;
    usize len = 0;
    void* dst = rmem_alloc(m, cap);
    for (;;) {
      if (!dst)
        return rerr_nomem;
      isize n = read(0, dst + len, cap - len);
      if (n < 0)
        return rerror_errno(errno);
      len += (usize)n;
      if ((usize)n < cap - len)
        break;
      dst = rmem_resize(m, dst, cap, cap*2);
      cap *= 2;
    }
    *p_put = dst;
    *len_out = len;
    return 0;
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

rerror parseu64(const char* src, usize srclen, int base, u64* result, u64 cutoff) {
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

// logbin is a little debug/development function which logs a number
// in binary, unsigned decimal and signed decimal.
void logbin(u32 v) {
  char buf[32];
  usize n = stru64(buf, v, 2);
  log("\e[2mbit   3322222222221111111111          \e[22m\n"
      "\e[2m      10987654321098765432109876543210\e[22m\n"
      "\e[2mbin   %.*s\e[22m%.*s\n"
      "\e[2mdec u \e[22m%u (0x%x)\n"
      "\e[2mdec s \e[22m%d",
      (int)(32-n), "00000000000000000000000000000000",
      (int)n, buf,
      v, v,
      (int)v);
}

// --- abuf functions ---

void abuf_c(abuf* s, char c) {
  *s->p = c;
  s->p = MIN(s->p + 1, s->lastp);
  s->len++;
}

void abuf_append(abuf* s, const char* p, usize len) {
  usize z = MIN(len, abuf_avail(s));
  memcpy(s->p, p, z);
  s->p += z;
  if (check_add_overflow(s->len, len, &s->len))
    s->len = USIZE_MAX;
}


void abuf_u64(abuf* s, u64 v, u32 base) {
  char buf[64];
  usize len = stru64(buf, v, base);
  return abuf_append(s, buf, len);
}


void abuf_fill(abuf* s, char c, usize len) {
  if (check_add_overflow(s->len, len, &s->len))
    s->len = USIZE_MAX;
  usize avail = abuf_avail(s);
  if (avail < len)
    len = avail;
  memset(s->p, c, len);
  s->p += len;
}


void abuf_repr(abuf* s, const char* srcp, usize len) {
  static const char* hexchars = "0123456789abcdef";

  char* p = s->p;
  char* lastp = s->lastp;
  usize nwrite = 0;

  for (usize i = 0; i < len; i++) {
    u8 c = (u8)*srcp++;
    switch (c) {
      // \xHH
      case '\1'...'\x08':
      case 0x0E ... 0x1F:
      case 0x7f ... 0xFF:
        if (LIKELY( p + 3 < lastp )) {
          p[0] = '\\';
          p[1] = 'x';
          if (c < 0x10) {
            p[2] = '0';
            p[3] = hexchars[(int)c];
          } else {
            p[2] = hexchars[(int)c >> 4];
            p[3] = hexchars[(int)c & 0xf];
          }
          p += 4;
        } else {
          p = lastp;
        }
        nwrite += 4;
        break;
      // \c
      case '\t'...'\x0D':
      case '\\':
      case '"':
      case '\0': {
        static const char t[] = {'t','n','v','f','r'};
        if (LIKELY( p + 1 < lastp )) {
          p[0] = '\\';
          if      (c == 0)                         p[1] = '0';
          else if (((usize)c - '\t') <= sizeof(t)) p[1] = t[c - '\t'];
          else                                     p[1] = c;
          p += 2;
        } else {
          p = lastp;
        }
        nwrite++;
        break;
      }
      // verbatim
      default:
        *p = c;
        p = MIN(p + 1, lastp);
        nwrite++;
        break;
    }
  }

  if (check_add_overflow(s->len, nwrite, &s->len))
    s->len = USIZE_MAX;
  s->p = p;
}


// void abuf_f64(abuf* s, f64 v, int ndec) {
//   #ifdef RSM_NO_LIBC
//     #warning TODO implement abuf_f64 for non-libc
//     assert(!"not implemented");
//     // TODO: consider using fmt_fp (stdio/vfprintf.c) in musl
//   #else
//     usize cap = abuf_avail(s);
//     int n;
//     if (ndec > -1) {
//       n = snprintf(s->p, cap+1, "%.*f", ndec, v);
//     } else {
//       n = snprintf(s->p, cap+1, "%f", v);
//     }
//     if (UNLIKELY( n <= 0 ))
//       return;
//     if (ndec < 0) {
//       // trim trailing zeros
//       char* p = &s->p[MIN((usize)n, cap) - 1];
//       while (*p == '0') {
//         p--;
//       }
//       if (*p == '.')
//         p++; // avoid "1.00" becoming "1." (instead, let it be "1.0")
//       n = (int)(uintptr)(p - s->p) + 1;
//       s->p[MIN((usize)n, cap)] = 0;
//     }
//     s->p += MIN((usize)n, cap);
//     s->len += n;
//   #endif
// }


void abuf_fmtv(abuf* s, const char* fmt, va_list ap) {
  #if defined(RSM_NO_LIBC) && !defined(__wasm__)
    dlog("abuf_fmtv not implemented");
    int n = vsnprintf(tmpbuf, sizeof(tmpbuf), format, ap);
  #else
    int n = vsnprintf(s->p, abuf_avail(s), fmt, ap);
    s->len += (usize)n;
    s->p = MIN(s->p + n, s->lastp);
  #endif
}


void abuf_fmt(abuf* s, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  abuf_fmtv(s, fmt, ap);
  va_end(ap);
}


bool abuf_endswith(const abuf* s, const char* str, usize len) {
  return s->len >= len && memcmp(s->p - len, str, len) == 0;
}


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
  return MEM_PAGESIZE;
}


void* nullable vmem_alloc(usize nbytes) {
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

    // note: VM_FLAGS_PURGABLE implies a 2GB allocation limit on macos 10
    // #if defined(__MACH__) && defined(__APPLE__) && defined(VM_FLAGS_PURGABLE)
    //   int fd = VM_FLAGS_PURGABLE; // Create a purgable VM object for new VM region
    // #else
    int fd = -1;

    void* ptr = mmap(0, nbytes, mmapprot, mmapflags, fd, 0);
    if UNLIKELY(ptr == MAP_FAILED)
      return NULL;

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


bool vmem_free(void* ptr, usize nbytes) {
  #ifdef HAS_MMAP
    return munmap(ptr, nbytes) == 0;
  #else
    return false;
  #endif
}


void _rarray_remove(rarray* a, u32 elemsize, u32 start, u32 len) {
  assert(a->len >= start+len);
  if (len == 0)
    return;
  void* dst = a->v + elemsize*start;
  void* src = dst + elemsize*len;
  // 0 1 2 3 4 5 6 7
  //       |  |
  // start 3
  // len 2
  //
  memmove(dst, src, elemsize*(a->len - start - len));
  a->len -= len;
}

bool rarray_grow(rarray* a, rmem m, usize elemsize, u32 addl) {
  u32 newcap = a->cap ? (u32)MIN((u64)a->cap * 2, U32_MAX) : MAX(addl, 4);
  usize newsize;
  if (check_mul_overflow((usize)newcap, elemsize, &newsize))
    return false;
  void* p2 = rmem_resize(m, a->v, a->cap*elemsize, newsize);
  if UNLIKELY(!p2)
    return false;
  a->v = p2;
  a->cap = newcap;
  return true;
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


// --- END musl code (resume original rsm code) ---
