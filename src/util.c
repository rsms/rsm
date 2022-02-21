#include "util.h"

#ifdef R_WITH_LIBC
  #include <stdio.h>
  #include <fcntl.h>
  #include <errno.h>
  #include <unistd.h>
  #include <sys/stat.h>
  #include <sys/mman.h>
#endif


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


rerror mmapfile(const char* filename, void** p_put, usize* len_out) {
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
}

void unmapfile(void* p, usize len) {
  munmap(p, len);
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
  static const char chars[] =
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


void abuf_f64(abuf* s, f64 v, int ndec) {
  #ifndef R_WITH_LIBC
    #warning TODO implement abuf_f64 for non-libc
    assert(!"not implemented");
    // TODO: consider using fmt_fp (stdio/vfprintf.c) in musl
  #else
    usize cap = abuf_avail(s);
    int n;
    if (ndec > -1) {
      n = snprintf(s->p, cap+1, "%.*f", ndec, v);
    } else {
      n = snprintf(s->p, cap+1, "%f", v);
    }
    if (UNLIKELY( n <= 0 ))
      return;
    if (ndec < 0) {
      // trim trailing zeros
      char* p = &s->p[MIN((usize)n, cap) - 1];
      while (*p == '0') {
        p--;
      }
      if (*p == '.')
        p++; // avoid "1.00" becoming "1." (instead, let it be "1.0")
      n = (int)(uintptr)(p - s->p) + 1;
      s->p[MIN((usize)n, cap)] = 0;
    }
    s->p += MIN((usize)n, cap);
    s->len += n;
  #endif
}


void abuf_fmtv(abuf* s, const char* fmt, va_list ap) {
  #ifndef R_WITH_LIBC
    assert(!"not implemented");
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
