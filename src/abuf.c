// string append buffer
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "abuf.h"

void abuf_c(abuf_t* s, char c) {
  *s->p = c;
  s->p = MIN(s->p + 1, s->lastp);
  s->len++;
}

void abuf_append(abuf_t* s, const char* p, usize len) {
  usize z = MIN(len, abuf_avail(s));
  memcpy(s->p, p, z);
  s->p += z;
  if (check_add_overflow(s->len, len, &s->len))
    s->len = USIZE_MAX;
}


void abuf_u64(abuf_t* s, u64 v, u32 base) {
  char buf[64];
  usize len = stru64(buf, v, base);
  return abuf_append(s, buf, len);
}


void abuf_fill(abuf_t* s, char c, usize len) {
  if (check_add_overflow(s->len, len, &s->len))
    s->len = USIZE_MAX;
  usize avail = abuf_avail(s);
  if (avail < len)
    len = avail;
  memset(s->p, c, len);
  s->p += len;
}


static const char* hexchars = "0123456789abcdef";

void abuf_repr(abuf_t* s, const void* srcp, usize len) {
  char* p = s->p;
  char* lastp = s->lastp;
  usize nwrite = 0;

  for (usize i = 0; i < len; i++) {
    u8 c = *(u8*)srcp++;
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

void abuf_reprhex(abuf_t* s, const void* srcp, usize len) {
  char* p = s->p;
  char* lastp = s->lastp;
  usize nwrite = 0;
  for (usize i = 0; i < len; i++) {
    u8 c = *(u8*)srcp++;
    if (LIKELY( p + 2 < lastp )) {
      if (i)
        *p++ = ' ';
      if (c < 0x10) {
        p[0] = '0';
        p[1] = hexchars[c];
      } else {
        p[0] = hexchars[c >> 4];
        p[1] = hexchars[c & 0xf];
      }
      p += 2;
    } else {
      p = lastp;
    }
    if (i)
      nwrite++;
    nwrite += 2;
  }
  if (check_add_overflow(s->len, nwrite, &s->len))
    s->len = USIZE_MAX;
  s->p = p;
}


// void abuf_f64(abuf_t* s, f64 v, int ndec) {
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


void abuf_fmtv(abuf_t* s, const char* fmt, va_list ap) {
  #if defined(RSM_NO_LIBC) && !defined(__wasm__)
    dlog("abuf_fmtv not implemented");
    int n = vsnprintf(tmpbuf, sizeof(tmpbuf), format, ap);
  #else
    int n = vsnprintf(s->p, abuf_avail(s), fmt, ap);
    s->len += (usize)n;
    s->p = MIN(s->p + n, s->lastp);
  #endif
}


void abuf_fmt(abuf_t* s, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  abuf_fmtv(s, fmt, ap);
  va_end(ap);
}


bool abuf_endswith(const abuf_t* s, const char* str, usize len) {
  return s->len >= len && memcmp(s->p - len, str, len) == 0;
}
