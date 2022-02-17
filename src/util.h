// internal utility functions, like a string buffer. Not namespaced.
#pragma once
#include "prelude.h"
R_ASSUME_NONNULL_BEGIN

usize stru64(char buf[64], u64 v, u32 base);

// abuf is a string append buffer for implementing snprintf-style functions which
// writes to a limited buffer and separately keeps track of the number of bytes
// that are appended independent of the buffer's limit.
//
// Here is a template for use with functions that uses abuf:
//
// // It writes at most bufcap-1 of the characters to the output buf (the bufcap'th
// // character then gets the terminating '\0'). If the return value is greater than or
// // equal to the bufcap argument, buf was too short and some of the characters were
// // discarded. The output is always null-terminated, unless size is 0.
// // Returns the number of characters that would have been printed if bufcap was
// // unlimited (not including the final `\0').
// usize myprint(char* buf, usize bufcap, int somearg) {
//   abuf s = abuf_make(buf, bufcap);
//   // call abuf_append functions here
//   return abuf_terminate(&s);
// }
//
typedef struct abuf abuf;
struct abuf {
  char* p;
  char* lastp;
  usize len;
};
#define abuf_make(p,size) ({ /* abuf abuf_make(char* buf, usize bufcap)     */\
  usize z__ = (usize)(size); char* p__ = (p); static char x__;                \
  UNLIKELY(z__ == 0) ? (abuf){ &x__, &x__, 0 } : (abuf){ p__, p__+z__-1, 0 }; \
})

// append functions
void abuf_append(abuf* s, const char* p, usize len);
void abuf_c(abuf* s, char c);
void abuf_u64(abuf* s, u64 v, u32 base);
void abuf_f64(abuf* s, f64 v, int ndec);
void abuf_fill(abuf* s, char c, usize len); // like memset
void abuf_repr(abuf* s, const char* srcp, usize len);
void abuf_fmt(abuf* s, const char* fmt, ...) ATTR_FORMAT(printf, 2, 3);
void abuf_fmtv(abuf* s, const char* fmt, va_list);
inline static void abuf_str(abuf* s, const char* cstr) { abuf_append(s, cstr, strlen(cstr)); }

inline static usize abuf_terminate(abuf* s) { *s->p = 0; return s->len; }
inline static usize abuf_avail(const abuf* s) { return (usize)(uintptr)(s->lastp - s->p); }
bool abuf_endswith(const abuf* s, const char* str, usize len);

// ---------------


R_ASSUME_NONNULL_END
