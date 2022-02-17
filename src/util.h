// internal utility functions, like a string buffer. Not namespaced.
#pragma once
#include "prelude.h"
R_ASSUME_NONNULL_BEGIN

usize stru64(char buf[64], u64 v, u32 base);

// sbuf is a string output buffer for implementing snprintf-style functions which
// writes to a limited buffer and separately keeps track of the number of bytes
// that are appended independent of the buffer's limit.
//
// Here is a template for use with functions that uses sbuf:
//
// // It writes at most bufcap-1 of the characters to the output buf (the bufcap'th
// // character then gets the terminating '\0'). If the return value is greater than or
// // equal to the bufcap argument, buf was too short and some of the characters were
// // discarded. The output is always null-terminated, unless size is 0.
// // Returns the number of characters that would have been printed if bufcap was
// // unlimited (not including the final `\0').
// usize myprint(char* buf, usize bufcap, int somearg) {
//   sbuf s = sbuf_make(buf, bufcap);
//   // call sbuf_append functions here
//   return sbuf_terminate(&s);
// }
//
typedef struct sbuf sbuf;
struct sbuf {
  char* p;
  char* lastp;
  usize len;
};
#define sbuf_make(p,size) ({                        \
  usize z__ = (usize)(size);                         \
  char* p__ = (p);                                   \
  static char x__;                                   \
  UNLIKELY(z__ == 0) ? (sbuf){ &x__, &x__, 0 }      \
                     : (sbuf){ p__, p__+z__-1, 0 }; \
})
void sbuf_init(sbuf* s, char* buf, usize bufsize); // bufsize must be >0
usize sbuf_terminate(sbuf* s);
#define sbuf_avail(s) ( (usize)(uintptr)((s)->lastp - (s)->p) )
void sbuf_appendc(sbuf* s, char c);
void sbuf_append(sbuf* s, const char* p, usize len);
void sbuf_appendu64(sbuf* s, u64 v, u32 base);
void sbuf_appendf64(sbuf* s, f64 v, int ndec);
static void sbuf_appendstr(sbuf* s, const char* cstr);
void sbuf_appendfill(sbuf* s, char c, usize len); // like memset
void sbuf_appendrepr(sbuf* s, const char* srcp, usize len);
void sbuf_appendfmt(sbuf* s, const char* fmt, ...) ATTR_FORMAT(printf, 2, 3);
void sbuf_appendfmtv(sbuf* s, const char* fmt, va_list);
bool sbuf_endswith(const sbuf* s, const char* str, usize len);

// ---------------

inline static void sbuf_appendstr(sbuf* s, const char* cstr) {
  sbuf_append(s, cstr, strlen(cstr));
}

R_ASSUME_NONNULL_END
