// string append buffer
// SPDX-License-Identifier: Apache-2.0
#pragma once
RSM_ASSUME_NONNULL_BEGIN

// abuf_t is a string append buffer for implementing snprintf-style functions which
// writes to a limited buffer and separately keeps track of the number of bytes
// that are appended independent of the buffer's limit.
typedef struct abuf {
  char* p;
  char* lastp;
  usize len;
} abuf_t;
// Here is a template for use with functions that uses abuf:
//
// // It writes at most bufcap-1 of the characters to the output buf (the bufcap'th
// // character then gets the terminating '\0'). If the return value is greater than or
// // equal to the bufcap argument, buf was too short and some of the characters were
// // discarded. The output is always null-terminated, unless size is 0.
// // Returns the number of characters that would have been printed if bufcap was
// // unlimited (not including the final `\0').
// usize myprint(char* buf, usize bufcap, int somearg) {
//   abuf_t s = abuf_make(buf, bufcap);
//   // call abuf_append functions here
//   return abuf_terminate(&s);
// }
//
extern char abuf_zeroc;
#define abuf_make(p,size) ({ /* abuf_t abuf_make(char* buf, usize bufcap)  */\
  usize z__ = (usize)(size); char* p__ = (p);                                \
  UNLIKELY(z__ == 0) ?                                                       \
    (abuf_t){ &abuf_zeroc, &abuf_zeroc, 0 } : (abuf_t){ p__, p__+z__-1, 0 }; \
})

// append functions
void abuf_append(abuf_t* s, const char* p, usize len);
void abuf_c(abuf_t* s, char c);
void abuf_u64(abuf_t* s, u64 v, u32 base);
void abuf_fill(abuf_t* s, char c, usize len); // like memset
void abuf_repr(abuf_t* s, const void* p, usize len);
void abuf_reprhex(abuf_t* s, const void* p, usize len);
void abuf_fmt(abuf_t* s, const char* fmt, ...) ATTR_FORMAT(printf, 2, 3);
void abuf_fmtv(abuf_t* s, const char* fmt, va_list);
inline static void abuf_str(abuf_t* s, const char* cstr) { abuf_append(s, cstr, strlen(cstr)); }

inline static usize abuf_terminate(abuf_t* s) { *s->p = 0; return s->len; }
inline static usize abuf_avail(const abuf_t* s) { return (usize)(uintptr)(s->lastp - s->p); }
bool abuf_endswith(const abuf_t* s, const char* str, usize len);

RSM_ASSUME_NONNULL_END
