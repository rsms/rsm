// Internal functionality.
// This file is included by every implementation file.
// SPDX-License-Identifier: Apache-2.0
#pragma once
#ifdef RSM_NO_INT_DEFS
  #undef RSM_NO_INT_DEFS
#endif
#include "rsm.h"

typedef signed char         i8;
typedef unsigned char       u8;
typedef signed short        i16;
typedef unsigned short      u16;
typedef signed int          i32;
typedef unsigned int        u32;
typedef signed long long    i64;
typedef unsigned long long  u64;
typedef float               f32;
typedef double              f64;
typedef unsigned int        uint;
typedef signed long         isize;
typedef unsigned long       usize;
#ifdef __INTPTR_TYPE__
  typedef __INTPTR_TYPE__   intptr;
  typedef __UINTPTR_TYPE__  uintptr;
#else
  typedef signed long       intptr;
  typedef unsigned long     uintptr;
#endif

#define I8_MAX    0x7f
#define I16_MAX   0x7fff
#define I32_MAX   0x7fffffff
#define I64_MAX   0x7fffffffffffffff
#define ISIZE_MAX __LONG_MAX__

#define I8_MIN    (-0x80)
#define I16_MIN   (-0x8000)
#define I32_MIN   (-0x80000000)
#define I64_MIN   (-0x8000000000000000)
#define ISIZE_MIN (-__LONG_MAX__ -1L)

#define U8_MAX    0xff
#define U16_MAX   0xffff
#define U32_MAX   0xffffffff
#define U64_MAX   0xffffffffffffffff
#define USIZE_MAX (__LONG_MAX__ *2UL+1UL)

#ifdef __INTPTR_MAX__
  #define INTPTR_MIN  (-__INTPTR_MAX__-1L)
  #define INTPTR_MAX  __INTPTR_MAX__
  #define UINTPTR_MAX __UINTPTR_MAX__
#else
  #define INTPTR_MIN  ISIZE_MIN
  #define INTPTR_MAX  ISIZE_MAX
  #define UINTPTR_MAX USIZE_MAX
#endif

#ifdef __cplusplus
  #define NORETURN noreturn
#else
  #define NORETURN      _Noreturn
  #define auto          __auto_type
  #define static_assert _Static_assert
#endif

#ifndef __has_extension
  #define __has_extension   __has_feature
#endif
#ifndef __has_include
  #define __has_include(x)  0
#endif
#ifndef __has_builtin
  #define __has_builtin(x)  0
#endif

#if __has_attribute(fallthrough)
  #define FALLTHROUGH __attribute__((fallthrough))
#else
  #define FALLTHROUGH
#endif
#if __has_attribute(musttail) && !defined(__wasm__)
  // Note on "!defined(__wasm__)": clang 13 claims to have this attribute for wasm
  // targets but it's actually not implemented and causes an error.
  #define MUSTTAIL __attribute__((musttail))
#else
  #define MUSTTAIL
#endif
#if __has_attribute(unused)
  #define ATTR_UNUSED __attribute__((unused))
#else
  #define ATTR_UNUSED
#endif

// ATTR_FORMAT(archetype, string-index, first-to-check)
// archetype determines how the format string is interpreted, and should be printf, scanf,
// strftime or strfmon.
// string-index specifies which argument is the format string argument (starting from 1),
// while first-to-check is the number of the first argument to check against the
// format string. For functions where the arguments are not available to be checked
// (such as vprintf), specify the third parameter as zero.
#if __has_attribute(format)
  #define ATTR_FORMAT(...) __attribute__((format(__VA_ARGS__)))
#else
  #define ATTR_FORMAT(...)
#endif

// UNLIKELY(integralexpr)->bool
#if __has_builtin(__builtin_expect)
  #define LIKELY(x)   (__builtin_expect((bool)(x), true))
  #define UNLIKELY(x) (__builtin_expect((bool)(x), false))
#else
  #define LIKELY(x)   (x)
  #define UNLIKELY(x) (x)
#endif

// _Noreturn abort()
#ifndef RSM_NO_LIBC
  void abort(void); // stdlib.h
#elif __has_builtin(__builtin_trap)
  #define abort __builtin_trap
#elif __has_builtin(__builtin_unreachable)
  #define abort __builtin_unreachable()
#else
  #error no abort()
#endif

#if __has_builtin(__builtin_unreachable)
  #define UNREACHABLE __builtin_unreachable()
#elif __has_builtin(__builtin_trap)
  #define UNREACHABLE __builtin_trap
#else
  #define UNREACHABLE abort()
#endif

#if defined(__clang__) || defined(__gcc__)
  #define _DIAGNOSTIC_IGNORE_PUSH(x)  _Pragma("GCC diagnostic push") _Pragma(#x)
  #define DIAGNOSTIC_IGNORE_PUSH(STR) _DIAGNOSTIC_IGNORE_PUSH(GCC diagnostic ignored STR)
  #define DIAGNOSTIC_IGNORE_POP()     _Pragma("GCC diagnostic pop")
#else
  #define DIAGNOSTIC_IGNORE_PUSH(STR)
  #define DIAGNOSTIC_IGNORE_POP()
#endif

#ifndef countof
  #define countof(x) \
    ((sizeof(x)/sizeof(0[x])) / ((usize)(!(sizeof(x) % sizeof(0[x])))))
#endif

#ifndef offsetof
  #if __has_builtin(__builtin_offsetof)
    #define offsetof __builtin_offsetof
  #else
    #define offsetof(st, m) ((usize)&(((st*)0)->m))
  #endif
#endif

#define MAX(a,b) ({__typeof__ (a) _a = (a); __typeof__ (b) _b = (b); _a > _b ? _a : _b; })
  // turns into CMP + CMOV{L,G} on x86_64
  // turns into CMP + CSEL on arm64

#define MIN(a,b) ({__typeof__ (a) _a = (a); __typeof__ (b) _b = (b); _a < _b ? _a : _b; })
  // turns into CMP + CMOV{L,G} on x86_64
  // turns into CMP + CSEL on arm64

// T ALIGN2<T>(T x, anyuint a)       rounds up x to nearest a (a must be a power of two)
// T ALIGN2_FLOOR<T>(T x, anyuint a) rounds down x to nearest a
// bool IS_ALIGN2(T x, anyuint a)    true if x is aligned to a
#define ALIGN2(x,a)           _ALIGN2_MASK(x, (__typeof__(x))(a) - 1)
#define ALIGN2_FLOOR(x, a)    ALIGN2((x) - ((a) - 1), (a))
#define IS_ALIGN2(x, a)       (((x) & ((__typeof__(x))(a) - 1)) == 0)
#define _ALIGN2_MASK(x, mask) (((x) + (mask)) & ~(mask))

// rsm_ctz returns the number of trailing 0-bits in x,
// starting at the least significant bit position. If x is 0, the result is undefined.
#define rsm_ctz(x) _Generic((x), \
  u32:   __builtin_ctz,       \
  usize: __builtin_ctzl,      \
  u64:   __builtin_ctzll)(x)

// __fls finds the last (most-significant) bit set
#define __fls(x) (x ? sizeof(x) * 8 - __builtin_clz(x) : 0)

// ILOG2 calculates the log of base 2
#define ILOG2(n) ( __builtin_constant_p(n) ? ((n) < 2 ? 0 : 63 - __builtin_clzll(n)) \
                                           : __fls(n) )

// CEIL_POW2 rounds up n to nearest power of two. Result is undefined when n is 0.
#define CEIL_POW2(n) ( \
  __builtin_constant_p(n) ? ( ((n) == 1) ? 1 : (1UL << (ILOG2((n) - 1) + 1)) ) \
                          : (1UL << __fls(n - 1)) )

static inline RSM_WARN_UNUSED_RESULT bool __must_check_unlikely(bool unlikely) {
  return UNLIKELY(unlikely);
}

#define check_add_overflow(a, b, dst) __must_check_unlikely(({  \
  __typeof__(a) a__ = (a);                 \
  __typeof__(b) b__ = (b);                 \
  __typeof__(dst) dst__ = (dst);           \
  (void) (&a__ == &b__);                   \
  (void) (&a__ == dst__);                  \
  __builtin_add_overflow(a__, b__, dst__); \
}))

#define check_mul_overflow(a, b, dst) __must_check_unlikely(({  \
  __typeof__(a) a__ = (a);                 \
  __typeof__(b) b__ = (b);                 \
  __typeof__(dst) dst__ = (dst);           \
  (void) (&a__ == &b__);                   \
  (void) (&a__ == dst__);                  \
  __builtin_mul_overflow(a__, b__, dst__); \
}))

typedef i32 rerror;
enum rerror {
  rerr_ok            =   0, // no error
  rerr_invalid       =  -1, // invalid data or argument
  rerr_sys_op        =  -2, // invalid syscall op or syscall op data
  rerr_badfd         =  -3, // invalid file descriptor
  rerr_bad_name      =  -4, // invalid or misformed name
  rerr_not_found     =  -5, // resource not found
  rerr_name_too_long =  -6, // name too long
  rerr_canceled      =  -7, // operation canceled
  rerr_not_supported =  -8, // not supported
  rerr_exists        =  -9, // already exists
  rerr_end           = -10, // end of resource
  rerr_access        = -11, // permission denied
  rerr_nomem         = -12, // cannot allocate memory
  rerr_mfault        = -13, // bad memory address
  rerr_overflow      = -14, // value too large
};

typedef __builtin_va_list va_list;
#ifndef va_start
  #define va_start __builtin_va_start
  #define va_end   __builtin_va_end
  #define va_arg   __builtin_va_arg
  #define va_copy  __builtin_va_copy
#endif

// ======================================================================================
// panic & assert

// panic prints msg to stderr and calls TRAP()
#define panic(fmt, args...) _panic(__FILE__, __LINE__, __FUNCTION__, fmt, ##args)

// void log(const char* fmt, ...)
#ifdef RSM_NO_LIBC
  #ifdef __wasm__
    // imported by wasm module
    // __attribute__((visibility("default"))) void logv(const char* _Nonnull format, va_list);
    void logv(const char* _Nonnull format, va_list);
    ATTR_FORMAT(printf, 1, 2) inline static void log(const char* _Nonnull format, ...) {
      va_list ap;
      va_start(ap, format);
      logv(format, ap);
      va_end(ap);
    }
  #else
    #warning no log() implementation
    #define log(format, ...) ((void)0)
    #define logv(format, ap) ((void)0)
  #endif
#else
  #include <stdio.h>
  #define log(format, args...) ({ fprintf(stderr, format "\n", ##args); ((void)0); })
  #define logv(format, ap)     ({ vfprintf(stderr, format "\n", (ap)); ((void)0); })
#endif

// void assert(expr condition)
#undef assert
#if defined(DEBUG)
  #ifdef NDEBUG
    #warning both DEBUG and NDEBUG defined
  #endif
  #undef DEBUG
  #undef NDEBUG
  #undef RSM_SAFE
  #define DEBUG 1
  #define RSM_SAFE 1

  #define _assertfail(fmt, args...) \
    _panic(__FILE__, __LINE__, __FUNCTION__, "Assertion failed: " fmt, args)
  // Note: we can't use ", ##args" above in either clang nor gcc for some reason,
  // or else certain applications of this macro are not expanded.

  #define assertf(cond, fmt, args...) \
    if (UNLIKELY(!(cond))) _assertfail(fmt " (%s)", ##args, #cond)

  #define assert(cond) \
    if (UNLIKELY(!(cond))) _assertfail("%s", #cond)

  #define assertcstreq(cstr1, cstr2) ({                  \
    const char* cstr1__ = (cstr1);                       \
    const char* cstr2__ = (cstr2);                       \
    if (UNLIKELY(strcmp(cstr1__, cstr2__) != 0))         \
      _assertfail("\"%s\" != \"%s\"", cstr1__, cstr2__); \
  })

  #define assertnull(a)  assert((a) == NULL)
  #define assertnotnull(a) ({                                              \
    __typeof__(a) val__ = (a);                                             \
    ATTR_UNUSED const void* valp__ = val__; /* build bug on non-pointer */ \
    if (UNLIKELY(val__ == NULL))                                           \
      _assertfail("%s != NULL", #a);                                       \
    val__; })

#else /* !defined(NDEBUG) */
  #undef DEBUG
  #undef NDEBUG
  #define NDEBUG 1
  #define assert(cond)            ((void)0)
  #define assertf(cond, fmt, ...) ((void)0)
  #define assertop(a,op,b)        ((void)0)
  #define assertcstreq(a,b)       ((void)0)
  #define asserteq(a,b)           ((void)0)
  #define assertne(a,b)           ((void)0)
  #define assertlt(a,b)           ((void)0)
  #define assertgt(a,b)           ((void)0)
  #define assertnull(a)           ((void)0)
  #define assertnotnull(a)        ({ a; }) /* note: (a) causes "unused" warnings */
#endif /* !defined(NDEBUG) */

// RSM_SAFE -- checks enabled in "debug" and "safe" builds (but not in "fast" builds.)
//
// void safecheck(EXPR)
// EXPR safecheckx(EXPR)
// void safecheckf(EXPR, const char* fmt, ...)
// typeof(EXPR) safenotnull(EXPR)
//
#if defined(RSM_SAFE)
  #undef RSM_SAFE
  #define RSM_SAFE 1
  #define _safefail(fmt, args...) _panic(__FILE__, __LINE__, __FUNCTION__, fmt, ##args)
  #define safecheckf(cond, fmt, args...) if UNLIKELY(!(cond)) _safefail(fmt, ##args)
  #ifdef DEBUG
    #define safecheck(cond) if UNLIKELY(!(cond)) _safefail("safecheck (%s)", #cond)
    #define safecheckexpr(expr, expect) ({                                        \
      __typeof__(expr) val__ = (expr);                                            \
      safecheckf(val__ == expect, "unexpected value (%s != %s)", #expr, #expect); \
      val__; })
    #define safenotnull(a) ({                                                \
      __typeof__(a) val__ = (a);                                             \
      ATTR_UNUSED const void* valp__ = val__; /* build bug on non-pointer */ \
      safecheckf(val__ != NULL, "unexpected NULL (%s)", #a);                 \
      val__; })
  #else
    #define safecheck(cond) if UNLIKELY(!(cond)) _safefail("safecheck")
    #define safecheckexpr(expr, expect) ({ \
      __typeof__(expr) val__ = (expr); safecheck(val__ == expect); val__; })
    #define safenotnull(a) ({                                                \
      __typeof__(a) val__ = (a);                                             \
      ATTR_UNUSED const void* valp__ = val__; /* build bug on non-pointer */ \
      safecheckf(val__ != NULL, "NULL");                                     \
      val__; })
  #endif
#else
  #define safecheckf(cond, fmt, args...) ((void)0)
  #define safecheck(cond)                ((void)0)
  #define safecheckexpr(expr, expect)    (expr) /* intentionally complain if not used */
  #define safenotnull(a)                 ({ a; }) /* note: (a) causes "unused" warnings */
#endif

// void dlog(const char* fmt, ...)
#ifdef DEBUG
  #ifdef RSM_NO_LIBC
    #define dlog(format, args...) \
      log("[D] " format " (%s:%d)", ##args, __FILE__, __LINE__)
  #else
    #include <unistd.h> // isatty
    #define dlog(format, args...) ({                                 \
      if (isatty(2)) log("\e[1;35m▍\e[0m" format " \e[2m%s:%d\e[0m", \
                         ##args, __FILE__, __LINE__);                \
      else           log("[D] " format " (%s:%d)",                   \
                         ##args, __FILE__, __LINE__);                \
      fflush(stderr); })
  #endif
#else
  #define dlog(format, ...) ((void)0)
#endif

// --------------------------------------------------------------------------------------
RSM_ASSUME_NONNULL_BEGIN

// minimal set of libc functions
#define HAS_LIBC_BUILTIN(f) (__has_builtin(f) && (!defined(__wasm__) || defined(__wasi__)))

#if HAS_LIBC_BUILTIN(__builtin_memset)
  #define memset __builtin_memset
#else
  void* memset(void* p, int c, usize n);
#endif

#if HAS_LIBC_BUILTIN(__builtin_memcpy)
  #define memcpy __builtin_memcpy
#else
  void* memcpy(void* restrict dst, const void* restrict src, usize n);
#endif

#if HAS_LIBC_BUILTIN(__builtin_memmove)
  #define memmove __builtin_memmove
#else
  void* memmove(void* dest, const void* src, usize n);
#endif

#if HAS_LIBC_BUILTIN(__builtin_memcmp)
  #define memcmp __builtin_memcmp
#else
  int memcmp(const void* l, const void* r, usize n);
#endif

#if HAS_LIBC_BUILTIN(__builtin_strlen)
  #define strlen __builtin_strlen
#else
  usize strlen(const char* s);
#endif

#if HAS_LIBC_BUILTIN(__builtin_strcmp)
  #define strcmp __builtin_strcmp
#else
  int strcmp(const char* l, const char* r);
#endif

#if defined(__wasm__) && !defined(__wasi__)
  int vsnprintf(char *restrict s, usize n, const char *restrict fmt, va_list ap);
  int snprintf(char* restrict s, usize n, const char* restrict fmt, ...);
#endif // printf

// rsm_qsort is qsort_r aka qsort_s
void rsm_qsort(void* base, usize nmemb, usize size,
  int(*cmp)(const void* x, const void* y, void* ctx), void* ctx);

// --------------------------------------------------------------------------------------
// internal utility functions, like a string buffer. Not namespaced. See util.c

#define UTF8_SELF 0x80 // UTF-8 "self" byte constant

// character classifiers
#define isdigit(c)    ( ((u32)(c) - '0') < 10 )                 /* 0-9 */
#define isalpha(c)    ( ((u32)(c) | 32) - 'a' < 26 )            /* A-Za-z */
#define isalnum(c)    ( isdigit(c) || isalpha(c) )              /* 0-9A-Za-z */
#define isupper(c)    ( ((u32)(c) - 'A') < 26 )                 /* A-Z */
#define islower(c)    ( ((u32)(c) - 'a') < 26 )                 /* a-z */
#define isprint(c)    ( ((u32)(c) - 0x20) < 0x5f )              /* SP-~ */
#define isgraph(c)    ( ((u32)(c) - 0x21) < 0x5e )              /* !-~ */
#define isspace(c)    ( (c) == ' ' || (u32)(c) - '\t' < 5 )     /* SP, \{tnvfr} */
#define ishexdigit(c) ( isdigit(c) || ((u32)c | 32) - 'a' < 6 ) /* 0-9A-Fa-f */

#define tolower(c) ((c) | 0x20)

usize stru64(char buf[64], u64 v, u32 base);
rerror parseu64(const char* src, usize srclen, int base, u64* result, u64 cutoff);

void logbin(u32 v);

rerror mmapfile(const char* filename, void** p_put, usize* len_out);
void unmapfile(void* p, usize len);
rerror read_stdin_data(rmem, usize maxlen, void** p_put, usize* len_out);

const char* rerror_str(rerror);

NORETURN void _panic(const char* file, int line, const char* fun, const char* fmt, ...)
  ATTR_FORMAT(printf, 4, 5);

usize mem_pagesize();
void* nullable vmem_alloc(usize nbytes);
bool vmem_free(void* ptr, usize nbytes);


// rarray is a dynamic typed array
typedef struct rarray rarray;
struct rarray {
  u8* nullable v; // u8 so we get -Wincompatible-pointer-types if we access .v directly
  u32 len, cap;
};
#define rarray_at(TYPE, a, index)           ( ((TYPE*)(a)->v) + (index) )
#define rarray_push(TYPE, a, m)             ((TYPE*)rarray_zpush(sizeof(TYPE),(a),(m)))
#define rarray_remove(TYPE, a, start, len)  rarray_zremove(sizeof(TYPE),(a),(start),(len))
#define rarray_free(TYPE, a, m)             rarray_zfree(sizeof(TYPE),(a),(m))

#define rarray_zpush(esize, a, m)             _rarray_push((a), (m), esize)
#define rarray_zremove(esize, a, start, len)  _rarray_remove((a), esize, (start), (len))
#define rarray_zfree(esize, a, m)  if ((a)->v) rmem_free((m),(a)->v,(usize)(a)->cap*esize)

bool rarray_grow(rarray* a, rmem, usize elemsize, u32 addl);
void _rarray_remove(rarray* a, u32 elemsize, u32 start, u32 len);
inline static void* nullable _rarray_push(rarray* a, rmem m, u32 elemsize) {
  if (UNLIKELY(a->len == a->cap) && !rarray_grow(a, m, (usize)elemsize, 1))
    return NULL;
  return a->v + elemsize*(a->len++);
}

// fastrand updates the PRNG and returns the next "random" number
u32 fastrand();
void fastrand_seed(u64 seed); // (re)sets the seed of fastrand

// hashcode is the storage type for hash functions
#if defined(__wasm__)
  typedef u64 hashcode;
  #define HASHCODE_MAX U64_MAX
#else
  typedef usize hashcode;
  #define HASHCODE_MAX USIZE_MAX
#endif

// hash computes a hash code for data p of size bytes length
static hashcode hash(const void* p, usize size, hashcode seed);
hashcode hash_2(const void* p, hashcode seed); // 2 bytes (eg. i16, u16)
hashcode hash_4(const void* p, hashcode seed); // 4 bytes (eg. i32, u32)
hashcode hash_8(const void* p, hashcode seed); // 8 bytes (eg. i64, u64)
hashcode hash_f32(const f32* p, hashcode seed); // f32, supports ±0 and NaN
hashcode hash_f64(const f64* p, hashcode seed); // f64, supports ±0 and NaN
hashcode hash_mem(const void* p, usize size, hashcode seed); // size bytes at p
inline static hashcode hash(const void* p, usize size, hashcode seed) {
  switch (size) {
    case 2:  return hash_2(p, seed);
    case 4:  return hash_4(p, seed);
    case 8:  return hash_8(p, seed);
    default: return hash_mem(p, size, seed);
  }
}

// uintptr hash_ptr(const void* p, uintptr seed)
// Must be a macro rather than inline function so that we can take its address.
#if UINTPTR_MAX >= 0xFFFFFFFFFFFFFFFFu
  #define hash_ptr hash_8
#else
  #define hash_ptr hash_4
#endif

// smap is a byte string to pointer map, implemented as a hash map
typedef struct smap    smap;    // string-keyed map
typedef struct smapent smapent; // smap entry
typedef u8             maplf;   // load factor
struct smapent {
  const char* nullable key; // NULL if this entry is empty
  usize                keylen;
  uintptr              value;
};
struct smap {
  u32      cap;  // capacity of entries
  u32      len;  // number of items currently stored in the map (count)
  u32      gcap; // growth watermark cap
  maplf    lf;   // growth watermark load factor (shift value; 1|2|3|4)
  hashcode hash0; // hash seed
  smapent* entries;
  rmem     mem;
};
enum maplf {
  MAPLF_1 = 1, // grow when 50% full; recommended for maps w/ balanced hit & miss lookups
  MAPLF_2 = 2, // grow when 75% full; recommended for maps of mostly hit lookups
  MAPLF_3 = 3, // grow when 88% full; miss (no match) lookups are expensive
  MAPLF_4 = 4, // grow when 94% full; miss lookups are very expensive
} RSM_END_ENUM(maplf)

// smap_make initializes a new map m.
// hint can be 0 and provides a hint as to how many items will initially be stored.
// Returns m on success, NULL on memory allocation failure or overflow from large hint.
smap* nullable smap_make(smap* m, rmem, u32 hint, maplf);
void smap_dispose(smap* m); // free m->entries. m is invalid; use smap_make to reuse
void smap_clear(smap* m);   // removes all items. m remains valid

// smap_assign assigns to the map, returning the location for its value,
// or NULL if memory allocation during growth failed. May return an existing item's value.
uintptr* nullable smap_assign(smap* m, const char* key, usize keylen);

// smap_lookup retrieves the value for key; NULL if not found.
uintptr* nullable smap_lookup(const smap* m, const char* key, usize keylen);

// smap_del removes an entry for key, returning whether an entry was deleted or not
bool smap_del(smap* m, const char* key, usize keylen);

// smap_itstart and smap_itnext iterates over a map.
// You can change the value of an entry during iteration but must not change the key.
// Any mutation to the map during iteration will invalidate the iterator.
// Example use:
//   for (smapent* e = smap_itstart(m); smap_itnext(m, &e); )
//     log("%.*s => %lx", (int)e->keylen, e->key, e->value);
inline static const smapent* nullable smap_itstart(const smap* m) { return m->entries; }
bool smap_itnext(const smap* m, const smapent** ep);

// smap_optimize tries to improve the key distribution of m by trying different
// hash seeds. Returns the best smap_score or <0.0 if rmem_alloc(mem) failed,
// leaving m with at least as good key distribution as before the call.
// mem is used to allocate temporary space for m's entries;
// space needed is m->entries*sizeof(smapent). Applies smap_compact(m) before returning.
double smap_optimize(smap* m, usize iterations, rmem mem);

// smap_cfmt prints C code for a constant static representation of m
usize smap_cfmt(char* buf, usize bufcap, const smap* m, const char* name);


// abuf is a string append buffer for implementing snprintf-style functions which
// writes to a limited buffer and separately keeps track of the number of bytes
// that are appended independent of the buffer's limit.
typedef struct abuf abuf;
struct abuf {
  char* p;
  char* lastp;
  usize len;
};
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
extern char abuf_zeroc;
#define abuf_make(p,size) ({ /* abuf abuf_make(char* buf, usize bufcap)                   */\
  usize z__ = (usize)(size); char* p__ = (p);                                               \
  UNLIKELY(z__ == 0) ? (abuf){ &abuf_zeroc, &abuf_zeroc, 0 } : (abuf){ p__, p__+z__-1, 0 }; \
})

// append functions
void abuf_append(abuf* s, const char* p, usize len);
void abuf_c(abuf* s, char c);
void abuf_u64(abuf* s, u64 v, u32 base);
void abuf_fill(abuf* s, char c, usize len); // like memset
void abuf_repr(abuf* s, const char* srcp, usize len);
void abuf_fmt(abuf* s, const char* fmt, ...) ATTR_FORMAT(printf, 2, 3);
void abuf_fmtv(abuf* s, const char* fmt, va_list);
inline static void abuf_str(abuf* s, const char* cstr) { abuf_append(s, cstr, strlen(cstr)); }

inline static usize abuf_terminate(abuf* s) { *s->p = 0; return s->len; }
inline static usize abuf_avail(const abuf* s) { return (usize)(uintptr)(s->lastp - s->p); }
bool abuf_endswith(const abuf* s, const char* str, usize len);

// ---------------

RSM_ASSUME_NONNULL_END
