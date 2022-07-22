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

#ifndef __cplusplus
  #define noreturn      _Noreturn
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
#if __has_attribute(always_inline)
  #define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
  #define ALWAYS_INLINE inline
#endif
#if __has_attribute(noinline)
  #define NOINLINE __attribute__((noinline)) inline
#else
  #define NOINLINE
#endif

#ifdef __wasm__
  #define WASM_EXPORT __attribute__((visibility("default")))
  #define WASM_IMPORT __attribute__((visibility("default")))
#else
  #define WASM_EXPORT
  #define WASM_IMPORT
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

// RSM_LITTLE_ENDIAN=0|1
#ifndef RSM_LITTLE_ENDIAN
  #if defined(__LITTLE_ENDIAN__) || \
      (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
    #define RSM_LITTLE_ENDIAN 1
  #elif defined(__BIG_ENDIAN__) || defined(__ARMEB__) \
        (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    #define RSM_LITTLE_ENDIAN 0
  #else
    #error Can't determine endianness -- please define RSM_LITTLE_ENDIAN=0|1
  #endif
#endif

#if __has_builtin(__builtin_bswap32)
  #define bswap32(x) __builtin_bswap32(x)
#elif defined(_MSC_VER)
  #define bswap32(x) _byteswap_ulong(x)
#else
  static inline u32 bswap32(u32 x) {
    return ((( x & 0xff000000u ) >> 24 )
          | (( x & 0x00ff0000u ) >> 8  )
          | (( x & 0x0000ff00u ) << 8  )
          | (( x & 0x000000ffu ) << 24 ));
  }
#endif

#if __has_builtin(__builtin_bswap64)
  #define bswap64(x) __builtin_bswap64(x)
#elif defined(_MSC_VER)
  #define bswap64(x) _byteswap_uint64(x)
#else
  static inline u64 bswap64(u64 x) {
    u64 hi = bswap32((u32)x);
    u32 lo = bswap32((u32)(x >> 32));
    return (hi << 32) | lo;
  }
#endif

#if RSM_LITTLE_ENDIAN
  #define htole32(n) (n)
  #define htobe32(n) bswap32(n)
  #define htole64(n) (n)
  #define htobe64(n) bswap64(n)
#else
  #define htole32(n) bswap32(n)
  #define htobe32(n) (n)
  #define htole64(n) bswap64(n)
  #define htobe64(n) (n)
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
#define IS_ALIGN2(x, a)       ( ((x) & ((__typeof__(x))(a) - 1)) == 0 )
#define IS_POW2(x)            ( ((x) & ((x) - 1)) == 0 )
#define _ALIGN2_MASK(x, mask) ( ((x) + (mask)) & ~(mask) )

// rsm_ctz returns the number of trailing 0-bits in x,
// starting at the least significant bit position. If x is 0, the result is undefined.
#define rsm_ctz(x) _Generic((x), \
  u32:   __builtin_ctz,       \
  usize: __builtin_ctzl,      \
  u64:   __builtin_ctzll)(x)

// rsm_fls(uint n) finds the last (most-significant) bit set
#define rsm_fls(n) ((sizeof(n) <= 4) ? __fls32(n) : __fls64(n))
static ALWAYS_INLINE int __fls32(unsigned int x) {
  return x ? sizeof(x) * 8 - __builtin_clz(x) : 0;
}
static ALWAYS_INLINE unsigned long __flsl(unsigned long x) {
  return (sizeof(x) * 8) - 1 - __builtin_clzl(x);
}
#if USIZE_MAX < 0xffffffffffffffff
  static ALWAYS_INLINE int __fls64(u64 x) {
    u32 h = x >> 32;
    if (h)
      return __fls32(h) + 32;
    return __fls32(x);
  }
#else
  static ALWAYS_INLINE int __fls64(u64 x) {
    if (x == 0)
      return 0;
    return __flsl(x) + 1;
  }
#endif

// ILOG2 calculates the log of base 2
#define ILOG2(n) ( \
  __builtin_constant_p(n) ? ((n) < 2 ? 0 : 63 - __builtin_clzll(n)) : rsm_fls(n) - 1 )

// CEIL_POW2 rounds up n to nearest power of two. Result is undefined when n is 0.
#define CEIL_POW2(n) ( \
  __builtin_constant_p(n) ? ( ((n) == 1) ? 1 : (1UL << (ILOG2((n) - 1) + 1)) ) \
                          : (1UL << rsm_fls(n - 1)) )

#define RSM_IPOW2(x) ((__typeof__(x))1 << (x))

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

typedef __builtin_va_list va_list;
#ifndef va_start
  #define va_start __builtin_va_start
  #define va_end   __builtin_va_end
  #define va_arg   __builtin_va_arg
  #define va_copy  __builtin_va_copy
#endif

// u32 CAST_U32(anyint z) => [0-U32_MAX]
#define CAST_U32(z) ({ \
  __typeof__(z) z__ = (z); \
  sizeof(u32) < sizeof(z__) ? (u32)MIN((__typeof__(z__))U32_MAX,z__) : (u32)z__; \
})

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
// void safecheck(COND)                        -- elided from non-safe builds
// void safecheckf(COND, const char* fmt, ...) -- elided from non-safe builds
// EXPR safecheckexpr(EXPR, EXPECT)            -- included in non-safe builds (without check)
// typeof(EXPR) safechecknotnull(EXPR)         -- included in non-safe builds (without check)
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
    #define safechecknotnull(a) ({                                           \
      __typeof__(a) val__ = (a);                                             \
      ATTR_UNUSED const void* valp__ = val__; /* build bug on non-pointer */ \
      safecheckf(val__ != NULL, "unexpected NULL (%s)", #a);                 \
      val__; })
  #else
    #define safecheck(cond) if UNLIKELY(!(cond)) _safefail("safecheck")
    #define safecheckexpr(expr, expect) ({ \
      __typeof__(expr) val__ = (expr); safecheck(val__ == expect); val__; })
    #define safechecknotnull(a) ({                                           \
      __typeof__(a) val__ = (a);                                             \
      ATTR_UNUSED const void* valp__ = val__; /* build bug on non-pointer */ \
      safecheckf(val__ != NULL, "NULL");                                     \
      val__; })
  #endif
#else
  #define safecheckf(cond, fmt, args...) ((void)0)
  #define safecheck(cond)                ((void)0)
  #define safecheckexpr(expr, expect)    (expr) /* intentionally complain if not used */
  #define safechecknotnull(a)            ({ a; }) /* note: (a) causes "unused" warnings */
#endif

// void dlog(const char* fmt, ...)
#ifdef DEBUG
  #ifdef RSM_NO_LIBC
    #define dlog(format, args...) \
      log("[D] " format " (%s:%d)", ##args, __FILE__, __LINE__)
  #else
    #include <unistd.h> // isatty
    #define dlog(format, args...) ({                                 \
      if (isatty(2)) log("\e[1;30m▍\e[0m" format " \e[2m%s:%d\e[0m", \
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
typedef int(*rsm_qsort_cmp)(const void* x, const void* y, void* nullable ctx);
void rsm_qsort(void* base, usize nmemb, usize size, rsm_qsort_cmp cmp, void* nullable ctx);

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
rerror writefile(const char* filename, u32 mode, const void* data, usize size);

rerror rerror_errno(int errnoval);

noreturn void _panic(const char* file, int line, const char* fun, const char* fmt, ...)
  ATTR_FORMAT(printf, 4, 5);

usize mem_pagesize();
void* nullable osvmem_alloc(usize nbytes);
bool osvmem_free(void* ptr, usize nbytes);

#ifdef __wasm__
  #define REG_FMTCOLORC(regno)  '1'
  #define REG_FMTNAME_PAT       "R%u"
  #define REG_FMTNAME(regno)    (regno)
  #define REG_FMTVAL_PAT(fmt)   fmt
  #define REG_FMTVAL(regno,val) (val)
#else
  // ANSI colors: (\e[3Nm or \e[9Nm) 1 red, 2 green, 3 yellow, 4 blue, 5 magenta, 6 cyan
  #define REG_FMTCOLORC(regno)  ('1'+((regno)%6))
  #define REG_FMTNAME_PAT       "\e[9%cmR%u\e[39m"
  #define REG_FMTNAME(regno)    REG_FMTCOLORC(regno), (regno)
  #define REG_FMTVAL_PAT(fmt)   "\e[9%cm" fmt "\e[39m"
  #define REG_FMTVAL(regno,val) REG_FMTCOLORC(regno), (val)
#endif

// rarray is a dynamic typed array
typedef struct rarray rarray;
struct rarray {
  u8* nullable v; // u8 so we get -Wincompatible-pointer-types if we access .v directly
  u32 len, cap;
};
#define rarray_at(T, a, index)             (((T*)(a)->v) + (index))
#define rarray_at_safe(T, a, i)            ({safecheck((i)<(a)->len);rarray_at(T,(a),(i));})
#define rarray_push(T, a, m)               ((T*)_rarray_push((a),(m),sizeof(T)))
#define rarray_remove(T, a, start, len)    _rarray_remove((a),sizeof(T),(start),(len))
#define rarray_move(T, a, dst, start, end) _array_move(sizeof(T),(a)->v,(dst),(start),(end))
#define rarray_free(T, a, m)   if ((a)->v)rmem_free((m),(a)->v,(usize)(a)->cap*sizeof(T))
#define rarray_reserve(T, a, m, addl)      _rarray_reserve((a),(m),sizeof(T),(addl))

bool rarray_grow(rarray* a, rmem, usize elemsize, u32 addl);
bool _rarray_reserve(rarray* a, rmem, usize elemsize, u32 addl);
void _rarray_remove(rarray* a, u32 elemsize, u32 start, u32 len);

inline static void* nullable _rarray_push(rarray* a, rmem m, u32 elemsize) {
  if (a->len == a->cap && UNLIKELY(!rarray_grow(a, m, (usize)elemsize, 1)))
    return NULL;
  return a->v + elemsize*(a->len++);
}

// _array_move moves the chunk [src,src+len) to index dst. For example:
//   _array_move(z, v, 5, 1, 1+2) = [1  2 3  4 5|6 7 8] ⟹ [1 4 5  2 3  6 7 8]
//   _array_move(z, v, 1, 4, 4+2) = [1|2 3 4  5 6  7 8] ⟹ [1  5 6  2 3 4 7 8]
#define _array_move(elemsize, v, dst, start, end) (                                 \
  (elemsize) == 4 ? _AMOVE_ROTATE(_arotate32,(dst),(start),(end),(u32* const)(v)) : \
  (elemsize) == 8 ? _AMOVE_ROTATE(_arotate64,(dst),(start),(end),(u64* const)(v)) : \
                    _AMOVE_ROTATE(_arotatemem,(dst),(start),(end),(elemsize),(v)) )
#define _AMOVE_ROTATE(f, dst, start, end, args...) (     \
  ((start)==(dst)||(start)==(end)) ? ((void)0) :         \
  ((start) > (dst)) ? (f)(args, (dst), (start), (end)) : \
  (f)(args, (start), (end), (dst)) )

// arotate rotates the order of v in the range [first,last) in such a way
// that the element pointed to by "mid" becomes the new "first" element.
// Assumes first <= mid < last.
#define arotate(elemsize, v, first, mid, last) (                          \
  (elemsize) == 4 ? _arotate32((u32* const)(v), (first), (mid), (last)) : \
  (elemsize) == 8 ? _arotate64((u64* const)(v), (first), (mid), (last)) : \
  _arotatemem((elemsize), (v), (first), (mid), (last)) )
void _arotatemem(u32 stride, void* v, u32 first, u32 mid, u32 last);
void _arotate32(u32* const v, u32 first, u32 mid, u32 last);
void _arotate64(u64* const v, u32 first, u32 mid, u32 last);

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
void abuf_repr(abuf* s, const void* p, usize len);
void abuf_reprhex(abuf* s, const void* p, usize len);
void abuf_fmt(abuf* s, const char* fmt, ...) ATTR_FORMAT(printf, 2, 3);
void abuf_fmtv(abuf* s, const char* fmt, va_list);
inline static void abuf_str(abuf* s, const char* cstr) { abuf_append(s, cstr, strlen(cstr)); }

inline static usize abuf_terminate(abuf* s) { *s->p = 0; return s->len; }
inline static usize abuf_avail(const abuf* s) { return (usize)(uintptr)(s->lastp - s->p); }
bool abuf_endswith(const abuf* s, const char* str, usize len);

// fmtinstr appends to s a printable representation of in
u32 fmtinstr(abuf* s, rinstr in, rfmtflag fl);

// unixtime stores the number of seconds + nanoseconds since Jan 1 1970 00:00:00 UTC
// at *sec and *nsec
rerror unixtime(i64* sec, u64* nsec);

// nanotime returns nanoseconds measured from an undefined point in time.
// It uses the most high-resolution, low-latency clock available on the system.
// u64 is enough to express 584 years in nanoseconds.
u64 nanotime();

// fmtduration appends human-readable time duration to buf, including a null terminator.
// Returns number of bytes written, excluding the null terminator.
usize fmtduration(char buf[25], u64 duration_ns);

// --------------------------------------------------------------------------------------
// assembler internals, shared by all asm*.c files
#ifndef RSM_NO_ASM
#define kBlock0Name "b0" // name of first block

typedef struct gstate gstate;
typedef struct pstate pstate;

typedef struct rposrange rposrange;
struct rposrange { rsrcpos start, focus, end; };

// rasm._internal[0] -- negated diaghandler return value
#define rasm_stop(a)        ( (bool)(a)->_internal[0] )
#define rasm_stop_set(a,v)  ( *(bool*)&(a)->_internal[0] = (v) )

// rasm._internal[1]-- reusable internal codegen state
#define rasm_gstate(a)        ( (gstate*)(a)->_internal[1] )
#define rasm_gstate_set(a,v)  ( *(gstate**)&(a)->_internal[1] = (v) )

// rasm._internal[2]-- reusable internal codegen state
#define rasm_pstate(a)        ( (pstate*)(a)->_internal[2] )
#define rasm_pstate_set(a,v)  ( *(pstate**)&(a)->_internal[2] = (v) )

const char* tokname(rtok t);

// tokis* returns true if t is classified as such in the language
#define tokistype(t)    ( RT_I1 <= (t) && (t) <= RT_I64 )
#define tokisintlit(t)  ( RT_INTLIT2 <= (t) && (t) <= RT_SINTLIT16 )
#define tokislit(t)     tokisintlit(t)
#define tokissint(t)    (((t) - RT_SINTLIT16) % 2 == 0) // assumption: tokisintlit(t)
#define tokisoperand(t) ( (t) == RT_IREG || (t) == RT_FREG || tokislit(t) || (t) == RT_NAME )
#define tokisexpr(t)    (tokisoperand(t) || (t) == RT_STRLIT)
#define tokhasname(t) ( (t) == RT_NAME || (t) == RT_COMMENT || \
                        (t) == RT_LABEL || (t) == RT_FUN || \
                        (t) == RT_CONST || (t) == RT_DATA )

inline static bool nodename_eq(const rnode* n, const char* str, usize len) {
  return n->sval.len == len && memcmp(n->sval.p, str, len) == 0;
}

rnode* nullable nlastchild(rnode* n);

rposrange nposrange(rnode*);

void errf(rasm*, rposrange, const char* fmt, ...) ATTR_FORMAT(printf, 3, 4);
void warnf(rasm*, rposrange, const char* fmt, ...) ATTR_FORMAT(printf, 3, 4);
void reportv(rasm*, rposrange, int code, const char* fmt, va_list ap);

typedef struct rrombuild rrombuild;
struct rrombuild {
  const rinstr* code;      // vm instructions array
  usize         codelen;   // vm instructions array length
  usize         datasize;  // data segment size
  u8            dataalign; // data segment alignment
  void*         userdata;
  rerror(*filldata)(void* dst, void* userdata);
};

rerror rom_build(rrombuild* rb, rmem mem, rrom* rom);

// ————————————————
// bufslab

#define BUFSLAB_MIN_CAP 512
#define BUFSLAB_ALIGN   16
static_assert(BUFSLAB_MIN_CAP >= BUFSLAB_ALIGN, "");
static_assert(IS_ALIGN2(BUFSLAB_MIN_CAP, BUFSLAB_ALIGN), "");

typedef struct bufslabs bufslabs;
typedef struct bufslab  bufslab;

// The chain of slabs looks like this: ("free" slabs only when recycled)
//
//    full ←—→ full ←—→ partial ←—→ free ←—→ free ←—→ free
//     |                   |
//    head                tail
//
struct bufslabs {
  bufslab* head;
  bufslab* tail;
};
struct bufslab {
  bufslab* nullable prev;
  bufslab* nullable next;
  usize len;
  usize cap;
  u8    data[];
};

void* nullable bufslab_alloc(bufslabs* slabs, rmem mem, usize nbyte);
void bufslabs_reset(bufslabs* slabs); // set all slab->len=0 and set slabs->head=tail
void bufslab_freerest(bufslab* s, rmem mem); // free all slabs after s


#endif // RSM_NO_ASM
// --------------------------------------------------------------------------------------
RSM_ASSUME_NONNULL_END

//———————————————————————————————————————————————————————————————————————————————————————
// OS threads and atomic memory operations
/*
  typedef int (*thrd_start_t)(void*);

  void YIELD_THREAD()
    yields for other threads to be scheduled on the current CPU by the OS
  void YIELD_CPU()
    yields for other work on a CPU core

  int    thrd_create(thrd_t *thr, thrd_start_t func, void *arg);
  void   thrd_exit(int res);
  int    thrd_join(thrd_t thr, int *res);
  int    thrd_detach(thrd_t thr);
  thrd_t thrd_current(void);
  int    thrd_equal(thrd_t a, thrd_t b);
  int    thrd_sleep(const struct timespec *ts_in, struct timespec *rem_out);
  void   thrd_yield(void);

  int    mtx_init(mtx_t *mtx, int type);
  void   mtx_destroy(mtx_t *mtx);
  int    mtx_lock(mtx_t *mtx);
  int    mtx_trylock(mtx_t *mtx);
  int    mtx_timedlock(mtx_t *mtx, const struct timespec *ts);
  int    mtx_unlock(mtx_t *mtx);

  int    cnd_init(cnd_t *cond);
  void   cnd_destroy(cnd_t *cond);
  int    cnd_signal(cnd_t *cond);
  int    cnd_broadcast(cnd_t *cond);
  int    cnd_wait(cnd_t *cond, mtx_t *mtx);
  int    cnd_timedwait(cnd_t *cond, mtx_t *mtx, const struct timespec *ts);

  int    tss_create(tss_t *key, tss_dtor_t dtor);
  void   tss_delete(tss_t key);
  int    tss_set(tss_t key, void *val);
  void*  tss_get(tss_t key);

*/

#if !defined(__STDC_NO_ATOMICS__)
  #include <stdatomic.h>

  #define AtomicLoad(x, order) atomic_load_explicit((x), (order))
  #define AtomicLoadAcq(x)     atomic_load_explicit((x), memory_order_acquire)

  #define AtomicStore(x, v, order) atomic_store_explicit((x), (v), (order))
  #define AtomicStoreRel(x, v)     atomic_store_explicit((x), (v), memory_order_release)

  // note: these operations return the previous value; _before_ applying the operation
  #define AtomicAdd(x, n, order) atomic_fetch_add_explicit((x), (n), (order))
  #define AtomicSub(x, n, order) atomic_fetch_sub_explicit((x), (n), (order))
  #define AtomicOr(x, n, order)  atomic_fetch_or_explicit((x), (n), (order))
  #define AtomicAnd(x, n, order) atomic_fetch_and_explicit((x), (n), (order))
  #define AtomicXor(x, n, order) atomic_fetch_xor_explicit((x), (n), (order))

  // Compare And Swap
  #define AtomicCAS(p, oldval, newval, order_succ, order_fail) \
    atomic_compare_exchange_strong_explicit( \
      (p), (oldval), (newval), (order_succ), (order_fail))

  #define AtomicCASRel(p, oldval, newval) \
    AtomicCAS((p), (oldval), (newval), memory_order_release, memory_order_relaxed)

  #define AtomicCASAcqRel(p, oldval, newval) \
    AtomicCAS((p), (oldval), (newval), memory_order_acq_rel, memory_order_relaxed)

  // The weak forms of AtomicCAS is allowed to fail spuriously, that is,
  // act as if *obj != *expected even if they are equal. When a compare-and-exchange
  // is in a loop, the weak version will yield better performance on some platforms.
  #define AtomicCASWeak(p, oldval, newval, order_succ, order_fail) \
    atomic_compare_exchange_weak_explicit( \
      (p), (oldval), (newval), (order_succ), (order_fail))

  #define AtomicCASRelaxed(p, oldval, newval) \
    AtomicCASWeak((p), (oldval), (newval), memory_order_relaxed, memory_order_relaxed)

  #define AtomicCASWeakAcq(p, oldval, newval) \
    AtomicCASWeak((p), (oldval), (newval), memory_order_acquire, memory_order_relaxed)

  #define AtomicCASWeakRelAcq(p, oldval, newval) \
    AtomicCASWeak((p), (oldval), (newval), memory_order_release, memory_order_acquire)

  #define AtomicExchange(p, desired_next_value, order) \
    atomic_exchange_explicit((p), (desired_next_value), (order))

#else
  #error "TODO: STDC_NO_ATOMICS"
#endif


// YIELD_THREAD() yields for other threads to be scheduled on the current CPU by the OS
#if (defined(WIN32) || defined(_WIN32))
  #include <windows.h>
  #define YIELD_THREAD() ((void)0)
#elif defined(RSM_NO_LIBC)
  #define YIELD_THREAD() ((void)0)
#else
  #include <sched.h>
  #define YIELD_THREAD() sched_yield() // equivalent to thrd_yield
#endif


// YIELD_CPU() yields for other work on a CPU core
#if defined(__i386) || defined(__i386__) || defined(__x86_64__)
  #define YIELD_CPU() __asm__ __volatile__("pause")
#elif defined(__arm__) || defined(__arm64__) || defined(__aarch64__)
  #define YIELD_CPU() __asm__ __volatile__("yield")
#elif defined(mips) || defined(__mips__) || defined(MIPS) || defined(_MIPS_) || defined(__mips64)
  #if defined(_ABI64) && (_MIPS_SIM == _ABI64)
    #define YIELD_CPU() __asm__ __volatile__("pause")
  #else
    // comment from WebKit source:
    //   The MIPS32 docs state that the PAUSE instruction is a no-op on older
    //   architectures (first added in MIPS32r2). To avoid assembler errors when
    //   targeting pre-r2, we must encode the instruction manually.
    #define YIELD_CPU() __asm__ __volatile__(".word 0x00000140")
  #endif
#elif (defined(WIN32) || defined(_WIN32))
  #include <immintrin.h>
  #define YIELD_CPU() _mm_pause()
#elif defined(RSM_NO_LIBC)
  #define YIELD_CPU() ((void)0)
#else
  // GCC & clang intrinsic
  #define YIELD_CPU() __builtin_ia32_pause()
#endif


// C11 threads API (with pthread shim)
#if defined(RSM_NO_LIBC)
  #error "TODO: impl thread RSM_NO_LIBC"
#elif defined(__STDC_NO_THREADS__) && __STDC_NO_THREADS__
  #include "thread_pthread.h"
#else
  #include <threads.h>
#endif


// RSema is a portable semaphore; a thin layer over the OS's semaphore implementation.
#if defined(_WIN32) || defined(__MACH__)
  typedef uintptr RSema; // intptr instead of void* to improve compiler diagnostics
#elif defined(__unix__)
  ASSUME_NONNULL_END
  #include <semaphore.h>
  ASSUME_NONNULL_BEGIN
  typedef sem_t RSema;
#endif /* RSema */


RSM_ASSUME_NONNULL_BEGIN

bool RSemaInit(RSema*, u32 initcount); // returns false if system impl failed (rare)
void RSemaDispose(RSema*);
bool RSemaWait(RSema*);    // wait for a signal
bool RSemaTryWait(RSema*); // try acquire a signal; return false instead of blocking
bool RSemaTimedWait(RSema*, u64 timeout_usecs);
bool RSemaSignal(RSema*, u32 count /*must be >0*/);


// RLSema is a "light-weight" semaphore which is more efficient than RSema under
// high-contention condition, by avoiding syscalls.
// Waiting when there's already a signal available is extremely cheap and involves
// no syscalls. If there's no signal the implementation will retry by spinning for
// a short while before eventually falling back to RSema.
typedef struct RLSema {
  _Atomic(isize) count;
  RSema          sema;
} RLSema;

bool RLSemaInit(RLSema*, u32 initcount); // returns false if system impl failed (rare)
void RLSemaDispose(RLSema*);
bool RLSemaWait(RLSema*);
bool RLSemaTryWait(RLSema*);
bool RLSemaTimedWait(RLSema*, u64 timeout_usecs);
void RLSemaSignal(RLSema*, u32 count /*must be >0*/);
usize RLSemaApproxAvail(RLSema*);


// RWMutex is a read-write mutex.
// There can be many concurrent readers but only one writer.
// While no write lock is held, up to 16777214 read locks may be held.
// While a write lock is held no read locks or other write locks can be held.
typedef struct RWMutex {
  mtx_t        w; // writer lock
  _Atomic(u32) r; // reader count
} RWMutex;
static int  RWMutexInit(RWMutex* m, int wtype);
static void RWMutexDispose(RWMutex* m);
int RWMutexRLock(RWMutex* m);     // acquire read-only lock (blocks until acquired)
int RWMutexTryRLock(RWMutex* m);  // attempt to acquire read-only lock (non-blocking)
int RWMutexRUnlock(RWMutex* m);   // release read-only lock
int RWMutexLock(RWMutex* m);      // acquire read+write lock (blocks until acquired)
int RWMutexTryLock(RWMutex* m);   // attempt to acquire read+write lock (non-blocking)
int RWMutexUnlock(RWMutex* m);    // release read+write lock


// RHMutex is a mutex that will spin for a short while and then block
typedef struct RHMutex {
  _Atomic(bool) flag;
  _Atomic(i32)  nwait;
  RSema         sema;
} RHMutex;
static bool RHMutexInit(RHMutex* m); // returns false if system failed to init semaphore
static void RHMutexDispose(RHMutex* m);
static void RHMutexLock(RHMutex* m);
static void RHMutexUnlock(RHMutex* m);
void _RHMutexWait(RHMutex* m);

//———————————————————————————————————————————————
// thread inline impl

static inline int RWMutexInit(RWMutex* m, int wtype) {
  assertf(wtype != mtx_timed, "mtx_timed not supported");
  m->r = 0;
  return mtx_init(&m->w, wtype);
}
static inline void RWMutexDispose(RWMutex* m) { mtx_destroy(&m->w); }

inline static bool RHMutexInit(RHMutex* m) {
  m->flag = false;
  m->nwait = 0;
  return RSemaInit(&m->sema, 0);
}

inline static void RHMutexDispose(RHMutex* m) {
  RSemaDispose(&m->sema);
}

inline static void RHMutexLock(RHMutex* m) {
  if (AtomicExchange(&m->flag, true, memory_order_acquire))
    _RHMutexWait(m); // already locked -- slow path
}

inline static void RHMutexUnlock(RHMutex* m) {
  AtomicExchange(&m->flag, false, memory_order_seq_cst);
  if (AtomicLoad(&m->nwait, memory_order_seq_cst) != 0) {
    // at least one thread waiting on a semaphore signal -- wake one thread
    RSemaSignal(&m->sema, 1); // TODO: should we check the return value?
  }
}

RSM_ASSUME_NONNULL_END
