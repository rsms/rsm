// Internal functionality.
// This file is included by every implementation file.
// SPDX-License-Identifier: Apache-2.0
#pragma once
#ifdef RSM_NO_INT_DEFS
  #undef RSM_NO_INT_DEFS
#endif
#include "rsm.h"
#ifndef RSM_NO_LIBC
  // note that rsm.h includes <stdint.h> and <stddef.h>
  #include <sys/types.h> // ssize_t
  typedef int8_t    i8;
  typedef uint8_t   u8;
  typedef int16_t   i16;
  typedef uint16_t  u16;
  typedef int32_t   i32;
  typedef uint32_t  u32;
  typedef int64_t   i64;
  typedef uint64_t  u64;
  typedef size_t    usize;
  typedef ssize_t   isize;
  typedef intptr_t  intptr;
  typedef uintptr_t uintptr;
#else
  typedef signed char        i8;
  typedef unsigned char      u8;
  typedef signed short       i16;
  typedef unsigned short     u16;
  typedef signed int         i32;
  typedef unsigned int       u32;
  typedef signed long long   i64;
  typedef unsigned long long u64;
  #ifdef __INTPTR_MAX__
    #define INTPTR_MIN  (-__INTPTR_MAX__-1L)
    #define INTPTR_MAX  __INTPTR_MAX__
    #define UINTPTR_MAX __UINTPTR_MAX__
  #else
    #define INTPTR_MIN  ISIZE_MIN
    #define INTPTR_MAX  ISIZE_MAX
    #define UINTPTR_MAX USIZE_MAX
  #endif
  #ifdef __SIZE_TYPE__
    typedef __SIZE_TYPE__ usize;
    #if defined(__UINTPTR_MAX__) && defined(__INTPTR_TYPE__) && \
        __SIZE_MAX__ == __UINTPTR_MAX__
      typedef __INTPTR_TYPE__ isize;
    #else
      typedef signed long isize;
    #endif
  #else
    typedef unsigned long usize;
    typedef signed long   isize;
  #endif
  #ifdef __INTPTR_TYPE__
    typedef __INTPTR_TYPE__  intptr;
    typedef __UINTPTR_TYPE__ uintptr;
  #else
    typedef signed long      intptr;
    typedef unsigned long    uintptr;
  #endif
#endif

// because PRIu64 is sad
typedef unsigned long long ull_t;
typedef long long          ill_t;

typedef float  f32;
typedef double f64;

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
#ifdef __SIZE_MAX__
  #define USIZE_MAX __SIZE_MAX__
#else
  #define USIZE_MAX (__LONG_MAX__ *2UL+1UL)
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
  #define UNUSED __attribute__((unused))
#else
  #define UNUSED
#endif
#if __has_attribute(always_inline)
  #define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
  #define ALWAYS_INLINE inline
#endif
#if __has_attribute(noinline)
  #define NOINLINE __attribute__((noinline))
#else
  #define NOINLINE
#endif

#if __has_attribute(aligned)
  #define ATTR_ALIGNED(alignment) __attribute__((__aligned__(alignment)))
#else
  #define ATTR_ALIGNED(alignment)
#endif

#if __has_attribute(packed)
  #define ATTR_PACKED __attribute__((__packed__))
#else
  #define ATTR_PACKED
#endif

#if __has_attribute(may_alias)
  #define ATTR_MAY_ALIAS __attribute__((__may_alias__))
#else
  #define ATTR_MAY_ALIAS
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

// don't use __builtin_* compiler intrinsics for WASM
#if defined(__wasm__) && !defined(__wasi__)
  #define RSM_NO_CC_BUILTINS 1
#endif

// expose Linux/GNU specific libc APIs
#ifdef __linux__
  #define _GNU_SOURCE
#endif

#ifndef _Nonnull
  #define _Nonnull
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
    #error "Can't determine endianness -- please define RSM_LITTLE_ENDIAN=0|1"
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

#ifndef _Pragma
  #error no _Pragma
  #define _Pragma(x)
#endif

#define _DIAGNOSTIC_IGNORE_PUSH(x)  _Pragma("GCC diagnostic push") _Pragma(#x)
#define DIAGNOSTIC_IGNORE_PUSH(STR) _DIAGNOSTIC_IGNORE_PUSH(GCC diagnostic ignored STR)
#define DIAGNOSTIC_IGNORE_POP()     _Pragma("GCC diagnostic pop")

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

// container_of returns a pointer to the parent struct of one of its members (ptr).
#define container_of(ptr, struct_type, struct_member) ({ \
  const __typeof__( ((struct_type*)0)->struct_member )* ptrx__ = (ptr); \
  (struct_type*)( (u8*)ptrx__ - offsetof(struct_type,struct_member) ); \
})

#define rsm_same_type(a, b) __builtin_types_compatible_p(__typeof__(a), __typeof__(b))

#define MAX(a,b) ({__typeof__(a) _a = (a); __typeof__(b) _b = (b); _a > _b ? _a : _b; })
#define MIN(a,b) ({__typeof__(a) _a = (a); __typeof__(b) _b = (b); _a < _b ? _a : _b; })
  // turns into CMP + CMOV{L,G} on x86_64
  // turns into CMP + CSEL on arm64

#define MAX_X(a,b) ((a) > (b) ? (a) : (b))
#define MIN_X(a,b) ((a) < (b) ? (a) : (b))

// T IDIV_CEIL(T x, ANY divisor) divides x by divisor, rounding up.
// If x is zero, returns max value of x (wraps.)
#define IDIV_CEIL(x, divisor) ({ \
  __typeof__(x) div__ = (__typeof__(x))(divisor); \
  ( (x) + div__ - 1 ) / div__; \
})
#define IDIV_CEIL_X(x, divisor) \
  ( ( (x) + (__typeof__(x))(divisor) - 1 ) / (__typeof__(x))(divisor) )

// T ALIGN2<T>(T x, anyuint a) rounds up x to nearest a (a must be a power of two)
#define ALIGN2(x,a) ({ \
  __typeof__(x) atmp__ = (__typeof__(x))(a) - 1; \
  ( (x) + atmp__ ) & ~atmp__; \
})
#define ALIGN2_X(x,a) ( \
  ( (x) + ((__typeof__(x))(a) - 1) ) & ~((__typeof__(x))(a) - 1) \
)

// T ALIGN2_FLOOR<T>(T x, anyuint a) rounds down x to nearest a
#define ALIGN2_FLOOR(x, a) ({ \
  __typeof__(x) atmp__ = (__typeof__(x))(a) - 1; \
  ( ((x) - (atmp__ - 1)) + atmp__ ) & ~atmp__; \
})
#define ALIGN2_FLOOR_X(x, a) ( \
  ( ((x) - (((__typeof__(x))(a) - 1) - 1)) + ((__typeof__(x))(a) - 1) ) & \
  ~((__typeof__(x))(a) - 1) \
)

// bool IS_ALIGN2(T x, anyuint a) returns true if x is aligned to a
#define IS_ALIGN2(x, a)  ( !((x) & ((__typeof__(x))(a) - 1)) )

// T ALIGN_CEIL(T x, T a) rounds x up to nearest multiple of a.
// e.g. ALIGN_CEIL(11, 5) => 15
#define ALIGN_CEIL(x, a) ({ \
  const __typeof__(x) atmp__ = (__typeof__(x))(a); \
  assert(atmp__ > 0); \
  ( (((x) + atmp__) - 1) / atmp__ ) * atmp__; \
})

// T ALIGN_FLOOR(T x, T a) rounds x down to nearest multiple of a.
// e.g. ALIGN_FLOOR(11, 5) => 10
#define ALIGN_FLOOR(x, a) ({ \
  const __typeof__(x) atmp__ = (__typeof__(x))(a); \
  ((x) / atmp__) * atmp__; \
})

// T COND_FLAG(T flags, T flag, bool on)
// branchless ( on ? (flags | flag) : (flags & ~flag) )
#define COND_FLAG(flags, flag, on) ({ \
  __typeof__(flags) flags__ = (flags); \
  (flags__ ^ (( (__typeof__(flags))-(!!(on)) ^ flags__ ) & (__typeof__(flags))(flag)));\
})
#define COND_FLAG_X(flags, flag, on) \
  ((flags) ^ (( (__typeof__(flags))-(!!(on)) ^ (flags) ) & (__typeof__(flags))(flag)))

// POISON constants are non-NULL addresses which will result in page faults on access.
// Values match those of Linux.
#define GENERIC_POISON1 ((void*)0x100)
#define GENERIC_POISON2 ((void*)0x122)
#define PAGE_POISON     0xaa


// int rsm_popcount(ANYINT v) returns the number of set bits in x
#define rsm_popcount(x) ( \
  __builtin_constant_p(x) ? RSM_POPCOUNT_X(x) : \
  _Generic((x), \
    i8:    __builtin_popcount,   u8:    __builtin_popcount, \
    i16:   __builtin_popcount,   u16:   __builtin_popcount, \
    i32:   __builtin_popcount,   u32:   __builtin_popcount, \
    isize: __builtin_popcountl,  usize: __builtin_popcountl, \
    i64:   __builtin_popcountll, u64:   __builtin_popcountll\
  )(x) \
)

// RSM_POPCOUNT_X constant-expression bit population count.
// T RSM_POPCOUNT_X(T v) {
//   v = v - ((v >> 1) & (T)~(T)0/3);
//   v = (v & (T)~(T)0/15*3) + ((v >> 2) & (T)~(T)0/15*3);
//   v = (v + (v >> 4)) & (T)~(T)0/255*15;
//   return (T)(v * ((T)~(T)0/255)) >> (sizeof(T) - 1) * CHAR_BIT;
// }
#define RSM_POPCOUNT_X(v) ((u32)( \
  (__typeof__(v))(_POPCNT_CONST_3(v) * ((__typeof__(v))~(__typeof__(v))0/255)) >> \
  (sizeof(__typeof__(v)) - 1) * 8 \
))
#define _POPCNT_CONST_3(v) \
  ( (_POPCNT_CONST_2(v) + (_POPCNT_CONST_2(v) >> 4)) & \
    (__typeof__(v))~(__typeof__(v))0/255*15 )
#define _POPCNT_CONST_2(v) \
  ( (_POPCNT_CONST_1(v) & (__typeof__(v))~(__typeof__(v))0/15*3) + \
    ((_POPCNT_CONST_1(v) >> 2) & (__typeof__(v))~(__typeof__(v))0/15*3) )
#define _POPCNT_CONST_1(v) \
  ( (v) - (((v) >> 1) & (__typeof__(v))~(__typeof__(v))0/3) )


// int rsm_ctz(ANYUINT x) counts trailing zeroes in x,
// starting at the least significant bit position.
// If x is 0, the result is undefined.
#define rsm_ctz(x) _Generic((x), \
  i8:   __builtin_ctz,   u8:    __builtin_ctz, \
  i16:  __builtin_ctz,   u16:   __builtin_ctz, \
  i32:  __builtin_ctz,   u32:   __builtin_ctz, \
  long: __builtin_ctzl,  unsigned long: __builtin_ctzl, \
  long long:  __builtin_ctzll, unsigned long long:   __builtin_ctzll)(x)

// int rsm_clz(ANYUINT x) counts leading zeroes in x,
// starting at the most significant bit position.
// If x is 0, the result is undefined.
#define rsm_clz(x) ( \
  _Generic((x), \
    i8:   __builtin_clz,   u8:    __builtin_clz, \
    i16:  __builtin_clz,   u16:   __builtin_clz, \
    i32:  __builtin_clz,   u32:   __builtin_clz, \
    long: __builtin_clzl,  unsigned long: __builtin_clzl, \
    long long:  __builtin_clzll, unsigned long long:   __builtin_clzll \
  )(x) - ( 32 - MIN_X(4, (int)sizeof(__typeof__(x)))*8 ) \
)
#define RSM_CLZ_X rsm_clz


// int rsm_ffs(ANYINT x) returns one plus the index of the least significant 1-bit of x,
// or if x is zero, returns zero.
#define rsm_ffs(x) _Generic((x), \
  i8:   __builtin_ffs,   u8:    __builtin_ffs, \
  i16:  __builtin_ffs,   u16:   __builtin_ffs, \
  i32:  __builtin_ffs,   u32:   __builtin_ffs, \
  long: __builtin_ffsl,   unsigned long: __builtin_ffsl, \
  long long:  __builtin_ffsll, unsigned long long: __builtin_ffsll \
)(x)
#define RSM_FFS_X  rsm_ffs

// int rsm_fls(ANYINT n) finds the Find Last Set bit (last = most-significant)
// (Note that this is not the same as rsm_ffs(x)-1).
// e.g. rsm_fls(0b1111111111111111) = 15
// e.g. rsm_fls(0b1000000000000000) = 15
// e.g. rsm_fls(0b1000000000000000) = 15
// e.g. rsm_fls(0b1000) = 3
#define rsm_fls(x)  ( (x) ? (int)(sizeof(__typeof__(x)) * 8) - rsm_clz(x) : 0 )
#define RSM_FLS_X   rsm_fls

// int ILOG2(ANYINT n) calculates the log of base 2, rounding down.
// e.g. ILOG2(15) = 3, ILOG2(16) = 4.
// Result is undefined if n is 0.
#define ILOG2(n) (rsm_fls(n) - 1)

// ANYINT FLOOR_POW2(ANYINT x) rounds down x to nearest power of two.
// Returns 1 if x is 0.
#define FLOOR_POW2(x) ({ \
  __typeof__(x) xtmp__ = (x); \
  FLOOR_POW2_X(xtmp__); \
})
// FLOOR_POW2_X is a constant-expression implementation of FLOOR_POW2.
// When used as a constant expression, compilation fails if x is 0.
#define FLOOR_POW2_X(x) ( \
  ((x) <= 1) ? (__typeof__(x))1 : \
  ((__typeof__(x))1 << ILOG2(x)) \
)

// ANYINT CEIL_POW2(ANYINT x) rounds up x to nearest power of two.
// Returns 1 when x is 0.
// Returns 0 when x is larger than the max pow2 for x's type (e.g. >0x80000000 for u32)
#define CEIL_POW2(x) ({ \
  __typeof__(x) xtmp__ = (x); \
  CEIL_POW2_X(xtmp__); \
})
// CEIL_POW2_X is a constant-expression implementation of CEIL_POW2
#define CEIL_POW2_X(x) ( \
  ((x) <= (__typeof__(x))1) ? (__typeof__(x))1 : \
  ( ( ((__typeof__(x))1 << \
          ILOG2( ((x) - ((x) == (__typeof__(x))1) ) - (__typeof__(x))1) \
      ) - (__typeof__(x))1 ) << 1 ) \
  + (__typeof__(x))2 \
)

// bool IS_POW2(T x) returns true if x is a power-of-two value
#define IS_POW2(x)    ({ __typeof__(x) xtmp__ = (x); IS_POW2_X(xtmp__); })
#define IS_POW2_X(x)  ( ((x) & ((x) - 1)) == 0 )

// T RSM_IPOW2(T x) returns the result of raising 2 to x (2^x).
// E.g. RSM_IPOW2(4) = 16, RSM_IPOW2(5) = 32
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

#define check_sub_overflow(a, b, dst) __must_check_unlikely(({  \
  __typeof__(a) a__ = (a);                 \
  __typeof__(b) b__ = (b);                 \
  __typeof__(dst) dst__ = (dst);           \
  (void) (&a__ == &b__);                   \
  (void) (&a__ == dst__);                  \
  __builtin_sub_overflow(a__, b__, dst__); \
}))

#define check_mul_overflow(a, b, dst) __must_check_unlikely(({  \
  __typeof__(a) a__ = (a);                 \
  __typeof__(b) b__ = (b);                 \
  __typeof__(dst) dst__ = (dst);           \
  (void) (&a__ == &b__);                   \
  (void) (&a__ == dst__);                  \
  __builtin_mul_overflow(a__, b__, dst__); \
}))

// bool check_align_ceil_overflow(ANYINT x, ANYINT a, ANYINT* dst)
// Attempts to align x to a, returning true if the operation overflowed.
#define check_align_ceil_overflow(x, a, dst) ({ \
  __typeof__(x) x__ = (x); \
  __typeof__(x) align__ = (a); \
  __typeof__(x) rem__ = x__ % align__; \
  __typeof__(dst) dst1__ = (dst); \
  rem__ ? check_add_overflow(x__, align__ - rem__, dst1__) : (*dst1__ = x, false); \
})

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

// kibibyte magnitudes for making code easier to read
#define KiB 1024lu
#define MiB 0x100000lu   /* 1024^2 */
#define GiB 0x40000000lu /* 1024^3 */
#define TiB 0x10000000000llu /* ... */
#define PiB 0x4000000000000llu
#define EiB 0x1000000000000000llu

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
#define comptime_assert(condition, msg) _Static_assert(condition, msg)
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
    (UNLIKELY(!(cond)) ? _assertfail(fmt " (%s)", ##args, #cond) : ((void)0))

  #define assert(cond) \
    (UNLIKELY(!(cond)) ? _assertfail("%s", #cond) : ((void)0))

  #define assertcstreq(cstr1, cstr2) ({                  \
    const char* cstr1__ = (cstr1);                       \
    const char* cstr2__ = (cstr2);                       \
    if (UNLIKELY(strcmp(cstr1__, cstr2__) != 0))         \
      _assertfail("\"%s\" != \"%s\"", cstr1__, cstr2__); \
  })

  #define assertnull(a)  assert((a) == NULL)
  #define assertnotnull(a) ({                                              \
    __typeof__(*(a))* nullable val__ = (a);                                \
    UNUSED const void* valp__ = val__; /* build bug on non-pointer */      \
    if (UNLIKELY(val__ == NULL))                                           \
      _assertfail("%s != NULL", #a);                                       \
    val__; })

#else /* !defined(NDEBUG) */
  #undef DEBUG
  #undef NDEBUG
  #define NDEBUG 1
  #define assert(cond)            ((void)0)
  #define assertf(cond, fmt, ...) ((void)0)
  #define assertcstreq(a,b)       ((void)0)
  #define assertnull(a)           ((void)0)
  #define assertnotnull(a)        ({ a; }) /* note: (a) causes "unused" warnings */
#endif /* !defined(NDEBUG) */

// RSM_SAFE -- checks enabled in "debug" and "safe" builds (but not in "fast" builds.)
//
// void safecheck(COND)                         stripped from non-safe builds
// void safecheckf(COND, const char* fmt, ...)  stripped from non-safe builds
// void safecheckx(COND)                        included in non-safe builds w/o check
// void safecheckxf(COND, const char* fmt, ...) included in non-safe builds w/o check
// typeof(EXPR) safecheckexpr(EXPR, EXPECT)     included in non-safe builds w/o check
// typeof(EXPR) safechecknotnull(EXPR)          included in non-safe builds w/o check
//
#if defined(RSM_SAFE)
  #undef RSM_SAFE
  #define RSM_SAFE 1
  #define _safefail(fmt, args...) _panic(__FILE__, __LINE__, __FUNCTION__, fmt, ##args)
  #define safecheckf(cond, fmt, args...)  if UNLIKELY(!(cond)) _safefail(fmt, ##args)
  #define safecheckxf safecheckf
  #define safecheckx safecheck
  #ifdef DEBUG
    #define safecheck(cond)  if UNLIKELY(!(cond)) _safefail("safecheck (%s)", #cond)
    #define safecheckexpr(expr, expect) ({                                        \
      __typeof__(expr) val__ = (expr);                                            \
      safecheckf(val__ == expect, "unexpected value (%s != %s)", #expr, #expect); \
      val__; })
    #define safechecknotnull(a) ({                                           \
      __typeof__(a) val__ = (a);                                             \
      UNUSED const void* valp__ = val__; /* build bug on non-pointer */ \
      safecheckf(val__ != NULL, "unexpected NULL (%s)", #a);                 \
      val__; })
  #else
    #define safecheck(cond) if UNLIKELY(!(cond)) _safefail("safecheck")
    #define safecheckexpr(expr, expect) ({ \
      __typeof__(expr) val__ = (expr); safecheck(val__ == expect); val__; })
    #define safechecknotnull(a) ({                                           \
      __typeof__(a) val__ = (a);                                             \
      UNUSED const void* valp__ = val__; /* build bug on non-pointer */ \
      safecheckf(val__ != NULL, "NULL");                                     \
      val__; })
  #endif
#else
  #define safecheckf(...)             ((void)0)
  #define safecheck(cond)             ((void)0)
  #define safecheckx(cond)            do{ UNUSED bool _ = (cond); }while(0)
  #define safecheckxf(cond, ...)      do{ UNUSED bool _ = (cond); }while(0)
  #define safecheckexpr(expr, expect) (expr) /* intentionally complain if not used */
  #define safechecknotnull(a)         ({ a; }) /* note: (a) causes "unused" warnings */
#endif

// void dlog(const char* fmt, ...)
#ifdef DEBUG
  #ifdef RSM_NO_LIBC
    #define dlog(format, args...) \
      log("[D] " format " (%s:%d)", ##args, __FILE__, __LINE__)
  #else
    #include <unistd.h> // isatty
    #define dlog(format, args...) ({                                 \
      flockfile(stderr);                                             \
      if (isatty(2)) log("\e[1;30m▍\e[0m" format " \e[2m%s:%d\e[0m", \
                         ##args, __FILE__, __LINE__);                \
      else           log("[D] " format " (%s:%d)",                   \
                         ##args, __FILE__, __LINE__);                \
      fflush(stderr);                                                \
      funlockfile(stderr); })
  #endif
#else
  #define dlog(format, ...) ((void)0)
#endif

// --------------------------------------------------------------------------------------
RSM_ASSUME_NONNULL_BEGIN

// minimal set of libc functions

#if __has_builtin(__builtin_memset) && !RSM_NO_CC_BUILTINS
  #define memset __builtin_memset
#else
  void* memset(void* p, int c, usize n);
#endif

#if __has_builtin(__builtin_memcpy) && !RSM_NO_CC_BUILTINS
  #define memcpy __builtin_memcpy
#else
  void* memcpy(void* restrict dst, const void* restrict src, usize n);
#endif

#if __has_builtin(__builtin_memmove) && !RSM_NO_CC_BUILTINS
  #define memmove __builtin_memmove
#else
  void* memmove(void* dest, const void* src, usize n);
#endif

#if __has_builtin(__builtin_memcmp) && !RSM_NO_CC_BUILTINS
  #define memcmp __builtin_memcmp
#else
  int memcmp(const void* l, const void* r, usize n);
#endif

#if __has_builtin(__builtin_strlen) && !RSM_NO_CC_BUILTINS
  #define strlen __builtin_strlen
#else
  usize strlen(const char* s);
#endif

#if __has_builtin(__builtin_strcmp) && !RSM_NO_CC_BUILTINS
  #define strcmp __builtin_strcmp
#else
  int strcmp(const char* l, const char* r);
#endif

#if RSM_NO_CC_BUILTINS
  int vsnprintf(char *restrict s, usize n, const char *restrict fmt, va_list ap);
  int snprintf(char* restrict s, usize n, const char* restrict fmt, ...);
#endif // printf

// rsm_qsort is qsort_r aka qsort_s
typedef int(*rsm_qsort_cmp)(const void* x, const void* y, void* nullable ctx);
void rsm_qsort(void* base, usize nmemb, usize size, rsm_qsort_cmp cmp, void* nullable ctx);

// --------------------------------------------------------------------------------------
// internal utility functions, like a string buffer. Not namespaced. See util.c

#define PAGE_SIZE       RSM_PAGE_SIZE
#define PAGE_SIZE_BITS  12u /* ILOG2(PAGE_SIZE) */

#define UTF8_SELF  0x80 // UTF-8 "self" byte constant

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

usize stru64(char* buf, u64 v, u32 base);
rerr_t parseu64(const char* src, usize srclen, int base, u64* result, u64 cutoff);

rerr_t mmapfile(const char* filename, rmem_t* data_out);
void unmapfile(rmem_t);
rerr_t read_stdin_data(rmemalloc_t* ma, usize maxlen, rmem_t* data_out);
rerr_t writefile(const char* filename, u32 mode, const void* data, usize size);

rerr_t rerr_errno(int errnoval);

noreturn void _panic(const char* file, int line, const char* fun, const char* fmt, ...)
  ATTR_FORMAT(printf, 4, 5);

#define RMEM_SAFECHECK(region) \
  safecheckf(RMEM_IS_VALID(region), \
    "invalid memory region " RMEM_FMT, RMEM_FMT_ARGS(region))

// rmem_scrubcheck is a memory-allocator debugging tool that checks if a memory region
// is *probably* uninitialized or freed, by testing if all bytes at ptr is a
// known "scrub" (aka "poison") byte, internal to the memory allocator.
// Returns:
//   "ok"      definitely not scrubbed
//   "uninit"  possibly uninitialized memory (all )
//   "freed"   possibly "use after free"
const char* rmem_scrubcheck(void* ptr, usize size);

// host memory functions
usize os_pagesize();
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

// forward declaration of abuf_t
typedef struct abuf abuf_t;

// fmtinstr appends to s a printable representation of in
u32 fmtinstr(abuf_t* s, rin_t in, rfmtflag_t fl);

// unixtime stores the number of seconds + nanoseconds since Jan 1 1970 00:00:00 UTC
// at *sec and *nsec
rerr_t unixtime(i64* sec, u64* nsec);

// nanotime returns nanoseconds measured from an undefined point in time.
// It uses the most high-resolution, low-latency clock available on the system.
// u64 is enough to express 584 years in nanoseconds.
u64 nanotime();

// rsm_nanosleep suspends the calling thread for nsec nanoseconds.
// Returns remaining time to sleep (if the thread was interrupted.)
// U64_MAX = 584.9 years (18446744073709551615/1000000000/60/60/24/365)
u64 rsm_nanosleep(u64 nsec);

// fmtduration appends human-readable time duration to buf, including a null terminator.
// Returns number of bytes written, excluding the null terminator.
usize fmtduration(char buf[25], u64 duration_ns);


RSM_ASSUME_NONNULL_END
