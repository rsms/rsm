#if defined(__wasm__) && !defined(__wasi__)
#include "rsmimpl.h"
/*
This code has been derived from the musl project and is licensed as followed (MIT)

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

_Pragma("GCC diagnostic ignored \"-Wimplicit-fallthrough\"")
_Pragma("GCC diagnostic ignored \"-Wshift-op-parentheses\"")

typedef usize size_t;
typedef usize off_t;
typedef usize uintmax_t;
typedef isize intmax_t;
typedef intptr ptrdiff_t;
typedef uintptr uintptr_t;
typedef u32 uint32_t;
typedef u16 uint16_t;
typedef u64 uint64_t;
typedef __WCHAR_TYPE__ wchar_t;

typedef struct _IO_FILE FILE;

#define INT_MAX    I32_MAX
#define ULONG_MAX  USIZE_MAX
#define INTMAX_MAX ISIZE_MAX

#define hidden __attribute__((__visibility__("hidden")))
// #define weak __attribute__((__weak__))
// #define weak_alias(old, new) \
//   extern __typeof(old) new __attribute__((__weak__, __alias__(#old)))

#undef EOF
#define EOF (-1)

// errno
static int errno = 0;
#define EINVAL          22
#define EOVERFLOW       75
#define EILSEQ          84

#define __LITTLE_ENDIAN 1234
#define __BYTE_ORDER __LITTLE_ENDIAN


static void *memchr(const void *src, int c, size_t n) {
  const unsigned char *s = src;
  c = (unsigned char)c;
  for (; n && *s != c; s++, n--);
  return n ? (void *)s : 0;
}


static size_t strnlen(const char *s, size_t n)
{
  const char *p = memchr(s, 0, n);
  return p ? p-s : n;
}



// ---

// typedef u64    rep_t;
// typedef i64    srep_t;
// typedef double fp_t;
// #define REP_C UINT64_C
// #define significandBits 52

// //===-- lib/comparetf2.c - Quad-precision comparisons -------------*- C -*-===//
// //
// //                     The LLVM Compiler Infrastructure
// //
// // This file is dual licensed under the MIT and the University of Illinois Open
// // Source Licenses. See LICENSE.TXT for details.
// //
// //===----------------------------------------------------------------------===//
// //
// // // This file implements the following soft-float comparison routines:
// //
// //   __eqtf2   __getf2   __unordtf2
// //   __letf2   __gttf2
// //   __lttf2
// //   __netf2
// //
// // The semantics of the routines grouped in each column are identical, so there
// // is a single implementation for each, and wrappers to provide the other names.
// //
// // The main routines behave as follows:
// //
// //   __letf2(a,b) returns -1 if a < b
// //                         0 if a == b
// //                         1 if a > b
// //                         1 if either a or b is NaN
// //
// //   __getf2(a,b) returns -1 if a < b
// //                         0 if a == b
// //                         1 if a > b
// //                        -1 if either a or b is NaN
// //
// //   __unordtf2(a,b) returns 0 if both a and b are numbers
// //                           1 if either a or b is NaN
// //
// // Note that __letf2( ) and __getf2( ) are identical except in their handling of
// // NaN values.
// //
// //===----------------------------------------------------------------------===//

// // #define QUAD_PRECISION

// enum LE_RESULT {
//     LE_LESS      = -1,
//     LE_EQUAL     =  0,
//     LE_GREATER   =  1,
//     LE_UNORDERED =  1
// };

// enum LE_RESULT __letf2(fp_t a, fp_t b) {

//     const srep_t aInt = toRep(a);
//     const srep_t bInt = toRep(b);
//     const rep_t aAbs = aInt & absMask;
//     const rep_t bAbs = bInt & absMask;

//     // If either a or b is NaN, they are unordered.
//     if (aAbs > infRep || bAbs > infRep) return LE_UNORDERED;

//     // If a and b are both zeros, they are equal.
//     if ((aAbs | bAbs) == 0) return LE_EQUAL;

//     // If at least one of a and b is positive, we get the same result comparing
//     // a and b as signed integers as we would with a floating-point compare.
//     if ((aInt & bInt) >= 0) {
//         if (aInt < bInt) return LE_LESS;
//         else if (aInt == bInt) return LE_EQUAL;
//         else return LE_GREATER;
//     }
//     else {
//         // Otherwise, both are negative, so we need to flip the sense of the
//         // comparison to get the correct result.  (This assumes a twos- or ones-
//         // complement integer representation; if integers are represented in a
//         // sign-magnitude representation, then this flip is incorrect).
//         if (aInt > bInt) return LE_LESS;
//         else if (aInt == bInt) return LE_EQUAL;
//         else return LE_GREATER;
//     }
// }

// enum GE_RESULT {
//     GE_LESS      = -1,
//     GE_EQUAL     =  0,
//     GE_GREATER   =  1,
//     GE_UNORDERED = -1   // Note: different from LE_UNORDERED
// };

// enum GE_RESULT __getf2(fp_t a, fp_t b) {

//     const srep_t aInt = toRep(a);
//     const srep_t bInt = toRep(b);
//     const rep_t aAbs = aInt & absMask;
//     const rep_t bAbs = bInt & absMask;

//     if (aAbs > infRep || bAbs > infRep) return GE_UNORDERED;
//     if ((aAbs | bAbs) == 0) return GE_EQUAL;
//     if ((aInt & bInt) >= 0) {
//         if (aInt < bInt) return GE_LESS;
//         else if (aInt == bInt) return GE_EQUAL;
//         else return GE_GREATER;
//     } else {
//         if (aInt > bInt) return GE_LESS;
//         else if (aInt == bInt) return GE_EQUAL;
//         else return GE_GREATER;
//     }
// }

// int __unordtf2(fp_t a, fp_t b) {
//     const rep_t aAbs = toRep(a) & absMask;
//     const rep_t bAbs = toRep(b) & absMask;
//     return aAbs > infRep || bAbs > infRep;
// }

// // The following are alternative names for the preceding routines.

// enum LE_RESULT __eqtf2(fp_t a, fp_t b) {
//     return __letf2(a, b);
// }

// enum LE_RESULT __lttf2(fp_t a, fp_t b) {
//     return __letf2(a, b);
// }

// enum LE_RESULT __netf2(fp_t a, fp_t b) {
//     return __letf2(a, b);
// }

// enum GE_RESULT __gttf2(fp_t a, fp_t b) {
//     return __getf2(a, b);
// }




// --- arch/x86_64/bits/float.h (matches wasm ... I think)

// #ifdef __FLT_EVAL_METHOD__
// #define FLT_EVAL_METHOD __FLT_EVAL_METHOD__
// #else
// #define FLT_EVAL_METHOD 0
// #endif

// #define LDBL_TRUE_MIN 3.6451995318824746025e-4951L
// #define LDBL_MIN     3.3621031431120935063e-4932L
// #define LDBL_MAX     1.1897314953572317650e+4932L
// #define LDBL_EPSILON 1.0842021724855044340e-19L

#define LDBL_MANT_DIG 64
// #define LDBL_MIN_EXP (-16381)
// #define LDBL_MAX_EXP 16384

// #define LDBL_DIG 18
// #define LDBL_MIN_10_EXP (-4931)
// #define LDBL_MAX_10_EXP 4932

// #define DECIMAL_DIG 21



// --- include/limits.h

#define NL_ARGMAX 9




// --- multibyte/wctomb.c

#define IS_CODEUNIT(c) ((unsigned)(c)-0xdf80 < 0x80)
#define MB_CUR_MAX 4 // (CURRENT_UTF8 ? 4 : 1)

static size_t wcrtomb(char *restrict s, wchar_t wc/*, mbstate_t *restrict st*/)
{
  if (!s) return 1;
  if ((unsigned)wc < 0x80) {
    *s = wc;
    return 1;
  } else if (MB_CUR_MAX == 1) {
    if (!IS_CODEUNIT(wc)) {
      errno = EILSEQ;
      return -1;
    }
    *s = wc;
    return 1;
  } else if ((unsigned)wc < 0x800) {
    *s++ = 0xc0 | (wc>>6);
    *s = 0x80 | (wc&0x3f);
    return 2;
  } else if ((unsigned)wc < 0xd800 || (unsigned)wc-0xe000 < 0x2000) {
    *s++ = 0xe0 | (wc>>12);
    *s++ = 0x80 | ((wc>>6)&0x3f);
    *s = 0x80 | (wc&0x3f);
    return 3;
  } else if ((unsigned)wc-0x10000 < 0x100000) {
    *s++ = 0xf0 | (wc>>18);
    *s++ = 0x80 | ((wc>>12)&0x3f);
    *s++ = 0x80 | ((wc>>6)&0x3f);
    *s = 0x80 | (wc&0x3f);
    return 4;
  }
  errno = EILSEQ;
  return -1;
}

static int wctomb(char *s, wchar_t wc)
{
  if (!s) return 0;
  return wcrtomb(s, wc);
}




// --- internal/stdio_impl.h

#define UNGET 8

// #define FFINALLOCK(f) ((f)->lock>=0 ? __lockfile((f)) : 0)
#define FLOCK(f) // int __need_unlock = ((f)->lock>=0 ? __lockfile((f)) : 0)
#define FUNLOCK(f) // do { if (__need_unlock) __unlockfile((f)); } while (0)

#define F_PERM 1
#define F_NORD 4
#define F_NOWR 8
#define F_EOF 16
#define F_ERR 32
#define F_SVB 64
#define F_APP 128

struct _IO_FILE {
  unsigned flags;
  unsigned char *rpos, *rend;
  int (*close)(FILE *);
  unsigned char *wend, *wpos;
  unsigned char *mustbezero_1;
  unsigned char *wbase;
  size_t (*read)(FILE *, unsigned char *, size_t);
  size_t (*write)(FILE *, const unsigned char *, size_t);
  off_t (*seek)(FILE *, off_t, int);
  unsigned char *buf;
  size_t buf_size;
  FILE *prev, *next;
  int fd;
  int pipe_pid;
  long lockcount;
  int mode;
  volatile int lock;
  int lbf;
  void *cookie;
  off_t off;
  char *getln_buf;
  void *mustbezero_2;
  unsigned char *shend;
  off_t shlim, shcnt;
  FILE *prev_locked, *next_locked;
  struct __locale_struct *locale;
};

// extern hidden FILE *volatile __stdin_used;
// extern hidden FILE *volatile __stdout_used;
// extern hidden FILE *volatile __stderr_used;

// hidden int __lockfile(FILE *);
// hidden void __unlockfile(FILE *);

// hidden size_t __stdio_read(FILE *, unsigned char *, size_t);
// hidden size_t __stdio_write(FILE *, const unsigned char *, size_t);
// hidden size_t __stdout_write(FILE *, const unsigned char *, size_t);
// hidden off_t __stdio_seek(FILE *, off_t, int);
// hidden int __stdio_close(FILE *);

// hidden int __toread(FILE *);
// hidden int __towrite(FILE *);
static int __towrite(FILE *f)
{
  f->mode |= f->mode-1;
  if (f->flags & F_NOWR) {
    f->flags |= F_ERR;
    return EOF;
  }
  /* Clear read buffer (easier than summoning nasal demons) */
  f->rpos = f->rend = 0;

  /* Activate write through the buffer. */
  f->wpos = f->wbase = f->buf;
  f->wend = f->buf + f->buf_size;

  return 0;
}

// hidden void __stdio_exit(void);
// hidden void __stdio_exit_needed(void);

// #if defined(__PIC__) && (100*__GNUC__+__GNUC_MINOR__ >= 303)
// __attribute__((visibility("protected")))
// #endif
// int __overflow(FILE *, int), __uflow(FILE *);

// hidden int __fseeko(FILE *, off_t, int);
// hidden int __fseeko_unlocked(FILE *, off_t, int);
// hidden off_t __ftello(FILE *);
// hidden off_t __ftello_unlocked(FILE *);
// hidden size_t __fwritex(const unsigned char *, size_t, FILE *);
static size_t __fwritex(const unsigned char *restrict s, size_t l, FILE *restrict f)
{
  size_t i=0;

  if (!f->wend && __towrite(f)) return 0;

  if (l > (size_t)(f->wend - f->wpos)) return f->write(f, s, l);

  if (f->lbf >= 0) {
    /* Match /^(.*\n|)/ */
    for (i=l; i && s[i-1] != '\n'; i--);
    if (i) {
      size_t n = f->write(f, s, i);
      if (n < i) return n;
      s += i;
      l -= i;
    }
  }

  memcpy(f->wpos, s, l);
  f->wpos += l;
  return l+i;
}

// hidden int __putc_unlocked(int, FILE *);

// hidden FILE *__fdopen(int, const char *);
// hidden int __fmodeflags(const char *);

// hidden FILE *__ofl_add(FILE *f);
// hidden FILE **__ofl_lock(void);
// hidden void __ofl_unlock(void);

// struct __pthread;
// hidden void __register_locked_file(FILE *, struct __pthread *);
// hidden void __unlist_locked_file(FILE *);
// hidden void __do_orphaned_stdio_locks(void);

// #define MAYBE_WAITERS 0x40000000

// hidden void __getopt_msg(const char *, const char *, const char *, size_t);

// #define feof(f) ((f)->flags & F_EOF)
// #define ferror(f) ((f)->flags & F_ERR)

// #define getc_unlocked(f) \
//   ( ((f)->rpos != (f)->rend) ? *(f)->rpos++ : __uflow((f)) )

// #define putc_unlocked(c, f) \
//   ( (((unsigned char)(c)!=(f)->lbf && (f)->wpos!=(f)->wend)) \
//   ? *(f)->wpos++ = (unsigned char)(c) \
//   : __overflow((f),(unsigned char)(c)) )

// /* Caller-allocated FILE * operations */
// hidden FILE *__fopen_rb_ca(const char *, FILE *, unsigned char *, size_t);
// hidden int __fclose_ca(FILE *);




// --- vfprintf.c


/* Convenient bit representation for modifier flags, which all fall
 * within 31 codepoints of the space character. */

#define ALT_FORM   (1U << ('#'-' '))
#define ZERO_PAD   (1U << ('0'-' '))
#define LEFT_ADJ   (1U << ('-'-' '))
#define PAD_POS    (1U << (' '-' '))
#define MARK_POS   (1U << ('+'-' '))
#define GROUPED    (1U << ('\''-' '))

#define FLAGMASK (ALT_FORM|ZERO_PAD|LEFT_ADJ|PAD_POS|MARK_POS|GROUPED)

/* State machine to accept length modifiers + conversion specifiers.
 * Result is 0 on failure, or an argument type to pop on success. */

enum {
  BARE, LPRE, LLPRE, HPRE, HHPRE, BIGLPRE,
  ZTPRE, JPRE,
  STOP,
  PTR, INT, UINT, ULLONG,
  LONG, ULONG,
  SHORT, USHORT, CHAR, UCHAR,
  LLONG, SIZET, IMAX, UMAX, PDIFF, UIPTR,
  // DBL, LDBL,
  NOARG,
  MAXSTATE
};

#define S(x) [(x)-'A']

static const unsigned char states[]['z'-'A'+1] = {
  { /* 0: bare types */
    S('d') = INT, S('i') = INT,
    S('o') = UINT, S('u') = UINT, S('x') = UINT, S('X') = UINT,
    // S('e') = DBL, S('f') = DBL, S('g') = DBL, S('a') = DBL,
    // S('E') = DBL, S('F') = DBL, S('G') = DBL, S('A') = DBL,
    S('c') = CHAR, S('C') = INT,
    S('s') = PTR, S('S') = PTR, S('p') = UIPTR, S('n') = PTR,
    S('m') = NOARG,
    S('l') = LPRE, S('h') = HPRE, S('L') = BIGLPRE,
    S('z') = ZTPRE, S('j') = JPRE, S('t') = ZTPRE,
  }, { /* 1: l-prefixed */
    S('d') = LONG, S('i') = LONG,
    S('o') = ULONG, S('u') = ULONG, S('x') = ULONG, S('X') = ULONG,
    // S('e') = DBL, S('f') = DBL, S('g') = DBL, S('a') = DBL,
    // S('E') = DBL, S('F') = DBL, S('G') = DBL, S('A') = DBL,
    S('c') = INT, S('s') = PTR, S('n') = PTR,
    S('l') = LLPRE,
  }, { /* 2: ll-prefixed */
    S('d') = LLONG, S('i') = LLONG,
    S('o') = ULLONG, S('u') = ULLONG,
    S('x') = ULLONG, S('X') = ULLONG,
    S('n') = PTR,
  }, { /* 3: h-prefixed */
    S('d') = SHORT, S('i') = SHORT,
    S('o') = USHORT, S('u') = USHORT,
    S('x') = USHORT, S('X') = USHORT,
    S('n') = PTR,
    S('h') = HHPRE,
  }, { /* 4: hh-prefixed */
    S('d') = CHAR, S('i') = CHAR,
    S('o') = UCHAR, S('u') = UCHAR,
    S('x') = UCHAR, S('X') = UCHAR,
    S('n') = PTR,
  }, { /* 5: L-prefixed */
    // S('e') = LDBL, S('f') = LDBL, S('g') = LDBL, S('a') = LDBL,
    // S('E') = LDBL, S('F') = LDBL, S('G') = LDBL, S('A') = LDBL,
    S('n') = PTR,
  }, { /* 6: z- or t-prefixed (assumed to be same size) */
    S('d') = PDIFF, S('i') = PDIFF,
    S('o') = SIZET, S('u') = SIZET,
    S('x') = SIZET, S('X') = SIZET,
    S('n') = PTR,
  }, { /* 7: j-prefixed */
    S('d') = IMAX, S('i') = IMAX,
    S('o') = UMAX, S('u') = UMAX,
    S('x') = UMAX, S('X') = UMAX,
    S('n') = PTR,
  }
};

#define OOB(x) ((unsigned)(x)-'A' > 'z'-'A')

union arg
{
  uintmax_t i;
  // long double f;
  void *p;
};

static void pop_arg(union arg *arg, int type, va_list *ap)
{
  switch (type) {
         case PTR:  arg->p = va_arg(*ap, void *);
  break; case INT:  arg->i = va_arg(*ap, int);
  break; case UINT: arg->i = va_arg(*ap, unsigned int);
  break; case LONG: arg->i = va_arg(*ap, long);
  break; case ULONG:  arg->i = va_arg(*ap, unsigned long);
  break; case ULLONG: arg->i = va_arg(*ap, unsigned long long);
  break; case SHORT:  arg->i = (short)va_arg(*ap, int);
  break; case USHORT: arg->i = (unsigned short)va_arg(*ap, int);
  break; case CHAR: arg->i = (signed char)va_arg(*ap, int);
  break; case UCHAR:  arg->i = (unsigned char)va_arg(*ap, int);
  break; case LLONG:  arg->i = va_arg(*ap, long long);
  break; case SIZET:  arg->i = va_arg(*ap, size_t);
  break; case IMAX: arg->i = va_arg(*ap, intmax_t);
  break; case UMAX: arg->i = va_arg(*ap, uintmax_t);
  break; case PDIFF:  arg->i = va_arg(*ap, ptrdiff_t);
  break; case UIPTR:  arg->i = (uintptr_t)va_arg(*ap, void *);
  // break; case DBL:  arg->f = va_arg(*ap, double);
  // break; case LDBL: arg->f = va_arg(*ap, long double);
  }
}

static void out(FILE *f, const char *s, size_t l)
{
  if (!(f->flags & F_ERR)) __fwritex((void *)s, l, f);
}

static void pad(FILE *f, char c, int w, int l, int fl)
{
  char pad[256];
  if (fl & (LEFT_ADJ | ZERO_PAD) || l >= w) return;
  l = w - l;
  memset(pad, c, l > (int)sizeof(pad) ? sizeof(pad) : l);
  for (; l >= (int)sizeof(pad); l -= sizeof(pad))
    out(f, pad, sizeof(pad));
  out(f, pad, l);
}

static const char* xdigits = "0123456789ABCDEF";

static char *fmt_x(uintmax_t x, char *s, int lower)
{
  for (; x; x>>=4) *--s = xdigits[(x&15)]|lower;
  return s;
}

static char *fmt_o(uintmax_t x, char *s)
{
  for (; x; x>>=3) *--s = '0' + (x&7);
  return s;
}

static char *fmt_u(uintmax_t x, char *s)
{
  unsigned long y;
  for (   ; x>ULONG_MAX; x/=10) *--s = '0' + x%10;
  for (y=x;           y; y/=10) *--s = '0' + y%10;
  return s;
}

/* Do not override this check. The floating point printing code below
 * depends on the float.h constants being right. If they are wrong, it
 * may overflow the stack. */
#if LDBL_MANT_DIG == 53
typedef char compiler_defines_long_double_incorrectly[9-(int)sizeof(long double)];
#endif


static int getint(char **s) {
  int i;
  for (i=0; isdigit(**s); (*s)++) {
    if (i > INT_MAX/10 || **s-'0' > INT_MAX-10*i) i = -1;
    else i = 10*i + (**s-'0');
  }
  return i;
}

static int printf_core(FILE *f, const char *fmt, va_list *ap, union arg *nl_arg, int *nl_type)
{
  char *a, *z, *s=(char *)fmt;
  unsigned l10n=0, fl;
  int w, p, xp;
  union arg arg;
  int argpos;
  unsigned st, ps;
  int cnt=0, l=0;
  size_t i;
  char buf[sizeof(uintmax_t)*3+3+LDBL_MANT_DIG/4];
  const char *prefix;
  int t, pl;
  wchar_t wc[2], *ws;
  char mb[4];

  for (;;) {
    /* This error is only specified for snprintf, but since it's
     * unspecified for other forms, do the same. Stop immediately
     * on overflow; otherwise %n could produce wrong results. */
    if (l > INT_MAX - cnt) goto overflow;

    /* Update output count, end loop when fmt is exhausted */
    cnt += l;
    if (!*s) break;

    /* Handle literal text and %% format specifiers */
    for (a=s; *s && *s!='%'; s++);
    for (z=s; s[0]=='%' && s[1]=='%'; z++, s+=2);
    if (z-a > INT_MAX-cnt) goto overflow;
    l = z-a;
    if (f) out(f, a, l);
    if (l) continue;

    if (isdigit(s[1]) && s[2]=='$') {
      l10n=1;
      argpos = s[1]-'0';
      s+=3;
    } else {
      argpos = -1;
      s++;
    }

    /* Read modifier flags */
    for (fl=0; (unsigned)*s-' '<32 && (FLAGMASK&(1U<<*s-' ')); s++)
      fl |= 1U<<*s-' ';

    /* Read field width */
    if (*s=='*') {
      if (isdigit(s[1]) && s[2]=='$') {
        l10n=1;
        nl_type[s[1]-'0'] = INT;
        w = nl_arg[s[1]-'0'].i;
        s+=3;
      } else if (!l10n) {
        w = f ? va_arg(*ap, int) : 0;
        s++;
      } else goto inval;
      if (w<0) fl|=LEFT_ADJ, w=-w;
    } else if ((w=getint(&s))<0) goto overflow;

    /* Read precision */
    if (*s=='.' && s[1]=='*') {
      if (isdigit(s[2]) && s[3]=='$') {
        nl_type[s[2]-'0'] = INT;
        p = nl_arg[s[2]-'0'].i;
        s+=4;
      } else if (!l10n) {
        p = f ? va_arg(*ap, int) : 0;
        s+=2;
      } else goto inval;
      xp = (p>=0);
    } else if (*s=='.') {
      s++;
      p = getint(&s);
      xp = 1;
    } else {
      p = -1;
      xp = 0;
    }

    /* Format specifier state machine */
    st=0;
    do {
      if (OOB(*s)) goto inval;
      ps=st;
      st=states[st]S(*s++);
    } while (st-1<STOP);
    if (!st) goto inval;

    /* Check validity of argument type (nl/normal) */
    if (st==NOARG) {
      if (argpos>=0) goto inval;
    } else {
      if (argpos>=0) nl_type[argpos]=st, arg=nl_arg[argpos];
      else if (f) pop_arg(&arg, st, ap);
      else return 0;
    }

    if (!f) continue;

    z = buf + sizeof(buf);
    prefix = "-+   0X0x";
    pl = 0;
    t = s[-1];

    /* Transform ls,lc -> S,C */
    if (ps && (t&15)==3) t&=~32;

    /* - and 0 flags are mutually exclusive */
    if (fl & LEFT_ADJ) fl &= ~ZERO_PAD;

    switch(t) {
    case 'n':
      switch(ps) {
      case BARE: *(int *)arg.p = cnt; break;
      case LPRE: *(long *)arg.p = cnt; break;
      case LLPRE: *(long long *)arg.p = cnt; break;
      case HPRE: *(unsigned short *)arg.p = cnt; break;
      case HHPRE: *(unsigned char *)arg.p = cnt; break;
      case ZTPRE: *(size_t *)arg.p = cnt; break;
      case JPRE: *(uintmax_t *)arg.p = cnt; break;
      }
      continue;
    case 'p':
      p = MAX(p, (int)(2*sizeof(void*)));
      t = 'x';
      fl |= ALT_FORM;
    case 'x': case 'X':
      a = fmt_x(arg.i, z, t&32);
      if (arg.i && (fl & ALT_FORM)) prefix+=(t>>4), pl=2;
      if (0) {
    case 'o':
      a = fmt_o(arg.i, z);
      if ((fl&ALT_FORM) && p<z-a+1) p=z-a+1;
      } if (0) {
    case 'd': case 'i':
      pl=1;
      if (arg.i>INTMAX_MAX) {
        arg.i=-arg.i;
      } else if (fl & MARK_POS) {
        prefix++;
      } else if (fl & PAD_POS) {
        prefix+=2;
      } else pl=0;
    case 'u':
      a = fmt_u(arg.i, z);
      }
      if (xp && p<0) goto overflow;
      if (xp) fl &= ~ZERO_PAD;
      if (!arg.i && !p) {
        a=z;
        break;
      }
      p = MAX(p, z-a + !arg.i);
      break;
    case 'c':
      *(a=z-(p=1))=arg.i;
      fl &= ~ZERO_PAD;
      break;
    // case 'm':
    //   if (1) a = strerror(errno); else
    case 's':
      a = arg.p ? arg.p : "(null)";
      z = a + strnlen(a, p<0 ? INT_MAX : p);
      if (p<0 && *z) goto overflow;
      p = z-a;
      fl &= ~ZERO_PAD;
      break;
    case 'C':
      wc[0] = arg.i;
      wc[1] = 0;
      arg.p = wc;
      p = -1;
    case 'S':
      ws = arg.p;
      for (i=l=0; (int)i<p && *ws && (l=wctomb(mb, *ws++))>=0 && l<=(int)(p-i); i+=l);
      if (l<0) return -1;
      if (i > INT_MAX) goto overflow;
      p = i;
      pad(f, ' ', w, p, fl);
      ws = arg.p;
      for (i=0; i<0U+p && *ws && i+(l=wctomb(mb, *ws++))<=(size_t)p; i+=l)
        out(f, mb, l);
      pad(f, ' ', w, p, fl^LEFT_ADJ);
      l = w>p ? w : p;
      continue;

    }

    if (p < z-a) p = z-a;
    if (p > INT_MAX-pl) goto overflow;
    if (w < pl+p) w = pl+p;
    if (w > INT_MAX-cnt) goto overflow;

    pad(f, ' ', w, pl+p, fl);
    out(f, prefix, pl);
    pad(f, '0', w, pl+p, fl^ZERO_PAD);
    pad(f, '0', p, z-a, 0);
    out(f, a, z-a);
    pad(f, ' ', w, pl+p, fl^LEFT_ADJ);

    l = w;
  }

  if (f) return cnt;
  if (!l10n) return 0;

  for (i=1; i<=NL_ARGMAX && nl_type[i]; i++)
    pop_arg(nl_arg+i, nl_type[i], ap);
  for (; i<=NL_ARGMAX && !nl_type[i]; i++);
  if (i<=NL_ARGMAX) goto inval;
  return 1;

inval:
  errno = EINVAL;
  return -1;
overflow:
  errno = EOVERFLOW;
  return -1;
}


static int vfprintf(FILE *restrict f, const char *restrict fmt, va_list ap)
{
  va_list ap2;
  int nl_type[NL_ARGMAX+1] = {0};
  union arg nl_arg[NL_ARGMAX+1];
  unsigned char internal_buf[80], *saved_buf = 0;
  int olderr;
  int ret;

  /* the copy allows passing va_list* even if va_list is an array */
  va_copy(ap2, ap);
  if (printf_core(0, fmt, &ap2, nl_arg, nl_type) < 0) {
    va_end(ap2);
    return -1;
  }

  FLOCK(f);
  olderr = f->flags & F_ERR;
  if (f->mode < 1) f->flags &= ~F_ERR;
  if (!f->buf_size) {
    saved_buf = f->buf;
    f->buf = internal_buf;
    f->buf_size = sizeof internal_buf;
    f->wpos = f->wbase = f->wend = 0;
  }
  if (!f->wend && __towrite(f)) ret = -1;
  else ret = printf_core(f, fmt, &ap2, nl_arg, nl_type);
  if (saved_buf) {
    f->write(f, 0, 0);
    if (!f->wpos) ret = -1;
    f->buf = saved_buf;
    f->buf_size = 0;
    f->wpos = f->wbase = f->wend = 0;
  }
  if (f->flags & F_ERR) ret = -1;
  f->flags |= olderr;
  FUNLOCK(f);
  va_end(ap2);
  return ret;
}





// --- vsnprintf.c

struct cookie {
  char *s;
  size_t n;
};

static size_t sn_write(FILE *f, const unsigned char *s, size_t l)
{
  struct cookie *c = f->cookie;
  size_t k = MIN(c->n, (size_t)(f->wpos - f->wbase));
  if (k) {
    memcpy(c->s, f->wbase, k);
    c->s += k;
    c->n -= k;
  }
  k = MIN(c->n, l);
  if (k) {
    memcpy(c->s, s, k);
    c->s += k;
    c->n -= k;
  }
  *c->s = 0;
  f->wpos = f->wbase = f->buf;
  /* pretend to succeed, even if we discarded extra data */
  return l;
}

int vsnprintf(char *restrict s, size_t n, const char *restrict fmt, va_list ap)
{
  unsigned char buf[1];
  char dummy[1];
  struct cookie c = { .s = n ? s : dummy, .n = n ? n-1 : 0 };
  FILE f = {
    .lbf = EOF,
    .write = sn_write,
    .lock = -1,
    .buf = buf,
    .cookie = &c,
  };
  if (n > INT_MAX) {
    errno = EOVERFLOW;
    return -1;
  }
  *c.s = 0;
  return vfprintf(&f, fmt, ap);
}


// --- snprintf.c

int snprintf(char* restrict s, usize n, const char* restrict fmt, ...) {
  int ret;
  va_list ap;
  va_start(ap, fmt);
  ret = vsnprintf(s, n, fmt, ap);
  va_end(ap);
  return ret;
}

#endif // defined(__wasm__) && !defined(__wasi__)
