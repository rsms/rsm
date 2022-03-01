// RSM virtual machine API
// SPDX-License-Identifier: Apache-2.0
#pragma once
#if defined(__wasm__) && !defined(__wasi__) && defined(__cplusplus)
  #define RSMAPI extern "C"
#elif defined(__wasm__) && !defined(__wasi__)
  #define RSMAPI // not exported in pure WASM builds
#elif defined(__cplusplus)
  #define RSMAPI extern "C" __attribute__((visibility("default")))
#else
  #define RSMAPI __attribute__((visibility("default")))
#endif
#if !defined(RSM_NO_LIBC) && defined(__wasm__) && !defined(__wasi__)
  #define RSM_NO_LIBC
#endif
#ifndef RSM_NO_INT_DEFS
  #ifndef __cplusplus
    typedef _Bool bool;
    #define true  ((bool)1)
    #define false ((bool)0)
  #endif
  typedef unsigned char      u8;
  typedef unsigned int       u32;
  typedef unsigned long long u64;
  typedef signed long        isize;
  typedef unsigned long      usize;
#endif
#ifndef __has_attribute
  #define __has_attribute(x)  0
#endif
#ifndef __has_feature
  #define __has_feature(x)  0
#endif
#ifndef NULL
  #define NULL ((void*)0)
#endif
#if defined(__clang__) && __has_feature(nullability)
  #ifndef nullable
    #define nullable _Nullable
  #endif
  #define RSM_ASSUME_NONNULL_BEGIN                                                \
    _Pragma("clang diagnostic push")                                              \
    _Pragma("clang diagnostic ignored \"-Wnullability-completeness\"")            \
    _Pragma("clang diagnostic ignored \"-Wnullability-inferred-on-nested-type\"") \
    _Pragma("clang assume_nonnull begin")
  #define RSM_ASSUME_NONNULL_END    \
    _Pragma("clang diagnostic pop") \
    _Pragma("clang assume_nonnull end")
#else
  #ifndef nullable
    #define nullable
  #endif
  #define RSM_ASSUME_NONNULL_BEGIN
  #define RSM_ASSUME_NONNULL_END
#endif
#if __has_attribute(__packed__)
  #define RSM_END_ENUM(NAME) __attribute__((__packed__)); \
    _Static_assert(sizeof(enum NAME) <= sizeof(NAME), "too many " #NAME " values");
#else
  #define RSM_END_ENUM(NAME) ;
#endif
#if __has_attribute(warn_unused_result)
  #define RSM_WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#else
  #define RSM_WARN_UNUSED_RESULT
#endif
#if __has_attribute(malloc)
  #define RSM_ATTR_MALLOC __attribute__((malloc))
#else
  #define RSM_ATTR_MALLOC
#endif

// --------------------------------------------------------------------------------------
// begin main interface
RSM_ASSUME_NONNULL_BEGIN

// Instructions are fixed-size at 32 bits long, little endian.
// PC and jump- & branch destinations are expressed in #instructions rather than bytes.
// There is room for 256 operations and 32+32 (int+float) registers (8 bit OP, 5 bit reg)
// Most instructions accept reg or immediate (i bit is set) as last argument.
typedef u32 rinstr;
//
//        ┌─────────────────┬─────────┬─────────┬─────────┬───────────────┐
//  bit   │3 3 2 2 2 2 2 2 2│2 2 2 1 1│1 1 1 1 1│1 1 1    │               │
//        │1 0 9 8 7 6 5 4 3│2 1 0 9 8│7 6 5 4 3│2 1 0 9 8│7 6 5 4 3 2 1 0│
//        ├───────────────┬─┼─────────┼─────────┼─────────┼───────────────┤
//  ABCD  │         D (8) │i│  C (5)  │  B (5)  │  A (5)  │     OP (8)    │
//        ├───────────────┴─┴───────┬─┼─────────┼─────────┼───────────────┤
//  ABCw  │                  C (13) │i│  B (5)  │  A (5)  │     OP (8)    │
//        ├─────────────────────────┴─┴───────┬─┼─────────┼───────────────┤
//  ABw   │                            B (18) │i│  A (5)  │     OP (8)    │
//        ├───────────────────────────────────┴─┴───────┬─┼───────────────┤
//  Aw    │                                      A (23) │i│     OP (8)    │
//        └─────────────────────────────────────────────┴─┴───────────────┘
//
// encoding legend:
//   enc     arguments
//   _       (none)
//   A       R(A)
//   Au      R(A) or immediate unsigned value
//   As      R(A) or immediate signed value
//   AB      R(A), R(B)
//   ABu     R(A), R(B) or immediate unsigned value
//   ABs     R(A), R(B) or immediate signed value
//   ABC     R(A), R(B), R(C)
//   ABCu    R(A), R(B), R(C) or immediate unsigned value
//   ABCs    R(A), R(B), R(C) or immediate signed value
//   ABCD    R(A), R(B), R(C), R(D)
//   ABCDu   R(A), R(B), R(C), R(D) or immediate unsigned value
//   ABCDs   R(A), R(B), R(C), R(D) or immediate signed value
// Changing instruction encoding? Remember to also update eval.c and genop in asm.c
#define RSM_FOREACH_OP(_) /* _(name, arguments, asmname, semantics) */ \
_( COPY  , ABu  , "copy"  /* R(A) = {R(B),B} aka "move" */)\
_( LOADK , ABu  , "loadk" /* R(A) = K(B) -- load constant  */)\
\
_( ADD   , ABCu , "add"   /* R(A) = R(B) + {R(C),Cu}                                */)\
_( SUB   , ABCu , "sub"   /* R(A) = R(B) - {R(C),Cu}                                */)\
_( MUL   , ABCu , "mul"   /* R(A) = R(B) * {R(C),Cu}                                */)\
_( DIV   , ABCu , "div"   /* R(A) = R(B) / {R(C),Cu}                                */)\
_( MOD   , ABCu , "mod"   /* R(A) = R(B) % {R(C),Cu}                                */)\
_( AND   , ABCu , "and"   /* R(A) = R(B) & {R(C),Cu}                                */)\
_( OR    , ABCu , "or"    /* R(A) = R(B) | {R(C),Cu}                                */)\
_( XOR   , ABCu , "xor"   /* R(A) = R(B) ^ {R(C),Cu}                                */)\
_( SHL   , ABCu , "shl"   /* R(A) = R(B) << {R(C),Cu}                               */)\
_( SHRS  , ABCs , "shrs"  /* R(A) = R(B) >> {R(C),Cs} sign-replicating (arithmetic) */)\
_( SHRU  , ABCu , "shru"  /* R(A) = R(B) >> {R(C),Cu} zero-replicating (logical)    */)\
\
_( CMPEQ , ABCu , "cmpeq" /* R(A) = R(B) == {R(C),Cu} */)\
_( CMPLT , ABCu , "cmplt" /* R(A) = R(B) < {R(C),Cu}  */)\
_( CMPGT , ABCu , "cmpgt" /* R(A) = R(B) > {R(C),Cu}  */)\
\
_( BRZ   , ABs  , "brz"   /* if R(A)==0 goto PC+{R(B),Bs} */)\
_( BRNZ  , ABs  , "brnz"  /* if R(A)!=0 goto PC+{R(B),Bs} */)\
\
_( CALL  , Au   , "call"  /* R0...R7 = call PC={R(A),Au} (R0...R7) */)\
_( SCALL , Au   , "scall" /* R0...R7 = system_call {R(A),Au} (R0...R7) */)\
  /* TODO: would be nice to be able to pass multiple imms to SCALL \
     e.g. "scall PUTC 'h'"; a new encoding AuBu maybe? */          \
\
_( RET   , _    , "ret" /* return */)\
// end RSM_FOREACH_OP

// opcode test macros. Update when adding affected opcodes
#define RSM_OP_IS_BR(op) (rop_BRZ <= (op) && (op) <= rop_BRNZ)

// size and position of instruction arguments
#define RSM_SIZE_OP  8
#define RSM_SIZE_i   1  /* immediate flag */
#define RSM_SIZE_A   5
#define RSM_SIZE_B   5
#define RSM_SIZE_C   5
#define RSM_SIZE_D   9
#define RSM_SIZE_Dw  (RSM_SIZE_D - RSM_SIZE_i)
#define RSM_SIZE_Cw  (RSM_SIZE_C + RSM_SIZE_D - RSM_SIZE_i)
#define RSM_SIZE_Bw  (RSM_SIZE_B + RSM_SIZE_C + RSM_SIZE_D - RSM_SIZE_i)
#define RSM_SIZE_Aw  (RSM_SIZE_A + RSM_SIZE_B + RSM_SIZE_C + RSM_SIZE_D - RSM_SIZE_i)

#define RSM_MAX_Au   0x7fffff /* 2^23 - 1 */
#define RSM_MAX_Bu   0x3ffff  /* 2^18 - 1 */
#define RSM_MAX_Cu   0x1fff   /* 2^13 - 1 */
#define RSM_MAX_Du   0xff     /* 2^8  - 1 */
#define RSM_MAX_As   (RSM_MAX_Au >> 1)
#define RSM_MAX_Bs   (RSM_MAX_Bu >> 1)
#define RSM_MAX_Cs   (RSM_MAX_Cu >> 1)
#define RSM_MAX_Ds   (RSM_MAX_Du >> 1)
#define RSM_MIN_As   (-RSM_MAX_As - 1)
#define RSM_MIN_Bs   (-RSM_MAX_Bs - 1)
#define RSM_MIN_Cs   (-RSM_MAX_Cs - 1)
#define RSM_MIN_Ds   (-RSM_MAX_Ds - 1)

#define RSM_POS_A    RSM_SIZE_OP
#define RSM_POS_Aw   (RSM_POS_A + RSM_SIZE_i)
#define RSM_POS_B    (RSM_POS_A + RSM_SIZE_A)
#define RSM_POS_Bw   (RSM_POS_B + RSM_SIZE_i)
#define RSM_POS_C    (RSM_POS_B + RSM_SIZE_B)
#define RSM_POS_Cw   (RSM_POS_C + RSM_SIZE_i)
#define RSM_POS_D    (RSM_POS_C + RSM_SIZE_C)
#define RSM_POS_Dw   (RSM_POS_D + RSM_SIZE_i)

// u32 RSM_GET_ARGN(rinstr, uint pos, uint size)
// rinstr RSM_SET_ARGN(rinstr, uint pos, uint size, uint val)
#define RSM_MASK1(n,p)  ( ( ~( (~(rinstr)0) << (n) ) ) << (p) ) /* n 1 bits at position p */
#define RSM_MASK0(n,p)  (~RSM_MASK1(n,p))  /* n 0 bits at position p */
#define RSM_GET_ARGN(i,pos,size) ((u32)( ((i) >> (pos)) & RSM_MASK1(size,0) ))
#define RSM_SET_ARGN(i,pos,size,v) \
  ( ((i) & RSM_MASK0(size,pos)) | ( (((rinstr)v) << pos) & RSM_MASK1(size,pos)) )

// rop RSM_GET_OP(rinstr) -- get opcode
// u32 RSM_GET_Ar(rinstr) -- get register-sized value in A
// u32 RSM_GET_Au(rinstr) -- get full-size unsigned value in A
// i32 RSM_GET_As(rinstr) -- get full-size signed value in A
#define RSM_GET_OP(i)  ((rop)( RSM_GET_ARGN(i,0,RSM_SIZE_OP) ))

#define RSM_GET_A(i)   RSM_GET_ARGN(i, RSM_POS_A, RSM_SIZE_A)
#define RSM_GET_Ai(i)  RSM_GET_ARGN(i, RSM_POS_A, 1)
#define RSM_GET_Au(i)  RSM_GET_ARGN(i, RSM_POS_Aw, RSM_SIZE_Aw)
#define RSM_GET_As(i)  ((int)(RSM_GET_Au(i) - (RSM_MAX_Au / 2)))

#define RSM_GET_B(i)   RSM_GET_ARGN(i, RSM_POS_B, RSM_SIZE_B)
#define RSM_GET_Bi(i)  RSM_GET_ARGN(i, RSM_POS_B, 1)
#define RSM_GET_Bu(i)  RSM_GET_ARGN(i, RSM_POS_Bw, RSM_SIZE_Bw)
#define RSM_GET_Bs(i)  ((int)(RSM_GET_Bu(i) - (RSM_MAX_Bu / 2)))

#define RSM_GET_C(i)   RSM_GET_ARGN(i, RSM_POS_C, RSM_SIZE_C)
#define RSM_GET_Ci(i)  RSM_GET_ARGN(i, RSM_POS_C, 1)
#define RSM_GET_Cu(i)  RSM_GET_ARGN(i, RSM_POS_Cw, RSM_SIZE_Cw)
#define RSM_GET_Cs(i)  ((int)(RSM_GET_Cu(i) - (RSM_MAX_Cu / 2)))

#define RSM_GET_D(i)   RSM_GET_ARGN(i, RSM_POS_D, RSM_SIZE_D)
#define RSM_GET_Di(i)  RSM_GET_ARGN(i, RSM_POS_D, 1)
#define RSM_GET_Du(i)  RSM_GET_ARGN(i, RSM_POS_Dw, RSM_SIZE_Dw)
#define RSM_GET_Ds(i)  ((int)(RSM_GET_Du(i) - (RSM_MAX_Du / 2)))

// rinstr RSM_SET_Ar(rinstr, rop op)    -- copy of in with opcode op
// rinstr RSM_SET_Ar(rinstr, int regno) -- copy of in with A set to register number
// rinstr RSM_SET_Au(rinstr, u32 val)   -- copy of in with A set to full-size unsigned value
// rinstr RSM_SET_As(rinstr, i32 sval)  -- copy of in with A set to full-size signed value
#define RSM_SET_OP(i,v)  RSM_SET_ARGN(i, 0,          RSM_SIZE_OP, v)
#define RSM_SET_A(i,v)   RSM_SET_ARGN(i, RSM_POS_A,  RSM_SIZE_A,  v)
#define RSM_SET_Au(i,v)  RSM_SET_ARGN(i, RSM_POS_Aw, RSM_SIZE_Aw, v)
#define RSM_SET_As(i,v)  RSM_SET_Au(i, ((rinstr)(v)) + (RSM_MAX_Au / 2))
#define RSM_SET_B(i,v)   RSM_SET_ARGN(i, RSM_POS_B,  RSM_SIZE_B,  v)
#define RSM_SET_Bu(i,v)  RSM_SET_ARGN(i, RSM_POS_Bw, RSM_SIZE_Bw, v)
#define RSM_SET_Bs(i,v)  RSM_SET_Bu(i, ((rinstr)(v)) + (RSM_MAX_Bu / 2))
#define RSM_SET_C(i,v)   RSM_SET_ARGN(i, RSM_POS_C,  RSM_SIZE_C,  v)
#define RSM_SET_Cu(i,v)  RSM_SET_ARGN(i, RSM_POS_Cw, RSM_SIZE_Cw, v)
#define RSM_SET_Cs(i,v)  RSM_SET_Cu(i, ((rinstr)(v)) + (RSM_MAX_Cu / 2))
#define RSM_SET_D(i,v)   RSM_SET_ARGN(i, RSM_POS_D,  RSM_SIZE_D,  v)
#define RSM_SET_Du(i,v)  RSM_SET_ARGN(i, RSM_POS_Dw, RSM_SIZE_Dw, v)

#define RSM_MAKE_ABCD(op,a,b,c,d) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A ) \
  | ( ((rinstr)b) << RSM_POS_B ) \
  | ( ((rinstr)c) << RSM_POS_C ) \
  | ( ((rinstr)d) << RSM_POS_Dw ) )
#define RSM_MAKE_ABCDu(op,a,b,c,d) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A ) \
  | ( ((rinstr)b) << RSM_POS_B ) \
  | ( ((rinstr)c) << RSM_POS_C ) \
  | (1 << RSM_POS_C) | ( ((rinstr)d) << RSM_POS_Dw ) )
#define RSM_MAKE_ABC(op,a,b,c) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A ) \
  | ( ((rinstr)b) << RSM_POS_B ) \
  | ( ((rinstr)c) << RSM_POS_Cw ) )
#define RSM_MAKE_ABCu(op,a,b,c) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A ) \
  | ( ((rinstr)b) << RSM_POS_B ) \
  | (1 << RSM_POS_C) | ( ((rinstr)c) << RSM_POS_Cw ) )
#define RSM_MAKE_AB(op,a,b) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A  ) \
  | ( ((rinstr)b) << RSM_POS_Bw ) )
#define RSM_MAKE_ABu(op,a,b) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A ) \
  | (1 << RSM_POS_B) | ( ((rinstr)b) << RSM_POS_Bw ) )
#define RSM_MAKE_A(op,a) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_Aw ) )
#define RSM_MAKE_Au(op,a) ( ((rinstr)op) \
  | (1 << RSM_POS_A) | ( ((rinstr)a) << RSM_POS_Aw ) )
#define RSM_MAKE__(op) ((rinstr)op)

#define RSM_MAKE_As(op,a)          RSM_MAKE_Au(op,((rinstr)(a)) + (RSM_MAX_Au / 2))
#define RSM_MAKE_ABs(op,a,b)       RSM_MAKE_ABu(op,a,((rinstr)(b)) + (RSM_MAX_Bu / 2))
#define RSM_MAKE_ABCs(op,a,b,c)    RSM_MAKE_ABCu(op,a,b,((rinstr)(c)) + (RSM_MAX_Cu / 2))
#define RSM_MAKE_ABCDs(op,a,b,c,d) RSM_MAKE_ABCDu(op,a,b,c,((rinstr)(d)) + (RSM_MAX_Du / 2))

// rop, rop_* -- opcode
typedef u8 rop;
enum rop {
  #define _(name, ...) rop_##name,
  RSM_FOREACH_OP(_)
  #undef _
  RSM_OP_COUNT,
} RSM_END_ENUM(rop)

// rmem is a memory allocator
typedef struct rmem rmem;
struct rmem {
  // a(s,NULL,0,>0) = new, a(s,p,>0,>0) = resize, a(s,p,>0,0) = free.
  void* nullable (*a)(void* state, void* nullable p, usize oldsize, usize newsize);
  void* state;
};

// rdiag is a diagnostic report
typedef struct rdiag rdiag;
struct rdiag {
  int         code;      // error code (1=error, 0=warning)
  const char* msg;       // descriptive message including "srcname:line:col: type:"
  const char* msgshort;  // short descriptive message without source location
  const char* srcname;   // eg filename
  u32         line, col; // origin (0 if unknown or not line-specific)
};
// rdiaghandler is called with a diagnostict report.
// Return false to stop the process (e.g. stop assembling.)
typedef bool(*rdiaghandler)(const rdiag*, void* nullable userdata);

// rcomp represents a compilation session
typedef struct rcomp rcomp;
struct rcomp {
  rmem           mem;         // memory allocator
  const char*    srcdata;     // input source bytes
  usize          srclen;      // length of srcdata
  const char*    srcname;     // symbolic name of source (e.g. filename)
  u32            errcount;    // number of errors reported
  rdiag          diag;        // last diagnostic report
  rdiaghandler   diaghandler; // diagnostic report callback
  void* nullable userdata;    // passed along to diaghandler
  // internal fields
  char _diagmsg[128];     // storage for diag.msg
  bool _stop;             // negated diaghandler return value
  void* nullable _gstate; // reusable internal codegen state
};

// rop_name returns the name of an opcode
RSMAPI const char* rop_name(rop);

// rsm_compile compiles assembly source text into vm bytecode.
// Uses c->mem for temporary storage, allocates *resp in resm.
// c can be reused with multiple calls.
// Returns the number of instructions at *resp on success, or 0 on failure.
RSMAPI usize rsm_compile(rcomp* c, rmem resm, rinstr** resp);

// rcomp_dispose frees resources of a compilation session structure
void rcomp_dispose(rcomp* c);

// rsm_vmexec executes a program, starting with instruction inv[0]
RSMAPI void rsm_vmexec(u64* iregs, u32* inv, usize inlen);

// rsm_fmtprog formats an array of instructions ip as "assembly" text to buf.
// It writes at most bufcap-1 of the characters to the output buf (the bufcap'th
// character then gets the terminating '\0'). If the return value is greater than or
// equal to the bufcap argument, buf was too short and some of the characters were
// discarded. The output is always null-terminated, unless size is 0.
// Returns the number of characters that would have been printed if bufcap was
// unlimited (not including the final `\0').
RSMAPI usize rsm_fmtprog(char* buf, usize bufcap, rinstr* nullable ip, usize ilen);
RSMAPI usize rsm_fmtinstr(char* buf, usize bufcap, rinstr in);

// RMEM_MK_MIN is the minimum size for rmem_mk*alloc functions
#define RMEM_MK_MIN (sizeof(void*)*4)

// rmem_mkbufalloc creates an allocator that uses size-RMEM_MK_MIN bytes from buf.
// The address buf and size may be adjusted to pointer-size alignment.
RSMAPI rmem rmem_mkbufalloc(void* buf, usize size);

// rmem_initbufalloc initializes an allocator that uses size bytes from buf in astate.
// The address buf and size may be adjusted to pointer-size alignment.
RSMAPI rmem rmem_initbufalloc(void* astate[4], void* buf, usize size);

// rmem_mkvmalloc creates an allocator backed by a slab of system-managed virtual memory.
// If size=0, a very large allocation is created (~4GB).
// On failure, the returned allocator is {NULL,NULL}.
RSMAPI rmem rmem_mkvmalloc(usize size);

// rmem_freealloc frees an allocator created with a rmem_mk*alloc function
RSMAPI void rmem_freealloc(rmem m);

// memory allocation interface
static void* nullable rmem_alloc(rmem m, usize size);
static void* nullable rmem_resize(rmem m, void* nullable p, usize oldsize, usize newsize);
static void rmem_free(rmem m, void* p, usize size);

RSM_ATTR_MALLOC RSM_WARN_UNUSED_RESULT
inline static void* nullable rmem_alloc(rmem m, usize size) {
  return m.a(m.state, NULL, 0, size);
}
RSM_ATTR_MALLOC RSM_WARN_UNUSED_RESULT
inline static void* nullable rmem_resize(rmem m, void* nullable p, usize oldsize, usize newsize) {
  return m.a(m.state, p, oldsize, newsize);
}
inline static void rmem_free(rmem m, void* p, usize size) {
  #if __has_attribute(unused)
  __attribute__((unused))
  #endif
  void* _ = m.a(m.state,p,size,0);
}

RSM_ASSUME_NONNULL_END
