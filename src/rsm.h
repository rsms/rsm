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
  #ifdef __INTPTR_TYPE__
    typedef __INTPTR_TYPE__   intptr;
    typedef __UINTPTR_TYPE__  uintptr;
  #else
    typedef signed long       intptr;
    typedef unsigned long     uintptr;
  #endif
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
typedef u32 rin_t;
//
//        ┌───────────────┬─────────┬─────────┬─────────┬─┬───────────────┐
//  bit   │3 3 2 2 2 2 2 2│2 2 2 2 1│1 1 1 1 1│1 1 1 1  │ │               │
//        │1 0 9 8 7 6 5 4│3 2 1 0 9│8 7 6 5 4│3 2 1 0 9│8│7 6 5 4 3 2 1 0│
//        ├───────────────┼─────────┼─────────┼─────────┼─┼───────────────┤
//  ABCD  │         D (8) │  C (5)  │  B (5)  │  A (5)  │i│     OP (8)    │
//        ├───────────────┴─────────┼─────────┼─────────┼─┼───────────────┤
//  ABCw  │                  C (13) │  B (5)  │  A (5)  │i│     OP (8)    │
//        ├─────────────────────────┴─────────┼─────────┼─┼───────────────┤
//  ABw   │                            B (18) │  A (5)  │i│     OP (8)    │
//        ├───────────────────────────────────┴─────────┼─┼───────────────┤
//  Aw    │                                      A (23) │i│     OP (8)    │
//        └─────────────────────────────────────────────┴─┴───────────────┘
//
// encoding legend:
//   enc     arguments
//   _       (none)
//   A       R(A)
//   Au      R(A) or immediate unsigned value
//   As      R(A) or immediate signed value
//   ABv     R(A), with Bu immediate trailing u32 values
//   AB      R(A), R(B)
//   ABu     R(A), R(B) or immediate unsigned value
//   ABs     R(A), R(B) or immediate signed value
//   ABC     R(A), R(B), R(C)
//   ABCu    R(A), R(B), R(C) or immediate unsigned value
//   ABCs    R(A), R(B), R(C) or immediate signed value
//   ABCD    R(A), R(B), R(C), R(D)
//   ABCDu   R(A), R(B), R(C), R(D) or immediate unsigned value
//   ABCDs   R(A), R(B), R(C), R(D) or immediate signed value
// result types:
//   reg   Result in register
//   mem   Result in memory
//   nil   No result, or implicit register result (i.e. calls)
// Changing instruction encoding? Remember to also update vm.c, fmt.c and compile.c
#define RSM_FOREACH_OP(_) /* _(name, arguments, result, asmname, semantics) */ \
_( COPY   , ABu  , reg , "copy"    /* RA = Bu -- aka "move" */)\
_( COPYV  , ABv  , reg , "copyv"   /* RA = instr[...]; PC+=Bu */)\
_( LOAD   , ABCs , reg , "load"    /* RA = mem[RB + Cs : 8]                           */)\
_( LOAD4U , ABCs , reg , "load4u"  /* RA = mem[RB + Cs : 4] -- zero-extend i32 to i64 */)\
_( LOAD4S , ABCs , reg , "load4s"  /* RA = mem[RB + Cs : 4] -- sign-extend i32 to i64 */)\
_( LOAD2U , ABCs , reg , "load2u"  /* RA = mem[RB + Cs : 2] -- zero-extend i16 to i64 */)\
_( LOAD2S , ABCs , reg , "load2s"  /* RA = mem[RB + Cs : 2] -- sign-extend i16 to i64 */)\
_( LOAD1U , ABCs , reg , "load1u"  /* RA = mem[RB + Cs : 1] -- zero-extend i8 to i64  */)\
_( LOAD1S , ABCs , reg , "load1s"  /* RA = mem[RB + Cs : 1] -- sign-extend i8 to i64  */)\
_( STORE  , ABCs , mem , "store"   /* mem[RB + Cs : 8] = RA                           */)\
_( STORE4 , ABCs , mem , "store4"  /* mem[RB + Cs : 4] = RA -- wrap i64 to i32        */)\
_( STORE2 , ABCs , mem , "store2"  /* mem[RB + Cs : 2] = RA -- wrap i64 to i16        */)\
_( STORE1 , ABCs , mem , "store1"  /* mem[RB + Cs : 1] = RA -- wrap i64 to i8         */)\
_( PUSH   , Au   , mem , "push"    /* SP -= 8; mem[SP] = Au                           */)\
_( POP    , A    , reg , "pop"     /* A = mem[SP]; SP += 8                            */)\
\
_( ADD   , ABCu , reg , "add"   /* RA = RB + Cu                                     */)\
_( SUB   , ABCu , reg , "sub"   /* RA = RB - Cu                                     */)\
_( MUL   , ABCu , reg , "mul"   /* RA = RB * Cu                                     */)\
_( DIV   , ABCu , reg , "div"   /* RA = RB / Cu                                     */)\
_( MOD   , ABCu , reg , "mod"   /* RA = RB % Cu                                     */)\
_( AND   , ABCu , reg , "and"   /* RA = RB & Cu                                     */)\
_( OR    , ABCu , reg , "or"    /* RA = RB | Cu                                     */)\
_( XOR   , ABCu , reg , "xor"   /* RA = RB ^ Cu                                     */)\
_( SHL   , ABCu , reg , "shl"   /* RA = RB << Cu                                    */)\
_( SHRS  , ABCu , reg , "shrs"  /* RA = RB >> Cu -- sign-replicating (arithmetic)   */)\
_( SHRU  , ABCu , reg , "shru"  /* RA = RB >> Cu -- zero-replicating (logical)      */)\
_( BINV  , ABu  , reg , "binv"  /* RA = ~Bu      -- bitwise complement, invert bits */)\
\
_( EQ    , ABCu , reg , "eq"   /* RA = RB == Cu */)\
_( NEQ   , ABCu , reg , "neq"  /* RA = RB != Cu */)\
_( LTU   , ABCu , reg , "ltu"  /* RA = RB <  Cu */)\
_( LTS   , ABCs , reg , "lts"  /* RA = RB <  Cs */)\
_( LTEU  , ABCu , reg , "lteu" /* RA = RB <= Cu */)\
_( LTES  , ABCs , reg , "ltes" /* RA = RB <= Cs */)\
_( GTU   , ABCu , reg , "gtu"  /* RA = RB >  Cu */)\
_( GTS   , ABCs , reg , "gts"  /* RA = RB >  Cs */)\
_( GTEU  , ABCu , reg , "gteu" /* RA = RB >= Cu */)\
_( GTES  , ABCs , reg , "gtes" /* RA = RB >= Cs */)\
\
_( IF    , ABs  , nil , "if"    /* if RA!=0 PC += Bs         */)\
_( IFZ   , ABs  , nil , "ifz"   /* if RA==0 PC += Bs         */)\
_( CALL  , Au   , nil , "call"  /* R0...R7 = push(PC); PC=Au */)\
_( JUMP  , Au   , nil , "jump"  /* PC = Au                   */)\
_( RET   , _    , nil , "ret"   /* PC = pop()                */)\
\
_( SCALL , Au    , nil , "scall" /* R0...R7 = system_call(Au) */)\
_( WRITE , ABCDu , reg , "write" /* RA = write dstaddr=RB size=RC fd=Du */)\
_( READ  , ABCDu , reg , "read"  /* RA = read  srcaddr=RB size=RC fd=Du */)\
_( DEVOPEN , ABu , reg , "devopen" /* RA = devaddr = device(Bu) */)\
_( MCOPY , ABCu  , mem , "mcopy" /* mem[RA:Cu] = mem[RB:Cu] */)\
_( MCMP  , ABCDu , reg , "mcmp"  /* RA = mem[RB:Du] <> mem[RC:Du] */)\
\
// end RSM_FOREACH_OP

// opcode test macros. Update when adding affected opcodes
#define RSM_OP_IS_BR(op)  (rop_IF <= (op) && (op) <= rop_IFZ)
#define RSM_OP_ACCEPTS_PC_ARG(op)  (rop_IF <= (op) && (op) <= rop_JUMP)

// size and position of instruction arguments
#define RSM_SIZE_OP  8
#define RSM_SIZE_i   1  /* immediate flag */
#define RSM_SIZE_A   5
#define RSM_SIZE_B   5
#define RSM_SIZE_C   5
#define RSM_SIZE_D   8
#define RSM_SIZE_Di  RSM_SIZE_D
#define RSM_SIZE_Ci  (RSM_SIZE_C + RSM_SIZE_D)
#define RSM_SIZE_Bi  (RSM_SIZE_B + RSM_SIZE_C + RSM_SIZE_D)
#define RSM_SIZE_Ai  (RSM_SIZE_A + RSM_SIZE_B + RSM_SIZE_C + RSM_SIZE_D)

#define RSM_POS_i    RSM_SIZE_OP
#define RSM_POS_A    (RSM_POS_i + RSM_SIZE_i)
#define RSM_POS_B    (RSM_POS_A + RSM_SIZE_A)
#define RSM_POS_C    (RSM_POS_B + RSM_SIZE_B)
#define RSM_POS_D    (RSM_POS_C + RSM_SIZE_C)

#define RSM_MAX_Au   ((1 << RSM_SIZE_Ai) - 1) /* (2^23 - 1) = 8,388,607  0x7fffff */
#define RSM_MAX_Bu   ((1 << RSM_SIZE_Bi) - 1) /* (2^18 - 1) =   262,143   0x3ffff */
#define RSM_MAX_Cu   ((1 << RSM_SIZE_Ci) - 1) /* (2^13 - 1) =     8,191    0x1fff */
#define RSM_MAX_Du   ((1 << RSM_SIZE_Di) - 1) /* (2^8  - 1) =       255      0xff */
#define RSM_MAX_As   (RSM_MAX_Au >> 1) /*                     4,194,303  0x3fffff */
#define RSM_MAX_Bs   (RSM_MAX_Bu >> 1) /*                       131,071   0x1ffff */
#define RSM_MAX_Cs   (RSM_MAX_Cu >> 1) /*                         4,095     0xfff */
#define RSM_MAX_Ds   (RSM_MAX_Du >> 1) /*                           127      0x7f */
#define RSM_MIN_As   (-RSM_MAX_As - 1) /*                    -4,194,304 -0x400000 */
#define RSM_MIN_Bs   (-RSM_MAX_Bs - 1) /*                      -131,072  -0x20000 */
#define RSM_MIN_Cs   (-RSM_MAX_Cs - 1) /*                        -4,096   -0x1000 */
#define RSM_MIN_Ds   (-RSM_MAX_Ds - 1) /*                          -128     -0x80 */

#define RSM_NREGS   32 /* total number of registers */
#define RSM_MAX_REG (RSM_NREGS - 1) /* == SP */

// calling convention
#define RSM_NARGREGS 8 /* number of regs to use for arguments (R0...R{RSM_NARGREGS}-1) */
#define RSM_NTMPREGS 19 /* R0…(RSM_NTMPREGS-1) are callee-owned/caller-save */

// u32 RSM_GET_ARGN(rin_t, uint pos, uint size)
// rin_t RSM_SET_ARGN(rin_t, uint pos, uint size, uint val)
#define RSM_MASK1(n,p) \
  ( ( ~( (~(rin_t)0) << (n) ) ) << (p) ) /* n 1 bits at position p */
#define RSM_MASK0(n,p) \
  (~RSM_MASK1(n,p))  /* n 0 bits at position p */
#define RSM_GET_ARGN(i,pos,size) \
  ((u32)( ((i) >> (pos)) & RSM_MASK1(size,0) ))
#define RSM_SET_ARGN(i,pos,size,v) \
  ( ((i) & RSM_MASK0(size,pos)) | ( (((rin_t)v) << pos) & RSM_MASK1(size,pos)) )

// rop RSM_GET_OP(rin_t)  -- get opcode
// rop RSM_GET_OPi(rin_t) -- get opcode with immediate flag
// bool RSM_GET_i(rin_t)  -- get immediate flag
// u32 RSM_GET_A(rin_t)   -- get register number
// u32 RSM_GET_Au(rin_t)  -- get full-size unsigned immediate value
// i32 RSM_GET_As(rin_t)  -- get full-size signed immediate value
#define RSM_GET_OP(i)  ((rop)( (rin_t)(i) & RSM_MASK1(RSM_SIZE_OP,0) ))
#define RSM_GET_OPi(i) RSM_GET_ARGN(i, 0, RSM_SIZE_OP + RSM_SIZE_i) // OP and i
#define RSM_GET_i(i)   RSM_GET_ARGN(i, RSM_POS_i, RSM_SIZE_i)
#define RSM_GET_A(i)   RSM_GET_ARGN(i, RSM_POS_A, RSM_SIZE_A)
#define RSM_GET_B(i)   RSM_GET_ARGN(i, RSM_POS_B, RSM_SIZE_B)
#define RSM_GET_C(i)   RSM_GET_ARGN(i, RSM_POS_C, RSM_SIZE_C)
#define RSM_GET_D(i)   RSM_GET_ARGN(i, RSM_POS_D, RSM_SIZE_D)
#define RSM_GET_Au(i)  RSM_GET_ARGN(i, RSM_POS_A, RSM_SIZE_Ai)
#define RSM_GET_Bu(i)  RSM_GET_ARGN(i, RSM_POS_B, RSM_SIZE_Bi)
#define RSM_GET_Cu(i)  RSM_GET_ARGN(i, RSM_POS_C, RSM_SIZE_Ci)
#define RSM_GET_Du(i)  RSM_GET_ARGN(i, RSM_POS_D, RSM_SIZE_Di)
#define RSM_GET_As(i)  ((int)(RSM_GET_Au(i) - (RSM_MAX_Au / 2)))
#define RSM_GET_Bs(i)  ((int)(RSM_GET_Bu(i) - (RSM_MAX_Bu / 2)))
#define RSM_GET_Cs(i)  ((int)(RSM_GET_Cu(i) - (RSM_MAX_Cu / 2)))
#define RSM_GET_Ds(i)  ((int)(RSM_GET_Du(i) - (RSM_MAX_Du / 2)))

// rin_t RSM_SET_OP(rin_t, rop op)
// rin_t RSM_SET_i(rin_t,  bool isimm)
// rin_t RSM_SET_A(rin_t,  u32 regno)
// rin_t RSM_SET_Au(rin_t, u32 uimmval)
// rin_t RSM_SET_As(rin_t, i32 simmval)
#define RSM_SET_OP(i,v)  RSM_SET_ARGN(i, 0,         RSM_SIZE_OP, v)
#define RSM_SET_i(i,v)   RSM_SET_ARGN(i, RSM_POS_i, RSM_SIZE_i,  v)
#define RSM_SET_A(i,v)   RSM_SET_ARGN(i, RSM_POS_A, RSM_SIZE_A,  v)
#define RSM_SET_B(i,v)   RSM_SET_ARGN(i, RSM_POS_B, RSM_SIZE_B,  v)
#define RSM_SET_C(i,v)   RSM_SET_ARGN(i, RSM_POS_C, RSM_SIZE_C,  v)
#define RSM_SET_D(i,v)   RSM_SET_ARGN(i, RSM_POS_D, RSM_SIZE_D,  v)
#define RSM_SET_Au(i,v)  RSM_SET_ARGN(i, RSM_POS_A, RSM_SIZE_Ai, v)
#define RSM_SET_Bu(i,v)  RSM_SET_ARGN(i, RSM_POS_B, RSM_SIZE_Bi, v)
#define RSM_SET_Cu(i,v)  RSM_SET_ARGN(i, RSM_POS_C, RSM_SIZE_Ci, v)
#define RSM_SET_Du(i,v)  RSM_SET_ARGN(i, RSM_POS_D, RSM_SIZE_Di, v)
#define RSM_SET_As(i,v)  RSM_SET_Au(i, ((rin_t)(v)) + (RSM_MAX_Au / 2))
#define RSM_SET_Bs(i,v)  RSM_SET_Bu(i, ((rin_t)(v)) + (RSM_MAX_Bu / 2))
#define RSM_SET_Cs(i,v)  RSM_SET_Cu(i, ((rin_t)(v)) + (RSM_MAX_Cu / 2))
#define RSM_SET_Ds(i,v)  RSM_SET_Du(i, ((rin_t)(v)) + (RSM_MAX_Du / 2))

#define RSM_MAKE__(op)              ((rin_t)op)
#define RSM_MAKE_A(op,a)          ( ((rin_t)op)           | (((rin_t)a) << RSM_POS_A) )
#define RSM_MAKE_AB(op,a,b)       ( RSM_MAKE_A(op,a)       | (((rin_t)b) << RSM_POS_B) )
#define RSM_MAKE_ABC(op,a,b,c)    ( RSM_MAKE_AB(op,a,b)    | (((rin_t)c) << RSM_POS_C) )
#define RSM_MAKE_ABCD(op,a,b,c,d) ( RSM_MAKE_ABC(op,a,b,c) | (((rin_t)d) << RSM_POS_D) )

#define RSM_MAKE_Au(op,a)          (RSM_MAKE_A(op,a)          | (1 << RSM_POS_i))
#define RSM_MAKE_ABu(op,a,b)       (RSM_MAKE_AB(op,a,b)       | (1 << RSM_POS_i))
#define RSM_MAKE_ABv(op,a,b)       RSM_MAKE_ABu((op),(a),(b))
#define RSM_MAKE_ABCu(op,a,b,c)    (RSM_MAKE_ABC(op,a,b,c)    | (1 << RSM_POS_i))
#define RSM_MAKE_ABCDu(op,a,b,c,d) (RSM_MAKE_ABCD(op,a,b,c,d) | (1 << RSM_POS_i))

#define RSM_MAKE_As(op,a)          RSM_MAKE_Au(op,((rin_t)(a))          + (RSM_MAX_Au / 2))
#define RSM_MAKE_ABs(op,a,b)       RSM_MAKE_ABu(op,a,((rin_t)(b))       + (RSM_MAX_Bu / 2))
#define RSM_MAKE_ABCs(op,a,b,c)    RSM_MAKE_ABCu(op,a,b,((rin_t)(c))    + (RSM_MAX_Cu / 2))
#define RSM_MAKE_ABCDs(op,a,b,c,d) RSM_MAKE_ABCDu(op,a,b,c,((rin_t)(d)) + (RSM_MAX_Du / 2))

#if RSM_LITTLE_ENDIAN
  #define RSM_ROM_MAGIC 0x52534d00 // "RSM\0"
#else
  #define RSM_ROM_MAGIC 0x004d5352 // "RSM\0"
#endif

// RSM_PAGE_SIZE is the size in bytes of one RSM virtual memory page
#define RSM_PAGE_SIZE 4096u

typedef u8 rop; // opcode
enum rop {
  #define _(name, ...) rop_##name,
  RSM_FOREACH_OP(_)
  #undef _
  RSM_OP_COUNT,
} RSM_END_ENUM(rop)

typedef u8 rfmtflag; // string formatting flags
enum rfmtflag {
  RSM_FMT_COLOR = 1 << 0, // use ANSI colors
} RSM_END_ENUM(rfmtflag)

typedef int rerror; // error code
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

//———————————————————————————————————————————————————————————————————————————————————————
// rmem_t describes a memory region
typedef struct {
  void* nullable p;    // start address
  usize          size; // size in bytes
} rmem_t;

#define RMEM(p, size)  ((rmem_t){ p, size })

// RMEM_FMT is used for printf formatting of a rmem_t
#define RMEM_FMT              "{%p … %p %zu}"
#define RMEM_FMT_ARGS(region) (region).p, ((region).p+(region).size), (region).size

// RMEM_IS_VALID returns true if region has a non-NULL address and a non-zero size
#define RMEM_IS_VALID(region)     (!RMEM_IS_NULL(region) & !RMEM_IS_OVERFLOW(region))
#define RMEM_IS_NULL(region)      (!(uintptr)(region).p | !(uintptr)(region).size)
#define RMEM_IS_OVERFLOW(region) \
  (((uintptr)(region).p + (uintptr)(region).size) < (uintptr)(region).p)

// rmem_fill fills memory with a byte value
inline static void rmem_fill(rmem_t m, u8 byte) { __builtin_memset(m.p, byte, m.size); }

// rmem_zerofill fills memory with zeroes
inline static void rmem_zerofill(rmem_t m) { __builtin_memset(m.p, 0, m.size); }

// rmem_align adjusts a memory region to a given power-of-two alignment.
// If the region is too small to be aligned, false is returned
// and region is left unchanged.
bool rmem_align(rmem_t* region, usize alignment);

//———————————————————————————————————————————————————————————————————————————————————————
// rmm_t is a Memory Manager.
// The memory manager owns and manages all of the host memory.
// Allocations are limited to pages (RSM_PAGE_SIZE).
// rmm_allocpages can only allocate a power-of-two number of pages per call.
typedef struct rmm_ rmm_t;

#define RMM_MIN_SIZE 4096

rmm_t* nullable rmm_create(void* memp, usize memsize);
rmm_t* nullable rmm_create_host_vmmap(usize memsize);
void rmm_dispose(rmm_t*);
bool rmm_dispose_host_vmmap(rmm_t* mm);
void* nullable rmm_allocpages(rmm_t*, usize npages);
void rmm_freepages(rmm_t* restrict mm, void* restrict ptr);
uintptr rmm_startaddr(const rmm_t*);
usize rmm_cap(const rmm_t* mm); // total capacity (number of pages)

// rmm_avail_total returns the total number of pages available to allocate
usize rmm_avail_total(rmm_t*);

// rmm_avail_maxregion returns the number of pages of the largest, free region
// of contiguous pages.
usize rmm_avail_maxregion(rmm_t*);

// rmm_allocpages_min performs a best-effort allocation; it attempts to allocate
// req_npages and if there's no contiguous region of that many pages, it tries to
// allocate one order less (req_npages << 1).
// min_npages is the smallest number of pages it will attempt to allocate.
// On success, req_npages is updated with the actual number of pages allocated.
void* nullable rmm_allocpages_min(rmm_t* mm, usize* req_npages, usize min_npages);

//———————————————————————————————————————————————————————————————————————————————————————
// rmemalloc_t is a generic memory allocator
typedef struct rmemalloc_ rmemalloc_t;

// Alignment for an allocator, used with rmem_allocator_create_buf.
// RMEM_ALLOC_ALIGN_MIN is the minimum alignment required and may lead to disabling
// of slab sub-allocators. RMEM_ALLOC_ALIGN is the ideal (but larger) alignment that
// guarantees enabling of slab sub-allocators.
#define RMEM_ALLOC_ALIGN     (sizeof(void*)*8)
#define RMEM_ALLOC_ALIGN_MIN sizeof(void*)

// rmem_allocator_create creates a new allocator that sources memory from mm.
// initsize is the desired initial memory and can be zero.
// initsize is rounded up to nearest min(CHUNK_SIZE, pow2(initsize)) size
// where CHUNK_SIZE = sizeof(void*) * 8. Since backing memory has alignment
// requirements, the actual memory available may be different than the requested
// initsize; use rmem_cap to get the actual capacity.
rmemalloc_t* nullable rmem_allocator_create(rmm_t* mm, usize initsize);

// rmem_allocator_create_buf creates a new allocator in memory at buf.
// If no memory manager is provided (mm is NULL), the allocator will
// not have the ability to grow.
// Returns NULL if (after alignment) bufsize is too small.
rmemalloc_t* nullable rmem_allocator_create_buf(
  rmm_t* nullable mm, void* buf, usize bufsize);

// rmem_allocator_free disposes of an allocator.
// The allocator is invalid after this call.
void rmem_allocator_free(rmemalloc_t*);

// rmem_alloc_aligned allocates size bytes with specific address alignment.
// The returned address will have a _minimum_ alignment of 'alignment'.
// Allocations less than CHUNK_SIZE are rounded up and aligned to nearest upper pow2.
// Eg. size=24 is rounded up to 32 and has a minimum of 32B alignment.
// Allocations >= CHUNK_SIZE are sized in CHUNK_SIZE steps with a minimum alignment
// of CHUNK_SIZE. Eg. size=130,alignment=16 returns 256 bytes with CHUNK_SIZE alignment.
// Returns .start==NULL if the allocator is out of memory.
rmem_t rmem_alloc_aligned(rmemalloc_t*, usize size, usize alignment);

// rmem_alloc allocates size bytes with sizeof(void*) alignment.
// Returns .start==NULL if the allocator is out of memory.
inline static rmem_t rmem_alloc(rmemalloc_t* a, usize size) {
  return rmem_alloc_aligned(a, size, 1);
}

// rmem_alloc_arrayt allocates an element of type T
// T* nullable rmem_alloc_arrayt(rmemalloc_t* a, TYPE T)
#define rmem_alloct(a, T)  ( (T*)rmem_alloc_aligned((a), sizeof(T), _Alignof(T)).p )

// rmem_freet frees an element of type T
// void rmem_freet(rmemalloc_t* a, T* element)
#define rmem_freet(a, ptr)  rmem_free((a), RMEM((ptr), sizeof(*(ptr))) )

// rmem_alloc_array allocates an array of elements, checking for overflow
rmem_t rmem_alloc_array(rmemalloc_t*, usize count, usize elemsize, usize alignment);

// rmem_alloc_arrayt allocates an array of type T elements, checking for overflow
// rmem_t rmem_alloc_arrayt(rmemalloc_t* a, usize count, TYPE T)
#define rmem_alloc_arrayt(a, count, T) \
  rmem_alloc_array((a), (count), sizeof(T), _Alignof(T))

// rmem_free frees a memory region allocated by the same allocator
void rmem_free(rmemalloc_t*, rmem_t);

// rmem_resize grows or shrinks the size of an allocated memory region to newsize.
// If resizing fails, false is returned and the region is unchanged; it is still valid.
// Address alignment of new address is min(pow2(newsize),oldalignment).
bool rmem_resize(rmemalloc_t*, rmem_t*, usize newsize);

// rmem_must_* works like rmem_* but panics on failure
rmem_t rmem_must_alloc(rmemalloc_t*, usize size);
void rmem_must_resize(rmemalloc_t*, rmem_t*, usize newsize);

// rmem_alloc_size returns the effective size of an allocation of that size.
// E.g. rmem_alloc_size(size) == rmem_alloc(a, size).size
usize rmem_alloc_size(usize);

// rmem_avail returns the total number of bytes available to allocate
usize rmem_avail(rmemalloc_t*);

// rmem_cap returns the total number of bytes managed by the allocator
usize rmem_cap(rmemalloc_t*);

// OLD memory allocation interface
// static void* nullable rmem_alloc(rmem m, usize size, usize alignment);
// static void* nullable rmem_resize(
//   rmem m, void* nullable p, usize oldsize, usize newsize, usize newalignment);
// static void rmem_free(rmem m, void* p, usize size);


//——————————————————————————————————————————————————————————————————————————————————————

// rromimg_t: ROM image layout, a portable binary blob
typedef struct {
  u8 magic[4]; // RSM_ROM_MAGIC
  u8 version;
  u8 data[];
} rromimg_t;

// rrom_t: ROM (read only media); the container for an RSM program
typedef struct {
  // ROM image
  rromimg_t* img;
  usize      imgsize; // size of img, in bytes
  rmem_t     imgmem;  // img memory region (for use with rmem_free)

  // fields populated on demand by rsm_loadrom
  const rin_t* code;      // vm instructions array (pointer into img)
  usize         codelen;   // vm instructions array length
  const void*   data;      // data segment initializer (pointer into img)
  usize         datasize;  // data segment size
  u32           dataalign; // data segment alignment (in bytes)
} rrom_t;

// rvmstatus: VM status
typedef u8 rvmstatus;
enum rvmstatus {
  RVM_INIT,
  RVM_ERROR, // check rvm.error
  RVM_END = 0xff,
};

// rvm: VM instance
typedef struct {
  rvmstatus status;
  u64       iregs[RSM_NREGS];
  double    fregs[RSM_NREGS];
  void*     rambase;
  usize     ramsize;
  void*     internal;
} rvm;


// rsm_init initializes global state; must be called before using the rest of the API.
// Returns false if initialization failed.
RSMAPI bool rsm_init();

// rvm_create creates a new VM instance
rvm* nullable rvm_create(rmemalloc_t*);

// rvm_dispose frees a VM instance
void rvm_dispose(rvm* vm);

// rvm_main loads & runs a program in a ROM image
rerror rvm_main(rvm* vm, rrom_t* rom);

// rsm_vmexec executes a program, starting with instruction 0
// Loads the ROM if needed.
RSMAPI rerror rsm_vmexec(rvm* vm, rrom_t* rom);

// rsm_loadrom parses rom->img of rom->imgsize bytes,
// populating the rest of the fields of the rrom_t struct.
RSMAPI rerror rsm_loadrom(rrom_t* rom);

// rsm_fmtprog formats an array of instructions ip as "assembly" text to buf.
// It writes at most bufcap-1 of the characters to the output buf (the bufcap'th
// character then gets the terminating '\0'). If the return value is greater than or
// equal to the bufcap argument, buf was too short and some of the characters were
// discarded. The output is always null-terminated, unless size is 0.
// Returns the number of characters that would have been printed if bufcap was
// unlimited (not including the final `\0').
RSMAPI usize rsm_fmtprog(
  char* buf, usize bufcap, const rin_t* nullable ip, usize ilen, rfmtflag);
// if pcaddp is not null, it is set to the PC advance for the instruction,
// which is 1 for all except COPYV.
RSMAPI usize rsm_fmtinstr(
  char* buf, usize bufcap, rin_t, u32* nullable pcaddp, rfmtflag);

// enum related functions
RSMAPI const char* rop_name(rop);      // name of an opcode
RSMAPI const char* rerror_str(rerror); // short description of an error

// rsm_loadfile loads a file into memory
rerror rsm_loadfile(const char* filename, rmem_t* data_out);
void rsm_unloadfile(rmem_t); // unload file loaded with rsm_loadfile


// --------------------------------------------------------------------------------------
// assembler API (optional)
#ifndef RSM_NO_ASM

typedef struct rasm    rasm_t;    // assembly session (think of it as one source file)
typedef struct rnode   rnode_t;   // AST node
typedef struct rdiag   rdiag_t;   // diagnostic report
typedef struct rsrcpos rsrcpos_t; // line & column source position

// source tokens (rtok)
typedef u8 rtok;
#define RSM_FOREACH_TOKEN(_) \
_( RT_END ) \
_( RT_COMMENT ) \
/* simple tokens */ \
_( RT_LPAREN ) _( RT_RPAREN ) \
_( RT_LBRACE ) _( RT_RBRACE ) \
_( RT_SEMI   ) /* ; */ \
_( RT_COMMA  ) /* , */ \
_( RT_ASSIGN ) /* = */ \
/* names              */ \
_( RT_IREG   ) /* Rn   */ \
_( RT_FREG   ) /* Fn   */ \
_( RT_LABEL  ) /* foo: */ \
_( RT_NAME   ) /* foo  */ \
_( RT_OP     ) /* brz */ \
/* literal numbers (order matters; see snumber) */ \
_( RT_INTLIT2  ) _( RT_SINTLIT2  ) /* 0b1111011       */ \
_( RT_INTLIT   ) _( RT_SINTLIT   ) /* 123, -123       */ \
_( RT_INTLIT16 ) _( RT_SINTLIT16 ) /* 0x7b            */ \
_( RT_STRLIT ) /* "foo" */ \
_( RT_ARRAY  ) /* T[] */ \
// end RSM_FOREACH_TOKEN
// RSM_FOREACH_BINOP_TOKEN maps an infix binary operation to opcodes,
// allowing "x + y" as an alternative to "add x y"
#define RSM_FOREACH_BINOP_TOKEN(_) /* token, unsigned_op, signed_op */\
_( RT_PLUS  , ADD  , ADD  ) /* + */ \
_( RT_MINUS , SUB  , SUB  ) /* - */ \
_( RT_STAR  , MUL  , MUL  ) /* * */ \
_( RT_SLASH , DIV  , DIV  ) /* / */ \
_( RT_PERC  , MOD  , MOD  ) /* % */ \
_( RT_AMP   , AND  , AND  ) /* & */ \
_( RT_PIPE  , OR   , OR   ) /* | */ \
_( RT_HAT   , XOR  , XOR  ) /* ^ */ \
_( RT_LT2   , SHL  , SHL  ) /* << */ \
_( RT_GT2   , SHRU , SHRS ) /* >> */ \
_( RT_GT3   , SHRU , SHRU ) /* >>> */ \
_( RT_EQ    , EQ   , EQ  )  /* == */ \
_( RT_NEQ   , NEQ  , NEQ  ) /* != */ \
_( RT_LT    , LTU  , LTS  ) /* < */ \
_( RT_GT    , GTU  , GTS  ) /* > */ \
_( RT_LTE   , LTEU , LTES ) /* <= */ \
_( RT_GTE   , GTEU , GTES ) /* >= */ \
// end RSM_FOREACH_BINOP_TOKEN
#define RSM_FOREACH_KEYWORD_TOKEN(_) \
_( RT_I1   , "i1"   ) \
_( RT_I8   , "i8"   ) \
_( RT_I16  , "i16"  ) \
_( RT_I32  , "i32"  ) \
_( RT_I64  , "i64"  ) \
_( RT_FUN  , "fun"  ) \
_( RT_CONST, "const") \
_( RT_DATA , "data" ) \
// end RSM_FOREACH_KEYWORD_TOKEN
enum rtok {
  #define _(name, ...) name,
  RSM_FOREACH_TOKEN(_)
  RSM_FOREACH_BINOP_TOKEN(_)
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _
  rtok_COUNT
} RSM_END_ENUM(rtok)

// rdiaghandler is called with a diagnostict report.
// Return false to stop the process (e.g. stop assembling.)
typedef bool(*rdiaghandler_t)(const rdiag_t*, void* nullable userdata);

struct rdiag {
  int         code;      // error code (1=error, 0=warning)
  const char* msg;       // descriptive message including "srcname:line:col: type:"
  const char* msgshort;  // short descriptive message without source location
  const char* srclines;  // source context (a few lines of the source; may be empty)
  const char* srcname;   // eg filename
  u32         line, col; // origin (0 if unknown or not line-specific)
};

struct rasm {
  rmemalloc_t*   memalloc;    // memory allocator
  const char*    srcdata;     // input source bytes
  usize          srclen;      // length of srcdata
  const char*    srcname;     // symbolic name of source (e.g. filename)
  u32            errcount;    // number of errors reported
  rdiag_t        diag;        // last diagnostic report
  rdiaghandler_t diaghandler; // diagnostic report callback
  void* nullable userdata;    // passed along to diaghandler
  void* _internal[8];
};

struct rsrcpos {
  u32 line, col;
};

struct rnode {
  rtok              t;    // type
  rsrcpos_t         pos;  // source position, or {0,0} if unknown
  rnode_t* nullable next; // intrusive list link
  struct {
    rnode_t* nullable head;
    rnode_t* nullable tail;
  } children;
  union { // depends on value of t
    u64 ival;
    struct { const char* p; u32 len; } sval; // valid while origin rasm struct is valid
  };
};

// rasm_parse parses assembly source text into an AST.
// Uses a->mem for allocating AST nodes. a can be reused.
// Returns AST representing the source (a->src* fields) module (NULL on memory alloc fail.)
// Caller should check a->errcount on return and call rasm_free_rnode when done with the
// resulting rnode.
RSMAPI rnode_t* nullable rasm_parse(rasm_t* a);

// rasm_gen builds VM code from AST.
// Uses a->mem for temporary storage, allocates data for rom with rommem.
// a can be reused.
RSMAPI rerror rasm_gen(rasm_t* a, rnode_t* module, rmemalloc_t* rommem, rrom_t* rom);

// rasm_dispose frees resources of a
RSMAPI void rasm_dispose(rasm_t* a);

// rasm_free_rnode frees n (entire tree, including all children) back to a->mem
RSMAPI void rasm_free_rnode(rasm_t* a, rnode_t* n);

#endif // RSM_NO_ASM

RSM_ASSUME_NONNULL_END
