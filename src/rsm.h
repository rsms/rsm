#pragma once
#include "prelude.h"
R_ASSUME_NONNULL_BEGIN

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
_( MOVE  , ABu  , "move"  /* R(A) = {R(B),B}  TODO: rename */)\
_( LOADK , ABu  , "loadk" /* R(A) = K(B) -- load constant  */)\
\
_( ADD   , ABCu , "add"   /* R(A) = R(B) + {R(C),C}                                */)\
_( SUB   , ABCu , "sub"   /* R(A) = R(B) - {R(C),C}                                */)\
_( MUL   , ABCu , "mul"   /* R(A) = R(B) * {R(C),C}                                */)\
_( DIV   , ABCu , "div"   /* R(A) = R(B) / {R(C),C}                                */)\
_( MOD   , ABCu , "mod"   /* R(A) = R(B) % {R(C),C}                                */)\
_( AND   , ABCu , "and"   /* R(A) = R(B) & {R(C),C}                                */)\
_( OR    , ABCu , "or"    /* R(A) = R(B) | {R(C),C}                                */)\
_( XOR   , ABCu , "xor"   /* R(A) = R(B) ^ {R(C),C}                                */)\
_( SHL   , ABCu , "shl"   /* R(A) = R(B) << {R(C),C}                               */)\
_( SHRS  , ABCs , "shrs"  /* R(A) = R(B) >> {R(C),C} sign-replicating (arithmetic) */)\
_( SHRU  , ABCu , "shru"  /* R(A) = R(B) >> {R(C),C} zero-replicating (logical)    */)\
\
_( CMPEQ , ABCu , "cmpeq" /* R(A) = R(B) == {R(C),C} */)\
_( CMPLT , ABCu , "cmplt" /* R(A) = R(B) < {R(C),C}  */)\
_( CMPGT , ABCu , "cmpgt" /* R(A) = R(B) > {R(C),C}  */)\
\
_( BRZ   , ABs  , "brz"   /* if R(A)==0 goto instr({R(B),PC+Bs}) */)\
_( BRNZ  , ABs  , "brnz"  /* if R(A)!=0 goto instr({R(B),PC+Bs}) */)\
\
_( RET   , _    , "ret" /* return */)\
// end RSM_FOREACH_OP

typedef u8 rop; // rop, rop_* -- opcode
enum rop {
  #define _(name, ...) rop_##name,
  RSM_FOREACH_OP(_)
  #undef _
  RSM_OP_COUNT,
} END_TYPED_ENUM(rop)

// types
#define RSM_FOREACH_TYPE(_) \
/* name, bitsize */ \
_( void, 0  ) \
_( i1,   1  ) \
_( i8,   8  ) \
_( i16,  16 ) \
_( i32,  32 ) \
_( i64,  64 ) \
_( f32,  32 ) \
_( f64,  64 ) \
_( ptr,  64 ) \
// end RSM_FOREACH_TYPE

// rtype, rt_* -- type code
typedef u8 rtype;
enum rtype {
  #define _(name, ...) rt_##name,
  RSM_FOREACH_TYPE(_)
  #undef _
} END_TYPED_ENUM(rtype)

// rmem is a memory allocator
typedef struct rmem rmem;
struct rmem {
  void* buf; // memory buffer
  usize len; // number of bytes used up in the memory buffer
  usize cap; // memory buffer size in bytes
  u32   flags;
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

// rasmctx holds information of an assembly session
typedef struct rasmctx rasmctx;
struct rasmctx {
  rmem*          mem;         // memory allocator
  const char*    srcdata;     // input source bytes
  usize          srclen;      // length of srcdata
  const char*    srcname;     // symbolic name of source (e.g. filename)
  u32            errcount;    // number of errors reported
  rdiag          diag;        // last diagnostic report
  rdiaghandler   diaghandler; // diagnostic report callback
  void* nullable userdata;    // passed along to diaghandler
  // internal fields
  char _diagmsg[128]; // storage for diag.msg
  bool _stop;         // negated diaghandler return value
};

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

#define RSM_MAX_Aw   0x7fffff /* 2^23 - 1  TODO use ILOG2 macro? */
#define RSM_MAX_Bw   0x3ffff  /* 2^18 - 1 */
#define RSM_MAX_Cw   0x1fff   /* 2^13 - 1 */
#define RSM_MAX_Dw   0xff     /* 2^8  - 1 */

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
#define RSM_GET_As(i)  ((int)(RSM_GET_Au(i) - (RSM_MAX_Aw / 2)))

#define RSM_GET_B(i)   RSM_GET_ARGN(i, RSM_POS_B, RSM_SIZE_B)
#define RSM_GET_Bi(i)  RSM_GET_ARGN(i, RSM_POS_B, 1)
#define RSM_GET_Bu(i)  RSM_GET_ARGN(i, RSM_POS_Bw, RSM_SIZE_Bw)
#define RSM_GET_Bs(i)  ((int)(RSM_GET_Bu(i) - (RSM_MAX_Bw / 2)))

#define RSM_GET_C(i)   RSM_GET_ARGN(i, RSM_POS_C, RSM_SIZE_C)
#define RSM_GET_Ci(i)  RSM_GET_ARGN(i, RSM_POS_C, 1)
#define RSM_GET_Cu(i)  RSM_GET_ARGN(i, RSM_POS_Cw, RSM_SIZE_Cw)
#define RSM_GET_Cs(i)  ((int)(RSM_GET_Cu(i) - (RSM_MAX_Cw / 2)))

#define RSM_GET_D(i)   RSM_GET_ARGN(i, RSM_POS_D, RSM_SIZE_D)
#define RSM_GET_Di(i)  RSM_GET_ARGN(i, RSM_POS_D, 1)
#define RSM_GET_Du(i)  RSM_GET_ARGN(i, RSM_POS_Dw, RSM_SIZE_Dw)
#define RSM_GET_Ds(i)  ((int)(RSM_GET_Du(i) - (RSM_MAX_Dw / 2)))

// rinstr RSM_SET_Ar(rinstr, rop op)    -- copy of in with opcode op
// rinstr RSM_SET_Ar(rinstr, int regno) -- copy of in with A set to register number
// rinstr RSM_SET_Au(rinstr, u32 val)   -- copy of in with A set to full-size unsigned value
// rinstr RSM_SET_As(rinstr, i32 sval)  -- copy of in with A set to full-size signed value
#define RSM_SET_OP(i,v)  RSM_SET_ARGN(i, 0,          RSM_SIZE_OP, v)
#define RSM_SET_A(i,v)   RSM_SET_ARGN(i, RSM_POS_A,  RSM_SIZE_A,  v)
#define RSM_SET_Au(i,v)  RSM_SET_ARGN(i, RSM_POS_Aw, RSM_SIZE_Aw, v)
#define RSM_SET_B(i,v)   RSM_SET_ARGN(i, RSM_POS_B,  RSM_SIZE_B,  v)
#define RSM_SET_Bu(i,v)  RSM_SET_ARGN(i, RSM_POS_Bw, RSM_SIZE_Bw, v)
#define RSM_SET_Bs(i,v)  RSM_SET_Bu(i, ((rinstr)(v)) + (RSM_MAX_Bw / 2))
#define RSM_SET_C(i,v)   RSM_SET_ARGN(i, RSM_POS_C,  RSM_SIZE_C,  v)
#define RSM_SET_Cu(i,v)  RSM_SET_ARGN(i, RSM_POS_Cw, RSM_SIZE_Cw, v)
#define RSM_SET_Cs(i,v)  RSM_SET_Cu(i, ((rinstr)(v)) + (RSM_MAX_Cw / 2))
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
  | (1 << RSM_POS_B) | ( ((rinstr)a) << RSM_POS_Aw ) )
#define RSM_MAKE__(op) ((rinstr)op)

#define RSM_MAKE_As(op,a)          RSM_MAKE_Au(op,((rinstr)(a)) + (RSM_MAX_Aw / 2))
#define RSM_MAKE_ABs(op,a,b)       RSM_MAKE_ABu(op,a,((rinstr)(b)) + (RSM_MAX_Bw / 2))
#define RSM_MAKE_ABCs(op,a,b,c)    RSM_MAKE_ABCu(op,a,b,((rinstr)(c)) + (RSM_MAX_Cw / 2))
#define RSM_MAKE_ABCDs(op,a,b,c,d) RSM_MAKE_ABCDu(op,a,b,c,((rinstr)(d)) + (RSM_MAX_Dw / 2))

const char* rop_name(rop); // name of an opcode
const char* rtype_name(rtype); // name of a type constant

// rsm_asm assembles instructions from source text src. Allocates *res in ctx->mem.
// ctx can be reused with multiple calls.
// Returns the number of instructions at *res on success, or 0 on failure.
usize rsm_asm(rasmctx* ctx, rinstr** res);

// rsm_fmtprog formats an array of instructions ip as "assembly" text to buf.
// It writes at most bufcap-1 of the characters to the output buf (the bufcap'th
// character then gets the terminating '\0'). If the return value is greater than or
// equal to the bufcap argument, buf was too short and some of the characters were
// discarded. The output is always null-terminated, unless size is 0.
// Returns the number of characters that would have been printed if bufcap was
// unlimited (not including the final `\0').
usize rsm_fmtprog(char* buf, usize bufcap, rinstr* nullable ip, usize ilen);
usize rsm_fmtinstr(char* buf, usize bufcap, rinstr in);

// rsm_eval executes a program, starting with instruction inv[0]
void rsm_eval(u64* iregs, u32* inv, u32 inc);

// rmem
// RMEM_MIN -- minimum amount of memory (in bytes) that rmem_init accepts
#define RMEM_MIN   (sizeof(rmem)*2)
#define RMEM_ALIGN sizeof(void*)  // must be pow2
rmem* rmem_make(void* buf, usize size); // create in buf
rmem* nullable rmem_makevm(usize size); // create in system-managed virtual memory
void rmem_release(rmem* m); // frees vmem if rmem_new was used
#define rmem_align_size(nbyte) ALIGN2((nbyte),RMEM_ALIGN)
#define rmem_align_addr(addr)  ALIGN2_FLOOR((uintptr)(addr),RMEM_ALIGN)
#define rmem_allocz(m, size)   _rmem_allocz((m), rmem_align_size(size))
#define rmem_alloc             rmem_allocz
void* _rmem_allocz(rmem* m, usize size)
  ATTR_MALLOC ATTR_ALLOC_SIZE(2) WARN_UNUSED_RESULT;
void* rmem_alloczv(rmem* m, usize elemsize, usize count)
  ATTR_MALLOC ATTR_ALLOC_SIZE(2, 3) WARN_UNUSED_RESULT;
inline static void rmem_free(void* p) {}
void* nullable rmem_resize(rmem* m, void* p, usize oldsize, usize newsize)
  ATTR_ALLOC_SIZE(3) WARN_UNUSED_RESULT;
void* rmem_dup(rmem* m, const void* p, usize len);
#define rmem_strdup(m, cstr) ((char*)rmem_dup((m),(cstr),strlen(cstr)))
// rmem_vmalloc allocates nbytes of virtual memory. Returns NULL on failure.
void* nullable rmem_vmalloc(usize nbytes);
usize rmem_vmpagesize();
// rmem_vmfree frees memory allocated with rmem_vmalloc
bool rmem_vmfree(void* ptr, usize nbytes);


R_ASSUME_NONNULL_END
