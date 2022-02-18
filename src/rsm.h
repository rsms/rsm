#pragma once
#include "prelude.h"
R_ASSUME_NONNULL_BEGIN

// Instructions are fixed-size at 32 bits long, little endian.
// PC and jump- & branch destinations are expressed in #instructions rather than bytes.
// There is room for 256 operations and 32+32 (int+float) registers (8 bit OP, 5 bit reg)
typedef u32 rinstr;
//
//        ┌─────────────────┬─────────┬─────────┬─────────┬───────────────┐
//  bit   │3 3 2 2 2 2 2 2 2│2 2 2 1 1│1 1 1 1 1│1 1 1    │               │
//        │1 0 9 8 7 6 5 4 3│2 1 0 9 8│7 6 5 4 3│2 1 0 9 8│7 6 5 4 3 2 1 0│
//        ├─────────────────┼─────────┼─────────┼─────────┼───────────────┤
//  ABCD  │          D (9)  │  C (5)  │  B (5)  │  A (5)  │     OP (8)    │
//        ├─────────────────┴─────────┼─────────┼─────────┼───────────────┤
//  ABCw  │                    C (14) │  B (5)  │  A (5)  │     OP (8)    │
//        ├───────────────────────────┴─────────┼─────────┼───────────────┤
//  ABw   │                              B (19) │  A (5)  │     OP (8)    │
//        ├─────────────────────────────────────┴─────────┼───────────────┤
//  Aw    │                                        A (24) │     OP (8)    │
//        └───────────────────────────────────────────────┴───────────────┘
//
// encoding legend:
//   A    register A,
//   AB   register A, register B
//   ABk  register A, constant B
//   ABu  register A, immediate unsigned B
//   ABs  register A, immediate signed B
//   ...
#define RSM_FOREACH_OP(_) \
/* name, instruction encoding, description */ \
_( MOVE  , AB   , "R(A) = R(B) -- copy register" ) \
_( LOADI , ABu  , "R(A) = I(B) -- load immediate" ) \
_( LOADK , ABk  , "R(A) = K(B) -- load constant" ) \
\
_( CMPEQ , ABC  , "R(A) = R(B) == R(C)" ) \
_( ADD   , ABC  , "R(A) = R(B) + R(C)" ) \
_( SUBI  , ABCu , "R(A) = R(B) - C" ) \
_( MUL   , ABC  , "R(A) = R(B) * R(C)" ) \
\
_( BRZ   , AB   , "goto instr(R(B)) if R(A) == 0 -- conditional branch absolute" ) \
_( BRNZ  , AB   , "goto instr(R(B)) if R(A) != 0 -- conditional branch absolute" ) \
_( BRZI  , ABs  , "goto PC±instr(Bs) if R(A) == 0 -- conditional branch relative" ) \
_( BRNZI , ABs  , "goto PC±instr(Bs) if R(A) != 0 -- conditional branch relative" ) \
\
_( RET   , _    , "return" ) \
// end RSM_FOREACH_OP

typedef u8 rop; // rop, rop_* -- opcode
enum rop {
  #define _(name, ...) rop_##name,
  RSM_FOREACH_OP(_)
  #undef _
  RSM_OP_COUNT,
} END_TYPED_ENUM(rop)

// rtype_*
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

// size and position of instruction arguments
#define RSM_SIZE_OP  8
#define RSM_SIZE_A   5
#define RSM_SIZE_B   5
#define RSM_SIZE_C   5
#define RSM_SIZE_D   9
#define RSM_SIZE_Cw  (RSM_SIZE_C + RSM_SIZE_D)
#define RSM_SIZE_Bw  (RSM_SIZE_B + RSM_SIZE_C + RSM_SIZE_D)
#define RSM_SIZE_Aw  (RSM_SIZE_A + RSM_SIZE_B + RSM_SIZE_C + RSM_SIZE_D)
#define RSM_MAX_Aw   0xffffff /* 2^24 - 1 */
#define RSM_MAX_Bw   0x7ffff  /* 2^19 - 1 */
#define RSM_MAX_Cw   0x3fff   /* 2^14 - 1 */
#define RSM_MAX_D    0x1ff    /* 2^9  - 1 */
#define RSM_POS_A    RSM_SIZE_OP
#define RSM_POS_B    (RSM_POS_A + RSM_SIZE_A)
#define RSM_POS_C    (RSM_POS_B + RSM_SIZE_B)
#define RSM_POS_D    (RSM_POS_C + RSM_SIZE_C)

// u32 RSM_GET_ARGN(rinstr, uint pos, uint size)
// rinstr RSM_SET_ARGN(rinstr, uint pos, uint size, uint val)
#define RSM_GET_ARGN(i,pos,size) ((u32)( ((i) >> (pos)) & RSM_MASK1(size,0) ))
#define RSM_SET_ARGN(i,pos,size,v) \
  ( ((i) & RSM_MASK0(size,pos)) | ( (((rinstr)v) << pos) & RSM_MASK1(size,pos)) )
#define RSM_MASK1(n,p)  ( ( ~( (~(rinstr)0) << (n) ) ) << (p) ) /* n 1 bits at position p */
#define RSM_MASK0(n,p)  (~RSM_MASK1(n,p))  /* n 0 bits at position p */

// rop RSM_GET_OP(rinstr) -- get opcode
// u32 RSM_GET_Ar(rinstr) -- get register-sized value in A
// u32 RSM_GET_Au(rinstr) -- get full-size unsigned value in A
// i32 RSM_GET_As(rinstr) -- get full-size signed value in A
#define RSM_GET_OP(i)  ((rop)( RSM_GET_ARGN(i,0,RSM_SIZE_OP) ))
#define RSM_GET_Ar(i)  RSM_GET_ARGN(i, RSM_POS_A, RSM_SIZE_A)
#define RSM_GET_Au(i)  RSM_GET_ARGN(i, RSM_POS_A, RSM_SIZE_Aw)
#define RSM_GET_As(i)  ((int)(RSM_GET_Au(i) - (RSM_MAX_Aw / 2)))
#define RSM_GET_Br(i)  RSM_GET_ARGN(i, RSM_POS_B, RSM_SIZE_B)
#define RSM_GET_Bu(i)  RSM_GET_ARGN(i, RSM_POS_B, RSM_SIZE_Bw)
#define RSM_GET_Bs(i)  ((int)(RSM_GET_Bu(i) - (RSM_MAX_Bw / 2)))
#define RSM_GET_Cr(i)  RSM_GET_ARGN(i, RSM_POS_C, RSM_SIZE_C)
#define RSM_GET_Cu(i)  RSM_GET_ARGN(i, RSM_POS_C, RSM_SIZE_Cw)
#define RSM_GET_Cs(i)  ((int)(RSM_GET_Cu(i) - (RSM_MAX_Cw / 2)))
#define RSM_GET_Du(i)  RSM_GET_ARGN(i, RSM_POS_D, RSM_SIZE_D)
#define RSM_GET_Ds(i)  ((int)(RSM_GET_Du(i) - (RSM_MAX_D / 2)))

// rinstr RSM_SET_Ar(rinstr, rop op)    -- copy of i with opcode op
// rinstr RSM_SET_Ar(rinstr, int regno) -- copy of i with A set to register number
// rinstr RSM_SET_Au(rinstr, u32 val)   -- copy of i with A set to full-size unsigned value
// rinstr RSM_SET_As(rinstr, i32 sval)  -- copy of i with A set to full-size signed value
#define RSM_SET_OP(i,v)  RSM_SET_ARGN(i,0,RSM_SIZE_OP,v)
#define RSM_SET_Ar(i,v)  RSM_SET_ARGN(i, RSM_POS_A, RSM_SIZE_A, v)
#define RSM_SET_Au(i,v)  RSM_SET_ARGN(i, RSM_POS_A, RSM_SIZE_Aw, v)
#define RSM_SET_Br(i,v)  RSM_SET_ARGN(i, RSM_POS_B, RSM_SIZE_B, v)
#define RSM_SET_Bu(i,v)  RSM_SET_ARGN(i, RSM_POS_B, RSM_SIZE_Bw, v)
#define RSM_SET_Cr(i,v)  RSM_SET_ARGN(i, RSM_POS_C, RSM_SIZE_C, v)
#define RSM_SET_Cu(i,v)  RSM_SET_ARGN(i, RSM_POS_C, RSM_SIZE_Cw, v)
#define RSM_SET_Dr(i,v)  RSM_SET_ARGN(i, RSM_POS_D, RSM_SIZE_D, v)

#define RSM_MAKE_ABCD(op,a,b,c,d) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A ) \
  | ( ((rinstr)b) << RSM_POS_B ) \
  | ( ((rinstr)c) << RSM_POS_C ) \
  | ( ((rinstr)d) << RSM_POS_D ) )
#define RSM_MAKE_ABC(op,a,b,c) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A ) \
  | ( ((rinstr)b) << RSM_POS_B ) \
  | ( ((rinstr)c) << RSM_POS_C ) )
#define RSM_MAKE_AB(op,a,b) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A  ) \
  | ( ((rinstr)b) << RSM_POS_B ) )
#define RSM_MAKE_A(op,a) ( ((rinstr)op) \
  | ( ((rinstr)a) << RSM_POS_A ) )
#define RSM_MAKE_As(op,a)          RSM_MAKE_A(op,((rinstr)(a)) + (RSM_MAX_Aw / 2))
#define RSM_MAKE_ABs(op,a,b)       RSM_MAKE_AB(op,a,((rinstr)(b)) + (RSM_MAX_Bw / 2))
#define RSM_MAKE_ABCs(op,a,b,c)    RSM_MAKE_AB(op,a,b,((rinstr)(c)) + (RSM_MAX_Cw / 2))
#define RSM_MAKE_ABCDs(op,a,b,c,d) RSM_MAKE_AB(op,a,b,c,((rinstr)(d)) + (RSM_MAX_D / 2))

const char* rop_name(rop); // name of an opcode
const char* rtype_name(rtype); // name of a type constant

// rsm_asm assembles instructions from source text src
usize rsm_asm(rinstr* idst, usize idstcap, const char* src);

// rsm_fmtprog formats an array of instructions ip as "assembly" text to buf.
// It writes at most bufcap-1 of the characters to the output buf (the bufcap'th
// character then gets the terminating '\0'). If the return value is greater than or
// equal to the bufcap argument, buf was too short and some of the characters were
// discarded. The output is always null-terminated, unless size is 0.
// Returns the number of characters that would have been printed if bufcap was
// unlimited (not including the final `\0').
usize rsm_fmtprog(char* buf, usize bufcap, rinstr* ip, usize ilen);
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
void* nullable rmem_resize(rmem* m, void* p, usize newsize)
  ATTR_ALLOC_SIZE(3) WARN_UNUSED_RESULT;
void* rmem_dup(rmem* m, const void* p, usize len);
#define rmem_strdup(m, cstr) ((char*)rmem_dup((m),(cstr),strlen(cstr)))
// rmem_vmalloc allocates nbytes of virtual memory. Returns NULL on failure.
void* nullable rmem_vmalloc(usize nbytes);
usize rmem_vmpagesize();
// rmem_vmfree frees memory allocated with rmem_vmalloc
bool rmem_vmfree(void* ptr, usize nbytes);


R_ASSUME_NONNULL_END
