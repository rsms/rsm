#pragma once
#include "prelude.h"
R_ASSUME_NONNULL_BEGIN

// rtype_*
#define DEF_RSM_TYPES(_) \
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
// end DEF_RSM_TYPES

#define DEF_RSM_OPS(_) \
/* name, instruction encoding, description */ \
_( MOVE  , AB   , "R(A) = R(B) -- copy register" ) \
_( LOADI , ABi  , "R(A) = I(B) -- load immediate" ) \
_( LOADK , ABk  , "R(A) = K(B) -- load constant" ) \
\
_( CMPEQ , ABC  , "R(A) = R(B) == R(C)" ) \
_( BRZ   , ABC  , "goto instr(R(C)) if R(A) == 0 -- conditional branch absolute" ) \
_( BRZI  , ABCi , "goto PC±instr(Cs) if R(A) == 0 -- conditional branch relative" ) \
_( BRNZ  , ABC  , "goto instr(R(C)) if R(A) != 0 -- conditional branch absolute" ) \
_( BRNZI , ABCi , "goto PC±instr(Cs) if R(A) != 0 -- conditional branch relative" ) \
_( RET   , _    , "return" ) \
\
_( ADD   , ABC  , "R(A) = R(B) + R(C)" ) \
_( SUBI  , ABCi , "R(A) = R(B) + C" ) \
_( MUL   , ABC  , "R(A) = R(B) * R(C)" ) \
// end DEF_RSM_OPS

#define RS_ARGS_MAX 4 // max # args of a rs_value (for its op)

// rtype, rtype_* -- type code
typedef u8 rtype;
enum rtype {
  #define _(name, ...) rtype_##name,
  DEF_RSM_TYPES(_)
  #undef _
} END_TYPED_ENUM(rtype)

// rop, rop_* -- opcode
typedef u8 rop;
enum rop {
  #define _(name, ...) rop_##name,
  DEF_RSM_OPS(_)
  #undef _
  rop_MAX_ = 0xff,
} END_TYPED_ENUM(rop)

// rinstr is an instruction
typedef u32 rinstr;

// rmem is a memory allocator
typedef struct rmem rmem;
struct rmem {
  void* buf; // memory buffer
  usize len; // number of bytes used up in the memory buffer
  usize cap; // memory buffer size in bytes
  u32   flags;
};

// rarray is an array of pointer-sized elements
typedef struct rarray rarray;
struct rarray {
  void** v;
  u32    len;
  u32    cap;
};

// {enum}_name returns the name of a symbolic constant
const char* rop_name(rop);
const char* rtype_name(rtype);

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


// rarray
void rarray_grow(rarray* a, rmem* m);
static void rarray_push(rarray* a, rmem* m, void* elem);


// rabuf is a string output buffer for implementing snprintf-style functions
typedef struct {
  char* p;
  char* lastp;
  usize len;
} rabuf;
#define rabuf_make(p,size) ({                        \
  usize z__ = (usize)(size);                         \
  char* p__ = (p);                                   \
  static char x__;                                   \
  UNLIKELY(z__ == 0) ? (rabuf){ &x__, &x__, 0 }      \
                     : (rabuf){ p__, p__+z__-1, 0 }; \
})
void rabuf_init(rabuf* s, char* buf, usize bufsize); // bufsize must be >0
usize rabuf_terminate(rabuf* s);
#define rabuf_avail(s) ( (usize)(uintptr)((s)->lastp - (s)->p) )
void rabuf_appendc(rabuf* s, char c);
void rabuf_append(rabuf* s, const char* p, usize len);
void rabuf_appendu64(rabuf* s, u64 v, u32 base);
void rabuf_appendf64(rabuf* s, f64 v, int ndec);
static void rabuf_appendstr(rabuf* s, const char* cstr);
void rabuf_appendfill(rabuf* s, char c, usize len); // like memset
void rabuf_appendrepr(rabuf* s, const char* srcp, usize len);
void rabuf_appendfmt(rabuf* s, const char* fmt, ...) ATTR_FORMAT(printf, 2, 3);
void rabuf_appendfmtv(rabuf* s, const char* fmt, va_list);
bool rabuf_endswith(const rabuf* s, const char* str, usize len);


// ---------------

inline static void rarray_push(rarray* a, rmem* m, void* elem) {
  if UNLIKELY(a->len == a->cap)
    rarray_grow(a, m);
  a->v[a->len++] = elem;
}

inline static void rabuf_appendstr(rabuf* s, const char* cstr) {
  rabuf_append(s, cstr, strlen(cstr));
}

R_ASSUME_NONNULL_END
