#pragma once
#include "prelude.h"
R_ASSUME_NONNULL_BEGIN

typedef u32 rinstr; // rinstr is an instruction (see rinst.h for details)

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

// rtype, rtype_* -- type code
typedef u8 rtype;
enum rtype {
  #define _(name, ...) rtype_##name,
  DEF_RSM_TYPES(_)
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

// rarray is an array of pointer-sized elements
typedef struct rarray rarray;
struct rarray {
  void** v;
  u32    len;
  u32    cap;
};

// rtype_name returns the name of a type constant
const char* rtype_name(rtype);

// fmtprog formats an array of instructions ip as "assembly" text to buf.
// It writes at most bufcap-1 of the characters to the output buf (the bufcap'th
// character then gets the terminating '\0'). If the return value is greater than or
// equal to the bufcap argument, buf was too short and some of the characters were
// discarded. The output is always null-terminated, unless size is 0.
// Returns the number of characters that would have been printed if bufcap was
// unlimited (not including the final `\0').
usize fmtprog(char* buf, usize bufcap, rinstr* ip, usize ilen);

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
usize rstrfmtu64(char buf[64], u64 v, u32 base);


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
