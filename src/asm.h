// assembler internals, shared by all asm*.c files and rom_build
// SPDX-License-Identifier: Apache-2.0
#pragma once
#ifndef RSM_NO_ASM
RSM_ASSUME_NONNULL_BEGIN

#define kBlock0Name "b0" // name of first block

typedef struct gstate gstate;
typedef struct pstate pstate;

typedef struct rposrange {
  rsrcpos_t start, focus, end;
} rposrange_t;

// rasm._internal[0] -- negated diaghandler return value
#define rasm_stop(a)        ( (bool)(a)->_internal[0] )
#define rasm_stop_set(a,v)  ( *(bool*)&(a)->_internal[0] = (v) )

// rasm._internal[1]-- reusable internal codegen state
#define rasm_gstate(a)        ( (gstate*)(a)->_internal[1] )
#define rasm_gstate_set(a,v)  ( *(gstate**)&(a)->_internal[1] = (v) )

// rasm._internal[2]-- reusable internal codegen state
#define rasm_pstate(a)        ( (pstate*)(a)->_internal[2] )
#define rasm_pstate_set(a,v)  ( *(pstate**)&(a)->_internal[2] = (v) )

const char* tokname(rtok_t t);

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

inline static bool nodename_eq(const rnode_t* n, const char* str, usize len) {
  return n->sval.len == len && memcmp(n->sval.p, str, len) == 0;
}

rnode_t* nullable nlastchild(rnode_t* n);

rposrange_t nposrange(rnode_t*);

void errf(rasm_t*, rposrange_t, const char* fmt, ...) ATTR_FORMAT(printf, 3, 4);
void warnf(rasm_t*, rposrange_t, const char* fmt, ...) ATTR_FORMAT(printf, 3, 4);
void reportv(rasm_t*, rposrange_t, int code, const char* fmt, va_list ap);

typedef struct rrombuild {
  const rin_t* code;      // vm instructions array
  usize        codelen;   // vm instructions array length
  usize        datasize;  // data segment size
  u8           dataalign; // data segment alignment
  void*        userdata;
  rerr_t(*filldata)(void* dst, void* userdata);
} rrombuild_t;

rerr_t rom_build(rrombuild_t* rb, rmemalloc_t* ma, rrom_t* rom);

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

void* nullable bufslab_alloc(bufslabs* slabs, rmemalloc_t*, usize nbyte);
void bufslabs_reset(bufslabs* slabs); // set all slab->len=0 and set slabs->head=tail
void bufslab_freerest(bufslab* s, rmemalloc_t*); // free all slabs after s


RSM_ASSUME_NONNULL_END
#endif // RSM_NO_ASM
