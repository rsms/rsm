// assembler: vm code generator
// SPDX-License-Identifier: Apache-2.0
#ifndef RSM_NO_ASM
#include "rsmimpl.h"
#include "array.h"
#include "map.h"
#include "abuf.h"
#include "asm.h"
#include "sched.h"

// DEBUG_LOG_DATALAYOUT: define to log debug messages about data layout
#define DEBUG_LOG_DATALAYOUT

// ASMGEN_TRACE: define to enable logging a lot of info via dlog
//#define ASMGEN_TRACE

// codegen
typedef struct gstate gstate;
typedef struct gfun   gfun;
typedef struct gblock gblock;
typedef struct gbhead gbhead; // head of gblock and gfun
typedef struct gref   gref;
typedef u8            grefflag;
typedef struct gnamed gnamed;
typedef u8            gnamedtype;
typedef struct gdata  gdata;

// typedef struct gdatav gdatav;

typedef struct gslab     gslab;
typedef struct gdataslab gdataslab;
typedef struct gfunslab  gfunslab;

enum gnamedtype {
  GNAMED_T_FUN,   // gfun
  GNAMED_T_BLOCK, // gblock
  GNAMED_T_DATA,  // gdata
  GNAMED_T_CONST, // gdata
} RSM_END_ENUM(gnamedtype)

// head of named structs, which may be referenced in gstate.names
struct gnamed {
  #define NAMED_HEAD       \
    gnamedtype  namedtype; \
    const char* name;      \
    u32         namelen;   \
    u32         nrefs;
  NAMED_HEAD
};

struct gdata { // base: gnamed
  NAMED_HEAD
  u16                  align;   // byte alignment (1 2 4 8 16 32 64 128 ...)
  usize                size;    // length of data in bytes
  usize                initlen; // bytes at initp (initlen<=size)
  const void* nullable initp;   // pointer to initial value, if any
  u64                  addr;    // address (valid only after layout)
  gdata* nullable      next;    // for gstate.datalist
  rnode_t*             origin;
};

// head of instruction-block structs
struct gbhead { // base: gnamed
  #define BLOCK_HEAD \
    NAMED_HEAD        \
    usize i; /* first instruction = iv[i] */
  BLOCK_HEAD
};

struct gfun { // base: gbhead
  BLOCK_HEAD
  rarray blocks; // gblock[]
  u32    fi;     // function table index (value that fits in Au)
  rarray ulv;    // gref[]; pending undefined references (temporary)
};

struct gblock { // base: gbhead
  BLOCK_HEAD
  rsrcpos_t pos;
};

#define SLAB_HEAD(TYPE) \
  TYPE* nullable next; \
  usize len; // data + len*elemsize == next free entry
struct gdataslab {
  SLAB_HEAD(gdataslab)
  gdata data[32];
};
struct gfunslab {
  SLAB_HEAD(gfunslab)
  gfun data[8];
};

struct gstate {
  rasm_t* a;       // compilation session/context
  rarray  iv;      // rin_t[]; instructions
  rarray  funs;    // gfun[]; functions
  rarray  udnames; // gref[]; pending undefined named references
  smap    names;   // name => gnamed*

  // gdata blocks may change order and may grow with separate memory allocations.
  // Referencing and patching data is made simpler (better?) by using gdata pointers
  // instead of secondary generated IDs. The downside is that we can't use a simple
  // rarray for this since as it grows, the addresses of gdata elements might change.
  // So, we use a list of gdata arrays that we never grow; used as a slab allocator.
  gdataslab  datavhead; // first slab + list of additional data slabs
  gdataslab* datavcurr; // current slab
  rarray     dataorder; // gdata*[]; valid after layout, points to gdatav entries
  usize      datasize;  // size of data segment
  u32        dataalign; // alignment of data segment (in bytes)

  // gfun storage
  gfunslab  fnvhead;
  gfunslab* fnvcurr;

  gfun* nullable fn; // current function
};

struct gref {
  u32              i;     // referrer's iv offset
  rnode_t*         n;     // referrer
  grefflag         flags;
  gnamed* nullable target;
};

enum grefflag {
  REF_ANY = 1 << 0, // target is either label or function
  REF_ABS = 1 << 1, // target is an address, not a delta
} RSM_END_ENUM(grefflag)


#if defined(DEBUG_LOG_DATALAYOUT) && defined(DEBUG)
  #define dlog_datalayout(fmt, args...) dlog(fmt, ##args)
#else
  #undef DEBUG_LOG_DATALAYOUT
  #define dlog_datalayout(...) ((void)0)
#endif


#if defined(ASMGEN_TRACE) && defined(DEBUG)
  #define trace(fmt, args...) dlog("[asmgen] %s: " fmt, __FUNCTION__, ##args)
#else
  #ifdef ASMGEN_TRACE
    #warning ASMGEN_TRACE has no effect unless DEBUG is enabled
    #undef ASMGEN_TRACE
  #endif
  #define trace(...) ((void)0)
#endif


#define ERRN(n, fmt, args...) errf(g->a, nposrange(n), fmt, ##args)

static bool check_alloc(gstate* g, void* nullable p) {
  if LIKELY(p != NULL)
    return false;
  errf(g->a, (rposrange_t){0}, "out of memory");
  return true;
}

#define GARRAY_PUSH_OR_RET(T, arrayp, ERRRET...) ({ \
  T* vp__ = rarray_push(T, (arrayp), g->a->memalloc); \
  if (check_alloc(g, vp__)) return ERRRET; \
  vp__; })

#define GSLAB_ALLOC(g, HEADFIELD, CURRFIELD, ERRRET...) ({       \
  if UNLIKELY(g->CURRFIELD->len == countof(g->HEADFIELD.data)) { \
    if (g->CURRFIELD->next) {                                    \
      g->CURRFIELD = g->CURRFIELD->next;                         \
      assert(g->CURRFIELD->len == 0);                            \
    } else {                                                     \
      __typeof__(g->CURRFIELD) tmp__ =                           \
        rmem_alloc(g->a->memalloc, sizeof(g->HEADFIELD)).p;      \
      if (check_alloc(g, tmp__))                                 \
        return ERRRET;                                           \
      tmp__->len = 0;                                            \
      tmp__->next = g->CURRFIELD;                                \
      g->CURRFIELD = tmp__;                                      \
    }                                                            \
  }                                                              \
  &g->CURRFIELD->data[g->CURRFIELD->len++];                      \
})

#define NAME_LOOKUP(g, name, namelen) ({          \
  uintptr* vp__ = smap_lookup(&g->names, (name), (namelen)); \
  vp__ ? *(gnamed**)vp__ : NULL;                             \
})

static gblock* nullable find_target_gblock(gfun* fn, rnode_t* referrer) {
  for (u32 i = 0; i < fn->blocks.len; i++) {
    gblock* b = rarray_at(gblock, &fn->blocks, i);
    if (nodename_eq(referrer, b->name, b->namelen))
      return b;
  }
  return NULL;
}

static const char* gnamedtype_name(gnamedtype t) {
  switch ((enum gnamedtype)t) {
    case GNAMED_T_FUN:   return "function";
    case GNAMED_T_BLOCK: return "label";
    case GNAMED_T_DATA:  return "data";
    case GNAMED_T_CONST: return "constant";
  }
  return "?";
}

static bool check_named_ref(
  gstate* g, rnode_t* referrer, u32 referreri, grefflag flags, gnamed* target)
{
  rop_t op = RSM_GET_OP(*rarray_at(rin_t, &g->iv, referreri));
  const char* expected;
  switch (op) {
    case rop_CALL:
    case rop_TSPAWN:
      if (target->namedtype != GNAMED_T_FUN) {
        expected = "function";
        break;
      }
      // using function instruction index means we can address
      // 2^23 = 8,388,608 instructions (2^23*4 = 33,554,432 bytes) with Au imm
      // and much more by putting a larger instruction index in R(A).
      if (((gfun*)target)->i > RSM_MAX_Au) {
        // TODO generate R(A)=dist instrs using COPY/COPYV
        panic("pc distance too large");
      }
      return true;

    case rop_JUMP:
      if (target->namedtype != GNAMED_T_FUN && target->namedtype != GNAMED_T_BLOCK) {
        expected = "function or label";
        break;
      }
      return true;

    default:
      return true;
  }
  const char* scrubcheck = rmem_scrubcheck(target, sizeof(*target));
  ERRN(referrer, "expected %s, got %s (0x%x, name \"%.*s\", scrubcheck: %s)",
    expected, gnamedtype_name(target->namedtype), target->namedtype,
    target->namelen >= 0xbb ? 0 : (int)target->namelen, target->name,
    scrubcheck);
  return false;
}

typedef u8 ropenc;
enum ropenc {
  ropenc__,
  ropenc_A,
  ropenc_Au,
  ropenc_As,
  ropenc_AB,
  ropenc_ABv,
  ropenc_ABu,
  ropenc_ABs,
  ropenc_ABC,
  ropenc_ABCu,
  ropenc_ABCs,
  ropenc_ABCD,
  ropenc_ABCDu,
  ropenc_ABCDs,
} RSM_END_ENUM(ropenc)

static const i64 ropenc_limittab[][2] = { // ropenc limits {min,max}
  /* _ */      {0,          0},
  /* A */      {0,          RSM_MAX_REG},
  /* Au */     {0,          RSM_MAX_Au},
  /* As */     {RSM_MIN_As, RSM_MAX_As},
  /* AB */     {0,          RSM_MAX_REG},
  /* ABv */    {I64_MIN,    I64_MAX}, // TODO: shouldn't max be U64_MAX?
  /* ABu */    {0,          RSM_MAX_Bu},
  /* ABs */    {RSM_MIN_Bs, RSM_MAX_Bs},
  /* ABC */    {0,          RSM_MAX_REG},
  /* ABCu */   {0,          RSM_MAX_Cu},
  /* ABCs */   {RSM_MIN_Cs, RSM_MAX_Cs},
  /* ABCD */   {0,          RSM_MAX_REG},
  /* ABCDu */  {0,          RSM_MAX_Du},
  /* ABCDs */  {RSM_MIN_Ds, RSM_MAX_Ds},
};

static void patch_lastregarg(gstate* g, rnode_t* patcher, rin_t* in, u32 regno) {
  assert(regno <= RSM_MAX_REG);
  *in = RSM_SET_i(*in, 0); // clear immediate flag

  #define p__(OP, RES)     // no arguments (handled by default case)
  #define p_A(OP, RES)     case rop_##OP: *in = RSM_SET_A(*in, regno); break;
  #define p_Au(OP, RES)    case rop_##OP: *in = RSM_SET_A(*in, regno); break;
  #define p_As(OP, RES)    case rop_##OP: *in = RSM_SET_A(*in, regno); break;
  #define p_AB(OP, RES)    case rop_##OP: *in = RSM_SET_B(*in, regno); break;
  #define p_ABv(OP, RES)   // imm only (handled by default case)
  #define p_ABu(OP, RES)   case rop_##OP: *in = RSM_SET_B(*in, regno); break;
  #define p_ABs(OP, RES)   case rop_##OP: *in = RSM_SET_B(*in, regno); break;
  #define p_ABC(OP, RES)   case rop_##OP: *in = RSM_SET_C(*in, regno); break;
  #define p_ABCu(OP, RES)  case rop_##OP: *in = RSM_SET_C(*in, regno); break;
  #define p_ABCs(OP, RES)  case rop_##OP: *in = RSM_SET_C(*in, regno); break;
  #define p_ABCD(OP, RES)  case rop_##OP: *in = RSM_SET_D(*in, regno); break;
  #define p_ABCDu(OP, RES) case rop_##OP: *in = RSM_SET_D(*in, regno); break;
  #define p_ABCDs(OP, RES) case rop_##OP: *in = RSM_SET_D(*in, regno); break;

  // route opcode to instruction encoder
  switch (RSM_GET_OP(*in)) {
    #define _(OP, ENC, RES, ...) p_##ENC(OP, RES)
    RSM_FOREACH_OP(_)
    #undef _
    default:
      assertf(0,"attempting to patch instruction without register arguments");
      return;
  }
  #undef p__
  #undef p_A
  #undef p_Au
  #undef p_As
  #undef p_AB
  #undef p_ABv
  #undef p_ABu
  #undef p_ABs
  #undef p_ABC
  #undef p_ABCu
  #undef p_ABCs
  #undef p_ABCD
  #undef p_ABCDu
  #undef p_ABCDs
}

// returns false if memory allocation failed
static bool make_bigv(gstate* g, rop_t op, rin_t* inp, u32 dstreg, u64 value) {
  static_assert(sizeof(rin_t) == sizeof(u32), "");
  *inp = RSM_MAKE_ABv(op, dstreg, 1 + (value > U32_MAX));
  rin_t* imm1 = GARRAY_PUSH_OR_RET(rin_t, &g->iv, false);
  if (value <= U32_MAX) {
    *imm1 = (u32)value;
  } else {
    rin_t* imm2 = GARRAY_PUSH_OR_RET(rin_t, &g->iv, false);
    *imm1 = (value >> 32) & U32_MAX;
    *imm2 = value & U32_MAX;
  }
  return true;
}

static bool rin_uses_ireg(rin_t in, u32 reg) {
  #define fr(N) ( RSM_GET_##N(in) == reg )
  #define fw(N) ( (RSM_GET_i(in) == 0) && (RSM_GET_##N(in) == reg) )

  #define fi__     return false;
  #define fi_A     return fr(A);
  #define fi_Au    return fw(A);
  #define fi_As    return fw(A);
  #define fi_AB    return fr(A) | fr(B);
  #define fi_ABv   return fr(A) | fr(B);
  #define fi_ABu   return fr(A) | fw(B);
  #define fi_ABs   return fr(A) | fw(B);
  #define fi_ABC   return fr(A) | fr(B) | fr(C);
  #define fi_ABCu  return fr(A) | fr(B) | fw(C);
  #define fi_ABCs  return fr(A) | fr(B) | fw(C);
  #define fi_ABCD  return fr(A) | fr(B) | fr(C) | fr(D);
  #define fi_ABCDu return fr(A) | fr(B) | fr(C) | fw(D);
  #define fi_ABCDs return fr(A) | fr(B) | fr(C) | fw(D);

  switch (RSM_GET_OP(in)) {
    #define _(OP, ENC, ...) case rop_##OP: fi_##ENC
    RSM_FOREACH_OP(_)
    #undef _
  }

  return false;
}

// select_scratchreg attempts to find an unused register
static u32 find_unused_reg(gstate* g, u32 before_iv_index) {
  u32 r = RSM_NTMPREGS - 1;
  assert(before_iv_index <= g->iv.len);
try_next:
  for (u32 i = before_iv_index; i > 0; ) {
    rin_t* in = rarray_at(rin_t, &g->iv, --i);
    if (rin_uses_ireg(*in, r)) {
      if (r == 0)
        return RSM_NREGS;
      r--;
      goto try_next;
    }
  }
  // r is unused
  return r;
}

// select_scratchreg is used to check for interference with dstreg in arguments.
// If dstreg is used for any of the arguments, RSM_NREGS is returned instead of dstreg.
inline static u32 select_scratchreg(rin_t in, u32 dstreg, u32 nregargs) {
  if ( (nregargs > 2 && RSM_GET_B(in) == dstreg) ||
       (nregargs > 3 && RSM_GET_C(in) == dstreg) )
  {
    return RSM_NREGS; // interference
  }
  return dstreg;
}

// patch_imm sets last argument of the rin_t at g->iv[inindex] to value.
// Resulting value (last argument of the rin_t) is assigned to *argp.
// returns true if result/patched arg is an immediate value rather than a register.
static bool patch_imm(gstate* g, rnode_t* patcher, u32 inindex, u64 value, i32* argp) {
  assert(inindex < g->iv.len);
  rin_t* in = rarray_at(rin_t, &g->iv, inindex);
  u32 scratchreg = RSM_NREGS; // if >-1, names a reg largeval can safely use

  #define SCRATCHREG_A_nil  (RSM_NREGS+1)
  #define SCRATCHREG_A_mem  (RSM_NREGS+2)
  #define SCRATCHREG_A_reg  RSM_GET_A(*in)

  #define U(OP, ENC, NARGS, SCRATCHREG) \
    case rop_##OP: \
      if (value > RSM_MAX_##ENC) { \
        scratchreg = select_scratchreg(*in, SCRATCHREG, NARGS); \
        goto largeval; \
      } \
      *in = RSM_SET_##ENC(*in, (u32)value); \
      break;

  #define S(OP, ENC, NARGS, SCRATCHREG) \
    case rop_##OP: \
      if ((i64)value < RSM_MIN_##ENC || (i64)value > RSM_MAX_##ENC) { \
        scratchreg = select_scratchreg(*in, SCRATCHREG, NARGS); \
        goto largeval; \
      } \
      *in = RSM_SET_##ENC(*in, (i32)value); \
      break;

  #define p__(OP, RES)
  #define p_A(OP, RES)
  #define p_Au(OP, RES)    U(OP, Au, 1, RSM_NREGS)
  #define p_As(OP, RES)    S(OP, As, 1, RSM_NREGS)
  #define p_AB(OP, RES)
  #define p_ABv(OP, RES)   assertf(0,"TODO");
  #define p_ABu(OP, RES)   U(OP, Bu, 2, SCRATCHREG_A_##RES)
  #define p_ABs(OP, RES)   S(OP, Bs, 2, SCRATCHREG_A_##RES)
  #define p_ABC(OP, RES)
  #define p_ABCu(OP, RES)  U(OP, Cu, 3, SCRATCHREG_A_##RES)
  #define p_ABCs(OP, RES)  S(OP, Cs, 3, SCRATCHREG_A_##RES)
  #define p_ABCD(OP, RES)
  #define p_ABCDu(OP, RES) U(OP, Du, 4, SCRATCHREG_A_##RES)
  #define p_ABCDs(OP, RES) S(OP, Ds, 4, SCRATCHREG_A_##RES)

  // route opcode to instruction encoder
  switch (RSM_GET_OP(*in)) {
    #define _(OP, ENC, RES, ...) p_##ENC(OP, RES)
    RSM_FOREACH_OP(_)
    #undef _
    default:
      assertf(0,"attempting to patch non-imm instruction at iv[%u]", inindex);
      return false;
  }

  // if we get here, value was small enough to fit as an immediate
  *argp = (i32)value;
  trace("op %s; small value %d -> imm", rop_name(RSM_GET_OP(*in)), *argp);
  return true;

  #undef U
  #undef S

largeval:
  // Does not fit in immediate value for the instruction.
  // Synthesize instructions to compute the value in a register.
  trace("op %s; large value 0x%llx", rop_name(RSM_GET_OP(*in)), value);

  if (scratchreg >= RSM_NREGS) {
    // We were unable to use the destination register for the instruction requiring
    // the imm. Either one of its inputs uses the same register or the instruction
    // does not produce a register result. Either way, we need to find a register to
    // use for our intermediate value.

    // TODO: this implementation is terrible. Replace it with something better.
    //
    // One idea is to find live ranges, kind of what a linear-scan register allocator
    // would need to do.
    //
    // We could also improve scanning by looking only in the current function.
    // gfun.i contains the iv index of a function's first instruction.
    // Since we might be patching instructions anywhere in the program, we would have
    // to find the function of the patchee (not the current function being generated.)
    // Currently the only way to do that is linear search over gstate.funs, finding
    // the gfun.i closest to inindex.

    scratchreg = find_unused_reg(g, inindex);
    if (scratchreg == RSM_NREGS) {
      errf(g->a, nposrange(patcher), "could not find a free scratch register");
      return false;
    }
    trace("using R%u as scratchreg", scratchreg);
  }

  // result in register scratchreg
  *argp = (i32)scratchreg;

  // table of rop => ropenc which we use to look up the encoding for COPY
  const ropenc openctab[RSM_OP_COUNT] = {
    #define _(OP, ENC, ...) ropenc_##ENC,
    RSM_FOREACH_OP(_)
    #undef _
  };

  // allocate instruction
  u32 subinindex = g->iv.len;
  rin_t* inp = GARRAY_PUSH_OR_RET(rin_t, &g->iv, false);

  // copymax is the max immediate value for COPY.
  assertf(ropenc_limittab[openctab[rop_COPY]][0] == 0,
    "COPY imm is assumed to be unsigned");
  u64 copymax = (u64)ropenc_limittab[openctab[rop_COPY]][1];

  // can we fit it into a COPY immediate?
  if (value <= copymax) {
    // yes we can -- single copy instruction
    trace("make COPY");
    *inp = RSM_MAKE_ABu(rop_COPY, scratchreg, (u32)value);
  } else {
    // No, we need to use a variable-length copyv instruction.
    // Here, make_bigv creates a copyv at *inp, appending a second instruction if
    // the value is larger than U32_MAX. This means that
    // Note that we don't care about return value from make_bigv; in case it fails to
    // allocate memory it reports an error with errf and returns false.
    trace("make COPYV");
    make_bigv(g, rop_COPYV, inp, scratchreg, value);
  }

  // set last arg of instruction "in" to register number scratchreg
  // TODO: Can we remove this? genop() sets it regardless using RSM_MAKE_##ENC(...)
  patch_lastregarg(g, patcher, in, scratchreg);

  // move inp...immN above patchee
  rarray_move(rin_t, &g->iv, inindex, subinindex, g->iv.len);

  return false;
}

static i32 ref_pcval(u32 referreri, gbhead* b, grefflag flags) {
  b->nrefs++; // increment refcount
  if (flags & REF_ABS)
    return (i32)b->i;
  referreri++;
  return referreri > b->i ? -(i32)(referreri - b->i) : (i32)(b->i - referreri);
}

// resolves pending PC arguments (jumps, calls and branch targets) pointing to b
static void gpostresolve_pc(gstate* g, rarray* refs, gbhead* b) {

  trace("%u refs to %s \"%.*s\"",
    refs->len,
    gnamedtype_name(((gnamed*)b)->namedtype),
    (int)(((gnamed*)b)->namelen), ((gnamed*)b)->name);

  for (u32 i = refs->len; i-- ; ) {
    gref* ref = rarray_at(gref, refs, i);
    assert(assertnotnull(ref->n)->t == RT_NAME);
    if (!nodename_eq(ref->n, b->name, b->namelen))
      continue;

    if LIKELY(check_named_ref(g, ref->n, ref->i, ref->flags, (gnamed*)b)) {

      #ifdef ASMGEN_TRACE
        char tmp[128];
        rin_t* ref_in = rarray_at(rin_t, &g->iv, ref->i);
        rsm_fmtinstr(tmp, sizeof(tmp), *ref_in, NULL, 0);
        trace("  referrer: %4x %s (gref.flags 0x%x)", ref->i, tmp, ref->flags);
      #endif

      i32 ignore, val = ref_pcval(ref->i, b, ref->flags);
      patch_imm(g, ref->n, ref->i, (u64)val, &ignore);

      // also remove from udnames
      if (refs != &g->udnames) for (u32 i = g->udnames.len; i-- ; ) {
        gref* ref2 = rarray_at(gref, &g->udnames, i);
        if (ref2->n == ref->n) {
          rarray_remove(gref, &g->udnames, i, 1);
          break;
        }
      }
    }

    rarray_remove(gref, refs, i, 1);
  }
}

// called after entire AST has been processed; resolve data references
static void resolve_undefined_names(gstate* g) {
  trace("udnames.len %u", g->udnames.len);
  for (u32 i = 0; i < g->udnames.len; i++ ) {
    gref* ref = rarray_at(gref, &g->udnames, i);

    assert(assertnotnull(ref->n)->t == RT_NAME);
    const char* name = ref->n->sval.p;
    u32 namelen = ref->n->sval.len;
    assert(namelen > 0);

    gnamed* target = ref->target;
    if (!target) {
      target = NAME_LOOKUP(g, name, namelen);
      if UNLIKELY(!target) {
        ERRN(ref->n, "undefined name \"%.*s\"", (int)namelen, name);
        continue;
      }
    }

    if (target->namedtype == GNAMED_T_CONST) {
      // TODO patch constant uses
      ERRN(ref->n, "constant must be declared before it's referenced");
      continue;
    }

    if (target->namedtype != GNAMED_T_DATA) {
      ERRN(ref->n, "%.*s is not data", (int)namelen, name);
      continue;
    }

    gdata* d = (gdata*)target;
    d->nrefs++;

    dlog_datalayout("patching data reference %.*s (gdata %p, addr 0x%llx)",
      (int)namelen, name, d, d->addr);

    i32 ignore;
    patch_imm(g, ref->n, ref->i, d->addr, &ignore);
  }
  g->udnames.len = 0;
}


// refnamed sets *argp by looking up named thing at refn.
// returns true if resulting *argp is an immediate value (rather than a register.)
static bool refnamed(gstate* g, rnode_t* refn, u32 refi, u32 argc, i32* argp) {
  rop_t op = RSM_GET_OP(*rarray_at(rin_t, &g->iv, refi));

  grefflag flags = 0;
  if (op == rop_JUMP || op == rop_CALL || op == rop_TSPAWN)
    flags |= REF_ABS;

  #ifdef ASMGEN_TRACE
    char tmp[128];
    rin_t* ref_in = rarray_at(rin_t, &g->iv, refi);
    rsm_fmtinstr(tmp, sizeof(tmp), *ref_in, NULL, 0);
    trace("%4x %s (argc %u, flags %u)", refi, tmp, argc, flags);
  #endif

  #define ADDGREF(refs) ({                             \
    gref* ref__ = GARRAY_PUSH_OR_RET(gref, (refs), 0); \
    ref__->i = refi;                                   \
    ref__->n = assertnotnull(refn);                    \
    ref__->flags = flags;                              \
    ref__->target = NULL;                              \
    ref__;                                             \
  })

  assert(refn->sval.len > 0);
  *argp = 0; // make sure *argp is set, even in case of error

  // look for local label
  gblock* b = find_target_gblock(g->fn, refn);
  if (b) {
    if UNLIKELY(!check_named_ref(g, refn, refi, flags, (gnamed*)b))
      return false;
    *argp = ref_pcval(refi, (gbhead*)b, flags);
    return true;
  }

  // look for global function, data or constant
  gnamed* target = NAME_LOOKUP(g, refn->sval.p, refn->sval.len);
  if (!target) {
    // not found; register in "undefined names" and return
    if (RSM_OP_ACCEPTS_PC_ARG(op)) {
      // also register as possible label/fun ref
      flags |= REF_ANY;
      ADDGREF(&assertnotnull(g->fn)->ulv);
    }
    ADDGREF(&g->udnames);
    return true;
  }

  // Found something!
  // Check to make sure the target is valid (e.g. check jump pc distance bounds.)
  if UNLIKELY(!check_named_ref(g, refn, refi, flags, target))
    return false;

  switch ((enum gnamedtype)target->namedtype) {
    case GNAMED_T_FUN:
    case GNAMED_T_BLOCK:
      *argp = ref_pcval(refi, (gbhead*)target, flags);
      return true;

    case GNAMED_T_CONST: {
      target->nrefs++;

      // get the constant's value (e.g. 0xBEEF for "const x = 0xBEEF").
      u64 value = *(const u64*)assertnotnull( ((gdata*)target)->initp );
      trace("constant value: 0x%llx", value);

      // patch_imm sets last argument of the rin_t at g->iv[refi] to value
      return patch_imm(g, refn, refi, value, argp);
    }

    case GNAMED_T_DATA:
      // mutable gdata is resolved later, after data layout is done
      ADDGREF(&g->udnames)->target = target;
      return true;
  }

  UNREACHABLE;
  return true;
  #undef ADDGREF
}

static u8 nregno(gstate* g, rnode_t* n) {
  if UNLIKELY(n->t != RT_IREG && n->t != RT_FREG) {
    ERRN(n, "expected register, got %s", tokname(n->t));
    return 0;
  }
  assert(n->ival <= RSM_MAX_REG); // parser checks this, so no real error checking needed
  return (u8)n->ival;
}

static void errintsize(
  rasm_t* c, rposrange_t pr, rop_t op, i64 minval, u64 maxval, u64 val, bool issigned)
{
  if (minval != 0 || issigned) {
    errf(c, pr, "value %lld out of range %lld...%llu for %s",
      val, minval, maxval, rop_name(op));
  } else {
    errf(c, pr, "value %llu out of range 0...%llu for %s",
      val, maxval, rop_name(op));
  }
}

static void names_assign(gstate* g, gnamed* entry) {
  uintptr* vp = smap_assign(&g->names, entry->name, entry->namelen);
  if (!check_alloc(g, vp))
    *vp = (uintptr)entry;
}

// getiargs checks & reads integer arguments for an operation described by AST rnode n.
// returns true if the last arg is an immediate value.
static bool getiargs(
  gstate* g, rnode_t* n, i32* argv, u32 wantargc, i64 minval, u64 maxval, rin_t** inp)
{
  assert(n->t == RT_OP);
  u32 argc = 0;
  rop_t op = (rop_t)n->ival;

  trace("[%s] wantargc %u, minval %lld, maxval 0x%llx",
    rop_name(op), wantargc, minval, maxval);

  // first argc-1 args are registers
  rnode_t* arg = n->children.head;
  for (; argc < wantargc-1 && arg; argc++, arg = arg->next)
    argv[argc] = nregno(g, arg);

  if UNLIKELY(arg == NULL || arg->next != NULL) {
    if (arg)
      argc += 2;
    goto err_argc;
  }

  u64 val = arg->ival;

  // last arg is either a register, immediate or label (resolved as immediate)
  switch (arg->t) {

  case RT_IREG:
    assert(val <= RSM_MAX_REG); // parser checks this; assert to catch bugs, not input
    argv[argc] = (i32)val;
    return false;

  case RT_NAME: {
    // set register values of instruction for refnamed & co to use.
    rin_t* in = *inp;
    assert(argc < 5);
    switch (argc) {
      case 0: break;
      case 1: *in = RSM_MAKE_A(op, argv[0]); break;
      case 2: *in = RSM_MAKE_AB(op, argv[0], argv[1]); break;
      case 3: *in = RSM_MAKE_ABC(op, argv[0], argv[1], argv[2]); break;
      case 4: *in = RSM_MAKE_ABCD(op, argv[0], argv[1], argv[2], argv[3]); break;
    }

    // set last arg argv[argc] by dereferencing name arg
    bool isimm = refnamed(g, arg, g->iv.len-1, argc+1, &argv[argc]);

    // update instruction in case a patch caused instructions to be generated
    *inp = rarray_at(rin_t, &g->iv, g->iv.len - 1);

    return isimm;
  }

  case RT_SINTLIT2: case RT_SINTLIT: case RT_SINTLIT16:
    if UNLIKELY((i64)val > (i64)maxval || (i64)val < minval) {
      errintsize(g->a, nposrange(arg), op, minval, maxval, val, /*issigned*/true);
      return false;
    }
    if UNLIKELY(RSM_OP_IS_BR(op) && val == 0)
      warnf(g->a, nposrange(arg), "zero jump offset for %s has no effect", rop_name(op));
    argv[argc] = (i32)val;
    return true;

  case RT_INTLIT2: case RT_INTLIT: case RT_INTLIT16:
    if UNLIKELY(val > maxval) {
      errintsize(g->a, nposrange(arg), op, minval, maxval, val, /*issigned*/false);
      return false;
    }
    if UNLIKELY(RSM_OP_IS_BR(op) && val == 0)
      warnf(g->a, nposrange(arg), "zero jump offset for %s has no effect", rop_name(op));
    argv[argc] = (i32)val;
    return true;

  default:
    ERRN(arg, "expected register or immediate integer, got %s", tokname(arg->t));
    argv[argc] = 0;
    return false;
  }

err_argc:
  if (argc < wantargc) {
    ERRN(n, "not enough arguments for %s; want %u, got %u", rop_name(op), wantargc, argc);
  } else {
    ERRN(n, "too many arguments for %s; want %u, got %u", rop_name(op), wantargc, argc);
  }
  return false;
}


static void genop(gstate* g, rnode_t* n);
static void genassign(gstate* g, rnode_t* n);


static void genop_call(gstate* g, rnode_t* n) {
  if UNLIKELY(n->children.head == NULL) {
    ERRN(n, "missing call destination");
    return;
  }

  trace("");

  // desugar call operands, converting e.g.
  //   call foo R5 3
  // to
  //   copy R0 R5
  //   copy R1 0x3
  //   call foo
  rnode_t* n_tmp;
  rnode_t* arg = n->children.head->next;
  if (!arg)
    goto gen_call;
  rnode_t dst_tmp = {0};
  rnode_t assign_tmp = {0};
  for (u32 dstreg = 0; arg; arg = arg->next, dstreg++) {
    // synthesize "dst = arg"
    if UNLIKELY(dstreg == RSM_NARGREGS) {
      // TODO: implement stack push of 8th+ arguments
      ERRN(arg, "too many arguments in call");
      break;
    }
    assert(arg->t != RT_OP && arg->t != RT_ASSIGN);
    dst_tmp.t = RT_IREG;
    dst_tmp.ival = dstreg;
    dst_tmp.next = arg;
    n_tmp = arg->next; arg->next = NULL;
    assign_tmp.t = RT_ASSIGN;
    assign_tmp.children.head = &dst_tmp;
    assign_tmp.children.tail = arg;
    genassign(g, &assign_tmp);
    arg->next = n_tmp;
  }

gen_call:
  // save & disconnect arguments
  n_tmp = n->children.head->next;
  n->children.head->next = NULL;

  // generate CALL instruction
  rin_t* in = GARRAY_PUSH_OR_RET(rin_t, &g->iv);
  *in = RSM_MAKE__(rop_CALL); // needed for refnamed to set flags correctly
  i32 genarg;
  if (getiargs(g, n, &genarg, 1, 0, RSM_MAX_Au, &in)) {
    *in = RSM_MAKE_Au(rop_CALL, genarg);
  } else {
    *in = RSM_MAKE_A(rop_CALL, genarg);
  }

  // restore arguments
  n->children.head->next = n_tmp;
}


static void genop(gstate* g, rnode_t* n) {
  assert(n->t == RT_OP);
  assert(n->ival < RSM_OP_COUNT);

  rop_t op = (rop_t)n->ival;

  trace("[%s]", rop_name(op));

  if (op == rop_CALL)
    return genop_call(g, n);

  // add placeholder instruction
  rin_t* in = GARRAY_PUSH_OR_RET(rin_t, &g->iv);
  *in = RSM_MAKE__(op); // all args are zeroed; ie R0 for register values

  // storage for ABCD args to RSM_MAKE_* macros
  i32 arg[4] = {0};

  //dlog_asm(g->a->memalloc, (const void*)g->iv.v, g->iv.len);

  // route opcode to instruction encoder
  switch (op) {
    #define _(OP, ENC, ...) case rop_##OP: goto make_##ENC;
    RSM_FOREACH_OP(_)
    #undef _
  }

  // instruction encoders
  #define NOIMM(argc, ENC, args...)                     \
    if (getiargs(g, n, arg, argc, 0, RSM_MAX_REG, &in)) \
      goto noimm;                                       \
    *in = RSM_MAKE_##ENC(op, args);                     \
    return;

  #define RoIMM(argc, ENC, ENCi, minval, maxval, args...) \
    trace("[%s] call getiargs", rop_name(op)); \
    if (getiargs(g, n, arg, argc, minval, maxval, &in)) { \
      /* last arg is immediate value */ \
      trace("[%s] g.iv[%lu] = RSM_MAKE_" #ENCi "(%d, %d, %d, %d)", \
        rop_name(op), ((uintptr)in - (uintptr)g->iv.v)/sizeof(rin_t), \
        arg[0], arg[1], arg[2], arg[3]); \
      *in = RSM_MAKE_##ENCi(op, args);                    \
      return; \
    } \
    /* last arg is a register number */ \
    trace("[%s] g.iv[%lu] = RSM_MAKE_" #ENC "(%d, %d, %d, %d)", \
      rop_name(op), ((uintptr)in - (uintptr)g->iv.v)/sizeof(rin_t), \
      arg[0], arg[1], arg[2], arg[3]); \
    *in = RSM_MAKE_##ENC(op, args);                     \
    goto finalize_reg_op; \

  #define RvIMM(argc, ENCv, minval, maxval, args...) {                \
    if (!getiargs(g, n, arg, argc, minval, maxval, &in))              \
      goto no_r;                                                      \
    u64 value = assertnotnull(nlastchild(n))->ival;                   \
    assert(argc == 2); /* fix if we add V ops with different arity */ \
    make_bigv(g, op, in, arg[0], value);                              \
    return;                                                           \
  }

  DIAGNOSTIC_IGNORE_PUSH("-Wunused-label")
  make__:
    if UNLIKELY(n->children.head)
      ERRN(n, "%s does not accept any arguments", rop_name(op));
    *in = RSM_MAKE__(op);
    return;
  make_A:     NOIMM(1, A          ,                         arg[0])
  make_Au:    RoIMM(1, A, Au      , 0,          RSM_MAX_Au, arg[0])
  make_As:    RoIMM(1, A, As      , RSM_MIN_As, RSM_MAX_As, arg[0])
  //make_AB:    NOIMM(2, AB         ,                         arg[0], arg[1])
  make_ABv:   RvIMM(2, ABv        , 0,          U64_MAX,    arg[0], arg[1])
  make_ABu:   RoIMM(2, AB, ABu    , 0,          RSM_MAX_Bu, arg[0], arg[1])
  make_ABs:   RoIMM(2, AB, ABs    , RSM_MIN_Bs, RSM_MAX_Bs, arg[0], arg[1])
  //make_ABC:   NOIMM(3, ABC        ,                         arg[0], arg[1], arg[2])
  make_ABCu:  RoIMM(3, ABC, ABCu  , 0,          RSM_MAX_Cu, arg[0], arg[1], arg[2])
  make_ABCs:  RoIMM(3, ABC, ABCs  , RSM_MIN_Cs, RSM_MAX_Cs, arg[0], arg[1], arg[2])
  //make_ABCD:  NOIMM(4, ABCD       ,                         arg[0], arg[1], arg[2], arg[3])
  make_ABCDu: RoIMM(4, ABCD, ABCDu, 0,          RSM_MAX_Du, arg[0], arg[1], arg[2], arg[3])
  make_ABCDs: RoIMM(4, ABCD, ABCDs, RSM_MIN_Ds, RSM_MAX_Ds, arg[0], arg[1], arg[2], arg[3])
  DIAGNOSTIC_IGNORE_POP()
  #undef RoIMM
  #undef NOIMM

finalize_reg_op:
  // eliminate instructions that have no effect, e.g. "COPY R1 R1"
  if (op == rop_COPY && RSM_GET_A(*in) == RSM_GET_B(*in)) {
    u32 inindex = (u32)( ((uintptr)in - (uintptr)g->iv.v) / sizeof(rin_t) );
    trace("rarray_remove g.iv[%u]", inindex);
    rarray_remove(rin_t, &g->iv, inindex, 1);
  }
  return;

noimm: // the operation does not accept an immediate-value as the last argument
  ERRN(n, "last argument to %s must be a register", rop_name(op));
  return;
no_r:
  ERRN(n, "last argument to %s must be an immediate value", rop_name(op));
  return;
}


static void genassign(gstate* g, rnode_t* n) {
  // convert assignment to op
  // TODO: don't mutate n; instead use a stack-local copy
  assertf(n->t == RT_ASSIGN, "n->t=%s", tokname(n->t));
  n->t = RT_OP;
  rnode_t* lhs = assertnotnull(n->children.head);
  rnode_t* rhs = assertnotnull(lhs->next);
  assertnull(rhs->next); // n must only have two operands

  if (rhs->children.head) {
    // a = op b c  ⟶  op a b c
    assertf(rhs->t == RT_OP, "%s", tokname(rhs->t));
    n->ival = rhs->ival;
    lhs->next = rhs->children.head;
  } else {
    // a = b  ⟶  move a b
    assert(rhs->t != RT_OP);
    n->ival = rop_COPY;
    // use COPYV for large values
    switch (rhs->t) {
      case RT_INTLIT2:
      case RT_INTLIT:
      case RT_INTLIT16:
        if (rhs->ival > RSM_MAX_Bu)
          n->ival = rop_COPYV;
        break;
      case RT_SINTLIT2:
      case RT_SINTLIT:
      case RT_SINTLIT16:
        if ((i64)rhs->ival > RSM_MAX_Bs || (i64)rhs->ival < RSM_MIN_Bs)
          n->ival = rop_COPYV;
        break;
      default:
        break;
    }
  }

  return genop(g, n);
}

static bool gdata_typesize(gstate* g, rnode_t* type, u16* alignp, usize* sizep) {
  switch (type->t) {
    case RT_I1:  *alignp = 1; *sizep = 1; return true;
    case RT_I8:  *alignp = 1; *sizep = 1; return true;
    case RT_I16: *alignp = 2; *sizep = 2; return true;
    case RT_I32: *alignp = 4; *sizep = 4; return true;
    case RT_I64: *alignp = 8; *sizep = 8; return true;
    case RT_ARRAY: {
      rnode_t* elemtype = assertnotnull(type->children.head);
      usize elemsize;
      if UNLIKELY(!gdata_typesize(g, elemtype, alignp, &elemsize))
        return false;
      if (check_mul_overflow(elemsize, (usize)type->ival, sizep)) {
        ERRN(type, "array too large; %zu×%zu", elemsize, (usize)type->ival);
        return false;
      }
      return true;
    }
    default:
      ERRN(type, "invalid type %s of data", tokname(type->t));
      return false;
  }
}

static void gendata(gstate* g, rnode_t* datn) {
  assert(datn->t == RT_DATA || datn->t == RT_CONST);

  // example: "data hello i32 = 123"
  // (DATA foo        (CONST bar
  //   (I32)            (I32)
  //   (INT10 123))     (INT10 123))
  // (DATA foo
  //   (I32))

  gdata* d = GSLAB_ALLOC(g, datavhead, datavcurr);
  d->namedtype = datn->t == RT_CONST ? GNAMED_T_CONST : GNAMED_T_DATA;
  d->name = datn->sval.p;
  d->namelen = datn->sval.len;
  d->nrefs = 0;
  d->align = 1;
  d->size = 0;
  d->origin = datn;
  d->initp = NULL;
  d->initlen = 0;

  rnode_t* type = assertnotnull(datn->children.head);
  if UNLIKELY(!gdata_typesize(g, type, &d->align, &d->size))
    return;

  rnode_t* init = type->next;
  if (init) {
    if (tokisintlit(init->t)) {
      init->ival = htole64(init->ival); // make sure byte order is LE
      d->initp = &init->ival;
      d->initlen = MIN(8lu, d->size);
    } else if (init->t == RT_STRLIT) {
      assert(type->t == RT_ARRAY);
      d->initp = init->sval.p;
      d->initlen = (usize)init->sval.len;
    } else {
      ERRN(init, "invalid value %s for data", tokname(init->t));
    }
  } else if UNLIKELY(d->namedtype == GNAMED_T_CONST) {
    ERRN(datn, "missing initial value for constant %.*s", (int)d->namelen, d->name);
    return;
  }

  assertf(IS_POW2(d->align), "align=%u", d->align);
  assertf(IS_ALIGN2(d->size, d->align), "size=%zu, align=%u", d->size, d->align);
  assertf((u64)d->initlen <= d->size, "initlen=%zu, size=%zu", d->initlen, d->size);

  // TODO: check that init value fits in type

  names_assign(g, (gnamed*)d);
}

static void genblock(gstate* g, rnode_t* block) {
  assert(block->t == RT_LABEL);
  assertnotnull(g->fn);

  // Register block. This is the only place where we initialize a new gblock
  gblock* b = GARRAY_PUSH_OR_RET(gblock, &g->fn->blocks);
  b->namedtype = GNAMED_T_BLOCK;
  b->name = block->sval.p;
  b->namelen = block->sval.len;
  b->nrefs = 0;
  b->i = g->iv.len;
  b->pos = block->pos;

  // resolve pending references
  gpostresolve_pc(g, &g->fn->ulv, (gbhead*)b);

  rnode_t* jumpn = NULL; // last unconditional jump
  for (rnode_t* cn = block->children.head; cn; cn = cn->next) {
    if (jumpn) {
      warnf(g->a, nposrange(cn), "unreachable code");
      break;
    }
    switch (cn->t) {
      case RT_OP:
        if ((rop_t)cn->ival == rop_JUMP || (rop_t)cn->ival == rop_RET)
          jumpn = cn;
        genop(g, cn); break;
      case RT_ASSIGN:
        genassign(g, cn); break;
      case RT_DATA:
      case RT_CONST:
        gendata(g, cn); break;
      default:
        ERRN(cn, "invalid block element %s", tokname(cn->t));
    }
  }
}

static void genfun(gstate* g, rnode_t* fun) {
  assert(fun->t == RT_FUN);

  // This is the only place where we initialize a new gfun
  gfun* fn = GSLAB_ALLOC(g, fnvhead, fnvcurr);
  fn->namedtype = GNAMED_T_FUN;
  fn->name = fun->sval.p;
  fn->namelen = fun->sval.len;
  fn->nrefs = 0;
  fn->i = g->iv.len;
  fn->blocks = (rarray){0};
  fn->fi = g->funs.len - 1; // TODO only exported functions' table index
  fn->ulv = (rarray){0};

  names_assign(g, (gnamed*)fn);

  // resolve pending references
  gpostresolve_pc(g, &g->udnames, (gbhead*)fn);

  // get body by traversing the function rnode's linked list
  rnode_t* params = fun->children.head;
  rnode_t* results = params->next;
  rnode_t* body = results->next;
  if (!body) // just a function declaration
    return;

  // reuse ulv storage from previously-generated function
  if (g->fn != NULL) {
    fn->ulv.v   = g->fn->ulv.v;
    fn->ulv.cap = g->fn->ulv.cap;
  }
  g->fn = fn;

  // generate function body
  for (rnode_t* cn = body->children.head; cn; cn = cn->next)
    genblock(g, cn);

  // make sure the last instruction of the last block of a function is RET or TCALL
  if (fn->i == g->iv.len) { // no function body
    *GARRAY_PUSH_OR_RET(rin_t, &g->iv) = RSM_MAKE__(rop_RET);
  } else {
    rin_t endin = *rarray_at(rin_t, &g->iv, g->iv.len - 1);
    if (RSM_GET_OP(endin) != rop_RET && RSM_GET_OP(endin) != rop_JUMP)
      *GARRAY_PUSH_OR_RET(rin_t, &g->iv) = RSM_MAKE__(rop_RET);
  }

  // report unresolved labels
  for (u32 i = 0; i < fn->ulv.len; i++) {
    gref* ref = rarray_at(gref, &fn->ulv, i);
    if (ref->flags&REF_ANY) // there's also an entry in g->udnames
      continue;
    rnode_t* n = ref->n;
    ERRN(n, "undefined label \"%.*s\"", (int)n->sval.len, n->sval.p);
  }

  // report unused labels
  for (u32 i = 0; i < fn->blocks.len; i++) {
    gblock* b = rarray_at(gblock, &fn->blocks, i);
    if (b->nrefs == 0 && strcmp(b->name, kBlock0Name) != 0) {
      warnf(g->a, (rposrange_t){.focus=b->pos}, "unused label \"%.*s\"",
        (int)b->namelen, b->name);
    }
  }
}

static void dlog_gdata(gdata* nullable d) {
  #ifdef DEBUG_LOG_DATALAYOUT
  if (!d) {
    dlog("ROM data layout:\nADDRESS            NAME             SIZE  ALIGN  DATA");
    return;
  }
  char reprbuf[128];
  char zerobuf[32];
  abuf_t s = abuf_make(reprbuf, sizeof(reprbuf));

  if (d->initp && d->initlen > 0) {
    assert((usize)d->initlen <= d->size);
    abuf_reprhex(&s, d->initp, d->initlen);
    // zero pad remaining bytes
    usize nbyte_remaining = d->size - d->initlen;
    if (nbyte_remaining > 0) {
      abuf_c(&s, ' ');
      usize z = MIN(sizeof(zerobuf), nbyte_remaining);
      memset(zerobuf, 0, z);
      abuf_reprhex(&s, zerobuf, z);
    }
  } else {
    usize nbyte = MIN(sizeof(zerobuf), d->size);
    memset(zerobuf, 0, nbyte);
    abuf_reprhex(&s, zerobuf, nbyte);
  }

  int namemax = 10, datamax = 30;
  int namelen = d->namelen, datalen = (int)s.len;
  const char* nametail = "", *datatail = "";
  if (namelen > namemax) { namemax--; namelen = namemax; nametail = "…"; }
  if (datalen > datamax) { datamax--; datalen = datamax; datatail = "…"; }
  log("0x%016llx %-*.*s%s %10zu     %2u  %.*s%s",
    d->addr,
    namemax, namelen, d->name, nametail,
    d->size, d->align,
    datalen, reprbuf, datatail);
  #endif
}

static int gdata_sort(const gdata** x, const gdata** y, void* ctx) {
  return (int)(*y)->align - (int)(*x)->align;
}

static void layout_data(gstate* g) {
  if (g->datavcurr->len == 0) {
    g->datasize = 0;
    return;
  }

  // put gdata to be included in an array for sorting
  usize datacount = 0;
  for (gdataslab* slab = &g->datavhead; slab && slab->len; slab = slab->next)
    datacount += slab->len;
  if UNLIKELY(!rarray_reserve(gdata*, &g->dataorder, g->a->memalloc, datacount))
    return errf(g->a, (rposrange_t){0}, "out of memory");
  usize datalen = 0;
  for (gdataslab* slab = &g->datavhead; slab; slab = slab->next) {
    if (slab->len == 0)
      break;
    for (usize i = 0; i < slab->len; i++) {
      assert(datalen < datacount);
      gdata* d = &slab->data[i];
      if UNLIKELY(d->namedtype == GNAMED_T_CONST)
        continue; // don't include constants
      *rarray_at(gdata*, &g->dataorder, datalen++) = &slab->data[i];
    }
  }
  g->dataorder.len = datalen;
  if (datalen == 0)
    return;

  // sort data chunks by alignment
  rsm_qsort(g->dataorder.v, g->dataorder.len, sizeof(void*),
    (rsm_qsort_cmp)&gdata_sort, NULL);

  // align is what we will use for the "align" field in the ROMs "data" table header
  // (note: largest alignment is g->data.v[0] after sorting)
  g->dataalign = (*rarray_at(gdata*, &g->dataorder, 0))->align;
  u64 addr = VM_ADDR_MIN;
  dlog_gdata(NULL); // header

  if (g->dataalign > RSM_ROM_ALIGN)
    return errf(g->a, (rposrange_t){0}, "data alignment %u too large", g->dataalign);

  for (u32 i = 0; i < g->dataorder.len; i++) {
    gdata* d = *rarray_at(gdata*, &g->dataorder, i);
    assertf(d->align > 0 && IS_POW2(d->align), "%u", d->align);
    d->addr = ALIGN2(addr, d->align);
    addr += (u64)d->size;
    dlog_gdata(d);
  }
  g->datasize = addr - VM_ADDR_MIN;
}

static rerr_t rom_on_filldata(void* base, void* gp) {
  gstate* g = gp;
  for (u32 i = 0; i < g->dataorder.len; i++) {
    gdata* d = *rarray_at(gdata*, &g->dataorder, i);
    void* dst = base + (usize)(d->addr - VM_ADDR_MIN);
    usize nbyte = d->size;
    assertf(d->align > 0 && IS_POW2(d->align), "%u", d->align);
    if (d->initp) {
      // copy initial value
      assert(d->initlen <= d->size);
      memcpy(dst, d->initp, d->initlen);
      if (d->size < (usize)d->align) // zero pad
        memset(dst + d->size, 0, (usize)d->align - d->size);
      nbyte -= d->initlen;
      dst += d->initlen;
    }
    // zero rest
    memset(dst, 0, nbyte);
  }
  return 0;
}

static void report_unresolved(gstate* g) {
  // report unresolved references
  for (u32 i = 0; i < g->udnames.len; i++) {
    gref* ref = rarray_at(gref, &g->udnames, i);
    rnode_t* n = ref->n;
    ERRN(n, "undefined name \"%.*s\"", (int)n->sval.len, n->sval.p);
  }

  for (gdataslab* slab = &g->datavhead; slab; slab = slab->next) {
    if (slab->len == 0)
      break;
    for (usize i = 0; i < slab->len; i++) {
      gdata* d = &slab->data[i];
      if UNLIKELY(d->nrefs == 0) {
        warnf(g->a, nposrange(d->origin), "unused %s %.*s",
          gnamedtype_name(d->namedtype), (int)d->namelen, d->name);
      }
    }
  }
}

static gstate* nullable init_gstate(rasm_t* a) {
  gstate* g = rasm_gstate(a);
  if (!g) {
    g = rmem_alloct(a->memalloc, gstate);
    if (!g)
      return NULL;
    memset(g, 0, sizeof(gstate));
    rasm_gstate_set(a, g);
    g->a = a;
    smap_make(&g->names, a->memalloc, 16, MAPLF_2);
    // preallocate instruction buffer
    rarray_grow(&g->iv, a->memalloc, sizeof(rin_t), 512/sizeof(rin_t));
  } else {
    // recycle gstate
    g->udnames.len = 0;
    g->funs.len = 0;
    g->iv.len = 0;
    g->dataorder.len = 0;
    g->datasize = 0;
    g->dataalign = 0;
    for (gdataslab* s = &g->datavhead; s; s = s->next)
      s->len = 0;
    for (gfunslab* s = &g->fnvhead; s; s = s->next)
      s->len = 0;
    assertf(g->names.memalloc == a->memalloc, "memory allocator changed");
    smap_clear(&g->names);
  }
  g->datavcurr = &g->datavhead;
  g->fnvcurr = &g->fnvhead;
  return g;
}

rerr_t rasm_gen(rasm_t* a, rnode_t* module, rrom_t* rom) {
  dlog("assembling \"%s\"", a->srcname);
  assert(module->t == RT_LPAREN);
  gstate* g = init_gstate(a);
  if UNLIKELY(g == NULL)
    return rerr_nomem;

  // generate data and functions
  for (rnode_t* cn = module->children.head; cn; cn = cn->next) {
    if (cn->t == RT_DATA || cn->t == RT_CONST) {
      gendata(g, cn);
    } else {
      genfun(g, cn);
    }
  }

  if UNLIKELY(rasm_stop(a) || a->errcount)
    return rerr_invalid;

  // compute data layout and resolve data references
  layout_data(g);
  resolve_undefined_names(g);

  // report unresolved references
  report_unresolved(g);

  // stop if there were errors
  if (a->errcount)
    return rerr_invalid;

  // build ROM image
  rrombuild_t rb = {
    .code = (const rin_t*)g->iv.v,
    .codelen = g->iv.len,
    .datasize = g->datasize,
    .dataalign = g->dataalign,
    .flags = a->flags,
    .userdata = g,
    .filldata = &rom_on_filldata,
  };
  return rom_build(&rb, a->memalloc, rom);
}

void gstate_dispose(gstate* g) {
  rmemalloc_t* ma = g->a->memalloc;
  if (g->fn)
    rarray_free(gref, &g->fn->ulv, ma);
  for (u32 i = 0; i < g->funs.len; i++) {
    gfun* fn = rarray_at(gfun, &g->funs, i);
    rarray_free(gblock, &fn->blocks, ma);
  }
  rarray_free(gref, &g->udnames, ma);
  rarray_free(gfun, &g->funs, ma);
  rarray_free(gfun, &g->dataorder, ma);
  smap_dispose(&g->names);

  for (gdataslab* s = g->datavhead.next; s; ) {
    gdataslab* tmp = s->next;
    rmem_free(ma, RMEM(s, sizeof(gdataslab)));
    s = tmp;
  }
  for (gfunslab* s = g->fnvhead.next; s; ) {
    gfunslab* tmp = s->next;
    rmem_free(ma, RMEM(s, sizeof(gfunslab)));
    s = tmp;
  }

  rmem_free(ma, RMEM(g, sizeof(gstate)));
}


#endif // RSM_NO_ASM
