// assembler: vm code generator
// SPDX-License-Identifier: Apache-2.0
#ifndef RSM_NO_ASM
#include "rsmimpl.h"

#define DEBUG_LOG_DATA // define to log debug messages about data layout

// codegen
typedef struct gstate gstate;
typedef struct gfun   gfun;
typedef struct gblock gblock;
typedef struct gbhead gbhead; // head of gblock and gfun
typedef struct gref   gref;
typedef u8            grefflag;
typedef struct gdata  gdata;
typedef struct gdatav gdatav;

struct gdata {
  const char*    name;
  u32            namelen;
  u32            nrefs;
  u32            align; // byte alignment (1, 2, 4, 8, 16 ...)
  usize          size;  // length of data in bytes
  void* nullable initp; // pointer to initial value, if any
  u64            addr;  // address (valid only after layout)

  gdata* nullable next; // for gstate.datalist
};

struct gdatav {
  gdatav* nullable next;
  usize len; // data[len] == next free entry
  gdata data[32];
};

struct gstate {
  rasm*  a;          // compilation session/context
  rarray iv;         // rinstr[]; instructions
  rarray ufv;        // gref[]; pending undefined function references
  rarray udv;        // gref[]; pending undefined data references
  rarray funs;       // gfun[]; functions
  smap   names;      // name => index in either funs or data fields

  // gdata blocks may change order and may grow with separate memory allocations.
  // Referencing and patching data is made simpler (better?) by using gdata pointers
  // instead of secondary generated IDs. The downside is that we can't use a simple
  // rarray for this since as it grows, the addresses of gdata elements might change.
  // So, we use a list of gdata arrays that we never grow; used as a slab allocator.
  gdatav          datavhead; // first slab + list of additional data slabs
  gdatav*         datavcurr; // current slab
  rarray          dataorder; // gdata*[]; valid after layout, points to gdatav entries
  gdata* nullable datalist;  // list of all in-use gdata (TODO: is this really needed?)
  usize           datasize;  // size of data segment
  u32             dataalign; // alignment of data segment (in bytes)

  gfun* nullable fn; // current function
};

#define GBLOCK_HEAD                                       \
  const char* name;                                       \
  u32         namelen;                                    \
  u32         nrefs; /* reference count */                \
  usize       i;     /* first instruction = iv[starti] */
struct gbhead { GBLOCK_HEAD };

struct gfun {
  GBLOCK_HEAD
  rarray blocks; // gblock[]
  u32    fi;     // function table index (value that fits in Au)
  rarray ulv;    // gref[]; pending undefined references (temporary)
};

struct gblock {
  GBLOCK_HEAD
  rsrcpos pos;
};

struct gref {
  u32      i;     // referrer's iv offset
  rnode*   n;     // referrer
  char     iarg;  // instruction argument to patch; 'A'|'B'|'C'|'D'
  grefflag flags;
};

enum grefflag {
  REF_ANY = 1 << 0, // target is either label or function
  REF_ABS = 1 << 1, // target is an address, not a delta
} RSM_END_ENUM(grefflag)


#define ERRN(n, fmt, args...) errf(g->a, nposrange(n), fmt, ##args)

#define GARRAY_PUSH_OR_RET(T, arrayp, ERRRET...) ({                                  \
  T* vp__ = rarray_push(T, (arrayp), g->a->mem);                                     \
  if (check_alloc(g, vp__)) return ERRRET;                                           \
  vp__; })

static bool check_alloc(gstate* g, void* nullable p) {
  if LIKELY(p != NULL)
    return false;
  errf(g->a, (rposrange){0}, "out of memory");
  return true;
}

static gfun* nullable find_target_gfun(gstate* g, rnode* referrer) {
  assert(referrer->name.len > 0);
  assert(referrer->name.p[0] != '@'); // we use this to encode what vp means
  uintptr* vp = smap_lookup(&g->names, referrer->name.p, referrer->name.len);
  if (!vp)
    return NULL;
  // using function instruction index means we can address
  // 2^23 = 8,388,608 instructions (2^23*4 = 33,554,432 bytes) with Au imm
  // and much more by putting a larger instruction index in R(A).
  gfun* fn = rarray_at_safe(gfun, &g->funs, *vp);
  if (fn->i > RSM_MAX_Au)
    panic("pc distance too large"); // TODO generate R(A)=dist instrs
  return fn;
}

static gblock* nullable find_target_gblock(gfun* fn, rnode* referrer) {
  for (u32 i = 0; i < fn->blocks.len; i++) {
    gblock* b = rarray_at(gblock, &fn->blocks, i);
    if (nodename_eq(referrer, b->name, b->namelen))
      return b;
  }
  return NULL;
}

static i32 addgref( // record a pending reference
  gstate* g, rnode* referrer, usize referreri, rarray* refs,
  char iarg, grefflag flags)
{
  gref* ref = GARRAY_PUSH_OR_RET(gref, refs, 0);
  ref->i = referreri;
  ref->n = assertnotnull(referrer);
  ref->iarg = iarg;
  ref->flags = flags;
  return 0;
}

static i32 ref_pcval(u32 referreri, gbhead* b, grefflag flags) {
  b->nrefs++;
  if (flags&REF_ABS)
    return (i32)b->i;
  referreri++;
  return referreri > b->i ? -(i32)(referreri - b->i) : (i32)(b->i - referreri);
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
  /* ABv */    {I64_MIN,    I64_MAX},
  /* ABu */    {0,          RSM_MAX_Bu},
  /* ABs */    {RSM_MIN_Bs, RSM_MAX_Bs},
  /* ABC */    {0,          RSM_MAX_REG},
  /* ABCu */   {0,          RSM_MAX_Cu},
  /* ABCs */   {RSM_MIN_Cs, RSM_MAX_Cs},
  /* ABCD */   {0,          RSM_MAX_REG},
  /* ABCDu */  {0,          RSM_MAX_Du},
  /* ABCDs */  {RSM_MIN_Ds, RSM_MAX_Ds},
};

static void patch_lastregarg(gstate* g, rnode* patcher, rinstr* in, u32 regno) {
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

// select_scratchreg is used to check for interference with dstreg in arguments.
// If dstreg is used for any of the arguments, RSM_NREGS is returned instead of dstreg.
inline static u32 select_scratchreg(rinstr in, u32 dstreg, u32 nregargs) {
  if ( (nregargs > 1 && RSM_GET_B(in) == dstreg) ||
       (nregargs > 2 && RSM_GET_C(in) == dstreg) ||
       (nregargs > 3 && RSM_GET_D(in) == dstreg) )
  {
    return RSM_NREGS; // interference
  }
  return dstreg;
}

// set immediate value of instruction g->iv[inindex]
static void patch_imm(gstate* g, rnode* patcher, u32 inindex, u64 value) {
  assert(inindex < g->iv.len);
  rinstr* in = rarray_at(rinstr, &g->iv, inindex);
  u32 scratchreg = RSM_NREGS; // if >-1, names a reg largeval can safely use

  #define SCRATCHREG_A_nil RSM_NREGS
  #define SCRATCHREG_A_mem RSM_NREGS
  #define SCRATCHREG_A_reg RSM_GET_A(*in)

  #define U(OP, ENC, NARGS, SCRATCHREG) \
    case rop_##OP: \
      if (value > RSM_MAX_##ENC) { \
        scratchreg = select_scratchreg(*in, SCRATCHREG, NARGS); \
        goto largeval; \
      } \
      *in = RSM_SET_##ENC(*in, (u32)value); return;

  #define S(OP, ENC, NARGS, SCRATCHREG) \
    case rop_##OP: \
      if ((i64)value < RSM_MIN_##ENC || (i64)value > RSM_MAX_##ENC) { \
        scratchreg = select_scratchreg(*in, SCRATCHREG, NARGS); \
        goto largeval; \
      } \
      *in = RSM_SET_##ENC(*in, (u32)value); return;

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
      return;
  }

  #undef U
  #undef S
largeval:
  // Does not fit in immediate value for the instruction.
  // Synthesize instructions to compute the value in a register.

  if (scratchreg == RSM_NREGS) {
    // no scratch register (instruction does not produce a register result)
    errf(g->a, nposrange(patcher), "value too large for instruction immediate");
    return;
  }

  // table of rop => ropenc which we use to look up the encoding for COPY
  const ropenc openctab[RSM_OP_COUNT] = {
    #define _(OP, ENC, ...) ropenc_##ENC,
    RSM_FOREACH_OP(_)
    #undef _
  };

  // can we fit it into a COPY imm?
  assertf(ropenc_limittab[openctab[rop_COPY]][0]==0, "COPY imm is assumed to be unsigned");
  u64 copymax = (u64)ropenc_limittab[openctab[rop_COPY]][1];
  u32 subinindex = g->iv.len;
  rinstr* inp = GARRAY_PUSH_OR_RET(rinstr, &g->iv);
  if (value <= copymax) {
    // yes we can -- single copy instruction
    *inp = RSM_MAKE_ABu(rop_COPY, scratchreg, (u32)value);
  } else {
    // no, we need to use a variable-length copyv instruction
    static_assert(sizeof(rinstr) == sizeof(u32), "");
    if (value <= U32_MAX) {
      *inp = RSM_MAKE_ABv(rop_COPYV, scratchreg, 1);
      rinstr* imm1 = GARRAY_PUSH_OR_RET(rinstr, &g->iv);
      *imm1 = (u32)value;
    } else {
      *inp = RSM_MAKE_ABv(rop_COPYV, scratchreg, 2);
      rinstr* imm1 = GARRAY_PUSH_OR_RET(rinstr, &g->iv);
      rinstr* imm2 = GARRAY_PUSH_OR_RET(rinstr, &g->iv);
      *imm1 = (value >> 32) & U32_MAX;
      *imm2 = value & U32_MAX;
    }
  }

  // if patchee is an op with
  // - no other arguments than the address, and
  // - result register in scratchreg (guaranteed w/ our current logic),
  // then replace that instruction instead of patching it.
  ropenc enc = openctab[RSM_GET_OP(g->iv.v[inindex])];
  if (enc == ropenc_AB || enc == ropenc_ABu || enc == ropenc_ABs) {
    // replace
    rarray_remove(rinstr, &g->iv, inindex, 1);
    subinindex--; // fixup the "source" index of our synthesized instruction(s)
  } else {
    // patch
    patch_lastregarg(g, patcher, in, scratchreg);
  }

  // move inp...immN above patchee
  rarray_move(rinstr, &g->iv, inindex, subinindex, g->iv.len);
}

// resolves pending PC arguments (jumps, calls and branch targets)
static void gpostresolve_pc(gstate* g, rarray* refs, gbhead* b) {
  for (u32 i = refs->len; i-- ; ) {
    gref* ref = rarray_at(gref, refs, i);
    assert(assertnotnull(ref->n)->t == RT_NAME);
    if (!nodename_eq(ref->n, b->name, b->namelen))
      continue;
    b->nrefs++; // increment refcount
    i32 val = ref_pcval(ref->i, b, ref->flags);
    patch_imm(g, ref->n, ref->i, (u64)val);
    rarray_remove(gref, refs, i, 1);
  }
}

static void gpostresolve_data(gstate* g) {
  for (u32 i = 0; i < g->udv.len; i++ ) {
    gref* ref = rarray_at(gref, &g->udv, i);

    assert(assertnotnull(ref->n)->t == RT_GNAME);
    const char* name = ref->n->name.p;
    u32 namelen = ref->n->name.len;
    assert(namelen > 0);
    assert(name[0] == '@'); // we use this to encode what vp means

    uintptr* vp = smap_lookup(&g->names, name, namelen);
    if UNLIKELY(!vp) {
      ERRN(ref->n, "undefined data reference %.*s", (int)namelen, name);
      continue;
    }
    gdata* d = (gdata*)*vp;
    d->nrefs++;

    #ifdef DEBUG_LOG_DATA
    dlog("patching data reference %.*s (gdata %p, addr 0x%llx)",
      (int)namelen, name, d, d->addr);
    #endif

    patch_imm(g, ref->n, ref->i, d->addr);
  }
  g->udv.len = 0;
}

static i32 reffun(gstate* g, rnode* referrer, u32 referreri, char iarg) {
  gfun* fn = find_target_gfun(g, referrer);
  grefflag flags = REF_ABS;
  if (fn) return ref_pcval(referreri, (gbhead*)fn, flags);
  return addgref(g, referrer, referreri, &g->ufv, iarg, flags);
}

static i32 reflabel(gstate* g, rnode* referrer, u32 referreri, char iarg) {
  gblock* b = find_target_gblock(g->fn, referrer);
  if (b) return ref_pcval(referreri, (gbhead*)b, 0);
  return addgref(g, referrer, referreri, &g->fn->ulv, iarg, 0);
}

static i32 reflabelorfun(gstate* g, rnode* referrer, u32 referreri, char iarg) {
  gblock* b = find_target_gblock(g->fn, referrer);
  gfun* fn = find_target_gfun(g, referrer);
  if (b && fn) {
    ERRN(referrer, "\"%.*s\" is confusingly both a label and a function",
      (int)referrer->name.len, referrer->name.p);
    return 0;
  }
  grefflag flags = REF_ABS|REF_ANY;
  if (b)  return ref_pcval(referreri, (gbhead*)b, flags);
  if (fn) return ref_pcval(referreri, (gbhead*)fn, flags);
  addgref(g, referrer, referreri, &g->fn->ulv, iarg, flags);
  return addgref(g, referrer, referreri, &g->ufv, iarg, flags);
}

static i32 refdata(gstate* g, rnode* gname, u32 referreri, char iarg) {
  return addgref(g, gname, referreri, &g->udv, iarg, 0);
}

static u8 nregno(gstate* g, rnode* n) {
  if UNLIKELY(n->t != RT_IREG && n->t != RT_FREG) {
    ERRN(n, "expected register, got %s", tokname(n->t));
    return 0;
  }
  assert(n->ival <= RSM_MAX_REG); // parser checks this, so no real error checking needed
  return (u8)n->ival;
}

static void errintsize(
  rasm* c, rposrange pr, rop op, i64 minval, i64 maxval, u64 val, bool issigned)
{
  if (minval != 0 || issigned) {
    errf(c, pr, "value %lld out of range %lld...%lld for %s",
      (i64)val, minval, maxval, rop_name(op));
  } else {
    errf(c, pr, "value %llu out of range 0...%lld for %s", val, maxval, rop_name(op));
  }
}

// getiargs checks & reads integer arguments for an operation described by AST rnode n.
// returns true if the last arg is an immediate value.
static bool getiargs(gstate* g, rnode* n, i32* argv, u32 wantargc, i64 minval, i64 maxval) {
  assert(n->t == RT_OP);
  u32 argc = 0;
  rop op = (rop)n->ival;

  // first argc-1 args are registers
  rnode* arg = n->children.head;
  for (; argc < wantargc-1 && arg; argc++, arg = arg->next)
    argv[argc] = nregno(g, arg);

  if UNLIKELY(!arg || arg->next != NULL) {
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
    u32 referreri = g->iv.len - 1;
    if (op == rop_CALL) {
      argv[argc] = reffun(g, arg, referreri, 'A');
    } else if (op == rop_JUMP) {
      argv[argc] = reflabelorfun(g, arg, referreri, 'A');
    } else {
      argv[argc] = reflabel(g, arg, referreri, 'B');
    }
    return true;
  }

  case RT_GNAME:
    argv[argc] = refdata(g, arg, g->iv.len - 1, 'B');
    return true;

  case RT_INTLIT2 ... RT_SINTLIT16:
    if UNLIKELY((i64)val > maxval || (i64)val < minval) {
      bool issigned = (arg->t - RT_SINTLIT16) % 2 == 0;
      errintsize(g->a, nposrange(arg), op, minval, maxval, val, issigned);
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


static void genop(gstate* g, rnode* n) {
  assert(n->t == RT_OP);
  assert(n->ival < RSM_OP_COUNT);

  rinstr* in = GARRAY_PUSH_OR_RET(rinstr, &g->iv);
  rop op = (rop)n->ival;
  i32 arg[4]; // ABCD args to RSM_MAKE_* macros

  // route opcode to instruction encoder
  switch (op) {
    #define _(OP, ENC, ...) case rop_##OP: goto make_##ENC;
    RSM_FOREACH_OP(_)
    #undef _
  }

  // instruction encoders
  #define NOIMM(argc, ENC, args...)                            \
    if (getiargs(g, n, arg, argc, 0, RSM_MAX_REG)) goto noimm; \
    *in = RSM_MAKE_##ENC(op, args); return;

  #define RoIMM(argc, ENC, ENCi, minval, maxval, args...) \
    if (getiargs(g, n, arg, argc, minval, maxval)) {      \
      *in = RSM_MAKE_##ENCi(op, args);                    \
    } else {                                              \
      *in = RSM_MAKE_##ENC(op, args);                     \
    }                                                     \
    return;

  #define RvIMM(argc, ENC, ENCi, args...) \
    assertf(0, "TODO");

  DIAGNOSTIC_IGNORE_PUSH("-Wunused-label")
  make__:
    if UNLIKELY(n->children.head)
      ERRN(n, "%s does not accept any arguments", rop_name(op));
    *in = RSM_MAKE__(op);
    return;
  make_A:     NOIMM(1, A          ,                         arg[0])
  make_Au:    RoIMM(1, A, Au      , 0,          RSM_MAX_Au, arg[0])
  make_As:    RoIMM(1, A, As      , RSM_MIN_As, RSM_MAX_As, arg[0])
  make_AB:    NOIMM(2, AB         ,                         arg[0], arg[1])
  make_ABv:   RvIMM(1, A, ABu     ,                         arg[0])
  make_ABu:   RoIMM(2, AB, ABu    , 0,          RSM_MAX_Bu, arg[0], arg[1])
  make_ABs:   RoIMM(2, AB, ABs    , RSM_MIN_Bs, RSM_MAX_Bs, arg[0], arg[1])
  make_ABC:   NOIMM(3, ABC        ,                         arg[0], arg[1], arg[2])
  make_ABCu:  RoIMM(3, ABC, ABCu  , 0,          RSM_MAX_Cu, arg[0], arg[1], arg[2])
  make_ABCs:  RoIMM(3, ABC, ABCs  , RSM_MIN_Cs, RSM_MAX_Cs, arg[0], arg[1], arg[2])
  make_ABCD:  NOIMM(4, ABCD       ,                         arg[0], arg[1], arg[2], arg[3])
  make_ABCDu: RoIMM(4, ABCD, ABCDu, 0,          RSM_MAX_Du, arg[0], arg[1], arg[2], arg[3])
  make_ABCDs: RoIMM(4, ABCD, ABCDs, RSM_MIN_Ds, RSM_MAX_Ds, arg[0], arg[1], arg[2], arg[3])
  DIAGNOSTIC_IGNORE_POP()
  #undef RoIMM
  #undef NOIMM

noimm: // the operation does not accept an immediate-value as the last argument
  ERRN(n, "last argument for %s must be a register", rop_name(op));
}


static void genassign(gstate* g, rnode* n) {
  assert(n->t == RT_EQ);
  // convert to op
  n->t = RT_OP;
  n->ival = rop_COPY;
  rnode* lhs = assertnotnull(n->children.head);
  rnode* rhs = assertnotnull(lhs->next);
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
  }

  return genop(g, n);
}

static void genblock(gstate* g, rnode* block) {
  assert(block->t == RT_LABEL);
  assertnotnull(g->fn);

  // register block
  gblock* b = GARRAY_PUSH_OR_RET(gblock, &g->fn->blocks);
  b->i = g->iv.len;
  b->name = block->name.p;
  b->namelen = block->name.len;
  b->nrefs = 0;
  b->pos = block->pos;

  // resolve pending references
  gpostresolve_pc(g, &g->fn->ulv, (gbhead*)b);

  rnode* jumpn = NULL; // last unconditional jump
  for (rnode* cn = block->children.head; cn; cn = cn->next) {
    if (jumpn) {
      warnf(g->a, nposrange(cn), "unreachable code");
      break;
    }
    switch (cn->t) {
      case RT_OP:
        if ((rop)cn->ival == rop_JUMP || (rop)cn->ival == rop_RET)
          jumpn = cn;
        genop(g, cn); break;
      case RT_EQ:
        genassign(g, cn); break;
      default:
        ERRN(cn, "invalid block element %s", tokname(cn->t));
    }
  }
}

static void names_assign(gstate* g, rnode* name, uintptr v) {
  uintptr* vp = smap_assign(&g->names, name->name.p, name->name.len);
  if (check_alloc(g, vp))
    return;
  *vp = (uintptr)v;
}


static void gendata(gstate* g, rnode* datn) {
  assert(datn->t == RT_GDEF);

  // allocate gdata
  gdatav* dv = g->datavcurr;
  if UNLIKELY(dv->len == countof(dv->data)) {
    // out of space; allocate a new gdatav slab
    dv = rmem_alloc(g->a->mem, sizeof(gdatav));
    if (check_alloc(g, dv))
      return;
    dv->next = g->datavcurr;
    dv->len = 0;
    g->datavcurr = dv;
  }
  gdata* d = &dv->data[dv->len++];
  d->next = g->datalist;
  g->datalist = d;

  // example: "hello = i32 123"
  // (GDEF (NAME hello)
  //       (I32 (INT10 123)))
  rnode* name = assertnotnull(datn->children.head);
  rnode* type = assertnotnull(name->next);
  d->name = name->name.p;
  d->namelen = name->name.len;
  d->nrefs = 0;

  names_assign(g, name, (uintptr)d);

  switch (type->t) {
    case RT_I1:  d->align = 1; d->size = 1; break;
    case RT_I8:  d->align = 1; d->size = 1; break;
    case RT_I16: d->align = 2; d->size = 2; break;
    case RT_I32: d->align = 4; d->size = 4; break;
    case RT_I64: d->align = 8; d->size = 8; break;
    default:
      ERRN(type, "invalid type %s of data", tokname(type->t));
  }

  rnode* init = type->children.head;
  d->initp = NULL;
  if (init) switch (init->t) {
    case RT_SINTLIT2:
    case RT_INTLIT2:
    case RT_SINTLIT:
    case RT_INTLIT:
    case RT_SINTLIT16:
    case RT_INTLIT16:
      init->ival = htole64(init->ival); // make sure byte order is LE
      d->initp = &init->ival;
      break;
    default:
      ERRN(init, "invalid value %s for data", tokname(init->t));
  }
  // TODO: check that init value fits in type
}

static void genfun(gstate* g, rnode* fun) {
  assert(fun->t == RT_FUN);

  // register function
  gfun* fn = GARRAY_PUSH_OR_RET(gfun, &g->funs);
  fn->name = fun->name.p;
  fn->namelen = fun->name.len;
  fn->i = g->iv.len;
  fn->nrefs = 0;
  fn->blocks = (rarray){0};
  fn->fi = g->funs.len - 1; // TODO only exported functions' table index
  fn->ulv = (rarray){0};

  names_assign(g, fun, g->funs.len - 1);

  // resolve pending references
  gpostresolve_pc(g, &g->ufv, (gbhead*)fn);

  // get body by traversing the function rnode's linked list
  rnode* params = fun->children.head;
  rnode* results = params->next;
  rnode* body = results->next;
  if (!body) // just a function declaration
    return;

  // reuse ulv storage from previously-generated function
  if (g->fn != NULL) {
    fn->ulv.v   = g->fn->ulv.v;
    fn->ulv.cap = g->fn->ulv.cap;
  }
  g->fn = fn;

  // generate function body
  for (rnode* cn = body->children.head; cn; cn = cn->next)
    genblock(g, cn);

  // make sure the last instruction of the last block of a function is RET or TCALL
  if (fn->i == g->iv.len) { // no function body
    *GARRAY_PUSH_OR_RET(rinstr, &g->iv) = RSM_MAKE__(rop_RET);
  } else {
    rinstr endin = *rarray_at(rinstr, &g->iv, g->iv.len - 1);
    if (RSM_GET_OP(endin) != rop_RET && RSM_GET_OP(endin) != rop_JUMP)
      *GARRAY_PUSH_OR_RET(rinstr, &g->iv) = RSM_MAKE__(rop_RET);
  }

  // report unresolved labels
  for (u32 i = 0; i < fn->ulv.len; i++) {
    gref* ref = rarray_at(gref, &fn->ulv, i);
    if (ref->flags&REF_ANY) // there's also an entry in g->ufv
      continue;
    rnode* n = ref->n;
    ERRN(n, "undefined label \"%.*s\"", (int)n->name.len, n->name.p);
  }

  // report unused labels
  for (u32 i = 0; i < fn->blocks.len; i++) {
    gblock* b = rarray_at(gblock, &fn->blocks, i);
    if (b->nrefs == 0 && strcmp(b->name, kBlock0Name) != 0) {
      warnf(g->a, (rposrange){.focus=b->pos}, "unused label \"%.*s\"",
        (int)b->namelen, b->name);
    }
  }
}

static void dlog_gdata(gdata* nullable d) {
  #if defined(DEBUG) && defined(DEBUG_LOG_DATA)
  if (!d) {
    dlog("data:\nADDRESS            NAME             SIZE  ALIGN  DATA");
    return;
  }
  char buf[1024];
  abuf s = abuf_make(buf, sizeof(buf));
  if (d->initp) {
    abuf_reprhex(&s, d->initp, d->size);
    if (d->size < (usize)d->align) {
      // pad
      abuf_c(&s, ' ');
      memset(buf + sizeof(buf)/2, 0, sizeof(buf) - sizeof(buf)/2);
      abuf_reprhex(&s, buf + sizeof(buf)/2, d->align - d->size);
    }
  } else {
    memset(buf + sizeof(buf)/2, 0, sizeof(buf) - sizeof(buf)/2);
    abuf_reprhex(&s, buf + sizeof(buf)/2, d->size);
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
    datalen, buf, datatail);
  #endif
}

static int gdata_sort(const gdata** x, const gdata** y, void* ctx) {
  return (int)(*y)->align - (int)(*x)->align;
}

static void layout_gdata(gstate* g) {
  if (g->datavcurr->len == 0) {
    g->datasize = 0;
    return;
  }

  // Sort data chunks by alignment.
  // Put them in an array (dataorder) so we can sort them.
  usize datacount = 0;
  for (gdatav* dv = &g->datavhead; dv && dv->len; dv = dv->next)
    datacount += dv->len;
  if UNLIKELY(!rarray_reserve(gdata*, &g->dataorder, g->a->mem, datacount))
    return errf(g->a, (rposrange){0}, "out of memory");
  g->dataorder.len = datacount;
  for (gdatav* dv = &g->datavhead; dv; dv = dv->next) {
    if (dv->len == 0)
      break;
    for (usize i = 0; i < dv->len; i++) {
      assert(datacount > 0);
      *rarray_at(gdata*, &g->dataorder, --datacount) = &dv->data[i];
    }
  }
  rsm_qsort(g->dataorder.v, g->dataorder.len, sizeof(void*),
    (rsm_qsort_cmp)&gdata_sort, NULL);

  // align is what we will use for the "align" field in the ROMs "data" table header
  // (note: largest alignment is g->data.v[0] after sorting)
  g->dataalign = (*rarray_at(gdata*, &g->dataorder, 0))->align;
  u64 addr = 0;
  dlog_gdata(NULL); // header

  for (u32 i = 0; i < g->dataorder.len; i++) {
    gdata* d = *rarray_at(gdata*, &g->dataorder, i);
    assert(d->align != 0);
    assert(d->align == 1 || CEIL_POW2(d->align) == d->align);
    d->addr = ALIGN2(addr, d->align);
    addr += d->size;
    dlog_gdata(d);
  }
  g->datasize = addr;
}

static rerror rom_on_filldata(void* base, void* gp) {
  gstate* g = gp;
  for (u32 i = 0; i < g->dataorder.len; i++) {
    gdata* d = *rarray_at(gdata*, &g->dataorder, i);
    void* dst = base + d->addr;
    if (d->initp) {
      // copy initial value
      memcpy(dst, d->initp, d->size);
      if (d->size < (usize)d->align) // zero pad
        memset(dst + d->size, 0, (usize)d->align - d->size);
    } else {
      // zero initial value
      memset(dst, 0, MAX(d->align, d->size));
    }
  }
  return 0;
}

static gstate* nullable init_gstate(rasm* a, rmem rommem) {
  gstate* g = rasm_gstate(a);
  if (!g) {
    g = rmem_alloc(a->mem, sizeof(gstate));
    if (!g)
      return NULL;
    memset(g, 0, sizeof(gstate));
    g->a = a;
    rasm_gstate_set(a, g);
    smap_make(&g->names, a->mem, 16, MAPLF_2);
    // preallocate instruction buffer
    rarray_grow(&g->iv, a->mem, sizeof(rinstr), 512/sizeof(rinstr));
  } else {
    // recycle gstate
    g->ufv.len = 0;
    g->udv.len = 0;
    g->funs.len = 0;
    g->iv.len = 0;
    g->dataorder.len = 0;
    g->datasize = 0;
    g->dataalign = 0;
    for (gdatav* v = &g->datavhead; v; v = v->next)
      v->len = 0;
    assertf(g->names.mem.state == a->mem.state, "allocator changed");
    smap_clear(&g->names);
  }
  g->datavcurr = &g->datavhead;
  return g;
}

rerror rasm_gen(rasm* a, rnode* module, rmem rommem, rrom* rom) {
  dlog("assembling \"%s\"", a->srcname);
  assert(module->t == RT_LPAREN);
  gstate* g = init_gstate(a, rommem);
  if UNLIKELY(g == NULL)
    return rerr_nomem;

  // generate data and functions
  for (rnode* cn = module->children.head; cn; cn = cn->next) {
    if (cn->t == RT_GDEF) {
      gendata(g, cn);
    } else {
      genfun(g, cn);
    }
  }

  if UNLIKELY(rasm_stop(a) || a->errcount)
    return rerr_invalid;

  // compute data layout and resolve data references
  layout_gdata(g);
  gpostresolve_data(g);

  // report unresolved function references
  for (u32 i = 0; i < g->ufv.len; i++) {
    gref* ref = rarray_at(gref, &g->ufv, i);
    rnode* n = ref->n;
    ERRN(n, "undefined function%s \"%.*s\"",
      (ref->flags&REF_ANY) ? " or label" : "", (int)n->name.len, n->name.p);
  }

  if (a->errcount)
    return rerr_invalid;

  // build ROM image
  // return gen_romimg(g, rommem, rom);
  rrombuild rb = {
    .code = (const rinstr*)g->iv.v,
    .codelen = g->iv.len,
    .datasize = g->datasize,
    .dataalign = g->dataalign,
    .userdata = g,
    .filldata = &rom_on_filldata,
  };
  return rom_build(&rb, rommem, rom);
}

static void gstate_dispose(gstate* g) {
  rmem mem = g->a->mem;
  if (g->fn)
    rarray_free(gref, &g->fn->ulv, mem);
  for (u32 i = 0; i < g->funs.len; i++) {
    gfun* fn = rarray_at(gfun, &g->funs, i);
    rarray_free(gblock, &fn->blocks, mem);
  }
  rarray_free(gref, &g->ufv, mem);
  rarray_free(gref, &g->udv, mem);
  rarray_free(gfun, &g->funs, mem);
  rarray_free(gfun, &g->dataorder, mem);
  smap_dispose(&g->names);

  for (gdatav* dv = g->datavhead.next; dv; ) {
    gdatav* tmp = dv->next;
    rmem_free(mem, dv, sizeof(gdatav));
    dv = tmp;
  }

  #ifdef DEBUG
  memset(g, 0, sizeof(gstate));
  #endif
  rmem_free(mem, g, sizeof(gstate));
}

void rasm_dispose(rasm* a) {
  gstate* g = rasm_gstate(a);
  if (g)
    gstate_dispose(g);
  #ifdef DEBUG
  memset(a, 0, sizeof(rasm));
  #endif
}


#endif // RSM_NO_ASM
