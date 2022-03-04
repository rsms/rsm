// assembler: vm code generator
// SPDX-License-Identifier: Apache-2.0
#ifndef RSM_NO_ASM
#include "rsmimpl.h"

// codegen
typedef struct gstate gstate;
typedef struct gfun   gfun;
typedef struct gblock gblock;
typedef struct gbhead gbhead; // head of gblock and gfun
typedef struct gref   gref;
struct gstate {
  rasm*          c;      // compilation session/context
  rmem           imem;   // allocator for iv
  rarray         iv;     // rinstr[]; instructions
  rarray         ufv;    // gref[]; pending undefined function references
  rarray         funs;   // gfun[]; functions
  smap           funm;   // name => funs_index
  gfun* nullable fn;     // current function
};
#define GBLOCK_HEAD                                       \
  const char* name;                                       \
  usize       namelen;                                    \
  usize       i;     /* first instruction = iv[starti] */ \
  usize       nrefs; /* reference count */
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
  u32    i;     // referrer's iv offset
  rnode* n;     // referrer
  char   iarg;  // instruction argument to patch; 'A'|'B'|'C'|'D'
  bool   isabs; // target is an address, not a pc delta
  bool   isany; // target is either label or function
};


#define GARRAY_PUSH_OR_RET(T, arrayp, ERRRET...) ({                                  \
  T* vp__ = rarray_push(T, (arrayp), g->c->mem);                                     \
  if UNLIKELY(!vp__) { errf(g->c, (rsrcpos){0,0}, "out of memory"); return ERRRET; } \
  vp__; })

static gfun* nullable find_target_gfun(gstate* g, rnode* referrer) {
  uintptr* vp = smap_lookup(&g->funm, referrer->name.p, referrer->name.len);
  if (!vp)
    return NULL;
  // using function instruction index means we can address
  // 2^23 = 8,388,608 instructions (2^23*4 = 33,554,432 bytes) with Au imm
  // and much more by putting a larger instruction index in R(A).
  gfun* fn = rarray_at(gfun, &g->funs, *vp);
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
  char iarg, bool isabs, bool isany)
{
  gref* ref = GARRAY_PUSH_OR_RET(gref, refs, 0);
  ref->i = referreri;
  ref->n = assertnotnull(referrer);
  ref->iarg = iarg;
  ref->isabs = isabs;
  ref->isany = isany;
  return 0;
}

static i32 ref_pcval(u32 referreri, gbhead* b, bool isabs) {
  b->nrefs++;
  if (isabs)
    return (i32)b->i;
  referreri++;
  return referreri > b->i ? -(i32)(referreri - b->i) : (i32)(b->i - referreri);
}

static void gpostresolve(gstate* g, rarray* refs, gbhead* b) {
  for (u32 i = 0; i < refs->len; ) {
    gref* ref = rarray_at(gref, refs, i);
    assert(assertnotnull(ref->n)->t == RT_NAME);
    if (!nodename_eq(ref->n, b->name, b->namelen)) {
      i++;
      continue;
    }
    b->nrefs++; // increment refcount
    rinstr* in = rarray_at(rinstr, &g->iv, ref->i); // referring instruction
    i32 val = ref_pcval(ref->i, b, ref->isabs);
    switch (ref->iarg) {
      case 'A': *in = ref->isabs ? RSM_SET_Au(*in, val) : RSM_SET_As(*in, val); break;
      case 'B': *in = ref->isabs ? RSM_SET_Bu(*in, val) : RSM_SET_Bs(*in, val); break;
      case 'C': *in = ref->isabs ? RSM_SET_Cu(*in, val) : RSM_SET_Cs(*in, val); break;
      case 'D': *in = ref->isabs ? RSM_SET_Du(*in, val) : RSM_SET_Ds(*in, val); break;
      default: assertf(0,"invalid iarg 0x%02x", ref->iarg);
    }
    rarray_remove(gref, refs, i, 1);
  }
}

static i32 reffun(gstate* g, rnode* referrer, u32 referreri, char iarg, bool isabs) {
  gfun* fn = find_target_gfun(g, referrer);
  if (fn) return ref_pcval(referreri, (gbhead*)fn, isabs);
  return addgref(g, referrer, referreri, &g->ufv, iarg, isabs, false);
}

static i32 reflabel(gstate* g, rnode* referrer, u32 referreri, char iarg, bool isabs) {
  gblock* b = find_target_gblock(g->fn, referrer);
  if (b) return ref_pcval(referreri, (gbhead*)b, isabs);
  return addgref(g, referrer, referreri, &g->fn->ulv, iarg, isabs, false);
}

static i32 refany(gstate* g, rnode* referrer, u32 referreri, char iarg, bool isabs) {
  gblock* b = find_target_gblock(g->fn, referrer);
  gfun* fn = find_target_gfun(g, referrer);
  if (b && fn) {
    errf(g->c, referrer->pos, "\"%.*s\" is confusingly both a label and a function",
      (int)referrer->name.len, referrer->name.p);
    return 0;
  }
  if (b)  return ref_pcval(referreri, (gbhead*)b, isabs);
  if (fn) return ref_pcval(referreri, (gbhead*)fn, isabs);
  addgref(g, referrer, referreri, &g->fn->ulv, iarg, isabs, true);
  return addgref(g, referrer, referreri, &g->ufv, iarg, isabs, true);
}

static u8 nregno(gstate* g, rnode* n) {
  if UNLIKELY(n->t != RT_IREG && n->t != RT_FREG) {
    errf(g->c, n->pos, "expected register, got %s", tokname(n->t));
    return 0;
  }
  assert(n->ival < 32); // parser checks this, so no real error checking needed
  return (u8)n->ival;
}

static void errintsize(
  rasm* c, rsrcpos pos, rop op, i64 minval, i64 maxval, u64 val, bool issigned)
{
  if (minval != 0 || issigned) {
    return errf(c, pos, "value %lld out of range %lld...%lld for %s",
      (i64)val, minval, maxval, rop_name(op));
  }
  errf(c, pos, "value %llu out of range 0...%lld for %s", val, maxval, rop_name(op));
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
    assert(val < 32); // parser checks this; assert to catch bugs, not input
    argv[argc] = (i32)val;
    return false;
  case RT_NAME:
    // TODO: isabs depends on instruction; maybe find a way to automate selection?
    if (op == rop_CALL) {
      argv[argc] = reffun(g, arg, g->iv.len - 1, 'A', /*isabs*/true);
    } else if (op == rop_JUMP) {
      argv[argc] = refany(g, arg, g->iv.len - 1, 'A', /*isabs*/true);
    } else {
      argv[argc] = reflabel(g, arg, g->iv.len - 1, 'B', /*isabs*/false);
    }
    return true;
  case RT_INT2 ... RT_SINT16:
    if UNLIKELY((i64)val > maxval || (i64)val < minval) {
      bool issigned = (arg->t - RT_SINT16) % 2 == 0;
      errintsize(g->c, arg->pos, op, minval, maxval, val, issigned);
      return false;
    }
    if UNLIKELY(RSM_OP_IS_BR(op) && val == 0)
      warnf(g->c, arg->pos, "zero jump offset for %s has no effect", rop_name(op));
    argv[argc] = (i32)val;
    return true;
  default:
    errf(g->c, arg->pos, "expected register or immediate integer, got %s", tokname(arg->t));
    argv[argc] = 0;
    return false;
  }

err_argc:
  if (argc < wantargc) {
    errf(g->c, n->pos, "not enough arguments for %s; want %u, got %u",
      rop_name(op), wantargc, argc);
  } else {
    errf(g->c, n->pos, "too many arguments for %s; want %u, got %u",
      rop_name(op), wantargc, argc);
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
  #define NOIMM(argc, ENC, args...)                   \
    if (getiargs(g, n, arg, argc, 0, 31)) goto noimm; \
    *in = RSM_MAKE_##ENC(op, args); return;

  #define RoIMM(argc, ENC, ENCi, minval, maxval, args...) \
    if (getiargs(g, n, arg, argc, minval, maxval)) {      \
      *in = RSM_MAKE_##ENCi(op, args);                    \
    } else {                                              \
      *in = RSM_MAKE_##ENC(op, args);                     \
    }                                                     \
    return;

  DIAGNOSTIC_IGNORE_PUSH("-Wunused-label")
  make__:
    if UNLIKELY(n->children.head)
      errf(g->c, n->pos, "%s does not accept any arguments", rop_name(op));
    *in = RSM_MAKE__(op);
    return;
  make_A:     NOIMM(1, A          ,                         arg[0])
  make_Au:    RoIMM(1, A, Au      , 0,          RSM_MAX_Au, arg[0])
  make_As:    RoIMM(1, A, As      , RSM_MIN_As, RSM_MAX_As, arg[0])
  make_AB:    NOIMM(2, AB         ,                         arg[0], arg[1])
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
  errf(g->c, n->pos, "last argument for %s must be a register", rop_name(op));
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
    assert(rhs->t == RT_OP);
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
  gpostresolve(g, &g->fn->ulv, (gbhead*)b);

  rnode* jumpn = NULL; // last unconditional jump
  for (rnode* cn = block->children.head; cn; cn = cn->next) {
    if (jumpn) {
      warnf(g->c, cn->pos, "unreachable code");
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
        errf(g->c, cn->pos, "invalid block element %s", tokname(cn->t));
    }
  }
}

static void genfun(gstate* g, rnode* fun) {
  assert(fun->t == RT_FUN);

  // register function
  gfun* fn = rarray_push(gfun, &g->funs, g->c->mem);
  uintptr* vp = smap_assign(&g->funm, fun->name.p, fun->name.len);
  if UNLIKELY(vp == NULL || fn == NULL)
    return errf(g->c, (rsrcpos){0,0}, "out of memory");
  *vp = (uintptr)g->funs.len - 1;
  fn->name = fun->name.p;
  fn->namelen = fun->name.len;
  fn->i = g->iv.len;
  fn->nrefs = 0;
  fn->blocks = (rarray){0};
  fn->fi = g->funs.len - 1; // TODO only exported functions' table index
  fn->ulv = (rarray){0};

  // resolve pending references
  gpostresolve(g, &g->ufv, (gbhead*)fn);

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
    if (ref->isany) // there's also an entry in g->ufv
      continue;
    rnode* n = ref->n;
    errf(g->c, n->pos, "undefined label \"%.*s\"", (int)n->name.len, n->name.p);
  }

  // report unused labels
  for (u32 i = 0; i < fn->blocks.len; i++) {
    gblock* b = rarray_at(gblock, &fn->blocks, i);
    if (b->nrefs == 0 && strcmp(b->name, kBlock0Name) != 0)
      warnf(g->c, b->pos, "unused label \"%.*s\"", (int)b->namelen, b->name);
  }
}


usize rasm_gen(rasm* c, rnode* module, rmem imem, rinstr** resp) {
  dlog("assembling \"%s\"", c->srcname);
  gstate* g = c->_gstate;
  if (!g) {
    g = rmem_alloc(c->mem, sizeof(gstate));
    memset(g, 0, sizeof(gstate));
    g->c = c;
    c->_gstate = g;
    smap_make(&g->funm, c->mem, 16, MAPLF_2);
  } else {
    // recycle gstate
    g->ufv.len = 0;
    g->funs.len = 0;
    g->iv.v = NULL; g->iv.len = 0; g->iv.cap = 0;
    assertf(g->funm.mem.state == c->mem.state, "allocator changed");
    smap_clear(&g->funm);
  }
  g->imem = imem;

  // preallocate instruction buffer to avoid excessive rarray_grow calls
  rarray_grow(&g->iv, imem, sizeof(rinstr), 512/sizeof(rinstr));
  assert(module->t == RT_LPAREN);

  // generate functions
  for (rnode* cn = module->children.head; cn; cn = cn->next)
    genfun(g, cn);
  if UNLIKELY(c->_stop || c->errcount)
    return 0;

  // report unresolved function references
  for (u32 i = 0; i < g->ufv.len; i++) {
    gref* ref = rarray_at(gref, &g->ufv, i);
    rnode* n = ref->n;
    errf(g->c, n->pos, "undefined function%s \"%.*s\"",
      ref->isany ? " or label" : "", (int)n->name.len, n->name.p);
  }

  *resp = (void*)g->iv.v;
  return g->iv.len;
}

void rasm_dispose(rasm* c) {
  gstate* g = c->_gstate;
  if (!g)
    return;
  rmem mem = c->mem;
  if (g->fn)
    rarray_free(gref, &g->fn->ulv, mem);
  for (u32 i = 0; i < g->funs.len; i++) {
    gfun* fn = rarray_at(gfun, &g->funs, i);
    rarray_free(gblock, &fn->blocks, mem);
  }
  rarray_free(usize, &g->ufv, mem);
  rarray_free(usize, &g->funs, mem);
  smap_dispose(&g->funm);
  #ifdef DEBUG
  memset(g, 0, sizeof(gstate));
  c->_gstate = NULL;
  #endif
  rmem_free(mem, g, sizeof(gstate));
}


#endif // RSM_NO_ASM
