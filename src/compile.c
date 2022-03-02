// compiler (parser & assembler)
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

// assembler does
// 1. scan the input for tokens
// 2. parses the semantic meaning of those tokens (ast)
// 3. checks that the ast is valid
// 4. generates code (our vm instructions)

//#define LOG_TOKENS /* define to log() token scanning */
//#define LOG_AST /* define to log() parsed top-level ast nodes */

// source tokens
typedef u8 rtok;
#define RSM_FOREACH_TOKEN(_) \
_( T_END ) \
_( T_COMMENT ) \
/* simple tokens */ \
_( T_LPAREN ) _( T_RPAREN ) \
_( T_LBRACE ) _( T_RBRACE ) \
_( T_SEMI   ) /* ; */ \
_( T_COMMA  ) /* , */ \
_( T_EQ     ) /* = */ \
/* names              */ \
_( T_IREG   ) /* Rn   */ \
_( T_FREG   ) /* Fn   */ \
_( T_LABEL  ) /* foo: */ \
_( T_NAME   ) /* foo  */ \
_( T_OP     ) /* brz */ \
/* literal numbers (order matters; see snumber) */ \
_( T_INT2   ) _( T_SINT2   ) /* 0b1111011       */ \
_( T_INT10  ) _( T_SINT10  ) /* 123, -123       */ \
_( T_INT16  ) _( T_SINT16  ) /* 0x7b            */ \
// end RSM_FOREACH_TOKEN
// RSM_FOREACH_BINOP_TOKEN maps an infix binary operation to opcodes,
// allowing "x + y" as an alternative to "add x y"
#define RSM_FOREACH_BINOP_TOKEN(_) \
_( T_PLUS  , ADD   ) /* + */ \
_( T_MINUS , SUB   ) /* - */ \
_( T_STAR  , MUL   ) /* * */ \
_( T_SLASH , DIV   ) /* / */ \
_( T_PERC  , MOD   ) /* % */ \
_( T_AMP   , AND   ) /* & */ \
_( T_PIPE  , OR    ) /* | */ \
_( T_HAT   , XOR   ) /* ^ */ \
_( T_LT2   , SHL   ) /* << */ \
_( T_GT2   , SHRS  ) /* >> */ \
_( T_GT3   , SHRU  ) /* >>> */ \
_( T_LT    , CMPLT ) /* < */ \
_( T_GT    , CMPGT ) /* > */ \
// end RSM_FOREACH_BINOP_TOKEN
#define RSM_FOREACH_KEYWORD_TOKEN(_) \
_( T_FUN , "fun" ) \
_( T_I1  , "i1"  ) \
_( T_I8  , "i8"  ) \
_( T_I16 , "i16" ) \
_( T_I32 , "i32" ) \
_( T_I64 , "i64" ) \
// end RSM_FOREACH_KEYWORD_TOKEN
enum rtok {
  #define _(name, ...) name,
  RSM_FOREACH_TOKEN(_)
  RSM_FOREACH_BINOP_TOKEN(_)
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _
  rtok_COUNT
} RSM_END_ENUM(rtok)

// kwcount -- total number of keywords
enum {
  #define _(token, ...) _kwcount_t_##token,
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _
  #define _(op, ...) _kwcount_o_##op,
  RSM_FOREACH_OP(_)
  #undef _
  kwcount
};

typedef u8 precedence;
enum precedence {
  PREC_LOWEST,
  PREC_ASSIGN,
  PREC_COMMA,
  PREC_LOGICAL_OR,
  PREC_LOGICAL_AND,
  PREC_BITWISE_OR,
  PREC_BITWISE_XOR,
  PREC_BITWISE_AND,
  PREC_EQUAL,
  PREC_COMPARE,
  PREC_SHIFT,
  PREC_ADD,
  PREC_MULTIPLY,
  PREC_UNARY_PREFIX,
  PREC_UNARY_POSTFIX,
  PREC_MEMBER,
};

// parse state
typedef struct pstate pstate;
struct pstate {
  rcomp*      c;          // compilation session/context
  const char* inp;        // source bytes cursor (source ends with 0x00)
  const char* inend;      // source bytes end
  const char* tokstart;   // start of current token in source
  const char* linestart;  // source position line start pointer (for column)
  u32         lineno;     // source position line
  rtok        tok;        // current token
  bool        insertsemi; // insert T_SEMI before next newline
  bool        isneg;      // true when parsing a negative number
  u64         ival;       // integer value for T_INT* tokens
  usize       nops;       // number of ops we encountered; a hint for codegen
};

// AST
typedef struct node     node;
typedef struct nlist    nlist;
typedef struct strslice strslice;
typedef struct srcpos   srcpos;
struct nlist {
  node* nullable head;
  node* nullable tail;
};
struct strslice {
  const char* p;
  usize len;
};
struct srcpos {
  u32 line, col;
};
struct node {
  node* nullable next; // list link
  rtok           t;    // type
  srcpos         pos;
  union { // field used depends on value of t
    u64      ival;
    strslice name;
  };
  nlist list;
};

// Pratt parselet
typedef struct parselet parselet;
#define PPARAMS pstate* p, precedence prec
#define PARGS   p, prec
typedef node*(*prefixparselet)(PPARAMS); // token...
typedef node*(*infixparselet)(PPARAMS, node* left); // (left token)...
struct parselet {
  prefixparselet nullable prefix;
  infixparselet  nullable infix;
  precedence     prec;
};

// codegen
typedef struct gstate gstate;
typedef struct gfun   gfun;
typedef struct gblock gblock;
typedef struct gbhead gbhead; // head of gblock and gfun
typedef struct gref   gref;
struct gstate {
  rcomp*         c;      // compilation session/context
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
  srcpos pos;
};
struct gref {
  u32   i;     // referrer's iv offset
  node* n;     // referrer
  char  iarg;  // instruction argument to patch; 'A'|'B'|'C'|'D'
  bool  isabs; // target is an address, not a pc delta
  bool  isany; // target is either label or function
};


static const char* kBlock0Name = "b0"; // name of first block

#if HASHCODE_MAX >= 0xFFFFFFFFFFFFFFFFu
static const smapent kwmap_entries[64] = {
 {"fun",3,33},{"brz",3,4877},{0},{0},{0},{"call",4,5645},{0},{"shl",3,3341},
 {"jump",4,5389},{0},{0},{"ret",3,6157},{0},{"load",4,269},{0},{0},
 {"pop",3,1037},{0},{"i16",3,36},{0},{"add",3,1293},{0},{"i32",3,37},{"i1",2,34},
 {0},{"sub",3,1549},{"shrs",4,3597},{0},{"cmpgt",5,4621},{0},{0},{"and",3,2573},
 {0},{0},{0},{"shru",4,3853},{0},{"mod",3,2317},{0},{0},{0},{"scall",5,5901},
 {"mul",3,1805},{0},{"brnz",4,5133},{0},{"i8",2,35},{0},{0},{"cmpeq",5,4109},
 {"xor",3,3085},{0},{"cmplt",5,4365},{"copy",4,13},{0},{"store",5,525},{0},
 {"div",3,2061},{"i64",3,38},{0},{0},{"push",4,781},{0},{"or",2,2829}};
static const struct{u32 cap,len,gcap;maplf lf;hashcode hash0;const smapent* ep;}
kwmap_data={64,31,48,2,0xfc453834,kwmap_entries};
static const smap* kwmap = (const smap*)&kwmap_data;
#else /* can't use static map; check_kwmap will build one */
static const smap* kwmap;
#endif


// istypetok returns true if t represents (or begins the description of) a type
static bool istypetok(rtok t) {
  switch (t) {
    case T_I1:
    case T_I8:
    case T_I16:
    case T_I32:
    case T_I64:
      return true;
    default:
      return false;
  }
}

static bool strsliceeqn(const strslice* s, const char* str, usize len) {
  return s->len == len && memcmp(s->p, str, len) == 0;
}
#define strsliceeq(s,cstr) strsliceeqn((s), (cstr), strlen(cstr))

static void nlist_append(nlist* l, node* n) {
  if (l->tail) {
    l->tail->next = n;
  } else {
    l->head = n;
  }
  l->tail = n;
  assert(n->next == NULL);
}

static void nlist_set2(nlist* l, node* n0, node* n1) {
  l->head = n0;
  l->tail = n1;
  n0->next = n1;
  assert(n1->next == NULL);
}


#define isname(c) (isalnum(c) || (c) == '_')

static const char* tokname(rtok t);
static usize toklen(pstate* p) { // length in bytes of current token
  assert((uintptr)p->inp >= (uintptr)p->tokstart);
  return (usize)(uintptr)(p->inp - p->tokstart);
}

static u32 pcolumn(pstate* p) { // source column of current token
  return (u32)((uintptr)p->tokstart - (uintptr)p->linestart) + 1;
}

static void reportv(rcomp* c, srcpos pos, int code, const char* fmt, va_list ap) {
  if (c->_stop)
    return; // previous call to diaghandler has asked us to stop
  c->diag.code = code;
  c->diag.msg = c->_diagmsg;
  c->diag.srcname = c->srcname;
  c->diag.line = pos.line;
  c->diag.col = pos.col;

  if (code)
    c->errcount++;

  abuf s = abuf_make(c->_diagmsg, sizeof(c->_diagmsg));
  if (pos.line > 0) {
    abuf_fmt(&s, "%s:%u:%u: ", c->srcname, pos.line, pos.col);
  } else {
    abuf_fmt(&s, "%s: ", c->srcname);
  }
  abuf_str(&s, code ? "error: " : "warning: ");
  c->diag.msgshort = s.p;
  abuf_fmtv(&s, fmt, ap);
  abuf_terminate(&s);

  c->_stop = !c->diaghandler(&c->diag, c->userdata);
}

ATTR_FORMAT(printf, 3, 4)
static void errf(rcomp* comp, srcpos pos, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reportv(comp, pos, 1, fmt, ap);
  va_end(ap);
}

ATTR_FORMAT(printf, 3, 4)
static void warnf(rcomp* comp, srcpos pos, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reportv(comp, pos, 0, fmt, ap);
  va_end(ap);
}

ATTR_FORMAT(printf, 2, 3)
static void serr(pstate* p, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reportv(p->c, (srcpos){p->lineno, pcolumn(p)}, 1, fmt, ap);
  va_end(ap);
  p->tok = T_END;
}

ATTR_FORMAT(printf, 3, 4)
static void perr(pstate* p, node* nullable n, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  if (n) {
    reportv(p->c, n->pos, 1, fmt, ap);
  } else {
    reportv(p->c, (srcpos){p->lineno, pcolumn(p)}, 1, fmt, ap);
  }
  va_end(ap);
}

static void scomment(pstate* p) { // line comment "// ... <LF>"
  p->tokstart += 2; // exclude "//"
  while (*p->inp && *p->inp != '\n')
    p->inp++;
  p->tok = T_COMMENT;
}

static bool utf8chomp(pstate* p) {
  // TODO: improve this to be better and fully & truly verify UTF8.
  // Note: UTF8 validation can be done very efficiently with SIMD. See:
  // https://lemire.me/blog/2020/10/20/ridiculously-fast-unicode-utf-8-validation/
  u8 a = (u8)*p->inp++;
  if ((a & 0xc0) != 0xc0 || ((u8)*p->inp & 0xc0) != 0x80)
    return false;
  if (*p->inp++ == 0)   return false; // len<2
  if ((a >> 5) == 0x6)  return true;  // 2 bytes
  if (*p->inp++ == 0)   return false; // len<3
  if ((a >> 4) == 0xE)  return true;  // 3 bytes
  if (*p->inp++ == 0)   return false; // len<4
  if ((a >> 3) == 0x1E) return true;  // 4 bytes
  return false;
}

static void snameunicode(pstate* p) {
  while (p->inp < p->inend) {
    if ((u8)*p->inp >= UTF8_SELF) {
      if (!utf8chomp(p))
        return serr(p, "invalid UTF8 sequence");
    } else if (isname(*p->inp)) {
      p->inp++;
    } else {
      if (*p->inp == ':') {
        p->inp++;
        p->tok = T_LABEL;
      }
      return;
    }
  }
}

static void sname(pstate* p) {
  p->tok = T_NAME;
  while (p->inp < p->inend && isname(*p->inp))
    p->inp++;
  if (p->inp < p->inend && (u8)*p->inp >= UTF8_SELF)
    return snameunicode(p);
  if (p->inp < p->inend && *p->inp == ':') {
    p->inp++;
    p->tok = T_LABEL;
    return;
  }
  p->insertsemi = true;
  uintptr* vp = smap_lookup(kwmap, p->tokstart, toklen(p)); // look up keyword
  if (!vp)
    return;
  p->tok = *vp & 0xff;
  if (*vp > 0xff)
    p->ival = (u64)(*vp >> sizeof(rtok)*8);
}

static rerror snumber1(pstate* p, int base) {
  u64 cutoff = 0xFFFFFFFFFFFFFFFF;
  u64 acc = 0;
  u64 cutlim = cutoff % base;
  cutoff /= base;
  int any = 0;
  rerror err = 0;
  for (u8 c = (u8)*p->inp; p->inp != p->inend; c = *++p->inp) {
    switch (c) {
      case '0' ... '9': c -= '0'; break;
      case 'A' ... 'Z': c -= 'A' - 10; break;
      case 'a' ... 'z': c -= 'a' - 10; break;
      case '_': continue; // ignore
      case '.': err = rerr_not_supported; goto end;
      default: goto end;
    }
    if UNLIKELY(c >= base) {
      err = rerr_invalid;
      goto end;
    }
    if (any < 0 || acc > cutoff || (acc == cutoff && (u64)c > cutlim)) {
      any = -1;
    } else {
      any = 1;
      acc *= base;
      acc += c;
    }
  }
end:
  if (any < 0) {
    err = rerr_overflow;
  } else if (p->isneg) {
    if UNLIKELY(acc > 0x8000000000000000llu)
      err = rerr_overflow;
    if LIKELY(p->tok != T_END) // tok is T_END if an error occurred
      p->tok++; // T_INT -> T_SINT
    p->ival = -acc;
  } else {
    p->ival = acc;
  }
  p->insertsemi = true;
  p->isneg = false;
  return err;
}

static void snumber(pstate* p, int base) {
  rerror err = snumber1(p, base);
  if (err) switch (err) {
    case rerr_not_supported: return serr(p, "floating-point literal not supported");
    case rerr_overflow:      return serr(p, "integer literal too large");
    default:                 return serr(p, "invalid integer literal");
  }
}

static void sreg(pstate* p) {
  assert(p->isneg == false); // or snumber1 will return a garbage token
  rerror err = snumber1(p, 10);
  if UNLIKELY(err || p->ival > 31)
    return serr(p, "invalid register");
}

static void sadvance(pstate* p) { // scan the next token
  const char* linestart = p->linestart;
  while (p->inp < p->inend && isspace(*p->inp)) {
    if (*p->inp == '\n') {
      p->lineno++;
      p->linestart = p->inp + 1;
    }
    p->inp++;
  }

  p->tokstart = p->inp;
  if (linestart != p->linestart && p->insertsemi) {
    p->insertsemi = false;
    p->tok = T_SEMI; return;
  }

  if UNLIKELY(p->inp == p->inend || p->c->_stop) {
    p->tokstart--;
    if (p->insertsemi) {
      p->insertsemi = false;
      p->tok = T_SEMI; return;
    }
    p->tokstart = p->inp;
    p->tok = T_END; return;
  }

  assert(p->isneg == false); // make sure we don't have a bug in snumber
  char c = *p->inp++; // read current byte and advance cursor
  bool insertsemi = p->insertsemi;
  p->insertsemi = false;

  switch (c) {
    case '(': p->tok = T_LPAREN; return;
    case ')': p->insertsemi = true; p->tok = T_RPAREN; return;
    case '{': p->tok = T_LBRACE; return;
    case '}': p->insertsemi = true; p->tok = T_RBRACE; return;
    case '=': p->tok = T_EQ; return;
    case ';': p->tok = T_SEMI; return;
    case ',': p->tok = T_COMMA; return;
    case '+': p->tok = T_PLUS; return;
    case '*': p->tok = T_STAR; return;
    case '%': p->tok = T_PERC; return;
    case '&': p->tok = T_AMP; return;
    case '|': p->tok = T_PIPE; return;
    case '^': p->tok = T_HAT; return;
    case '<':
      if (*p->inp != '<') { p->tok = T_LT; return; }
      p->inp++;             p->tok = T_LT2; return;
    case '>':
      if (*p->inp != '>')   p->tok = T_GT; return;
      p->inp++;
      if (*p->inp != '>') { p->tok = T_GT2; return; }
      p->inp++;             p->tok = T_GT3; return;

    // number or "-"
    case '-': // "-" | "-" numlit
      if (!isdigit(*p->inp)) { p->tok = T_MINUS; return; }
      p->inp++;
      p->isneg = true;
      FALLTHROUGH;
    case '0': switch (*p->inp) {
      case 'B': case 'b': p->inp++; p->tok = T_INT2;  return snumber(p, 2);
      case 'X': case 'x': p->inp++; p->tok = T_INT16; return snumber(p, 16);
    } FALLTHROUGH;
    case '1' ... '9': p->inp--; p->tok = T_INT10; return snumber(p, 10);

    case '/':
      if (*p->inp == '/') {
        p->inp++;
        p->insertsemi = insertsemi;
        // return scomment(p); // alt 1: produce comments
        scomment(p); MUSTTAIL return sadvance(p); // alt 2: ignore comments
      }
      p->tok = T_SLASH; return;
    case 'R': p->tok = T_IREG; return sreg(p);
    case 'F': p->tok = T_FREG; return sreg(p);
    default: // anything else is the start of a name
      if ((u8)c >= UTF8_SELF) {
        p->inp--;
        p->tok = T_NAME;
        return snameunicode(p);
      }
      if UNLIKELY(!isalnum(c) && c != '_' && c != '$')
        return serr(p, "unexpected character");
      return sname(p);
  }
}

#ifdef LOG_TOKENS
  #define sadvance(p) ({ sadvance(p); logpstate(p); (p)->tok; })
  static void logpstate(pstate* p) {
    u32 line = p->lineno, col = pcolumn(p);
    rtok t = p->tok;
    const char* tname = tokname(t);
    const char* tvalp = p->tokstart;
    int tvalc = (int)toklen(p);
    if (t == T_INT2 || t == T_INT10 || t == T_INT16 || t == T_IREG || t == T_FREG) {
      return log("%3u:%-3u %-12s \"%.*s\"\t%llu\t0x%llx",
        line, col, tname, tvalc, tvalp, p->ival, p->ival);
    }
    if (t == T_SINT2 || t == T_SINT10 || t == T_SINT16) { // uses ival
      return log("%3u:%-3u %-12s \"%.*s\"\t%lld\t0x%llx",
        line, col, tname, tvalc, tvalp, (i64)p->ival, p->ival);
    }
    log("%3u:%-3u %-12s \"%.*s\"", line, col, tname, tvalc, tvalp);
  }
#endif

// got comsumes the next token if p->tok == t
static bool got(pstate* p, rtok t) {
  if (p->tok != t)
    return false;
  sadvance(p);
  return true;
}

// sfastforward advances the scanner until one of the tokens in stoplist is encountered.
// stoplist should be NULL-terminated.
static void sfastforward(pstate* p, const rtok* stoplist) {
  const char* inp = p->inp;
  while (p->tok != T_END) {
    const rtok* lp = stoplist;
    while (*lp) {
      if (*lp++ == p->tok)
        goto end;
    }
    sadvance(p);
  }
end:
  if (inp == p->inp)
    sadvance(p);  // guarantee progress
  got(p, T_SEMI); // slurp a trailing semicolon
}

// --- parse


static node* pstmt(PPARAMS);
#define pexpr pstmt

// perrunexpected reports a syntax error along with the current token
static void perrunexpected(
  pstate* p, node* nullable n, const char* expected, const char* got)
{
  perr(p, n, "expected %s, got %s", expected, got);
}

// expect reports a syntax error if p->tok != t
static void expecttok(pstate* p, rtok t) {
  if UNLIKELY(p->tok != t)
    perrunexpected(p, NULL, tokname(t), tokname(p->tok));
}

static bool expecttype(pstate* p, node* n) {
  if UNLIKELY(!istypetok(n->t)) {
    perrunexpected(p, NULL, "type", tokname(n->t));
    return false;
  }
  return true;
}

static bool expectname(pstate* p, node* n) {
  if UNLIKELY(n->t != T_NAME) {
    perrunexpected(p, n, "name", tokname(n->t));
    return false;
  }
  return true;
}

// eat comsumes the next token, reporting a syntax error if p->tok != t
static void eat(pstate* p, rtok t) {
  expecttok(p, t);
  sadvance(p);
}

// mk* functions makes new nodes
static node* mknode(pstate* p) {
  node* n = rmem_alloc(p->c->mem, sizeof(node));
  memset(n, 0, sizeof(node));
  n->t = p->tok;
  n->pos.line = p->lineno;
  n->pos.col = pcolumn(p);
  return n;
}

static node* mklist(pstate* p) {
  node* n = mknode(p);
  n->t = T_LPAREN;
  return n;
}

static node* mknil(pstate* p) {
  node* n = mknode(p);
  n->t = T_END;
  return n;
}

static node* ptype(PPARAMS) {
  node* n = pstmt(PARGS);
  expecttype(p, n);
  return n;
}

// parse any token with an ival
static node* prefix_int(PPARAMS) {
  node* n = mknode(p);
  n->ival = p->ival;
  sadvance(p); // consume token
  return n;
}

// assign = (reg | name) "=" expr ";"
static node* infix_eq(PPARAMS, node* dst) {
  node* n = mknode(p);
  sadvance(p);
  p->nops++;
  node* src = pexpr(PARGS);
  nlist_set2(&n->list, dst, src);
  return n;
}

static node* prefix_name(PPARAMS) {
  node* n = mknode(p);
  n->name.p = p->tokstart;
  n->name.len = toklen(p);
  sadvance(p);
  return n;
}

#define prefix_type prefix_name

static node* pargs(PPARAMS, node* n) {
  while (p->tok != T_SEMI && p->tok != T_END) {
    node* arg = pstmt(PARGS);
    nlist_append(&n->list, arg);
  }
  return n;
}

// operation = op arg*
static node* prefix_op(PPARAMS) {
  node* n = prefix_int(PARGS);
  p->nops++;
  return pargs(PARGS, n);
}

// (reg | name) op expr
static node* infix_op(PPARAMS, node* arg0) {
  node* n = mknode(p);
  // map token to opcode
  switch (n->t) {
    #define _(tok, op) case tok: n->ival = rop_##op; break;
    RSM_FOREACH_BINOP_TOKEN(_)
    #undef _
    default: UNREACHABLE;
  }
  n->t = T_OP;
  sadvance(p);
  p->nops++;
  nlist_append(&n->list, arg0);
  return pargs(PARGS, n);
}

// blockbody = blockstmt*
// blockstmt = operation | assignment
static node* pblockbody(PPARAMS, node* block) {
  for (;;) {
    switch (p->tok) {
      // stop tokens
      case T_RBRACE:
      case T_END:
      case T_LABEL:
        return block;
      // will-parse tokens
      case T_OP:
      case T_EQ:
      case T_IREG:
      case T_FREG:
        break;
      // unexpected
      default: {
        if (p->tok == T_NAME) {
          perr(p, NULL, "unknown operation \"%.*s\"", (int)toklen(p), p->tokstart);
        } else {
          perrunexpected(p, NULL, "operation or assignment", tokname(p->tok));
        }
        const rtok stoplist[] = { T_SEMI, 0 };
        sfastforward(p, stoplist);
        continue;
      }
    }
    node* cn = pstmt(PARGS);
    nlist_append(&block->list, cn);
    eat(p, T_SEMI);
  }
}

// labelblock = name ":" (operation | assignment)*
static node* prefix_label(PPARAMS) {
  node* n = prefix_name(PARGS); // label
  n->name.len--; // trim off trailing ":"
  assert(n->name.p[n->name.len] == ':');
  return pblockbody(PARGS, n);
}

// param = (name type | type)
static node* pparam(PPARAMS) {
  node* n = pexpr(PARGS);
  if (!istypetok(p->tok)) {
    expecttype(p, n); // n should be a type
    return n; // just type
  }
  // both name and type
  expectname(p, n);
  node* typ = ptype(PARGS);
  nlist_append(&n->list, typ);
  return n;
}

// params = param ("," param)*
static node* pparams(PPARAMS) {
  node* n = mklist(p);
  for (;;) {
    nlist_append(&n->list, pparam(PARGS));
    if (!got(p, T_COMMA))
      return n;
  }
}

// fundef = "fun" name "(" params? ")" result? "{" body "}"
// result = params
// body   = (stmt ";")*
static node* prefix_fun(PPARAMS) {
  node* n = mknode(p);
  sadvance(p); // consume token

  // name
  expecttok(p, T_NAME);
  n->name.p = p->tokstart;
  n->name.len = toklen(p);
  sadvance(p);

  // parameters
  eat(p, T_LPAREN);
  if (got(p, T_RPAREN)) {
    nlist_append(&n->list, mknil(p));
  } else {
    nlist_append(&n->list, pparams(PARGS));
    eat(p, T_RPAREN);
  }

  // result
  if (p->tok == T_LBRACE || p->tok == T_SEMI) {
    nlist_append(&n->list, mknil(p));
  } else {
    nlist_append(&n->list, pparams(PARGS));
  }

  if (p->tok == T_SEMI) // function declaration
    return n;

  // body "{" ... "}"
  expecttok(p, T_LBRACE);
  node* block0 = mknode(p); // recycle the "{" token
  node* body = mklist(p); // body starts at position of "{"
  nlist_append(&n->list, body);
  sadvance(p); // consume "{"
  if (got(p, T_RBRACE)) // empty function body
    return n;

  // implicit first block?
  if (p->tok != T_LABEL) {
    block0->t = T_LABEL;
    block0->name.p = kBlock0Name;
    block0->name.len = 2;
    pblockbody(PARGS, block0);
    nlist_append(&body->list, block0);
  }
  while (p->tok == T_LABEL) {
    node* block = prefix_label(PARGS);
    if (body->list.head != NULL && strsliceeq(&block->name, kBlock0Name))
      perr(p, block, "block named %s must be first block", kBlock0Name);
    nlist_append(&body->list, block);
  }
  eat(p, T_RBRACE);
  return n;
}

static const parselet parsetab[rtok_COUNT] = {
  [T_IREG]  = {prefix_int, NULL, PREC_MEMBER},
  [T_FREG]  = {prefix_int, NULL, PREC_MEMBER},
  [T_OP]    = {prefix_op, NULL, PREC_MEMBER},
  [T_LABEL] = {prefix_label, NULL, PREC_MEMBER},
  [T_NAME]  = {prefix_name, NULL, PREC_MEMBER},
  [T_I1]    = {prefix_type, NULL, PREC_MEMBER},
  [T_I8]    = {prefix_type, NULL, PREC_MEMBER},
  [T_I16]   = {prefix_type, NULL, PREC_MEMBER},
  [T_I32]   = {prefix_type, NULL, PREC_MEMBER},
  [T_I64]   = {prefix_type, NULL, PREC_MEMBER},

  [T_EQ]    = {NULL, infix_eq, PREC_MEMBER},

  #define _(tok, ...) [tok] = {NULL, infix_op, PREC_MEMBER},
  RSM_FOREACH_BINOP_TOKEN(_)
  #undef _

  [T_INT2]   = {prefix_int, NULL, PREC_MEMBER},
  [T_SINT2]  = {prefix_int, NULL, PREC_MEMBER},
  [T_INT10]  = {prefix_int, NULL, PREC_MEMBER},
  [T_SINT10] = {prefix_int, NULL, PREC_MEMBER},
  [T_INT16]  = {prefix_int, NULL, PREC_MEMBER},
  [T_SINT16] = {prefix_int, NULL, PREC_MEMBER},

  [T_FUN] = {prefix_fun, NULL, PREC_MEMBER},
};

// stmt = anynode ";"
static node* pstmt(PPARAMS) {
  const parselet* ps = &parsetab[p->tok];

  if UNLIKELY(!ps->prefix) {
    perrunexpected(p, NULL, "statement", tokname(p->tok));
    node* n = mknil(p);
    const rtok stoplist[] = { T_SEMI, 0 };
    sfastforward(p, stoplist);
    return n;
  }

  ATTR_UNUSED const void* p1 = p->inp;
  ATTR_UNUSED bool insertsemi = p->insertsemi;
  node* n = ps->prefix(PARGS);
  assertf(insertsemi != p->insertsemi || (uintptr)p1 < (uintptr)p->inp,
    "parselet did not advance scanner");

  // infix
  while (p->tok != T_END) {
    ps = &parsetab[p->tok];
    if (ps->infix == NULL || ps->prec < prec)
      return n;
    n = ps->infix(PARGS, n);
  }
  return n;
}

// --- ast formatter

#ifdef LOG_AST

  typedef u32 fmtflag;
  enum fmtflag {
    FMT_HEAD = 1 << 0, // is list head
  } RSM_END_ENUM(fmtflag)

  static void fmtnode1(abuf* s, node* n, usize indent, fmtflag fl) {
    if ((fl & FMT_HEAD) == 0) {
      abuf_c(s, '\n');
      for (usize i = 0; i < indent; i++)
        abuf_c(s, ' ');
    }
    fl &= ~FMT_HEAD;
    indent += 2;

    // atoms
    switch ((enum rtok)n->t) {
      case T_I1:
      case T_I8:
      case T_I16:
      case T_I32:
      case T_I64:
        return abuf_str(s, tokname(n->t));
      case T_END:
        return abuf_str(s, "nil");
      default: break;
    }

    abuf_c(s, '(');

    // list
    if (n->t == T_LPAREN) {
      for (node* cn = n->list.head; cn; cn = cn->next) {
        abuf_c(s, ' ');
        fmtnode1(s, cn, indent, cn == n->list.head ? fl | FMT_HEAD : fl);
      }
      abuf_c(s, ' ');
      return abuf_c(s, ')');
    }

    // complex
    abuf_str(s, tokname(n->t));
    abuf_c(s, ' ');
    switch ((enum rtok)n->t) {
      case T_IREG:
      case T_FREG:
        abuf_u64(s, n->ival, 10); break;

      case T_NAME:
      case T_COMMENT:
      case T_LABEL:
      case T_FUN:
        abuf_append(s, n->name.p, n->name.len); break;

      case T_OP:
        abuf_str(s, rop_name((rop)n->ival)); break;

      case T_SINT2:  abuf_c(s, '-'); FALLTHROUGH;
      case T_INT2:   abuf_str(s, "0b"); abuf_u64(s, n->ival, 2); break;
      case T_SINT10: abuf_c(s, '-'); FALLTHROUGH;
      case T_INT10:  abuf_u64(s, n->ival, 10); break;
      case T_SINT16: abuf_c(s, '-'); FALLTHROUGH;
      case T_INT16:  abuf_str(s, "0x"); abuf_u64(s, n->ival, 16); break;

      case T_EQ:
        break; // only list

      default:
        abuf_str(s, "[TODO fmtnode1]");
        break;
    }
    for (node* cn = n->list.head; cn; cn = cn->next) {
      abuf_c(s, ' ');
      fmtnode1(s, cn, indent, fl);
    }
    abuf_c(s, ')');
  }

  static usize fmtnode(char* buf, usize bufcap, node* n) {
    abuf s = abuf_make(buf, bufcap);
    fmtnode1(&s, n, 0, FMT_HEAD);
    return abuf_terminate(&s);
  }

#endif // LOG_AST

static const char* tokname(rtok t) {
  switch (t) {
  #define _(name, ...) case name: return &#name[2]; // [2] to skip "T_" prefix
  RSM_FOREACH_TOKEN(_)
  RSM_FOREACH_BINOP_TOKEN(_)
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _
  }
  return "?";
}

// --- codegen functions


#define GARRAY_PUSH_OR_RET(T, arrayp, ERRRET...) ({                                 \
  T* vp__ = rarray_push(T, (arrayp), g->c->mem);                                    \
  if UNLIKELY(!vp__) { errf(g->c, (srcpos){0,0}, "out of memory"); return ERRRET; } \
  vp__; })

static gfun* nullable find_target_gfun(gstate* g, node* referrer) {
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

static gblock* nullable find_target_gblock(gfun* fn, node* referrer) {
  for (u32 i = 0; i < fn->blocks.len; i++) {
    gblock* b = rarray_at(gblock, &fn->blocks, i);
    if (strsliceeqn(&referrer->name, b->name, b->namelen))
      return b;
  }
  return NULL;
}

static i32 addgref( // record a pending reference
  gstate* g, node* referrer, usize referreri, rarray* refs,
  char iarg, bool isabs, bool isany)
{
  gref* ref = GARRAY_PUSH_OR_RET(gref, refs, 0);
  ref->i = referreri;
  ref->n = referrer;
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
    assert(assertnotnull(ref->n)->t == T_NAME);
    if (!strsliceeqn(&ref->n->name, b->name, b->namelen)) {
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
    rarray_remove(gblock, refs, i, 1);
  }
}

static i32 reffun(gstate* g, node* referrer, u32 referreri, char iarg, bool isabs) {
  gfun* fn = find_target_gfun(g, referrer);
  if (fn) return ref_pcval(referreri, (gbhead*)fn, isabs);
  return addgref(g, referrer, referreri, &g->ufv, iarg, isabs, false);
}

static i32 reflabel(gstate* g, node* referrer, u32 referreri, char iarg, bool isabs) {
  gblock* b = find_target_gblock(g->fn, referrer);
  if (b) return ref_pcval(referreri, (gbhead*)b, isabs);
  return addgref(g, referrer, referreri, &g->fn->ulv, iarg, isabs, false);
}

static i32 refany(gstate* g, node* referrer, u32 referreri, char iarg, bool isabs) {
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

static u8 nregno(gstate* g, node* n) {
  if UNLIKELY(n->t != T_IREG && n->t != T_FREG) {
    errf(g->c, n->pos, "expected register, got %s", tokname(n->t));
    return 0;
  }
  assert(n->ival < 32); // parser checks this, so no real error checking needed
  return (u8)n->ival;
}

static void errintsize(
  rcomp* c, srcpos pos, rop op, i64 minval, i64 maxval, u64 val, bool issigned)
{
  if (minval != 0 || issigned) {
    return errf(c, pos, "value %lld out of range %lld...%lld for %s",
      (i64)val, minval, maxval, rop_name(op));
  }
  errf(c, pos, "value %llu out of range 0...%lld for %s", val, maxval, rop_name(op));
}

// getiargs checks & reads integer arguments for an operation described by AST node n.
// returns true if the last arg is an immediate value.
static bool getiargs(gstate* g, node* n, i32* argv, u32 wantargc, i64 minval, i64 maxval) {
  assert(n->t == T_OP);
  u32 argc = 0;
  rop op = (rop)n->ival;

  // first argc-1 args are registers
  node* arg = n->list.head;
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
  case T_IREG:
    assert(val < 32); // parser checks this; assert to catch bugs, not input
    argv[argc] = (i32)val;
    return false;
  case T_NAME:
    // TODO: isabs depends on instruction; maybe find a way to automate selection?
    if (op == rop_CALL) {
      argv[argc] = reffun(g, arg, g->iv.len - 1, 'A', /*isabs*/true);
    } else if (op == rop_JUMP) {
      argv[argc] = refany(g, arg, g->iv.len - 1, 'A', /*isabs*/true);
    } else {
      argv[argc] = reflabel(g, arg, g->iv.len - 1, 'B', /*isabs*/false);
    }
    return true;
  case T_INT2 ... T_SINT16:
    if UNLIKELY((i64)val > maxval || (i64)val < minval) {
      bool issigned = (arg->t - T_SINT16) % 2 == 0;
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


static void genop(gstate* g, node* n) {
  assert(n->t == T_OP);
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
    if UNLIKELY(n->list.head)
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


static void genassign(gstate* g, node* n) {
  assert(n->t == T_EQ);
  // convert to op
  n->t = T_OP;
  n->ival = rop_COPY;
  node* lhs = assertnotnull(n->list.head);
  node* rhs = assertnotnull(lhs->next);
  assertnull(rhs->next); // n must only have two operands

  if (rhs->list.head) {
    // a = op b c  ⟶  op a b c
    assert(rhs->t == T_OP);
    n->ival = rhs->ival;
    lhs->next = rhs->list.head;
  } else {
    // a = b  ⟶  move a b
    assert(rhs->t != T_OP);
    n->ival = rop_COPY;
  }

  return genop(g, n);
}

static void genblock(gstate* g, node* block) {
  assert(block->t == T_LABEL);
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

  for (node* cn = block->list.head; cn; cn = cn->next) {
    switch (cn->t) {
      case T_OP: genop(g, cn); break;
      case T_EQ: genassign(g, cn); break;
      default:   errf(g->c, cn->pos, "invalid block element %s", tokname(cn->t));
    }
  }
}

static void genfun(gstate* g, node* fun) {
  assert(fun->t == T_FUN);

  // register function
  gfun* fn = rarray_push(gfun, &g->funs, g->c->mem);
  uintptr* vp = smap_assign(&g->funm, fun->name.p, fun->name.len);
  if UNLIKELY(vp == NULL || fn == NULL)
    return errf(g->c, (srcpos){0,0}, "out of memory");
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

  // get body by traversing the function node's linked list
  node* params = fun->list.head;
  node* results = params->next;
  node* body = results->next;
  if (!body) // just a function declaration
    return;

  // reuse ulv storage from previously-generated function
  if (g->fn != NULL) {
    fn->ulv.v   = g->fn->ulv.v;
    fn->ulv.cap = g->fn->ulv.cap;
  }
  g->fn = fn;

  // generate function body
  for (node* cn = body->list.head; cn; cn = cn->next)
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
    node* n = ref->n;
    errf(g->c, n->pos, "undefined label \"%.*s\"", (int)n->name.len, n->name.p);
  }

  // report unused labels
  for (u32 i = 0; i < fn->blocks.len; i++) {
    gblock* b = rarray_at(gblock, &fn->blocks, i);
    if (b->nrefs == 0 && b->name != kBlock0Name)
      warnf(g->c, b->pos, "unused label \"%.*s\"", (int)b->namelen, b->name);
  }
}

// --- main functions

static node* parse(rcomp* c) {
  pstate p = {
    .c         = c,
    .inp       = c->srcdata,
    .inend     = c->srcdata + c->srclen,
    .linestart = c->srcdata,
    .lineno    = 1,
  };
  sadvance(&p); // prime parser with initial token
  node* module = mklist(&p);
  while (p.tok != T_END && !c->_stop) {
    node* n = pstmt(&p, PREC_MEMBER);
    if (p.tok != T_END) // every statement ends with a semicolon
      eat(&p, T_SEMI);
    nlist_append(&module->list, n);

    if UNLIKELY(n->t != T_FUN)
      perr(&p, n, "expected function, got %s", tokname(n->t));

    #ifdef LOG_AST
      char buf[4096];
      fmtnode(buf, sizeof(buf), n);
      log("input:%u:%u: parsed top-level statement:\n%s", n->pos.line, n->pos.col, buf);
    #endif
  }
  return module;
}


static usize codegen(rcomp* c, node* module, rmem imem, rinstr** resp) {
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
  assert(module->t == T_LPAREN);

  // generate functions
  for (node* cn = module->list.head; cn; cn = cn->next)
    genfun(g, cn);
  if UNLIKELY(c->_stop || c->errcount)
    return 0;

  // report unresolved function references
  for (u32 i = 0; i < g->ufv.len; i++) {
    gref* ref = rarray_at(gref, &g->ufv, i);
    node* n = ref->n;
    errf(g->c, n->pos, "undefined function%s \"%.*s\"",
      ref->isany ? " or label" : "", (int)n->name.len, n->name.p);
  }

  *resp = (void*)g->iv.v;
  return g->iv.len;
}


void rcomp_dispose(rcomp* c) {
  gstate* g = c->_gstate;
  if (!g)
    return;

  if (g->fn)
    rarray_free(gref, &g->fn->ulv, c->mem);

  for (u32 i = 0; i < g->funs.len; i++) {
    gfun* fn = rarray_at(gfun, &g->funs, i);
    rarray_free(gblock, &fn->blocks, c->mem);
  }

  rarray_free(usize, &g->ufv, c->mem);
  rarray_free(usize, &g->funs, c->mem);
  smap_dispose(&g->funm);

  #ifdef DEBUG
  memset(g, 0, sizeof(gstate));
  #endif

  rmem_free(c->mem, g, sizeof(gstate));
}


static void check_kwmap() {
#if HASHCODE_MAX < 0xFFFFFFFFFFFFFFFFu || defined(DEBUG)
  static bool didcheck = false; if (didcheck) return; didcheck = true;

  // build kwmap with all keywords
  static u8 memory[1568];
  static smap kwmap2 = {0}; smap* m = &kwmap2;
  rmem mem = rmem_mkbufalloc(memory, sizeof(memory));
  assertnotnull(smap_make(m, mem, kwcount, MAPLF_2)); // increase sizeof(memory)
  #if HASHCODE_MAX < 0xFFFFFFFFFFFFFFFFu
    m->hash0 = fastrand();
  #else
    m->hash0 = kwmap->hash0;
  #endif
  uintptr* vp;
  #define _(token, kw) \
    vp = assertnotnull(smap_assign(m, kw, strlen(kw))); \
    *vp = token;
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _
  #define _(op, enc, kw, ...) \
    vp = assertnotnull(smap_assign(m, kw, strlen(kw))); \
    *vp = (T_OP | ((uintptr)rop_##op << (sizeof(rtok)*8)));
  RSM_FOREACH_OP(_)
  #undef _
  #if HASHCODE_MAX < 0xFFFFFFFFFFFFFFFFu
    kwmap = m; // use m since we have no static map
  #endif
#endif
#if defined(DEBUG) && HASHCODE_MAX >= 0xFFFFFFFFFFFFFFFFu
  // check to see if keywords has changed; if kwmap is outdated
  for (const smapent* e = smap_itstart(m); smap_itnext(m, &e); )
    if (smap_lookup(kwmap, e->key, e->keylen) == NULL) goto differ;
  for (const smapent* e = smap_itstart(kwmap); smap_itnext(kwmap, &e); )
    if (smap_lookup(m, e->key, e->keylen) == NULL) goto differ;
  return; // kwmap is good
differ:   // kwmap is outdated -- optimize and print replacement C code
  #if 1 /* generator disabled -- change to "0" to run it */
    dlog("————————————————————————————————————————————————————————");
    dlog("kwmap needs updating — find me and enable the generator");
    dlog("————————————————————————————————————————————————————————");
  #else /* generator enabled */
    dlog("————————————————————————————————————————————————————————");
    dlog("generating kwmap, running smap_optimize...");
    u8 tmpmemory[sizeof(memory)];
    rmem tmpmem = rmem_mkbufalloc(tmpmemory, sizeof(tmpmemory));
    double score = smap_optimize(m, 5000000, tmpmem);
    dlog("new kwmap hash0 0x%llx, score %f, cap %u, len %u, gcap %u",
      (u64)m->hash0, score, m->cap, m->len, m->gcap);
    char buf[4096];
    smap_cfmt(buf, sizeof(buf), m, "kwmap");
    log("————————————————————————————————————————————————————————");
    log("  Please update %s with the following code:", __FILE__);
    log("————————————————————————————————————————————————————————\n"
        "\n%s\n\n"
        "—————————————————————————————————————————————————————————", buf);
  #endif // generator
  kwmap = m; // use fresh map
#endif // DEBUG
}

// compiler entry point
usize rsm_compile(rcomp* c, rmem resm, rinstr** resp) {
  check_kwmap();
  c->_stop = false;
  c->errcount = 0;
  dlog("assembling \"%s\"", c->srcname);

  node* module = parse(c);
  if UNLIKELY(c->_stop || c->errcount)
    return 0;

  #ifdef LOG_AST
    char buf[4096];
    fmtnode(buf, sizeof(buf), module);
    log("\"%s\" module parsed as:\n%s", c->srcname, buf);
  #endif

  return codegen(c, module, resm, resp);
}

