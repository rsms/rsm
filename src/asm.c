// assembler
#include "rsm.h"
#include "util.h"

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
} END_TYPED_ENUM(rtok)

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
  rasmctx*    ctx;
  const char* inp;         // source bytes cursor (source ends with 0x00)
  const char* inend;       // source bytes end
  const char* tokstart;    // start of current token in source
  const char* linestart;   // source position line start pointer (for column)
  u32         lineno;      // source position line
  rtok        tok;         // current token
  bool        insertsemi;  // insert T_SEMI before next newline
  bool        isneg;       // true when parsing a negative number
  u64         ival;        // integer value for T_INT* tokens
  usize       nops;        // number of ops we encountered; a hint for codegen
};

typedef struct node     node;
typedef struct nlist    nlist;
typedef struct strslice strslice;
struct nlist {
  node* nullable head;
  node* nullable tail;
};
struct strslice {
  const char* p;
  usize len;
};
struct node {
  node* nullable next;      // list link
  rtok           t;         // type
  u32            line, col; // source postion
  union { // field used depends on value of t
    u64      ival;
    strslice name;
  };
  nlist list;
};

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

static const char* kBlock0Name = "b0"; // name of first block

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

static void errv(rasmctx* ctx, u32 line, u32 col, const char* fmt, va_list ap) {
  if (ctx->_stop)
    return; // previous call to diaghandler has asked us to stop
  ctx->diag.code = 1;
  ctx->diag.msg = ctx->_diagmsg;
  ctx->diag.srcname = ctx->srcname;
  ctx->diag.line = line;
  ctx->diag.col = col;
  ctx->errcount++;

  abuf s = abuf_make(ctx->_diagmsg, sizeof(ctx->_diagmsg));
  if (line > 0) {
    abuf_fmt(&s, "%s:%u:%u: error: ", ctx->srcname, line, col);
  } else {
    abuf_fmt(&s, "%s: error: ", ctx->srcname);
  }
  ctx->diag.msgshort = s.p;
  abuf_fmtv(&s, fmt, ap);
  abuf_terminate(&s);

  ctx->_stop = !ctx->diaghandler(&ctx->diag, ctx->userdata);
}

ATTR_FORMAT(printf, 4, 5)
static void errf(rasmctx* ctx, u32 line, u32 col, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  errv(ctx, line, col, fmt, ap);
  va_end(ap);
}

ATTR_FORMAT(printf, 2, 3)
static rtok serr(pstate* p, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  errv(p->ctx, p->lineno, pcolumn(p), fmt, ap);
  va_end(ap);
  return p->tok = T_END;
}

ATTR_FORMAT(printf, 3, 4)
static void perr(pstate* p, node* nullable n, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  if (n) {
    errv(p->ctx, n->line, n->col, fmt, ap);
  } else {
    errv(p->ctx, p->lineno, pcolumn(p), fmt, ap);
  }
  va_end(ap);
}

static rtok scomment(pstate* p) { // line comment "// ... <LF>"
  p->tokstart += 2; // exclude "//"
  while (*p->inp && *p->inp != '\n')
    p->inp++;
  return p->tok = T_COMMENT;
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

static rtok snameunicode(pstate* p) {
  while (*p->inp) {
    u8 b = (u8)*p->inp;
    if (b < UTF8_SELF) {
      if (!isname(*p->inp))
        break;
      p->inp++;
      continue;
    }
    if (!utf8chomp(p))
      return serr(p, "invalid UTF8 sequence");
  }
  return p->tok = T_NAME;
}

static rtok sname(pstate* p) {
  p->tok = T_NAME;
  while (p->inp < p->inend && isname(*p->inp))
    p->inp++;
  if (p->inp < p->inend && (u8)*p->inp >= UTF8_SELF)
    snameunicode(p);
  if (p->inp < p->inend && *p->inp == ':') {
    p->inp++;
    return p->tok = T_LABEL;
  }
  p->insertsemi = true;
  usize len = toklen(p);

  // map keywords to T_ tokens
  #define _(token, kw)                                            \
    if (len == strlen(kw) && memcmp((kw), p->tokstart, len) == 0) \
      return p->tok = token;
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _

  // map operations to T_OP token
  #define _(op, enc, asmname, ...) \
    if (len == strlen(asmname) && memcmp(asmname, p->tokstart, len) == 0) { \
      p->ival = rop_##op;                                                   \
      return p->tok = T_OP;                                                 \
    }
  RSM_FOREACH_OP(_)
  #undef _

  // okay, its just a symbolic name
  return p->tok;
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

static rtok snumber(pstate* p, int base) {
  rerror err = snumber1(p, base);
  if (err) switch (err) {
    case rerr_not_supported: return serr(p, "floating-point literal not supported");
    case rerr_overflow:      return serr(p, "integer literal too large");
    default:                 return serr(p, "invalid integer literal");
  }
  return p->tok;
}

static rtok sreg(pstate* p) {
  assert(p->isneg == false); // or snumber1 will return a garbage token
  rerror err = snumber1(p, 10);
  if (err || p->ival > 31)
    goto invalid;
  return p->tok;
invalid:
  return serr(p, "invalid register");
}

static rtok sadvance(pstate* p) { // scan the next token
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
    return p->tok = T_SEMI;
  }

  if UNLIKELY(p->inp == p->inend || p->ctx->_stop) {
    p->tokstart--;
    if (p->insertsemi) {
      p->insertsemi = false;
      return p->tok = T_SEMI;
    }
    p->tokstart = p->inp;
    return p->tok = T_END;
  }

  assert(p->isneg == false); // make sure we don't have a bug in snumber
  char c = *p->inp++; // read current byte and advance cursor
  bool insertsemi = p->insertsemi;
  p->insertsemi = false;
  // dlog("0x%02x %c", c, isprint(c) ? c : ' ');

  switch (c) {
    case '(': return p->tok = T_LPAREN;
    case ')': p->insertsemi = true; return p->tok = T_RPAREN;
    case '{': return p->tok = T_LBRACE;
    case '}': p->insertsemi = true; return p->tok = T_RBRACE;
    case '=': return p->tok = T_EQ;
    case ';': return p->tok = T_SEMI;
    case ',': return p->tok = T_COMMA;
    case '+': return p->tok = T_PLUS;
    case '*': return p->tok = T_STAR;
    case '%': return p->tok = T_PERC;
    case '&': return p->tok = T_AMP;
    case '|': return p->tok = T_PIPE;
    case '^': return p->tok = T_HAT;
    case '<':
      if (*p->inp != '<') return p->tok = T_LT;
      p->inp++;           return p->tok = T_LT2;
    case '>':
      if (*p->inp != '>') return p->tok = T_GT;
      p->inp++;
      if (*p->inp != '>') return p->tok = T_GT2;
      p->inp++;           return p->tok = T_GT3;

    // number or "-"
    case '-': // "-" | "-" numlit
      if (!isdigit(*p->inp)) return p->tok = T_MINUS;
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
      return p->tok = T_SLASH;
    case 'R': p->tok = T_IREG; return sreg(p);
    case 'F': p->tok = T_FREG; return sreg(p);
    default: // anything else is the start of a name
      if ((u8)c >= UTF8_SELF) {
        p->inp--;
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
  node* n = rmem_allocz(p->ctx->mem, sizeof(node));
  n->t = p->tok;
  n->line = p->lineno;
  n->col = pcolumn(p);
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

  if (p->tok == T_SEMI)
    return n;

  // body "{" ... "}"
  expecttok(p, T_LBRACE);
  node* block0 = mknode(p); // recycle the "{" token
  node* body = mklist(p); // body starts at position of "{"
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
  nlist_append(&n->list, body);
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

  UNUSED const void* p1 = p->inp;
  UNUSED bool insertsemi = p->insertsemi;
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
  } END_TYPED_ENUM(fmtflag)

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

// codegen state
typedef struct gstate    gstate;
typedef struct labelinfo labelinfo;
struct labelinfo {
  const char* name;
  usize       namelen;
  usize       i; // iv offset (location for lv, referrer for ulv)
};
struct gstate {
  rasmctx* ctx;

  // instructions
  rinstr*  iv;
  usize    ilen;
  usize    icap;

  // functions, mapping [function index] => iv offset
  usize*   fv;
  usize    flen;
  usize    fcap;

  // defined labels
  labelinfo* lv;
  usize      llen;
  usize      lcap;

  // undefined label references
  labelinfo* ulv;
  usize      ullen;
  usize      ulcap;
};

static u8 nregno(gstate* g, node* n) {
  if UNLIKELY(n->t != T_IREG && n->t != T_FREG) {
    errf(g->ctx, n->line, n->col, "expected register, got %s", tokname(n->t));
    return 0;
  }
  assert(n->ival < 32); // parser checks this, so no real error checking needed
  return (u8)n->ival;
}

static usize labelderef(gstate* g, usize referreri, const strslice* name) {
  for (usize i = 0; i < g->llen; i++) {
    labelinfo* li = &g->lv[i];
    if (strsliceeqn(name, li->name, li->namelen)) { // label found
      referreri++;
      i64 idelta = referreri > li->i ? (i64)(referreri - li->i) : -(i64)(li->i - referreri);
      return -idelta;
    }
  }
  // label is not (yet) defined -- record a pending reference
  assertf(g->ullen < g->ulcap, "TODO grow ulv if needed");
  labelinfo* li = &g->ulv[g->ullen++];
  li->i = referreri;
  li->name = name->p;
  li->namelen = name->len;
  return 0;
}

// getiargs checks & reads integer arguments for an operation described by AST node n.
// returns true if the last arg is an immediate value.
static bool getiargs(gstate* g, node* n, i32* argv, u32 wantargc) {
  assert(n->t == T_OP);
  u32 argc = 0;

  // first argc-1 args are registers
  node* arg = n->list.head;
  for (; argc < wantargc-1 && arg; argc++, arg = arg->next)
    argv[argc] = nregno(g, arg);

  if UNLIKELY(!arg || arg->next != NULL) {
    if (arg)
      argc += 2;
    goto err_argc;
  }

  // last arg is either a register, immediate or label (resolved as immediate)
  switch (arg->t) {
  case T_IREG:
    assert(arg->ival < 32);
    argv[argc] = (i32)arg->ival;
    return false;
  case T_NAME: // label
    argv[argc] = labelderef(g, g->ilen - 1, &arg->name);
    return true;
  case T_INT2 ... T_SINT16:
    if UNLIKELY(arg->ival > RSM_MAX_Bw) {
      errf(g->ctx, arg->line, arg->col, "value %llu too large", arg->ival);
      return false;
    }
    argv[argc] = (i32)arg->ival;
    return true;
  default:
    errf(g->ctx, arg->line, arg->col,
      "expected register or immediate integer, got %s", tokname(arg->t));
    argv[argc] = 0;
    return false;
  }

err_argc:
  if (argc < wantargc) {
    errf(g->ctx, n->line, n->col, "not enough arguments for %s; want %u, got %u",
      rop_name((rop)n->ival), wantargc, argc);
  } else {
    errf(g->ctx, n->line, n->col, "too many arguments for %s; want %u, got %u",
      rop_name((rop)n->ival), wantargc, argc);
  }
  return false;
}


static void genop(gstate* g, node* n) {
  assert(n->t == T_OP);
  assert(n->ival < RSM_OP_COUNT);
  assertf(g->ilen < g->icap, "TODO grow iv if needed");

  rinstr* in = &g->iv[g->ilen++]; // allocate instruction
  rop op = (rop)n->ival;
  i32 arg[4]; // ABCD args to RSM_MAKE_* macros

  // route opcode to instruction encoder
  switch (op) {
    #define _(OP, ENC, ...) case rop_##OP: goto make_##ENC;
    RSM_FOREACH_OP(_)
    #undef _
  }

  // instruction encoders
  #define NOIMM(argc, ENC, args...)            \
    if (getiargs(g, n, arg, argc)) goto noimm; \
    *in = RSM_MAKE_##ENC(op, args); return;

  #define RoIMM(argc, ENC, ENCi, args...) \
    if (getiargs(g, n, arg, argc)) {      \
      *in = RSM_MAKE_##ENCi(op, args);    \
    } else {                              \
      *in = RSM_MAKE_##ENC(op, args);     \
    }                                     \
    return;

  DIAGNOSTIC_IGNORE_PUSH("-Wunused-label")
  make__:
    if UNLIKELY(n->list.head)
      errf(g->ctx, n->line, n->col, "%s does not accept any arguments", rop_name(op));
    *in = RSM_MAKE__(op);
    return;
  make_A:     NOIMM(1, A          , arg[0])
  make_Au:    RoIMM(1, A, Au      , arg[0])
  make_As:    RoIMM(1, A, As      , arg[0])
  make_AB:    NOIMM(2, AB         , arg[0], arg[1])
  make_ABu:   RoIMM(2, AB, ABu    , arg[0], arg[1])
  make_ABs:   RoIMM(2, AB, ABs    , arg[0], arg[1])
  make_ABC:   NOIMM(3, ABC        , arg[0], arg[1], arg[2])
  make_ABCu:  RoIMM(3, ABC, ABCu  , arg[0], arg[1], arg[2])
  make_ABCs:  RoIMM(3, ABC, ABCs  , arg[0], arg[1], arg[2])
  make_ABCD:  NOIMM(4, ABCD       , arg[0], arg[1], arg[2], arg[3])
  make_ABCDu: RoIMM(4, ABCD, ABCDu, arg[0], arg[1], arg[2], arg[3])
  make_ABCDs: RoIMM(4, ABCD, ABCDs, arg[0], arg[1], arg[2], arg[3])
  DIAGNOSTIC_IGNORE_POP()
  #undef RoIMM
  #undef NOIMM

noimm: // the operation does not accept an immediate-value as the last argument
  errf(g->ctx, n->line, n->col, "last argument for %s must be a register", rop_name(op));
}


static void genassign(gstate* g, node* n) {
  assert(n->t == T_EQ);
  // convert to op
  n->t = T_OP;
  n->ival = rop_MOVE;
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
    n->ival = rop_MOVE;
  }

  return genop(g, n);
}

static void genblock(gstate* g, node* block) {
  assert(block->t == T_LABEL);
  assertf(g->llen < g->lcap, "TODO grow lv if needed");

  // record label's name and "position"
  labelinfo* li = &g->lv[g->llen++];
  li->i = g->ilen;
  li->name = block->name.p;
  li->namelen = block->name.len;

  // resolve any pending references
  for (usize i = 0; i < g->ullen; i++) {
    labelinfo* li = &g->ulv[i];
    if (strsliceeqn(&block->name, li->name, li->namelen)) {
      rinstr in = g->iv[li->i];
      i64 idelta = (i64)(g->ilen - li->i - 1);
      g->iv[li->i] = RSM_SET_Bs(in, idelta);

      // dlog("patch instruction iv[%lu] label %.*s Bs = %lld",
      //   li->i, (int)block->name.len, block->name.p, idelta);

      assertf(RSM_GET_OP(in) == rop_BRZ, "TODO handle other ops");
      dlog("TODO splice out from ulv"); // reference is now resolved
    }
  }

  for (node* cn = block->list.head; cn; cn = cn->next) {
    switch (cn->t) {
      case T_OP: genop(g, cn); break;
      case T_EQ: genassign(g, cn); break;
      default: errf(g->ctx, cn->line, cn->col, "invalid block element %s", tokname(cn->t));
    }
  }
}

static void genfun(gstate* g, node* fun) {
  assert(fun->t == T_FUN);
  node* params = fun->list.head;
  node* results = params->next;
  node* body = results->next;
  if (!body)
    return;
  assertf(g->flen < g->fcap, "TODO grow fv if needed");
  g->fv[g->flen++] = g->ilen; // record function's "position"
  g->llen = 0; // clear label mappings from any past functions
  for (node* cn = body->list.head; cn; cn = cn->next)
    genblock(g, cn);
}

// --- main functions

static node* parse(rasmctx* ctx) {
  pstate p = {
    .ctx       = ctx,
    .inp       = ctx->srcdata,
    .inend     = ctx->srcdata + ctx->srclen,
    .linestart = ctx->srcdata,
    .lineno    = 1,
  };
  sadvance(&p); // prime parser with initial token
  node* module = mklist(&p);
  while (p.tok != T_END && !ctx->_stop) {
    node* n = pstmt(&p, PREC_MEMBER);
    if (p.tok != T_END) // every statement ends with a semicolon
      eat(&p, T_SEMI);
    nlist_append(&module->list, n);

    if UNLIKELY(n->t != T_FUN)
      perr(&p, n, "expected function, got %s", tokname(n->t));

    #ifdef LOG_AST
      char buf[4096];
      fmtnode(buf, sizeof(buf), n);
      log("input:%u:%u: parsed top-level statement:\n%s", n->line, n->col, buf);
    #endif
  }
  return module;
}

static node* analyze(rasmctx* ctx, node* n) {
  // TODO
  return n;
}

static usize codegen(rasmctx* ctx, node* module, rinstr** res) {
  gstate g = { .ctx=ctx };

  g.icap = 4096/sizeof(rinstr);
  g.iv = rmem_allocz(ctx->mem, g.icap*sizeof(rinstr));

  g.fcap = 16;
  g.fv = rmem_allocz(ctx->mem, g.fcap*sizeof(usize));

  g.lcap = 16;
  g.lv = rmem_allocz(ctx->mem, g.lcap*sizeof(labelinfo));

  g.ulcap = 16;
  g.ulv = rmem_allocz(ctx->mem, g.ulcap*sizeof(labelinfo));

  assert(module->t == T_LPAREN);
  for (node* cn = module->list.head; cn; cn = cn->next)
    genfun(&g, cn);
  *res = g.iv;
  return g.ilen;
}

// assembler entry point
usize rsm_asm(rasmctx* ctx, rinstr** res) {
  ctx->_stop = false;
  ctx->errcount = 0;
  dlog("assembling \"%s\"", ctx->srcname);

  node* module = parse(ctx);
  if UNLIKELY(ctx->_stop || ctx->errcount)
    return 0;

  #ifdef LOG_AST
    char buf[4096];
    fmtnode(buf, sizeof(buf), module);
    log("\"%s\" module parsed as:\n%s", ctx->srcname, buf);
  #endif

  module = analyze(ctx, module);

  return codegen(ctx, module, res);
}

