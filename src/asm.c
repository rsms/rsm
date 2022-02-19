// assembler
#include "rsm.h"
#include "util.h"

// assembler does
// 1. scan the input for tokens
// 2. parses the semantic meaning of those tokens (ast)
// 3. checks that the ast is valid
// 4. generates code (our vm instructions)

// source tokens
typedef u8 rtok;
#define RSM_FOREACH_TOKEN(_) \
_( T_END ) \
_( T_COMMENT ) \
/* simple tokens */ \
_( T_LPAREN ) _( T_RPAREN ) \
_( T_LBRACE ) _( T_RBRACE ) \
_( T_SEMI   ) /* ; */ \
_( T_EQ     ) /* = */ \
_( T_SLASH  ) /* / */ \
_( T_MINUS  ) /* - */ \
_( T_COMMA  ) /* , */ \
/* names              */ \
_( T_IREG   ) /* Rn   */ \
_( T_FREG   ) /* Fn   */ \
_( T_LABEL  ) /* foo: */ \
_( T_SYM    ) /* foo  */ \
/* literal numbers (order matters; see snumber) */ \
_( T_INT2   ) _( T_SINT2   ) /* 0b1111011       */ \
_( T_INT10  ) _( T_SINT10  ) /* 123, -123       */ \
_( T_INT16  ) _( T_SINT16  ) /* 0x7b            */ \
/* special ast-only tokens */ \
_( T_OP ) \
// end RSM_FOREACH_TOKEN
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
  rmem*       mem;        // memory allocator
  const char* inp;        // source bytes cursor (source ends with 0x00)
  const char* inend;      // source bytes end
  const char* tokstart;   // start of current token in source
  const char* linestart;  // source position line start pointer (for column)
  u32         lineno;     // source position line
  rtok        tok;        // current token
  bool        insertsemi; // insert T_SEMI before next newline
  bool        isneg;      // true when parsing a negative number
  u64         ival;       // integer value for T_INT* tokens

  const char* nullable errstr; // non-null when an error occured
};

typedef struct node  node;
typedef struct nlist nlist;
struct nlist {
  node* nullable head;
  node* nullable tail;
};
struct node {
  node* nullable next;      // list link
  rtok           t;         // type
  u32            line, col; // source postion
  union { // field used depends on value of t
    u64 ival;
    struct { const char* p; usize len; } strslice;
  };
  nlist list;
};

typedef struct parselet parselet;
#define PPARAMS       rmem* mem, pstate* p, precedence prec
#define PARGS         mem, p, prec
typedef node*(*prefixparselet)(PPARAMS); // token...
typedef node*(*infixparselet)(PPARAMS, node* left); // (left token)...
struct parselet {
  prefixparselet nullable prefix;
  infixparselet  nullable infix;
  precedence     prec;
};

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

static rtok perr(pstate* p, const char* errstr) {
  log("input:%u:%u: error: %s", p->lineno, pcolumn(p), errstr);
  p->errstr = errstr;
  return p->tok = T_END;
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
      return perr(p, "invalid UTF8 sequence");
  }
  return p->tok = T_SYM;
}

static rtok sname(pstate* p) {
  p->tok = T_SYM;
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
  #define _(token, kw) \
    if (len == strlen(kw) && memcmp((kw), p->tokstart, len) == 0) \
      return p->tok = token;
  RSM_FOREACH_KEYWORD_TOKEN(_)
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
    case rerr_not_supported: return perr(p, "floating-point literal not supported");
    case rerr_overflow:      return perr(p, "integer literal too large");
    default:                 return perr(p, "invalid integer literal");
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
  return perr(p, "invalid register");
}

static rtok padvance(pstate* p) { // read the next token
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

  if UNLIKELY(p->inp == p->inend) {
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
    case '-':
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
      if (*p->inp) { p->inp++; p->insertsemi = insertsemi; return scomment(p); }
      return p->tok = T_SLASH;
    case 'R': p->tok = T_IREG; return sreg(p);
    case 'F': p->tok = T_FREG; return sreg(p);
    default: // anything else is the start of a name (symbol, label etc)
      if ((u8)c >= UTF8_SELF) {
        p->inp--;
        return snameunicode(p);
      }
      return sname(p);
  }
}

#define padvance(p) ({ padvance(p); logpstate(p); (p)->tok; })
static void logpstate(pstate* p);

// --- parse

static node* pstmt(PPARAMS);
#define pexpr pstmt
#define ptype pstmt

// if the current token is t, advances scanner and returns true
static bool got(pstate* p, rtok t) {
  if (p->tok != t)
    return false;
  padvance(p);
  return true;
}

// expect reports a syntax error if p->tok != t. Does not advance parser.
static void expect(pstate* p, rtok t) {
  if UNLIKELY(p->tok != t) {
    dlog("unexpected token (expected %s)", tokname(t));
    perr(p, "unexpected token");
  }
}

// pskip calls padvance(). Reports a syntax error if got(t) is false.
static void pskip(pstate* p, rtok t) {
  if UNLIKELY(!got(p, t)) {
    expect(p, t);
    padvance(p);
  }
}

static node* mknode(pstate* p) {
  node* n = rmem_allocz(p->mem, sizeof(node));
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

static node* pint(PPARAMS) {
  node* n = mknode(p);
  n->ival = p->ival;
  padvance(p); // consume token
  return n;
}

// [infix] regop = op reg operand* ";"
static node* preg_infix(PPARAMS, node* op) {
  op->t = T_OP;
  node* reg = pint(PARGS);
  nlist_append(&op->list, reg);
  while (p->tok != T_SEMI && p->tok != T_END) {
    node* cn = pstmt(PARGS);
    nlist_append(&op->list, cn);
  }
  return op;
}

// [infix] assign = (reg | sym) "=" expr ";"
static node* peq_infix(PPARAMS, node* dst) {
  node* n = mknode(p);
  padvance(p);
  node* src = pexpr(PARGS);
  nlist_set2(&n->list, dst, src);
  return n;
}

static node* pname(PPARAMS) {
  node* n = mknode(p);
  n->strslice.p = p->tokstart;
  n->strslice.len = toklen(p);
  padvance(p);
  return n;
}

// block = (assignment | op | sym)*
static node* pblock(PPARAMS) {
  node* n = pname(PARGS);
  if (n->t == T_LABEL) {
    n->strslice.len--; // trim off trailing ":"
    assert(n->strslice.p[n->strslice.len] == ':');
  }
  while (p->tok != T_RBRACE && p->tok != T_END && p->tok != T_LABEL) {
    node* cn = pstmt(PARGS);
    if (cn->t == T_SYM) // block-level symbol is operation, e.g. "ret"
      cn->t = T_OP;
    nlist_append(&n->list, cn);
    got(p, T_COMMENT); // FIXME
    pskip(p, T_SEMI);
  }
  return n;
}

// param = (name type | type)
static node* pparam(PPARAMS) {
  node* n = pexpr(PARGS);
  if (p->tok == T_SEMI || p->tok == T_COMMA || p->tok == T_RPAREN || p->tok == T_END)
    return n;
  node* tuple = mklist(p);
  nlist_append(&tuple->list, n);
  nlist_append(&tuple->list, ptype(PARGS));
  return tuple;
}

// fundef = "fun" name "(" params? ")" result? "{" body "}"
// params = param ("," param)*
// result = param ("," param)*
// body   = (stmt ";")*
static node* pfun(PPARAMS) {
  node* n = mknode(p);
  padvance(p); // consume token

  // name
  expect(p, T_SYM);
  n->strslice.p = p->tokstart;
  n->strslice.len = toklen(p);
  padvance(p);

  // parameters
  expect(p, T_LPAREN);
  node* params = mklist(p);
  padvance(p); // consume T_LPAREN
  while (p->tok != T_RPAREN) {
    node* param = pparam(PARGS);
    nlist_append(&params->list, param);
    if (!got(p, T_COMMA))
      break;
  }
  pskip(p, T_RPAREN);
  nlist_append(&n->list, params);

  // result
  node* result;
  if (p->tok == T_LBRACE) {
    result = mknil(p);
  } else {
    result = pparam(PARGS);
    if (got(p, T_COMMA)) {
      node* list = mklist(p);
      nlist_append(&list->list, result);
      result = list;
      for (;;) {
        node* param = pparam(PARGS);
        nlist_append(&list->list, param);
        if (!got(p, T_COMMA))
          break;
      }
    }
  }
  nlist_append(&n->list, result);

  // body "{" ... "}"
  pskip(p, T_LBRACE);
  if UNLIKELY(got(p, T_RBRACE)) {
    perr(p, "missing function body");
    return n;
  }
  node* body = mklist(p);
  for (;;) {
    node* cn = pstmt(PARGS);
    nlist_append(&body->list, cn);
    if (p->tok == T_RBRACE || p->tok == T_END)
      break;
  }
  nlist_append(&n->list, body);
  pskip(p, T_RBRACE);

  return n;
}

static const parselet parsetab[rtok_COUNT] = {
  [T_IREG] = {pint, preg_infix, PREC_MEMBER},
  [T_FREG] = {pint, preg_infix, PREC_MEMBER},
  [T_EQ] = {NULL, peq_infix, PREC_MEMBER},
  [T_COMMENT] = {pname, NULL, PREC_MEMBER},
  [T_SYM] = {pname, NULL, PREC_MEMBER},
  [T_LABEL] = {pblock, NULL, PREC_MEMBER},
  [T_I1] = {pname, NULL, PREC_MEMBER},
  [T_I8] = {pname, NULL, PREC_MEMBER},
  [T_I16] = {pname, NULL, PREC_MEMBER},
  [T_I32] = {pname, NULL, PREC_MEMBER},
  [T_I64] = {pname, NULL, PREC_MEMBER},

  [T_INT2] = {pint, NULL, PREC_MEMBER},
  [T_SINT2] = {pint, NULL, PREC_MEMBER},
  [T_INT10] = {pint, NULL, PREC_MEMBER},
  [T_SINT10] = {pint, NULL, PREC_MEMBER},
  [T_INT16] = {pint, NULL, PREC_MEMBER},
  [T_SINT16] = {pint, NULL, PREC_MEMBER},

  [T_FUN] = {pfun, NULL, PREC_MEMBER},
};

// stmt = anynode ";"
static node* pstmt(PPARAMS) {
  const parselet* ps = &parsetab[p->tok];
  if (!ps->prefix) {
    dlog("no prefix parselet for %s", tokname(p->tok));
    node* n = mknil(p);
    padvance(p); // make progress
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

void fmtnode1(abuf* s, node* n, int depth, bool ishead) {
  if (!ishead) {
    abuf_c(s, '\n');
    for (int i = 0; i < depth; i++)
      abuf_c(s, ' ');
  }

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
      fmtnode1(s, cn, depth+2, cn == n->list.head);
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

    case T_SYM:
    case T_COMMENT:
    case T_LABEL:
    case T_FUN:
    case T_OP:
      abuf_append(s, n->strslice.p, n->strslice.len); break;

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
    fmtnode1(s, cn, depth+2, false);
  }
  abuf_c(s, ')');
}

usize fmtnode(char* buf, usize bufcap, node* n) {
  abuf s = abuf_make(buf, bufcap);
  fmtnode1(&s, n, 0, true);
  return abuf_terminate(&s);
}

// --- main parse function

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

static void parse(rmem* mem, const char* src) {
  /*
  fun factorial (i32) i32
    b0:
      R1 = R0  // ACC = n (argument 0)
      123 -456 0xface 0b101 F31
      \xF0\x9F\x91\xA9\xF0\x9F\x8F\xBE\xE2\x80\x8D\xF0\x9F\x9A\x80
      ret            // RES is at R0
  */
  pstate p = {  .mem=mem,.inp=src, .inend=src+strlen(src), .linestart=src, .lineno=1 };
  padvance(&p);
  while (p.tok != T_END) {
    node* n = pstmt(mem, &p, PREC_MEMBER);
    if UNLIKELY(!got(&p, T_SEMI) && p.tok != T_END) {
      perr(&p, "unexpected token (expected \";\")");
      padvance(&p);
    }

    char buf[1024];
    fmtnode(buf, sizeof(buf), n);
    log("input:%u:%u: parsed:\n%s", n->line, n->col, buf);
  }
}


usize rsm_asm(rmem* mem, rinstr* idst, usize idstcap, const char* src) {
  parse(mem, src);
  // TODO: analysis
  // TODO: codegen
  return 0;
}


static const char* tokname(rtok t) {
  switch (t) {
  #define _(name, ...) case name: return &#name[2]; // [2] to skip "T_" prefix
  RSM_FOREACH_TOKEN(_)
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _
  }
  return "?";
}
