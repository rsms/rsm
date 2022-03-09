// assembler: parser
// SPDX-License-Identifier: Apache-2.0
#ifndef RSM_NO_ASM
#include "rsmimpl.h"

//#define LOG_TOKENS /* define to log() token scanning */
//#define LOG_AST /* define to log() parsed top-level ast nodes */
//#define LOG_PRATT(args...) dlog(args) /* define to log pratt dispatch */

#ifndef LOG_PRATT
  #define LOG_PRATT(args...) ((void)0)
#endif

// parse state
typedef struct pstate pstate;
struct pstate {
  rasm*       a;          // compilation session/context
  const char* inp;        // source bytes cursor (source ends with 0x00)
  const char* inend;      // source bytes end
  const char* tokstart;   // start of current token in source
  const char* linestart;  // source position line start pointer (for column)
  u32         lineno;     // source position line
  rtok        tok;        // current token
  bool        insertsemi; // insert RT_SEMI before next newline
  bool        isneg;      // true when parsing a negative number
  u64         ival;       // integer value for RT_INTLIT* tokens
};

typedef u8 precedence;
enum precedence {
  PREC_LOWEST,
  PREC_ASSIGN,
  PREC_COMMA,
  PREC_BINOP,
  PREC_UNARY_PREFIX,
  PREC_UNARY_POSTFIX,
  PREC_MEMBER,
};

#define PPARAMS pstate* p, precedence prec
#define PARGS   p, prec

// Pratt parselet
typedef struct parselet parselet;
typedef rnode*(*prefixparselet)(PPARAMS); // token...
typedef rnode*(*infixparselet)(PPARAMS, rnode* left); // (left token)...
struct parselet {
  prefixparselet nullable prefix;
  infixparselet  nullable infix;
  precedence     prec;
};

typedef u8 ropres; // operation result type
enum ropres {
  ropres_nil,
  ropres_reg,
  ropres_mem,
};


static smap kwmap = {0}; // keywords map


static ropres rop_result(rop op) {
  switch (op) {
    #define _(name, enc, res, ...) case rop_##name: return ropres_##res;
    RSM_FOREACH_OP(_)
    #undef _
    default: return ropres_nil;
  }
}

static rnode* appendchild(rnode* parent, rnode* child) {
  if (parent->children.tail) {
    parent->children.tail->next = child;
  } else {
    parent->children.head = child;
  }
  parent->children.tail = child;
  assert(child->next == NULL);
  return parent;
}

static rnode* setchildren2(rnode* parent, rnode* child1, rnode* child2) {
  parent->children.head = child1;
  parent->children.tail = child2;
  child1->next = child2;
  assert(child2->next == NULL);
  return parent;
}

static usize toklen(pstate* p) { // length in bytes of current token
  assert((uintptr)p->inp >= (uintptr)p->tokstart);
  return (usize)(uintptr)(p->inp - p->tokstart);
}

static u32 toklen32(pstate* p) { // length in bytes of current token
  assert((uintptr)p->inp >= (uintptr)p->tokstart);
  assert((uintptr)(p->inp - p->tokstart) <= U32_MAX);
  return (u32)(uintptr)(p->inp - p->tokstart);
}

static u32 pcolumn(pstate* p) { // source column of current token
  return (u32)((uintptr)p->tokstart - (uintptr)p->linestart) + 1;
}

static rposrange sposrange(pstate* p) {
  return (rposrange){.focus={ p->lineno, pcolumn(p) }};
}

ATTR_FORMAT(printf, 2, 3)
static void serr(pstate* p, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  reportv(p->a, sposrange(p), 1, fmt, ap);
  va_end(ap);
  p->tok = RT_END;
}

ATTR_FORMAT(printf, 3, 4)
static void perr(pstate* p, rnode* nullable n, const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  if (n) {
    reportv(p->a, nposrange(n), 1, fmt, ap);
  } else {
    reportv(p->a, sposrange(p), 1, fmt, ap);
  }
  va_end(ap);
}

static void scomment(pstate* p) { // line comment "// ... <LF>"
  p->tokstart += 2; // exclude "//"
  while (*p->inp && *p->inp != '\n')
    p->inp++;
  p->tok = RT_COMMENT;
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

#define isname(c) (isalnum(c) || (c) == '_')

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
        p->tok = RT_LABEL;
      }
      return;
    }
  }
}

static void sname(pstate* p) {
  while (p->inp < p->inend && isname(*p->inp))
    p->inp++;
  if (p->inp < p->inend && (u8)*p->inp >= UTF8_SELF)
    return snameunicode(p);
  if (p->inp < p->inend && *p->inp == ':') {
    p->inp++;
    p->tok = RT_LABEL;
    return;
  }
  p->insertsemi = true;
}

static void sname_or_kw(pstate* p) { // "foo"
  sname(p);
  uintptr* vp = smap_lookup(&kwmap, p->tokstart, toklen(p)); // look up keyword
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
    if LIKELY(p->tok != RT_END) // tok is RT_END if an error occurred
      p->tok++; // RT_INTLIT -> RT_SINTLIT
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
  if UNLIKELY(err || p->ival > RSM_MAX_REG)
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
    p->tok = RT_SEMI; return;
  }

  if UNLIKELY(p->inp == p->inend || rasm_stop(p->a)) {
    p->tokstart--;
    if (p->insertsemi) {
      p->insertsemi = false;
      p->tok = RT_SEMI; return;
    }
    p->tokstart = p->inp;
    p->tok = RT_END; return;
  }

  assert(p->isneg == false); // make sure we don't have a bug in snumber
  char c = *p->inp++; // read current byte and advance cursor
  bool insertsemi = p->insertsemi;
  p->insertsemi = false;

  switch (c) {
    case '(': p->tok = RT_LPAREN; return;
    case ')': p->insertsemi = true; p->tok = RT_RPAREN; return;
    case '{': p->tok = RT_LBRACE; return;
    case '}': p->insertsemi = true; p->tok = RT_RBRACE; return;
    case '=': p->tok = RT_EQ; return;
    case ';': p->tok = RT_SEMI; return;
    case ',': p->tok = RT_COMMA; return;
    case '+': p->tok = RT_PLUS; return;
    case '*': p->tok = RT_STAR; return;
    case '%': p->tok = RT_PERC; return;
    case '&': p->tok = RT_AMP; return;
    case '|': p->tok = RT_PIPE; return;
    case '^': p->tok = RT_HAT; return;
    case '<':
      if (*p->inp != '<') { p->tok = RT_LT; return; }
      p->inp++;             p->tok = RT_LT2; return;
    case '>':
      if (*p->inp != '>') { p->tok = RT_GT; return; }
      p->inp++;
      if (*p->inp != '>') { p->tok = RT_GT2; return; }
      p->inp++;             p->tok = RT_GT3; return;

    // number or "-"
    case '-': // "-" | "-" numlit
      if (!isdigit(*p->inp)) { p->tok = RT_MINUS; return; }
      p->inp++;
      p->isneg = true;
      FALLTHROUGH;
    case '0': switch (*p->inp) {
      case 'B': case 'b': p->inp++; p->tok = RT_INTLIT2;  return snumber(p, 2);
      case 'X': case 'x': p->inp++; p->tok = RT_INTLIT16; return snumber(p, 16);
    } FALLTHROUGH;
    case '1' ... '9': p->inp--; p->tok = RT_INTLIT; return snumber(p, 10);

    case '/':
      if (*p->inp == '/') {
        p->inp++;
        p->insertsemi = insertsemi;
        // return scomment(p); // alt 1: produce comments
        scomment(p); MUSTTAIL return sadvance(p); // alt 2: ignore comments
      }
      p->tok = RT_SLASH; return;
    case 'R': p->tok = RT_IREG; return sreg(p);
    case 'F': p->tok = RT_FREG; return sreg(p);
    default: // anything else is the start of a name
      if ((u8)c >= UTF8_SELF) {
        p->inp--;
        p->tok = RT_NAME;
        return snameunicode(p);
      }
      if UNLIKELY(!isalnum(c) && c != '_')
        return serr(p, "unexpected character");
      p->tok = RT_NAME;
      return sname_or_kw(p);
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
    if (t == RT_INTLIT2 || t == RT_INTLIT || t == RT_INTLIT16 || t == RT_IREG || t == RT_FREG) {
      return log("%3u:%-3u %-12s \"%.*s\"\t%llu\t0x%llx",
        line, col, tname, tvalc, tvalp, p->ival, p->ival);
    }
    if (t == RT_SINTLIT2 || t == RT_SINTLIT || t == RT_SINTLIT16) { // uses ival
      return log("%3u:%-3u %-12s \"%.*s\"\t%lld\t0x%llx",
        line, col, tname, tvalc, tvalp, (i64)p->ival, p->ival);
    }
    log("%3u:%-3u %-12s \"%.*s\"", line, col, tname, tvalc, tvalp);
  }
#endif

static bool got(pstate* p, rtok t);

// sfastforward advances the scanner until one of the tokens in stoplist is encountered.
// stoplist should be NULL-terminated.
static void sfastforward(pstate* p, const rtok* stoplist) {
  const char* inp = p->inp;
  while (p->tok != RT_END) {
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
}

static void sfastforward_semi(pstate* p) {
  const rtok stoplist[] = { RT_SEMI, 0 };
  sfastforward(p, stoplist);
}


static rnode* pstmt(PPARAMS);

static const char* nname(rnode* n) {
  // TODO improve with better names, e.g. "assignment" instead of "EQ"
  return tokname(n->t);
}

// perrunexpected reports a syntax error along with the current token
static void perrunexpected(
  pstate* p, rnode* nullable n, const char* expected, const char* got)
{
  perr(p, n, "expected %s, got %s", expected, got);
}

// got comsumes the next token if p->tok == t
static bool got(pstate* p, rtok t) {
  if (p->tok != t)
    return false;
  sadvance(p);
  return true;
}

// eat comsumes the next token if it's t, reporting a syntax error if p->tok != t
static void eat(pstate* p, rtok t) {
  if UNLIKELY(p->tok != t) {
    perrunexpected(p, NULL, tokname(t), tokname(p->tok));
    return;
  }
  sadvance(p);
}

// expect reports a syntax error if p->tok != t
static void expecttok(pstate* p, rtok t) {
  if UNLIKELY(p->tok != t)
    perrunexpected(p, NULL, tokname(t), tokname(p->tok));
}

static bool expecttype(pstate* p, rnode* n) {
  if UNLIKELY(!tokistype(n->t)) {
    perrunexpected(p, n, "type", nname(n));
    return false;
  }
  return true;
}

static bool expectname(pstate* p, rnode* n) {
  if UNLIKELY(n->t != RT_NAME) {
    perrunexpected(p, n, "name", nname(n));
    return false;
  }
  return true;
}

static bool nsigned(rnode* n) {
  return n->t == RT_SINTLIT2 || n->t == RT_SINTLIT || n->t == RT_SINTLIT16;
}

void rasm_free_rnode(rasm* a, rnode* n) {
  for (rnode* cn = n->children.head; cn; cn = cn->next)
    rasm_free_rnode(a, cn);
  rmem_free(a->mem, n, sizeof(rnode));
}

// mk* functions makes new nodes
static rnode* mknodet(pstate* p, rtok t) {
  rnode* n = rmem_alloc(p->a->mem, sizeof(rnode));
  memset(n, 0, sizeof(rnode));
  n->t = t;
  n->pos.line = p->lineno;
  n->pos.col = pcolumn(p);
  return n;
}

static rnode* mknode(pstate* p) { return mknodet(p, p->tok); }
static rnode* mklist(pstate* p) { return mknodet(p, RT_LPAREN); }
static rnode* mknil(pstate* p)  { return mknodet(p, RT_END); }

static rnode* mktype_for_constexpr(pstate* p, rnode* expr) {
  rtok t = 0;
  switch (expr->t) {
    case RT_INTLIT2:
    case RT_INTLIT:
    case RT_INTLIT16:
      t = ( expr->ival <= U8_MAX  ? RT_I8 :
            expr->ival <= U16_MAX ? RT_I16 :
            expr->ival <= U32_MAX ? RT_I32 :
            RT_I64 );
      break;
    case RT_SINTLIT2:
    case RT_SINTLIT:
    case RT_SINTLIT16: {
      i64 v = (i64)expr->ival;
      t = ( (v >= I8_MIN  && v <= I8_MAX)  ? RT_I8 :
            (v >= I16_MIN && v <= I16_MAX) ? RT_I16 :
            (v >= I32_MIN && v <= I32_MAX) ? RT_I32 :
            RT_I64 );
      break;
    }
    default:
      perr(p, expr, "cannot infer type of %s", nname(expr));
  }
  return mknodet(p, t);
}

static rnode* pexpr(PPARAMS) {
  rnode* n = pstmt(PARGS);
  if UNLIKELY(!tokisexpr(n->t))
    perrunexpected(p, n, "expression", nname(n));
  return n;
}

static rnode* ptype(PPARAMS) {
  prec = PREC_MEMBER;
  rnode* n = pstmt(PARGS);
  expecttype(p, n);
  return n;
}

static rnode* prefix_int(PPARAMS) {
  rnode* n = mknode(p);
  n->ival = p->ival;
  sadvance(p);
  return n;
}

// storagedef = constdef | datadef
// constdef   = "const" name type? "=" expr ";"
// datadef    = "data" name type ("=" expr)? ";"
static rnode* prefix_storage(PPARAMS) {
  rnode* n = mknode(p);
  sadvance(p);
  if UNLIKELY(p->tok != RT_NAME) {
    perrunexpected(p, NULL, "name", tokname(p->tok));
    n->name.len = 0;
    return n;
  }
  n->name.p = p->tokstart;
  n->name.len = toklen32(p);
  sadvance(p);
  if (n->t == RT_CONST) { // constdef
    rnode* typ = NULL;
    if (p->tok != RT_EQ)
      typ = ptype(PARGS);
    eat(p, RT_EQ);
    rnode* init = pexpr(PARGS);
    if (typ == NULL) { // infer type
      typ = mktype_for_constexpr(p, init);
      typ->pos = (rsrcpos){0};
    }
    setchildren2(n, typ, init);
  } else { // datadef
    rnode* typ = ptype(PARGS);
    appendchild(n, typ);
    if (got(p, RT_EQ)) {
      rnode* init = pexpr(PARGS);
      appendchild(n, init);
    }
  }
  return n;
}

static rnode* passign_storage(PPARAMS, rnode* eq) {
  rnode* init = pexpr(PARGS);
  appendchild(eq, init);
  return eq;
}

// assignreg = reg "=" (operation | operand) ";"
static rnode* passign_reg(PPARAMS, rnode* eq) {
  rnode* rhs = pstmt(PARGS);
  appendchild(eq, rhs);
  switch (rhs->t) {
    case RT_OP: {
      ropres res = rop_result(rhs->ival);
      if UNLIKELY(res != ropres_reg) {
        // note: using srcpos of eq here to make hanging equal
        perr(p, rhs, "register assignment of operation %s which does not produce a %sresult",
          rop_name(rhs->ival), res == ropres_nil ? "" : "register ");
      }
      break;
    }
    default:
      if UNLIKELY(!tokisoperand(rhs->t))
        perrunexpected(p, rhs, "operation or operand", nname(rhs));
  }
  return eq;
}

static rnode* pname(PPARAMS) {
  assertf(p->tok == RT_NAME || p->tok == RT_LABEL, "%s", tokname(p->tok));
  rnode* n = mknode(p);
  n->name.p = p->tokstart;
  n->name.len = toklen32(p);
  sadvance(p);
  return n;
}

// assignment = assignreg | assigndata
// assignreg  = reg "=" (operation | operand) ";"
// assigndata = gname "=" type expr? ";"
static rnode* infix_eq(PPARAMS, rnode* lhs) {
  rnode* n = mknode(p);
  sadvance(p);
  appendchild(n, lhs);
  switch (lhs->t) {
    case RT_DATA: case RT_CONST: return passign_storage(PARGS, n);
    case RT_IREG: case RT_FREG:  return passign_reg(PARGS, n);
  }
  perrunexpected(p, lhs, "constant, @name or register", nname(lhs));
  if (p->tok != RT_SEMI) // attempt to recover and also improve error message
    appendchild(n, pexpr(PARGS));
  return n;
}

static rnode* prefix_name(PPARAMS) {
  return pname(PARGS);
}

static rnode* prefix_type(PPARAMS) {
  rnode* n = mknode(p);
  sadvance(p);
  return n;
}

// operand = reg | literal
static rnode* poperand(PPARAMS) {
  rnode* operand = pstmt(PARGS);
  if UNLIKELY(!tokisoperand(operand->t)) {
    perrunexpected(p, operand, "register or literal", nname(operand));
    sfastforward_semi(p);
  }
  return operand;
}

// operands = operand*
static rnode* poperands(PPARAMS, rnode* n) {
  while (p->tok != RT_SEMI && p->tok != RT_END) {
    rnode* operand = poperand(PARGS);
    appendchild(n, operand);
  }
  return n;
}

// operation = op operand*
static rnode* prefix_op(PPARAMS) {
  rnode* n = prefix_int(PARGS);
  return poperands(PARGS, n);
}

// binop = operand ("-" | "+" | "*" | "/") operand
static rnode* infix_binop(PPARAMS, rnode* lhs) {
  rnode* n = mknode(p);
  sadvance(p);
  // set n->ival by mapping token to opcode
  switch (n->t) {
    #define _(TOK, op_u, op_s) \
      case TOK: n->ival = nsigned(lhs) ? rop_##op_s : rop_##op_u; break;
    RSM_FOREACH_BINOP_TOKEN(_)
    #undef _
    default: assertf(0,"n->t %u (%s)", n->t, tokname(n->t));
  }
  n->t = RT_OP;
  rnode* rhs = poperand(PARGS);
  return setchildren2(n, lhs, rhs);
}

// blockbody = blockstmt*
// blockstmt = operation | assignment
static rnode* pblockbody(PPARAMS, rnode* block) {
  for (;;) {
    switch (p->tok) {
      // stop tokens
      case RT_RBRACE:
      case RT_END:
      case RT_LABEL:
        return block;
      // // will-parse tokens
      // case RT_OP:
      // case RT_EQ:
      // case RT_IREG:
      // case RT_FREG:
      //   break;
      // // unexpected
      // default: {
      //   if (p->tok == RT_NAME) {
      //     perr(p, NULL, "unknown operation \"%.*s\"", (int)toklen(p), p->tokstart);
      //   } else {
      //     perrunexpected(p, NULL, "operation or assignment", tokname(p->tok));
      //   }
      //   const rtok stoplist[] = { RT_SEMI, 0 };
      //   sfastforward(p, stoplist);
      //   continue;
      // }
    }
    rnode* cn = pstmt(PARGS);
    appendchild(block, cn);
    eat(p, RT_SEMI);
  }
}

// labelblock = name ":" (operation | assignment)*
static rnode* prefix_label(PPARAMS) {
  rnode* n = prefix_name(PARGS); // label
  n->name.len--; // trim off trailing ":"
  assert(n->name.p[n->name.len] == ':');
  return pblockbody(PARGS, n);
}

// param = (name type | type)
static rnode* pparam(PPARAMS) {
  rnode* n = pstmt(PARGS);
  if (!tokistype(p->tok)) {
    expecttype(p, n); // n should be a type
    return n; // just type
  }
  // both name and type
  expectname(p, n);
  rnode* typ = ptype(PARGS);
  return appendchild(n, typ);
}

// params = param ("," param)*
static rnode* pparams(PPARAMS) {
  rnode* n = mklist(p);
  for (;;) {
    appendchild(n, pparam(PARGS));
    if (!got(p, RT_COMMA))
      return n;
  }
}

// fundef = "fun" name "(" params? ")" result? "{" body "}"
// result = params
// body   = (stmt ";")*
static rnode* prefix_fun(PPARAMS) {
  rnode* n = mknode(p);
  sadvance(p); // consume token

  // name
  expecttok(p, RT_NAME);
  n->name.p = p->tokstart;
  n->name.len = toklen32(p);
  sadvance(p);

  // parameters
  eat(p, RT_LPAREN);
  if (got(p, RT_RPAREN)) {
    appendchild(n, mknil(p));
  } else {
    appendchild(n, pparams(PARGS));
    eat(p, RT_RPAREN);
  }

  // result
  if (p->tok == RT_LBRACE || p->tok == RT_SEMI) {
    appendchild(n, mknil(p));
  } else {
    appendchild(n, pparams(PARGS));
  }

  if (p->tok == RT_SEMI) // function declaration
    return n;

  // body "{" ... "}"
  expecttok(p, RT_LBRACE);
  rnode* block0 = mknode(p); // recycle the "{" token
  rnode* body = mklist(p); // body starts at position of "{"
  appendchild(n, body);
  sadvance(p); // consume "{"
  if (got(p, RT_RBRACE)) // empty function body
    return n;

  // implicit first block?
  if (p->tok != RT_LABEL) {
    block0->t = RT_LABEL;
    block0->name.p = kBlock0Name;
    block0->name.len = 2;
    pblockbody(PARGS, block0);
    appendchild(body, block0);
  }
  while (p->tok == RT_LABEL) {
    rnode* block = prefix_label(PARGS);
    if (body->children.head != NULL && nodename_eq(block, kBlock0Name, strlen(kBlock0Name)))
      perr(p, block, "block named %s must be first block", kBlock0Name);
    appendchild(body, block);
  }
  eat(p, RT_RBRACE);
  return n;
}

static const parselet parsetab[rtok_COUNT] = {
  [RT_IREG]  = {prefix_int, NULL, 0},
  [RT_FREG]  = {prefix_int, NULL, 0},
  [RT_OP]    = {prefix_op, NULL, 0},
  [RT_LABEL] = {prefix_label, NULL, 0},
  [RT_NAME]  = {prefix_name, NULL, 0},
  [RT_CONST] = {prefix_storage, NULL, 0},
  [RT_DATA]  = {prefix_storage, NULL, 0},
  [RT_I1]    = {prefix_type, NULL, 0},
  [RT_I8]    = {prefix_type, NULL, 0},
  [RT_I16]   = {prefix_type, NULL, 0},
  [RT_I32]   = {prefix_type, NULL, 0},
  [RT_I64]   = {prefix_type, NULL, 0},
  [RT_EQ]    = {NULL, infix_eq, PREC_ASSIGN},

  #define _(tok, ...) [tok] = {NULL, infix_binop, PREC_BINOP},
  RSM_FOREACH_BINOP_TOKEN(_)
  #undef _

  [RT_INTLIT2]   = {prefix_int, NULL, 0},
  [RT_SINTLIT2]  = {prefix_int, NULL, 0},
  [RT_INTLIT]    = {prefix_int, NULL, 0},
  [RT_SINTLIT]   = {prefix_int, NULL, 0},
  [RT_INTLIT16]  = {prefix_int, NULL, 0},
  [RT_SINTLIT16] = {prefix_int, NULL, 0},

  [RT_FUN] = {prefix_fun, NULL, 0},
};

// stmt = anynode ";"
static rnode* pstmt(PPARAMS) {
  const parselet* ps = &parsetab[p->tok];

  if UNLIKELY(!ps->prefix) {
    LOG_PRATT("PREFIX %s not found", tokname(p->tok));
    perrunexpected(p, NULL, "statement", tokname(p->tok));
    rnode* n = mknil(p);
    sfastforward_semi(p);
    return n;
  }

  LOG_PRATT("PREFIX %s call", tokname(p->tok));
  ATTR_UNUSED const void* p1 = p->inp;
  ATTR_UNUSED bool insertsemi = p->insertsemi;
  rnode* n = ps->prefix(PARGS);
  assertf(insertsemi != p->insertsemi || (uintptr)p1 < (uintptr)p->inp,
    "parselet did not advance scanner");

  // infix
  for (;;) {
    ps = &parsetab[p->tok];
    if (ps->infix == NULL || ps->prec < prec) {
      if (ps->infix) {
        LOG_PRATT("INFIX %s skip (ps->prec %d < prec %d)", tokname(p->tok), ps->prec, prec);
      } else if (p->tok != RT_SEMI) {
        LOG_PRATT("INFIX %s not found", tokname(p->tok));
      }
      return n;
    }
    LOG_PRATT("INFIX %s call", tokname(p->tok));
    n = ps->infix(PARGS, n);
  }
  return n;
}

#ifdef LOG_AST
static usize fmtnode(char* buf, usize bufcap, rnode* n);
#endif

rnode* rasm_parse(rasm* a) {
  rasm_stop_set(a, false);
  a->errcount = 0;
  pstate p = {
    .a         = a,
    .inp       = a->srcdata,
    .inend     = a->srcdata + a->srclen,
    .linestart = a->srcdata,
    .lineno    = 1,
  };

  #ifdef LOG_AST
    char buf[4096*8];
  #endif

  dlog("parsing \"%s\"", a->srcname);
  sadvance(&p); // prime parser with initial token
  rnode* module = mklist(&p);

  while (p.tok != RT_END && !rasm_stop(a)) {
    rnode* n = pstmt(&p, PREC_LOWEST);
    if LIKELY(p.tok != RT_END) // every statement ends with a semicolon
      eat(&p, RT_SEMI);
    appendchild(module, n);

    if UNLIKELY(n->t != RT_FUN && n->t != RT_CONST && n->t != RT_DATA) {
      perr(&p, n, "unexpected top-level statement");
      #ifdef LOG_AST
        fmtnode(buf, sizeof(buf), n);
        log("%s", buf);
      #endif
    }
  }

  #ifdef LOG_AST
    fmtnode(buf, sizeof(buf), module);
    log("\"%s\" module parsed as:\n%s", a->srcname, buf);
  #endif

  return module;
}


// --- ast formatter

#ifdef LOG_AST

  typedef u32 fmtflag;
  enum fmtflag {
    FMT_HEAD = 1 << 0, // is list head
  } RSM_END_ENUM(fmtflag)

  static void fmtnode1(abuf* s, rnode* n, usize indent, fmtflag fl) {
    if ((fl & FMT_HEAD) == 0) {
      abuf_c(s, '\n');
      for (usize i = 0; i < indent; i++)
        abuf_c(s, ' ');
    }
    fl &= ~FMT_HEAD;
    indent += 2;

    // atoms
    switch ((enum rtok)n->t) {
      case RT_END:
        return abuf_str(s, "nil");
      default: break;
    }

    abuf_c(s, '(');

    // list
    if (n->t == RT_LPAREN) {
      for (rnode* cn = n->children.head; cn; cn = cn->next) {
        abuf_c(s, ' ');
        fmtnode1(s, cn, indent, cn == n->children.head ? fl | FMT_HEAD : fl);
      }
      abuf_c(s, ' ');
      return abuf_c(s, ')');
    }

    // complex
    abuf_str(s, tokname(n->t));
    char* sp = s->p;
    abuf_c(s, ' ');
    switch ((enum rtok)n->t) {
      case RT_IREG:
      case RT_FREG:
        abuf_u64(s, n->ival, 10); break;

      case RT_NAME:
      case RT_DATA:
      case RT_COMMENT:
      case RT_LABEL:
      case RT_FUN:
      case RT_CONST:
        abuf_append(s, n->name.p, n->name.len); break;

      case RT_OP:
        abuf_str(s, rop_name((rop)n->ival)); break;

      case RT_SINTLIT2:  abuf_c(s, '-'); FALLTHROUGH;
      case RT_INTLIT2:   abuf_str(s, "0b"); abuf_u64(s, n->ival, 2); break;
      case RT_SINTLIT: abuf_c(s, '-'); FALLTHROUGH;
      case RT_INTLIT:  abuf_u64(s, n->ival, 10); break;
      case RT_SINTLIT16: abuf_c(s, '-'); FALLTHROUGH;
      case RT_INTLIT16:  abuf_str(s, "0x"); abuf_u64(s, n->ival, 16); break;

      // only list
      case RT_EQ:
      case RT_I1:
      case RT_I8:
      case RT_I16:
      case RT_I32:
      case RT_I64:
        s->p = sp; s->len--; // undo abuf_c(s, ' ')
        break;

      default:
        abuf_str(s, "[TODO fmtnode1]");
        break;
    }
    for (rnode* cn = n->children.head; cn; cn = cn->next) {
      abuf_c(s, ' ');
      fmtnode1(s, cn, indent, fl);
    }
    abuf_c(s, ')');
  }

  static usize fmtnode(char* buf, usize bufcap, rnode* n) {
    abuf s = abuf_make(buf, bufcap);
    fmtnode1(&s, n, 0, FMT_HEAD);
    return abuf_terminate(&s);
  }

#endif // LOG_AST


// print_keywords
#if 0 && defined(DEBUG) && !defined(RSM_NO_LIBC)
  static int cstrsort(const char** p1, const char** p2, void* ctx) {
    return strcmp(*p1, *p2);
  }
  ATTR_UNUSED static void print_keywords() {
    const char* list[128];
    usize n = 0;
    for (const smapent* e = smap_itstart(&kwmap); smap_itnext(&kwmap, &e); )
      list[n++] = e->key;
    rsm_qsort(list, n, sizeof(void*), (int(*)(const void*,const void*,void*))&cstrsort, NULL);
    for (usize i = 0; i < n; i++)
      fprintf(stdout, i?" %s":"%s", list[i]);
    putc('\n', stdout);
  }
#else
  #define print_keywords(...) ((void)0)
#endif

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

rerror parse_init() {
  static void* memory[3104/sizeof(void*)];
  rmem mem = rmem_mkbufalloc(memory, sizeof(memory));
  smap* m = smap_make(&kwmap, mem, kwcount, MAPLF_2); // increase sizeof(memory)
  if UNLIKELY(m == NULL)
    return rerr_nomem;
  m->hash0 = 0x89f025ba;
  uintptr* vp;
  #define _(token, kw) \
    vp = assertnotnull(smap_assign(m, kw, strlen(kw))); \
    *vp = token;
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _
  #define _(op, enc, res, kw, ...) \
    vp = assertnotnull(smap_assign(m, kw, strlen(kw))); \
    *vp = (RT_OP | ((uintptr)rop_##op << (sizeof(rtok)*8)));
  RSM_FOREACH_OP(_)
  #undef _
  #ifdef DEBUG
    // dlog("kwmap hash0 0x%lx", m->hash0);
    void* p = rmem_alloc(mem,1);
    if (p) dlog("kwmap uses only %zu B memory -- trim memory", (usize)(p - (void*)memory));
  #endif
  print_keywords();
  return 0;
}


#endif // RSM_NO_ASM
