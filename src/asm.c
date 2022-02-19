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
_( T_SEMI   ) /* ; */ \
_( T_EQ     ) /* = */ \
_( T_SLASH  ) /* / */ \
_( T_MINUS  ) /* - */ \
/* names              */ \
_( T_LABEL  ) /* foo: */ \
_( T_IREG   ) /* Rn   */ \
_( T_FREG   ) /* Fn   */ \
_( T_SYM    ) /* foo  */ \
/* literal numbers (order matters; see snumber) */ \
_( T_INT2   ) _( T_SINT2   ) /* 0b1111011       */ \
_( T_INT10  ) _( T_SINT10  ) /* 123, -123       */ \
_( T_INT16  ) _( T_SINT16  ) /* 0x7b            */ \
// end RSM_FOREACH_TOKEN
#define RSM_FOREACH_KEYWORD_TOKEN(_) \
_( KW_FUN , "fun" ) \
_( KW_I1  , "i1"  ) \
_( KW_I8  , "i8"  ) \
_( KW_I16 , "i16" ) \
_( KW_I32 , "i32" ) \
_( KW_I64 , "i64" ) \
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

typedef struct node node;
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

struct node {
  rtok t;
  u32  line, col;
  union {
    u64   ival;       // T_IREG, T_FREG
    node* operand[2]; // unaryop, binop
  };
};

static const node _badnode = {0};
static node* badnode = (node*)&_badnode;


#define isname(c) (isalnum(c) || (c) == '_')

static const char* tokname(rtok t);
static usize toklen(pstate* p) { // length in bytes of current token
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
  // map keywords to KW_ tokens
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
  p->insertsemi = false;
  // dlog("0x%02x %c", c, isprint(c) ? c : ' ');

  switch (c) {
    case '(': return p->tok = T_LPAREN;
    case ')': return p->tok = T_RPAREN;
    case '=': return p->tok = T_EQ;
    case ';': return p->tok = T_SEMI;
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
      if (*p->inp) { p->inp++; return scomment(p); }
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

static void want(pstate* p, rtok expect) {
  if UNLIKELY(p->tok != expect)
    perr(p, "unexpected token");
}

static node* pignore(PPARAMS) {
  padvance(p); // consume token
  return badnode;
}

static node* preg(PPARAMS) {
  node* n = rmem_allocz(mem, sizeof(node));
  n->t = p->tok;
  n->line = p->lineno;
  n->col = pcolumn(p);
  n->ival = p->ival;
  padvance(p); // consume token
  return n;
}

// [infix] assign = (reg | sym) "=" expr ";"
static node* peq_infix(PPARAMS, node* dst) {
  node* n = rmem_allocz(mem, sizeof(node));
  n->t = p->tok;
  n->line = p->lineno;
  n->col = pcolumn(p);
  padvance(p); // consume token
  n->operand[0] = dst;
  node* src = pexpr(PARGS);
  n->operand[1] = src;
  want(p, T_SEMI);
  return n;
}

static const parselet parsetab[rtok_COUNT] = {
  [T_IREG] = {preg, NULL, PREC_MEMBER},
  [T_FREG] = {preg, NULL, PREC_MEMBER},
  [T_EQ] = {NULL, peq_infix, PREC_MEMBER},
  [T_COMMENT] = {pignore, NULL, PREC_MEMBER},
};

// stmt = anynode ";"
static node* pstmt(PPARAMS) {
  const parselet* ps = &parsetab[p->tok];
  if (!ps->prefix) {
    dlog("no prefix parselet for %s", tokname(p->tok));
    padvance(p); // make progress
    return badnode;
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
    break;
  }

  if (p->tok != T_SEMI)
    perr(p, "unexpected token; expected \";\"");
  padvance(p);

  return n;
}

// --- ast formatter

void fmtnode1(abuf* s, node* n) {
  abuf_c(s, '(');
  abuf_str(s, tokname(n->t));
  switch ((enum rtok)n->t) {
    case T_EQ:
      abuf_c(s, ' ');
      fmtnode1(s, n->operand[0]);
      abuf_c(s, ' ');
      fmtnode1(s, n->operand[1]);
      break;
    case T_IREG:
      abuf_c(s, ' ');
      abuf_u64(s, n->ival, 10);
      break;
  }
  abuf_c(s, ')');
}

usize fmtnode(char* buf, usize bufcap, node* n) {
  abuf s = abuf_make(buf, bufcap);
  fmtnode1(&s, n);
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

#if rstrst
  while (p->tok != TNone && p->err == 0) {
    Node* n = parse_next_tuple(p, PREC_LOWEST, PFlagNone);
    NodeArrayPush(&file->a, n, p->build->mem);
    NodeTransferUnresolved(file, n);

    // if we didn't end on EOF and we didn'd find a semicolon, report error
    if (UNLIKELY( p->tok != TNone && !got(p, TSemi) )) {
      syntaxerr(p, "after top level declaration");
      if (n && is_IdNode(n)) {
        b_notef(b, (PosSpan){n->pos,NoPos},
          "Did you mean \"var %s\"?", as_IdNode(n)->name);
      }
      Tok stoplist[] = { TType, TFun, TSemi, 0 };
      advance(p, stoplist);
    }
  }
#endif

static void parse(rmem* mem, const char* src) {
  /*
  fun factorial (i32) i32
    b0:
      R1 = R0  // ACC = n (argument 0)
      123 -456 0xface 0b101 F31
      \xF0\x9F\x91\xA9\xF0\x9F\x8F\xBE\xE2\x80\x8D\xF0\x9F\x9A\x80
      ret            // RES is at R0
  */
  pstate p = { .inp=src, .inend=src+strlen(src), .linestart=src, .lineno=1 };
  padvance(&p);
  while (p.tok != T_END) {
    node* n = pstmt(mem, &p, PREC_MEMBER);

    char buf[256];
    fmtnode(buf, sizeof(buf), n);
    log("input:%u:%u: parsed: %s", n->line, n->col, buf);
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
  #define _(name, ...) case name: return #name;
  RSM_FOREACH_TOKEN(_)
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _
  }
  return "?";
}
