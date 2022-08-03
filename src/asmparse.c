// assembler: parser
// SPDX-License-Identifier: Apache-2.0
#ifndef RSM_NO_ASM
#include "rsmimpl.h"
#include "map.h"
#include "asm.h"

//#define LOG_TOKENS // define to log() token scanning
//#define LOG_AST    // define to log() parsed top-level ast nodes
//#define LOG_PRATT(args...) dlog(args) // define to log pratt dispatch
//#define PRODUCE_COMMENT_NODES // define to include comments in the AST

#ifndef LOG_PRATT
  #define LOG_PRATT(args...) ((void)0)
#endif

// parse state
typedef struct pstate pstate;
struct pstate {
  rasm*       a;          // compilation session/context
  usize       memsize;    // size of pstate memory region
  const char* inp;        // source bytes cursor (source ends with 0x00)
  const char* inend;      // source bytes end
  const char* tokstart;   // soruce start offset of current token
  rsrcpos     startpos;   // source start position of current token
  const char* linestart;  // current source position line start pointer (for column)
  u32         lineno;     // current source position line
  rtok        tok;        // current token
  bool        insertsemi; // insert RT_SEMI before next newline
  bool        isneg;      // true when parsing a negative number
  bufslabs    bufslabs;
  union { // depends on value of tok
    u64 ival; // integer value for RT_INTLIT* tokens
    struct { const char* p; u32 len; } sval; // value for RT_STRLIT
  };
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


// make sure rnode can be allocated with a slabheap by rmemalloc
#define SLABHEAP_MAX_SIZE  64 /* from rmemalloc impl */
static_assert(sizeof(rnode) <= SLABHEAP_MAX_SIZE, "slabheap miss!");


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

static rnode* setchildren2(rnode* parent, rnode* child1, rnode* nullable child2) {
  parent->children.head = child1;
  parent->children.tail = child2 ? child2 : child1;
  child1->next = child2;
  assert(child2 == NULL || child2->next == NULL);
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

static rposrange sposrange(pstate* p) {
  return (rposrange){.focus=p->startpos};
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

static void snewline(pstate* p) {
  p->lineno++;
  p->linestart = p->inp + 1;
}

static void scomment(pstate* p) { // line comment "// ... <LF>"
  p->tokstart += 2; // exclude "//"
  p->tok = RT_COMMENT;
  while (p->inp < p->inend && *p->inp != '\n')
    p->inp++;
}

static void scommentblock(pstate* p) { // /* ... */
  p->tokstart += 2; // exclude "/*"
  p->tok = RT_COMMENT;
  while (p->inp < p->inend) {
    if (*p->inp == '/') {
      if (*(p->inp - 1) == '*') {
        p->inp++; // consume '*'
        return;
      }
    } else if (*p->inp == '\n') {
      snewline(p);
    }
    p->inp++;
  }
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
  if (toklen(p) == 2 && p->tokstart[0] == 'S' && p->tokstart[1] == 'P') {
    p->tok = RT_IREG;
    p->ival = RSM_MAX_REG;
    return;
  }
  uintptr* vp = smap_lookup(&kwmap, p->tokstart, toklen(p)); // look up keyword
  if (!vp)
    return;
  p->tok = *vp & 0xff;
  if (*vp > 0xff)
    p->ival = (u64)(*vp >> sizeof(rtok)*8);
}

static u32 sstring_multiline(pstate* p, const char* start, const char* end) {
  if ((usize)(end - start) >= (usize)U32_MAX)
    return U32_MAX;

  if UNLIKELY(*start != '\n') {
    serr(p, "multiline string must start with \"|\" on a new line");
    return 0;
  }

  u32 extralen = 0;
  const char* src = start;
  const char* ind = start;
  u32 indlen = 0;
  u32 lineno = p->startpos.line;
  u32 col = p->startpos.col;

  while (src != end) {
    if (*src++ != '\n') {
      col++;
      continue;
    }

    col = 1;
    lineno++;
    const char* l = src;

    // find '|', leaving while loop with src positioned just after '|'
    while (src != end) {
      char c = *src++;
      if (c == '|') break;
      if UNLIKELY(c != ' ' && c != '\t') {
        rposrange pr = {.focus={ lineno, col }};
        errf(p->a, pr, "missing \"|\" after linebreak in multiline string");
        return 0;
      }
    }

    u32 len = (u32)((src - 1) - l);
    extralen += len;

    if (indlen == 0) {
      indlen = len;
      ind = l;
    } else if UNLIKELY(indlen != len || memcmp(l, ind, len) != 0) {
      rposrange pr = {.focus={ lineno, col }};
      warnf(p->a, pr, "inconsitent indentation of multiline string");
    }
  }
  return extralen;
}

static void sstring_buffered(pstate* p, u32 extralen, bool ismultiline) {
  const char* src = p->tokstart + 1;
  u32 len = CAST_U32((p->inp - 1) - src);
  p->sval.p = "";
  p->sval.len = 0;

  // calculate effective string length
  if (ismultiline) {
    if UNLIKELY(extralen >= len) {
      // sstring assumes \n is followed by |, but it isn't the case.
      // i.e. a string of only linebreaks.
      return serr(p, "missing \"|\" after linebreak in multiline string");
    }
    // verify indentation and calculate nbytes used for indentation
    u32 indentextralen = sstring_multiline(p, src, src + len);
    if UNLIKELY(indentextralen == 0) // an error occured
      return;
    if (check_add_overflow(extralen, indentextralen, &extralen))
      return serr(p, "string literal too large");
    src++; len--;  // sans leading '\n'
  }
  assert(extralen <= len);
  len -= extralen;

  // allocate buffer
  if UNLIKELY(len == U32_MAX - extralen)
    return serr(p, "string literal too large");
  char* dst = bufslab_alloc(&p->bufslabs, p->a->memalloc, len);
  if UNLIKELY(!dst) {
    serr(p, "unable to allocate memory for string literal");
    return;
  }

  p->sval.p = dst;
  p->sval.len = len;
  const char* chunkstart = src;

  #define FLUSH_BUF(end) { \
    usize nbyte = (usize)((end) - chunkstart); \
    memcpy(dst, chunkstart, nbyte); \
    dst += nbyte; \
  }

  if (ismultiline) {
    while (*src++ != '|') {}
    chunkstart = src;
  }

  while (src < p->inend) {
    switch (*src) {
      case '\\':
        FLUSH_BUF(src);
        src++;
        switch (*src) {
          case 'n': *dst++ = 0xA; break;
          default:  *dst++ = *src; break; // verbatim
        }
        chunkstart = ++src;
        break;
      case '\n':
        src++;
        FLUSH_BUF(src);
        // note: sstring_multiline has verified syntax already
        while (*src++ != '|') {}
        chunkstart = src;
        break;
      case '"':
        goto done;
      default:
        src++;
    }
  }
done:
  FLUSH_BUF(src);
  // if (ismultiline) {
  //   // sans leading '\n'
  //   p->sval.p++;
  //   p->sval.len--;
  // }
}

static void sstring(pstate* p) {
  p->insertsemi = true;
  u32 extralen = 0;
  bool ismultiline = false;
  while (p->inp < p->inend) {
    char c = *p->inp++;
    switch (c) {
      case '\\':
        p->inp++;
        extralen++;
        break;
      case '\n':
        snewline(p);
        ismultiline = true;
        extralen++;
        break;
      case '"': {
        if (extralen || ismultiline)
          return sstring_buffered(p, extralen, ismultiline);
        p->sval.p = p->tokstart + 1;
        p->sval.len = CAST_U32(p->inp - p->tokstart) - 2;
        return;
      }
    }
  }
  p->sval.p = "";
  p->sval.len = 0;
  serr(p, "unterminated string literal");
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
  if UNLIKELY(err || p->ival > RSM_MAX_REG) {
    return serr(p, "invalid register");
  }
}

static void sadvance(pstate* p) { // scan the next token
  const char* linestart = p->linestart;
  while (p->inp < p->inend && isspace(*p->inp)) {
    if (*p->inp == '\n')
      snewline(p);
    p->inp++;
  }

  p->tokstart = p->inp;
  if (linestart != p->linestart && p->insertsemi) {
    p->insertsemi = false;
    p->startpos.line = p->lineno - (linestart != p->linestart);
    p->startpos.col = (u32)(uintptr)(p->tokstart - linestart);
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

  // swap insertsemi
  bool insertsemi = p->insertsemi;
  p->insertsemi = false;

  // update startpos
  assert(p->tokstart >= p->linestart);
  p->startpos.line = p->lineno;
  p->startpos.col = (u32)(uintptr)(p->tokstart - p->linestart) + 1;


  // _( EQ    , ABCu , reg , "eq"    RA = RB == Cu )
  // _( NEQ   , ABCu , reg , "neq"   RA = RB != Cu )
  // _( LTU   , ABCu , reg , "ltu"   RA = RB <  Cu )
  // _( LTS   , ABCs , reg , "lts"   RA = RB <  Cs )
  // _( LTEU  , ABCu , reg , "lteu"  RA = RB <= Cu )
  // _( LTES  , ABCs , reg , "ltes"  RA = RB <= Cs )
  // _( GTU   , ABCu , reg , "gtu"   RA = RB >  Cu )
  // _( GTS   , ABCs , reg , "gts"   RA = RB >  Cs )
  // _( GTEU  , ABCu , reg , "gteu"  RA = RB >= Cu )
  // _( GTES  , ABCs , reg , "gtes"  RA = RB >= Cs )

  switch (c) {
    case '(': p->tok = RT_LPAREN; return;
    case ')': p->insertsemi = true; p->tok = RT_RPAREN; return;
    case '{': p->tok = RT_LBRACE; return;
    case '}': p->insertsemi = true; p->tok = RT_RBRACE; return;
    case ';': p->tok = RT_SEMI; return;
    case ',': p->tok = RT_COMMA; return;
    case '+': p->tok = RT_PLUS; return;
    case '*': p->tok = RT_STAR; return;
    case '%': p->tok = RT_PERC; return;
    case '&': p->tok = RT_AMP; return;
    case '|': p->tok = RT_PIPE; return;
    case '^': p->tok = RT_HAT; return;
    case '!':
      if (*p->inp == '=') { p->inp++; p->tok = RT_NEQ; return; } // !=
      return serr(p, "unexpected token \"!\"");
    case '=':
      if (*p->inp == '=') { p->inp++; p->tok = RT_EQ; return; } // ==
      p->tok = RT_ASSIGN; return; // =
    case '<':
      if (*p->inp == '=') { p->inp++; p->tok = RT_LTE; return; } // <=
      if (*p->inp == '<') { p->inp++; p->tok = RT_LT2; return; } // <<
                                      p->tok = RT_LT; return; // <
    case '>': switch (*p->inp) {
      case '>':
        p->inp++;
        if (*p->inp != '>') { p->tok = RT_GT2; return; } // >>
        p->inp++; p->tok = RT_GT3; return; // >>>
      case '=':
        p->inp++; p->tok = RT_GTE; return; // >=
      }
      p->tok = RT_GT; return; // >

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

    case '/': switch (*p->inp) {
      case '/':
        p->inp++;
        p->insertsemi = insertsemi;
        #ifdef PRODUCE_COMMENT_NODES
          return scomment(p);
        #else
          scomment(p);
          MUSTTAIL return sadvance(p);
        #endif
      case '*':
        p->inp++;
        p->insertsemi = insertsemi;
        #ifdef PRODUCE_COMMENT_NODES
          return scommentblock(p);
        #else
          scommentblock(p);
          MUSTTAIL return sadvance(p);
        #endif
      default:
        p->tok = RT_SLASH; return;
    }

    case '"': p->tok = RT_STRLIT; return sstring(p);
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
    u32 line = p->startpos.line, col = p->startpos.col;
    rtok t = p->tok;
    const char* tname = tokname(t);
    const char* tvalp = p->tokstart;
    int tvalc = (int)toklen(p);
    char buf[1024];
    switch (t) {
      case RT_INTLIT2:
      case RT_INTLIT:
      case RT_INTLIT16:
      case RT_IREG:
      case RT_FREG:
        return log("%3u:%-3u %-12s \"%.*s\"\t%llu\t0x%llx",
          line, col, tname, tvalc, tvalp, p->ival, p->ival);

      case RT_SINTLIT2:
      case RT_SINTLIT:
      case RT_SINTLIT16:
        return log("%3u:%-3u %-12s \"%.*s\"\t%lld\t0x%llx",
          line, col, tname, tvalc, tvalp, (i64)p->ival, p->ival);

      case RT_STRLIT: {
        abuf s = abuf_make(buf, sizeof(buf));
        abuf_repr(&s, p->sval.p, p->sval.len);
        abuf_terminate(&s);
        return log("%3u:%-3u %-12s \"%s\"", line, col, tname, buf);
      }

      default:
        log("%3u:%-3u %-12s \"%.*s\"", line, col, tname, tvalc, tvalp);
    }
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
// advances the scanner to the next semicolon on error.
static void eat(pstate* p, rtok t) {
  if UNLIKELY(p->tok != t) {
    perrunexpected(p, NULL, tokname(t), tokname(p->tok));
    sfastforward_semi(p);
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
  rmem_free(a->memalloc, RMEM(n, sizeof(rnode)));
}

static rnode last_resort_node = {0};

// mk* functions makes new nodes
static rnode* mknodet(pstate* p, rtok t) {
  rnode* n = rmem_alloc_aligned(p->a->memalloc, sizeof(rnode), _Alignof(rnode)).p;
  if UNLIKELY(n == NULL) {
    errf(p->a, (rposrange){0}, "out of memory");
    return &last_resort_node;
  }
  n->t = t;
  n->pos = p->startpos;
  n->next = NULL;
  n->children.head = NULL;
  n->children.tail = NULL;
  n->ival = 0;
  return n;
}

static rnode* mknode(pstate* p) { return mknodet(p, p->tok); }
static rnode* mklist(pstate* p) { return mknodet(p, RT_LPAREN); }
static rnode* mknil(pstate* p)  { return mknodet(p, RT_END); }

static rnode* mkarraytype(pstate* p, u64 size, rnode* elemtype) {
  rnode* n = mknodet(p, RT_ARRAY);
  n->ival = size;
  appendchild(n, elemtype);
  return n;
}

static rnode* infer_type(pstate* p, rnode* expr, bool useregsize) {
  rtok t = 0;
  switch (expr->t) {
    case RT_INTLIT2:
    case RT_INTLIT:
    case RT_INTLIT16:
      t = ( useregsize ? RT_I64 :
            expr->ival <= U8_MAX  ? RT_I8 :
            expr->ival <= U16_MAX ? RT_I16 :
            expr->ival <= U32_MAX ? RT_I32 :
            RT_I64 );
      break;
    case RT_SINTLIT2:
    case RT_SINTLIT:
    case RT_SINTLIT16: {
      i64 v = (i64)expr->ival;
      t = ( useregsize ? RT_I64 :
            (v >= I8_MIN  && v <= I8_MAX)  ? RT_I8 :
            (v >= I16_MIN && v <= I16_MAX) ? RT_I16 :
            (v >= I32_MIN && v <= I32_MAX) ? RT_I32 :
            RT_I64 );
      break;
    }
    case RT_STRLIT:
      return mkarraytype(p, (u64)expr->sval.len, mknodet(p, RT_I8));
    case RT_END:
      if (p->a->errcount)
        break;
      FALLTHROUGH;
    default:
      perr(p, expr, "cannot infer type of %s", nname(expr));
  }
  return mknodet(p, t);
}

static rnode* pexpr(PPARAMS) {
  rnode* n = pstmt(PARGS);
  if UNLIKELY(!tokisexpr(n->t)) {
    if (n->t != RT_END || p->a->errcount == 0)
      perrunexpected(p, n, "expression", nname(n));
  }
  return n;
}

static rnode* ptype(PPARAMS) {
  if UNLIKELY(!tokistype(p->tok)) {
    perrunexpected(p, NULL, "type", tokname(p->tok));
    return mknil(p);
  }
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

static rnode* prefix_strlit(PPARAMS) {
  rnode* n = mknode(p);
  n->sval.p = p->sval.p;
  n->sval.len = p->sval.len;
  sadvance(p);
  return n;
}

// storagedef = constdef | datadef
// constdef   = "const" name type? "=" expr ";"
// datadef    = "data" name (type ("=" expr)? | "=" expr) ";"
static rnode* prefix_storage(PPARAMS) {
  rnode* n = mknode(p);
  sadvance(p);
  if UNLIKELY(p->tok != RT_NAME) {
    perrunexpected(p, NULL, "name", tokname(p->tok));
    n->sval.len = 0;
    return n;
  }
  n->sval.p = p->tokstart;
  n->sval.len = toklen32(p);
  sadvance(p);

  rnode* typ = NULL;
  rnode* init = NULL;
  if (p->tok != RT_ASSIGN)
    typ = ptype(PARGS);

  if (n->t == RT_CONST) { // const must have initial value
    eat(p, RT_ASSIGN);
    init = pexpr(PARGS);
  } else { // data must have at least type or initial value
    assert(n->t == RT_DATA);
    if (typ) {
      if (got(p, RT_ASSIGN))
        init = pexpr(PARGS);
    } else {
      eat(p, RT_ASSIGN);
      init = pexpr(PARGS);
    }
  }

  if (typ == NULL) { // infer type
    // use register size for integer data, e.g. "data x = 3" => "data x i64 = 3"
    bool useregsize = n->t == RT_DATA;
    typ = infer_type(p, assertnotnull(init), useregsize);
    typ->pos = init->pos;
    if UNLIKELY(useregsize && init->t != RT_STRLIT) {
      // TODO: consider making this an error
      warnf(p->a, (rposrange){.focus=n->pos},
        "integer data without explicit type defaults to %s", nname(typ));
    }
  }

  setchildren2(n, typ, init);
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
  n->sval.p = p->tokstart;
  n->sval.len = toklen32(p);
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

// static rnode* pcall(PPARAMS, rnode* n) {
//   dlog("CALL");
//   // call destination is the only operand
//   rnode* operand = poperand(PARGS);
//   appendchild(n, operand);

//   // [sugar] remaining operands prepended as args R0...
//   while (p->tok != RT_SEMI && p->tok != RT_END) {
//     rnode* arg = poperand(PARGS);
//     appendchild(n, operand);
//   }

//   if (n->children.head) for (rnode* arg = )
// }

// operation = op operand*
static rnode* prefix_op(PPARAMS) {
  rnode* n = prefix_int(PARGS);
  // if (n->ival == rop_CALL)
  //   return pcall(PARGS, n);
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
      // case RT_ASSIGN:
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
  n->sval.len--; // trim off trailing ":"
  assert(n->sval.p[n->sval.len] == ':');
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

  // name (can be an op, e.g. "read")
  if (p->tok != RT_NAME && p->tok != RT_OP)
    perrunexpected(p, NULL, "name", tokname(p->tok));
  n->sval.p = p->tokstart;
  n->sval.len = toklen32(p);
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
    block0->sval.p = kBlock0Name;
    block0->sval.len = 2;
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
  [RT_ASSIGN]    = {NULL, infix_eq, PREC_ASSIGN},

  #define _(tok, ...) [tok] = {NULL, infix_binop, PREC_BINOP},
  RSM_FOREACH_BINOP_TOKEN(_)
  #undef _

  [RT_INTLIT2]   = {prefix_int, NULL, 0},
  [RT_SINTLIT2]  = {prefix_int, NULL, 0},
  [RT_INTLIT]    = {prefix_int, NULL, 0},
  [RT_SINTLIT]   = {prefix_int, NULL, 0},
  [RT_INTLIT16]  = {prefix_int, NULL, 0},
  [RT_SINTLIT16] = {prefix_int, NULL, 0},
  [RT_STRLIT]    = {prefix_strlit, NULL, 0},

  [RT_FUN] = {prefix_fun, NULL, 0},
};

// stmt = anynode ";"
static rnode* pstmt(PPARAMS) {
  const parselet* ps = &parsetab[p->tok];

  if UNLIKELY(!ps->prefix) {
    if (p->tok == RT_END)
      return mknil(p);
    LOG_PRATT("PREFIX %s not found", tokname(p->tok));
    perr(p, NULL, "unexpected %s", tokname(p->tok));
    rnode* n = mknil(p);
    sfastforward_semi(p);
    return n;
  }

  LOG_PRATT("PREFIX %s call", tokname(p->tok));
  UNUSED const void* p1 = p->inp;
  UNUSED bool insertsemi = p->insertsemi;
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


#define PSTATE_BUFSLAB0_OFFS ALIGN2(sizeof(pstate), _Alignof(bufslab))
#define PSTATE_ALLOC_SIZE    (PSTATE_BUFSLAB0_OFFS + sizeof(bufslab) + BUFSLAB_MIN_CAP)

void pstate_dispose(pstate* p) {
  // note: p->bufslabs.head is part of the pstate allocation
  bufslab_freerest(p->bufslabs.head, p->a->memalloc);
  #ifdef DEBUG
  memset(p, 0, PSTATE_BUFSLAB0_OFFS + sizeof(bufslab));
  #endif
  rmem_free(p->a->memalloc, RMEM(p, p->memsize));
}

rnode* nullable rasm_parse(rasm* a) {
  rasm_stop_set(a, false);
  a->errcount = 0;

  pstate* p = rasm_pstate(a);
  if (!p) {
    usize align = MAX(_Alignof(pstate), _Alignof(bufslab));
    rmem_t m = rmem_alloc_aligned(a->memalloc, PSTATE_ALLOC_SIZE, align);
    if (!m.p)
      return NULL;
    p = m.p;
    memset(p, 0, sizeof(pstate));
    rasm_pstate_set(a, p);
    p->a = a;
    p->memsize = m.size;
    p->bufslabs.head = (void*)p + PSTATE_BUFSLAB0_OFFS;
    p->bufslabs.head->next = NULL;
    p->bufslabs.head->len = 0;
    p->bufslabs.head->cap = BUFSLAB_MIN_CAP;
  }
  p->inp       = a->srcdata;
  p->inend     = a->srcdata + a->srclen;
  p->linestart = a->srcdata;
  p->lineno    = 1;
  bufslabs_reset(&p->bufslabs);

  #ifdef LOG_AST
    char buf[4096*8];
  #endif

  dlog("parsing \"%s\"", a->srcname);
  sadvance(p); // prime parser with initial token
  rnode* module = mklist(p);

  while (p->tok != RT_END && !rasm_stop(a)) {
    rnode* n = pstmt(p, PREC_LOWEST);
    if LIKELY(p->tok != RT_END) // every statement ends with a semicolon
      eat(p, RT_SEMI);
    appendchild(module, n);

    if UNLIKELY(n->t != RT_FUN && n->t != RT_CONST && n->t != RT_DATA) {
      perr(p, n, "unexpected top-level statement");
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
        abuf_append(s, n->sval.p, n->sval.len); break;

      case RT_OP:
        abuf_str(s, rop_name((rop)n->ival)); break;

      case RT_SINTLIT2:  abuf_c(s, '-'); FALLTHROUGH;
      case RT_INTLIT2:   abuf_str(s, "0b"); abuf_u64(s, n->ival, 2); break;
      case RT_SINTLIT:   abuf_c(s, '-'); FALLTHROUGH;
      case RT_INTLIT:    abuf_u64(s, n->ival, 10); break;
      case RT_SINTLIT16: abuf_c(s, '-'); FALLTHROUGH;
      case RT_INTLIT16:  abuf_str(s, "0x"); abuf_u64(s, n->ival, 16); break;

      case RT_STRLIT: {
        abuf_c(s, '"');
        abuf_repr(s, n->sval.p, n->sval.len);
        abuf_c(s, '"');
        break;
      }
      case RT_ARRAY:
        abuf_u64(s, n->ival, 10); break;
        break;

      // no extra fields
      case RT_END: case rtok_COUNT:
      case RT_LPAREN: case RT_RPAREN:
      case RT_LBRACE: case RT_RBRACE:
      case RT_SEMI:
      case RT_COMMA:
      case RT_ASSIGN:

      case RT_PLUS:
      case RT_MINUS:
      case RT_STAR:
      case RT_SLASH:
      case RT_PERC:
      case RT_AMP:
      case RT_PIPE:
      case RT_HAT:
      case RT_LT2:
      case RT_GT2:
      case RT_GT3:
      case RT_LT:
      case RT_GT:

      case RT_I1:
      case RT_I8:
      case RT_I16:
      case RT_I32:
      case RT_I64:
        s->p = sp; s->len--; // undo abuf_c(s, ' ')
        break;

       break;
    }
    if (s->p == sp+1)
      dlog("TODO fmtnode1 %s", tokname(n->t));
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


// dlog_keywords
#if 0 && defined(DEBUG) && !defined(RSM_NO_LIBC)
  static int cstrsort(const char** p1, const char** p2, void* ctx) {
    return strcmp(*p1, *p2);
  }
  UNUSED static void dlog_keywords() {
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
  #define dlog_keywords(...) ((void)0)
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

rerror init_asmparse() {
  // create kwmap
  #if USIZE_MAX >= 0xFFFFFFFFFFFFFFFFu
    static u8 memory[3072] ATTR_ALIGNED(sizeof(void*));
  #else
    static u8 memory[1552] ATTR_ALIGNED(sizeof(void*));
  #endif

  // temporary sparse stack memory for building the map
  void* membuf[(PAGE_SIZE * 2) / sizeof(void*)] ATTR_ALIGNED(RMEM_ALLOC_ALIGN);
  rmemalloc_t* ma =
    assertnotnull( rmem_allocator_create_buf(NULL, membuf, sizeof(membuf)) );

  smap tmpmap_;
  smap* m = smap_make(&tmpmap_, ma, kwcount, MAPLF_2);
  if UNLIKELY(m == NULL) {
    assertf(0, "sizeof(membuf) too small");
    return rerr_nomem;
  }

  m->hash0 = 0x89f025ba;
  uintptr* vp;

  #define _(op, enc, res, kw, ...) \
    vp = assertnotnull(smap_assign(m, kw, strlen(kw))); \
    *vp = (RT_OP | ((uintptr)rop_##op << (sizeof(rtok)*8)));
  RSM_FOREACH_OP(_)
  #undef _

  #define _(token, kw) \
    vp = assertnotnull(smap_assign(m, kw, strlen(kw))); \
    *vp = token;
  RSM_FOREACH_KEYWORD_TOKEN(_)
  #undef _

  // copy into BSS memory
  rmem_t entries_mem = RMEM(memory, sizeof(memory));
  usize ideal_size = smap_copy(&kwmap, m, entries_mem, ma);
  assertf(ideal_size <= sizeof(memory),
    "not enough memory %zu; need %zu", sizeof(memory), ideal_size);
  if (ideal_size < sizeof(memory))
    dlog("kwmap uses only %zu B memory -- trim 'memory'", ideal_size);

  // // smap_cfmt prints C code for a constant static representation of m
  // rmem_t buf = rmem_alloc(ma, rmem_avail(ma));
  // usize n = smap_cfmt(buf.p, buf.size, m, "lolcat");
  // dlog("kwmap:\n%.*s", (int)n, (const char*)buf.p);

  dlog_keywords();

  return 0;
}


#endif // RSM_NO_ASM
