// assembler
#include "rsm.h"
#include "util.h"

// rtok, T_* -- source tokens
typedef u8 rtok;
#define RSM_FOREACH_TOKEN(_) \
_( END ) \
_( COMMENT ) \
_( LPAREN ) _( RPAREN ) \
_( SEMI   ) /* ;    */ \
_( EQ     ) /* =    */ \
_( SLASH  ) /* /    */ \
_( LABEL  ) /* foo: */ \
_( REG    ) /* r4   */ \
_( SYM    ) /* foo  */ \
_( INT    ) /* 123  */ \
/* keywords */ \
_( KW_FUN ) \
_( KW_I1  ) \
_( KW_I8  ) \
_( KW_I16 ) \
_( KW_I32 ) \
_( KW_I64 ) \
// end RSM_FOREACH_TOKEN
enum rtok {
  #define _(name, ...) T_##name,
  RSM_FOREACH_TOKEN(_)
  #undef _
  rtok_COUNT
} END_TYPED_ENUM(rtok)

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

  const char* nullable errstr; // non-null when an error occured
};

#define UTF8_SELF 0x80

#define isdigit(c)    ( ((u32)(c) - '0') < 10 )                 /* 0-9 */
#define isalpha(c)    ( ((u32)(c) | 32) - 'a' < 26 )            /* A-Za-z */
#define isalnum(c)    ( isdigit(c) || isalpha(c) )              /* 0-9A-Za-z */
#define isupper(c)    ( ((u32)(c) - 'A') < 26 )                 /* A-Z */
#define islower(c)    ( ((u32)(c) - 'a') < 26 )                 /* a-z */
#define isprint(c)    ( ((u32)(c) - 0x20) < 0x5f )              /* SP-~ */
#define isgraph(c)    ( ((u32)(c) - 0x21) < 0x5e )              /* !-~ */
#define isspace(c)    ( (c) == ' ' || (u32)(c) - '\t' < 5 )     /* SP, \{tnvfr} */
#define ishexdigit(c) ( isdigit(c) || ((u32)c | 32) - 'a' < 6 ) /* 0-9A-Fa-f */

#define isname(c) (isalnum(c) || (c) == '_')

static const char* tokname(rtok t);
static usize toklen(pstate* p) { // length in bytes of current token
  return (usize)(uintptr)(p->inp - p->tokstart);
}

static u32 pcolumn(pstate* p) { // source column of current token
  return (u32)((uintptr)p->tokstart - (uintptr)p->linestart) + 1;
}

static rtok perr(pstate* p, const char* errstr) {
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
    if (!utf8chomp(p)) {
      perr(p, "invalid UTF8 sequence");
      break;
    }
  }
  return p->tok = T_SYM;
}

static rtok sname(pstate* p) {
  while (p->inp < p->inend && isname(*p->inp))
    p->inp++;
  p->tok = T_SYM;
  if (p->inp < p->inend && (u8)*p->inp >= UTF8_SELF)
    snameunicode(p);
  if (p->inp < p->inend && *p->inp == ':') {
    p->inp++;
    return p->tok = T_LABEL;
  }
  usize len = toklen(p);
  if (len > 1 && len < 4 && *p->tokstart == 'R' && isdigit(p->tokstart[1])) { // Rnn
    p->tokstart++;
    p->tok = T_REG;
  }
  #define KEYWORD(kw,t) \
    if (len == strlen(kw) && memcmp((kw), p->tokstart, len) == 0) return p->tok = t;
  KEYWORD("fun", T_KW_FUN);
  KEYWORD("i1",  T_KW_I1);
  KEYWORD("i8",  T_KW_I8);
  KEYWORD("i16", T_KW_I16);
  KEYWORD("i32", T_KW_I32);
  KEYWORD("i64", T_KW_I64);
  p->insertsemi = true;
  return p->tok;
}

static rtok snumber(pstate* p) {
  while (p->inp < p->inend && isdigit(*p->inp))
    p->inp++;
  p->insertsemi = true;
  return p->tok = T_INT;
}

static rtok scantok(pstate* p) { // read the next token
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
    return p->tok = T_END;
  }

  char c = *p->inp++; // read current byte and advance cursor
  // dlog("0x%02x %c", c, isprint(c) ? c : ' ');

  switch (c) {
    case '(':         return p->tok = T_LPAREN;
    case ')':         return p->tok = T_RPAREN;
    case '=':         return p->tok = T_EQ;
    case ';':         return p->tok = T_SEMI;
    case '0' ... '9': return snumber(p);
    case '/': switch (*p->inp) {
      case '/': p->inp++; return scomment(p);
      default:        return p->tok = T_SLASH;
    }
    default: // anything else is the start of a name (symbol, label etc)
      if ((u8)c >= UTF8_SELF) {
        p->inp--;
        return snameunicode(p);
      }
      return sname(p);
  }
}

static void parse(const char* src) {
  pstate p = { .inp=src, .inend=src+strlen(src), .linestart=src, .lineno=1 };
  while (scantok(&p) != T_END) {
    log("%3u:%-3u %s\t\"%.*s\"",
      p.lineno, pcolumn(&p), tokname(p.tok), (int)toklen(&p), p.tokstart);
  }
  if (p.errstr)
    log("input:%u:%u: error: %s", p.lineno, pcolumn(&p), p.errstr);
}

usize rsm_asm(rinstr* idst, usize idstcap, const char* src) {
  parse(src);
  return 0;
}


static const char* tokname(rtok t) {
  switch (t) {
  #define _(name, ...) case T_##name: return #name;
  RSM_FOREACH_TOKEN(_)
  #undef _
  }
  return "?";
}
