// string formatting
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

static void _fr(abuf* s, u32 v) {
  assert(v < 32);
  abuf_fmt(s, "\t" REG_FMTNAME_PAT, REG_FMTNAME(v));
}
static void _fu(abuf* s, u32 v) { abuf_fmt(s, "\t0x%x", v); }
static void _fs(abuf* s, u32 v) { abuf_fmt(s, "\t%d", (i32)v); }

#define fr(N) _fr(s, RSM_GET_##N(in))
#define fu(N) (RSM_GET_i(in) ? _fu(s, RSM_GET_##N##u(in)) : _fr(s, RSM_GET_##N##u(in)))
#define fs(N) (RSM_GET_i(in) ? _fs(s, RSM_GET_##N##s(in)) : _fr(s, RSM_GET_##N##s(in)))

DIAGNOSTIC_IGNORE_PUSH("-Wunused-function")
static void fi__(abuf* s, rinstr in)     { }
static void fi_A(abuf* s, rinstr in)     { fr(A); }
static void fi_Au(abuf* s, rinstr in)    { fu(A); }
static void fi_As(abuf* s, rinstr in)    { fs(A); }
static void fi_AB(abuf* s, rinstr in)    { fr(A); fr(B); }
static void fi_ABu(abuf* s, rinstr in)   { fr(A); fu(B); }
static void fi_ABs(abuf* s, rinstr in)   { fr(A); fs(B); }
static void fi_ABC(abuf* s, rinstr in)   { fr(A); fr(B); fr(C); }
static void fi_ABCu(abuf* s, rinstr in)  { fr(A); fr(B); fu(C); }
static void fi_ABCs(abuf* s, rinstr in)  { fr(A); fr(B); fs(C); }
static void fi_ABCD(abuf* s, rinstr in)  { fr(A); fr(B); fr(C); fr(D); }
static void fi_ABCDu(abuf* s, rinstr in) { fr(A); fr(B); fr(C); fu(D); }
static void fi_ABCDs(abuf* s, rinstr in) { fr(A); fr(B); fr(C); fs(D); }
DIAGNOSTIC_IGNORE_POP()

void fmtinstr(abuf* s, rinstr in) {
  abuf_str(s, rop_name(RSM_GET_OP(in)));
  switch (RSM_GET_OP(in)) {
    #define _(OP, ENC, ...) case rop_##OP: fi_##ENC(s, in); break;
    RSM_FOREACH_OP(_)
    #undef _
  }
}

usize rsm_fmtinstr(char* buf, usize bufcap, rinstr in) {
  abuf s = abuf_make(buf, bufcap);
  fmtinstr(&s, in);
  return abuf_terminate(&s);
}

usize rsm_fmtprog(char* buf, usize bufcap, rinstr* nullable ip, usize ilen) {
  assert(ip != NULL || ilen == 0); // ok to pass NULL,0 but not NULL,>0
  abuf s1 = abuf_make(buf, bufcap); abuf* s = &s1;
  for (usize i = 0; i < ilen; i++) {
    if (i)
      abuf_c(s, '\n');
    rinstr in = ip[i];
    abuf_fmt(s, "%4lx  ", i);
    fmtinstr(s, in);
  }
  return abuf_terminate(s);
}


const char* rop_name(rop op) {
  switch (op) {
    #define _(name, enc, asmname, ...) case rop_##name: return asmname;
    RSM_FOREACH_OP(_)
    #undef _
  }
  return "?";
}
