// string formatting
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

#define _fr_c(v)  abuf_fmt(s, "\t" REG_FMTNAME_PAT, REG_FMTNAME(v))
#define _fr_nc(v) abuf_fmt(s, "\t" "R%u", v)
#define _fu(v)    abuf_fmt(s, "\t0x%x", v)
#define _fs(v)    abuf_fmt(s, "\t%d", (i32)v)

#define fr(N) ( (fl&RSM_FMT_COLOR) ? _fr_c(RSM_GET_##N(in)) : _fr_nc(RSM_GET_##N(in)) )
#define fu(N) ( RSM_GET_i(in) ? _fu(RSM_GET_##N##u(in)) : fr(N) )
#define fs(N) ( RSM_GET_i(in) ? _fs(RSM_GET_##N##s(in)) : fr(N) )

#define fi__
#define fi_A     fr(A)
#define fi_Au    fu(A)
#define fi_As    fs(A)
#define fi_AB    fr(A); fr(B)
#define fi_ABu   fr(A); fu(B)
#define fi_ABs   fr(A); fs(B)
#define fi_ABC   fr(A); fr(B); fr(C)
#define fi_ABCu  fr(A); fr(B); fu(C)
#define fi_ABCs  fr(A); fr(B); fs(C)
#define fi_ABCD  fr(A); fr(B); fr(C); fr(D)
#define fi_ABCDu fr(A); fr(B); fr(C); fu(D)
#define fi_ABCDs fr(A); fr(B); fr(C); fs(D)

void fmtinstr(abuf* s, rinstr in, rfmtflag fl) {
  abuf_str(s, rop_name(RSM_GET_OP(in)));
  switch (RSM_GET_OP(in)) {
    #define _(OP, ENC, ...) case rop_##OP: fi_##ENC; break;
    RSM_FOREACH_OP(_)
    #undef _
  }
}

usize rsm_fmtinstr(char* buf, usize bufcap, rinstr in, rfmtflag fl) {
  abuf s = abuf_make(buf, bufcap);
  fmtinstr(&s, in, fl);
  return abuf_terminate(&s);
}

usize rsm_fmtprog(char* buf, usize bufcap, rinstr* nullable ip, usize ilen, rfmtflag fl) {
  assert(ip != NULL || ilen == 0); // ok to pass NULL,0 but not NULL,>0
  abuf s1 = abuf_make(buf, bufcap); abuf* s = &s1;
  for (usize i = 0; i < ilen; i++) {
    if (i)
      abuf_c(s, '\n');
    rinstr in = ip[i];
    abuf_fmt(s, "%4lx  ", i);
    fmtinstr(s, in, fl);
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
