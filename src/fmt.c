// string formatting
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"
#include "abuf.h"

#define _fr_c(v) ({ \
  u32 r__ = (v); \
  r__ == RSM_MAX_REG ? abuf_fmt(s, "\t" "\e[9%cm" "SP" "\e[39m", REG_FMTCOLORC(r__)) : \
  abuf_fmt(s, "\t" REG_FMTNAME_PAT, REG_FMTNAME(r__)); \
})
#define _fr_nc(v) abuf_fmt(s, "\tR%u", v)
#define _fu(v)    abuf_fmt(s, "\t0x%x", v)
#define _fs(v)    abuf_fmt(s, "\t%d", (i32)v)

#define fr(N) ( (fl&RSM_FMT_COLOR) ? _fr_c(RSM_GET_##N(in)) : _fr_nc(RSM_GET_##N(in)) )
#define fu(N) ( RSM_GET_i(in) ? _fu(RSM_GET_##N##u(in)) : fr(N) )
#define fs(N) ( RSM_GET_i(in) ? _fs(RSM_GET_##N##s(in)) : fr(N) )


u32 fmtinstr(abuf_t* s, rin_t in, rfmtflag_t fl) {
  #define fi__     break;
  #define fi_A     fr(A); break;
  #define fi_Au    fu(A); break;
  #define fi_As    fs(A); break;
  #define fi_AB    fr(A); fr(B); break;
  #define fi_ABv   fr(A); fu(B); assert(RSM_GET_OP(in)==rop_COPYV); return 1+RSM_GET_Bu(in);
  #define fi_ABu   fr(A); fu(B); break;
  #define fi_ABs   fr(A); fs(B); break;
  #define fi_ABC   fr(A); fr(B); fr(C); break;
  #define fi_ABCu  fr(A); fr(B); fu(C); break;
  #define fi_ABCs  fr(A); fr(B); fs(C); break;
  #define fi_ABCD  fr(A); fr(B); fr(C); fr(D); break;
  #define fi_ABCDu fr(A); fr(B); fr(C); fu(D); break;
  #define fi_ABCDs fr(A); fr(B); fr(C); fs(D); break;
  abuf_str(s, rop_name(RSM_GET_OP(in)));
  switch (RSM_GET_OP(in)) {
    #define _(OP, ENC, ...) case rop_##OP: fi_##ENC
    RSM_FOREACH_OP(_)
    #undef _
  }
  return 1;
}

usize rsm_fmtinstr(char* buf, usize bufcap, rin_t in, u32* pcaddp, rfmtflag_t fl) {
  abuf_t s = abuf_make(buf, bufcap);
  u32 pcadd = fmtinstr(&s, in, fl);
  if (pcaddp)
    *pcaddp = pcadd;
  return abuf_terminate(&s);
}

usize rsm_fmtprog(
  char* buf, usize bufcap, const rin_t* nullable ip, usize ilen, rfmtflag_t fl)
{
  assert(ip != NULL || ilen == 0); // ok to pass NULL,0 but not NULL,>0
  abuf_t s1 = abuf_make(buf, bufcap); abuf_t* s = &s1;
  for (usize i = 0; i < ilen; i++) {
    if (i)
      abuf_c(s, '\n');
    rin_t in = ip[i];
    abuf_fmt(s, "%4zx  ", i);
    u32 pcadd = fmtinstr(s, in, fl);

    // variable imm
    while (--pcadd)
      abuf_fmt(s, " 0x%08x", ip[++i]);
  }
  return abuf_terminate(s);
}
