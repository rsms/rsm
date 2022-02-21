// string formatting
#include "rsmimpl.h"

#define RA  FMTR(RSM_GET_A(in))
#define RB  FMTR(RSM_GET_B(in))
#define RC  FMTR(RSM_GET_C(in))
#define RD  FMTR(RSM_GET_D(in))

#define RAu ( RSM_GET_Ai(in) ? FMTu((u64)RSM_GET_Au(in)) : RA )
#define RBu ( RSM_GET_Bi(in) ? FMTu((u64)RSM_GET_Bu(in)) : RB )
#define RCu ( RSM_GET_Ci(in) ? FMTu((u64)RSM_GET_Cu(in)) : RC )
#define RDu ( RSM_GET_Di(in) ? FMTu((u64)RSM_GET_Du(in)) : RD )

#define RAs ( RSM_GET_Ai(in) ? FMTs((i64)RSM_GET_As(in)) : RA )
#define RBs ( RSM_GET_Bi(in) ? FMTs((i64)RSM_GET_Bs(in)) : RB )
#define RCs ( RSM_GET_Ci(in) ? FMTs((i64)RSM_GET_Cs(in)) : RC )
#define RDs ( RSM_GET_Di(in) ? FMTs((i64)RSM_GET_Ds(in)) : RD )

// ANSI colors: (\e[3Nm or \e[9Nm) 1 red, 2 green, 3 yellow, 4 blue, 5 magenta, 6 cyan
#define FMTR(v) abuf_fmt(s, "\t\e[9%cmR%u\e[39m", '1'+((v)%6), (v))
#define FMTu(v) abuf_fmt(s, "\t0x%llx", (v))
#define FMTs(v) abuf_fmt(s, "\t%lld", (v))

static void fi__(abuf* s, rinstr in)     { }
static void fi_A(abuf* s, rinstr in)     { RA; }
// static void fi_Au(abuf* s, rinstr in)    { RAu; }
// static void fi_As(abuf* s, rinstr in)    { RAs; }
static void fi_AB(abuf* s, rinstr in)    { fi_A(s, in);   RB; }
static void fi_ABu(abuf* s, rinstr in)   { fi_A(s, in);   RBu; }
static void fi_ABs(abuf* s, rinstr in)   { fi_A(s, in);   RBs; }
// static void fi_ABC(abuf* s, rinstr in)   { fi_AB(s, in);  RC; }
static void fi_ABCu(abuf* s, rinstr in)  { fi_AB(s, in);  RCu; }
static void fi_ABCs(abuf* s, rinstr in)  { fi_AB(s, in);  RCs; }
// static void fi_ABCD(abuf* s, rinstr in)  { fi_ABC(s, in); RD; }
// static void fi_ABCDu(abuf* s, rinstr in) { fi_ABC(s, in); RDu; }
// static void fi_ABCDs(abuf* s, rinstr in) { fi_ABC(s, in); RDs; }

static void fmtinstr1(abuf* s, rinstr in) {
  abuf_str(s, rop_name(RSM_GET_OP(in)));
  switch (RSM_GET_OP(in)) {
    #define _(OP, ENC, ...) case rop_##OP: fi_##ENC(s, in); break;
    RSM_FOREACH_OP(_)
    #undef _
  }
}

usize rsm_fmtinstr(char* buf, usize bufcap, rinstr in) {
  abuf s = abuf_make(buf, bufcap);
  fmtinstr1(&s, in);
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
    fmtinstr1(s, in);
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
