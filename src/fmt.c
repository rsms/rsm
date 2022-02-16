// string formatting
#include "rsm.h"

// ANSI colors: (\e[3Nm or \e[9Nm) 1 red, 2 green, 3 yellow, 4 blue, 5 magenta, 6 cyan
#define FMT_R(s,v) rabuf_appendfmt(s, "\t\e[9%cmR%u\e[39m", '1'+((v)%6), (v))
#define FMT_K(s,v) rabuf_appendfmt(s, "\tK%u", (v))
#define FMT_U(s,v) rabuf_appendfmt(s, "\t0x%x", (v))
#define FMT_S(s,v) rabuf_appendfmt(s, "\t%d", (int)(v))

void fmtinstr__(rabuf* s, rinstr in)     { }
void fmtinstr_A(rabuf* s, rinstr in)     { FMT_R(s,RSM_GET_Ar(in)); }
void fmtinstr_Au(rabuf* s, rinstr in)    { FMT_U(s,RSM_GET_Au(in)); }
void fmtinstr_As(rabuf* s, rinstr in)    { FMT_S(s,RSM_GET_As(in)); }
void fmtinstr_AB(rabuf* s, rinstr in)    { fmtinstr_A(s, in);   FMT_R(s,RSM_GET_Br(in)); }
void fmtinstr_ABu(rabuf* s, rinstr in)   { fmtinstr_A(s, in);   FMT_U(s,RSM_GET_Bu(in)); }
void fmtinstr_ABs(rabuf* s, rinstr in)   { fmtinstr_A(s, in);   FMT_S(s,RSM_GET_Bs(in)); }
void fmtinstr_ABk(rabuf* s, rinstr in)   { fmtinstr_A(s, in);   FMT_K(s,RSM_GET_Bu(in)); }
void fmtinstr_ABC(rabuf* s, rinstr in)   { fmtinstr_AB(s, in);  FMT_R(s,RSM_GET_Cr(in)); }
void fmtinstr_ABCu(rabuf* s, rinstr in)  { fmtinstr_AB(s, in);  FMT_U(s,RSM_GET_Cu(in)); }
void fmtinstr_ABCs(rabuf* s, rinstr in)  { fmtinstr_AB(s, in);  FMT_R(s,RSM_GET_Cs(in)); }
void fmtinstr_ABCD(rabuf* s, rinstr in)  { fmtinstr_ABC(s, in); FMT_R(s,RSM_GET_Du(in)); }
void fmtinstr_ABCDs(rabuf* s, rinstr in) { fmtinstr_ABC(s, in); FMT_R(s,RSM_GET_Ds(in)); }

usize fmtprog(char* buf, usize bufcap, rinstr* ip, usize ilen) {
  rabuf s1 = rabuf_make(buf, bufcap); rabuf* s = &s1;
  for (usize i = 0; i < ilen; i++) {
    rinstr in = ip[i];
    rabuf_appendfmt(s, "%4lx  %s", i, rop_name(RSM_GET_OP(in)));
    switch (RSM_GET_OP(in)) {
      #define _(name, args, ...) case rop_##name: fmtinstr_##args(s, in); break;
      DEF_RSM_OPS(_)
      #undef _
    }
    rabuf_appendc(s, '\n');
  }
  return rabuf_terminate(s);
}


// logbin is a little debug/development function which logs a number
// in binary, unsigned decimal and signed decimal.
void logbin(u32 v) {
  char buf[32];
  usize n = rstrfmtu64(buf, v, 2);
  log("\e[2mbit   3322222222221111111111          \e[22m\n"
      "\e[2m      10987654321098765432109876543210\e[22m\n"
      "\e[2mbin   %.*s\e[22m%.*s\n"
      "\e[2mdec u \e[22m%u (0x%x)\n"
      "\e[2mdec s \e[22m%d",
      (int)(32-n), "00000000000000000000000000000000",
      (int)n, buf,
      v, v,
      (int)v);
}


const char* rop_name(rop op) {
  switch (op) {
    #define _(name, ...) case rop_##name: return #name;
    DEF_RSM_OPS(_)
    #undef _
  }
  return "?";
}


const char* rtype_name(rtype t) {
  switch (t) {
    #define _(name, ...) case rtype_##name: return #name;
    DEF_RSM_TYPES(_)
    #undef _
  }
  return "?";
}
