#include "rsmimpl.h"

#ifdef DEBUG
  static void logstate_header() {
    fprintf(stderr, "\e[2m");
    for (int i = 0; i < 6; i++)
      fprintf(stderr, "\e[9%cm  R%d\e[39m", '1'+(i%6), i);
    fprintf(stderr, "  │  PC  INSTRUCTION\e[22m\n");
  }
  static void logstate(u64* nullable iregs, rinstr* inv, isize pc) {
    for (int i = 0; i < 6; i++)
      fprintf(stderr, "\e[9%cm%4llx\e[39m", '1'+(i%6), iregs[i]);
    char buf[128];
    rsm_fmtinstr(buf, sizeof(buf), inv[pc]);
    fprintf(stderr, "  │ %3ld  %s\n", pc, buf);
  }
#else
  #define logstate_header(...) ((void)0)
  #define logstate(...) ((void)0)
#endif


void rsm_vmexec(u64* iregs, rinstr* inv, u32 incount) {
  #define RA  iregs[ar]
  #define RB  iregs[RSM_GET_B(in)]
  #define RC  iregs[RSM_GET_C(in)]
  #define RD  iregs[RSM_GET_D(in)]

  #define RAu ( RSM_GET_Ai(in) ? (u64)RSM_GET_Au(in) : RA )
  #define RBu ( RSM_GET_Bi(in) ? (u64)RSM_GET_Bu(in) : RB )
  #define RCu ( RSM_GET_Ci(in) ? (u64)RSM_GET_Cu(in) : RC )
  #define RDu ( RSM_GET_Di(in) ? (u64)RSM_GET_Du(in) : RD )

  #define RAs ( RSM_GET_Ai(in) ? (i64)RSM_GET_As(in) : (i64)RA )
  #define RBs ( RSM_GET_Bi(in) ? (i64)RSM_GET_Bs(in) : (i64)RB )
  #define RCs ( RSM_GET_Ci(in) ? (i64)RSM_GET_Cs(in) : (i64)RC )
  #define RDs ( RSM_GET_Di(in) ? (i64)RSM_GET_Ds(in) : (i64)RD )

  isize pc = 0; // program counter; current instruction#
  logstate_header();

  for (;;) {
    assertf(pc > -1 && pc < (isize)incount, "pc=%ld incount=%u", pc, incount);
    logstate(iregs, inv, pc);

    rinstr in = inv[pc++]; // load current instruction and advance pc
    u8 ar = RSM_GET_A(in); // argument Ar, which almost every instruction needs
    switch ((enum rop)RSM_GET_OP(in)) {

    case rop_MOVE:  { RA = RBu; break; }
    case rop_LOADK: { dlog("TODO"); break; }

    case rop_ADD:   { RA = RB + RCu; break; }
    case rop_SUB:   { RA = RB - RCu; break; }
    case rop_MUL:   { RA = RB * RCu; break; }
    case rop_DIV:   { RA = RB / RCu; break; }
    case rop_MOD:   { RA = RB % RCu; break; }
    case rop_AND:   { RA = RB & RCu; break; }
    case rop_OR:    { RA = RB | RCu; break; }
    case rop_XOR:   { RA = RB ^ RCu; break; }
    case rop_SHL:   { RA = RB << RCu; break; }
    case rop_SHRS:  { RA = (u64)((i64)RB >> RCs); break; }
    case rop_SHRU:  { RA = RB >> RCu; break; }

    case rop_CMPEQ: { RA = RB == RCu; break; }
    case rop_CMPLT: { RA = RB < RCu; break; }
    case rop_CMPGT: { RA = RB > RCu; break; }

    case rop_BRZ:  if (RA == 0) pc = RSM_GET_Bi(in) ? pc + RSM_GET_Bs(in) : RB; break;
    case rop_BRNZ: if (RA != 0) pc = RSM_GET_Bi(in) ? pc + RSM_GET_Bs(in) : RB; break;

    case rop_RET:   { return; }

    case RSM_OP_COUNT: assert(0);
  }}
}
