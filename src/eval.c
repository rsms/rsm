#include "rsm.h"

static void logstate(u64* nullable iregs, rinstr* inv, isize pc) {
  #ifdef DEBUG
  if (pc == 0) {
    fprintf(stderr, "\e[2m");
    for (int i = 0; i < 6; i++)
      fprintf(stderr, "\e[9%cm  R%d\e[39m", '1'+(i%6), i);
    fprintf(stderr, "  │  PC  INSTRUCTION\e[22m\n");
  }
  for (int i = 0; i < 6; i++)
    fprintf(stderr, "\e[9%cm%4llx\e[39m", '1'+(i%6), iregs[i]);
  char buf[128];
  rsm_fmtinstr(buf, sizeof(buf), inv[pc]);
  fprintf(stderr, "  │ %3ld  %s\n", pc, buf);
  #endif
}


void rsm_eval(u64* iregs, rinstr* inv, u32 incount) {
  #define RA iregs[ar]
  #define RB iregs[RSM_GET_Br(in)]
  #define RC iregs[RSM_GET_Cr(in)]
  #define RD iregs[RSM_GET_Dr(in)]

  isize pc = 0; // program counter; current instruction#

  for (;;) {
    assertf(pc < (isize)incount, "pc=%ld incount=%u", pc, incount);
    logstate(iregs, inv, pc);

    rinstr in = inv[pc++];  // current instruction
    u8 ar = RSM_GET_Ar(in); // argument Ar, which almost every instruction needs
    switch ((enum rop)RSM_GET_OP(in)) {

    case rop_MOVE:  { RA = RB; break; }
    case rop_LOADI: { RA = RSM_GET_Bu(in); break; }
    case rop_LOADK: { dlog("TODO"); break; }

    case rop_ADD:   { RA = RB + RC; break; }
    case rop_SUB:   { RA = RB - RC; break; }
    case rop_SUBI:  { RA = RB - (u64)RSM_GET_Cu(in); break; }
    case rop_MUL:   { RA = RB * RC; break; }
    case rop_DIV:   { RA = RB / RC; break; }
    case rop_MOD:   { RA = RB % RC; break; }
    case rop_AND:   { RA = RB & RC; break; }
    case rop_OR:    { RA = RB | RC; break; }
    case rop_XOR:   { RA = RB ^ RC; break; }
    case rop_SHL:   { RA = RB << RC; break; }
    case rop_SHRS:  { RA = RB >> RC; break; }
    case rop_SHRU:  { RA = RB >> RC; break; }

    case rop_CMPEQ: { RA = RB == RC; break; }
    case rop_CMPLT: { RA = RB < RC; break; }
    case rop_CMPGT: { RA = RB > RC; break; }

    case rop_BRZ:   { if (RA == 0) pc = RB; break; }
    case rop_BRNZ:  { if (RA != 0) pc = RB; break; }
    case rop_BRZI:  { if (RA == 0) pc += RSM_GET_Bs(in); break; }
    case rop_BRNZI: { if (RA != 0) pc += RSM_GET_Bs(in); break; }

    case rop_RET:   { return; }

    case RSM_OP_COUNT: assert(0);
  }}
  #undef RA
  #undef RB
  #undef RC
  #undef RD
}
