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
  isize  pc = 0; // program counter; current offset into inv
  rinstr in;     // current instruction
  u8     ar;     // argument Ar, which almost every instruction needs

  #define RA iregs[ar]
  #define RB iregs[RSM_GET_Br(in)]
  #define RC iregs[RSM_GET_Cr(in)]
  #define RD iregs[RSM_GET_Dr(in)]

  for (;;) {
    assertf(pc < (isize)incount, "pc=%ld incount=%u", pc, incount);
    logstate(iregs, inv, pc);

    in = inv[pc++];
    ar = RSM_GET_Ar(in);
    switch ((enum rop)RSM_GET_OP(in)) {

    case rop_MOVE:  { RA = RB; break; }
    case rop_LOADI: { RA = RSM_GET_Bu(in); break; }
    case rop_LOADK: { dlog("TODO"); break; }
    case rop_BRZ:   { if (RA == 0) pc = RB; break; }
    case rop_BRNZ:  { if (RA != 0) pc = RB; break; }
    case rop_BRZI:  { if (RA == 0) pc += RSM_GET_Bs(in); break; }
    case rop_BRNZI: { if (RA != 0) pc += RSM_GET_Bs(in); break; }
    case rop_CMPEQ: { RA = RB == RC; break; }
    case rop_ADD:   { RA = RB + RC; break; }
    case rop_SUBI:  { RA = RB - (u64)RSM_GET_Cu(in); break; }
    case rop_MUL:   { RA = RB * RC; break; }
    case rop_RET:   { return; }

    case RSM_OP_COUNT: assert(0);
  }}
  #undef RA
  #undef RB
  #undef RC
  #undef RD
}
