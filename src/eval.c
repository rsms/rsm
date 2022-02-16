#include "rsm.h"

void rsm_eval(u64* iregs, u32* inv, u32 inc) {
  int    pc = 0; // program counter; current offset into inv
  rinstr in;  // current instruction
  u8     ar;      // argument Ar, which almost every instruction needs

  for (;;) {
    in = inv[pc++];
    ar = RSM_GET_Ar(in);
    switch (RSM_GET_OP(in)) {

    case rop_MOVE: { // R(A) = R(B)
      dlog("MOVE R%u = R%u (%llx)", ar, RSM_GET_Br(in), iregs[RSM_GET_Br(in)]);
      iregs[ar] = iregs[RSM_GET_Br(in)];
      break;
    }
    case rop_LOADI: {
      dlog("LOADI R%u = %u", ar, RSM_GET_Bu(in));
      iregs[ar] = (u64)RSM_GET_Bu(in);
      break;
    }
    case rop_LOADK: { iregs[ar]=rop_LOADK; dlog("LOADK"); break; }
    case rop_CMPEQ: { iregs[ar]=rop_CMPEQ; dlog("CMPEQ"); break; }
    case rop_BRZ:   { iregs[ar]=rop_BRZ;   dlog("BRZ");   break; }
    case rop_BRZI:  { iregs[ar]=rop_BRZI;  dlog("BRZI");  break; }
    case rop_BRNZ:  { iregs[ar]=rop_BRNZ;  dlog("BRNZ");  break; }
    case rop_BRNZI: { iregs[ar]=rop_BRNZI; dlog("BRNZI"); break; }
    case rop_ADD:   { iregs[ar]=rop_ADD;   dlog("ADD");   break; }
    case rop_SUBI:  { iregs[ar]=rop_SUBI;  dlog("SUBI");  break; }
    case rop_MUL:   { iregs[ar]=rop_MUL;   dlog("MUL");   break; }
    case rop_RET:   { iregs[ar]=rop_RET;   dlog("RET");   return; }
  }}
}
