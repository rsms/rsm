// virtual machine
// SPDX-License-Identifier: Apache-2.0
#include "rsmimpl.h"

typedef struct vmstate vmstate;
struct vmstate {
  rinstr* instart; // start of instruction array
  rinstr* inend;   // end of instruction array
};

#if defined(DEBUG) && !defined(RSM_NO_LIBC)
  #include <stdio.h>
  static void logstate_header() {
    fprintf(stderr, "\e[2m");
    for (int i = 0; i < 6; i++)
      fprintf(stderr, "\e[9%cm  R%d\e[39m", '1'+(i%6), i);
    fprintf(stderr, "  │  PC  INSTRUCTION\e[22m\n");
  }
  static void logstate(vmstate* vs, u64* iregs, rinstr* inp) {
    for (int i = 0; i < 6; i++)
      fprintf(stderr, "\e[9%cm%4llx\e[39m", '1'+(i%6), iregs[i]);
    char buf[128];
    rsm_fmtinstr(buf, sizeof(buf), *inp);
    fprintf(stderr, "  │ %3ld  %s\n", (usize)(inp - vs->instart), buf);
  }
#else
  #define logstate_header(...) ((void)0)
  #define logstate(...) ((void)0)
#endif

#define RA  iregs[ar]
#define RB  iregs[RSM_GET_B(in)]
#define RC  iregs[RSM_GET_C(in)]
#define RD  iregs[RSM_GET_D(in)]

#define RAu ( RSM_GET_Ai(in) ? (u64)RSM_GET_Au(in) : iregs[RSM_GET_Au(in)] )
#define RBu ( RSM_GET_Bi(in) ? (u64)RSM_GET_Bu(in) : iregs[RSM_GET_Bu(in)] )
#define RCu ( RSM_GET_Ci(in) ? (u64)RSM_GET_Cu(in) : iregs[RSM_GET_Cu(in)] )
#define RDu ( RSM_GET_Di(in) ? (u64)RSM_GET_Du(in) : iregs[RSM_GET_Du(in)] )

#define RAs ((i64)( RSM_GET_Ai(in) ? RSM_GET_As(in) : iregs[RSM_GET_Au(in)] ))
#define RBs ((i64)( RSM_GET_Bi(in) ? RSM_GET_Bs(in) : iregs[RSM_GET_Bu(in)] ))
#define RCs ((i64)( RSM_GET_Ci(in) ? RSM_GET_Cs(in) : iregs[RSM_GET_Cu(in)] ))
#define RDs ((i64)( RSM_GET_Di(in) ? RSM_GET_Ds(in) : iregs[RSM_GET_Du(in)] ))


static void scall(vmstate* vs, u64* iregs, u8 ar, rinstr in) {
  // dlog("scall #%llu arg1 0x%llx", RA, RBs);
  iregs[0] = 0; // result = no error
  switch (RA) {
    case 1: putc(RBs, stdout); break;
    default: iregs[0] = 1; // error; invalid scall
  }
}

static void vmexec(vmstate* vs, u64* iregs, rinstr* inp) {
  for (;;) {
    assertf(inp >= vs->instart, "pc underrun: inp %p < instart %p", inp, vs->instart);
    assertf(inp < vs->inend, "pc overrun: inp %p >= inend %p", inp, vs->inend);
    logstate(vs, iregs, inp);

    rinstr in = *inp++; // load current instruction and advance pc
    u8 ar = RSM_GET_A(in); // argument Ar, which almost every instruction needs
    switch ((enum rop)RSM_GET_OP(in)) {

    case rop_COPY:  RA = RBu; break;
    case rop_LOADK: dlog("TODO"); break;

    case rop_ADD:   RA = RB + RCu; break;
    case rop_SUB:   RA = RB - RCu; break;
    case rop_MUL:   RA = RB * RCu; break;
    case rop_DIV:   RA = RB / RCu; break;
    case rop_MOD:   RA = RB % RCu; break;
    case rop_AND:   RA = RB & RCu; break;
    case rop_OR:    RA = RB | RCu; break;
    case rop_XOR:   RA = RB ^ RCu; break;
    case rop_SHL:   RA = RB << RCu; break;
    case rop_SHRS:  RA = (u64)((i64)RB >> RCs); break;
    case rop_SHRU:  RA = RB >> RCu; break;

    case rop_CMPEQ: RA = RB == RCu; break;
    case rop_CMPLT: RA = RB < RCu; break;
    case rop_CMPGT: RA = RB > RCu; break;

    case rop_BRZ:  if (RA == 0) inp += (intptr)RBs; break;
    case rop_BRNZ: if (RA != 0) inp += (intptr)RBs; break;

    case rop_CALL:  inp = vs->instart + (uintptr)RAu; break;
    case rop_SCALL: scall(vs, iregs, ar, in); break;

    case rop_RET: return;

    case RSM_OP_COUNT: assert(0);
  }}
}

void rsm_vmexec(u64* iregs, rinstr* inv, usize inlen) {
  vmstate vs = { .instart=inv, .inend=inv+inlen };
  logstate_header();
  return vmexec(&vs, iregs, inv);
}
