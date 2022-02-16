#include "rsm.h"

#define PARAMS     const void* dtablep, u64* iregs, rinstr* inp, rinstr in, i64 pc
#define ARGS       dtablep, iregs, inp, in, pc
#define EVAL_NEXT  MUSTTAIL return eval_next(ARGS)
typedef void (*evalfun)(PARAMS);

static void eval_next(PARAMS) {
  in = inp[pc++];
  u8 op = RSM_GET_OP(in);
  MUSTTAIL return ((evalfun*)dtablep)[op](ARGS);
}

static void eval_MOVE(PARAMS) { // R(A) = R(B)
  dlog("MOVE R%u = R%u (%llx)", RSM_GET_Ar(in), RSM_GET_Br(in), iregs[RSM_GET_Br(in)]);
  iregs[RSM_GET_Ar(in)] = iregs[RSM_GET_Br(in)];
  EVAL_NEXT;
}
static void eval_LOADI(PARAMS) {
  dlog("LOADI R%u = %u", RSM_GET_Ar(in), RSM_GET_Bu(in));
  iregs[RSM_GET_Ar(in)] = (u64)RSM_GET_Bu(in);
  EVAL_NEXT;
}
static void eval_LOADK(PARAMS) { dlog("LOADK"); EVAL_NEXT; }
static void eval_CMPEQ(PARAMS) { dlog("CMPEQ"); EVAL_NEXT; }
static void eval_BRZ(PARAMS) { dlog("BRZ"); EVAL_NEXT; }
static void eval_BRZI(PARAMS) { dlog("BRZI"); EVAL_NEXT; }
static void eval_BRNZ(PARAMS) { dlog("BRNZ"); EVAL_NEXT; }
static void eval_BRNZI(PARAMS) { dlog("BRNZI"); EVAL_NEXT; }
static void eval_ADD(PARAMS) { dlog("ADD"); EVAL_NEXT; }
static void eval_SUBI(PARAMS) { dlog("SUBI"); EVAL_NEXT; }
static void eval_MUL(PARAMS) { dlog("MUL"); EVAL_NEXT; }
static void eval_RET(PARAMS) { dlog("RET"); }

void rsm_eval(u64* iregs, u32* inv, u32 inc) {
  static const evalfun dtable[] = {
    #define _(name, ...) &eval_##name,
    DEF_RSM_OPS(_)
    #undef _
  };
  return eval_next(dtable, iregs, inv, 0, 0);
}
