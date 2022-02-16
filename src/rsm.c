#include "rsm.h"
#include "rinstr.h"

void logbin(u32 v);


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

static void eval(u64* iregs, u32* inv, u32 inc) {
  static const evalfun dtable[] = {
    #define _(name, ...) &eval_##name,
    DEF_RSM_OPS(_)
    #undef _
  };
  return eval_next(dtable, iregs, inv, 0, 0);
}


int main(int argc, const char** argv) {
  rmem* m = rmem_makevm(4096*1000);

  rinstr* ip = rmem_allocz(m, sizeof(rinstr)*32);
  u32 pc = 0;
  // fun factorial (i32) i32
  //   b0:              //
  //     r8 = r0        // ACC = n (n is in r0, argument 0)
  //     r0 = 1         // RES (return value 0)
  //     brz r8 end     // if n==0 goto end
  //   b1:              // <- [b0] b1  ("[b]=implicit/fallthrough")
  //     r0 = mul r8 r0 // RES = ACC * RES
  //     r8 = sub r8 1  // ACC = ACC - 1
  //     brnz r8  b1    // if n!=0 goto b1
  //   end:             // <- b0 [b1]
  //     ret            // RES is at r0
  // u32 b0 = pc; // b0:
  ip[pc++] = RSM_MAKE_AB(rop_MOVE, 8, 0); // r8 = r0
  ip[pc++] = RSM_MAKE_AB(rop_LOADI, 0, 1); // r0 = 1
  ip[pc++] = RSM_MAKE_ABs(rop_BRZI, 8, 3); // brz r8 end -- PC+3=end (TODO patch marker)
  u32 b1 = pc; // b1:
  ip[pc++] = RSM_MAKE_ABC(rop_MUL, 0, 0, 8); // r0 = mul r8 r0
  ip[pc++] = RSM_MAKE_ABC(rop_SUBI, 8, 0, 1); // r8 = sub r8 1
  ip[pc]   = RSM_MAKE_ABs(rop_BRNZI, 8, -(pc+1-b1)); // brnz r8 b1 (TODO sign)
  pc++;
  // u32 end = pc; // end:
  ip[pc++] = RSM_MAKE_A(rop_RET, 0); // ret

  ip = rmem_resize(m, ip, pc*sizeof(rinstr));
  dlog("function size: %lu B", pc*sizeof(rinstr));

  // logbin(RSM_GET_Bs(RSM_MAKE_ABs(rop_BRNZI, 8, -3)));

  char buf[512];
  fmtprog(buf, sizeof(buf), ip, pc);
  log("%s", buf);

  // call factorial(3)
  u64 iregs[32];
  iregs[0] = 3; // arg 1
  eval(iregs, ip, pc);

  return 0;
}
