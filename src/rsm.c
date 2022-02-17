#include "rsm.h"

void logbin(u32 v);

// TODO:
// - build an evaluator and try it with the factorial function
// - think about how a "program" is represented (functions + constants)

// reference implementation of factorial
u64 factorial(u64 n) {
  if (n == 0)
    return 1;
  else
    return n * factorial(n-1);
}

int main(int argc, const char** argv) {
  rmem* m = rmem_makevm(4096*1000);

  rinstr* ip = rmem_allocz(m, sizeof(rinstr)*32);
  u32 pc = 0;
  // fun factorial (i32) i32
  //   b0:              //
  //     r1 = r0        // ACC = n (argument 0)
  //     r0 = 1         // RES (return value 0)
  //     brz r1 end     // if n==0 goto end
  //   b1:              // <- [b0] b1
  //     r0 = mul r1 r0 // RES = ACC * RES
  //     r1 = sub r1 1  // ACC = ACC - 1
  //     brnz r1  b1    // if n!=0 goto b1
  //   end:             // <- b0 [b1]
  //     ret            // RES is at r0
  // u32 b0 = pc; // b0:
  ip[pc++] = RSM_MAKE_AB(rop_MOVE, 1, 0); // r1 = r0
  ip[pc++] = RSM_MAKE_AB(rop_LOADI, 0, 1); // r0 = 1
  ip[pc++] = RSM_MAKE_ABs(rop_BRZI, 1, 3); // brz r1 end -- PC+3=end (TODO patch marker)
  u32 b1 = pc; // b1:
  ip[pc++] = RSM_MAKE_ABC(rop_MUL, 0, 1, 0); // r0 = mul r1 r0
  ip[pc++] = RSM_MAKE_ABC(rop_SUBI, 1, 1, 1); // r1 = sub r1 1
  ip[pc]   = RSM_MAKE_ABs(rop_BRNZI, 1, -(pc+1-b1)); // brnz r1 b1 (TODO sign)
  pc++;
  // u32 end = pc; // end:
  ip[pc++] = RSM_MAKE_A(rop_RET, 0); // ret

  // shrinkwrap memory allocation
  ip = rmem_resize(m, ip, pc*sizeof(rinstr));

  // print function
  dlog("function size: %lu B", pc*sizeof(rinstr));
  char buf[512];
  rsm_fmtprog(buf, sizeof(buf), ip, pc);
  log("%s", buf);

  // eval
  u64 iregs[32];
  iregs[0] = 3; // arg 1
  dlog("reference factorial(%llu) => %llu", iregs[0], factorial((i64)iregs[0]));
  dlog("evaluating vm factorial(%lld)", (i64)iregs[0]);
  rsm_eval(iregs, ip, pc);
  dlog("result: %llu", iregs[0]);

  return 0;
}
