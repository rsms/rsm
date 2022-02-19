#include "rsm.h"

// TODO:
// - assembler
//   - source scanner & parser
//   - ir representation (graph)
//   - register allocation to support "locals"
//   - codegen
// - think about how a "program" is represented (functions + constants)

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
  //
  ip[pc++] = RSM_MAKE_AB(rop_MOVE, 1, 0); // r1 = r0
  ip[pc++] = RSM_MAKE_AB(rop_LOADI, 0, 1); // r0 = 1
  ip[pc++] = RSM_MAKE_ABs(rop_BRZI, 1, 3); // brz r1 end -- PC+3=end (TODO patch marker)
  u32 b1 = pc; // b1:
  ip[pc++] = RSM_MAKE_ABC(rop_MUL, 0, 1, 0); // r0 = mul r1 r0
  ip[pc++] = RSM_MAKE_ABC(rop_SUBI, 1, 1, 1); // r1 = sub r1 1
  ip[pc]   = RSM_MAKE_ABs(rop_BRNZI, 1, -(pc+1-b1)); // brnz r1 b1 (TODO sign)
  pc++;
  // end:
  ip[pc++] = RSM_MAKE_A(rop_RET, 0); // ret

  // shrinkwrap memory allocation
  ip = rmem_resize(m, ip, pc*sizeof(rinstr));

  // print function
  dlog("function size: %lu B", pc*sizeof(rinstr));
  char buf[512];
  rsm_fmtprog(buf, sizeof(buf), ip, pc);
  log("%s", buf);

  // eval
  u64 iregs[32] = {0};
  iregs[0] = 3; // arg 1
  dlog("evaluating factorial(%lld)", (i64)iregs[0]);
  rsm_eval(iregs, ip, pc);
  dlog("result: %llu", iregs[0]);

  // assemble
  rinstr* idst = rmem_allocz(m, sizeof(rinstr)*32);
  dlog("parsing assembly source");
  usize icount = rsm_asm(m, idst, 32,
    "fun factorial (n i32, j i32) a i32 {\n"
    "    R1 = R0        // ACC = n (argument 0)\n"
    "    R0 = 1         // RES (return value 0)\n"
    "    brz R1 end     // if n==0 goto end\n"
    "  b1:              // <- [b0] b1\n"
    "    R0 = mul R1 R0 // RES = ACC * RES\n"
    "    R1 = sub R1 1  // ACC = ACC - 1\n"
    "    brnz R1  b1    // if n!=0 goto b1\n"
    "  end:             // <- b0 [b1]\n"
    "    ret            // RES is at R0\n"
    "}\n"
    // "    123 -456 0xface 0b101 F31\n"
    // //   U+1F469 woman, U+1F3FE skin tone mod 5, U+200D zwj, U+1F680 rocket = astronaut
    // "    \xF0\x9F\x91\xA9\xF0\x9F\x8F\xBE\xE2\x80\x8D\xF0\x9F\x9A\x80\n"
  );
  rsm_fmtprog(buf, sizeof(buf), idst, icount);
  log("rsm_asm =>\n%s", buf);

  return 0;
}
