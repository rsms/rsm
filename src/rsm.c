#include "rsm.h"
#include "util.h"

// TODO:
// - assembler
//   - source scanner & parser
//   - ir representation (graph)
//   - register allocation to support "locals"
//   - codegen
// - think about how a "program" is represented (functions + constants)

// diaghandler is called by the assembler when an error occurs
bool diaghandler(const rdiag* d, void* userdata) {
  log("%s", d->msg);
  return true; // keep going (show all errors)
}

int main(int argc, const char** argv) {
  rmem* m = rmem_makevm(4096*1000);
  char buf[512]; // for logging stuff
  u64 iregs[32] = {0}; // for eval

  #if 0
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
  ip[pc++] = RSM_MAKE_ABu(rop_MOVE, 0, 1); // r0 = 1
  ip[pc++] = RSM_MAKE_ABs(rop_BRZ, 1, 3); // brz r1 end -- PC+3=end (TODO patch marker)
  u32 b1 = pc; // b1:
  ip[pc++] = RSM_MAKE_ABC(rop_MUL, 0, 1, 0); // r0 = mul r1 r0
  ip[pc++] = RSM_MAKE_ABCu(rop_SUB, 1, 1, 1); // r1 = sub r1 1
  ip[pc]   = RSM_MAKE_ABs(rop_BRNZ, 1, -(pc+1-b1)); // brnz r1 b1 (TODO sign)
  pc++;
  // end:
  ip[pc++] = RSM_MAKE_A(rop_RET, 0); // ret

  // shrinkwrap memory allocation
  ip = rmem_resize(m, ip, pc*sizeof(rinstr));

  // print function
  dlog("function size: %lu B", pc*sizeof(rinstr));
  rsm_fmtprog(buf, sizeof(buf), ip, pc);
  log("%s", buf);

  // eval
  iregs[0] = 3; // arg 1
  dlog("evaluating factorial(%lld)", (i64)iregs[0]);
  rsm_eval(iregs, ip, pc);
  dlog("result: %llu", iregs[0]);
  #endif

  // assemble
  const char* src =
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
  ;
  rasmctx actx = {
    .mem         = m,
    .srcdata     = src,
    .srclen      = strlen(src),
    .srcname     = "factorial",
    .diaghandler = diaghandler,
  };
  rinstr* iv = NULL;
  usize icount = rsm_asm(&actx, &iv);
  rsm_fmtprog(buf, sizeof(buf), iv, icount);
  if (!icount)
    return 1;
  log("assembled some sweet vm code:\n%s", buf);

  // eval
  iregs[0] = 3; // arg 1
  log("evaluating function0(%lld)", (i64)iregs[0]);
  rsm_eval(iregs, iv, icount);
  log("result: %llu", iregs[0]);

  return 0;
}
