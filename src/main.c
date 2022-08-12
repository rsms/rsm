// rsm CLI program
// SPDX-License-Identifier: Apache-2.0
#if !defined(__wasm__) || defined(__wasi__)
#include "rsmimpl.h"
#ifndef RSM_NO_LIBC
  #include <stdio.h>
  #include <stdlib.h>
  #include <unistd.h>
#endif

static const char* prog = ""; // argv[0]
static const char* outfile = NULL;
static bool opt_run = false;
static bool opt_print_asm = false;
static bool opt_print_debug = false;
static bool opt_newexec = false;
static usize vm_ramsize = 1024*1024;

#define errmsg(fmt, args...) fprintf(stderr, "%s: " fmt "\n", prog, ##args)

static int setreg(u64* iregs, const char* s) {
  usize p = 0, len = strlen(s);
  while (p < len && s[p++] != '=') {}
  if (p == len)
    goto malformed;
  const char* vals = s + p;
  usize vallen = len - p;
  int base = 10;
  u64 regno = 0;
  if (vallen > 2 && vals[0] == '0' && (vals[1] == 'x' || vals[1] == 'X')) {
    base = 16;
    vals += 2;
    vallen -= 2;
  }
  if (parseu64(s, p-1, 10, &regno, 0xff) != 0)
    goto malformed;
  if (regno > RSM_MAX_REG) {
    errmsg("invalid register -R%llu", regno);
    return 1;
  }
  if (parseu64(vals, vallen, base, &iregs[regno], 0xffffffffffffffff) == 0)
    return 0;
malformed:
  errmsg("malformed option -R%s (expected e.g. -R3=123 or -R3=0xfAcE)", s);
  return 1;
}

static int parse_bytesize_opt(char opt, const char* arg, usize* resultp) {
  u64 result;
  usize len = strlen(arg);
  rerr_t err;
  if (len == 0) {
    err = rerr_invalid;
    goto error;
  }
  // parse suffix -- G[iga], M[ega], k[ilo]
  u64 multiplier = 1;
  switch (arg[len-1]) {
    case 'G': case 'g': len--; multiplier = 1024*1024*1024; break;
    case 'M': case 'm': len--; multiplier = 1024*1024; break;
    case 'k': case 'K': len--; multiplier = 1024; break;
  }
  err = parseu64(arg, len, 10, &result, USIZE_MAX);
  if (err) goto error;
  if (result == 0) { err = rerr_invalid; goto error; }

  if (check_mul_overflow(result, multiplier, &result)) {
    err = rerr_overflow;
    goto error;
  }

  *resultp = (usize)result;
  return 0;
error:
  errmsg("-%c %s: invalid value: %s", opt, arg, rerr_str(err));
  return 1;
}

static void usage() {
  printf(
    "RSM virtual machine <https://rsms.me/rsm/>\n"
    "Usage: %s [options] [<infile>]\n"
    "Options:\n"
    "  -h           Show help and exit\n"
    "  -r           Run the program (implied unless -o or -p are set)\n"
    "  -p           Print assembly on stdout\n"
    "  -d           Print timing and register state on stdout at end\n"
    "  -X           Run new experimental execution engine\n"
    "  -R<N>=<val>  Initialize register R<N> to <val> (e.g. -R0=4, -R3=0xff)\n"
    "  -m <nbytes>  Set VM memory to <nbytes> (default: %zu)\n"
    "  -o <file>    Write compiled ROM to <file>\n"
    "<infile>\n"
    "  Either a ROM image or an assembly source file.\n"
    "  If \"-\" or not given, read from stdin (unless stdin is a TTY.)\n"
    "Examples:\n"
    "  %s -R1=6 examples/factorial.rsm     # compile & run\n"
    "  %s -o compiled.rom source.rsm       # compile & save\n"
    "  %s compiled.rom                     # run precompiled ROM\n"
    "  echo 'fun main() { R0=123; }' | %s  # compile & run stdin\n"
    ,prog
    ,vm_ramsize
    ,prog
    ,prog
    ,prog
    ,prog
  );
}

static int parse_cli_opts(int argc, char*const* argv, u64* iregs) {
  prog = argv[0];
  extern char* optarg; // global state in libc... coolcoolcool
  extern int optind, optopt;
  int nerrs = 0;
  for (int c; (c = getopt(argc, argv, ":hrpdXR:o:m:")) != -1;) switch(c) {
    case 'h': usage(); exit(0);
    case 'r': opt_run = true; break;
    case 'p': opt_print_asm = true; break;
    case 'd': opt_print_debug = true; break;
    case 'X': opt_newexec = true; break;
    case 'R': nerrs += setreg(iregs, optarg); break;
    case 'o': outfile = optarg; break;
    case 'm': nerrs += parse_bytesize_opt(optopt, optarg, &vm_ramsize); break;
    case ':': errmsg("option -%c requires a value", optopt); nerrs++; break;
    case '?': errmsg("unrecognized option -%c", optopt); nerrs++; break;
  }
  if (nerrs) exit(1);
  if (!outfile && !opt_print_asm) opt_run = true;
  return optind;
}

static bool loadfile(
  rmemalloc_t* ma, const char* nullable infile, rmem_t* data_out)
  // rmemalloc_t* ma, const char* nullable infile, void** p, usize* size)
{
  if (infile) {
    rerr_t err = rsm_loadfile(infile, data_out);
    if (err) {
      fprintf(stderr, "%s: %s\n", infile, rerr_str(err));
      return false;
    }
    return true;
  }
  usize limit = 1024 * MiB;
  rerr_t err = read_stdin_data(ma, limit, data_out);
  if (err == 0)
    return true;
  if (err == rerr_badfd) { // stdin is a tty
    errmsg("missing <infile> (see %s -h for help)", prog);
  } else {
    errmsg("error reading stdin: %s", rerr_str(err));
  }
  return false;
}

static void print_asm(rmemalloc_t* ma, const rin_t* iv, usize icount) {
  rmem_t m = rmem_must_alloc(ma, 4096);
  rfmtflag_t fl = isatty(1) ? RSM_FMT_COLOR : 0;
  usize n = rsm_fmtprog(m.p, m.size, iv, icount, fl);
  if (n >= m.size) {
    rmem_must_resize(ma, &m, n + 1);
    rsm_fmtprog(m.p, m.size, iv, icount, fl);
  }
  fwrite(m.p, n, 1, stdout); putc('\n', stdout);
  rmem_free(ma, m);
}

static void print_regstate_x64(bool pad, u64 v) {
  int w = ( // (OMG I'm ashamed of this "shitty log16" code...)
    v > 0xfffffffffffffff ? 16 : v > 0xffffffffffffff ? 15 :
    v > 0xfffffffffffff ? 14 :   v > 0xffffffffffff ? 13 :
    v > 0xfffffffffff ? 12 :     v > 0xffffffffff ? 11 :
    v > 0xfffffffff ? 10 :       v > 0xffffffff ? 9 :
    v > 0xfffffff ? 8 :          v > 0xffffff ? 7 :
    v > 0xfffff ? 6 :            v > 0xffff ? 5 :
    v > 0xfff ? 4 :              v > 0xff ? 3 :
    v > 0xf ? 2 :                         1
  );
  printf(pad ? " %*s0x%llx" : "%*s0x%llx", 18 - w, "", v);
}

static void print_regstate(u64* iregs) {
  printf("register state:\n");
  for(u32 i=0;i<4;i++)
    printf(i==3 ? " R%u" : i ? " R%-19u" : "                      R%-19u", i);
  printf("\nU64 "); for (u32 i=0;i<4;i++) print_regstate_x64(i, iregs[i]);
  printf("\nU64 "); for(u32 i=0;i<4;i++) printf(i ? " %20llu" : "%20llu", iregs[i]);
  printf("\nS64 "); for(u32 i=0;i<4;i++) printf(i ? " %20lld" : "%20lld", (i64)iregs[i]);
  printf("\nS32 "); for(u32 i=0;i<4;i++) printf(i ? " %20d"   : "%20d",   (i32)iregs[i]);
  printf("\nS16 "); for(u32 i=0;i<4;i++) printf(i ? " %20d"   : "%20d",   (i16)iregs[i]);
  printf("\nS8  "); for(u32 i=0;i<4;i++) printf(i ? " %20d"   : "%20d",   (i8)iregs[i]);
  printf("\n\n");
  for(u32 i=4;i<8;i++)
    printf(i==7 ? " R%u" : i>4 ? " R%-19u" : "                      R%-19u", i);
  printf("\nU64 "); for(u32 i=4;i<8;i++) print_regstate_x64(i>4, iregs[i]);
  printf("\nU64 "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20llu" : "%20llu", iregs[i]);
  printf("\nS64 "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20lld" : "%20lld", (i64)iregs[i]);
  printf("\nS32 "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20d"   : "%20d",   (i32)iregs[i]);
  printf("\nS16 "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20d"   : "%20d",   (i16)iregs[i]);
  printf("\nS8  "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20d"   : "%20d",   (i8)iregs[i]);
  printf("\n");
}

static bool diaghandler(const rdiag_t* d, void* userdata) {
  // called by the compiler when an error occurs
  fwrite(d->msg, strlen(d->msg), 1, stderr);
  putc('\n', stderr);
  if (d->code > 0 && d->srclines[0]) {
    fwrite(d->srclines, strlen(d->srclines), 1, stderr);
    putc('\n', stderr);
  }
  return true; // keep going (show all errors)
}

static bool compile(
  rmemalloc_t* ma, const char* nullable srcfile, rmem_t srcdata, rrom_t* rom)
{
  rasm_t a = {
    .memalloc = ma,
    .diaghandler = diaghandler,
    .srcname = srcfile ? srcfile : "stdin",
    .srcdata = srcdata.p,
    .srclen = srcdata.size,
  };

  rnode_t* mod = rasm_parse(&a);
  if UNLIKELY(mod == NULL) {
    errmsg("failed to allocate memory for parser");
    return false;
  }

  if (a.errcount) // note: errors have been reported by diaghandler
    return false;

  rerr_t err = rasm_gen(&a, mod, ma, rom);
  if (err) {
    errmsg("(rasm_gen) %s", rerr_str(err));
    return false;
  }

  if (outfile) {
    dlog("writing ROM to %s (%zu B)", outfile, rom->imgsize);
    rerr_t err = writefile(outfile, 0777, rom->img, rom->imgsize);
    if (err) {
      errmsg("%s: %s", outfile, rerr_str(err));
      return false;
    }
  }

  if (opt_print_asm)
    print_asm(ma, rom->code, rom->codelen);
  return true;
}

int main(int argc, char*const* argv) {
  if (!rsm_init()) return 1;

  // vm execution state (can be initialized with e.g. -R3=0xff)
  rvm_t vm = {0};

  // parse command-line options
  int argi = parse_cli_opts(argc, argv, vm.iregs);
  const char* infile = NULL;
  if (argi < argc) { // <srcfile> [R0 [R1 ...]]
    infile = strcmp(argv[argi],"-")==0 ? NULL : argv[argi];
    if (argi < argc-1)
      errmsg("ignoring %d extra command-line arguments", argc-1 - argi);
  }

  // create a memory manager and a memory allocator
  usize total_mem_size = 1 * GiB; // total memory from host
  usize memalloc_size = 64 * MiB; // memory to use for allocator
  // rest of memory is used for runtime virtual memory
  rmm_t* mm = assertnotnull( rmm_create_host_vmmap(total_mem_size) );
  rmemalloc_t* ma = rmem_allocator_create(mm, memalloc_size);
  if (!ma) {
    errmsg("failed to create memory allocator");
    return 1;
  }

  // load input file or read stdin
  rmem_t indata;
  if (!loadfile(ma, infile, &indata))
    return 1;

  // input: load ROM or compile source
  rrom_t rom = {0};
  if (indata.size > 4 && *(u32*)indata.p == RSM_ROM_MAGIC) {
    rom.img = indata.p;
    rom.imgsize = indata.size;
  } else if (!compile(ma, infile, indata, &rom)) {
    return 1;
  }

  if (!opt_run)
    return 0;

  u64 time = nanotime();

  // —————————————— new execution engine ——————————————
  if (opt_newexec) {

    rmachine_t* machine = safechecknotnull( rmachine_create(mm) );
    rerr_t err2 = rmachine_execrom(machine, &rom);
    if (err2) {
      errmsg("rmachine_execrom: %s", rerr_str(err2));
      return 1;
    }
    rmachine_dispose(machine);

  // —————————————— old execution engine ——————————————
  } else {
    // allocate memory and execute program
    vm.ramsize = vm_ramsize;
    vm.rambase = osvmem_alloc(vm_ramsize);
    if UNLIKELY(vm.rambase == NULL) {
      errmsg("failed to allocate %zu B of memory", vm_ramsize);
      return 1;
    }
    rerr_t err = rsm_vmexec(&vm, &rom);
    if (err) {
      errmsg("vmexec: %s", err == rerr_nomem ? "not enough memory" : rerr_str(err));
      return 1;
    }
  }// ——————————————

  if (opt_print_debug) {
    char duration[25];
    fmtduration(duration, nanotime() - time);
    log("Execution finished in %s", duration);
    print_regstate(vm.iregs);
  }

  return 0;
}

#endif // __wasm__
