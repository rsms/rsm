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
static usize vm_memsize = 1024*1024;

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
  rerror err;
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
  errmsg("-%c %s: invalid value: %s", opt, arg, rerror_str(err));
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
    ,vm_memsize
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
  for (int c; (c = getopt(argc, argv, ":hrpR:o:m:")) != -1;) switch(c) {
    case 'h': usage(); exit(0);
    case 'r': opt_run = true; break;
    case 'p': opt_print_asm = true; break;
    case 'R': nerrs += setreg(iregs, optarg); break;
    case 'o': outfile = optarg; break;
    case 'm': nerrs += parse_bytesize_opt(optopt, optarg, &vm_memsize); break;
    case ':': errmsg("option -%c requires a value", optopt); nerrs++; break;
    case '?': errmsg("unrecognized option -%c", optopt); nerrs++; break;
  }
  if (nerrs) exit(1);
  if (!outfile && !opt_print_asm) opt_run = true;
  return optind;
}

static bool loadfile(rmem mem, const char* nullable infile, void** p, usize* size) {
  if (infile) {
    rerror err = rsm_loadfile(infile, p, size);
    if (err) {
      fprintf(stderr, "%s: %s\n", infile, rerror_str(err));
      return false;
    }
    return true;
  }
  usize limit = 1024*1024; // 1MB
  rerror err = read_stdin_data(mem, limit, p, size);
  if (err == 0)
    return true;
  if (err == rerr_badfd) { // stdin is a tty
    errmsg("missing <infile> (see %s -h for help)", prog);
  } else {
    errmsg("error reading stdin: %s", rerror_str(err));
  }
  return false;
}

static void print_asm(rmem mem, const rinstr* iv, usize icount) {
  char* buf = rmem_alloc(mem, 512); if (!buf) panic("out of memory");
  rfmtflag fl = isatty(1) ? RSM_FMT_COLOR : 0;
  usize n = rsm_fmtprog(buf, 512, iv, icount, fl);
  if (n >= 512) {
    buf = rmem_resize(mem, buf, 512, n + 1); if (!buf) panic("out of memory");
    rsm_fmtprog(buf, n, iv, icount, fl);
  }
  fwrite(buf, n, 1, stdout); putc('\n', stdout);
}

static void print_regstate(u64* iregs) {
  printf("register state:\n");
  for(u32 i=0;i<4;i++)
    printf(i==3 ? " R%u" : i ? " R%-19u" : "                      R%-19u", i);
  printf("\nU64 "); for(u32 i=0;i<4;i++) printf(i ? " %20llx" : "%20llx", iregs[i]);
  printf("\nU64 "); for(u32 i=0;i<4;i++) printf(i ? " %20llu" : "%20llu", iregs[i]);
  printf("\nS64 "); for(u32 i=0;i<4;i++) printf(i ? " %20lld" : "%20lld", (i64)iregs[i]);
  printf("\nS32 "); for(u32 i=0;i<4;i++) printf(i ? " %20d"   : "%20d",   (i32)iregs[i]);
  printf("\nS16 "); for(u32 i=0;i<4;i++) printf(i ? " %20d"   : "%20d",   (i16)iregs[i]);
  printf("\nS8  "); for(u32 i=0;i<4;i++) printf(i ? " %20d"   : "%20d",   (i8)iregs[i]);
  printf("\n\n");
  for(u32 i=4;i<8;i++)
    printf(i==7 ? " R%u" : i>4 ? " R%-19u" : "                      R%-19u", i);
  printf("\nU64 "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20llx" : "%20llx", iregs[i]);
  printf("\nU64 "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20llu" : "%20llu", iregs[i]);
  printf("\nS64 "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20lld" : "%20lld", (i64)iregs[i]);
  printf("\nS32 "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20d"   : "%20d",   (i32)iregs[i]);
  printf("\nS16 "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20d"   : "%20d",   (i16)iregs[i]);
  printf("\nS8  "); for(u32 i=4;i<8;i++) printf(i>4 ? " %20d"   : "%20d",   (i8)iregs[i]);
  printf("\n");
}

static bool diaghandler(const rdiag* d, void* userdata) {
  // called by the compiler when an error occurs
  fwrite(d->msg, strlen(d->msg), 1, stderr); putc('\n', stderr);
  if (d->srclines[0]) {
    fwrite(d->srclines, strlen(d->srclines), 1, stderr); putc('\n', stderr);
  }
  return true; // keep going (show all errors)
}

static bool compile(
  rmem mem,
  const char* nullable srcfile, void* srcdata, usize srclen,
  rrom* rom)
{
  rasm a = {
    .mem = mem,
    .diaghandler = diaghandler,
    .srcname = srcfile ? srcfile : "stdin",
    .srcdata = srcdata,
    .srclen = srclen,
  };

  rnode* mod = rasm_parse(&a);
  if (a.errcount) // note: errors have been reported by diaghandler
    return false;

  rerror err = rasm_gen(&a, mod, mem, rom);
  if (err) {
    errmsg("(rasm_gen) %s", rerror_str(err));
    return false;
  }

  if (outfile) {
    dlog("writing ROM to %s (%zu B)", outfile, rom->imgsize);
    rerror err = writefile(outfile, 0777, rom->img, rom->imgsize);
    if (err) {
      errmsg("%s: %s", outfile, rerror_str(err));
      return false;
    }
  }

  if (opt_print_asm)
    print_asm(mem, rom->code, rom->codelen);
  return true;
}

int main(int argc, char*const* argv) {
  if (!rsm_init()) return 1;

  // vm execution state (can be initialized with e.g. -R3=0xff)
  u64 iregs[RSM_NREGS] = {0};

  // parse command-line options
  int argi = parse_cli_opts(argc, argv, iregs);
  const char* infile = NULL;
  if (argi < argc) { // <srcfile> [R0 [R1 ...]]
    infile = strcmp(argv[argi],"-")==0 ? NULL : argv[argi];
    if (argi < argc-1) errmsg("ignoring %d extra command-line arguments", argc-1 - argi);
  }

  // memory allocator for compiler and for buffering stdin
  rmem mem = rmem_mkvmalloc(0);
  if (mem.state == NULL) {
    errmsg("failed to allocate virtual memory");
    return 1;
  }

  // load input file or read stdin
  void* indata; usize insize;
  if (!loadfile(mem, infile, &indata, &insize))
    return 1;

  // input: load ROM or compile source
  rrom rom = {0};
  if (insize > 4 && *(u32*)indata == RSM_ROM_MAGIC) {
    rom.img = indata;
    rom.imgsize = insize;
  } else if (!compile(mem, infile, indata, insize, &rom)) {
    return 1;
  }

  if (!opt_run)
    return 0;

  // allocate memory and execute program
  void* membase = vmem_alloc(vm_memsize);
  if UNLIKELY(membase == NULL) {
    errmsg("failed to allocate %zu B of memory", vm_memsize);
    return 1;
  }

  u64 time = nanotime();
  rerror err = rsm_vmexec(&rom, iregs, membase, vm_memsize);
  time = nanotime() - time;
  if (err) {
    errmsg("vmexec: %s", err == rerr_nomem ? "not enough memory" : rerror_str(err));
    return 1;
  }

  char duration[25];
  fmtduration(duration, time);
  log("execution finished in %s", duration);
  print_regstate(iregs);
  return 0;
}

#endif // __wasm__
