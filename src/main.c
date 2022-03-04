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
  if (regno > 31) {
    errmsg("invalid register -R%llu", regno);
    return 1;
  }
  if (parseu64(vals, vallen, base, &iregs[regno], 0xffffffffffffffff) == 0)
    return 0;
malformed:
  errmsg("malformed option -R%s (expected e.g. -R3=123 or -R3=0xfAcE)", s);
  return 1;
}

static void usage() {
  printf(
    "RSM virtual machine <https://rsms.me/rsm/>\n"
    "Usage: %s [options] [<asmfile>]\n"
    "Options:\n"
    "  -h           Show help and exit\n"
    "  -r           Run the program (implied unless -o or -p are set)\n"
    "  -p           Print assembly on stdout\n"
    "  -R<N>=<val>  Initialize register R<N> to <val> (e.g. -R0=4, -R3=0xff)\n"
    "  -o <file>    Write compiled ROM to <file>\n"
    "<asmfile>\n"
    "  If not given or if it's \"-\", read from stdin (stdin can't be a TTY.)\n"
    "Example:\n"
    "  echo 'fun main() { R0=R1*R1; }' | out/rsm -R1=18\n"
    ,prog);
}

static int parse_cli_opts(int argc, char*const* argv, u64* iregs) {
  prog = argv[0];
  extern char* optarg; // global state in libc... coolcoolcool
  extern int optind, optopt;
  int nerrs = 0;
  for (int c; (c = getopt(argc, argv, ":hrpR:o:")) != -1;) switch(c) {
    case 'h': usage(); exit(0);
    case 'r': opt_run = true; break;
    case 'p': opt_print_asm = true; break;
    case 'R': nerrs += setreg(iregs, optarg); break;
    case 'o': outfile = optarg; break;
    case ':': errmsg("option -%c requires a value", optopt); nerrs++; break;
    case '?': errmsg("unrecognized option -%c", optopt); nerrs++; break;
  }
  if (nerrs) exit(1);
  if (!outfile && !opt_print_asm) opt_run = true;
  return optind;
}

static bool load_srcfile(rasm* a, const char* nullable infile) {
  if (infile) {
    rerror err = rasm_loadfile(a, infile);
    if (err) {
      fprintf(stderr, "%s: %s\n", infile, rerror_str(err));
      return false;
    }
    return true;
  }
  a->srcname = "stdin";
  usize limit = 1024*1024; // 1MB
  rerror err = read_stdin_data(a->mem, limit, (void**)&a->srcdata, &a->srclen);
  if (err == 0)
    return true;
  if (err == rerr_badfd) { // stdin is a tty
    errmsg("missing <srcfile>");
  } else {
    errmsg("error reading stdin: %s", rerror_str(err));
  }
  return false;
}

static void print_asm(rmem mem, rinstr* iv, usize icount) {
  char* buf = rmem_alloc(mem, 512); if (!buf) panic("out of memory");
  rfmtflag fl = isatty(1) ? RSM_FMT_COLOR : 0;
  usize n = rsm_fmtprog(buf, 512, iv, icount, fl);
  if (n >= 512) {
    buf = rmem_resize(mem, buf, 512, n + 1); if (!buf) panic("out of memory");
    rsm_fmtprog(buf, n, iv, icount, fl);
  }
  fwrite(buf, n, 1, stdout); putc('\n', stdout);
}

static bool diaghandler(const rdiag* d, void* userdata) {
  // called by the compiler when an error occurs
  fwrite(d->msg, strlen(d->msg), 1, stderr); putc('\n', stderr);
  return true; // keep going (show all errors)
}

static usize compile(const char* nullable srcfile, rinstr** ivp) {
  rmem mem = rmem_mkvmalloc(0);
  if (mem.state == NULL) {
    errmsg("failed to allocate virtual memory");
    return 0;
  }
  rasm a = { .mem = mem, .diaghandler = diaghandler };
  if (!load_srcfile(&a, srcfile))
    exit(1);
  rnode* mod = rasm_parse(&a);
  if (a.errcount) // note: errors have been reported by diaghandler
    return 0;
  usize icount = rasm_gen(&a, mod, mem, ivp);
  if (a.errcount)
    return 0;
  if (icount == 0) {
    errmsg("empty program");
    return 0;
  }
  if (opt_print_asm)
    print_asm(mem, *ivp, icount);
  return icount;
}

int main(int argc, char*const* argv) {
  if (!rsm_init()) return 1;

  // vm execution state (can be initialized with e.g. -R3=0xff)
  u64 iregs[32] = {0};

  // parse command-line options
  int argi = parse_cli_opts(argc, argv, iregs);
  const char* infile = NULL;
  if (argi < argc) { // <srcfile> [R0 [R1 ...]]
    infile = strcmp(argv[argi],"-")==0 ? NULL : argv[argi];
    if (argi < argc-1) errmsg("ignoring %d extra command-line arguments", argc-1 - argi);
  }

  // compile input source file
  rinstr* iv;
  usize icount = compile(infile, &iv);
  if (icount == 0) return 1;

  // execute program
  if (opt_run) {
    u8 memory[1024*1024];
    u64 starttime = nanotime();
    rsm_vmexec(iregs, iv, icount, memory, sizeof(memory));
    char duration[25];
    fmtduration(duration, nanotime() - starttime);
    log("execution finished in %s", duration);
    printf("%llu\t%llu\t%llu\t%llu\t%llu\t%llu\t%llu\t%llu\n",
      iregs[0], iregs[1], iregs[2], iregs[3], iregs[4], iregs[5], iregs[6], iregs[7]);
  }

  return 0;
}

#endif // __wasm__
