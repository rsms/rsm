# RSM

Virtual computer

Project goals:
1. learn, have fun -- simplicity
2. substrate, a thing to make other thing on
3. longevity -- I want to be able to run a (multimedia) program in 10+ years

Constraints
- embraces the linear memory model
- simple semantics:
  - global constants
  - vm uses registers
  - assembly language offers named (automatic regalloc) locals
    - mutable locals, parameters and variables are all treated the same
    - simple syntax where white-space is ignored

"RSM" initially stands for "rsms's smol machine" but can also be interpreted as
"Really smol machine", or "Raggedy-ass special mumbojumbo", or
the recursive acronym "RSM smol machine" (in case you miss the golden days of PHP), or
anything you'd like it to mean! Your imagination is really the limit here my friend.

TODO
- memory (program layout, load & store, grow?)
- syscalls..? Some way to interface with peripherals/devices, time, file system etc
- display framebuffer
- audio


## Building & running

```
./build.sh -debug
./out/rsm examples/factorial.rsm 15
```

You'll need the following things to build rsm:
- bash (or a bash-compatible shell like zsh)
- [ninja](https://ninja-build.org) (or a ninja-compatible program like [samurai](https://github.com/michaelforney/samurai))
- C11 compiler with libc (e.g. clang or GCC)

You can use `rsm` as a really awkward calculator:

```sh
$ echo 'fun x() { R0 = R0 * 2; ret; }' | out/rsm - 123
assembled some sweet vm code:
   0  mul R0  R0  0x2
   1  ret
evaluating function0(123)
result R0..R7: 246 0 0 0 0 0 0 0
```

If you're a bit crazy you can also embed it by copying the source files
into your project and using the API in `rsm.h`.

## Example

```sh
$ cat <<EXAMPLE > example.rsm
fun factorial (i32) i32 {
    R1 = R0        // ACC = n (argument 0)
    R0 = 1         // RES (return value 0)
    brz R1 end     // if n==0 goto end
  b1:              // <- [b0] b1
    R0 = mul R1 R0 // RES = ACC * RES
    R1 = sub R1 1  // ACC = ACC - 1
    brnz R1  b1    // if n!=0 goto b1
  end:             // <- b0 [b1]
    ret            // RES is at R0
}
EXAMPLE
$ out/rsm example.rsm 15
assembled some sweet vm code:
   0  copy   R1   R0
   1  copy   R0   0x1
   2  brz    R1   3
   3  mul    R0   R1   R0
   4  sub    R1   R1   0x1
   5  brnz   R1   -3
   6  ret
evaluating function0(15)
result R0..R7: 1307674368000 0 0 0 0 0 0 0
```


<a name="isa"></a>
## Instruction Set Architecture

Instructions are fixed-size, 32 bits wide, little endian.
PC and jump- & branch destinations are expressed in #instructions rather than bytes.
There is room for 256 operations and 32+32 (int+fp) registers (8 bit OP, 5 bit reg)
Most instructions accept reg or immediate (`i` bit is set) as last argument

```
        ┌───────────────┬─────────┬─────────┬─────────┬─┬───────────────┐
  bit   │3 3 2 2 2 2 2 2│2 2 2 2 1│1 1 1 1 1│1 1 1 1  │ │               │
        │1 0 9 8 7 6 5 4│3 2 1 0 9│8 7 6 5 4│3 2 1 0 9│8│7 6 5 4 3 2 1 0│
        ├───────────────┼─────────┼─────────┼─────────┼─┼───────────────┤
  ABCD  │         D (8) │  C (5)  │  B (5)  │  A (5)  │i│     OP (8)    │
        ├───────────────┴─────────┼─────────┼─────────┼─┼───────────────┤
  ABCw  │                  C (13) │  B (5)  │  A (5)  │i│     OP (8)    │
        ├─────────────────────────┴─────────┼─────────┼─┼───────────────┤
  ABw   │                            B (18) │  A (5)  │i│     OP (8)    │
        ├───────────────────────────────────┴─────────┼─┼───────────────┤
  Aw    │                                      A (23) │i│     OP (8)    │
        └─────────────────────────────────────────────┴─┴───────────────┘
```

Registers:
- 30 general-purpose integer registers (R0 ... R29)
- Context register (CTX aka R30)
- Stack pointer (SP aka R31)
- 31 floating-point registers (F0 ... F30)
- Floating-point status (FPSR aka F31)
- _TODO: Callee/caller saves what? Maybe just use AAPCS64?_
- _TODO: is a fp control reg needed for stuff like 0div traps? No... no._

Calling convention:
- first 8 int inputs/outputs in R0 ... R7, rest on stack
- first 8 fp  inputs/outputs in F0 ... F7, rest on stack
- anything larger than register size goes on stack


## Assembly language

### Syntax

White-space is ignored

```abnf
file       = fundef*
fundef     = "fun" name "(" params? ")" result? funbody?
params     = param ("," param)*
result     = param ("," param)*
param      = name type | type
funbody    = "{" block0? block* "}"
block0     = blockstmt*
block      = name ":" blockstmt*
blockstmt  = operation | assignment | binop
operation  = op arg*
           ; brz R1 end
binop      = arg ("-" | "+" | "*" | "/") arg
           ; x + 3
assignment = (reg | local) "=" (operation | arg)
           ; R3 = add R1 R4
           ; x  = add y z
arg        = reg | local | literal
literal    = intlit
intlit     = "-"? (binlit | declit | hexlit)
binlit     = "0b" ("0" | "1")+
declit     = (0-9)+
hexlit     = "0x" (0-9A-Fa-f)+
```
