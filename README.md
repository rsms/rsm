<img src="etc/rsm.svg" alt="RSM" width=96>

RSM is a virtual computer, a form of virtual machine.

- [simple RISC instruction set](#isa) with plenty of general-purpose registers.
- [virtual memory](etc/vmem.txt) means programs have no access to host memory addresses and all memory accesses are checked. It also means all RSM programs have the same address space (currently 48 bits; 256 TiB) and could even be migrated across hosts at runtime.
- embraces the linear memory model -- addressing memory is a simple and easy to-understand way to deal with data. Prefer memory-mapped I/O over custom system calls.
- [multi-threaded Go-style task scheduler](src/sched.h)
- portable and embeddable, without dependencies. (RSM can even build without libc.)
  - entire API in one header: [src/rsm.h](src/rsm.h)
- simple explicit [assembly language](#assembly-language)
  - compiles to very compact, self-contained ["ROM" files](etc/rom-layout.txt)
  - RSM comes with an integrated assembler, making it possible to compile & run code at runtime. (It can be disabled with a macro if desired in embedding scenarios.)
  - includes an AST API for code generation without an intermediate assembly step, useful if you want to make a compiler that targets RSM.

Project goals:
1. learn, have fun -- simplicity
2. substrate, a thing to make other thing on
3. longevity -- I want to be able to run a (multimedia) program in 10+ years

"RSM" initially stands for "rsms's smol machine" but can also be interpreted as
"Really smol machine", or "Raggedy-ass special mumbojumbo", or
the recursive acronym "RSM smol machine" (in case you miss the golden days of PHP), or
anything you'd like it to mean! Your imagination is really the limit here my friend.

> **Status of this project:** This is a passion project and thus is not "production grade" stuff. The instruction set and semantics are changing. I'd be thrilled and happy if you play with RSM and build stuff on it, but please do keep in mind that stuff will change. Contributions are welcome after [an initial discussion](https://twitter.com/rsms)

Work in progress, TODO:
- Finish the scheduler
- Framebuffer for drawing graphics (maybe even webgpu?)
- Audio
- Lots of little small pieces here and there [marked with "TODO" in the code](https://github.com/rsms/rsm/search?l=C&o=desc&q=TODO&s=indexed&type=code)


## Building & running

```sh
./build.sh -debug
./out/debug/rsm -d -R0=15 examples/factorial.rsm
# R0 will contain the result 1307674368000
```

You'll need the following things to build rsm:
- bash (or a bash-compatible shell like zsh)
- [ninja](https://ninja-build.org) (or a ninja-compatible program like [samurai](https://github.com/michaelforney/samurai))
- C11 compiler with libc (e.g. clang or GCC)

You can use `rsm` as a really awkward calculator:

```sh
$ echo 'fun x() { R0 = R0 * 2; ret; }' | out/debug/rsm -d -R0=123
# R0 will contain the result 246
```

RSM assembly can be compiled into a ROM file which can later be executed:

```sh
$ echo 'fun x() { R0 = R0 * 2; ret; }' | out/debug/rsm -o multiply.rom
$ out/debug/rsm -d -R0=123 multiply.rom
# R0 will contain the result 246
```

## Example

```sh
$ cat <<EXAMPLE > example.rsm
fun factorial(i32) i32 {
    R1 = R0        // ACC = n (argument 0)
    R0 = 1         // RES (return value 0)
    ifz R1 end     // if n==0 goto end
  b1:              // <- [b0] b1
    R0 = R1 * R0   // RES = ACC * RES
    R1 = R1 - 1    // ACC = ACC - 1
    if R1 b1       // if n!=0 goto b1
  end:             // <- b0 [b1]
    ret            // RES is at R0
}
EXAMPLE
$ out/debug/rsm -d -R0=15 example.rsm
# R0 will contain the result 1307674368000
```

See the `examples/` directory for more.

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
- 30 general-purpose integer registers R0…R29
- 30 general-purpose floating-point registers F0…F29
- Context register CTX (R30)
- Stack pointer SP (R31)
- Floating-point status FPSR (F31)
- _TODO: is a fp control reg needed for stuff like 0div traps? No... no._


### Calling convention

- first 8 integer argument/return values in R0…R7, rest on stack
- first 8 F.P. argument/return values in F0…F7, rest on stack
- anything larger than the register size goes on stack
- caller saves R0…R18, F0…F18 (owned by callee)
- callee saves R19…R29, F19…F29 (owned by caller)
- convention inspired by [AAPCS64](https://github.com/ARM-software/abi-aa)

#### Callee-owned registers

Callee-owned (caller-saved, temporary) registers.
Caller needs to save these before a call (if caller uses them.)
Callee can freely use these registers.

    R0…R7   1st…8th integer argument/return value
    F0…F7   1st…8th floating-point argument/return value
    R8…R18  General purpose
    F8…F18  General purpose

#### Caller-owned registers

Caller-owned (callee-saved, long-lived) registers.
Caller does not need to save these registers.
Callee using these must save and later restore their values before returning.

    R19…R29   General purpose
    F19…F29   General purpose
    SP (R31)  Stack pointer


### Special registers

    CTX  (R30)  Context (like AAPCS platform reg and Go's G)
    SP   (R31)  Stack pointer
    -    (F30)  Reserved (unused)
    FPSR (F31)  Floating-point status


## Assembly language

### Syntax

White-space is ignored

```abnf
file       = (fundef | constdef | datadef)*

constdef   = "const" name type? "=" expr ";"
datadef    = "data" name (type ("=" expr)? | "=" expr) ";"

fundef     = "fun" name "(" params? ")" result? funbody?
params     = param ("," param)*
result     = param ("," param)*
param      = name type | type
funbody    = "{" block0? block* "}"
block0     = blockstmt*
block      = name ":" blockstmt*
blockstmt  = operation | assignment | binop | constdef | datadef

type       = inttype | arraytype
inttype    = "i1" | "i8" | "i16" | "i32" | "i64"
arraytype  = type "[" intlit "]"

operation  = opcode operand*
           ; brz R1 end
binop      = operand ("-" | "+" | "*" | "/") operand
           ; x + 3
assignment = reg "=" (operation | operand) ";"
operand    = reg | literal | name

literal    = intlit
intlit     = "-"? (binlit | declit | hexlit)
binlit     = "0b" ("0" | "1")+
declit     = (0-9)+
hexlit     = "0x" (0-9A-Fa-f)+

name       = ("_" | A-Za-z | uniprint) ("_" | A-Za-z | 0-9 | uniprint)

uniprint   = <utf8 encoding of printable unicode codepoint>

opcode     = copy | copyv
           | load | load4u | load4s | load2u | load2s | load1u | load1s
           | store | store4 | store2 | store1
           | push | pop
           | add | sub | mul | adds | subs | muls
           | div | mod
           | and | or | xor | shl | shrs | shru | binv
           | not
           | eq | neq | ltu | lts | lteu | ltes | gtu | gts | gteu | gtes
           | if | ifz | call | jump | ret
           | mcopy | mcmp
           | write | read
```

Comments are ignored and can appear wherever whitespace can appear

```abnf
comment      = linecomment | blockcomment
linecomment  = "//" <any character except LF> <LF>
blockcomment = "/*" <any character> "*/"
```
