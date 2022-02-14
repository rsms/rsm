# rsm — rsms's smol machine

Virtual computer

Project goals:
1. learn, have fun -- simlicity
2. substrate, a thing to make other thing on
3. longevity -- I want to be able to run a (multimedia) program in 10+ years

Constraints
- embraces the linear memory model
- simple semantics:
  - global constants
  - vm uses registers
  - assmebly language offers named (automatic regalloc) locals
    - mutable locals, parameters and variables are all treated the same
    - simple syntax where whitespace is ignored


## Building & running

```
./build.sh -debug -w -run=out/rsm
```


## Example

Factorial using explicit registers and named locals

```
fun factorial (i32) i32
  b0:              //
    r8 = r0        // ACC = n (n is in r0, argument 0)
    r0 = 1         // RES (return value 0)
    ifz r8 end     // if n==0 goto end
  b1:              // <- [b0] b1  ("[b]=implicit/fallthrough")
    r0 = mul r8 r0 // RES = ACC * RES
    r8 = sub r8 1  // ACC = ACC - 1
    ifnz r8 b1     // if n!=0 goto b1
  end:             // <- b0 [b1]
    ret            // RES is at r0
```

```
fun factorial (n i32) i32
  var ACC i32         // accumulator
  var RES i32         // result
  b0:                 //
    ifz n end         // if n==0 goto end
    ACC = n           // initialize ACC to n
    RES = 1           // initialize RES to 1
  b1:                 // <- [b0] b1  ("[b]=implicit/fallthrough")
    RES = mul ACC RES // RES = ACC * RES
    ACC = sub ACC 1   // ACC = ACC - 1
    ifnz ACC b1       // if n!=0 goto b1
  end:                // <- b0 [b1]
    ret RES
```


## Instruction Set Architecture

Instructions are fixed-size, 32 bits wide, little endian.
PC and jump- & branch destinations are expressed in #instructions rather than bytes.
There is room for 256 operations and 32+32 (int+fp) registers (8 bit OP, 5 bit reg)

```
       ┌─────────────────┬─────────┬─────────┬─────────┬───────────────┐
 bit   │3 3 2 2 2 2 2 2 2│2 2 2 1 1│1 1 1 1 1│1 1 1    │               │
       │1 0 9 8 7 6 5 4 3│2 1 0 9 8│7 6 5 4 3│2 1 0 9 8│7 6 5 4 3 2 1 0│
       ├─────────────────┼─────────┼─────────┼─────────┼───────────────┤
 ABCD  │          D (9)  │  C (5)  │  B (5)  │  A (5)  │     OP (8)    │
       ├─────────────────┴─────────┼─────────┼─────────┼───────────────┤
 ABCw  │                    C (14) │  B (5)  │  A (5)  │     OP (8)    │
       ├───────────────────────────┴─────────┼─────────┼───────────────┤
 ABw   │                              B (19) │  A (5)  │     OP (8)    │
       ├─────────────────────────────────────┴─────────┼───────────────┤
 Aw    │                                        A (24) │     OP (8)    │
       └───────────────────────────────────────────────┴───────────────┘
```

Registers:
- 30 general-purpose integer registers (R0 ... R29)
- Context register (CTX; 31st int register)
- Stack pointer (SP; 32nd int register)
- 31 floating-point registers (F0 ... F30)
- Floating-point status (FPSR; 32nd fp register)
- _TODO: Callee/caller saves what? Maybe just use AAPCS64?_
- _TODO: is a fp control reg needed for stuff like 0div traps? No... no._

Calling convention:
- first 8 int inputs/outputs in R0 ... R7, rest on stack
- first 8 fp  inputs/outputs in F0 ... F7, rest on stack
- anything larger than register size goes on stack


## Assembly language

[this is just at the idea stage]

Simple syntax where whitespace is ignored

```
fundef = "fun" name "(" params? ")" result? LF
params = name type ("," name type)*
result = type ("," type)*
type   = i1 | i8 | i16 | i32 | i64
       | u1 | u8 | u16 | u32 | u64 | addr
       | f32 | f64 | f128
instr  = [reg "="] op arg*
```
