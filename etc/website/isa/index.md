---
title: Instruction Set Architecture
---

# {{title}}

RSM offers 30 general-purpose 64-bit integer [registers](#registers) and 30 64-bit floating-point registers. Instructions are 32 bits wide, fixed size. PC and jump- & branch destinations are expressed in #instructions rather than bytes.


## Instructions

Most instructions accept register number or immediate value as their last argument.
See [Instruction Encoding](#instruction-encoding) for more details.

<table>
  <thead>
    <tr><th>Name</th><th>Inputs</th><th>Output</th><th>Semantics</th></tr>
  </thead>
  <tbody>
    {{
    for (let op of site.rsm_ops) {
      print(`<tr>`)
      print(`<td><a href="op.${op.name}.html">${op.name}</a></td>`)
      print(`<td>${op.args}</td>`)
      print(`<td>${op.result == "nil" ? "–" : op.result}</td>`)
      print(`<td>${op.semantics}</td>`)
      print(`</tr>\n`)
    }
    }}
  </tbody>
</table>

### Instruction encoding

Instructions are fixed-size, 32 bits wide, little endian.
PC and jump- & branch destinations are expressed in #instructions rather than bytes.
There is room for 256 operations and 32+32 (int+fp) registers (8 bit OP, 5 bit reg)
Most instructions accept reg or immediate (`i` bit is set) as last argument.

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

               Min         Max                  Min         Max
    Au           0   8,388,607       As  -4,194,304   4,194,303
    Bu           0     262,143       Bs    -131,072     131,071
    Cu           0       8,191       Cs      -4,096       4,095
    Du           0         255       Ds        -128         127


#### Instruction argument encoding

| Encoding | Arguments
|----------|------------------------------------
| _        | (none)
| A        | R(A)
| Au       | R(A) or immediate unsigned value
| As       | R(A) or immediate signed value
| ABv      | R(A), with Bu immediate trailing u32 values
| AB       | R(A), R(B)
| ABu      | R(A), R(B) or immediate unsigned value
| ABs      | R(A), R(B) or immediate signed value
| ABC      | R(A), R(B), R(C)
| ABCu     | R(A), R(B), R(C) or immediate unsigned value
| ABCs     | R(A), R(B), R(C) or immediate signed value
| ABCD     | R(A), R(B), R(C), R(D)
| ABCDu    | R(A), R(B), R(C), R(D) or immediate unsigned value
| ABCDs    | R(A), R(B), R(C), R(D) or immediate signed value


## Registers

### Callee-owned registers

Callee-owned (caller-saved, temporary) registers.
Caller needs to save these before a call (if caller uses them.)
Callee can freely use these registers.

    R0…R7   1st…8th integer argument/return value
    F0…F7   1st…8th floating-point argument/return value
    R8…R18  General purpose
    F8…F18  General purpose

### Caller-owned registers

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



## Calling convention

- first 8 integer argument/return values in R0…R7, rest on stack
- first 8 F.P. argument/return values in F0…F7, rest on stack
- anything larger than the register size goes on stack
- caller saves R0…R18, F0…F18 (owned by callee)
- callee saves R19…R29, F19…F29 (owned by caller)
- convention inspired by [AAPCS64](https://github.com/ARM-software/abi-aa)

