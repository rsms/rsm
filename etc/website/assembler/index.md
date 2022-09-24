---
title: Assembler
---

# {{title}}

RSM includes an integrated assembler, making it possible to compile & run code at runtime.
The assembler can be disabled/removed from builds with a macro, if desired.

Includes an AST API for code generation without an intermediate assembly step, useful if you want to make a compiler that targets RSM.


## Compiling

Assembly programs can be compiled just in time when executing a program or ahead of time with the `rsm` tool's `-o` flag to produce a ROM image.

hello.rsm

```rsm
data message = "Hello world\n"
fun main(i32) {
  R0 = message       // address of message
  R1 = 12            // length of message
  R0 = write R0 R1 1 // write to stdout
}
```

Running it directly:

```shell
$ rsm hello.rsm
Hello world
```

Compiling to a ROM image and then running it:

```shell
$ rsm -o hello.rom hello.rsm
$ rsm hello.rom
Hello world
```


## Assembly language

### Example

```rsm
// This program asks for your name on stdin and then greets you
const STDIN = 0
const STDOUT = 1
const STKSIZE = 64 // stack space (must be a multiple of 8)

data ask_msg     = "What is your name?\n"
data readerr_msg = "Failed to read stdin\n"
data no_name_msg = "I see, you don't want to tell me.\n"
data reply_msg1  = "Hello "
data reply_msg2  = "! Nice to meet ya\n"

fun main(i32) {
  // reserve STKSIZE bytes of stack space
  SP = sub SP STKSIZE

  // ask the user for their name
  call print ask_msg 19
  if R0 end

  // read up to STKSIZE bytes from STDIN, to stack memory at SP
  R0 = STKSIZE // number of bytes to read
  R8 = read SP R0 STDIN
  R0 = lts R8 0 // check if read failed ()
  if R0 readerr
  R8 = R8 - 1   // exclude any line break
  R0 = lts R8 1 // check if read was empty
  if R0 noname

  // reply with the user's name
  call print reply_msg1 6
  call print SP R8 // SP = address of name read
  call print reply_msg2 18
  jump end

noname:
  call print no_name_msg 34
  jump end
readerr:
  // note: To test this, use "0<&-" in csh, e.g. out/rsm askname.rsm 0<&-
  call print readerr_msg 21
end:
  SP = add SP STKSIZE // restore stack pointer
}

fun print(addr i64, size i64) ok i1 {
  R0 = write R0 R1 STDOUT
  R0 = lts R0 R1  // return 1 on failed or short write
}
```

Compile and run:

```shell
$ rsm examples/askname.rsm
What is your name?
Robin
Hello Robin! Nice to meet ya
$
```


### Syntax

Whitespace is ignored

```bnf
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

opcode     {{!
for (let i = 0; i < site.rsm_ops.length; i++) {
  let op = site.rsm_ops[i].name
  if (i % 7 == 6)
    print('\n          ')
  print((i == 0 ? '= ' : ' | ') + op)
}
}}

```

Comments are ignored and can appear wherever whitespace can appear

```bnf
comment      = linecomment | blockcomment
linecomment  = "//" <any character except LF> <LF>
blockcomment = "/*" <any character> "*/"
```

