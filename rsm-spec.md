# RSM Virtual Machine Specification

This document specifies the RSM virtual machine: instruction set, virtual memory,
and execution model. It intentionally does not specify an assembly language.

The VM is still evolving. Details marked "current implementation" describe the
implementation in this repository and may change.

## 1. Machine Model

RSM is a register-based virtual machine with fixed-width instructions and a
page-based virtual address space.

An RSM program is executed from a ROM image. The ROM provides:

- a code section: an array of 32-bit instructions;
- a data section: initial bytes mapped into VM memory;
- metadata such as data size and alignment.

The program counter, `PC`, is an instruction index into the code array, not a byte
address. Branch and jump destinations are expressed in instruction units.

Integer operations are defined on 64-bit values unless the operation explicitly
uses a smaller memory width. Floating-point registers exist in the register file,
but the current ISA does not define floating-point operations.

## 2. Registers

The VM has 32 integer registers and 32 floating-point registers.

Integer registers:

| Register | Meaning |
| --- | --- |
| `R0`...`R29` | General-purpose 64-bit integer registers |
| `R30` / `CTX` | Task context register |
| `R31` / `SP` | Stack pointer |

Floating-point registers:

| Register | Meaning |
| --- | --- |
| `F0`...`F29` | General-purpose 64-bit floating-point registers |
| `F30` | Reserved |
| `F31` / `FPSR` | Floating-point status register |

### Calling Convention

The calling convention is part of the execution contract between code generators
and callees.

- `R0`...`R7` hold the first 8 integer arguments and return values.
- `F0`...`F7` hold the first 8 floating-point arguments and return values.
- Additional arguments and values larger than one register are passed on the stack.
- `R0`...`R18` and `F0`...`F18` are caller-saved.
- `R19`...`R29`, `F19`...`F29`, and `SP` are callee-saved.
- `CTX` is a special context register and is not a general argument register.

## 3. Instruction Encoding

Each instruction is a 32-bit little-endian word.

```
        +---------------+---------+---------+---------+-+---------------+
  bit   |31          24 |23    19 |18    14 |13     9 |8|7            0 |
        +---------------+---------+---------+---------+-+---------------+
  ABCD  | D (8)         | C (5)   | B (5)   | A (5)   |i| OP (8)       |
        +---------------+---------+---------+---------+-+---------------+
  ABCu  | C immediate (13)        | B (5)   | A (5)   |i| OP (8)       |
        +-------------------------+---------+---------+-+---------------+
  ABu   | B immediate (18)                  | A (5)   |i| OP (8)       |
        +-----------------------------------+---------+-+---------------+
  Au    | A immediate (23)                            |i| OP (8)       |
        +---------------------------------------------+-+---------------+
```

`OP` identifies the operation. The `i` bit selects an immediate form for
operations whose final operand may be either a register or an immediate value.

Unsigned immediate ranges:

| Field | Bits | Range |
| --- | ---: | ---: |
| `Au` | 23 | `0`...`8,388,607` |
| `Bu` | 18 | `0`...`262,143` |
| `Cu` | 13 | `0`...`8,191` |
| `Du` | 8 | `0`...`255` |

Signed immediate ranges:

| Field | Bits | Range |
| --- | ---: | ---: |
| `As` | 23 | `-4,194,304`...`4,194,303` |
| `Bs` | 18 | `-131,072`...`131,071` |
| `Cs` | 13 | `-4,096`...`4,095` |
| `Ds` | 8 | `-128`...`127` |

Instruction argument encodings:

| Encoding | Arguments |
| --- | --- |
| `_` | no operands |
| `A` | `R(A)` |
| `Au` | `R(A)` or unsigned immediate `Au` |
| `As` | `R(A)` or signed immediate `As` |
| `ABv` | `R(A)`, followed by `Bu` trailing 32-bit words |
| `AB` | `R(A), R(B)` |
| `ABu` | `R(A), R(B)` or unsigned immediate `Bu` |
| `ABs` | `R(A), R(B)` or signed immediate `Bs` |
| `ABC` | `R(A), R(B), R(C)` |
| `ABCu` | `R(A), R(B), R(C)` or unsigned immediate `Cu` |
| `ABCs` | `R(A), R(B), R(C)` or signed immediate `Cs` |
| `ABCD` | `R(A), R(B), R(C), R(D)` |
| `ABCDu` | `R(A), R(B), R(C), R(D)` or unsigned immediate `Du` |
| `ABCDs` | `R(A), R(B), R(C), R(D)` or signed immediate `Ds` |

In the tables below, a final operand named `Bu`, `Cu`, `Ds`, and so on means
"the final operand selected by the encoding": a register in register form or an
immediate in immediate form.

## 4. Instruction Set

### Data Movement

| Name | Encoding | Result | Semantics |
| --- | --- | --- | --- |
| `COPY` | `ABu` | register | `RA = Bu` |
| `COPYV` | `ABv` | register | load immediate words following the instruction into `RA`; advance `PC` by `Bu` |

`COPYV` is used to materialize values too large for the normal immediate fields.
The trailing words are part of the instruction stream and are skipped after use.

### Memory Access

All memory addresses are virtual addresses. Effective addresses are computed as
`RB + Cs`, where `Cs` is interpreted as signed.

| Name | Encoding | Result | Semantics |
| --- | --- | --- | --- |
| `LOAD` | `ABCs` | register | `RA = mem64[RB + Cs]` |
| `LOAD4U` | `ABCs` | register | `RA = zero_extend(mem32[RB + Cs])` |
| `LOAD4S` | `ABCs` | register | `RA = sign_extend(mem32[RB + Cs])` |
| `LOAD2U` | `ABCs` | register | `RA = zero_extend(mem16[RB + Cs])` |
| `LOAD2S` | `ABCs` | register | `RA = sign_extend(mem16[RB + Cs])` |
| `LOAD1U` | `ABCs` | register | `RA = zero_extend(mem8[RB + Cs])` |
| `LOAD1S` | `ABCs` | register | `RA = sign_extend(mem8[RB + Cs])` |
| `STORE` | `ABCs` | memory | `mem64[RB + Cs] = RA` |
| `STORE4` | `ABCs` | memory | `mem32[RB + Cs] = truncate32(RA)` |
| `STORE2` | `ABCs` | memory | `mem16[RB + Cs] = truncate16(RA)` |
| `STORE1` | `ABCs` | memory | `mem8[RB + Cs] = truncate8(RA)` |

Memory accesses check alignment, address range, mapping, and page permissions.

### Integer Arithmetic and Bit Operations

| Name | Encoding | Result | Semantics |
| --- | --- | --- | --- |
| `ADD` | `ABCu` | register | `RA = RB + Cu`, wrapping on overflow |
| `SUB` | `ABCu` | register | `RA = RB - Cu`, wrapping on overflow |
| `MUL` | `ABCu` | register | `RA = RB * Cu`, wrapping on overflow |
| `ADDS` | `ABCs` | register | signed `RA = RB + Cs`, trap on overflow |
| `SUBS` | `ABCs` | register | signed `RA = RB - Cs`, trap on overflow |
| `MULS` | `ABCs` | register | signed `RA = RB * Cs`, trap on overflow |
| `DIV` | `ABCu` | register | `RA = RB / Cu` |
| `MOD` | `ABCu` | register | `RA = RB % Cu` |
| `AND` | `ABCu` | register | `RA = RB & Cu` |
| `OR` | `ABCu` | register | `RA = RB | Cu` |
| `XOR` | `ABCu` | register | `RA = RB ^ Cu` |
| `SHL` | `ABCu` | register | `RA = RB << Cu` |
| `SHRS` | `ABCu` | register | signed arithmetic right shift |
| `SHRU` | `ABCu` | register | unsigned logical right shift |
| `BINV` | `ABu` | register | `RA = ~Bu` |
| `NOT` | `ABu` | register | `RA = !Bu` |

Shift counts must be less than 64. A larger shift count traps in safe execution.

### Comparison

Comparison results are integer booleans: `0` for false and `1` for true.

| Name | Encoding | Result | Semantics |
| --- | --- | --- | --- |
| `EQ` | `ABCu` | register | `RA = (RB == Cu)` |
| `NEQ` | `ABCu` | register | `RA = (RB != Cu)` |
| `LTU` | `ABCu` | register | `RA = (RB < Cu)`, unsigned |
| `LTEU` | `ABCu` | register | `RA = (RB <= Cu)`, unsigned |
| `GTU` | `ABCu` | register | `RA = (RB > Cu)`, unsigned |
| `GTEU` | `ABCu` | register | `RA = (RB >= Cu)`, unsigned |
| `LTS` | `ABCs` | register | `RA = ((i64)RB < (i64)Cs)` |
| `LTES` | `ABCs` | register | `RA = ((i64)RB <= (i64)Cs)` |
| `GTS` | `ABCs` | register | `RA = ((i64)RB > (i64)Cs)` |
| `GTES` | `ABCs` | register | `RA = ((i64)RB >= (i64)Cs)` |

### Control Flow

The interpreter fetches an instruction at `PC`, increments `PC`, then executes
the instruction. Relative branch offsets are therefore relative to the instruction
after the branch.

| Name | Encoding | Result | Semantics |
| --- | --- | --- | --- |
| `IF` | `ABs` | none | if `RA != 0`, `PC = PC + Bs` |
| `IFZ` | `ABs` | none | if `RA == 0`, `PC = PC + Bs` |
| `CALL` | `Au` | none | push return `PC`; `PC = Au` |
| `JUMP` | `Au` | none | `PC = Au` |
| `RET` | `_` | none | pop return `PC` |

`CALL` and `RET` use the task stack and store return addresses as 64-bit
instruction indices.

### Tasks, System Calls, and I/O

| Name | Encoding | Result | Semantics |
| --- | --- | --- | --- |
| `TSPAWN` | `Au` | register | `R0 = spawn_task(pc=Au, args=R0...R7)` |
| `SYSCALL` | `Au` | none | invoke system call `Au` with arguments in registers |
| `WRITE` | `ABCDs` | register | `RA = write(fd=Ds, srcaddr=RB, size=RC)` |
| `READ` | `ABCDu` | register | `RA = read(fd=Du, dstaddr=RB, size=RC)` |
| `MCOPY` | `ABCu` | memory | copy `Cu` bytes from `RB` to `RA` |
| `MCMP` | `ABCDu` | register | compare `Du` bytes at `RB` and `RC` |
| `STKMEM` | `As` | none | adjust stack by `As`, growing or shrinking stack mappings as needed |

Current system calls:

| Code | Name | Arguments | Result |
| ---: | --- | --- | --- |
| `0` | `SC_EXIT` | `R0` status | exits the program |
| `1` | `SC_SLEEP` | `R0` nanoseconds | `R0` receives remaining sleep time or error |
| `RSM_MAX_Au` | `SC_TEXIT` | none | exits the current task |

Current implementation notes:

- `READ` is not implemented by the scheduler interpreter.
- `TSPAWN` creates a task running the same code as its parent, but argument
  copying is still incomplete.
- The scheduler appends a one-instruction epilogue to main code:
  `SYSCALL SC_TEXIT`.

## 5. Virtual Memory

RSM programs use virtual addresses, not host addresses. The current address-space
configuration is:

| Constant | Value |
| --- | ---: |
| Page size | `4096` bytes |
| Page offset bits | `12` |
| Virtual address bits | `48` |
| First valid address | `0x1000` |
| Last valid address | `0xffffffffffff` |
| Addressable space | `256 TiB`, excluding page zero |

Page zero is not a valid program address. This reserves `0` as a null-like
invalid address.

### Address Translation

A virtual address is split into:

- a virtual frame number, `VFN = (vaddr >> 12) - 1`;
- a page offset, `vaddr & 0xfff`.

The VM uses a 4-level page-table tree:

| Level | Index bits | Entries | Coverage per entry |
| ---: | ---: | ---: | ---: |
| 0 | 9 | 512 | 512 GiB |
| 1 | 9 | 512 | 1 GiB |
| 2 | 9 | 512 | 2 MiB |
| 3 | 9 | 512 | 4 KiB |

Each page table is exactly one VM page: `512` entries times `8` bytes.

A page table entry is 64 bits. For a leaf page entry, low bits hold metadata and
permission bits, and high bits hold the host frame number. For a table entry,
low bits hold table metadata and high bits hold the next page table address.

Leaf page permissions:

| Bit | Meaning |
| --- | --- |
| `R` | page may be read |
| `W` | page may be written |

Other page metadata includes uncacheable, purgeable, accessed, written, and type
fields. These are implementation-level metadata and are not currently exposed to
programs.

### Mapping Rules

`vm_map_add` maps a virtual page range with read and/or write permissions.

- If a host address is supplied, virtual pages map to that host page range.
- If the host address is zero, backing pages are allocated lazily on first access.
- `vm_map_del` removes a virtual page range.
- Caches must be invalidated after mappings are removed or changed.

The current runtime maps ROM data at `VM_ADDR_MIN` (`0x1000`) as read-write.
Code is stored in host memory as an instruction array and is not currently mapped
as executable virtual memory.

### Translation Cache

Each machine thread owns VM translation caches for read, write, and read-write
access. The cache is a small direct-mapped TLB:

- 256 entries;
- indexed by low VFN bits;
- stores a tag and `host_page_address - virtual_page_address` difference.

The cache tag also encodes alignment requirements. A cache miss resolves the page
through the page table tree, verifies address range, alignment, and permissions,
then updates the cache.

### Memory Faults

The VM traps or reports an error for:

- virtual addresses outside the valid range;
- unmapped pages;
- permission violations;
- alignment violations;
- stack overflow or unbalanced stack operations;
- unsupported operations in the current interpreter.

## 6. Initial Memory Layout

The scheduler execution model uses the 48-bit virtual address space as follows:

```
0x000000000000        invalid / unmapped page zero
0x000000001000        data segment starts here
...                   free VM space available for mappings
0x1000000000000       top stack boundary (STACK_VADDR, one past valid VM space)
```

Current constants:

- `DATA_VADDR = VM_ADDR_MIN`
- `STACK_VADDR = align_down(VM_ADDR_MAX + 1, STK_ALIGN)`
- `STK_ALIGN = 8`
- default stack virtual size is `1 MiB`
- minimum stack backing is one page

The main task starts with:

- `PC = 0`;
- `R0 = 0`, `R1 = 0` for `argc` and `argv`;
- `SP` near the top of its stack;
- `CTX` set to the task context pointer.

The task stack grows downward. `CALL`, `RET`, context switching, and `STKMEM`
all use the task stack.

## 7. Execution Model

### Interpreter Loop

The interpreter repeatedly:

1. checks that `PC` is within the instruction array;
2. fetches `instr = code[PC]`;
3. increments `PC`;
4. decodes opcode, immediate flag, and operands;
5. executes the operation;
6. repeats until the task exits, blocks in a system call, or traps.

The current scheduler interpreter uses either a computed-goto jump table or a
switch statement, depending on build configuration. This is an implementation
choice and does not affect instruction semantics.

### Tasks

A task is a coroutine-like VM execution context. Each task has:

- task ID;
- parent task pointer;
- `PC`;
- instruction array and instruction count;
- saved `SP`;
- stack low/high bounds;
- status;
- split-stack count.

Task statuses:

| Status | Meaning |
| --- | --- |
| `T_IDLE` | allocated but not initialized |
| `T_RUNNABLE` | queued and ready to run |
| `T_RUNNING` | executing user code |
| `T_SYSCALL` | blocked or running in a system call |
| `T_DEAD` | exited or available for reuse |

### Scheduler

The scheduler follows a Go-like M/P/T model:

| Term | Meaning |
| --- | --- |
| `T` | task, a coroutine-like execution context |
| `M` | machine, an OS thread |
| `P` | processor, an execution resource required to run tasks |

An `M` must hold a `P` to execute a `T`. An `M` may block in a system call
without holding a `P`, allowing another `M` to run tasks on that `P`.

Scheduling uses:

- per-`P` run queues;
- a global run queue;
- work stealing between `P`s;
- idle and spinning `M` management;
- timer hooks, currently incomplete.

### Context Switching

Temporary/caller-saved registers are owned by the running code and are not saved
as part of task suspension. Long-lived/callee-saved registers are preserved in a
task context record stored on the task stack.

Saved task context includes:

- `F19`...`F31`;
- `R19`...`R30`;
- saved `SP` in the task structure.

`SP` itself is restored from the task's saved `sp` field.

### Stack Management

The stack grows downward. `STKMEM` adjusts `SP`:

- positive `As`: allocate stack space by decreasing `SP`;
- negative `As`: release stack space by increasing `SP`.

If a positive adjustment would move below the current stack mapping, the runtime
tries to extend the contiguous stack mapping. If that space is unavailable, it
allocates a split stack elsewhere and links it to the previous stack.

When shrinking reaches the end of a split stack, the split segment is unmapped
and the previous stack range is restored.

## 8. Errors and Traps

The safe execution path diagnoses:

- unaligned memory access;
- unaligned stack pointer;
- stack overflow;
- out-of-bounds load, store, or `PC`;
- opcode/immediate-form mismatch;
- invalid shift exponent;
- signed arithmetic overflow for `ADDS`, `SUBS`, and `MULS`.

Current implementation behavior for many traps is to panic or abort execution
with diagnostic output. A future embedding API may expose these as recoverable
VM errors.
