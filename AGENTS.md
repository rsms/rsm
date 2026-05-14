# RSM virtual machine

This file defines the default working protocol for coding agents for this project.
Scope: entire subdirectory (i.e. dirname of AGENTS.md).

## 1) Project overview

RSM is a virtual computer, a form of virtual machine. Read README.md

## 2) Engineering Principles (Normative)

These principles are mandatory. They are implementation constraints, not suggestions.

### 2.1 KISS (Keep It Simple, Stupid)

Required:
- Prefer straightforward control flow over meta-programming.
- Prefer explicit comptime branches and typed structs over hidden dynamic behavior.
- Keep error paths obvious and localized.

### 2.2 YAGNI (You Aren't Gonna Need It)

Required:
- Do not add config features, command line arguments or other features without a concrete caller/user.
- Do not introduce speculative abstractions.
- Keep unsupported paths explicit (panic or return clear error) rather than silent no-ops.

### 2.3 DRY (Don't Repeat Yourself) + Rule of Three ("Three strikes and you refactor")

Required:
- Duplicate small local logic when it preserves clarity.
- Extract shared helpers only after repeated, stable patterns (rule-of-three).
- When extracting, preserve module boundaries and avoid hidden coupling.

### 2.4 Fail fast + Explicit errors

Required:
- Prefer explicit errors for unsupported or unsafe states.
- Never silently broaden permissions or capabilities.

## 3) Agent Workflow (Required)

1. **Read before write** — inspect existing implementation before editing.
2. **Define scope boundary** — one concern per change; avoid mixed feature+refactor+infra patches.
3. **Implement minimal patch** — apply KISS/YAGNI/DRY rule-of-three explicitly.
4. **Test** — Write tests for new features or changes
5. **Incremental** — Take an incremental approach: keep the program working at each step.
6. **Version control**:
    - Before committing changes, run tests that are affected by changes
    - Serialize git index writes: never run `git add`, `git commit`, `git rm`, `git mv`, or similar index-mutating commands in parallel.

## 4) Building and testing

- `./test.sh` build in debug mode and run all tests
- `./test.sh -release` build in release mode (with safechecks) and run all tests
- `./build.sh` builds rsm native in debug mode
- `./build.sh -release` builds rsm native in release mode (with safechecks)
- `./build.sh -- rsm.wasm` builds wasm products in debug mode
- `./build.sh -- rms rsm.wasm` builds native and wasm products in debug mode
- `./build.sh -j1 -- rsm.wasm` builds wasm products in debug mode, without parallelism (useful for debugging build errors)
- `./build.sh -analyze` run clang static analyzer on source code
- `./build.sh -help` shows options

## 5) Documentation

- `etc/website/isa/index.md` describes the virtual instruction set architecture
- `etc/website/isa/op.*.md` describes an ISA operation
- `etc/website/ops.json` lists all ISA operations (generated from `src/rsm.h`)
- `etc/website/assembler/index.md` describes the assembly language
- `etc/website/virtual-memory/index.md` describes virtual memory
- Some data in the website is generated from `src/rsm.h` by `etc/website/_config.js` which is run by `etc/website/build.sh`
