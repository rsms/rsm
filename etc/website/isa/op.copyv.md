---
title: copyv
template: template-op
---

`copyv ABv → reg`

Materializes a large constant from values following the {{title}} instruction as one or more trailing 32-bit chunks.

## Semantics

    dstreg = arg(A)
    n = arg(B)
    R(dstreg) = instr[PC+1]
    PC ++
    if n > 2
      R(dstreg) = (R(dstreg) << 32) | instr[PC+2]
      PC ++

## Examples

```rsm
copyv R0 400000000
// encoded as: copyv R0 0x1 0x17d78400

copyv R1 0xdeadbeefbabeface
// encoded as: copyv R1 0x2 0xdeadbeef 0xbabeface

// Equivalent "assignment" syntax:
R1 = 0xdeadbeefbabeface
// encoded as: copyv R1 0x2 0xdeadbeef 0xbabeface
```
