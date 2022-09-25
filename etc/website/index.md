---
title: RSM virtual computer
---

# {{ title }}

What is RSM?<br>
RSM is a virtual computer; an imaginary set of hardware with a minimal OS foundation.
It defines things like a [CPU instruction set](isa/),
[memory semantics](virtual-memory/) and [program images](rom/) which together forms a computing environment that is truly portable — an RSM program running in a web browser, on macOS, Linux (or some wild system dreamed up by you) behaves the same.
RSM runs on top of other "real" OSes and hardware.

But... wat... I can't even...<br>
You might be wondering what I've been smoking. Why do something like this? Why WHY?!?

Software becomes unusable way too quickly. Remember that game or app you really loved ten years ago? It doesn't work anymore because in this world of relentless industrialization of software that we've built for ourselves, only today matters; there's always a new version with new features. More more more. A culture of putting many disparate pieces together into fragile houses of cards, all for the purpose of satisfying a never-ending thirst for short-lived novelty.

I'd very much like software to have a longer life. After all, writing a good program takes a lot of effort and emotion.

So how could you make software that can run ten years from now? Or even in 50 years? Computers will keep on evolving, hardware architectures come and go, and OSes will keep on changing. The only option is to build a system that can run or be emulated on top of whatever computer we will have tomorrow. RSM is an attempt at building such a system.

Another reason I'm excited to work on RSM is because it [gives me joy](v1/) to build something like this. Almost every day of working on this project has [taught me something](https://github.com/rsms/rsm/tree/main/.logbook).

So, summarizing the reason RSM exists:

- Offer an option for software longevity: I want to be able to run a program in 10+ years
- Substrate, a portable platform that can be archived
- For the fun of it, for the joy of learning and building


## Installing

For source code and instructions on how to install & use RSM,
see [github.com/rsms/rsm](https://github.com/rsms/rsm)

> **Status of this project:** This is a passion project and thus is not "production grade" stuff. The instruction set and semantics are changing. I'd be thrilled and happy if you play with RSM and build stuff on it, but please do keep in mind that stuff will change. Contributions are welcome, especially contributions of the intellectual kind; conversations and ideas.


## Documentation

- [Instruction Set Architecture](isa/)
- [Assembler](assembler/)
- [Virtual Memory](virtual-memory/)
- [ROM image layout](rom/)


## History

- [Initial conversation on twitter](https://twitter.com/rsms/status/1492582847982440448)
  and [related tweets](https://twitter.com/search?q=from%3Arsms%20%22rsm%22&f=live)
- [Video diary of the inception of RSM](v1/)
- [Web browser playground](play/) (old outdated version)
