---
title: Virtual Memory
---

# {{title}}

RSM programs run in a 64-bit virtual address space, independent of the host OS's memory capabilities.
Every invocation of a program is deterministic in regards to memory addressing.

Currently the address space implementation is limited to 48 bits, but can easily be extended up to 64 bits in the future. 48 bits allows RSM programs to address 256 TiB of memory.
Memory is arrranged in pages of 4096 bytes. I.e. the address `0xbeef` and `0xbabe` are both part of the same page `0xb000`.
At runtime, memory accesses are translated and mapped onto "actual" memory of the host OS.

There are many upsides to virtual memory beyond determinism. For example, RSM can catch and diagnose memory errors at runtime like invalid alignment and access violations. Other upsides includes the possibility to serialize and deserialize program runtime state, even migrating or duplicating running programs across computers. Mapping I/O onto memory addresses is another possibility.


## Address translation

Assuming a page size of 4096 and 48-bit address space.
We need 12 bits for the offset (kVFNBits=12=log2(4096)),
which in turn means that we need 36 bits (48-12) for the page number (VFN.)
Allocating one big array the size of 2^36 would be crazy; 64 GiB!
So instead we use a hierarchy of page tables allocated as needed,
splitting up the VFN space.

Each page table should ideally be sized at an even multiple of the page size.
This leaves us with the following set of constraints:

- offset must be 12 bits
- VFN must be 36 total spread over n page tables
- each page table should be a multiple of 4096

A few possible solutions:

    LEVELS  TABLE BITS     TABLE SIZE (#PTEs)
    2       18+18              262144
    3       12+12+12             4096
    4       9+9+9+9               512
    4       12+8+8+8         4096,512
    6       6+6+6+6+6+6            64
    ...

We want to choose a balance between few levels and small tables.
Further, our implementation will be simpler with symmetric tables; same size.
Fewer levels means faster lookup. Smaller tables means less memory needed on average.
The first page table is always allocated and never swapped (part the page directory.)
Lets say sizeof(PTE)=64B, then a two-level approach means the page directory is huge:
16 MB (262144×64). And this is per process.
Amd64 uses 4 levels, 9 bit per page table with a pagesize of 4096.
That's what we'll be using too.


### Page table layout

We need kVFNBits bits to represent all possible VFNs.
Examples using 4096 B page size:

    32-bit address space needs 20 bits to index          1048576 VFNs (  4 GiB.)
    48-bit address space needs 36 bits to index      68719476736 VFNs (256 TiB.)
    52-bit address space needs 40 bits to index    1099511627776 VFNs (  4 PiB.)
    64-bit address space needs 52 bits to index 4503599630000000 VFNs ( 16 ZiB.)



### Address translation example

Let's explore translating the address 0xdeadbeef (little endian.)

First we split up the address into its Virtual Frame Number (VFN) and offset.

    addr    0xdeadbeef  11011110101011011011111011101111  ()
    ------  ----------  --------------------------------  ---------------
    VFN     0xdeadb     11011110101011011011              (addr >> 12)
    offset  0xeef                           111011101111  (addr & 4096-1)

Next we traverse the page table hierarchy using the VFN: (9 bits per table)

          ┌─────────────────┬─────────────────┬─────────────────┬─────────────────┐
    bit#  │                 │1 1 1 1 1 1 1 1 1│1 2 2 2 2 2 2 2 2│2 2 3 3 3 3 3 3 3│
          │1 2 3 4 5 6 7 8 9│0 1 2 3 4 5 6 7 8│9 0 1 2 3 4 5 6 7│8 9 0 1 2 3 4 5 6│
          ├─────────────────┼─────────────────┼─────────────────┼─────────────────┤
    VFN   │0 0 0 0 0 0 0 0 0│0 0 0 0 0 0 0 1 1│0 1 1 1 1 0 1 0 1│0 1 1 0 1 1 0 1 1│
          │        0        │        3        │       245       │       219       │
          └─────────────────┴─────────────────┴─────────────────┴─────────────────┘
          ┌────────────┐    ┌────────────┐    ┌────────────┐    ┌────────────┐
          │ PT1        │    │ PT2        │    │ PT3        │    │ PT4        │
    addr  │ 148600000  │    │ 148601000  │    │ 148602000  │    │ 148603000  │
          ├────────────┤    ├────────────┤    ├────────────┤    ├────────────┤
          │   0 148601 ━━━━▶│ ...        │ ┏━▶│ ...        │ ┏━▶│ ...        │
          │ ...        │    │   3 148602 ━━┛  │ 245 148603 ━━┛  │ 219 106800 ━━▶ page
          └────────────┘    └────────────┘    └────────────┘    └────────────┘
          512 GB regions     1 GB regions      2 MB regions      4 kB regions

We now know what host address 0xdeadbeef maps to: 0x106800eef

    page_addr = page << 12         │ 0x106800 << 12 = 0x106800000
    host_addr = page_addr + offset │ 0x106800000 + 0xeef = 0x106800eef


### Page Table Entry

With 4096B pages, we need 52 bits for host-page addresses (addr >> 12 (12=log2(4096)).)
In practice we could use fewer bits as the host is likely only using 48 or 52 bits of
address space, not the full 64 bits. But, to make the implementation simpler,
we assume 64 bits (i.e 52 bits of the PTE, for page addressing.)
Each page table is allocated in an even number of host pages, making it possible
to store the PTE "value" (address of the next table or page) in 52 bits.
Making each PTE 8 bytes (64 bits total) with 4 levels page tables means that
each page table is exactly one 4096 page large: (2^9=512) * 8 = 4096. Nice.

          ┌─────────────────────────────────────────────────────────────────────┐
          │                          Page Table Entry                           │
          ├──────────────┬──────────────────────────────────────────────────────┤
    bit#  │ 1…12         │ 13…64                                                │ MSB
          │ ............ │ .................................................... │
    use   │ metadata     │ output address                                       │
          └──────────────┴──────────────────────────────────────────────────────┘


### Page table B-tree illustration

The page table data structure is essentially a B-tree, indexed by VFN (page address.)
Each page table entry holds a pointer to the next table or leaf along with metadata.
In this diagram, VFNs 0x1, 0x200 and 0x8000000 are mapped to a host page.
I.e. addresses 0x1000, 0x200000 and 0x8000000000.

                 ┌───────────┬───────────┬─────┬───────────┬───────────┐
           root  │ 000000000 │ 008000000 │ ··· │ ff0000000 │ ff8000000 │
                 └─────╻─────┴─────╻─────┴─────┴─────◯─────┴─────◯─────┘
          ┏━━━━━━━━━━━━┛           ┗━━━━━━━━━━━━┓
          ┡━━━━━━━━━┯━━━━━━━━━┯━━━━━┯━━━━━━━━━┑ ┡━━━━━━━━━┯━━━━━━━━━┯━━━━━┯━━━━━━━━━┑
          │ 0000000 │ 0040000 │ ··· │ 7fc0000 │ │ 8000000 │ 8040000 │ ··· │ ffc0000 │ …
          └────╻────┴────◯────┴─────┴────◯────┘ └────╻────┴────◯────┴─────┴────◯────┘
       ┏━━━━━━━┛                              ┏━━━━━━┛
       ┡━━━━━━━┯━━━━━━━┯━━━━━┯━━━━━━━┑        ┡━━━━━━━━━┯━━━━━┯━━━━━━━━━┑
       │ 00000 │ 00200 │ ··· │ 3fe00 │        │ 8000000 │ ··· │ 803fe00 │ …
       └───╻───┴───╻───┴─────┴───◯───┘        └────╻────┴─────┴────◯────┘
    ┏━━━━━━┛       ┗━━━━━━━━━━┓                    ┗━━━━━━┓
    ┡━━━━━┯━━━━━┯━━━━━┯━━━━━┑ ┡━━━━━┯━━━━━┯━━━━━┯━━━━━┑   ┡━━━━━━━━━┯━━━━━┯━━━━━━━━━┑
    │ 000 │ 001 │ ··· │ 1ff │ │ 200 │ 201 │ ··· │ 3ff │   │ 8000000 │ ··· │ 80001ff │ …
    └──◯──┴──╻──┴─────┴──◯──┘ └──╻──┴──◯──┴─────┴──◯──┘   └────╻────┴─────┴────◯────┘
          ┌──┸────────┐       ┌──┸────────┐                 ┌──┸────────┐
          │ host page │       │ host page │                 │ host page │
          └───────────┘       └───────────┘                 └───────────┘

Empty cirlce "◯" signifies a NULL pointer, meaning that entry is not mapped.
In practice, the page address 0x0 is used, offset by the minimum address (no waste.)



## References

- "The Adaptive Radix Tree: ARTful Indexing for Main-Memory Databases"<br>
  Viktor Leis, Alfons Kemper, Thomas Neumann

- "Some Efficient Techniques for Simulating Memory"<br>
  Peter Magnusson, Bengt Werner

- "Operating Systems: Three Easy Pieces", chapter 20 "Advanced Page Tables"<br>
  Remzi H. Arpaci-Dusseau, Andrea C. Arpaci-Dusseau
  https://pages.cs.wisc.edu/~remzi/OSTEP/vm-smalltables.pdf

- "Introduction to Virtual Memory"<br>
  Burt Rosenberg (2009)
  https://cs.miami.edu/home/burt/learning/Csc521.101/notes/virtual-memory-notes.html

- "FreeBSD Architecture Handbook", chapter 7 "Virtual Memory System"<br>
  https://docs.freebsd.org/en/books/arch-handbook/vm/

- "Code Tour: Kernel virtual memory system in SerenityOS"<br>
  https://youtube.com/watch?v=3xgOybGlYes

- "Paging" in the OS Dev wiki (x86)<br>
  https://wiki.osdev.org/Paging

- "Notes on x86_64 Linux Memory Management Part 1: Memory Addressing"<br>
  Jason Cai
  https://jasoncc.github.io/kernel/jasonc-mm-x86.html

- "AArch64 MMU Programming"<br>
  Ilya Kartashov
  https://lowenware.com/blog/osdev/aarch64-mmu-programming/

- "x86 Paging Tutorial"<br>
  Ciro Santilli
  https://cirosantilli.com/x86-paging

- paging in bochs emulator (bochs/cpu/paging.cc@b8f38eb8d3a9a3dd57a)<br>
  https://github.com/stlintel/Bochs

- "Hypervisor From Scratch – Part 4: Address Translation Using Extended Page Table (EPT)"<br>
  Mohammad Sina Karvandi
  https://rayanfam.com/topics/hypervisor-from-scratch-part-4/

- "PAGE REPLACEMENT IN VMSIM"<br>
  Ananya R, Chirag Jamadagni (2015)


## Common terminology

Common terminology found across literature and implementations

- Page — Region of memory. Usually 4096, 8192 or 16384 bytes
- Page Frame — Same as a Page but when talking about physical memory. Same size as a Page.
- Page Table — Per virtual address space table mapping virtual addresses to Pages.
- Page Directory — Like Page Table and is often the same thing. The first level of Page Tables.
- PTE - Page Table Entry — A "record" or "row" of a Page Table
- VFN - Virtual Frame Number — Index of a Page (aka frame?) in the context of a Page Table
- PFN - Physical Frame Number — Index of a physical memory frame/page in the context of the host
- TLB - Translation Lookaside Buffer — Virtaddr-to-hostaddr cache. Used for avoiding expensive Page Table lookups.
