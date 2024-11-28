> If I have seen further, it is by standing on the shoulders of giants,
> But I have not seen further, I mostly just fall off thier shoulders.

## Code

Franca includes code from ported other (permissively licenced) projects.  
Individual files have license headers that reference thier original authors.

- Qbe: a hobby-scale optimising compiler backend.
  - <https://c9x.me/compile>
  - MIT License. © 2015-2024 Quentin Carbonneaux <quentin@c9x.me>
  - <https://c9x.me/git/qbe.git/tree/?id=626f0b278137ff6f8b7d910d9b3fc3cbdfbb39fc>
  - <https://lists.sr.ht/~mpu/qbe/patches/54822>
  - see `backend/README.md`
- Zig: a general-purpose programming language and toolchain for maintaining robust, optimal and reusable software.
  - <https://ziglang.org>
  - MIT License. Copyright (c) Zig contributors.
  - Mach-O: packing an export trie.
- Wikipedia: the free encyclopedia
  - Great for when you want to waste your time with lies about calling conventions
  - sha256

## Resources

### x64

- <https://wiki.osdev.org/X86-64_Instruction_Encoding>
- <http://ref.x86asm.net/coder64.html>
- <https://www.felixcloutier.com/x86>

### arm

- <https://developer.arm.com/documentation/dui0802/b/A64-General-Instructions/A64-general-instructions-in-alphabetical-order>
- <https://devblogs.microsoft.com/oldnewthing/20220726-00/?p=106898> (series of like 25)

### mach-o

- <https://github.com/qyang-nj/llios/tree/main/macho_parser/docs> (and main/dynamic_linking/chained_fixups.md)
- <https://llvm.org/doxygen/BinaryFormat_2MachO_8h_source.html>
- <https://github.com/apple-oss-distributions/xnu/blob/main/EXTERNAL_HEADERS/mach-o/loader.h>
- <https://github.com/horsicq/XMachOViewer> a gui that lets you poke around in memory
