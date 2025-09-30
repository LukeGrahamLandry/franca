An optimising compiler backend based on [Qbe](https://c9x.me/compile/).
Generated code is better than llvm -O0 and much worse than llvm -O2 (not the most specific range i realize).
It also compiles faster than even llvm -O0.
Almost everything (ir design, opt passes, isel/abi) is ported directly from Qbe, with some light editing to use a style I find less confusing.
The main changes are working towards being more usable as a standalone library
by removing dependencies on external assemblers/linkers and removing serialization steps to/from text files.
You can still print out the ir as human readable text between passes and modify it for testing.

## Example Frontends

- Franca (../compiler/emit_ir.fr)
- C11    (../examples/import_c/compile.fr)
- Qbe IR (../backend/meta/parse.fr)
- Wasm   (../examples/import_wasm/convert.fr)
- Kaleidoscope (../examples/kaleidoscope.fr)
- BrainFuck    (../examples/bf/bf2ir.fr)

## Changes from Qbe

### Targets

- Generate machine code directly without depending on an external assembler (arm clang is so slow!).
  Your program will contain only the finest organic bit patterns lovingly transcribed from the Arm Architecture Reference Manual
  (or painfully scavenged from whatever amd tables i can find).
  self contained instruction encoding for arm64/aarch64, amd64/x86_64, and riscv64. 
- Jit compile your program and run it in memory without needing to emit/link an executable.
  You can freely call between jit and aot code (even extern-c code from other compilers) because they follow the same standard abi.
  (This is what Franca uses for compile-time execution).
- Emit Mach-O or Elf executables directly, without depending on an external linker, 
  or (relocatable or dynamic) libraries for linking with other languages.
- Ad-hoc signetures for Mach-O binaries so you can target macOS without depending on Apple's `codesign` program.
- Added a Web Assembly target (outputs the binary format directly).
  - WIP: no dynamic/relocatable libraries and can't export/import globals.  
  - WIP: code quality is poor. there's lots of low hanging fruit optimisations in wasm/isel.fr.  
  - WIP: irreducible control flow is not supported. 
  - WIP: late Cached .frc is not supported. 

### Features

- A library interface for producing and compiling ir in memory instead of outputting text and exec-ing a seperate program to process it.
  (in my brief profiling qbe -O2, it spends 40% of its time parsing the input text. i don't know how that's possible, maybe apple's fgetc is slow).
- Added some instructions
  - bit manipulation: byte swap, rotate left/right, count trailing/leading zeros, count ones
  - floats: sqrt, min, max
  - atomic compare-and-swap 
    - a bit useless because you need to insert your own fences
  - conditional select (ie. `a ? b : c`)
- Binary serialization of the ir that's faster to load than the text form. 
  (could be a building block for incremental compilation, see @/docs/caching.md)
- Removed support for thread locals. Franca achieves an equivilent by passing around an implicit env parameter.
- Removed support for custom section names.

#### fixed upstream

- arm abi: respect the platform register (it gets zeroed when you context switch on macos) [issue](https://lists.sr.ht/~mpu/qbe/%3CCAHT_M7NPc_vufQ7hj+JwdB2cVrZKOmKmRk2z8ETLJ4T9=25YRw@mail.gmail.com%3E)
- arm abi: large FHA (ie. struct of 4 doubles) [issue](https://lists.sr.ht/~mpu/qbe/%3CCAHT_M7Pp-6_vSjOd-WkRt4ACJWLrKq=YpgUrnzW0Vy=T-7AFYg@mail.gmail.com%3E)

### Optimisations

- added peephole optimisations for arm isel
  - fold address computation chains
  - use small immediates for instructions that allow them. 
    - u12: add/sub, ldr/str, bit fields: shl/shr/sar/rotr, and/or/xor
  - fuse mul followed by add into 3 arg madd.
  - fuse ldr+ldr/str+str pairs into ldp/stp
- Inline small (single block) functions.
- Convert simple phi instructions into conditional select 
- elide some redundant memory operations introduced when the abi passes aggregates in registers. (see opt/slots.fr)
- reuse constants that are produced multiple times in the same basic block (but not across calls) 
- simplified matching of amd64 addressing modes: instead of 1600 lines of ocaml implementing a dsl
  with 150 lines of glue code to use the results, just write 150 lines of tedious code to solve the problem directly.
- Strength reduction for signed div/rem by power of two (using a conditional move to get the right rounding for negative numbers). 

## Qbe License

© 2015-2024 Quentin Carbonneaux <quentin@c9x.me>

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.

### Credits

> notable contributions by other people that i inherited from qbe

- [global value numbering, Roland Paterson-Jones](https://lists.sr.ht/~mpu/qbe/patches/54774)
- [single block inlining, Roland Paterson-Jones](https://lists.sr.ht/~mpu/qbe/patches/55945)
- [phi to sel ifopt, Roland Paterson-Jones](https://lists.sr.ht/~mpu/qbe/patches/55968)
- [riscv64 isel/abi, Michael Forney](https://lists.sr.ht/~mpu/qbe/patches/29328)
