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
  - WIP
- Kaleidoscope (../examples/kaleidoscope.fr)
- BrainFuck    (../examples/bf/bf2ir.fr)

## Changes from Qbe

### Targets

- Generate machine code directly without depending on an external assembler (arm clang is so slow!).
  Your program will contain only the finest organic bit patterns lovingly transcribed from the Arm Architecture Reference Manual
  (or painfully scavenged from whatever amd tables i can find).
  self contained instruction encoding for arm64 and amd64. 
- Jit compile your program and run it in memory without needing to emit/link an executable.
  You can freely call between jit and aot code (even extern-c code from other compilers) because they follow the same standard abi.
  (This is what Franca uses for compile-time execution).
- Emit Mach-O executables directly without depending on an external linker or (relocatable or dynamic) libraries for linking with other languages.
- Ad-hoc signetures for Mach-O binaries so you can target macOS without depending on Apple's `codesign` program.
- Added a Web Assembly target (outputs the binary format directly).
  - WIP: the .ssa tests pass in my import_wasm but don't typecheck in real wasm runtimes
  - WIP: does not correctly follow the extern-c abi
- Emit Elf executables (and relocatable libraries) directly
  - WIP: I can't do dynamic libraries yet and exes rely on franca_runtime_init
- Removed the RISC-V target for now
  - WIP: started adding it back but instruction encoding is very unfinished
  
### Features

- A library interface for producing and compiling ir in memory instead of outputting text and exec-ing a seperate program to process it.
  (in my brief profiling qbe -O2, it spends 40% of its time parsing the input text. i don't know how that's possible, maybe apple's fgetc is slow).
- Added some instructions
  - bit manipulation: byte swap, rotate left/right, count trailing/leading zeros, count ones
  - floats: sqrt, min, max
  - atomic compare-and-swap 
    - a bit useless because you need to insert your own fences
  - conditional select (ie. `a ? b : c`)
  - walk the call-stack (ie. for printing crash backtraces)
- Binary form of the ir that's faster to load than the text form (could be a building block for incremental compilation)
- Insert raw asm bytes in function bodies with arbitrary input/output/clobber register lists.
  > currently very limited: you pick specific registers, do your own assembling, can't reference symbols.  
  > but it lets you access instructions that we don't know about without calling convention spilling overhead.  
  > example use case: a c compiler implementing `__builtin_clzl` as a single instruction.
  > (which is a dumb example because i have a clz instruction). or @/tests/exe/sys.fr uses it to make syscalls. 
  > (not well tested because franca doesn't use it yet but it will likely be converted eventually).
- binary serialization format of the ir that can be used as a building block for incremental compilation. (see @/docs/caching.md)
- Removed support for thread locals. Franca achieves an equivilent by passing around an implicit env parameter.
- Removed support for custom section names.

#### fixed upstream

- arm abi: respect the platform register (it gets zeroed when you context switch on macos) [issue](https://lists.sr.ht/~mpu/qbe/%3CCAHT_M7NPc_vufQ7hj+JwdB2cVrZKOmKmRk2z8ETLJ4T9=25YRw@mail.gmail.com%3E)
- arm abi: large FHA (ie. struct of 4 doubles) [issue](https://lists.sr.ht/~mpu/qbe/%3CCAHT_M7Pp-6_vSjOd-WkRt4ACJWLrKq=YpgUrnzW0Vy=T-7AFYg@mail.gmail.com%3E)

### Optimisations

- added peephole optimisations for arm isel: ~40% code size reduction vs Qbe on the franca compiler.
  - fold address computation chains
  - use small immediates for instructions that allow them. 
    - u12: add/sub, ldr/str
    - bit fields: shl/shr/sar/rotr, and/or/xor
  - fuse mul followed by add into 3 arg madd.
  - fuse ldr+ldr/str+str pairs into ldp/stp
- Inline small (single block) functions.
  - based on <https://lists.sr.ht/~mpu/qbe/patches/55945>
- elide some redundant memory operations introduced when the abi passes aggregates in registers. (see opt/slots.fr)
- reuse constants that are produced multiple times in the same basic block (but not across calls) 
- simplified matching of amd64 addressing modes: instead of 1600 lines of ocaml implementing a dsl
  with 150 lines of glue code to use the results, just write 150 lines of tedious code to solve the problem directly.
- Strength reduction for signed div/rem by power of two (using a conditional move to get the right rounding for negative numbers). 
- Removed several codegen optimisations until we have a solid foundation (but I want to bring them back eventually).
  - all: load float constants from memory (instead of using int immediate + fmov)
  - amd: negating floats by xoring from memory
  - amd: use spilled floats directly from memory

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
