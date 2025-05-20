Trying to keep track of things I stole and exposure to external entropy. 

## Ports

This is code that has been ported to franca, heavily refactored to fit my needs, 
and lives in this repository now. 

Big Subprojects: (see individual readme for changes)

- backend / qbe (opts, abi, isel, ir, .ssa tests)
- import_c / chibicc (tokenize, preprocess, parse, .c tests)
- graphics / sokol (app, gfx, debugtext, gl)

Smaller Things: 

- sha256 from wikipedia
- mach-o exports tree from zig

Examples:

- kalidescope based on llvm tutorial
- lox based on craftinginterpreters book

## Examples

- stb (truetype, image_write)
- jetbrains mono (font)
- .laz files for examples/geo
  - this is problematic. i should generate them. 

## Testing

Some of the test programs use data that i download at compile time. 

- `examples/prospero` (.vm models) from fidget
- `examples/lox` (.lox test programs) from craftinginterpreters
- import_c needs some non-trivial c programs to test
  - lua
  - wuffs

## Linker, c++, etc

These are problematic

- dawn and a c++ compiler
- llvm-mc for #log_asm
- dearimgui and a c++ compiler
- tracy and a c++ compiler

## Dynamic

- loader
- on macos you pretty much need to link thier libc
- linux syscalls
- graphics drivers

## Bindings

- webgpu
- macos frameworks
- dearimgui
- libc

## Magic Numbers

With binary formats there's really no winning, I have to read the spec and type the numbers 
into my program. So I didn't invent the numbers but it's probably socially acceptable. 
They tend to be quarantined in a file called `bits.fr`. 

- instructions: arm64, amd64, wasm
- object files: mach-o, elf
- linux syscall numbers

## Boot

problematic because it bloats the repo every time i need to change it 
and i only commit one for macos. 
