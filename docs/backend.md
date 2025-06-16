# notes about `@/backend`

See backend/README.md for some info:
- jit/aot codegen for arm64/amd64 machO/elf
- platform independent ssa cfg ir
- extern-c abi, stack slot promotion, inlining, dead code elimination, constant folding, reg alloc
- adapted from qbe, changed to output machine code without assembler/linker/codesign dependency
- fast enough to power franca's compile time execution 
- examples: import_c (C11 frontend), import_wasm, prospero.fr, kaleidoscope.fr

## architecture

Program is input as an infinite-registers control-flow-graph intermediate representation 
(like llvm-ir but allows mutable variables) and then lowered to machine code through 
many small passes. Only abi/isel/emit passes need to be rewritten for each target platform. 
Same IR data structure is used throughout the whole compilation. 
The input is allowed mutable variables, gets converted to ssa with phi nodes, various 
optimisations, registers allocated based on isa limitations, then output machine code. 

## just-in-time compilation

I don't do anything cool about tiering, tracing, dropping out into an interpreter, etc. 
that fancy JITs do, I just mean that it outputs machine code in memory and then 
jumps there and starts executing directly WITHOUT outputting binaries and going 
through exec/dlopen. However, I want it to be flexible enough to be a base library 
for those more advanced techniques. For example, you can add new functions to a jitted 
module after it's started executing (see anything called "shim" in `@/compiler/jit.fr` 
for examples). 

## finding imported symbols

C compilers want to think of symbols as existing in a flat namespace, 
but exe formats want you to tell it which library to look in to find 
each imported symbol. In general the linker is responsable for papering 
over that difference, so if you're fine with that dependency you can just 
use the Relocatable output format and everything should "just work" like 
it would in C. Otherwise, you need to supply the library name along with 
the symbol name for any imports. Ideally, the library name would be in the form 
of a file path that is different for each target platform and the frontend would 
be resoponsible for handling that but currently the backend has special 
support for handling macOS frameworks hacked in. 

For jitting, we don't do anything special to help with imports. 
The frontend is expected to call dlopen or whatever and just pass 
the function pointers to the backend. 

- macho: ChainedImport has a lib_ordinal index into LinkLibrary load commands
- elf: ?
- wasm: Import has a module name

## notes

- We're very often iterating backwards so `emit` inserts into the block buffer backwards!
  For now we use the buffer in Qbe.Globals but that will move out eventually.
- When looking at the debug dumps, register numbers are off by one. ie. first argument on arm (x0) is in R1.
- When trying to understand what phi instructions do, remember that they're totally isomorphic to block arguments and then it's suddenly super simple.
  It's just a choice to represent it as the callee knowing which value to use for each argument for each caller.
- amd64, x86_64, and x64 are the same thing. x86 means the 32 bit one but nobody cares about that anymore so sometimes that's the same thing too.
- arm64 and aarch64 are the same thing. the latter is what arm calls it but that's such an annoying word.
- If you use llvm you don't get full c-abi compatibility for free.
For example, here's where rustc does it in the frontend https://github.com/rust-lang/rust/blob/master/compiler/rustc_target/src/abi/call/mod.rs
