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

## Instructions

A block contains a flat sequence of instructions. 
Each instruction is stored as (operation: O, class: Cls, destination: Ref, arg0: Ref, arg1: Ref).
The class is what data type will be output by the instruction: i32 (Kw), i64 (Kl), f32 (Ks), or f64 (Kd). 
Many instructions are valid for multiple classes. When instructions do not have an output (ie. store), use Kw. 
Some operations require multi-instruction sequences to supply extra arguments. 

- integer overflow is wrapping
- signed integers are two's compliment so there are no seperate instrucitons for signed vs unsigned add/sub/mul. 
  div/mod have udiv/umod varients that act on unsigned values. 
- add/sub/mul/div/neg act on integers or floats (depending on class)
- integer division can trap on x86_64 
- floats are IEEE 754 
- shifts (shl, shr, sar, rotr, rotl) are done modulo the data size. so `(.shl, .Kw, dest, x, 33) == (.shl, .Kw, dest, x, 1)`
  - shl/shr (logical-shift left/right): new bits are 0
  - shl (arithmetic-shift right): sign extension. new bits are the same as the original sign bit
  - rotr/rotl (rotate right/left)
- neg is a unary instruciton equivilent to subtracting its argument from zero
- there is no instruction for bitwise not. instead use xor with -1. 
- min/max/sqrt are only implemented for floats
- unary bit manipulation: byteswap, ctz (count trailing zeros), clz (count leading zeros), ones (count ones / popcnt)
- store b/h/w/l/s/d. 
  - a0 is the value, a1 is the destination address (note: BACKWARDS). no result. 
- load works for any class. for smaller loads, there's an instruction corresponding to each extension instruction.
- alloc reserves space on the call stack. it goes away when the function returns. 
  - there are different instructions for different byte alignments (4/8/16). 
  - the argument is the size in bytes. 
  - (note: since temporaries are mutable, frontends are not required to alloc for variables if their address is not taken), 
- dbgloc

### Comparison 

Note that the class is the output type which will always be an integer (boolean 0 or 1). 
There is are seperate instructions for each class of operand. 
Integers have signed and unsigned varients. 
Float comparisons return false when either argument is NaN, except for uo which returns true if either argument is nan. 

- integer: eq, ne, sge, sgt, sle, slt, uge, utl, ult, ult. 
- float: eq, ge, gt, le, lt, ne, o, uo

### Casts

- extension: signed/unsigned b(8)/h(16)/w(32). 
  - the unsigned varient zeroes the high bits. the signed varient duplicates the sign bit. 
- exts/truncd convert between float sizes. f32->f64, f64->f32
- float<->integer have signed/unsigned varients. 
  - stosi/stoui/dtosi/dtoui/swtof/uwtof/sltof/ultof
- cast preserves bits (NOT value) and changes between int<->float (in/out must be the same size)
- copy must be int<->int or float<->float

### Three argument instructions 

- blit: implements memcpy (of constant size)
  - blit0: a0 is source, a1 is destination (note: BACKWARDS from memcpy). blit1: a0 is size in bytes.
- atomic compare and swap. 
  - integers only, 4/8 bytes depending on class. 
  - TODO: you have to do your own fences which makes it kinda useless 
  - cas0: a0 is pointer. cas1: a0 is old, a1 is new, result is the previous value at the memory location
- conditional select (like the ternary operator in c: `cond ? a0 : a1`)
  - sel0: a0 is the condition. sel1: result is a0 if true, a1 if false 

### Call

The call sequence is the only place in the ir that needs more detailed type information. 
What's required is just enough to implement the abi correctly. 
The backend does not do any type checking between calls and a function's declaration. 

- arg/par 
- arg/par have sb/ub/sh/uh variants for smaller integers
- argc/parc agragate (struct/union) parameter
- arge/pare secret environment parameter
- argv
- vastart
- vaarg

return value from call:
- void
- scalar
- aggregate

- frontends are NOT required to insert extra copies for arguments/returns. 
  the backend will add them as needed to conform to the target system abi. 
  if you pass a pointer as argc and the callee mutates its parameter, the caller will not see the change. 

## Phi

stored seperatly from instructions. 
optional: frontends can use mutable temporaries instead and the backend will handle converting them to phis. 
