# Franca

A self sufficient programming language.  

## Goals

- make a language stable enough to use for future projects
- keep it small enough to be maintained by one person
- learn as much as possible, which means not leaning on tools/libraries written by someone else
- be fast enough that you never spend time waiting  
  (currently unacceptable; it takes a whole second to compile itself)

## Features

- full compile-time code execution: anything you can do at runtime, you can do at comptime 
- macros are functions that run at compile time, call compiler apis, and return ast nodes
- hygenic quasiquote to generate code in macros
- types are values at compile time so generics are just functions 
- static multiple dispatch overload resolution
- lazy analysis, functions/constants that are statically unreachable may contain invalid code 
- nonlocal returns from (non-escaping) inline lambdas so you can define your own control structures
- manual memory management with explicit allocators 
- no seperate build system language. write a program that builds your program
- cross compilation and reproducible builds
- full c abi support so you can use extern-c code written in other languages

## Supported Targets

The self-hosted backend generates native machine code for arm64 (aarch64) and amd64 (x86_64). 
There is no dependency on assemblers, linkers, llvm, or xcode-codesign. 

- mach-o (macos): executables, dynamic libraries, relocatable object files
- elf (linux): executables only for now
- jit: for comptime execution 

All the tests pass on macos-arm64 and macos-amd64.  
On linux-amd64, the compiler can compile itself but not all tests pass.  
(I haven't transcribed all the platform specific struct layouts/magic numbers yet). 
On windows you can use WSL. 

The compiler does not depend on libc (on linux, when built with -syscalls). 

A bootstrap binary is committed for macos-arm64 only. 
Binaries for other platforms are available as [github actions artifacts](https://github.com/LukeGrahamLandry/franca/actions).

> There is vauge work towards targetting wasm and llvm, but it is not yet usable. 

## Example Programs

> The following philosophical objections apply: The most significant program written in your language is its own compiler. 

- The franca compiler itself is written in franca ([./compiler])
- Lightly optimising multi-platform codegen backend ([./backend]) (based on Qbe)
- A C11 compiler good enough to compile the lua interpreter ([./examples/import_c]) (based on Chibicc)
- Comptime code generation demos ([./examples/bf])
- Simple graphics (`./examples/{farm_game.fr, edit.fr}`) (using Sokol, DearImGui)
- Boring obj-dump-like utilities (`./examples/{dump_macho.fr, dump_elf.fr, dump_wasm.fr}`)
- Animated ascii-art mandelbrot set by recompiling every frame ([./examples/60fps.fr])

## Tradeoffs

I strongly believe in conservation of misery so if there's a bunch of stuff I like about this language
there must be proportional terrible things or I'm probably just lying.

- Don't compile programs you don't trust. comptime code is arbitrary code execution!
  it can use inline assembly! it can dlopen libc and `system("curl | bash")`!
  treat compiling a program with the same discretion as running it!
- Stability is not a core tenet. If it was worth writing once, it's probably worth writing twice.
- I don't care about being easy to learn. I want it to be fun. Rust is fun.
- Stuff's unsafe, deal with it, simply don't make a mistake. i.e. `fn main() = { evil := i64.int_to_ptr(123); println(evil[]);` will compile, run, and (probably) crash.
  - You can screw up managing pointers because the languge was made by goblins in the 70s and just start trimming your os.
- I don't care about fallibile memory allocation
- It's a little heavy on the punctuation.
- I believe I've never touched a big endian computer so I don't care.
- Pointers are 64 bits. Wasm can just use twice as much memory as it should, not my problem, tho it irks me.
- It's probably not a good fit for large teams where you want the language to help you design your whole program defensively because you don't trust your coworkers.
- Sadly the error messages are completely incomprehensible.
- Sadly I don't have nice IDE integration.
- Sadly I don't have incremental builds. every time you run a program, you recompile the standard library for comptime. (...but its so fast it doesn't matter yet).
- Sadly I don't have a nice debug mode that detects undefined behaviour (overflow, wrong tagged field, etc).
- The compiler does an insane amount of redundant work.
  Like sometimes it reparses and re-resolves names for each specialization of a generic.
  There just happens to only be ~40k lines of code ever written in this langauge so its not a big deal yet.
  See examples/60fps.fr before judging its performace.
