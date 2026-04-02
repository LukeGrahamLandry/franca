# Franca

A self sufficient programming language.  

## Features

The main gimmick is full compile-time code execution: anything you can do at runtime, you can do at comptime. 
Comptime code doesn't run in an interpreter. It's JITted to machine code by the same backend as the rest of your program. 
Supporting that while also allowing cross compilation and reproducible builds is... nontrivial, but it mostly works. 
Comptime code can dynamically allocate memory, make syscalls, and generate code. 
(approx.) Any data structures that are reachable from 
runtime code will automatically be included in the final binary. 

- macros are functions that run at compile time, call compiler apis, and return ast nodes
- hygenic quasiquote to generate code in macros
- types are values at compile time so generics are just functions 
- static multiple dispatch overload resolution
- lazy analysis, functions/constants that are statically unreachable may contain invalid code 
- nonlocal returns from (non-escaping) inline lambdas so you can define your own control structures
- manual memory management with explicit allocators 
- no seperate build system language. write a program that builds your program
- full c abi support so you can call extern-c code written in other languages
  - (limitations: no f16,f80,f128,i128,bitfields,tls,stack switching)

## Supported Targets

The self-hosted backend generates machine code for arm64 (aarch64), amd64 (x86_64), riscv64, and wasm32. 
There is no dependency on assemblers, linkers, llvm, or xcode-codesign. 

- mach-o (macos), elf (linux)
- executables, dynamic libraries, relocatable object files
- jit for comptime execution 

On windows you can use WSL. 

The compiler does not depend on libc (on linux, when built with -syscalls). 

The webassembly target is still a work in progress (doesn't follow c abi, poor codegen, etc), 
but the compiler can (slowly) compile itself. 

## Nontrivial Example Programs

[![](https://builds.sr.ht/~lukegrahamlandry/franca.svg)](https://builds.sr.ht/~lukegrahamlandry/franca)
[![](https://github.com/LukeGrahamLandry/franca/actions/workflows/test.yml/badge.svg)](https://github.com/LukeGrahamLandry/franca/actions/workflows/test.yml)

> The following philosophical objections apply: The most significant program written in your language is its own compiler.

- The franca compiler itself is written in franca [@/compiler](./compiler)
- Lightly optimising codegen backend [@/backend](./backend) (based on Qbe)
- C compiler (using that backend^) [@/examples/import_c](./examples/import_c) (based on Chibicc)
  - can run at comptime so franca programs can import c libraries directly without depending on another compiler
  - compiles real programs: see tests/external/(lua, hare, bubblewrap, raylib, ...).fr
- Windowing/3d graphics library [@/graphics](./graphics) (based on Sokol)
  - very unfinished! macos-arm64-metal/webgpu only currently
- You can try the [WebAssembly demo](https://franca.lukegrahamlandry.ca) in your browser without installing anything. 
  It can even cross compile a native version of the compiler that you can download as a way to bootstrap your first franca installation. 
- The beginnings of an operating system for aarch64 [@/examples/os](./examples/os)

## Documentation

I suspect the best way to learn at this point is to just look at the examples folder, 
but some things are subtle enough that it would feel rude if I didn't write them down, 
and some things are dumb enough that I should defend myself. 
These are not polished yet but hopefully better than nothing. 

- [introduction](./docs/introduction.md): just enough syntax to be dangerous 
- [aggregates](./docs/aggregates.md): more complicated types like structs, enums, slices, etc. 
- [generics](./docs/generics.md): how to write functions that work for arbitrary types
- [compilation](./docs/compilation.md): high level overview of the franca compilation model
- [aot_bake](./docs/aot_bake.md): how to make ahead-of-time executables and control what data is included 
- [imports](./docs/imports.md): how to load new code
- [annotations](./docs/annotations.md): how to attach metadata to franca programs and query it from comptime code
- [comptime](./docs/comptime.md): example usecases for full compile time execution
- [higher_level](./docs/higher_level.md): a nonexhaustive list of features i don't have
- [debugging](./docs/debugging.md)
- [caching](./docs/caching.md): details about `.frc` files
- [lib_summary](./docs/lib_summary.md): list of useful code provided with the franca distribution 

## Getting Started

There's no global "install" process, you just need the compiler binary and the code in this repository.  

```
git clone https://git.sr.ht/~lukegrahamlandry/franca && cd franca && sh ./boot/strap.sh -no-test
```

That will download the source and an old version of the compiler, use it to compile 
the new compiler (a few times until it stabilizes). Alternatively, you can download 
prebuilt binaries from <https://fr.lukegrahamlandry.ca> 
(only the latest so if you're automating it, prefer to bootstrap 
and pin to a specific commit to avoid breaking changes). 

To run the tests, remove the `-no-test` argument or (once you have the compiler) run 
```
./target/franca.out ./tests/run_tests.fr core
```
The tests have more system dependencies (mainly: libc, clang, curl, git) than the compiler itself. 
See tests/external/deps.fr for source that will be curl-ed for testing import_c. 
All downloads have their hash checked and are saved in target/franca/fetch so network is only needed the first time. 

## Goals

- make a language stable enough to use for future projects
- keep it small enough to be maintained by one person
- learn as much as possible, which means not leaning on tools/libraries written by someone else
- be fast enough that you never spend time waiting  
  (currently unacceptable; it takes a whole second to compile itself)

Just to be clear, I don't know any information. None of the things ending up doing in this project were things I knew how to do or even knew i wanted to do. 
This is the first project I've worked on that is big enough that the limiting factor is how much I can remember about code I wrote a long time ago. 

But it turns out you can google the cpu spec and then type in the bits and then suddenly you have a jit compiler that like works and stuff. 
And when you want to write an optimiser or a c compiler or a graphics library you can just google ones people have done before 
and you can just type in the code they typed in and convert it line by line and change things that don't make sense 
(and then change them back when you realize the first person knew what they were doing), 
and at the end of that process you'll understand a new thing. 

I make no claim that this code is good or robust or fit for a particular purpose. 
My only claim is that if i work on it for four hours a day, every day, it will get better every day, 
and after a year of that it turns out it's like really a lot better than i ever imagined it would be. 

Whenever there's a choice between learning something and being useful, I'd rather do the learning one. 
The output I care about here is not this specific program being the best it can be, 
the goal is (my ability to make cool things go up) and rate at which (my ability to make cool things) go up, go up. 
The pattern I've seen is that future me is invariably better equipped to make the right choices than current me. 

> It's not symmetrical or perfect; But it's beautiful and it's mine; What else can I do?

## Tradeoffs

I strongly believe in conservation of misery so if there's a bunch of stuff I like about this language
there must be proportional terrible things or I'm probably just lying.

- Don't compile programs you don't trust. comptime code is arbitrary code execution!
  it can use inline assembly! it can dlopen libc and `system("curl | bash")`!
  treat compiling a program with the same discretion as running it!
- Stability is not a core tenet. If it was worth writing once, it's probably worth writing twice.
- I don't care about being easy to learn. I want it to be fun. Rust is fun.
- Stuff's unsafe, deal with it, simply don't make a mistake. 
  - i.e. `main :: fn() void = { evil := i64.ptr_from_int(-123); println(evil[]); }` will compile, run, and crash.
  - You can screw up managing pointers because the languge was made by goblins in the 70s and just start trimming your os.
- I don't care about fallibile memory allocation
- I believe I've never touched a big endian computer so I don't care.
- I do unaligned memory accesses (again, i believe i've never touched a computer that disallows them)
- Pointers are 64 bits. Wasm can just use twice as much memory as it should, not my problem, tho it irks me.
- It's probably not a good fit for large teams where you want the language to help you design your whole program defensively because you don't trust your coworkers.
- The error messages are completely incomprehensible.
- I don't have nice IDE integration.
- I don't have a nice debug mode that detects undefined behaviour (overflow, wrong tagged field, etc).
- The compiler does an insane amount of redundant work.
  Like sometimes it reparses and re-resolves names for each specialization of a generic.
  There just happens to only be ~90k lines of code ever written in this langauge so its not a big deal yet.
  See examples/60fps.fr for a fun speed test.
- There's some hoops you need to jump through when you want conditional compilation on different platforms 
  (because I care about cross compiling working seamlessly). 
- It has only been used by (approximately) one person so I'm sure there are hella compiler bugs that I haven't stumbled upon yet. 
  Every time i write a new program the todo.md file gets longer. 
- spawning new operating system threads from comptime code is a bit fragile. 
  it does work but you're exposed to some unfortunate compiler implementation details. 
  (re. jit-shims and MAP_JIT). hopefully this will be fixed eventually. 
- I've neglected language design / frontend quality-of-life things 
  because i find that less interesting than playing with code generation and ffi stuff. 
  So there's some unpleasent stuff (order dependent overload set instantiations) 
  that could be fixed by unfinished features (#where) that I haven't bothered to work on. 
- Similarly, there are some known bugs (see tests/todo/) that I don't care enough to fix until they annoy me (or someone else) more. 
- No source level debugging
