# Franca

> a self hosted programming language. 

If you just want to see what the syntax looks like, click on examples or compiler or lib or tests, really anywhere, just click a folder.

## Goals

- make something i can use for future projects
- keep the language small enough to be maintained by one person
- learn as much as possible, which means not leaning on tools/libraries written by someone else
- be fast enough that you never spend time waiting

## Features

- full compile-time code execution: anything you can do at runtime, you can do at comptime
- macros are functions that run at compile time, call compiler apis, and return ast nodes
- hygenic quasiquote to generate code in macros
- types are values at compile time so generics are just functions 
- multiple dispatch overload resolution
- lazy analysis, functions/constants that are statically unreachable may contain invalid code 
- nonlocal returns from inline lambda
- manual memory management with explicit allocators 
- no seperate build system language. write a program that builds your program
- cross compilation and reproducible builds

> an ad hoc, informally-specified, bug-ridden, slow implementation of half of Common Lisp.

## Backends

- **fully self hosted**: only supports macos. generates native executables (we don't depend on an assembler, linker, or xcode-codesign).
- **llvm ir text**: you can feed it to clang to produce an AOT optimised build or integrate with projects written in other languages.

> The llvm backend doesn't fully implement the extern-c abi or the internal calling convention required by some multithreaded programs.  
> The self hosted backend will be eventually be able to output ELF programs for Linux as well.  

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
- Sadly I don't have good modules / namespace management.
- The compiler does an insane amount of redundant work.
  Like sometimes it reparses and re-resolves names for each specialization of a generic.
  There just happens to only be ~40k lines of code ever written in this langauge so its not a big deal yet.
  See examples/60fps.fr before judging its performace.
