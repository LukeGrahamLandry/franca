# Franca

> Self Hosted!

A programing language understandable by one person.
That may be interpreted as "any person will be able to understand it" or "one specific person is able to understand it", at your option.

If you just want to see what the syntax looks like, click on examples or compiler or lib or tests, really anywhere, just click a folder.

## Features

- all code can run at compile time (like nim/jai)
- macros are functions that run at compile time, call compiler apis, and return ast nodes (like swift/nim)
- hygenic quasiquote to generate code in macros (like scheme)
- types are values at compile time so generics are just functions (like zig)
- multiple dispatch overload resolution (like julia)
- no blessed methods, call any function with dot syntax (like d/nim)
- lazy analysis, functions/constants that are statically unreachable may contain invalid code (like zig)
- nonlocal returns from inline lambdas (like kotlin/ruby)
- manual memory management with explicit allocators (like zig)
- no seperate build system language. write a program that builds your program (like zig/jai)
- custom asm backend for quick debug builds, optional llvm backend for optimised release builds (like zig/jai)
- ships with [sokol](https://github.com/floooh/sokol) bindings so its not a research exercise if you just want to put a triangle on the screen.
- cross compilation and reproducible builds. all supported targets produce identical llvm-ir for all supported targets.

## Backends

- **aarch64 machine code**: i put the bytes in memory, i mark it executable, and i jump there. no assembler, no linker, no problems.
  no optimisation but compiles fast. only JIT, cannot output an AOT executable.
- **x64 machine code**: (WIP) same as the above but for different computers.
- **llvm ir text**: you can feed it to clang to produce an AOT optimised build or integrate with projects written in other languages.

## Tradeoffs

I strongly believe in conservation of misery so if there's a bunch of stuff I like about this language
there must be proportional terrible things or I'm probably just lying.

- DO NOT COMPILE PROGRAMS YOU DONT TRUST. comptime code is arbitrary code execution!
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
- Sadly my implementation of the C ABI is buggy. It's good enough for the example programs tho.
  - arm: obvious problems are passing struct(u32, u32) in 2 registers instead of 1 and passing struct(f64, f64, f64, f64) in memory instead of 4 registers.
  - x64: i don't pass structs in xmm registers
  - (this will be fixed when the new backend is ready)
- Sadly I don't have a nice debug mode that detects undefined behaviour (overflow, wrong tagged field, etc).
- Sadly I don't have good modules / namespace management.
- I've only tried arm/x64 macos and x64 linux. Only arm mac works fully but all three can build the self hosted compiler.
- The compiler does an insane amount of redundant work.
  Like sometimes it reparses and re-resolves names for each specialization of a generic.
  There just happens to only be ~25k lines of code ever written in this langauge so its not a big deal yet.
  See examples/60fps.fr judging its performace.
- I can't emit my own elf files and i don't have my own linker.
  You need to have clang if you want AOT builds that interface with other languages.
  (however, franca only mach-o arm works and x64 is wip)
