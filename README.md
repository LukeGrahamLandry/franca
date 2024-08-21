# Franca

A programing language understandable by one person.
That may be interpreted as "any person will be able to understand it" or "one specific person is able to understand it", at your option.

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
- no seperate build system. write a program that builds your program (like jai)
- custom jit for quick debug builds, optional llvm backend for optimised release builds (like zig/jai)
- use c abi functions/structs with no overhead (like everything)
- ships with [sokol](https://github.com/floooh/sokol) bindings so its not a research exercise if you just want to put a triangle on the screen.

## Backends

- aarch64 machine code.
  - i put the bytes in memory, i mark it executable, and i jump there. no assembler, no linker, no problems.
- llvm

## Progress

- `src`: the sema stuff.
- `compiler`: the self hosted parser, llvm backend, and the jit used for comptime execution.
- `lib`: the standard library.
- See `tests`, and `examples` for smaller programs.

## Future Goals

- macros that derive implimentations of common operations on your types (like rust).
- parse declarations from other languages and generate ffi bindings.
- checked mode with less undefined behaviour.
- i want to produce an executable such that every byte in the file is there because i personally put it there.
- support x86-64

## Tradeoffs

I strongly believe in conservation of misery so if there's a bunch of stuff I like about this language
there must be proportional terrible things or I'm probably just lying.

- Stability is not a core tenet. If it was worth writing once, it's probably worth writing twice.
- I don't care about being easy to learn. I want it to be fun. Rust is fun.
- Stuff's unsafe, deal with it, simply don't make a mistake. i.e. `fn main() = { evil := i64.int_to_ptr(123); println(evil[]);` will compile, run, and (probably) crash.
- I don't care about fallibile memory allocation
- It's a little heavy on the punctuation.
- I believe I've never touched a big endian computer so I don't care.
- Pointers are 64 bits. Wasm can just use twice as much memory as it should, not my problem, tho it irks me.
- Sadly the error messages are completely incomprehensible.
- Sadly I don't have nice IDE integration.
- Sadly I don't have incremental builds. every time you run a program, you recompile the standard library for comptime. (...but its so fast it doesn't matter yet).
- Sadly the compiler only runs on aarch64. (the llvm backend can cross compile to other targets tho, but I can't run comptime code on them yet).
- Sadly my implementation of the C ABI is buggy. It's good enough for the example programs tho.
- Sadly I don't have a nice debug mode that detects undefined behaviour (overflow, wrong tagged field, etc).
