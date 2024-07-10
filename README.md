# Franca

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

- `src`: the sema stuff and the jit used for comptime execution.
- `compiler`: the self hosted parser and llvm backend.
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

- I don't care about being pragmatic for large corporations. My main goal is to make sure its not boring.
- I don't care about being easy to learn. I want it to be fun. Rust is fun.
- Stuff's unsafe, deal with it, simply don't make a mistake. i.e. `fn main() = { let a: *i64 = 123.int_to_ptr(); println(a[]);` will compile, run, and (probably) crash.
- I don't care about fallibile memory allocation
- It's a little heavy on the punctuation.
- I believe I've never touched a big endian computer so I don't care.
- (TEMP) completely incomprehensible error messages.
- (TEMP) no caching. every time you run a program, you recompile the standard library for comptime. (...but its so fast it doesn't matter yet).
- (TEMP) the compiler only runs on aarch64. (the llvm backend can cross compile to other targets tho)
