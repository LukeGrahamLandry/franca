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

## Backends

- aarch64 machine code.
  - i put the bytes in memory, i mark it executable, and i jump there. no assembler, no linker, no problems.
  - tested rust targets: aarch64-apple-darwin
- cranelift
  - tested rust targets: aarch64-apple-darwin, x86_64-apple-darwin, x86_64-unknown-linux-musl

## Progress

- `src` contains the frontend and the jit used for comptime execution.
- The programs in `tests`, `lib`, and `examples` actually work with the existing jit compiler.

## Future Goals

- No seperate build config, everything done with comptime execution (like jai).
  The compiler will have one command line argument.
  It is the job of the source code to contain exactly the information required to compile the program.
  You should be able to download the compiler, point at a file, and produce a program.
- macros that derive implimentations of common operations on your types (like rust).
- the goal is to pull as much as possible out of the compiler and into the library. but I do like being able to build from source easily without a complicated bootstrapping problem.
- parse declarations from other languages and generate ffi bindings.
- checked mode with less undefined behaviour.
- llvm backend for optimised release builds
- i want to produce an executable such that every byte in the file is there because i personally put it there.

## Tradeoffs

I strongly believe in conservation of misery so if there's a bunch of stuff I like about this language
there must be proportional terrible things or I'm probably just lying.

- I don't care about being pragmatic for large corporations. My main goal is to make sure its not boring.
- I don't care about being easy to learn. I want it to be fun. Rust is fun.
- Stuff's unsafe, deal with it, simply don't make a mistake. i.e. `fn main() = { let a: *i64 = 123.int_to_ptr(); println(a[]);` will compile, run, and (probably) crash.
- I don't care about fallibile memory allocation
- (TEMP?) there are no arithmetic operators, if you want to add, just call the function add.
- (TEMP) completely incomprehensible error messages.
- (TEMP) no caching. every time you run a program, you recompile the standard library for comptime. (...but its so fast it doesn't matter yet).
