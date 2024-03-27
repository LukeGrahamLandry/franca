# Franca

## Aspirations

- almost every function can run at compile-time.
- no seperate build config, everything is done with comptime execution.
- comptime functions can manipulate and create types.
- macros are functions that directly manipulate the ast.
- the goal is to pull as much as possible out of the compiler and into the library.

The compiler will have one command line argument.
It is the job of the source code to contain exactly the information required to compile the program.
You should be able to download the compiler, point at a file, and produce a program.

## Tradeoffs

I strongly believe in conservation of misery so if there's a bunch of stuff I like about this language
there must be propoertional terrible things or I'm probably just lying.

## Progress

- `src` contains the frontend and the interpreter used for comptime execution.
- The programs in `tests` and `lib` actually work on the existing interpreter.
- The programs in `plan` are just trying out how potential language features feel.

### Backends

- aarch64 machine code.
  - i put the bytes in memory, i mark it executable, and i jump there. no assembler, no linker, no problems.
- LLVM
  - a crossplatform optimising compiler toolchain (used by clang, rustc, swift, julia, the list goes on)

### Architecture

- lex, parse, resolve variable scope happens on all the code at the beginning
