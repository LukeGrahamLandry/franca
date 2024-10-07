An optimising compiler backend based on [Qbe](https://c9x.me/compile/).
Almost everything (ir design, opt passes, isel/abi) is ported directly from Qbe, with some light editing to use a style I find less confusing.

In my brief profiling (with -O2), Qbe spends ~45% of its time parsing the input text and another ~15% outputting the assembly text.
Then clang takes as long to assemble that text as the whole Qbe part (or somehow 10x as long on aarch64).
However I want to use this as a JIT for comptime execution, which will only be practical if I remove those serialization steps.
You can still print out the ir as human readable text between passes and modify it for testing.

If you use llvm you don't get full c-abi compatibility for free.
For example, here's where rustc does it in the frontend https://github.com/rust-lang/rust/blob/master/compiler/rustc_target/src/abi/call/mod.rs

## Changes from Qbe

- Generate machine code directly without depending on an external assembler.
  Your program will contain only the finest organic bit patterns lovingly transcribed from the Arm Architecture Reference Manual.
- Jit compile your program and run it in memory without needing to emit/link an executable.
  You can freely call between jit and aot code (even extern-c code from other compilers) because they follow the same standard abi.
- Moved a bunch of static variables to a data structure we pass around explicitly.
  Eventually you will be able to compile multiple modules on parallel threads (not yet tho, there's still more unported).
- Removed several codegen optimisations until we have a solid foundation (but I want to bring them back eventually).
  - arm: loading float constants from memory
  - arm: logimm single instruction load int more constants
  - arm: use of bl for calling a constant (instead of blr)
  - arm: constant immediate without an extra register when accessing thread locals
- Removed the RISC-V target for now because I haven't done thier instruction encoding yet.

## Qbe License

© 2015-2024 Quentin Carbonneaux <quentin@c9x.me>

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
