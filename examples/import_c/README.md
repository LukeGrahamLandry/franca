A C11 compiler adapted from <https://github.com/rui314/chibicc>.

Compared to chibicc, my changes: 
- support more target platforms (macos, arm, riscv, wasm).
- make it a standalone program without depending on binutils' assembler/linker.
- improve performace (both of the compiler and the generated code).
- tackle the second 90% of the work to make more real programs build.  
  see `../../tests/external/*.fr` for various build scripts 
  that test other people's code with this compiler (and run in github actions on every commit). 

## Language Support

- core language: functions, variables, structs, unions, arrays, control flow, bit fields, etc.
- preprocessor: include, include_next, define, undef, if, ifdef, ifndef, elif, else, endif, line, pragma once, error, warning
- C99: `_Bool`, variable length arrays, flexible array members, designated initializers, compound literals, variadic macros
- C11: `_Generic`, `_Noreturn`, `_Alignof`, `_Static_assert`, anonymous structs/unions, `_Atomic`
- C23: `__VA_OPT__`, typeof
- GNU: statement expressions, `a :? b`
- attributes: packed, aligned

### NOT IMPLEMENTED

- `_Thread_local`
- `_Complex`, `_Imaginary`
- `_Alignas`
- gnu extended inline assembly
- simd intrinsics (immintrin.h, arm_neon.h, etc.)
- digraphs, trigraphs
- the builtin standard headers are incomplete
  > but you can use your system headers instead with a bit of cajoling, 
  > see ../../tests/external/(raylib, curl, bubblewrap).fr for examples.

## Changes from Chibicc

### Fixes

- fixed sizeof to ignore flexible array member 
- allow `type function(), variable;` (which means `function :: fn() type; variable: type;`)
- don't segfault when accessing anonymous union fields
- don't assume that a file starting with an `#ifdef` and ending with an `#endif` can be treated like `#pragma once` 
  (there may be multiple top level `#ifdef` blocks in a file so the last `#endif` isn't closing the first `#ifdef`). 
- don't segfault on `puts(*&"");`
- allow a union declaration with a single expression assigning the whole union 
  instead of a treating it as a single element initializer list initializing only the first member. 
- allow reading const variables in constant expressions

### Features

- use the franca compiler backend instead of generating amd64 assembly as text and depending on someone else's assembler and linker. 
- export to the franca compiler's type/function data structures so you don't need to manually write bindings to c libraries. 
(for demos, see `examples/bf/c_source.fr` and `examples/view_image.fr`). 
- allow some function attributes
- show chain of macro declarations in error report
- declare symbol aliases with the `asm` keyword
- `__builtin`: clz, ctz, popcount, bswap, rotateleft, rotateright, expect, constant_p

### Refactors 

- less verbose style of handling operator precedence.
- convert static variables to a struct explicitly passed around so you can have multiple compilation contexts at the same time. 
- simplify preprocessor by tracking a stack of disabled macros instead of storing a hideset on each token
- optimize handling of large zero initialized static arrays

### Regressions

- (temporarily) removed support for: thread locals, `_Alignas`
- removed some GNU extensions: labels-as-values, case ranges
- the asm statement uses my own **very** limited assembler instead of shelling out to an external one
- removed support for x87 80-bit long doubles
- i've sacrificed code readability for performance. 
  chibicc is a much better resource for learning about compilers than mine. 

## Chibicc License

MIT License

Copyright (c) 2019 Rui Ueyama

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
