A C11 compiler adapted from <https://github.com/rui314/chibicc>.

## Language Support

- core language: functions, variables, structs, unions, arrays, control flow, bit fields, etc.
- preprocessor: include, include_next, define, undef, if, ifdef, ifndef, elif, else, endif, line, pragma once, error, warning
- C99: `_Bool`, variable length arrays, flexible array members, designated initializers, compound literals, variadic macros
- C11: `_Generic`, `_Noreturn`, `_Alignof`, anonymous structs/unions
- C23: `__VA_OPT__`, typeof

### NOT IMPLEMENTED

- `_Thread_local`, `_Atomic`
- `_Complex`, `_Imaginary`
- `_Alignas`
- inline assembly
- the builtin standard headers are incomplete

## Changes from Chibicc

### Fixes

- fixed sizeof to ignore flexible array member 
- allow `type function(), variable;` (which means `function :: fn() type; variable: type;`)
- don't segfault when accessing anonymous union fields
- don't assume that a file starting with an `#ifdef` and ending with an `#endif` can be treated like `#pragma once` 
  (there may be multiple top level `#ifdef` blocks in a file so the last `#endif` isn't closing the first `#ifdef`). 

### Features

- use the franca compiler backend instead of generating amd64 assembly as text and depending on someone else's assembler and linker. 
- export to the franca compiler's type/function data structures so you don't need to manually write bindings to c libraries. 
(WIP. for demos, see `examples/bf/c_source.fr` and `examples/view_image.fr`). 
- optionally use the franca calling convention (passing a hidden environment pointer)
- allow some function attributes
  - ignored: `__format__`, `__const__`, `format`, `noreturn`
- show chain of macro declarations in error report

### Refactors 

- less verbose style of handling operator precedence.
- convert static variables to a struct explicitly passed around so you can have multiple compilation contexts at the same time. 
- simplify preprocessor by tracking a stack of disabled macros instead of storing a hideset on each token
- optimize handling of large zero initialized static arrays

### Regressions

- (temporarily) removed support for: thread locals, atomics, `_Alignas`
- removed some GNU extensions: labels-as-values, case ranges
- removed support for inline assembly
- removed support for x87 80-bit long doubles

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
