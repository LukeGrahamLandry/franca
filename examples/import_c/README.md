A C11 compiler adapted from <https://github.com/rui314/chibicc>.

## Changes from Chibicc

- use the franca compiler backend instead of generating amd64 assembly as text and depending on someone else's assembler and linker. 
- less verbose style of handling operator precedence.
- removed some GNU extensions: labels-as-values, case ranges
- removed support for inline assembly
- removed support for x87 80-bit long doubles
- convert static variables to a struct explicitly passed around so you can have multiple compilation contexts at the same time. 
- (temporarily) removed support for: thread locals, atomics, `_Alignas`

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
