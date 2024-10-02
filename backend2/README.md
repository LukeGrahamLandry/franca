An optimising compiler backend based on [Qbe](https://c9x.me/compile/).
The passes in `opt` are ported directly from Qbe, with some light editing to use a style I find less confusing.

In my brief profiling (with -O2), Qbe spends ~45% of its time parsing the input text and another ~15% outputting the assembly text.
Then clang takes as long to assemble that text as the whole Qbe part (or somehow 10x as long on aarch64).
However I want to use this as a JIT for comptime execution, which will only be practical if I remove those serialization steps.
You can still print out the ir as human readable text between passes and modify it for testing.

> When trying to understand what phi instructions do, remember that they're totally isomorphic to block arguments and then it's suddenly super simple.
> It's just a choice to represent it as the callee knowing which value to use for each argument for each caller.

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
