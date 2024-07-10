# Foreign Bindings

This directory contains eXtra code to make it more ergonomic to use the eXternal libraries in `../../bindings`.

There's nothing special about the libraries chosen here.
You can write Franca code that calls into arbitrary foreign code that speaks your platform's C ABI.
These are just things I felt were needed to make interesting examples
and were hard enough problems that I don't have time to do it myself.
The language isn't at a scale where it makes sense to have seperate repositories for every library you might want to link against.

## Libraries

- [Sokol](https://github.com/floooh/sokol): cross-platform way to create a window, get user input, and talk to the gpu.
- [Wuffs](https://github.com/google/wuffs): decoding various image/compression formats, hash functions, json lexer.
- [Tracy](https://github.com/wolfpld/tracy/tree/master): profiler.

## Bindings vs x-libs

- The seperation is the same as Rust's `-sys` crates convention.
- The raw `bindings` version is (conceptually) auto generated from c header files.
- The raw versions are, by definition, unopinionated. They correspond directly to the library's interface.
- You might disagree with the abstractions chosen by the `x` versions.
- Only one person has to figure out how to build and describe the interface of foreign code.
- It's easier to avoid dependency conflicts if different higher level interfaces can explicitly show they use the same native library.
  Then you don't have to include multiple copies of the base code (just hope they want the same version or you're back at square one).
