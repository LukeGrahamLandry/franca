This converts the analyzed ast of a Franca function to a stack based bytecode which can then be,

- compiled directly to arm64/amd64 machine code in memory. This compiles very quickly but generates poor quality code.
- converted to llvm-ir or qbe-ir text for an optimising AOT build.

Previously, this was the path taken by all Franca code but I'm slowly moving to an ir that's more conducive to running optimisations without relying on llvm.

Still useful because...

- I don't have very much code in this language so I want to keep this as a test case.
- It's a good example of something cool you can do with driver programs.
- It can use llvm to generate more optimised code than my backend.
- It proves a point about backwards compatibility.
  Exposing compiler internals isn't a big deal because if they need to change,
  the old version can just become a library loaded by your driver program.
