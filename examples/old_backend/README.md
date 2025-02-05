# Legacy Franca Backend

This converts the analyzed ast of a Franca function to a stack based bytecode 
which can then be converted to llvm-ir or qbe-ir text for an AOT build.
Originally, this was the only way to compile Franca code 
but now we have a fully self hosted backend that doesn't rely on llvm.

This legacy backend is still useful because...

- I don't have very much code in this language so I want to keep this as a test case.
- It's a good example of something cool you can do with driver programs.
- It can use llvm to generate more optimised code than my backend.
- llvm can generate debug info 

## Limitations 

- Not all language features are supported. 
  - The llvm target doesn't implement the context passing abi, so programs are not thread safe. 
  - #ir doesn't work so you can't compile `../backend/opt/fold.fr`
- The c abi is not followed correctly. Programs compiled by these backends can call eachother, 
but they cannot call extern-c functions safely (the new backend follows the c abi correctly). 
- If you use this from your driver program (like examples/default_driver.fr does),
it gets recompiled from scratch by the comptime-jit every time you compile your program. 
So there will be an increase in frontend time compared to using the precompiled backend 
included in the compiler. This can be mitigated by compiling your driver to a dylib. 
- These backends run on the same thread as the frontend unlike the new one which is parallelized. 
