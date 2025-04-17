## Entry Points

- run: execute a wasm module as though it were a native program.
- ffi: import a wasm module so that another franca program can call its functions.

## Limitations

- THIS IS NOT A SANDBOX
- it only supports the features that franca generates (no gc, no vectors, no multivalue, etc.)
- slow: no tiering or wasm specific optimisations 

## Why

abstraction will continue until moral improves. 

- i want to support compiling to wasm because it's nice to be able to show
someone a program without making them download something but i want to be 
able to test that without opening chrome. 
- it's easier to learn how to output a binary format by learning how to 
read it at the same time. instead of minimal error message from a large 
codebase i don't understand, i get assertion failed from a small codebase 
i just wrote. 
- since im kinda unenthused about "wnebsnite" as a platform, i don't want 
you to be able to do more when running in a browser than when compiled to 
native code. so since browser lets you import wasm module and call a 
function, native code should also let you do that. 
