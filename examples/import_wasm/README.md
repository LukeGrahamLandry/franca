## Entry Points

- run: execute a wasm module as though it were a native program.
  (a host that provides libc-like imports for my tests but doesn't follow a standard abi)
- ffi: import a wasm module so that another franca program can call its functions.
  (see examples/bf/via_wasm.fr for simple usage)

## Limitations

- THIS IS NOT A SANDBOX
- it only supports the features that franca generates
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
- it's nice to have more examples of the backend api

## Feature Support

- basic arithmetic and control flow
- globals, functions, data segments
- tables: call_indirect, set, get, grow
- memory: init, grow
- atomics: rmw.cmpxchg, rmw.cmpxchg, wait32, notify, fence

### NOT IMPLEMENTED

- traps
- validation
- float: trunc, ceil, floor, nearest, copysign
- saturating conversions
- memory.copy: allow non-constant size. allow overlapping where you need to copy backwards. 
- memory ops: fill/drop
- more atomic ops
- atomic wait with timeout
- more table ops: fill/copy/init/drop
- multivalue returns
- simd
- externref as a first class value (rn only allowed in table for call_indirect)
- reference types: null/is_null/func/as_non_null/br_on_null/eq/br_on_non_null
- gc
- exception handling
- reference typed strings
- atomic.notify: return number woken (mine always says 0)
