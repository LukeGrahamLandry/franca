
See `../annotations.md` for info about builtin tags you can put on functions. 

```
#libc
#import
#ir
#intrinsic
#comptime_addr
#redirect
#macro
```

talk about replacements for:
```
#target_os
#asm #x86_bytes #aarch64
#linkrename
#syscall
```

- import() and #use
- add_comptime_library
- @import_symbol
- lib/dynamic_lib.fr open()/get() -- same as dlopen/dlsym
- import_c/ffi.fr/include()
- import_frc
- add_to_scope, intern_type, intern_func
- using the backend library to insert ir directly
- fetch_or_crash

Talk about doing stuff from within comptime vs through a CompCtx. 

Talk about jit shims. Why the compiler needs to know if a symbol is a function or a data variable. 
Mention "tried to call uncompiled function" errors.

## Assembly

TODO: update to talk about AsmFunction instead of removed annotations (but the idea is the same).  

### asm

Instead of treating the body as normal franca code, it is evaluated at comptime 
to produce some machine code that will execute when the function is called. 
Requires `#aarch64` or `#x86_bytes` to declare which architecture you're supporting. 
An overload set can contain functions with the same arg/ret types but different 
`#asm` architectures. There are some basic examples in `tests/inline_asm_jit.fr`. 

Since I need to support full compile time execution and also cross compilation, 
having a very structured system for target splits like this is very important. 

This is quite limited compared to more serious languages:
- There is no way to request relocations in your assembly so you cannot refer to other symbols 
(functions, constants, etc), you must pass in runtime known pointers as needed. 
- Assembly can only be inserted at a function call boundary. You can't mix franca 
and assembly in the same function body and assembly can't access local variables. 
- I don't provide an assembler so you have to construct the bytes with awkward function calls. 
However, since you can run arbitrary code at comptime, you can write your own assembler if you're 
excited about that. 

### aarch64

Requires `#asm` and targets arm64. The body is a tuple of u32 machine code instructions. 
You can import `@/backend/arm64/bits.fr` for definitions of some useful constants. 

### x86_bytes

Requires `#asm` and targets amd64. The body is a function `fn(out: *List(u8)) void`. 
Whatever bytes you append to that list will be inserted into your program as machine code. 
You can import `@/backend/amd64/bits.fr` for definitions of some useful constants. 


## Bindings vs Ports

