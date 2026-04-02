
## Loading New Code

The basic interface the franca compiler provides is to execute one source file. 
When you want to split your program into smaller files for organisation, 
you need to tell the compiler how to find all your code. 
There isn't a seperate layer of build system for declaring what files 
are part of your program. Programs describe themselves and explicitly load new files as needed. 

Call `import("@/path/to/file.fr")` to load a new file and return a scope value containing 
its symbols (which can be accessed with dot syntax). 
Instead of binding the scope to a name and explicitly accessing symbols, 
you can pass it to a `#use(S);` statement to dump them into your own scope 
so they can be accessed more easily. If the argument to `#use()` is a string 
it is treated as though it were first passed to `import()`. 
Passing a string literal instead of a scope expression avoids the sketchiness 
of needing to compile the expression but then adding more symbols to the scope later 
(it also works better at top level where it might run before the full compilation environment is ready). 
TODO: explain that better. 
TODO: explain how namespaces work. there main file is the global namespace, child files you #use will have to #use eachother or not get symbols. 
TODO: note you can use #use in a function signeture and it will just affect the body. 
TODO: talk about SNEAK (using a FuncId as a scope)
TODO: note overload sets are always global
TODO: compare to import_module. 
TODO: be consistant about where root path is. 
TODO: talk about passing string of source code to import() instead of a file path. 

## Linking Extern Symbols

Sometimes loading new franca code is not enough, you want to import functions written in another language. 
This is done by putting `#import("foo")` on a function without a body (see `@/docs/annotations.md` for syntax overview). 
The argument to import is the library name. For example, when linking frameworks 
on macos the loader needs to know which framework contains each symbol 
(see `@/graphics/macos/frameworks.fr` for examples). There's also #libc which means #import("libc"). 

add_comptime_library(fr, name, handle) allows you to use symbols from an external library at comptime (or jitted runtime). 
`name` is the string for use with `#import` and handle is from `@/lib/dynamic_lib.fr/open` (dlopen). 
At comptime it becomes dlopen/dlsym but the extra indirection lets the same function declarations 
also be used at runtime, resolved automatically by the dynamic loader. 
See `@/graphics/easy.fr/build_for_graphics` which allows the graphics demos to work jitted as well as aot.  
You can also use dlopen/dlsym directly (as you would in c) from both comptime and runtime, 
but it's more verbose because you have to cast pointers to the right signeture and pass them around. 

Adding `#weak` allows the dynamic loader to provide null for that symbol. 
You'll crash if you try to access an unfilled weak symbol but currently you can't 
trust that a symbol being nonnull means it was filled (for example, when running at 
comptime the compiler generates shims so you'll get the address of the shim but you still can't call it). 
You have to alrady know in what conditions the import will be available. 
For example when linking libc functions return extra information in errno and 
the function to access that doesn't have a consistant name so `@/lib/sys/posix.fr/errno` 
chooses between `__error` and `__errno_location` based on the operating system. 

`@import_symbol("symbol_name", "library_name")` is an alternative interface that gives you back a rawptr. 
It also lets you access data symbols, not just functions. 
It's useful in comptime code where you want to do things in a less declarative way. 
(example: `@/lib/sys/syscall.fr/syscall` dynamically chooses to import a libc function or use direct syscalls). 

## Assembly

AsmFunction takes a function signeture and some code evaluated at comptime 
to produce some machine code that will execute when the function is called. 
You provide an implementation for each architecture you might want to target. 
- arm/rv: the value is a slice of u32 machine code instructions.
- amd/wasm: the value is a function `fn(out: *List(u8)) void`. 
  whatever bytes you append to that list will be inserted into your program as machine code.
You can import `@/backend/<arch>/bits.fr` for definitions of some useful constants. 

Since I need to support full compile time execution and also cross compilation, 
having a very structured system for target splits like this is very important. 

This is quite limited compared to more serious languages:
- There is no way to request relocations in your assembly so you cannot refer to other symbols 
(functions, constants, etc), you must pass in runtime known pointers as needed. 
- Assembly can only be inserted at a function call boundary. You can't mix franca 
and assembly in the same function body and assembly can't access local variables. 
- Since you can't access locals you need to know the calling convention the franca compiler uses. 
- On wasm it's extra limited because I don't expose a way to add entries to the global type table. 
- I don't provide an assembler so you have to construct the bytes with awkward function calls. 
However, since you can run arbitrary code at comptime, you can write your own assembler if you're 
excited about that. 

### asm usage examples

- `@/tests/fr/inline_asm_jit.fr` trivial examples just to show that it works. 
- `@/lib/sys/linux.fr/perform_clone` the clone syscall for spawning a new thread on linux returns twice 
  so it can't sanely be written in a high level language that might want to 
  spill variables on the stack (because one thread will return first 
  and stomp the stack space the other thread is still using). 
- `@/lib/sys/syscall.fr/perform_syscall` needs to use syscall instruction and remap to a different calling convention.  
- `@/lib/sys/jump.fr` implementations of setjmp/longjmp want to directly access callee saved registers. 
- `@/examples/os/kernel/start.fr/(interrupt_trampoline, return_to_user_impl)`
  need to save/restore all the user registers. 
- `@/lib/sys/process.fr/aarch64_clear_instruction_cache` uses cache control instructions 
  that are so rarely needed there's no point exposing them as a language feature. 

## Alternative Frontends

- import_c/ffi.fr/include()
- import_frc
- add_to_scope, intern_type, intern_func
- using the backend library to insert ir directly

Talk about doing stuff from within comptime vs through a CompCtx. 

## Fetching Dependencies

See `@/examples/fetch.fr/(single, group)` and `@/tests/deps.fr`. 
The only situation I help with is when you know exactly what files you want 
(the hash of their contents and a url to get it). 
I don't provide anything for version resolution of transitive dependencies
(i think the road to hell is paved with "pip install"). 
Remember that compiling a program is just as insecure as executing it. 
