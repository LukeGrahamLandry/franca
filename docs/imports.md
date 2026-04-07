
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

## Fetching Dependencies

See `@/examples/fetch.fr/(single, group)` and `@/tests/deps.fr`. 
The only situation I help with is when you know exactly what files you want 
(the hash of their contents and a url to get it). 
I don't provide anything for version resolution of transitive dependencies
(i think the road to hell is paved with "pip install"). 
Remember that compiling a program is just as insecure as executing it. 

Style Guide:
- make dependencies optional (ex. grep for `FEAT_PNG` in the franca examples folder). 
- fetch source not binaries. 
- provide franca code that describes how to build the external code 
  and run it every time you compile so it can't rot. 
  (tests/external has some examples of using import_c to do this for c libraries). 

## Alternative Frontends

The metaprogramming stuff is powerful enough that you can often access code written in other languages 
from the comfort of your franca compilation context without relying on specific system binaries being installed. 
See [docs/codegen](./codegen.md) for more information. 
