
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

Importing strings starting with `@/` first looks up the path in the library root folder and then falls back to the current working directory if it doesn't exist. 
The root folder is something that contains a file called "lib/franca_stdlib_1.fr". 
The compiler will guess it's location based on the location of the compiler executable but you can override it with the FRANCA_LIBRARY_DIR environment variable. 

Importing strings starting with "{" treats them as franca code. (see `./codegen.md## Import String` for details). 

### Scopes

The entry point file passed to the compiler contains the global scope. 
Any other files you import() have that global scope as their parent (they can see identifiers declared there). 
If they need to access constants declared in other files not #use-ed by the global scope they must import them explicitly. 
Calls to import() are memoized. Importing the same file twice doesn't recompile it and uses the same versions of @static variables, etc. 
Overload set entries (`fn foo(a) r = ();`) default to being in the global scope unless there's 
a closer empty local overload set declaration (`fn foo();` without a body) for them to bind to. 
Local overload sets like that are constant values in their scope (can be accessed with dot syntax or #use like any other). 

TODO: talk about other ways of making scopes (Type.scope_of and FuncId.scope_of) and other ways of using scopes (get_constants, get_constant). 

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

If you want the inverse (another language is the main program and it imports symbols from franca) 
extra care is needed, see [docs/freestanding](./freestanding.md). 

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
