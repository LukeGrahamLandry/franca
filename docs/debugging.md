# Tips for Debugging Franca Programs

## Stack Traces

-keep-names  
-debug-info  
FRANCA_BACKTRACE=1  
crash_report.fr, hook_backtrace

## Invasive Library Configuration

These all rely on your whole program being compiled from source every time 
and having the source for all the libraries. So you can just... edit the code, even low level 
syscall wrappers, allocators, etc. 

You do have to be careful when doing this because comptime code 
and runtime code are both compiled from the same source. For example, if you try to make your own 
version of strace by adding a print call to the wrappers in lib/sys/syscall.fr. Well your print 
is going to try to make the write syscall and you're going to loop on that forever. That particular 
example you can solve if you're dynamically linking libc and give yourself a 
`printf :: fn(fmt: CStr, va: CVariadic) i32 #libc`, but that pattern of circular dependencies 
is something to watch out for. 

These are some constants in `lib/core.fr` that are used in the library code 
and are likely to work when toggled (but are not yet automatically tested 
so it's not impossible that they're broken at any given time). They generally 
let you trade off performance for extra runtime safety checks. 

- SLOW_MEMORY_DEBUGGING
- NEVER_USE_LIBC_ALLOC: replaces general_allocator. actually much more likely to have bugs than libc_allocator 
  but means it uses the same allocation code no matter what operating system. 
- SLOW_DEBUG_ALLOC
- SLOW_PROTECT_ARENA
- SLOW_USERSPACE_THREADS
- SLOW_ARENA_CANARY
- SLOW_LEAK_ARENAS

## Thread Locals

Some important state is stored in StaticTls. 
This includes the current operating system, temporary allocator, and panic hook.
When external code calls franca code from a thread that didn't go through franca_runtime_init,
you can get into a confusing situation where you can't safely do any syscalls,
allocate memory, or even panic so you'll just crash and burn pretty quickly. 

I don't follow the extern-c/elf/whatever abi. Instead, callstacks are allocated a with certain alignment 
and the tls area placed at the left where it can be accessed by masking off the low bits of sp.
This works even if the other language (or its libc) needs to control the FS/TPIDR_EL0/TP 
register for their own thread locals. As long as the franca side created the thread, 
or you swap to a stack with the correct arrangement (like franca_runtime_init does), 
you can call between languages safely. 

This feature is mainly used for:
- running the frontend once and getting IR that dynamically chooses the right behaviour for the current operating system
- comptime code inheriting state from the compiler
- me not needing to implement the "normal" thread local abi

These problems can be avoided, you just have to know they exist, and it's a bit tedious. 
As proof, the graphics programs on macos are a nest of franca callbacks handed out 
to the system's objective c frameworks and it all works out just fine in the end 
(see graphics/macos/app.fr for nontrivial example). 

## Compiler Logging

-d  
dump_bin.fr  
#log_ir  
#log_ast  

## Finding Compiler Bugs

- There's lots of known broken stuff in todo.md and tests/todo/*.fr, 
  if something's super confusing check if any of those look familiar. 
- Try using a debug build of the compiler 
  (built without the `-unsafe` flag: `franca examples/default_driver.fr build compiler/main.fr -keep-names -o franca_debug.out`). 
  See if that hits an assertion while trying to compile your program. 
- Try running in different environments. If they behave differently it's probably a compiler bug. 
  - native vs cross compiling
  - arm vs amd vs wasm
  - macos vs linux
  - linking libc vs `-syscalls`
  - compilation modes:
    - `franca foo.fr`
    - `FRANCA_NO_CACHE=1 franca foo.fr` 
    - aot: `franca examples/default_driver.fr build foo.fr && ./a.out` 
    - comptime: `main :: fn() void = @run { /*whatever*/ };`
    - linker: `franca examples/default_driver.fr build foo.fr -c -o a.o && clang a.o -o a.out -Wl,-undefined,dynamic_lookup && ./a.out` 
- Make sure your build is reproducible. If you compile your program twice do you get identical binaries? 
  If not, it could be a harmless mistake in comptime code that just should be fixed for more sane debugging, 
  or it could reveal a more important compiler bug that prevents reproducibility. 
- The best case reproduction is if you can minimize to a franca program and a c or zig program 
  that should clearly do the same thing but don't. 
