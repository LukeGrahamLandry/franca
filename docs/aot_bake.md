# Ahead-Of-Time Compiled Executables

- talk about drivers here? apis for controling compilation yourself. 
- even when jitting there's a seperation between comptime and runtime. 
  - semantics: whether bake overloads get run. 
  - caching: it's nice to always compile to .frc module anyway. 
  - when passing file to franca cli, comptime code doesn't run if files didn't change. 
- can't access the implict compiler at runtime. 
  - but it's just franca code so you can use it as a library if you want. see examples/repl.fr

## Comptime Initialized Data

Since you're allowed to execute arbitrary code during comptime,
it feels natural to use that to initilize arbitrarily complex data structures.
However, anytime that involves memory allocation, you end up with your values in the compiler process's address space.
That's good enough for JIT evaluation of the program but for producing an AOT executable,
you need to be able to serialize that into a format the OS program loader and deal with.

Additionally, you don't want to blindly include a copy of all memory allocated by comptime execution.
It's perfectly valid to do some memory intensive computation where only the final result is needed by the final program.
In fact, all macros are like that, the ast nodes they create can be thrown away when the compiler is done generating code for them.
When emitting code, the backends walk the graph of objects accessable from your entry point functions and thier callees,
and only keep what is necessary.

> - C++20 requiries you to delete any new-ed memory in the same constexpr.
>   If you want to return a collection from a constexpr, you have to manually copy it to a statically sized array.
> - In rust/zig, standard collections are not usable in const/comptime contexts (issues: [const-eval#20](https://github.com/rust-lang/const-eval/issues/20)/[zig#1291](https://github.com/ziglang/zig/issues/1291).

The compiler inherently understands how to include values of the basic types in an executable.

- Integers are trivial.
- @struct/Array recursively includes thier fields (zeroing padding).
- @tagged recursively includes the fields of the active varient (zeroing padding).
- `*T` recursively includes T and registers an appropriate relocation with the linker.
- Fn and Type decay to an opaque integer id (the only runtime use is comparing them for equality).
- FnPtr looks up the backing function, ensures its added to the executable, and asks the linker for a relocation.

However, some types have special requirements.

- `Slice(T)` is defined as `@struct(ptr: *T, len: i64)`.
  The system above would treat all slices as though they had a length of one.
  The compiler needs to know that is should offset the pointer `len` times.
- `CStr` is just a `*u8` but we want to include more than once byte, all the way to the null terminator.
- `List(T)` allocates extra space to provide more efficient appends, but its silly to include uninitilized memory in the final program.
  It should truncate its capacity at the end of compilation.
  An alternate solution would be represent the struct as (items, cap) instead of (maybe_uninit, len).
- Any collection containing an `Alloc` implies that it would be valid to call free on it, but that doesn't make any sense for data stored in the executable.
  You really don't want a naive heap allocator putting read-only memory in its free-list to crash on later when it tries to fulfill an allocation.
- A type representing an operating system resource (like a file descriptor) probably doesn't make sense to include as constant data.

This is addressed by allowing user code to add overloads of `fn bake_relocatable_value(self: *T) Slice(BakedEntry)`.
The question you're answering for the compiler is "If I have a T, how can I represent that with only the expressivity of a C global array initilizer?"
Most of the time you'll never have to think about it because the standard library provides commonly needed implementations,
but the extra power is there if you need it.

> `BakedEntry/BakedVar` are semantically equivalent cranelift's `DataDescription`.

It is considered disrespectful to lie about the type of a constant.
For example if you cast a pointer to an integer, store that in a constant, and then cast it back at runtime,
the value behind the pointer won't have been considered reachable and the number will just be a useless address in the compiler's address space.
Its fine to do all the bit-casts you want during comptime, all that matters is the final type seen by the compiler when the executable is created.

Currently `bake_relocatable_value` is only called if the type contains some type of pointer and some byte of the value is non-zero. 

this even works on internal pointers. TODO: explain more
but it's fragile and relies on the outer most thing being seen first. which is dumb and needs to be fixed. 

- ClearOnAotBake(T)
- note similar to tracing garbage collection
- note: since the bake_relocatable_value functions are always jitted, it's fine if thier implementations 
  contains constants of their own type, they never get baked anyway. 
  ie. this is fine even though it looks like it would be a cyclic dependency. 
  ``` 
  fn bake_relocatable_value(self: Foo) []BakedEntry = {
    another := :: zeroed Foo; // this is a constant of type Foo but will never be baked.  
    // <snip>
  }
  ```

- talk about how jit-shims are used to avoid compiling for comptime but still give you something that bakes to the same thing. 
