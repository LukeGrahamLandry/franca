This is an overview of Franca's compilation model, not a guide on how to actually write programs.

## Lazy Sema

Unreachable code is not type checked. The compiler starts at main/tests/whatever, walks the callgraph as needed, and throws away the rest. I don't even parse the bodies of unreachable functions (the lexer skips over squiggly brackets until they match). There are certainly drawbacks to this approch. It's frustrating when you write a bunch of code but forget to make it reachable and get all the errors at once later.

> This is similar to zig (but more extreme) where only type instantiations that actually happen are checked, and unlike rust where traits are used to prove generics correct for every possible instantiation.

Any code that actually runs is statically typed and compiled. There is no runtime duck typing. Field accesses compile to pointer offsets not hash table lookups. There is no `eval(String)` function (unless you write one yourself and link against the compiler as a library I guess...).

The main advantage is making the lack of a module system / imports less crippling.
Its not a big deal to load all the code in the universe every time you compile a program, because most of it is immediately ignored.

The lack of traits and modules is not a philosophical decision. They'll likely be supported in some form eventually. I find it hard to read code when everything is `T: anytype`.

## Comptime Execution

All code can be evaluated during compilation. Any function you write that can be called at runtime can also be used to initialize a constant at comptime.

```
init_at_comptime :: do_stuff();
init_at_runtime  := do_stuff();
```

Any function arguments declared as `const` must be known at comptime. Calling a function with const arguments causes the function body to be duplicated, with the callsite's values baked into that version of the function. This process is memoized, a new function will only be instantiated once for each unique value passed as const args. However, the function is still called at every callsite, so it doesn't have to be a pure function even if all arguments are const.

Generics are implemented as functions that take constant types as arguments and return types. You often need a sad amount of type annotations when declaring variables with generic types.

```
Pair :: fn($T: Type) Type = @struct(fst: T, snd: T);
ints: Pair(i64) = (fst = 123, snd = 456);
assert_eq(Pair(i64), Pair(i64)); // identical types.
```

Const type arguments can be used in types later in the argument list (if the function uses the #generic tag). (this doesn't work with overloading yet).

```
// Requires overloads for `fn zero() T` and `fn add(T, T) T`
sum :: fn($T: Type, lst: Slice(T)) T #generic = {
    n: T = zero();
    for(lst, fn(x) => { n += x; });
    n
};

// TODO: it can't resolve the overload if you swap argument orders!
assert_eq(6, i64.sum(@slice (1, 2, 3)));
assert_eq(6.0, f64.sum(@slice (1.0, 2.0, 3.0)));
```

Macros are functions that take ast nodes as arguments and return ast nodes to be substituted into the program. They can also access compiler apis to evaluate and introspect thier arguments. Unlike the functions above, the bodies of macros are not duplicated, only the ast nodes they return are.
The macro function is called once for each callsite, they are not deduplicated.

Macro invocations are of the form `@name(arg1)` or `@name(arg1) arg2` or `@name arg1`.

```
// each callsite produces a unique type, even when the arguments are the same.
assert_ne(@struct(i: i64), @struct(i: i64))
```

There are several contexts that force an expression to be evaulated at comptime.

- The type hint on the right of a colon.
- A value passsed to a const arg.
- The value of a constant. like `name :: e`
- An expression after the `::` prefix operator.
- Any macro might choose to force evaluate an argument.
- The default value of a struct field.
- The value of an enum.

## Driver Program

Your program should contain all information required to compile itself.
I'd rather write build scripts in the same language as the program than in Bash.

## Compilation Order

I don't guarentee any specific order of compilation. Note that is a very different statement from "I guarentee you won't be able to observe the order of compilation". Any comptime code is free to make syscalls or store global mutable state, its up to you to be careful and not dig yourself into a confusing hole. In general, life is easier if macros are pure functions, so do that when possible. Programs that restrict access to the outside world to the driver portion, rather than the comptime portion, will likely be easier to reason about because then you can easily see a function that runs top to bottom. However, I don't want the compiler to be in the business of saying no, I just want to be able to write a program.

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

TODO: internal pointers are not yet supported. 

## Reproducible Builds

For a given (franca compiler, source code, target platform) combination, the output 
AOT binary should be exactly the same bytes no matter what computer does the compiling 
(including cross compiling!). 

Since Franca allows arbitrary compile time execution, you can trivially choose to do 
something that defeats reproducibility (like doing an http request to get the current 
phase of the moon and sticking that in your binary)... so don't do that if you want 
reproducible builds. The compiler doesn't inject its own non-determinism but it can't 
fix stupid. Similarly if you use someone else's linker, there's nothing I can do if 
it decides to mix the bytes around. 

The claim is that reproducibility works for all the example programs in 
the franca repository. However, the testing for this is not robust yet so it only 
mostly kinda probably works and it doesn't work at all with `-debug-info`. 
You can easily check a program by compiling with the environment variable 
`FRANCA_LOG_STATS=true` (twice) and checking that the `repro: <hash>` line matches. 

When the compiler gets more multi-threaded and more out of order, this might need to 
be relegated to a special option you can enable at the cost of reduced compile speed. 
