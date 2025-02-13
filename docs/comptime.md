# Metaprogramming in Franca 

Franca allows full compile time execution. 
Anything you can do at runtime, you can do at comptime too.
In fact, you have more power at comptime than runtime because you can dynamically generate code to be added to your program. 
I claim our version of this is more powerful than `comptime` in zig, `constexpr` in C++, or `const fn` in Rust.
In Franca, comptime code is just-in-time compiled to native machine code, following the same calling conventions. 
So you can freely call between JIT and AOT code (even extern-c code written in another language). 

There are exactly two limitations: 
  - you can't call pthread_jit_write_protect_np(false) on macos. 
    (because you'll make the comptime code itself non-executable)
  - you can't call fork (starting new threads is fine).
    (because the child process won't have the compiler's thread 
    so if you try to call a function that hasn't been compiled yet, it never will be).

## Inserting Ir

The franca self hosted backend started as a port of [Qbe](https://c9x.me/compile/) and 
one of the optimisation passes is constant folding. Part of that requires the ability 
to take any ir operation and evaluate it. A simple implementation of this may involve 
a giant switch statement over all the operations and all the primitive types you support. 

Here's an excerpt from Qbe's [fold.c](https://c9x.me/git/qbe.git/tree/fold.c?id=5e9726946dcb9248dbd34ded1bdd4f7af8dc2d31): 

```
case Osar:  x = (w ? l.s : (int32_t)l.s) >> (r.u & (31|w<<5)); break;
case Oshr:  x = (w ? l.u : (uint32_t)l.u) >> (r.u & (31|w<<5)); break;
case Oshl:  x = l.u << (r.u & (31|w<<5)); break;
```

It becomes an awkward dance to figure out the right incantation to make the c compiler 
generate exactly the instruction you need to get bitwise matching output as if you'd 
generated code to do the operation at runtime instead. 

error prone.   
we already have codegen that understands our ir.   

It would be convenient if we could write code like this instead: 

```
// pseudocode
fn eval(rt_o: Op, rt_k: Cls, a0: Bits, a1: Bits) Bits = {
  for_enum Op { $ct_o |  // add, sub, etc.
    for_enum Cls { $ct_k |  // i64, f32, etc.
      if can_fold(ct_o, ct_k) && rt_o == ct_o && rt_k == ct_k {
        f :: /* somehow ask the compiler to insert code that runs ct_o here */;
        return f(a0, a1);
      };
    };
  };
}
```

The full code for this is at the bottom of `backend/opt/fold.fr` (the do_fold function). 
It's more code than I'd like and it's less obvious than the giant switch but it does the job 
and I'm sure the language can be refined farther to make it less painful. 

## Parsing Strings

The franca self hosted backend started as a port of [Qbe](https://c9x.me/compile/) and there 
are a few places where there are instructions that are convient for frontends to generate but don't 
map to a single assembly instruction for all architectures. 

Qbe's [sysv.c](https://c9x.me/git/qbe.git/tree/amd64/sysv.c?id=5e9726946dcb9248dbd34ded1bdd4f7af8dc2d31#n536). 

backwards.  
comment on top (might rot). 

## String Formatting

> Zig can do this too so if you've used that this section will be boring and you should skip it. 

@fmt("% to %", a, z);

You probably don't want runtime overhead of parsing the string to look for percent signs every time you print something. 

This plays nicely with function overloading so @fmt can handle user defined types. 

## Generics

> Zig can do this too so if you've used that this section will be boring and you should skip it. 

Types are first class values. You can write a function that takes a type as an argument, 
returns a type, and have the compiler generate code specialized for each different version.  

## Generating Tables

> Zig can do this too so if you've used that this section will be boring and you should skip it. 

When outputting Mach-O executables (for macOS), you need to include a section with the sha256 hash of each page of memory. 

nothing up my sleeve numbers.  
pasting tables.  

## Adhoc Library Loading

There are more robust ways to do this but sometimes it's nice to be able to just experiment with things quickly. 

```
SRC :: """
long add_with_ffi(long a, long b) {
  return a + b;
}
""";

:: {
    write_entire_file("./add.c", SRC) || @panic("TODO: create file");
    args := @slice("add.c", "-dynamiclib", "-o", "adder_dependency.dylib");
    run_cmd_blocking("clang", args) || @panic("failed compile");
    lib := dlopen("adder_dependency.dylib", DlFlag.Lazy);
    ctx := current_compiler_context();
    ctx.add_comptime_library(@symbol "adder", lib);
};

fn add_with_ffi(a: i64, b: i64) i64 #import("adder");

fn main() void = {
  println(add_with_ffi(1, 2));
}
```

This snippet has many flaws: 
- The program cannot be aot compiled. Calling dlopen at comptime loads 
the code in the compiler's address space and add_comptime_library lets 
it look there for functions but neither tells anyone to bundle that code 
into the final executable. This would be fine if only other comptime 
code needed that library. 
- It spews random files around. 
- You add a dependency on another compiler. 

## Observing the Environment
