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

## DSLs in Strings

The franca self hosted backend started as a port of [Qbe](https://c9x.me/compile/) and there 
are a few places where there are instructions that are convient for frontends to generate but don't 
map to a single assembly instruction for all architectures. 

Qbe's [sysv.c](https://c9x.me/git/qbe.git/tree/amd64/sysv.c?id=5e9726946dcb9248dbd34ded1bdd4f7af8dc2d31#n536). 

backwards.  
comment on top (might rot). 

In the previous example we were generating ir that would go directly in the program. 
Here we're genenerating code that when run will generate ir to include in a different program. 

You could probably do this in Zig but you'd have to write your code carefully to not dynamiclly allocate memory, etc. 
Here it's just regular code i already had . 

In rust, macros are done at the token level so you can have things that don't parse and then pass that 
as input to a seperate program to generate some new tokens. 

## Reflection to Generate Shaders    

- https://github.com/floooh/sokol/blob/master/util/sokol_debugtext.h#L2374-L3879


- can rot. if i run thier shader compiler i don't get the same bytes of metal ir out. 
probably that's just because i have a different version of apple's shader compiler but who knows. 

```
Shaders :: @struct(
    InV  :: @struct(position: Vec2 #attribute(0), texcoord0: Vec2 #attribute(1), color0: Vec4 #attribute(2)),
    OutV :: @struct(uv: Vec2 #user(locn0), color: Vec4 #user(locn1), pos: Vec4 #position),
    vs   :: fn(in: Shaders.InV) Shaders.OutV = (
        pos = @vec(in.position * (@vec(2.0, -2.0)) + @vec(-1.0, 1.0), 0.0, 1.0),
        uv = in.texcoord0,
        color = in.color0,
    ),
    InF  :: @struct(uv: Vec2 #user(locn0), color: Vec4 #user(locn1)),
    OutF :: @struct(frag_color: Vec4 #color(0)),
    UniF :: @struct(tex: ShaderTexture2f #texture(0), smp: ShaderSampler #sampler(0)),
    fs   :: fn(in: Shaders.InF, uni: Shaders.UniF) Shaders.OutF = (
        frag_color = @swizzle sample(uni.tex, uni.smp, in.uv).xxxx * in.color,
    ),
);
```

## Data in Strings

- https://github.com/floooh/sokol/blob/master/util/sokol_debugtext.h#L825-L2372


since franca can read files at compile time, we could just store the data in a binary file and embed it in the program. 

if you ever want to edit it you still have to go scounge around for some program that speaks your specific font format. 

human readable / editable 

...@@... .....@@. @@@..... ..@@@...
........ ........ .@@..... ...@@...
..@@@... ....@@@. .@@..@@. ...@@...
...@@... .....@@. .@@.@@.. ...@@...
...@@... .....@@. .@@@@... ...@@...
...@@... .@@..@@. .@@.@@.. ...@@...
..@@@@.. .@@..@@. @@@..@@. ..@@@@..
........ ..@@@@.. ........ ........

## Generating Switches

- https://github.com/rui314/chibicc/blob/main/tokenize.c#L144-L156

it's much faster if you manually write out a bit switch statement. 

// @switch(p[]) {
//     @case("<".ascii()) => @switch(p.offset(1)[]) {
//         @case("=".ascii()) => .@"<<";
//         @default => .@"<";

> I could imagine that an `inline for` in Zig would be optimised to the same thing by llvm.
> Franca does have an `@inline_for` but my backend is too dumb to generate something sane from it so far. 

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

how do you embed the current git hash in a c program. 

## Inserting Ir for Constant Folding

> This is a bad example because it's cheating by being part of the compiler but it's still kinda neat. 

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
