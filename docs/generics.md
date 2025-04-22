# Generics

When using languages that are not slow, you may find yourself inconvenienced by static typing. 

Franca has three general styles for accessing that functionality 
(with purposely stupid names I've assigned just now and will hopefully never use again). 
They're ordered in increasing level of convenience/magic. 
- types as values
- instantiated overload sets
- where synthesis

If you read through the more detailed descriptions, you'll see these are all quite similar,
they're just different perspectives on what c++ calls templates. 

If monomorphization is too restrictive, you can always fall back to struct of function pointers. 

## Types as Values

> If you're familiar with Zig, this is the same idea as what they do. 

Types are values like any other and thus you can define functions with parameters of type `Type`. 
At runtime, type values are fairly useless (they're just an opaque index into a compiler data 
structure that has ceased to exist). However at comptime, types values can do all kinds of things. 
For example, the type annotation when declaring a variable is just an expression that returns 
a type and is evaluated at comptime. The last piece of the puzzle is forcing a parameter to 
be known at comptime by prefixing its name with `$`. If you try to pass a runtime known expression 
for that parameter, you'll get a compile error. 

```
count :: fn($T: Type, arr: []T, value: T) i64 #generic = {
  n := 0;
  for arr { it |
    if it == value {
      n += 1;
    }
  };
  n
}

ints := @slice(1, 4, 3, 4);
n := count(i64, ints, 4);
@assert_eq(n, 2);

// You can return types as well 
Pair :: fn($Left: Type, $Right: Type) Type = 
  @struct(left: Left, right: Right);
one_1: Pair(Str, i64) = (left = "one", right = 1);

left_is :: fn($Left: Type, $Right: Type, self: Pair(Left, Right), check: Left) bool #generic = {
  self.left == check
}

@assert(left_is(Str, i64, one_1, "one"));

// Since functions are first class values too, you could write it like this. 
left_is2 :: fn($Left: Type, $Right: Type) FuncId = {
  (fn(self: Pair(Left, Right), check: Left) bool = self.left == check)
}

// give a name to a partial application
left_is_SI :: left_is2(Str, i64);
@assert(left_is_SI(one_1, "one"));
// or call it inline
@assert(left_is2(Str, i64)(one_1, "one"));
```

TODO: the #generic in the first function signeture is a stupid thing that exists
because I handle variable scoping in a silly way. Without it, variable references in 
the type annotations won't bind to the previous parameters. The compiler should just 
make it work, I just haven't gotten around to fixing it yet. 

This is elegant from a language simplicity perspective. It's not really a language feature, 
it's more like I would have to artificially restrict things to prevent this from working. 

The downside to this system is that you have to specify the types all over the place, 
even when it seems like the compiler could trivially infer them from the other 
arguments to the call. Especially the `left_is` example above is quite painful, 
you're forced to pass two types (and one of them you don't even care about). 
You can probably see how this could spiral out of control as your program gets more complicated. 
Zig reduces that problem by having methods attached to types, but that only helps when
the first parameter is the only generic type and only for functions defined inside the type's 
definition (so it doesn't help you when using types from some other library). 
Franca doesn't have methods but the next style provides a similar solution. 

## Instantiated Overload Sets 

Franca supports function overloading. You can have a collection of functions with the same 
name but different parameter types and the compiler will choose the right one for 
each callsite. 

```
fn count($T: Type) void = {
  fn count(arr: []T, value: T) i64 = {
    n := 0;
    for arr { it |
      if it == value {
        n += 1;
      }
    };
    n
  }
}

::count(i64);
ints := @slice(1, 4, 3, 4);
n := count(ints, 4);
@assert_eq(n, 2);

Pair :: fn($Left: Type, $Right: Type) Type = {
  Self :: @struct(left: Left, right: Right);
  
  fn left_is(self: Self, check: Left) bool = {
    self.left == check
  }
  
  Self
}
one_1: Pair(Str, i64) = (left = "one", right = 1);

@assert(left_is(one_1, "one"));
```

- The inner and outer functions being in the same overload set (`count`) 
in the first example is arbitrary (they could have different names), it 
just makes it easy to remember which function to call to instantiate it. 
- You can mix the styles. A function in an overload set can still have `$const` parameters. 

## Where Synthesis

This style addresses the compilation order complaint above, 
instead of manually instantiating your overload sets, let the 
compiler do it for you based on the types of arguments at the callsite. 
We introduce a new glyph: `~`. 

```
count :: fn(arr: []T, value: ~T) i64 #where = {
  n := 0;
  for arr { it |
    if it == value {
      n += 1;
    }
  };
  n
}

ints := @slice(1, 4, 3, 4);
n := count(ints, 4);
@assert_eq(n, 2);

Pair :: fn($Left: Type, $Right: Type) Type = 
    @struct(left: Left, right: Right, L :: Left);
  
fn left_is(self: ~P, check: P.L) bool #where = {
    self.left == check
}

one_1: Pair(Str, i64) = (left = "one", right = 1);

@assert(left_is(one_1, "one"));
```

All the previous styles make it very easy to define functions that claim to 
work for any `Type` but actually have extra requirements. Our examples 
require that `T` and `Left` provide an implementation of operator `==` 
but you'll only discover that by trying to call it with types that don't 
work and getting a compile error somewhere in the implementation. 

Since Franca has full compile time execution and lets you introspect on your program, 
you could address that problem by writing your own code that does extra type checking. 
This will work for any of the styles discussed so far. 

Alternatively, you can provide a predicate as and argument to the `#where` annotation.
That predicate is called at comptime for each unique set of parameter types used. 
It is passed the types of the placeholders and returns true to allow the call (or false to deny it). 
This can be used to build up something similar to a trait system. 

TODO: example

TODO: `#where` is not fully implemented yet! there are many expressions 
that you can try to pass as an argument that the compiler will 
give up guessing a type for. this will be fixed eventually. 
it's not a fundamental limitation, it's just a part of the compiler 
that i hate working on because i wrote it in a garbage way, so i haven't 
gotten around to it. 

## Struct of Function Pointers

```
Widget :: @struct {
  data: rawptr;
  vptr: *VTable;
  
  VTable :: @struct {
    frobulate: @FnPtr(self: rawptr, enthusiasm: f64) i64;
  };
};

fn frobulate(self: Widget, enthusiasm: f64) i64 = 
  self'vptr'frobulate(self.data, enthusiasm);
```

The main benifit to this approach is that anything could be a Widget, 
its implementation could do anything, you know nothing about it,
and the compiler knows nothing about it. On the other hand, the downside to
this approach is that anything could be a Widget, its implementation could 
do anything, you know nothing about it, and the compiler knows nothing about it.

Many languages give you sugar for doing this:
in Rust it's `&dyn Trait`, in c++ it's virtual methods, 
in Java it's iterfaces (and also all methods). Currently 
Franca takes the C/Zig approach: good luck; you're welcome 
to write the code that one of the previous languages would have
generated for you. 

Depending on your personality, you may also be interested in trying 
hash table of function pointers, or even linked list of hash tables
of function pointers (congratulations: you've discovered SmallTalk). 
Beware the road to simplicity may lead instead to confusion. 

## Code Generation

If you want a less structured option without drinking the dynamic coolaid, 
you could try generating code. Franca gives you ways to write macros that 
work on AST nodes, generate source code as strings that can be imported, 
or generate IR that skips the frontend all together. These will have thier 
own documentation eventually. Just keep in mind it's a very big hammer. 
