# Introduction

This is a brief overview that should show you enough franca syntax that you 
can start reading examples programs. Nothing here is a super cool language feature, 
it's just the basic stuff that most programs will use and has a direct translation 
into other languages. I suspect that if you've used any of (zig,rust,jai,c,c++,d) 
this will all feel fairly natural. If you get bored, go look at an example, and 
come back when you get confused. 

## Running Code 

To run a franca program, pass a file path as a command line argument to the compiler. 
That will just-in-time compile main() to native machine code and run it. 

The incantation for outputting an AOT binary is slightly longer but it also 
doesn't matter until you know more of the language. Anything that works
in AOT code will work in JIT code (if not, it's a compiler bug). 

## Everyone's Favourite Program

```
main :: fn() void = {
  println("Hello World");
}
```

## Declarations

Variables are declared with `name: Type = value;`.
Often the type can be inferred from the value, in which case 
you can leave out the type so `name := value;` (and that behaves like auto in c++). 

Assignment to an existing variable is `name = new_value;`. 

You cannot leave off the value part of a declaration. You can create a zero 
initialized variable with `t := zeroed T;` or an uninitialized variable with 
`t := @uninitialized T;`. It is considered disrepectful to read uninitialized memory 
or dereference zeroed pointers. 

Constants are declared with a double colon `name: Type : value;`. 
When the type can be inferred (which is almost always), leave off the type 
like before so `name :: value;`. The expression on the right is evaluated at compile time 
and cannot be modified (but the memory it refers to might be able to be modified). 

The most common example of a constant declaration is a function declaration. 
```
f :: fn(a: A, b: B) R = {
  // body goes here
}
```
That declares a function called `f` with two parameters (`a` of type `A` and `b` of type `B`), 
returning a value of type `R`. 

There is another style of function declaration (with no `::` and the name is flipped to the other side of the `fn`): 
```
fn f(a: A, b: B) R = {
  // body goes here
}
```
This is adding the function to an overload set called `f` instead of declaring a unique function. 
In this style, there can be many declarations of `f` as long as the argument/return types are 
different so the compiler can unambiguously tell which function you intended to call. 

A function parameter with a `$` before the name must have its value known 
at compile time for each callsite (this is spelled `comptime` in zig). 

Variables with the same names shadow each other (including when declared multiple times 
in the same scope, the new variable is used after its declaration). Constants can be shadowed 
in child scopes but since they are order independent, a constant can only be defined once in each scope. 

## Operators 

These all behave as you'd expect. 

- math: `+ - * /`
- assignment: `= += -= *= /=`
- equality: `== !=`
- comparison: `> < >= <=`
- not: `!`
- short circuiting: `|| &&`

However there is no syntax sugar for bitwise shift/xor/and/or/not, modulo, or exponentiation. 

Operators can be overloaded for user defined types but that's not important yet. 
By the time you need that you'll be able to find an example of how to do it. 

The operator `::` is reused (from declaring a constant), as a prefix it forces
an expression to be evaluated at compile time (this is spelled `comptime` in zig). 

## Pointers

This is the type of language where can screw up managing pointers 
because the languge was made by goblins in the 70s and just start 
trimming your os. Variables have an address in memory, represented 
as an integer that you can manipulate in your program. 

- The operator to dereference a pointer is a suffix `[]`. 
It means the same thing as suffix `[0]` in c which is more commenly written as a prefix `*` 
(but that tends to require extra parens). 
- The operator to take the address of a variable is suffix `&` 
(so it's on the other side of the expression as in c). Again this avoids parens. 
- Field accesses are auto-dereferenced. So if even foo is a pointer you can do `foo.bar`
without a special glyph for `->`. 
- You can create a null pointer with `zeroed(*T)` or `T.ptr_from_int(0)`. 
Generally you should prefer using a type `?*T` with the value `.None` when 
a value may not be present. 
- The operator for indexing a list/slice is suffix `[i]` (like most languages). 
That returns a value. You can do `.index(i)` to get a pointer to the element. 
- A pointer type is spelled `*T` (like Zig)

## Trailing Expressions 

When the last expression in a block does not end with a semicolon,
that value is implicitly returned from the block. 

```
add_one :: fn(a: i64) i64 = {
  a += 1;
  a
}
```

Most of the time you don't need a semicolon after a closing brace in the middle of a block. 
But you will see a lot of examples where I have extra semicolons because it used to be required 
so now i have the muscle memory to add them. 

## Calls

By far the most common operation is calling a function and franca has a variety of syntax options for doing so. 
For example, all the operators described above desugar to function calls. 

The basic call syntax is `f(a, b)` which calls `f` and passes it two arguments: `a` and `b`. 
The value `f` could be a compile time function or overload set OR a runtime known function pointer. 

You can also use dot calls if that feels more familier. 
The example above becomes `a.f(b)`, which means exactly the same thing (there's still no vtable lookup). 
Note this syntax conflicts with field access so sometimes you might need to do `(a.f)(b)`,
which means read the field called `f` on the value `a` as a function value and call it with 
the single argument `b`. As a less paren heavy option, `a'f(b)` means the same thing (`f` is a field not a function). 

When there is only one argument, you can write the call as `f a;` instead of `f(a)`. 

## Control Flow

The builtins provided by the language are `@if` and `@loop`, 
but you will generally access these through more convenient library functions. 

Execute one of two branches: 

```
if cond {
  // then body
} else {
  // else body
}
```

An if statement can also return a value:

```
a := if cond {
  then_body
} else {
  else_body
};
```

Loop until the condition is false: 

```
while => cond {
  // loop body
} 
```

Loop over all the items in a collection:

```
for collection { it |
  // loop body
}
```

## Inline Lambdas

Normal functions, as seen before, are declared with a `=` before the body. 
When you have a chain of function calls, each one gets its own call frame 
and can't access variables in its caller except those passed as arguments. 

Sometimes you just want to pass a little piece of code to a library function 
and it's really irritating to thread all the context you need through multiple 
levels of call. For this I provide lambda functions which are declared with 
a `=>` before the body. When you call a lambda function the implementation is 
pasted into that callsite (without creating a new stack frame at runtime) so it 
can access any variables from the caller's stack frame. 

Franca does not have real closures. Lambdas can not escape
thier scope and can only be called when the callee is constantly known. 
Since the implementation is inlined at every callsite, they cannot be recursive. 
You can have multiple levels of nested lambda but you cannot have a lambda that 
calls itself. 

There is lots of syntax sugar for passing lambdas as arguments to other functions. 
Additionally, lambdas do not require type annotations when they can be inferred 
by the function you're passing the lambda to. 

These all mean the same thing: 

```
for(items, fn(it: *T) void => body(it));

for(items, fn(it) => body(it));

for(items) { (it: *T) void |
  body(it)
}

for(items) { it |
  body(it)
}

for items { it |
  body(it)
}
```

And so do these: 

```
if(c, fn() T => a(), fn() T => b());

if(c, fn() => a(), fn() => b());

if(c, => a(), => b());

if(c, => a()) {
  b()
}

if c {
  a()
} else {
  b()
}
```

The type annotation for a lambda (or function) value looks like `@Fn(a: A, b: B) R`. 
So a function that takes a lambda parameter would be declared like: 

```
f :: fn($callee: @Fn(n: i64) i64, x: i64) i64 = {
  callee(x + 1) * 2
}
```

Lambda values can be assigned to constants just like normal functions: 

```
g :: fn(a, b) => a + b;
g(1, 2);
f(g, 5);
```

## Early Returns

Inside a function (that has a `=` before the body), you can use `return` to exit early. 

IMPORTANT FOOTGUN: in most languages `return;` will halt a void function, THIS IS NOT TRUE IN FRANCA,
you need to do `return();`, the former will just evaluate the value called `return` and discard it. 
Sorry about that! It makes porting code from other languages kinda suck because it's easy to make that mistake. 
When returning a value, it's fine to leave off the parens, `return x;` does what you'd expect. 

Inside a lambda (a fn with `=>` or trailing block to a call), you can use `local_return` to exit early. 
Franca does not have keywords for break/continue. However, the trailing block of an 
if/while/for/etc is in fact a lambda expression so you can use `local_return` to exit early. 
`local_return` always refers to the inner most nested lambda. However, it is a constant 
value of type `LabelId` and it can be given a name like any other value. Using local_return 
directly is almost never what you want, using it to create a named label is much more useful. 

```
if true {
  break :: local_return;
  while => cond {
    continue :: local_return;
    
    if a() {
      continue();
    }
    if b() {
      break()
    }
  }
}
```

## Allocators

When you want to dynamically allocate memory, you get it from an allocator (a value of type `Alloc`). 
An allocator provides the functions `alloc` to create new memory and `dealloc` to release it. 
Often you won't see direct calls to alloc/dealloc, you'll just pass an allocator to a collection 
(like a list or a hash map) that will handle the details for you. 

Some common allocator instances: 
- libc_allocator: Calls malloc() and free() from your system's libc. 
- page_allocator: Uses the mmap() and munmap() syscalls. using this directly is very slow. 
It should only be used as a root source of memory to be managed by another allocator.
- general_allocator(): This will be libc_allocator if you're linking libc or my slow block 
allocator if you're not linking libc. 
- temp(): This is an arena that gets reset at some consistant time in your program. 
So that might be after every frame for a game or after every requst for a web server. 
The idea is anything you know will only be needed within one of those reset cycles, 
you can just allocate in temp() and never free because it will disappear on its own. 
This resetting does not happen magically, you need to call `mark_temporary_storage()` 
and `reset_temporary_storage()` somewhere in your program. But for little scripts 
that just run to completion and then exit, you can just use temp(), never reset it, 
leak all the memory, and let the operating system disappear your address space in 
O(1) when you're done. 

In multithreaded programs `general_allocator()` will always be something thread safe, 
so it's fine to allocate something on one thread and free it on another. However, each 
thread gets its own `temp()` allocator, so you can't share that memory between threads 
unless you add your own synchronisation to force them to tick cycles at the same rate 
(which will probably defeat the point of having multiple threads, so don't do that). 

See `lib/alloc/` for some example allocator implementations. 

## Imports

Some code is always implicitly available to all franca programs (anything in `lib/core.fr`), 
but putting everything there leads to a cluttered and confusing namespace. So most library 
code must be imported explicitly if you want to use it. 

The `import` function will return a `ScopeId` whose declarations you can access with the `.` or `'` operators. 
When the argument to import is a string that starts with `@/` it is treated as a file path 
in the franca directory. There is a listing of the files you can import in `./lib_summary.md`. 

You can also say `#use(S)` to bring all the declarations in S into the current scope 
(so you can access them like normal variables). `S` can be a `ScopeId` from `import`, 
a struct with constant declarations, or a string (which is treated the same as when passed to `import`). 

## Comments

- C style `//` comments go until the end of the line. 
- Multiline comments with `/*` and `*/` (they nest like in rust). 
- You can use multiline string literals as comments if you're in the mood: `"""text""";`
- Since typechecking is lazy you can also comment out code with `@if(false) { body };` or 
`_ :: fn() = { body };`

## Literals

- numeric
  - decimal: 1234
  - hex: 0x04D2
  - binary: 0b10011010010
  - float: 1234.0
- string
  - "single line" (escape sequences like `\n \t \0 \\ \"`)
  - """multiline""" (no escapes, a backslash is just a backslash)

Numbers can have underscores. They have no meaning but may help readability: 1_000_000 == 1000000. 

## Coercion 

For the most part, Franca does not have subtyping. Each value has one type 
and can only be assigned to variables of that same type. However, there 
are some things that feel really bad without a bit more flexibility. 

- Numeric literals coerce to int/float types if they're in range
- String literals coerce to Str (slice of bytes) or CStr (pointer to null terminated bytes)
- Constant function values coerce to runtime known function pointers
- Function pointers coerce to untyped `rawptr`
- Enums coerce to thier raw representation and back with an explicit `@as(Type) value` cast

Also keep in mind that you can have functions that overload on thier return type. 
This can look like there is one function that returns something that can coerce 
to many different types. Instead, it is many different functions that each 
return exactly one type and the compiler is choosing which to call based on what type 
you're asking for. 

There's a limit to how deeply nested the compiler is willing to 
infer types when you have rely on this, sometimes you will need to add `@as` casts 
to give an expression a result type hint. This avoids a super slow (*cough* swift *cough*) 
exponential search of all the overload paths. A significant language design constraint 
is that have exactly one second of patience for waiting for the compiler to compile itself. 
So whenever it's slower than that I have to drop everything and optimise it or take out 
language features until it stops being painful. 

TODO: Since I am bad at my job, the way coercion interacts with function overloading 
is not super well thought out or consistant. For now it mostly just works out how I 
kinda feel like it should and doesn't get in the way of writing useful programs. 
Eventually I need to get serious and rework it into something sane that I can describe. 
Today is not that day. 

## Macros

Macros are a special type of function call. 
Instead of passing values to some code that executes at runtime and returns a value, 
you're passing AST nodes to some code that executes at comptime and returns an AST node. 
An Abstract Syntax Tree is how the compiler represents your program internally while doing 
semantic analysis. 

The macro system deserves its own more detailed documentation but the important thing 
to know is that macro calls start with a `@` and do not eagarly evaluate thier arguments. 
How the argument expressions are used is entirely up to the callee. This provides some 
extra flexibility to create constructs that can't be expressed as normal franca code. 

Macro calls look like basic function calls `@f(a, b, c)` or no parens if there is only 
one argument `@f a` but can also have an extra expression after the closing paren like `@f(a) b`. 

If you come across `@{ foo }` and `@[foo]` it's probably good to know that those 
mean quote and unquote but that's not important yet. They're useful when writing macros 
not when using macros. 

## Evaluation Order

- fields of struct initializers are evaluated left to right (regardless of field order in the declaration)
- function arguments are evaluated left to right (this includes binary operators)
- assignment statement are evaluated right to left (the value before the destination)
- macros can rearrange their operands to evaluate in whatever order they want
