# Annotations / Tags

Annotations are a little piece of metadata you can attatch to functions, parameters, declarations, or struct fields. 
Same idea as `#[attributes]` in rust, `@annotations` in java, `@tags` in jai.
I use the term "annotation" and "tag" interchangeably, but tag is shorter to type. 

The general rule is that you can put whatever tags you want on things and they won't do anything 
unless some other code reflects on them and does something useful. However, there are some 
where that other code is the compiler itself so they have a special meaning (see the "builtins" section below). 

An annotation consists of a `#` and then a name. Optionally it can take arguments 
(surrounded by parens, like a function call). Those arguments are arbitrary expressions 
(any type checking is done by the consuming code). Generally tags come after a type 
annotations (which is a confusing reuse of that word, sorry!). There can be multiple. 

## Reflection 

Your comptime code can inspect the annotations present in your program and do whatever you 
want with that information. 

- `get_function_ast` is a builtin that returns a `Func` which has an `annotations` array. 
the `arg` field has `Binding`s which has a `nullable_tag` field.
- `get_fields` gives a list of struct fields which have a `nullable_tag` field. 
- in a macro you might find a `Decl` or `StructLiteralP` ast node which holds a `Binding` which has a `nullable_tag` field. 

## Real Usecases 

- asking the compiler for special treatment without adding new syntax (see builtins below)
- generating cli help messages (lib/args.fr used by backend/meta/test.fr)
- adding extra metadata for translating to MSL (graphics/shaders.fr used by graphics/debug_text.fr)
- creating a table for a parser that calls a function depending which token it sees (examples/import_c/compile.fr)

## Syntax Example 

This is not what normal code looks like. It's purposely trying to spam as many as possible. 

```
Noun :: @struct {
  a: T #tag(value) #thing = 123;
  b: U #bar;
  c: V #align(7 + 1);
  D: E #F : G;
}

#outer 
foo : FuncId #inner : fn(a: T #hello("world"), b: U #hi) Ret #after = {
  for a { (c: TT #ann) #otation | 
    g: H #i = body(c);
  }
}

foo2 :: foo;

#a
fn overloading(b: C #d) E #f = ();

bar: Baz #qux : quux();
```

## Rough Edges 

- Sometimes it's confusing which item a tag will apply to. 
In the above example, `#inner` applies to the name binding `foo` while 
`#outer` and `#after` apply to the function value. so foo2 will have the latter but not the former. 
- If you typo a tag, you won't get a compiler error because nobody knows that someone else isn't looking for it. 
- The syntax looks the same as statement level annotations like `#use(S);`. 
Especially confusing in cases like `#outer` above where it applies to the function 
but wouldn't if you put a semicolon after it. In that case it would be a stand alone statement 
with a totally different meaning. 
- TODO: you can't get the tags out of a constant declaration once it's been baked into a scope

## Builtins

Some annotations have special meaning to the compiler. 
But since the franca compiler is just another franca program that looks
at your program, these aren't even that special. The only difference 
is that I wrote it instead of you. Fundamentally it's the same idea 
as when your program introspects on itself. 

### macro

Instead of a normal function call, the body is some code that will run at comptime for every 
callsite. You're passed AST nodes of that call's argument expressions and return a new AST 
node to paste in place of the call. Since the arguments are not fully type checked before 
you get them they can contain invalid code that only has the meaning you choose to give it. 
You can call functions in the compiler to do useful things with those nodes. For example, 
`const_eval` to execute an expression and get a comptime known value or `compile_ast` to 
just type check it like normal. 

This is similar to swift's macro system, except that you don't need to define the macro 
in a seperate compilation unit. Everything is transparently jitted as needed. 
Unlike rust's macro system which works on tokens, the arguments to franca macros must 
parse as valid franca syntax. If you want to invent new syntax, you can put it in a multiline 
string literal `"""` and then parse it however you want (see `@emit_instructions` in `backend`) for 
an example of that. 

### fold

If all the arguments to a call to this function are constant, evaluate the call at comptime instead of runtime. 
You probably want to be careful to not mark functions with side effects as `#fold` because 
that will do the side effects at comptime instead of runtime. The compiler does not 
enforce sanity. 

### where

Allows the functions to have arguments with type annotations prefixed by a `~`, 
which will have a new version synthesized by the compiler for each unique set of types 
used at a callsite. The argument can be a function that takes the polymorphic types 
and returns a bool depending if that instantiation should be allowed (so you can use it 
to implement a trait system). 

This is an incomplete feature, sometimes expressions will be too large to immediately 
know the type and it won't compile. It's also complicated enough that it deserves its own 
documentation. 

### import

This function is implemented in an external library. The argument is a string (the name of the library to import from). 
You can use `add_comptime_library` (generally from a driver program) to register a library handle 
(from dlopen) with a specific name, and then functions that import from that name can be used at comptime. 

### libc

Like `#import("libc")` but gets special treatment from the compiler because this came first. 
Should probably be removed and libc should just be treated like a normal library. 
It's just hard to think about because you can't really do anything until you can allocate memory, read files, etc. 
So if you want to implement `import` as normal comptime code, you really need to already have access
to basic OS stuff that you'd generally get from libc. 

### ir

Used internally to implement functions that should map to some cpu instruction 
the backend knows how to generate. The argument is an operation and a primitive type. 
The correct machine code will be inserted at the callsite instead of a function call. 

### redirect

Instead of providing a body, have calls redirect to another function instead. 
The argument is two types `(Arg, Ret)` of the implementation in the same overload set to redirect to. 

It's kinda useful if you're doing a "newtype idiom" thing where you know your type has 
identical representation to some other type and want to inherit some behaviour. 
For example, you might write `FuncId :: @struct(id: u32)` so you get type checking 
when passing those around but you want to implement `==` as just doing whatever 
would happen for a normal `u32`. 

This is very error prone because it relies on knowing that the calling convention will be the same 
for both sets of types. It is considered disrespectful if you make a mistake. 
This should probably be removed, it just felt cute before i had an optimiser. 

### log_asm, log_ir, log_ast 

print out some information about the compiler's internal representation 
of a function. The argument of log_ir can be a string with different letters corrisponding to 
different phases of the backend to inspect. See `backend/ir.fr/Qbe'DebugKey` for a list of valid letters. 

This is mostly useful for debugging the compiler. Using on large functions
will probably just be a lot of useless spam. If it was less painful to compose 
driver programs and the compiler gave them a little more information, 
you could replace `log_asm`, `log_ir`, and `log_ast` with something in "userspace"

### inline

When you call this function its body will be pasted into the caller instead of generating a function call instruction. 
Inlined functions cannot be recursive. 

### noinline

Prevent the compiler from optimising across calls to this function. 

### use

This can be used on functions and on fields. 

TLDR: `#use(Foo)` is like `from Foo import *` or `use crate::Foo::*`.
When used on functions, it takes a scope as an argument. A scope could be a ScopeId, 
a Type, or a string as passed to `import()`. The constant declarations of that scope 
will be accessible in the body of the function. This is the same as `#use(S);` 
at statement level. It's fine to create cycles with mutually recursive `#use`s. 

When used on a field, you can access subfields on the child struct as though they were on the parent. 
Provides a subset of the convience of inheritance. Same idea as anonymous fields in c. 
It does not affect overload resolution or type checking. The `#use`d field can be behind a pointer, 
so it's less opinionated about layout than most Noun Enthused languages. 

### reexport

When you have a chain of `#use` on a scope, constants are only available one level up. 
So you if you `#use` some code from a library, you don't get the implementation 
details of what libraries they themselves `#use`d dumped into your scope as well. 
`#reexport` changes that behaviour so a library can organize things into multiple scopes 
internally but provide a flat namespace to consumers. 

### generic

Forces parameter binding to go left to right so later type annotations can refer 
to previous constant arguments. This allows you to write polymorphic functions 
in a style similar to Zig. 

Having this be something you ask for verbosely is stupid and i should just make 
this work automatically. It only exists as a special annotation because I was lazy 
when writing name resolution in the bootstrap compiler and haven't bothered to change it. 

### duplicated

This goes on function parameters. When the parameter is an inline lambda like `$body: @Fn() void`, 
the compiler requires that it only have one callsite. If you're going to call it multiple times, 
you need to explicitly mark it as `#duplicated`. 

This is an arbitrary requirement that would be trivial to remove. However, since I don't have real 
closures, lambda calls like that are always inlined into each callsite so if you duplicate them a 
lot, you can end up bloating your executable more than expected. So I personally like having a 
syntactic reminder that something weird is going on. Maybe this should be moved to a driver 
check that you can choose to enable if you care a lot about binary size. Maybe I should just 
implement real closures and fallback to that instead of generating redundant copies of the same code. 

### intrinsic

Used internally to implement builtin_get_dynamic_context/set_dynamic_context. 
In the old comptime jit, all math operations used this but it has been mostly replaced by `#ir`. 
Should probably be removed and merged into something else since it's used so little. 

### comptime_addr

Used internally to implement the builtin comptime functions. 
The argument is an integer that is a function pointer in the comptime address space. 
Instead of compiling a function body, the compiler will just call that pointer directly and hope for the best.  
It's sort of like baby's first dlopen. 

### once

Assert that this function only has one callsite. 
This should probably be removed and done with a driver program instead. 

I find this useful sometimes when I'm writing a function just to give 
a name to a block of code instead of to reuse it. Sometimes there's some 
subtle invarient that's not obvious from the name and it's nice to force you 
to revist that when you add a new callsite. Since it's my language I get to 
play with my random dumb ideas and you have to deal with it, sorry :)

### outputs

The argument is of type `Type`. 
Put this on a macro when the type of the resulting expression is statically known without expanding the macro. 
In the bootstrap compiler this was used to simplify type inference but it is currently unused. 

### unsafe_noop_cast

Make the function compile to nothing (just a bit cast). 
It is considered disrespectful if the argument and return types have different memory representations (size/alignment). 
This should probably be removed. It just felt cute before i had an optimiser at all (to avoid a load/store through a pointer cast). 
But now the sane option is just to write out the body and hope it disappears anyway. 

### avoid_shim

This is for functions where you have to lie about parameter types like objc_msgSend.  
TODO: explain why
This is implied by CVariadic

### align

This applies to struct fields. The argument is an integer (number of bytes). 
Normally padding is inserted between fields to ensure everything is aligned to 
thier natural alignment (ie. the address of an i64 should always be a multiple of 8), 
because that's what c compilers do (because that's what old cpus liked) so it makes 
ffi with other languages much easier. But sometimes you want more control over your 
data layout so this annotation lets you set your own alignment requirements for 
each field. For example, putting `#align(1)` on all the fields would be like `#pragma pack` in c. 

This should probably be removed and done in "userspace" since i barely use it, 
and it's pretty easy to make your own TypeInfo (with the field offsets already filled out) 
and submit it to the compiler. However, implementing `#align` as a compiler builtin 
inexplicably made the compiler noticably slower (30ms on self-compile), 
so I want to understand what's going on with that before fix the problem accidently by 
just taking out the slow code. 
