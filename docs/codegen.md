# Codegen

The basic interface provided by the compiler is just running a text file of franca code. 
However, there are many apis provided for that code to generate new code and add it to the program. 

> This file shows you how to do it in your own programs.
> See [docs/comptime.md](./comptime.md) for examples of why it's useful.

## Macros 

> See lib/macros.fr for examples. 

Franca macros are functions that interact with the compiler during semantic analysis. 
They're similar to swift's macros except that they live in the same compilation unit as the rest of your program.  

When a function call expression is prefixed by a `@`, it becomes a macro expansion, 
which causes the following changes: 
- the argument expression is quoted (it is not compiled/evaluated and its type becomes `FatExpr`), 
  - similar to the `@{ (arg0, argN) }` syntax but doesn't insert an extra surounding block.
    multiple arguments will be passed as `Expr.Tuple`. 
- the callee declaration is required to be marked `#macro` 
  - the callee signeture must be `A :: fn(a: FatExpr) FatExpr` or `B :: fn(a: FatExpr, b: FatExpr) FatExpr`.
  - if it has two paremeters, the second is written after the call: `@A(a);` vs `@B(a) b` 
    (the later call syntax is only allowed for macros. `C(a) b` is an error). 
- the call is evaluated at compile time. 
  - compile time evaluation can also be used on normal values without the quoting by passing expressions to `@run`
- the return value (an ast node) is expanded into the callsite and then compilation continues. 
  - if produce a constant ast node some other way, it can be inserted into code with `@expand expr`

Alternatively: 
- `@f(a) b` is similar to `@expand f(@{ a }, @{ b })`
- `@run f()` is similar to `@expand { e := @{ f() }; e = compile_ast(e); @{ const_eval(@[@literal e.ty], @[e]) })}`

The body of the macro can be arbitrarily complex franca code or a simple template: 
```
// note the `@` on the body block. the expression is quoted, returned from the macro, not evaluated in the macro.
inc :: fn(a: FatExpr) FatExpr #macro = @{ @[a] + 1 }; 
// (y := @inc(x);) => (y := { x + 1 };)
```

### Ast Apis

> a trend you might notice here is many of these discriptions are cyclic.
> it's somewhat arbitrary what is implemented as a compiler builtin and what is implemented as user code. 
> there just needs to be some base case. 

The definition of the FatExpr type is in [@/lib/driver_api.fr](../lib/driver_api.fr). 
It's the same data structure the compiler uses internally 
(which in the long term is a bad plan for stability but we'll see how long it lasts). 

The compiler provides several functions for working with ast nodes:
- `fn compile_ast(e: FatExpr) FatExpr`:  
  - resumes compilation of the expression. this is useful because it will set `expr.ty` (a Type) so you can do your own type checking. 
  - note the difference between `e.ty` and `@type e`, the former is the type it will be when evaluated, the later is always FatExpr. 
  - avoids some redundant work if you duplicate the ast node in a bunch of places after calling this 
    (which is really something you shouldn't do but i'm not strict about it yet). 
- `fn const_eval($T: Type, e: FatExpr) T`:  
  - evaluates the expression at compile time and gives back a value that the comptime code in the macro can use. 
  - similar to: `fn($T, e) = { e = compile_ast(@{ @run @as(T) @[e] }); @assert(e.expr&.is(.Value)); T.ptr_from_raw(e.expr.Value.bytes&.rawptr_from_value())[] }`
- `fn literal(e: FatExpr) FatExpr #macro` 
  - convert a value into an ast node that evaluates to that value (as a constant Expr.Value). 
  - same idea as quoting with `@{ a }` except it can be a runtime value instead of a constant one. 
    like `fn(e) = @{ E :: @[e]; @{ E } };`, except that wouldn't work if `e` is runtime known at the callsite of the macro. 
    - ok: `@run { a := 1; e: FatExpr = @literal a; b := i64.const_eval(e); }` 
    - ok: `@run { a :: 1; e: FatExpr = @{ a }; b := i64.const_eval(e); }` 
    - ERROR: `@run { a := 1; e: FatExpr = @{ a }; b := i64.const_eval(e); }` 
  - `const_eval(@type a, @literal a) == a`
  - the macro `@literal` is the typesafe version of the primitive `fn literal_ast(T: Type, p: rawptr) FatExpr`
- `fn items(e: *FatExpr) []FatExpr`
  - access the elements of a tuple. 
  - since all arguments of a macro call are passed as one parameter, this is needed to get them out again. 
- `fn str(i: Symbol) Str` / `fn sym(i: Str) Symbol` / `fn c_str(s: Symbol) CStr`
  - access the compiler's string pool. ast nodes that contain identifiers represent them as symbols. 
- `fn compile_error(msg: Str, loc: Span) Never`
  - crash with an error message referencing a specific source location
  - e.expr.loc is a Span, and @source_location() can be a fallback for comptime code that doesn't have access to any ast nodes. 
- `fn debug_log_ast(e: FatExpr) void` / `fn debug_log_type(t: Type) void` / `fn debug_log_func(e: *Func) void`
  - print an ast node back as text so you have some hope of debugging while working on macros.

### Hygiene

Macros are hygienic. New variables declared inside a macro are not in the same scope as the calling code. 
```
f :: fn(e: FatExpr) FatExpr #macro = @{ x := 99999; @[e] }; 
x := 1;
@f(assert_eq(x, 1));
```
If you want to access a variable it must be passed in expcility. 
```
g :: fn(var: FatExpr, e: FatExpr) FatExpr #macro = @{ @[var] = 2; @[e] }; 
x := 1;
@g(x) assert_eq(x, 2);
```

## Import String

> `import("@/path.fr")` is similar to `import(@tfmt("@{ % }", read_entire_file_or_crash("path.fr")))`

A very blunt alternative to macros is calling `import("{ name :: value; };")`. 
When the string starts with a `{` it's treated as source code instead of as a file path. 
As with importing from a file path, it returns a ScopeId with which you can access the constants declared in the string. 
The string passed in can be any constant expression (which means it can be arbitrary code run at comptime to generate arbitrary code). 
This avoids interacting with compiler internals about the shapes of the asts. 
However, you lose the nice templating quote/unquote syntax and the parent scope is always the global one so there's no way to reference locals in the calling scope.

Imports of the same string are still deduplicated just like when its from a file path. 
If you need to reevaluate the constants for some reason (ie. if they have side effects) 
the hack is to string format a random number into a comment in the string. 
You should strive to have reproducible builds so prefer using some surrounding context to make the string unique rather than an actual random number. 

## Assembly

> See [@/lib/meta.fr](../lib/meta.fr) for various overloads of `fn AsmFunction();`

AsmFunction takes a function signeture and some code evaluated at comptime 
to produce some machine code that will execute when the function is called. 
You provide an implementation for each architecture you might want to target. 
- arm/rv: the value is a slice of u32 machine code instructions.
- amd/wasm: the value is a function `fn(out: *List(u8)) void`. 
  whatever bytes you append to that list will be inserted into your program as machine code.
You can import `@/backend/<arch>/bits.fr` for definitions of some useful constants. 

Since I need to support full compile time execution and also cross compilation, 
having a very structured system for target splits like this is very important. 

This is quite limited compared to more serious languages:
- There is no way to request relocations in your assembly so you cannot refer to other symbols 
(functions, constants, etc), you must pass in runtime known pointers as needed. 
- Assembly can only be inserted at a function call boundary. You can't mix franca 
and assembly in the same function body and assembly can't access local variables. 
- Since you can't access locals you need to know the calling convention the franca compiler uses. 
- On wasm it's extra limited because I don't expose a way to add entries to the global type table. 
- I don't provide an assembler so you have to construct the bytes with awkward function calls. 
However, since you can run arbitrary code at comptime, you can write your own assembler if you're 
excited about that. 

### asm usage examples

- `@/tests/fr/inline_asm_jit.fr` trivial examples just to show that it works. 
- `@/lib/sys/linux.fr/perform_clone` the clone syscall for spawning a new thread on linux returns twice 
  so it can't sanely be written in a high level language that might want to 
  spill variables on the stack (because one thread will return first 
  and stomp the stack space the other thread is still using). 
- `@/lib/sys/syscall.fr/perform_syscall` needs to use syscall instruction and remap to a different calling convention.  
- `@/lib/sys/jump.fr` implementations of setjmp/longjmp want to directly access callee saved registers. 
- `@/examples/os/kernel/start.fr/(interrupt_trampoline, return_to_user_impl)`
  need to save/restore all the user registers. 
- `@/lib/sys/process.fr/aarch64_clear_instruction_cache` uses cache control instructions 
  that are so rarely needed there's no point exposing them as a language feature. 

## examples/import_c

- see [import_c/README](../examples/import_c/README.md) for features/credits/etc.. 
- see header comment in [import_c/ffi](../examples/import_c/ffi.fr) for caveats for the franca integration.   

import_c isn't a magic builtin thing, it's just some code in the examples folder. 
it's a c compiler written in franca that uses the import_frc api to add ir and type info generated from c programs. 
however, it's so useful that it deserves some special mention. 
it can be used as a standalone c compiler (import_c/cc.fr), as a franca library (import_c/lib.fr), 
or as an import() alternative that accepts c instead of franca (import_c/ffi.fr). 

the end result of all that work is that this is a valid franca program: 
```
main :: import("@/examples/import_c/ffi.fr")'include("""
    #include <stdio.h>
    void main(void) {
        printf("Hello World!\n");
    }
""")'main;
```

Obviously that's a trivial example but, as is the trend, that string literal can be any constant expression. 
So you can run code that downloads a c libary, saves it somewhere, 
formats that path into a string with `#add_include_path` and builds that into your program.
Indeed most of `tests/external/*.fr` does just that. 

## TODO: document me!

- import_frc
- add_to_scope, intern_type, intern_func
- using the backend library to insert ir directly
- Talk about doing stuff from within comptime vs through a CompCtx. 

<!--
### reflection

- `fn resolve_overload(os: OverloadSet, arg: Type, ret: Type, loc: Span) FuncId`
- `fn get_function_ast(fid: FuncId, resolve_sign: bool, resolve_body: bool, infer_sign: bool, infer_body: bool) *Func`
- `fn get_type_info_ref(T: Type) *TypeInfo`
- `fn Tag(tagged: Type) Type`
- `fn size_of(T: Type) i64`
- `fn tag_value(E: Type, case_name: Symbol) i64`
- `fn get_meta(s: Type) TypeMeta`
- `fn type(e) #macro #outputs(Type)`
  - get the type of an expression (may run comptime code, will not run runtime code)
- `fn intern_type_ref(info: *TypeInfo) Type`
  - create a new type 
- `fn require_layout_ready(type: Type) void`

### scopes

- `fn import(descriptor: Str) ScopeId`
- `fn scope_from_value(ty: Type, ptr: rawptr) ?ScopeId`
- `fn get_constants(s: ScopeId) []Symbol`
- `fn get_constant(s: ScopeId, name: Symbol, type: Type, out: rawptr) bool`
- `fn get_constant(s: ScopeId, name: Symbol) ?Ty(rawptr, Type)`

### settings

- `fn get_comptime_environment() *ComptimeEnvironment`
  - for smuggling information between the compiler, the driver, and the comptime code. 
- `fn safety_check_enabled(check: SafetyCheck) bool`
  - read from build options (only exists because it's needed before EnumMap can be compiled when bootings)
- `fn current_compiler_context() CompCtx`
- `__builtin_compiler_has_feature :: fn(s: Str) bool`
- `fn ast_alloc() Alloc`
-->

