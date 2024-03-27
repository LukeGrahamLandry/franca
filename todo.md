## Feature Ideas

- fold infinite loops so it knows when you dont need a return at the end of the function to typecheck
- mix named and positional args
- default arg values (any const expr and inject at callsite? or based on other args so generate shims or multiple entry points to the function)
- anon structs for named return values. maybe commit to no !struct but what if you want an enum maybe its better to have no blessed default case
- maybe anon struct literals with inferred type should be fine.
- !defer
- Box(T) and box(t) for quick allocations
- fix impl generics now that public vars work
- give macros access to type info for auto debug printing
- const SafetyCheck = @flagset(Bounds, Overflow, DivByZero, UnreachableCode, WrongEnumTag, CastBounds, Align, FfiNull, UseUninitCanary);
  safety(Bounds, fn() = lt(i, len(self));
- deref for constants,
- inferred container type for constants. so fn a(b: @enum(A, B)); can be called like a(.A);
- enum bitset
- c style flags: fn Enum(Arr(Symbol)) Type; so Enum(A, B, C) === @enum(i64) (A = 0, B = 1, C = 2). could be macro so don't have to deal with passing symbols.
- quick union types: fn Enum(Arr(Type)) Type; so Enum(A, B, C) === (A: A, B: B, C: C)!enum;
- user defined operators so you can pick what meaning of &T makes sense for you (per module)
- some sort of prefix call syntax because brackets are annoying.
  - maybe Rc(RefCell(T)) === Rc$ RefCell$ T
  - tho I do already have T.RefCell().Rc() but that seems backwards from how I want to think about it but I'm not sure why.
    "an Rc containing a RefCell containing a T" vs "a T in a RefCell in an Rc"
    Ptr$T "a pointer to a T" vs T.Ptr() "a T that is behind a pointer"
  - Rc$ T.RefCell() === Rc(RefCell(T))
- function that take a slice of args called like variadic functions.
- field auto ref/deref for primitive types?
- my expression output thingy broke some struct stuff (aliasing + asm consecutive slots)
- be able to address a byte so you can use libc for stuff

## Sema

- remove the need for forward declarations
- module system: want to be able to seperate things to help lsp.
  - hard to think about how that should interact with wanting to use global overloads as traits.
- nominal type-checking
- clean up tracking backend specific function bodies
- transitive function annotations (@env, @ct, eventually @async) where you get one if you call someone that has one.

## Replacing Interp

- give executors access to the compiler so dont need the hacky message passing with errors
- asm stack traces
- convert bytes + type back to Values
- asm needs to impl Executor. seperate storing bytecode from interp.
- fix any_reg. asm backend needs access to an executor

## Backend

- get llvm backend to parity with aarch64
- llvm output an executable
- using mir for compiling c dependencies would be cool
- figure out if llvm-sys statically links itself and if it can cross compile
- reference counting and deduplication of heap constants
- be able to serialize asm for any function so can cache macro handlers. is it faster to hash source than recompile?
- explicit uninit vars to make asm allocate slots
- trying to call print on asm tries to call builtin alloc somehow
  I think the problem is that you need two versions of things that are called at both comptime and runtime when not for the same architecture.

## Ui

- repl
- pass source as cli arg
- support shebang line

## Lsp

- factor out lsp dispatch
- more tolarant parser. Need to be able to represent holes in the ast.
