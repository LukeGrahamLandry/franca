- !!!!!! something makes some asm tests fail intermittently.
  (signed_truncate range check is only in debug so aslr or whatever messes you up somehow and then in release you just get garbage instructions?)

## Deeply annoying

- no u32/u16/u8
- constants must be forward declared. no mutual recursion
- comptime runs on interp instead of asm
- no typechecking through fn arg/ret so if/loop body closures need annotations
- can't output an exe, only jit
- no way to say trait bounds on generics (it just tries to compile like c++ templates)
- need to explicitly instantiate generics
- asm no print stack traces
- stdlib isn't in seperate modules. need to allow nested modules.
- no nonlocal returns for break/continue
- test runner glue code is done in rust instead of fancy metaprogram
- no string escape codes like "\n"
- sometimes it can't show the codemap. should never use garbage_loc.

## Feature Ideas

- adding 'e->name(a) === (e.vptr.name)(e, a)' would make <- the "function oriented programming" operator and -> the "object-al programming" operator.
- need chained compile errors.
  - maybe loc should be a specific token so you don't highlight the whole expression just to complain about a function signature
  - macros should show both expansion site and code that generated it.
- annotations should be powerful enough to derive recursively eq/clone/drop/hash based on fields.
  - need to let you access the default even if you override.
    like adding extra drop logic shouldn't mean manually dropping all fields.
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
  safety(Bounds, fn= lt(i, len(self));
- deref for constants,
- inferred container type for constants. so fn a(b: @enum(A, B)); can be called like a(.A);
- enum bitset
- c style flags: fn Enum(Arr(Symbol)) Type; so Enum(A, B, C) === @enum(i64) (A = 0, B = 1, C = 2). could be macro so don't have to deal with passing symbols.
- quick union types: fn Enum(Arr(Type)) Type; so Enum(A, B, C) === (A: A, B: B, C: C)!enum;
  - if you had all the tags unique (use TypeId) instead of starting at 0, you could make it free to convert to a super set.
    that probably gives up u8 or niche tags but even u16 is enough that its fine.
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
- let macros code generate a string and compile that
- panics show stack trace
- super cold calling convention for panics with constant args where you use return address to lookup arg values (i feel like i can do better than rustc which is really strange)
- asm panic on invalid enum field instead of faulting
- explicit runtime data struct.
- feature to turn off backtrace-rs dependency
- u32/u16 pointers as indexes into per type arrays. deref trait so that can be a library feature? want to be able to toggle easily not at every use so can benchmark
- command line argument parser
- getters and setters so enums and flag sets could be done in the language and still have natural syntax.
- free standing versions of functions. so like during comptime you want the compiler to control allocations/printing/panics probably
  but need to be able compile a real binary too. this gets back into the problem of compiling anything used at both multiple times.
  do you try to represent that in the ast so they share work or have fully seperate Func instances for anything that indirectly calls an env function.
- have addressable bytes (u8, u16, u32, u64) and measure size_of in bytes.
  - then you could reliably do c ffi
  - seperate logical (pattern matching) size from real size.
- c struct padding logic for ffi
- expose c variadic functions on llvm (because why not)
- add @rt for freestanding which denies calling @ct fns and can resolve overloads.
  - tho I suppose you should be able to still call @rt at compile time if no overload conflict? or does that make it pointless?
  - one of the usecases would be having panic be exit(1) when freestanding but otherwise just hook into the compiler's error mechanism
    so you don't just want the arch to be what decides.
- should really have a more extensible way of describing an environment.
  - ie. what arch? do you have the compiler context? what os? do you have libc?
  - the end goal for this would be to expose what imports each module needs and let the comptime resolve differently if you really wanted.
- real closures
  - I don't want to implicitly heap allocate escaping ones but should be able to expose the list of captures and fill in a struct of pointers.
  - my goal would be writing rust like iterators but without making you explicitly put your locals in a struct
  - need to be careful about what happens if you hold on to internal pointers, then it can't move.
  - could just say you have to put it at the outer stack frame and not let it escape.
  - would be nice to change between current inlining and closures without changing source so you could opt for size or speed.
- https://github.com/arun11299/How-not-to-async-rs/tree/main
- something that tried randomly deleting statements in your files to warn you if tests still passed would be cool.
- let macros access requested type (of thier call site, not just infered of thier args) or provide an infered a type before processing.
- split up different types of casts. @as is weird. Don't do the implicit voidptr cast based on result location.
- should expose untagged unions because you might need them for ffi.
  tho really that can be done with a blob of bytes and casting through a void pointer.
  so would be cooler to add more powerful custom types and do unions as a library.
- a way to say ne=not(eq) for any type that doesn't already have a ne impl
  - ideally you could just inline the eq and distribute the not so it would never matter if you couldn't define your own ne (other than order)
  - feels sad if my asm backend doesn't know it can do int ne in one instruction,
    but for comptime, its probably not worth an opt pass that tries to fix conditionals.
- comptime code should be able to just compile a string which means

## UB

- (CHECKED) accessing the wrong enum field
- holding a pointer to an enum member while changing its tag
- read off the end of an array
- div(0)
- overflow/underflow. add,sub,mul
- bitshifting too far
- constructing a slice with a bad length
- escaping pointer to stack
- double free, use after free
- holding pointer accross a collection resize (common case of ^)
- alias block result location when returning a struct without calling a function
- void pointer cast
- hacky rust pointer ffi
- inline asm

## Testing

- clean up the way I do tests.
- be able to write a thing in my language that says which backend combinations to test on each snippet
- so need to expose apis for compiling source to my language. does that mean I should allow runtime @ct and have you opt into bundling the compiler?
- output assert_eq counts to stdout and check them from another language so it can't cheat?
- replace shell scripts with my language?
- should really make the canary thing optional.
- define assert_eq in my language so I can use freestanding. maybe have expect_eq that calls record_passed_assertion for currect sanity counting.
  but maybe its too weird to say you can't use that in loops because it counts lexically. its so comforting tho.

## QBE

- https://c9x.me/compile/
- need to expose a function that I can call on a string of ir instead of using them as an executable that reads a file
- make sure the reason they don't expose a lib isn't because they never free anything
- they emit assembly not code so I have to write an assembler
  - which would be nice to have anyway because then I could have clang output asm for other libs and link them into my stuff that way
- setup benchmarking system so I can make sure its dramatically faster than my own shitty asm
- does it make sense to put all thier statics in a thread local and hope that makes it thread safe?
- figure out how to make a memory backed \*FILE

## Sema

- remove the need for forward declarations
- module system: want to be able to seperate things to help lsp.
  - hard to think about how that should interact with wanting to use global overloads as traits.
- nominal type-checking
- clean up tracking backend specific function bodies
- transitive function annotations (@env, @ct, eventually @async) where you get one if you call someone that has one.

## Replacing Interp

- give executors access to the compiler so dont need the hacky message passing with errors
- convert bytes + type back to Values
- asm needs to impl Executor. seperate storing bytecode from interp.
- fix any_reg. asm backend needs access to an executor

## Backend

- get llvm backend to parity with aarch64
  - convert struct arg/ret to pointers
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
