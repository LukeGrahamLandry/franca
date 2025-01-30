## graphics

- finish updating documentation to not use c syntax
- translate a subset of my language into the shader languages
- use []u8 and interpret_as_bytes instead of SgRange
- :GfxLogging

##

- check enum names are unique
- this should work
```
A :: @struct();
B :: @tagged(a: A);
b: B = (a = ()); 
b = .a;
```
- backend fails_typecheck make sure sel0:sel1 and blit0:blit1
- fold.fr:
//!     TODO: I think it would be much faster if the frontend just tracked what 
//!           should be constant, and only materialized it upon seeing a 
//!           non-arithmetic #fold, and otherwise just defer until the backend.  
- is :jnz_is_Kw really what i want the semantic to be?
- auto #fold functions returning StructLiteralP (same as already do for Value).
ie. `fn init() Self = (arr = @as(Slice(Entry)) empty(), len_including_tombstones = 0, capacity = 0);`
- RSlot overflow
- make trailing_zeros(0) == 64 work on x64
- finish #x86_bytes tests: call_in_stack, throw, try, truncate_to_byte, read_pair, write_pair, both_pair
- detect if you try to do the jit thing with the backend library from jitted code on macos instead of just `bus error`-ing
- make `#where` based on argument names instead of order.
- `#where` access constant arguments of the call
- 🤡 don't fork+exec for touch+chmod 🤡 (+ fix my write_entire_file to create it if needed)
- figure out what `-Wl,-z,common-page-size=65536,-z,max-page-size=65536` does so blink can run my elf faster
- wasm stackifier
- cross arch repro is broken again
- fix compiler_gui so it sees all functions from a full compile
- #syscall
- output_elf: link libc, relocatable object, dynamic library
- Wasip1Libc
- still allow coerce to c string if there was a `\` escape.
- `[CPU Time]` on macos-x64 is wrong. (different libc magic numbers? clang libc_constants says no). // :WrongClockTime
- maybe have a `push_compiler_error_context` and `pop_compiler_error_context` for macros so @fmt could easily add a message like `while evaluating format string`?
- make the old jit backend runnable again as an example
- repro doesn't work when you do `-repeat`
- be less strict about amd64 address folding when there's a large constant pointer (which is valid when jitting)
- don't hardcode page size to 16k
- :TodoAmdFoldOps
- bring back nice error message for trying to call a #ct at runtime
- things that should be cleaned up next time i replace `boot` :UpdateBoot
  - :DumbNameAlias NoMangle
  - AbiHackSpan
  - :InlineFoldHack `@if(::!T.has_pointers()`
  - fn zeroed
  - .sym().c_str()
  - `@rec` in backend/ir.fr and wasm/instructions:Wasm
- more calling convention tests between jitted code and c.
- make (logical not) an intrinsic
- bring back lex error for unterminated comment.
- `Illegal place expression: GetNamed` is not as helpful as "tried to assign to undeclared variable %"
- αcτµαlly pδrταblε εxεcµταblε
- probably want to port the build stuff. https://github.com/jart/cosmopolitan/blob/master/tool/build/apelink.c
  i assume i can make it less complicated since i'm the one generating the thing in the first place?
  and i only care about linux and xnu. windows can just use wsl.
- catch multiple branches with the same switch value.
- unpack `a, b, c = call()` without declaring new variables
- check that the keyword between block arguments is correct (ie `if a {| b } else {| c }` should require `else` not some other identifier),
  and use that as an argument name for overloading.
- have a test where you force inline everything that's not recursive to stress test the backend dealing with large functions.
- broke repro again. is there a certain size of program is doesn't like?
- fix run_tests/qbe-jit and add that to CI
- make macros more similar to functions. it's weird that the syntax is slightly different.
  you should be able to overload off some argument types and have others be a symbolic ast node?
  but that would make overload resolution even more fragile which might be a bad idea.
- `Type Error` should tell you where the difference is! and use the right names for enums, etc.
- `place expression expected pointer dereference` when you forget a `[]` after a call and field access should tell you that.
- either fix new emit_ir run_tests naming or just change run_tests to generate a big file and compile it as its own thing.
- combine include_bytes and #include_std somehow
- @match/@switch should just be @inline_match/@inline_switch if the thing is constant.
- `fn fmodf(f32, f32) f32 #libc;` should treat them as types not arg names
- try semaphores
- rename RsVec. vector is a stupid name for that
- fix overflow when lexing large float literals
- make compilation order of struct sizing less sketchy for offset_of
- propagate types through constants. `a: u32 : 0; b := a;` b should have type u32 even though int literals default to i64.
  (to fix `foo.id != SG_INVALID_ID.trunc()`)
- show const fields better in log_type
- (cap, alloc) args are swaped between list:List and init:RsVec

// - less verbose if let/let else? think about how to express with a macro.
// - auto coerce payload to enum when branches aren't ambigous? maybe? thats a bit creepy. doing it for unit when you just forget to return something would be bad.
// - default function arguments because 'self.parse_expr(Prec.None)' and symmetry with structs.
// - track all the errors you hit when failing overloads.
// - local_return and return implciitly rebinding is a bit confusing

##

- fix all the places i `:HardcodeOs`
- #target_arch
- have one test command that runs all the code that exists.
  also rn examples are only jit by run_tests.fr (not llvm aot).
  should have a more declaritive way of describing what programs exist such that they can be run by multiple backends.
  ie. i want to add cranelift, wasm and qbe to the actually tested category.
- wasm's totally broken.
- i still think wasi's kinda dumb but running the compiler in a browser would be very pleasing.
  tho i could just use blink https://trungnt2910.com/blink/blink.html
- support loading driver from a dylib becuase you can't load the driver without a jit
- automated test that builds are still reproducible
- default function arguments and mixed named/positional.
- shims advanced version: don't comptime jit until the first time you call something. can't decide if thats too creepy.
- finish getting run_tests to run on blink
- make github actions fail if a test fails (currently it only requires examples to work).
- try to give llvm less work to do. clean up first call to intrinsic/redirect (it hurts deduplication! i have a billon fn alloc now).
  would it help if i did less dumb inttoptr and add 0 for int constants?
- be nicer about warning invalid arguments in examples/default_driver and compiler/first
- clean up what goes in lib/build.fr vs lib/sys/fs.fr
- instead of QbeIr+LlvmIr+X86AsmText, just have `IrText(type: Symbol, code: Symbol)`,
  so the compiler doesn't have to know about all backends.
  and then backends can just recognise whatever `type`s they want to support.
  have the `#asm` tag take a string argument.
- parse zig headers and generate extern declarations (like examples/c_bindgen). then i could use bits of thier standard libary.
  they have a bunch of fun stuff that doesn't use use comptime in the api and im not interested in writing myself:
  http, hashing, random, zip, magic numbers for wasm/elf/dwarf/macho
- should make it easy to build rust/zig projects from your driver program since they can generally cross compile well and have libraries you might want.
- there's a few places `defer` would be really nice to have
- do i want to expose the idea of different libcs? musl vs glibc vs cosmo
- would be cool to transcribe my own linux syscalls
- have it download stuff for you (ask first!) like qbe, sokol, llvm?
  at least make the error messages for missing stuff more helpful.

```
@impl(fn(T) = T.is_sequential_enum()) {
    T :: @placeholder 0;
    fn eq(a: T, b: T) bool = {}
};
```

this is much nicer but needs new syntax:

```
#where(fn(T) => T.is_sequential_enum())
fn eq(a: ~T, b: T) bool = {}
```

##

- generic Read/Write instead of hardcoding List(u8).
- handle #generic in overloading.rs so you don't have to add useless @as casts in nested expressions.
- seperate fn stride_of and fn size_of so you can avoid extra padding in nested structs.
  then need to allow different reprs.
- sign extend
- test that struct padding is zeroed before emitting comptime data and before being used as a key to cache const args instantiations.
- bake_relocatable_value for List, Alloc, Fd.
- deduplicate constants (strings especially), its just annoying cause i don't have const pointers.
  can do it by address at least, cause if it was a string literal in the pool, will be the same.
- make the quote syntax not make a redundant block for single expressions that you have to manually get rid of if you want to access the actual node.
- combine places that do multiple walk_ast/clone passes (renumber/unmark_done)
- auto test repl. at least compile all the example programs in run_tests.fr, at least compile the compiler.
- `name :: @merged { fn() #asm #aarch64 = ...; fn() #asm #x86_bytes = ...; };`
  so you can use inline asm without making an overload set.

## data structure changes

(when compiling the compiler) need to seperate the ast types used for the comptime interfacing with the active compiler and the ones used internally by the new compiler.

- 16 byte RsVec because address space is really 48 bits probable so 48 bit ptr and len and spread cap accross the upper 16 of each.
- use null pointer as niche for `?*T`
- make .None tag be 0 so zero initilized is a sane default (just need to swap the order in the declaration).

## regressions

- @tagged tag check on field access

## Deeply annoying

- no way to say trait bounds on generics (it just tries to compile like c++ templates). at least have fn require_overload(Ident, Type, Type);
- need to explicitly instantiate generics
- can't write a macro that expands to statements
- errors don't show multiple locations (like conflicting overloads should show the problem)

## Feature Ideas

- should make a derivable (fn arbitrary(rng) T) for testing hashes or stuff on psudorandom valid values of a type.
  like eq and hash would be nice to test like that. also ffi with c.
- loops piss me off, why isn't it just expressed as tail recursion.
- https://llvm.org/docs/CoverageMappingFormat.html https://llvm.org/docs/LangRef.html#llvm-instrprof-increment-intrinsic
- good error message for accidently using ' as character literal.
- embeding other languages would be a good demo of the comptime/meta programming stuff.
  - need a raw string syntax that passes it to a macro (like nim?)
  - lualit would be cool cause they have c abi stuff and dynamic language so notably different from mine.
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
- fix impl generics now that public vars work
- const SafetyCheck = @flagset(Bounds, Overflow, DivByZero, UnreachableCode, WrongEnumTag, CastBounds, Align, FfiNull, UseUninitCanary);
  safety(Bounds, fn= lt(i, len(self));
- enum bitset
- quick union types: fn Enum(Arr(Type)) Type; so Enum(A, B, C) === (A: A, B: B, C: C)!enum;
  - if you had all the tags unique (use TypeId) instead of starting at 0, you could make it free to convert to a super set.
    that probably gives up u8 or niche tags but even u16 is enough that its fine.
- user defined operators so you can pick what meaning of &T makes sense for you (per module)
- function that take a slice of args called like variadic functions.
- let macros code generate a string and compile that
- super cold calling convention for panics with constant args where you use return address to lookup arg values (i feel like i can do better than rustc which is really strange)
- asm panic on invalid enum field instead of faulting
- explicit runtime data struct.
- u32/u16 pointers as indexes into per type arrays. deref trait so that can be a library feature? want to be able to toggle easily not at every use so can benchmark
- command line argument parser
- getters and setters so enums and flag sets could be done in the language and still have natural syntax.
- expose c variadic functions on llvm (because why not)
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
- should expose untagged unions because you might need them for ffi.
  tho really that can be done with a blob of bytes and casting through a void pointer.
  so would be cooler to add more powerful custom types and do unions as a library.
- a way to say ne=not(eq) for any type that doesn't already have a ne impl
  - ideally you could just inline the eq and distribute the not so it would never matter if you couldn't define your own ne (other than order)
  - feels sad if my asm backend doesn't know it can do int ne in one instruction,
    but for comptime, its probably not worth an opt pass that tries to fix conditionals.
- comptime code should be able to just compile a string which means
- I like multi-argument functions just taking tuples so you can refer to function types generically.
  but that means if tuple type is written as tuple of types, you can't have a function that takes multiple types as arguments in a row
  because it doesn't know where to split them if you flatten too soon. when fn f(X: Type, Y: Type) then f((a, b), c) vs f(a, (b, c))
  // Needs to be builtin because Type has a special load function which needs to evaluate \*Type to typecheck.
  // fn operator_star_prefix(T: Type) Type = Ptr(T);
- https://btmc.substack.com/p/tracing-garbage-collection-for-arenas

## UB

- accessing the wrong enum field
- holding a pointer to an enum member while changing its tag
- read off the end of an array
- div(0)
- overflow/underflow. add,sub,mul
- bitshifting too far
- constructing a slice with a bad length
- escaping pointer to stack
- holding a pointer to a variable that goes out of scope (stack slot might get reused)
- double free, use after free
- holding pointer accross a collection resize (common case of ^)
- void pointer cast
- hacky rust pointer ffi
- inline asm
- reading from uninitialized memory
- init from small union field then read from large.
- #c_variadic

## Testing

- be able to write a thing in my language that says which backend combinations to test on each snippet

## Lsp

- more tolarant parser. Need to be able to represent holes in the ast.
