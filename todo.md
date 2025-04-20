## Quest Lines

- compiler: add a defer expression that lets you run cleanup from a closure even if you jump out past it
- graphics: finish porting macos/app from objective c
- wasm:     get all the ssa tests working. some have c drivers so either need to output linkable wasm or use import_c
- linux:    finish transcribing structs so FRANCA_BACKTRACE=true works. make all the tests pass with -syscalls
- external: make it not a 100 line copy-paste to setup a driver that links an object file
- llvm:     get all the ssa tests working

## stuff i broke

- add a print for Dat2
- `fn emit_llvm(m: *QbeModule, dat: *Qbe.Dat) void = {` update to new Dat2
- :TodoChangeMessageEmitDataEndToUseBakedValueInsteadOfBackendDat
- adding #align was a compile speed regression
- formalize ENABLE_INCREMENTAL in backend/arm64/emit (+ support on amd64)
- `./q.out -t wasm32 -o target/out/q.wasm -cc backend/test/abi8.ssa -d AI`
%�%.104 =w pop
- fast memcpy (need to deal with fallback when not linking a libc)

## language consistancy

- #c_variadic + conflicting overload (ie for open()) where you want the non-va version to also be #syscall. maybe that's not worth fixing. 
- #c_variadic as part of FnType (or CallConv?) so function pointers work 
- #use field in guess_type for #where
- make auto deref always work (you shouldn't need to `[]` for constants or returned structs)
- hello_va doesn't work on linux??
- compiler/values.fr has a big comment
- make namespacing nice enough that i can have less stuff loaded in every program by `core.fr`
- allow #libc before #link_rename
- if constant folding can get rid of all the branchs that use an import, the binary shouldn't need that import
- auto #fold functions returning StructLiteralP (same as already do for Value).
ie. `fn init() Self = (arr = @as(Slice(Entry)) empty(), len_including_tombstones = 0, capacity = 0);`
- check that the keyword between block arguments is correct (ie `if a {| b } else {| c }` should require `else` not some other identifier),
  and use that as an argument name for overloading.
- default function arguments and mixed named/positional.
  because 'self.parse_expr(Prec.None)' and symmetry with structs.
- unpack `a, b, c = call()` without declaring new variables
- make compilation order of struct sizing less sketchy for offset_of
- propagate types through constants. `a: u32 : 0; b := a;` b should have type u32 even though int literals default to i64.
  (to fix `foo.id != SG_INVALID_ID.trunc()`)
- still allow coerce to c string if there was a `\` escape.- this should work
```
A :: @struct();
B :: @tagged(a: A);
b: B = (a = ()); 
b = .a;
```
- mix named and positional args
- allow specifying the Tag type for @tagged and default to the smallest possible
- let macros access requested type (of thier call site, not just infered of thier args) or provide an infered a type before processing.
- `@Fn(a, b) ...` means a different thing than `fn(a, b) ...` (the former sees types, the latter sees parameter names). 
- there are no clear rules about how const coercion works
- the body of a `=>` function gets re-sema-ed every time you call it (which makes inline_for hurt compile times more than it should)
- having the crt linux stuff in franca_runtime_init means you can't use the backend without the franca frontend and target linux exe

## library robustness

- ask the os for the correct page size
- :TodoLinux :HardcodeOs
- cure remaining stds
- stop using libc for random shit like float math or number formatting. 
- there's a bunch of library-ish / style things that need more work to get to the pointer where
the right/fast/safe/whatever thing to do is also the easy thing to do. 
  - make number casts less annoying and less error prone 
  - do a pass at trying to replace pointer<->int casts with some higher level thing
  - be consistant about using rawptr when it's a pointer (not i64)
  - replacement for thread locals by dynamically adding things to the context. 
  need to be able to do it from jit as well (ie. it can't just be extra fields on the struct). 
  - rename print() to debug()? make it clear that it's not what you should be using if you just want 
  to output text (it's unbuffered because i think it's more important to not lose things if you crash). 
  probably always to stderr. 
  - don't be using relative file paths. i feel like having a cwd is dumb.
  - errno stuff
- remove clowns (places where we exec things instead of using libc)
  - makedir
- make sure all the allocators respect required alignment 
- catch multiple branches with the same switch value.
- (cap, alloc) args are swaped between list:List and init:RsVec
- `[CPU Time]` on macos-x64 is wrong. (different libc magic numbers? clang libc_constants says no). // :WrongClockTime
- maybe have a `push_compiler_error_context` and `pop_compiler_error_context` for macros so @fmt could easily add a message like `while evaluating format string`?
- is :jnz_is_Kw really what i want the semantic to be?
- be less strict about amd64 address folding when there's a large constant pointer (which is valid when jitting)
- RSlot overflow
- for import_c, decide when to reset temp() and audit all the allocations in tok/pre
- default arg values (any const expr and inject at callsite? or based on other args so generate shims or multiple entry points to the function)
- clean up what goes in lib/build.fr vs lib/sys/fs.fr
- make fetching dependencies (ie. lua for testing import_c) not embarrassing
- import_c/ffi has a big comment
- implement examples/testing.fr/fetch_or_crash() with import_c (wuffs and libcurl) instead of exec-ing shit
- use import_c for the parts of sokol i haven't ported yet (can't for the mac stuff because thats objective c)
- shouldn't have `alloc` be the nice name, it should be `alloc_uninit`
- :ThisSureIsABigBlit
- :RethinkTheFfiCompileApi
- make import_wasm not crash on implicit return 

## cleanup 

- make comptime.fr exporting stuff less painful 
- sema needs to get simplified. 
- clean up import_c ref tracking
- fix the hack that requires `#ir({ (o, k) })`. it should allow just a tuple and still evaluate it as an expression instead of just grabbing the identifiers. 
- backend fails_typecheck make sure sel0:sel1 and blit0:blit1 and cas0:cas1. 
also stop pasting around code for handling the multi-part ops
- check enum names are unique
- combine places that do multiple walk_ast/clone passes (renumber/unmark_done)
- quest to get rid of magic numbers
  - laz/return_map_m table
  - BlockAlloc powers of 2
  - x64/arm/wasm/macho/elf but there's no winning there
  - boot but i really don't want to write the compiler again... so idk what to do about that
  - some of the qbe ssa tests are generated: vararg2, strspn, strcmp, queen, mem3, cprime, abi8
- extend hacky_incremental into something that can be used seriously 
- finish amd64/isel/NEW_ADDR_FOLDING
- as an extension of argparse it would be cool if all the demo programs could be both 
and exe and a dylib so if you want to run from cli it parses to a struct and calls the impl,
but if you're calling from a program you can import the struct, dlopen the thing, 
and not need to serialize the arguments to a string. 
- seperate out the platform specific fields of QbeModule
- finish curing stds

## tests

- pending repros: uninit_stack_slot, typchk_unquote
- have a test where you force inline everything that's not recursive to stress test the backend dealing with large functions.
- compiler/test.fr run for jit as well
- automated test that builds are still reproducible
- fix the test programs to not all write to `./a.out` or whatever so they can run in parallel.  
- test compile error for conflicting #use
- compile all the examples in run_tests: aoc, toy
- repro doesn't work when you do `-repeat`
- more calling convention tests between jitted code and c.
- have one command that lets me run the tests on all targets
- test slow_debug_threads (+ one that needs the phis in replace_frontend_ops)
- tests for failing progeams. ie. panic backtrace
- auto test repl. 
- compiler/tests.fr stops when something doesn't compile but it should show which other tests passed like it does for runtime failures
- make it clear that you can't do this: `franca self.fr && ./a.out driver.dylib run b.fr`. 
it doesn't like that you stomp a.out, default_driver:run should pick a unique path probably. 
or just default to jitting and force you to enable aot by specifying an output path. 
- sort the array of test files so you can always diff the output without hoping the file system iterates in a consistant order 
- think about how to test the gui programs more convincingly than just that they produce a binary
- run the deps tests in github actions
- test using import_(c, wasm)/ffi from a precompiled driver to make sure they're not relying on being in their own compilation context 

## linux

- signal handlers (the compiler does it and tests/basic_libc.fr)
- relocatable and shared libraries (backend/elf/emit.fr)
- shader translation for the gui examples

## error messages

- errors don't show multiple locations (like conflicting overloads should show the problem)
- `Type Error` should tell you where the difference is! and use the right names for enums, etc.
- `place expression expected pointer dereference` when you forget a `[]` after a call and field access should tell you that.
- detect if you try to do the jit thing with the backend library from jitted code on macos instead of just `bus error`-ing
- bring back nice error message for trying to call a #ct at runtime
- `Illegal place expression: GetNamed` is not as helpful as "tried to assign to undeclared variable %"
- need chained compile errors.
  - maybe loc should be a specific token so you don't highlight the whole expression just to complain about a function signature
  - macros should show both expansion site and code that generated it.
- better error message from backend if you forget nunion=1 on a struct

## language decisions

- make #macro less of a special case? it would be nice if it could participate in overloading on some arguments. 
so you could say `fn fmt(out: *List(u8), template: Str, arg: FatExpr #macro) FatExpr`. 
then you need a new way to express that you need `template` to be constant in the program but 
it can be runtime known in the body of the fmt macro. 
- #generic is painful
- get more strict about calling conventions
- more powerful driver program. it should be able to see all your functions stream by and poke things in. 
ie. ptrace demo (see libc.fr). 
the more immediate problem that would solve for me is being able to use newer language features 
by patching them out without updaing the whole ./boot binary. 
ie. add a rotr instruction and instead of needing to keep a has_feature() uglying up the code forever,
could put that fallback implementation in the driver program and it could fix things from the outside. 
the thing that's painful is then you have programs that can only be compiled by a specific driver and they don't compose well. 
- try to move some of #asm #libc #ir #import #import_os #link_rename #syscall #log_ast 
to a more powerful thing where you can give a function to transform the body. 
so like maybe `#with(asm, "aarch64")` would call `asm(fid, "aarch64")`. 
if i had that i could rename #x86_bytes without updating ./boot. 
maybe this could just be done with the driver program. 
you also want like a first class thing for choosing implementations. 
so instead of #import_os being a hardcoded painful thing, and emit_ir 
also having super ugly stuff with FuncImpl, it would be nice if we just 
called some comptime function and passed everything we know about the target and it 
could choose. 
- make #where type inference more powerful.
this is super important for making ResolveOverload faster. 
and for being more out of order because you wouldn't need to `::Foo(T)` all over the place. 
but it's hard to test until i replace everything and it's hard to replace everything until i test. 
maybe a good starting point would be to convert `fn(a, b) => ()` without type annotations to `fn(a: ~_0, b: ~_1) => ()`,
and make sure to deduplicate by type (rn we re-sema every call which is sad). 
- make macros more similar to functions. it's weird that the syntax is slightly different.
  you should be able to overload off some argument types and have others be a symbolic ast node?
  but that would make overload resolution even more fragile which might be a bad idea.
- combine include_bytes and #include_std somehow
- @switch should just be @inline_switch if the thing is constant.
- local_return and return implciitly rebinding is a bit confusing
- would be cool if #where could let you infer new types instead of just checking them: 
```
#where(=> /* let us access $o and $k and then we can use argcls() to get the cls of A0/A1/R and can tell you what types are allowed somehow */)
fn invoke($o: Qbe.O, $k: Qbe.Cls, a0: ~A0, a1: ~A1) ~R #ir(o, k);
```
- nicer named return values
- make `#where` based on argument names instead of order.
- `#where` access constant arguments of the call
- fold.fr:
//!     TODO: I think it would be much faster if the frontend just tracked what 
//!           should be constant, and only materialized it upon seeing a 
//!           non-arithmetic #fold, and otherwise just defer until the backend.  
- shims advanced version: don't comptime jit until the first time you call something. can't decide if thats too creepy.
- context() leak allocator
- generic Read/Write instead of hardcoding List(u8).
- seperate fn stride_of and fn size_of so you can avoid extra padding in nested structs.
  then need to allow different reprs.
- test that struct padding is zeroed before emitting comptime data and before being used as a key to cache const args instantiations.
- deduplicate constants (strings especially), its just annoying cause i don't have const pointers.
  can do it by address at least, cause if it was a string literal in the pool, will be the same.
- make the quote syntax not make a redundant block for single expressions that you have to manually get rid of if you want to access the actual node.
- auto coerce payload to enum when branches aren't ambigous? maybe? thats a bit creepy. doing it for unit when you just forget to return something would be bad.
- a way to say ne=not(eq) for any type that doesn't already have a ne impl
  - ideally you could just inline the eq and distribute the not so it would never matter if you couldn't define your own ne (other than order)
- i want to allow more uniсode in identifiers beсause that seems polite but you 
really need an option to error on insane сharaсters.
this is interesting for... seсurity... сan you tell why? 
(hint: paste the letter с into google and think about why the first result is Си_(язык_программирования)). 
also this: https://github.com/golang/go/issues/20209
- make keyword arguments work all the time. be careful about still supporting a.f(b = c) gives f(a, (b = c)) instead of f(a, b = c). 
because it turns out the former is actually useful a lot of the time. 
- would be cool if this worked: `S :: @struct(TAG :: .c); Tag :: @enum(a, b, c); a: Tag = S.TAG;`, 
usecase: examples/lox Obj header

## demos 

- finish examples/view_image (which means finishing examples/import_c/ffi)
- go through c++26 reflection examples and make sure i can do them better https://isocpp.org/files/papers/P2996R4.html
- examples/bf that translates to wasm and then uses examples/import_wasm to run it
- get wasm to work well enough that i can make a compiler-explorer like thing for the .ssa/.c/.wasm 
(.fr is probably harder because need to figure out how to jit. is making a new module for every function feasable?)
- https://andrewkelley.me/post/string-matching-comptime-perfect-hashing-zig.html
- fix examples/compiler_gui
- rust format! macro. they have format_args! builtin to the compiler which is kinda funny
- make graphics/shaders translation support a more interesting subset of the language 
- profiler gui. it's silly that i have to open CLion just for it to run DTrace and draw a graph
- something that generates point clouds / LAZ files so you can use the geo demo without 
needing to go find some data in the right format (and without me including a blob for it)   

## make it not suck

- `#[derive]`, hash, print. should be fixed by #where (but now im afraid that might be super slow because of the #align fiasco)
- numeric stuff sucks because of all the casting and it's bad about letting constants/literals coerce correctly
- comptime feature flags like use_threads, graphics::backend, os/arch.
need to support them being different at comptime and runtime in a uniform way. 
- using foreign libraries. as soon as you need to write your own driver it's super annoying. 
- nested error messages. need to be able to show more than one source location. 
- keep reducing the amount of stuff in the prelude 
- allow adding your own fields to the dynamic environment parameter
- i really need to output debug info
- nested array syntax is kinda ass. `Symbol.Array(2).Array(3)` and `Array(Array(Symbol, 2), 3)` 
is much worse than zig's `[3][2]Symbol`, even c's `Symbol the_name_in_the_middle[3][2]` 
has the numbers in the right order at least. rust has my problem too `[[Symbol; 2]; 3]` but nicer special case syntax for it. 
- bit flags, @tagged abi. see sys/linux/LandLock

##

- graphics: finish updating documentation to not use c syntax
- translate a more serious subset of my language into the shader languages
- use []u8 and interpret_as_bytes instead of SgRange
- :GfxLogging
- fix compiler_gui so it sees all functions from a full compile
- output_elf: relocatable object, dynamic library
- Wasip1Libc
- things that should be cleaned up next time i :UpdateBoot
  - :DumbNameAlias NoMangle
  - AbiHackSpan
  - `@rec` in backend/ir.fr and wasm/instructions:Wasm
  - use sqrt/min/max
- αcτµαlly pδrταblε εxεcµταblε
- probably want to port the build stuff. https://github.com/jart/cosmopolitan/blob/master/tool/build/apelink.c
  i assume i can make it less complicated since i'm the one generating the thing in the first place?
  and i only care about linux and xnu. windows can just use wsl.
- fix overflow when lexing large float literals
- show const fields better in log_type
- less verbose if let/let else? think about how to express with a macro.
- #target_arch
- i still think wasi's kinda dumb but running the compiler in a browser would be very pleasing.
  tho i could just use blink https://trungnt2910.com/blink/blink.html
- be nicer about warning invalid arguments in examples/default_driver and compiler/first
- parse zig headers and generate extern declarations (like examples/c_bindgen). then i could use bits of thier standard libary.
  they have a bunch of fun stuff that doesn't use use comptime in the api and im not interested in writing myself:
  http, hashing, random, zip, magic numbers for wasm/elf/dwarf/macho
- should make it easy to build rust/zig projects from your driver program since they can generally cross compile well and have libraries you might want.
- do i want to expose the idea of different libcs? musl vs glibc vs cosmo

## data structure changes

(when compiling the compiler) need to seperate the ast types used for the comptime interfacing with the active compiler and the ones used internally by the new compiler.

- 16 byte RsVec because address space is really 48 bits probable so 48 bit ptr and len and spread cap accross the upper 16 of each.
- use null pointer as niche for `?*T`
- make .None tag be 0 so zero initilized is a sane default (just need to swap the order in the declaration).

## regressions

- @tagged tag check on field access

## Feature Ideas

- should make a derivable (fn arbitrary(rng) T) for testing hashes or stuff on psudorandom valid values of a type.
  like eq and hash would be nice to test like that. also ffi with c.
- tail recursion
- https://llvm.org/docs/CoverageMappingFormat.html https://llvm.org/docs/LangRef.html#llvm-instrprof-increment-intrinsic
- good error message for accidently using ' as character literal.
- embeding other languages would be a good demo of the comptime/meta programming stuff.
  - need a raw string syntax that passes it to a macro (like nim?)
  - lualit would be cool cause they have c abi stuff and dynamic language so notably different from mine.
- adding 'e->name(a) === (e.vptr.name)(e, a)' would make <- the "function oriented programming" operator and -> the "object-al programming" operator.
- annotations should be powerful enough to derive recursively eq/clone/drop/hash based on fields.
  - need to let you access the default even if you override.
    like adding extra drop logic shouldn't mean manually dropping all fields.
- fold infinite loops so it knows when you dont need a return at the end of the function to typecheck
- anon structs for named return values. maybe commit to no !struct but what if you want an enum maybe its better to have no blessed default case
- maybe anon struct literals with inferred type should be fine.
- const SafetyCheck = @flagset(Bounds, Overflow, DivByZero, UnreachableCode, WrongEnumTag, CastBounds, Align, FfiNull, UseUninitCanary);
  safety(Bounds, fn= lt(i, len(self));
- enum bitset
- quick union types: fn Enum(Arr(Type)) Type; so Enum(A, B, C) === (A: A, B: B, C: C)!enum;
  - if you had all the tags unique (use TypeId) instead of starting at 0, you could make it free to convert to a super set.
    that probably gives up u8 or niche tags but even u16 is enough that its fine.
- user defined operators so you can pick what meaning of &T makes sense for you (per module)
- function that take a slice of args called like variadic functions.
- u32/u16 pointers as indexes into per type arrays. deref trait so that can be a library feature? want to be able to toggle easily not at every use so can benchmark
- command line argument parser
- getters and setters so enums and flag sets could be done in the language and still have natural syntax.
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
- I like multi-argument functions just taking tuples so you can refer to function types generically.
  but that means if tuple type is written as tuple of types, you can't have a function that takes multiple types as arguments in a row
  because it doesn't know where to split them if you flatten too soon. when fn f(X: Type, Y: Type) then f((a, b), c) vs f(a, (b, c))
- https://btmc.substack.com/p/tracing-garbage-collection-for-arenas

## behaviour considered disrespectful

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
