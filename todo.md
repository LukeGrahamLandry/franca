- fix all the places i `:HardcodeOs`
- #target_arch
- have one test command that runs all the code that exists.
  also rn examples are only jit by run_tests.fr (not llvm aot).
  should have a more declaritive way of describing what programs exist such that they can be run by multiple backends.
  ie. i want to add cranelift, wasm and qbe to the actually tested category.
- wasm's totally broken.
- i still think wasi's kinda dumb but running the compiler in a browser would be very pleasing.
  tho i could just use blink https://trungnt2910.com/blink/blink.html
- the compiler shouldn't know about cranelift.
  let driver supply comptime jit vtable (and support loading driver from a dylib becuase you can't load the driver without a jit).
- more type safety in int vs float registers in jit backends would be nice.
  also arm vs x86 register constants (they both have an sp but they're different numbers).
- automated test that builds are still reproducible
- default function arguments and mixed named/positional.
- shims! the easy version is just a better error message for "tried to call uncompiled <...>",
  that would let me get rid of the "WARNING: <...> libc" and just give a useful error message if you actually make the mistake.
  advanced version is don't comptime jit until the first time you call something. can't decide if thats too creepy.
- finish getting run_tests to run on blink
- finish >6 arg functions on x64
- make github actions fail if a test fails (currently it only requires examples to work).
- try to give llvm less work to do. clean up first call to intrinsic/redirect (it hurts deduplication! i have a billon fn alloc now).
  would it help if i did less dumb inttoptr and add 0 for int constants?
- be nicer about warning invalid arguments in examples/default_driver and compiler/first
- think about sharing temp allocator when loading dylibs
- clean up what goes in lib/build.fr vs lib/sys/fs.fr
- instead of QbeIr+LlvmIr+X86AsmText, just have `IrText(type: Symbol, code: Symbol)`,
  so the compiler doesn't have to know about all backends.
  and then backends can just recognise whatever `type`s they want to support.
  have the `#asm` tag take a string argument.
- really the compiler shouldn't know about `#test` but its convenient for debugging if i break run_tests.fr.
  so maybe i need to be able to load driver from a dylib first.
- parse zig headers and generate extern declarations (like examples/c_bindgen). then i could use bits of thier standard libary.
  they have a bunch of fun stuff that doesn't use use comptime in the api and im not interested in writing myself:
  http, hashing, random, zip, magic numbers for wasm/elf/dwarf/macho
- there's a few places `defer` would be really nice to have
- do i want to expose the idea of different libcs? musl vs glibc vs cosmo
- would be cool to transcribe my own linux syscalls
- can i use address sanitizer with llvm ir? presumably that's how rust's `-Z sanitizer=address` works.
- make shim when you do an indirect call from a jit backend.

```
@impl(fn(T) = T.is_sequential_enum()) {
    T :: @placeholder 0;
    fn eq(a: T, b: T) bool = {

    }
};
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

wait until fully self hosted so its less painful because don't have to keep both sides in sync.
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

// TODO: fix new constant shadowing old constant of same name. or at least give error -- Apr 22

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
- Box(T) and box(t) for quick allocations
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

## Testing

- be able to write a thing in my language that says which backend combinations to test on each snippet

## QBE

- https://c9x.me/compile/
- need to expose a function that I can call on a string of ir instead of using them as an executable that reads a file
- make sure the reason they don't expose a lib isn't because they never free anything
- they emit assembly not code so I have to write an assembler
  - which would be nice to have anyway because then I could have clang output asm for other libs and link them into my stuff that way
- setup benchmarking system so I can make sure its dramatically faster than my own shitty asm
- does it make sense to put all thier statics in a thread local and hope that makes it thread safe?
- figure out how to make a memory backed \*FILE

## Lsp

- more tolarant parser. Need to be able to represent holes in the ast.
