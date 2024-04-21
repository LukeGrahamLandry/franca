## getting rid of interpreter (Apr 20)

So end goal is for comptime repr of values to be the same as rust repr so I don't have to do any serialization when you call into the compiler.
But that's such a massive change that would be so painful so doing it in tiny steps and keeping the compiler functional throughout the transition.

First step was making all ffi calls in macro handlers go through an array of integers instead of an array of interp values.
You shouldn't need the runtime type (rust enum) tags since you know the function signetures statically.
That's what the flat_call calling convention does. Just pass in argptr/len and retptr/len and have both sides serialize the values out.
But helpfully the int repr is what my asm uses already so that side is a no-op. (most of this was yesterday).

- Seperated out the macro_msg impls from the serialization code.
- Required some ugly hacks to dereference pointers before going into the flat call because interp stack pointers aren't real pointers.
- Mistake with enum padding being added twice because of the double serialization.

So after that, we're at the point where all the interesting ffi stuff is happening in native code.
The interpreter is only doing moves and calls, which asm can do on its own.
The interface to the interpreter is really thin, its just 'fn run'.
So now allow TargetArch::Aarch64 at comptime and replace interp run with emit_asm and then jumping to the jitted page like you would for runtime calling main.
Now that ast functions are in asm, its a problem that I can't do large structs so had to use flat call for internal things too sometimes (not just compiler ffi).
Really need to implement a less stupid cc than that at some point.

So that kinda worked but caused my favourite bug.

## (plan) passing by pointer (Apr 14)

Llvm doesn't do all the c ABI stuff for you. Need to use pointers to pass structs by value.
Other backends are likely need the same thing.
Currently my asm one just hopes all the args will fit in 8 registers but that doesn't follow the abi for calling other c code
so needs to change too.
So it would be nice to do it on the bytecode representation instead of in the backend,
but that gets a bit sketchy because what if different abis have different rules about when to use registers vs pointers?
Like the aarch64 one sometimes passes on the stack without passing that pointer in a register (because the caller knows where to look).
(https://learn.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions?view=msvc-170).  
Tho following the calling convention only matters for ffi but if I have to support it anyway, why not do it all the time.

## burn it with fire (Apr 10)

Code is evil. I want less of it.
Go back to commit "remove unused experiments" when ready to try again.

## Planning a Linker

- calls
- jump tables for switch stmts. (a-b)>>2
- function pointers
- const vtables
- imports
- global vars @PAGEOFF
- extern globals @PAGEOFFGOT
- `___stack_chk_guard`

> Note: qbe doesn't give thread-safe vibes. There's a lot of static vars. Probably part of why they don't distribute as a lib.

## Planning Casts (Apr 9)

Currently @as sets the result location, and stops inference from going deeper, so you can use it for resolving an overload.

- But it doesn't actually change the type returned by compile_expr so its not as powerful as specifying a variable/return type.
- And it doesn't do a type check (because that's the point), which means `let a: i64 = @as(Str) 1;` is legal (and a no-op) which is kinda dumb.

Doing casts as annotations works for helping overload system but its weird to not be a function cause they really are.

- `as(Str)(e)` feels like too many brackets but `as(Str)$e` is the same as the `@` one.
- Then `as` could be a comptime function returning the cast function.
  - `fn as(const t: T) Fn(__any__, T);` but there's no type I can put for the input.
- Really `e.as(Str)` would be better but means a different thing.
  - That sounds like evaluate `e` as whatever type it is, and then convert to `Str` instead of giving that as the result type.
  - Maybe you want `fn as(e: T, const T: Type) T = e;` which would match the current `@as`.
    - But I can't do that with my current generics yet.

(want different types of cast, just using `as` for example).

## new test runner (Apr 6)

at first i wrote a dumb serial one which was fine but 3x slower than cargo test.

- what the heck is happening.
- its doing something for a really long time before calling main but after it claims to have run the exe when
  calling through cargo run but if i just do it directly its fine. 0.5 vs 1.3 seconds
- but cargo build and then run it doesn't help. its just slow the first time you run regardless.
- it would make sense if it were like getting the files from disk thats later cached but its not even running main
  (i printed something at the beginning and flushed stdout)
- same behaviour in release mode
- oh damn turning off the llvm feature flag makes it fast again.
- so the reason i didnt notice before is most of my tests were in the compiler crate, not the llvm-backend crate so that didnt include it.
  and the i did see that my llvm tests took longer but i thought that was just running llvm is slow not... the existance of llvm is slow.
- so like maybe its because llvm is doing initilization shit but then why only the first time? so not that
- then must be because the binary is giant (9MB vs 69MB)? so it has to load the whole thing into memory? or macos is dumb as fuck and like sending a hash somewhere to check for viruses??
- speaking of which how the fuck is it 9MB even without llvm?? ok release is 2.2 which is better.
- looking in Instruments, its spending the time in dyld4::prepare so i guess it is the linker thing.
  so like am i dynamiclly linking? theres no way, my exe is so much bigger. loader doing offsets?
  and like maybe it can cache that for latter if i run the thing again immediatly?
- oh it probably doesn't even get a new address space. ok science time.
  nah. 'println!("main addr: {}", main as usize);' is different every time so not that simple to understand sadly.
  i also tried on a function from llvm c api but its also different every time.
- but like also, even without llvm its 0.195 vs 0.077 the second time you run it (release mode).

## VoidPtr & overloads (Apr 5)

It's cringe to make the language more verbose just to make the compiler, but void pointers aren't what I want anyway.
The only reason they exist in the first place is that my type system is too shitty to express things I want.

- Recursive types
  - Current use is linked list of stack frames for unwind.
  - Didn't feel important because I'm used to rust where lifetimes make it painful anyway but obviously need it for ast nodes soon so idk what i was thinking.
  - Require pointer indirection so you know the size. Need to be able to reference the name while making the type.
  - Mutually recursive should be allowed too (like stmt/expr).
- Use const args in return type even when the whole thing isn't comptime
  - current example is alloc which should return a Slice(T) instead of a VoidPtr
  - means I need to support const args in split FuncRef too. so just do both specializations I guess. Should notice which never get called at runtime anyway (macros).
- raw_slice used to impl real Slice is probably fine just being a cast there
  - There's a hacky test in generics.fr with tuple layout that I want to make unspecifed anyway
  - MySlice generics.fr is more reasonable but its fine to make that more clunky.
- literal_ast needs const arg used in later arg type except that it's a builtin function where you don't actually want to generate multiple bodies.
  - but currently used through a macro as a work around anyway

My overload change also made ""!type vs Str more painful. I really need some auto cast subtyping stuff.

Looks like it was a bad change but really its just revealing latent problems that jsut happened to pass because I have small enough programs that it just worked out.
It feels extreamly important to actually resolve overloads robustly and not just pick the first.

## How small can it get?

```
RUSTFLAGS="-Zlocation-detail=none --remap-path-prefix $HOME=~" cargo +nightly bloat -Z build-std=std,panic_abort -Z build-std-features=panic_immediate_abort --target aarch64-apple-darwin --release --no-default-features -n 50
```

Surely I can do libffi in less than 30KB given I have to do it anyway for my own asm backend.

## const args (Mar 28)

You want Fn and Type to be comptime only so when you pass them as an argument it generates a new version of the function specialized for each callsite.
But some comptime functions want to be able to work on those values without generating a bunch of redundant versions of themselves,
because they just do stuff to the compiler data structures, they don't actually use the type/func in thier body.
So maybe it makes more sense to say you have to be explicit about the difference
and declare the argument 'const' if you want to call the function or have a variable of the type.
Which is nice because its more clear when calling a function will slowly bloat your binary.
Plus it means I don't have to deal with some weirdness around which capturing_call args need to be added to 'constants' vs 'vars'.
So makes the compiler simplier and kinda has a justification in the language.
I think that was a win, I had to add it in like 6 places and got to delete some compiler code.

## llvm agragate returns (Mar 22)

LLVM can't return structs by value.
Seems like clang will return a vector for two words but use pointer param for 3 or more (https://godbolt.org/z/qGWvbf4K4).
I probably want that for other backends anyway so perhaps I'll make that a pass on the AST.

## trying to make an lsp

- "expected tuple of two elements found map": request is (id, params) but notification is just params.
- symbol provider vs highlighting vs colours vs semantic tokens.
- Kinda works in Kate, it will give colours for "Data Type" for ENUM, comment works, but you get "Normal" for everything else regardless of what you ask for (those work in theme editor).
- Dot completion works in Kate tho.
- Maybe kate is just broken, so want to try in vscode but you cant just connect to an lsp, you have to make an extension that just delegates to the lsp.
- That just spews errors and doesnt show my guy in the drop down of logs.
- Of course the magic command that's the only way they tell you to setup a project targets a version newer than it is possible to install, silly me!
- So now I can see my stderr when selected in the drop down so I know its answering the requests, but it still doesnt show colours or dot completion.
  It works less than Kate so the problem must be in my extension not my lsp.
- problem was not telling it which TextDocumentSyncCapability i wanted. kate just picked one i guess.
  But now it works and I do get colours, so that part at least was just a kate problem.

## Dot call syntax (Mar 15)

Syntax sugar for 'o.a(b)' === 'a(o, b)'

number literals need the brackets because they lex as floats. thats easy, not ambigous, just backtrack if its not a number after the dot.

Having a callable in a field will be ambigous.
I think prioritize dot calls, force brackets for fields, and use macros to generate wrappers for things with vtables.
You want them to be special anyway to get intuative single dynamic dispatch like c++ virtual, java, rust trait objects, etc.
They need to look up the function pointer to call in the vtable, but also pass themselves as the first argument.
Lua handles that problem with 'o:a(b)' === 'o.a(o, b)'. So you can only have the infix syntax if you're doing dynamic dispatch.

```
fn log(self: Ptr(i64), important: bool) Str = str(self[]);
fn log(self: Ptr(Str), important: bool) Str = self[];

const Loggable = (
  vtable: FnPtr(Ty(VoidPtr, bool), Str),
  dataptr: VoidPtr
)!struct;

fn log(self: Loggable, important: bool) Str =
  (self.vtable)(self.dataptr, important);

fn upcast(self: Ptr(i64)) Loggable = {
  const log: Fn((Ty(VoidPtr, bool), Str)) = log;  // Somewhere to hang the type annotation to resolve the overload.
  (dataptr: self, vtable: log!fn_ptr)
}

var hello = "Hi";
assert_eq(true, str_eq("Hi", hello!addr.log(true)));
var n = 24;
assert_eq(true, str_eq("24", n!addr.log(true)));
var unknown: Loggable = hello!addr.upcast();
assert_eq(true, str_eq("Hi", unknown.log(true)));
unknown = n!addr.upcast();
assert_eq(true, str_eq("24", unknown.log(true)));
```

## Thinking about continuations (Mar 11)

It feels like you could get a bunch of features for free if you had a good way of
referring to what happens after a function call.
Like return is really the same as throw/panic, just you know its only going up one stack frame.
Generators, async, and coroutines are like that, but you can go back and forth multiple times.
Return is a lot like a tail-call to your caller and while loops can be trivially expressed as tail-calls.
I was thinking you then have to optimise out the tracking of where to return next, but normal languages already do that.
Really lr is a function pointer that you tail call when you return.
I guess you have to use fp if you want to unwind multiple frames, but you want that anyway for debuggers.

```
const Cont = .{
     returnAddress: VoidPtr, // lr
     resultAddress: VoidPtr, // x8/&x0
     stackPointer: VoidPtr,  // fp
};
var return: Cont;
```

If the return value is big, the caller wants to give you an address as an argument,
and you don't pass args to return. The normal case where you return a value in x0,
even matches the c calling convention where you pass the first argument in x0.
It fits so well, maybe this just something obvious that everyone already knows.

So you need a way to talk about stack-frames at the call-site.

- call: push a frame for the callee
- return: pop your own frame
- self tail call: leave frames alone
- throw: pop frames until you hit a catch block
- yield: pop one frame but store it somewhere for when you get called again

For generators, if it's not recursive and doesn't escape, you know how much stack space it needs for itself and all internal callees,
so you could put that space in the caller's frame so the generator's locals can live across calls.
Even if it does escape, there might be a statically known result address that gets passed in, so you could construct it in place.
Probably don't want to allow moving closures anyway because then you can't take pointers to locals.

When you call a function, you pass as an argument the function pointer to call when it's done.
You need to include how to adjust the stack frames, but in the common case you know statically.
Separate where to reset the stack pointer to when you're done from where you're allowed to grow your stack into.

## Cleaning up fn addrs (Mar 5)

For the libc things where you need to get the addresses at comptime,
make a list of signatures/pointers and then treat calling those pointers uniformly with inline assembly.
That also gives a place to put annotations saying you want to dynamically link for when I want to output real executables.
So you can have one FuncId with `@dyn_link` to call at runtime and `@comptime_addr(_)` to call at comptime.

## learning about outputting executables (Mar 4)

- https://en.wikipedia.org/wiki/Mach-O
- https://github.com/horsicq/XMachOViewer
- https://github.com/aidansteele/osx-abi-macho-file-format-reference

So there's a command for telling it which address to start at,
and that's like an offset into the virtual address space you've loaded the binary into?
You have load commands that map the bytes in the file to where you want them to be in that virtual address space.
Everything is relative to some base address the os chooses for you when the thing actually runs.

There's an indirect symbol table where you list the names of the functions you want the loader to give you.

Every time you want to call one of those dynamic functions, you jump to a stub like

```
adrp x16, #0x100004000
ldr x16, [x16, #I]
br x16
```

where I is the offset into the indirect table.
and you carefully put those in a named **stubs section, so it can patch them in after the first call?
That adrp thing is the address (encoded as an offset) of the page that the loader filled in all the pointers you asked for,
But how does it know where I expect them to be? Oh, you have a **got section, and it fills that in.

## improving inference (Mar 3)

Instead of LazyType::Infer just becoming Any and leaking around everywhere,
it needs to be legal for a function to have partially known types for longer during compilation.
You want to remember that we don't know the return type yet, then try to compile the body without that information
and stick whatever you get in as the return type.
So there's a few of places (like if/while) where I used to just unwrap_ty, but now I need to
infer -> get the arg -> check it -> emit the body -> then either check ret or set it to ret.
It gets wierd with closures, you can't always just compile a function any time you see it,
have to wait as long as possible?
I need to fix resolving overloads so if it sees something with an inferred return type
it just tries to compile the function (should be since it must not be a closure),
right now it just always skips those options unless someone else somehow caused it to get compiled already.
Or for now just... don't have inferred returns in generic impls.
But it does work, infer_types_progress no longer lies and says "we do definitely know the type, its Any!"

The other popular place that uses Any is assert_eq.
But changing it to have a bunch of overloads breaks some places that can't infer which to call.

- one was my weird generics test where I was treating a tuple of types as a tuple of ints: just fix the test.
- !tag: easy, Ptr(i64) for now
- `assert_eq(no.None[], unit);`, I guess that should be legal, it's an easy answer.
- @At macros, i guess you really need to expand them to get the type. but now we're going deeper into the territory of type_of
  being the same as compile_expr. which really it already was, so maybe I should just stop pretending they're different things.
  That leads to a wierd thing of do you decide which function you're calling before or after you compile the arguments.
  And I guess its just try one then the other and if you can't figure it out it's an error. Maybe that's fine.
- bit_literal. painfully special cased because I already have the function for that since its type is right there and
  I don't want to commit to doing the comptime call right away.
- `assert_eq(0b101, 5);` right so I guess I should lie and say i64 is the type because I don't want to deal with
  real subtyping or assert_eq for all eq and eq for all ints yet.
  and it doesnt break the bit length checking for the assembler functions so seems fine for now.
- `assert_eq(25, (fn(u: Unit) i64 = { 25 })(unit));`. Have to actually look through the function expr and try to see its type.
  Again that's unfortunate because that might mean really compiling it so would be nicer if type-checking were defined that way.

## quest for an assembler friendly language (Feb 27)

Bit syntax.

- dumb mistake of treating 0x0 as 16 bits not 16 values.

Enum constants (what java calls an enum).

- the macro emits a pointer to the struct instead of the struct itself,
  becasue the interpreter guesses that it will only need one stack slot when there's no type annotation.
- hacky special case for addr of constant pointer where it just gives you back itself instead of its address.

// I've heard it said that quickly selfhosting leads to a language thats just good for writing compilers... that's exactly what I want.

## spliting sema (Feb 24)

I want to do a jit asm backend and use that for comptime instead of my dumb interpreter,
so i guess should start by seperating the stuff that emits bytecode from the comptime rewriting the ast.
They're clearly two seperate things happening together in compiler.rs currently.
Then it should be less painful to add more backends.

## serializing asts

- fix logging of recursive types
- my InterpSend for option always said None cause im dumb
- need to make sure everything has a staticly known serialized size. so padding for enums and vecs as allocations.

## the road to partial evaluation (Feb 19)

Need to track which values are const known to make it easier to deal with baking closures passed as arguments to other functions.

- resolve_function was typechecking the arg before checking if the func var was just a GetFn instead of an OverloadSet
- sad day remove_named calling Vec::retain with flipped condition so i was removing all args accept the one i bound.

## sneaky swapping for mutation (Feb 16)

Previously I was cloning a bunch of stuff to deal with all the asts being in the program struct.
As a seperate adventure, I want to work towards baking all the comptime work into the ast so I can write other backends
that just have to handle the translation for runtime. So it would be really nice to mutate the ast as I emit code for them.

I suppose I've jsut invented a refcell with extra steps.

## reworking constants

- each compiled Func needs to track which consts it closed.
  those were the state when you saw its declaration so you can use them when you go to compile its body which might be later.
- i knew it would be slower but its like a bit unacceptable.

##

Trying to ensure_compiled preemptivly instead of jitting so it can be more like real compilation,
but that makes it pass ((a, b)) isntead of (a, b) to assert_eq. But really its just making
inline calls actually happen for builtin wrappers which is what i was trying to do anyway.
so we've just learned inlining passes arguments wrong somehow. doesn't unbox them.
Fuck, it's on a function labeled `// TODO: this needs to be an error`.
distressing that inlining changed behaviour tho.

## web

static LIB: &str = include_str!(...)
I think I don't want a const because I don't want it inlined at every use (if there were ever more than one).
Do they really do that?

Looking in devtools profiler, print is as long as whole parsing. Need to buffer it.
TODO: why does opening the devtools profiler make the loc/sec better? is my counting fucked up somehow?

## structs

I need to let expected type flow down the tree so it will treat your map literal as the type of your variable.
Don't want to deal with a generic map thing that i then convert rn.

## generics

##

I really like the Zed thing where you get a file of all the errors.
It's great for mass changes like adding an argument to a function when you're always in a context where you can trivially get the arg anyway.
Tho sometimes the same place shows up twice which is unfortunate.
Same err from rustc and rust-analyzer?

## parser rewrite (Feb 10)

Normal assign left can be a tuple for pattern match-y assign multiple at once (like rust/python).
But that means you can't put an annotation there because its ambigous if the tuple is names or a call on the annotation?
@a (x, y) = z; but no, because @a(x, y) = z; is illegal so what's the problem.

I gave up. Maybe its just a skill issue but its so easy if you just write the boring code.
There's no way the parsing is the slow part. I need to have error messages.
I don't understand why they wouldn't tell me what tokens it was expecting.
As bonus with the privilege of error messages you also get to trivially target wasm without figuring out how to ask the c compiler to do it.
It seems like it wont really be that much more code than the hoops I was jumping through to get a useful ast out of thier parse tree.
Maybe I should have tried to find something that genreated types off the tree sitter thing but the only one i saw seemed to
generate a ridiculously big amount of code.

## general closures (Feb 8)

- i did a special case for if but need to do that for all.
  resolver does it right but need to be able to inline call into same result.
- painful debugging but made logging a lot better
- want to track captures so know which need inline.
  was confused by it thinking almost everything was a closure but it's finding the type constants in the outer scope.
  i guess constants never need to be treated as captures since you just inline them into the function anyway.

## scope (Feb 7)

- do arg vars based my new thing
- switch the look ups on GetNamed to GetVar, etc.
- allow shadowing
- now that vars are numbered, how to do inlining becomes obvious.
  i ended up just doing it on the bytecode tho but might want to renumber the ast as well later?
- forgot to resolve types in functions/vars which was very confusing when tried to switch builtins to being definied as constants in a prelude.
- putting a bunch of cfg checks in logging functions (mostly in parse.rs) makes it like 2x faster (release mode)
  so i guess it wasnt optimising out my big recursive traversals even though the log macros did nothing.

## Interp (Feb 1)

Having a linear type system seems very debuggable.
Every computation is put in a new stack slot and replaced with a Poison when passed to a function.
Every time you read a value, check if its poison.
Im sure this is super slow tho.
Pretty cool tho, already catching stupid things like forgot to implement add, just left builtin body empty, lol.

## Tree Sitter (Jan 28)

I need to figure out what the `•` symbol precicly means in error messages. Brackets not in quotes are like s-expression where the first thing in the list is the name of the production and the rest are its arguments. I think `•` is a sequence separator. So `(call_expr _expr  • tuple)  <-> call_expr(seq(_expr, tuple)`

## Side Tracked (Jan 27)

I'm really annoyed by how slow swift's type inference is so i want to try to write a simple version so maybe ill understand it better.

cool how you kinda build up tools for starting projects quickly.
can snatch the lexer from my relational algebra project, tho i guess normally you'd just have a generator for that or whatever.

string pool.
idk if lifetimes are gonna get too annoying.
`*mut str` eq+hash uses the address value, not the str hash impl.
fixed that with a wrapper that does the unsafe deref.
tried to make it generic over anything that was already hash+eq but couldn't figure out how to express the bound that `&T: Hash`
actually no, thats not the problem, its that you need the `T: ?Sized` on the impl as well as the main struct generic. Similarly, you can't derive Copy/Clone
because it implicitly adds that bound.
