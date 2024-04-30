## working towards replacing 'enum Value' with bytes (Apr 30)

- Made Value::Heap just be the pointer. I have to track type anyway so i already know the length.
- can get rid of the hack about reconstructing slices
- c call go though ints instead of values
- reduce the big dispatch thing for turing values into asm consts
- can get rid of the values version of serialize
- need to get rid of type_of_raw because you don't be able to do that when its just bytes. but thats fine casue it was only used redundantly it seems.
  same for type_of(Value)
- switch overload set to tagged_index because i live in fear
- use a generic over InterpSend to create Expr::Value when the rust type is statically known.
  rn its worse cause it has to allocate the vec even for ints but makes a thinner interface to switch to byte repr and after that's done it will be better than the old way.
- have to be careful about which rust ffi types get wrapped in a unique and which don't. like i64 is the same on both sides.

Almost successfully changed Values::Many to be a vec of ints instead of tagged Value.
Only problem is tests/flat_call where its trying to pass a function pointer to the compiler as a callback.
What used to be a GetNativeFnPtr now gets serialized to an int and its a problem if you try to emit asm for one thats already been turned into a pointer?

## ConstantData arena (Apr 28)

- The idea here is that unsafe language with no const pointer type yet,
  but it gets pretty scary if you mutate something the compiler expects to be constant (like an interned string).
  So put all the constants together and any time you build up a whole page, mark it as read only.
  So there's a chance that a program trying to do crimes will fault but I don't have to pay the price of adding checking to every dereference.
  Doing it with 16K granularity isn't really enough to be reassuring but mprotect is already ~10% of compile time because of the jit stuff, so i dont really want to do extra on every ffi call.
  I really need to work on a more clever page rotating scheme.
- I've dug myself such a hole with the lifetimes that I mostly just leak everything, so might as well put it in an arena and not pay for the book-keeping.
- I think I'll need to be strict about memory segments when trying to emit an exe anyway so might as well start now and have everything go through a central point.
- Allowing constants with internal pointers is weird for exe so being able to reason about whether its in the range and needs to be remapped seems useful.
  Then maybe this should store type info too because what if you just happen to have a large int constant in the range by chance.

ok fuck it, the parser isnt actually in parallel (i looked in Instuments) because it needs to wait on files at the beginning because it needs the whole top level before it can to anything.
and i cant add more threads because then the contention for the intern pool lock is too high and it gets way slower.
so no point in extra complexity of thread stuff.

## lox ch 1/2 (Apr 27/28)

Feel like i need to do some project in my language to see what the biggest pain points are to decide what to work on.
Gonna go through crafting interpreters again and keep a list of problems I have.

Did:

- more pointer math helper functions.
- print negative numbers.
- round float to int asm and print that. TODO: llvm
- inline the basic functions (like add) asm so dont have to do the jump.
- load/store for u8 for working with CStrs.

## out of order constants (Apr 27)

Currently constants can bind names out of order but they still get evaluated in lexical order.
So functions can reference each other becuase evaling that statement just means putting it in the overload set and bodies are done later, but normal constants can't.
Want to not do 'eval_and_close_local_constants' at the beginning, just do it when you try to read a constant for the first time.
I've already got the constants stored globally, so don't have to worry about needing to clone them with the functions.

- now treating 'Unit' as type unit even tho has type annotation? but it didn't even try to eval that const? im printing. its getting in consts map before somehow.
  oh parse puts empty tuple out as 'unit' so since i dont eval the const early, it doesn't get cast by the type annotation, and since there's a value there not an expression, it doesn't think it needs to bother declaring it.
  using '()!type' for that value fixes half the tests.
- checking for saved type being finished instead of just that the expr is Values fixes all but 1.
  I should make my handing of casting values more consistant maybe. Anything with unique types and overloadsets vs Fns is a bit fishy.
- the last failure is parse_asm `self.program.overload_sets[i].pending.is_empty()` but it works if you turn off that debug check.
  but still, why'd it change? i guess that makes sence, evaling the types of a function might cause a data dependency on some constant that causes a new instantiation when evaluated, that's probably fine.'
  just extend and keep looping until they're all done. it happens only for 'fn init' currently which is like pretty popular so that's fair.

damn that was like not at all painful. i guess i did most of the work before when i moved local_constants to hold names instead of expressions.
that's so pleasing, can finally just do shit. can put all my imports at the bottom :)
Would be nice if I fixed the template instantiation order stuff because now that feels really out of place.

That's also another 50% less make_lit_fn because those are for evaling expressions and now a bunch of constants get skipped.

The tangible benifit of all this would be if panic could print a stack trace but i think that will need fixing the calleees tracking for larger mutually recursive graphs.

made sure inline asm expansion on resolve_body not when added to the overload set. if you don't call it, it shoudn't bother.
it was. so not every program needs to jit the float functions every time. it happens in emit_body which makes sense.

## lazy parsing (Apr 26)

- was using .source to get a str to slice so it was taking the whole file not just my little span. which was fine before when i always did the whole file at once.

I was thinking i'd be reparsing for every resolve when they're nested because the same source text will get multiple numbers.
because when you clone the ast for resolve, it would stop at the nested function bodies and they'd get different index numbers.
actually i think it cant happen because even if a clone just clones the number, when it looks up the index, it wont recursively do work.
cause only the parser adds things to the list of tasks, the resolver doesn't trigger work, it just waits for existing work to be done.
So no matter how much you clone the numbers, you can't trigger two paths parsing the same thing.

## fewer fn clones (Apr 25)

- doing the comptome memo lookup before the clone makes Consts.log go 2200 lines -> 1800 lines. saves 200 States::fn_body_resolve
- previously quick eval always failed if it was a comptime_addr in the compiler because the FnBody wouldn't be in ready. make_lit_fn 2423 -> 2346. meh.
- UInt/SInt seems to be the main thing not in 'ready'? oh im dumb, its once per run: the first time they get called for a const, then after that they're ready and can just be a jit call. that's not so bad.
- operator_star_prefix is often an overload set not a function.

quick eval overload set with no pending and one ready: make_lit_fn goes 2346 -> 1465.
can't do it when arg is macro !addr or !slice becuase then you're trying to const eval pointer onto the stack frame.
the only place that shows up is my inline asm where i always pass the slice of insturctions to something that allocates it as a list to return.
discovered by skipping (name == "lst" || name == "heap" || name == "fcmp_with_cond" || name == "hackheap") by trial and error.
its kinda handy that any non-bootstrapped inline asm bug breaks tests/floats cause thats such a random one to not work.

so thats ~40% less lit_fn and made debug mode --no-fork ~5% faster.

// DID: just treat @builtin as a normal expression instead of a magic thing that const looks for so you can do 'const Type: @builtin("Type") = @builtin("Type");'
// then you dont have to eat this check for every constant, you just get there when you get there.

// DID: allow macros do add to a HashMap<TypeId, HashMap<Ident, Values>>,
// to give generic support for 'let x: E.T[] = T.Value[] === let x: T.T[] = .Value' like Zig/Swift.
// then have @test(.aarch64, .llvm) instead of current special handling.
// also add a new annotation for declaring macros with some other type as the argument where it const evals the expr before calling the macro.
// that should be doable in the language once I allow user macros to modify functions like the builtin ones do.
// -- Apr 19

## dont resolve vars up front (Apr 24/25)

did a bunch of refactoring the scope system yesterday.

You get to a problem where you can't call a comptime function,
because you can't resolve the overload, because you need to know the types,
but ive paired resolving the argument names with the argument types because what about when you're allowed to refer to previous arguments.
Can't go back to @comptime args not being const because you want to use them in types of declarations in the body, if anything i want to get rid of @comptime.
Maybe start with the easy @impl case where you know the types without binding the args.

So answer is have resolve_sign not bind argument names, just resolve the type expressions, which works for anything that's not @generic
because you can't read argument vars in thier types. Then had a very long problem of it trying to do an overload on the template
version of functions in an @impl @comptime, so they would never be bound. but it also wasnt finding the specialized versions.
needed to add an extra block around args because all the args and constants were going into the function's outer scope,
so specializations would shadow which is an error so the resolve would fail and I think a mut_replace! somewhere in the
overloading so it lost the function.

then little problem of now generic args dont get renumbered.
i'm declaring args during resolve_sign becuase I wanted to have later args depend on values of previous,
but my tests don't use that and the whole type checking system isn't super setup for partial argument types where you need to
know previous arg values for type of later ones. tests just use generic return types so probably fine to limit to that for now.

## stricter closure capture handling (Apr 22)

:ChainedCaptures

trying to have stricter rules about captures. started by marking const args as const vars in resolve binding,
which caused two tests failing (before this check) on Missing resolved variable. with this check six fail. but this really shouldn't be happening.
seems the problem is when you call a captured const Fn. but i dont see how it worked before becuase it still must have looked up variables in the rusult for set stmts.
maybe in emit_body binding the args? the pattern used to say it was a var so it would get added as just a tpye and the constant would get found somehwere else latter?
Actually I bet the problem is in type_of because it doesn't happen in test/closures, so feels like it could be
just when complicated generics means it has to backtrack to infer types so hasn't properly got to the point of dealing with the closure but tries to type check it too soon?
but that doesn't make sense because im hitting the error in capturing_call. surely I don't get there before doing the rest of the body before it?
I wasn't adding closed_constants in bind_const_arg but that didn't help.
Oh the other thing is expanded macros don't recompute captures, which might be why changing Option::if to not use @match helped.
I would make sense if it was broken for fn stmts because of constant hoisting trying to compile the body before, but that doesn't happen because they become an overload set.
its always Expr::closure values that are broken.

I think there's some situation where it only figures out that it needed to have captures too late so its already created a new result.
Like you're doing a capturing call and then you pass a closure into another function as a const arg,
but it doesn't realize that function needs to be a capturing call until it gets to the body where it calls the argument?
OH! I think when there's a chain of passing const Fn through multiple capturing_call, the first time the argument gets bound,
it assumes the capture was handled when it might be from an upper caller so should have stayed a requirement of the
newly specialized function. am i doing it from the inside going out instead of top down?
bind_const_arg just extends capture list.

Spent such a long time on something that should have been easy.
Feel like I need to be more rigorous in making the compiler easy to reason about.

## scan ahead to resolve constants (Apr 22)

(plan)  
I want to change how identifier resolution works so its breadth first instead of depth first so you can have out of order constants.
So I want the resolve pass do be done gradually as part of the compile pass.
So you do a whole scope to get names before going into implementations and doing thier names, so you can skip resolve if the code never runs.
The more interesting goal being having a normal macro that says "oh you're trying to compile me? i'll parse this header file and spit out all those names into your scope".
I think my existing broken (they can't even nest) module system is more work to keep than remove and rewrite a better one later.
Ideally a module could be just a struct of functions (like zig) but then an @using macro can add those names to your scope.
The other weird thing currently is constant hoisting.
All constants in a function are pulled out during resolve so macros can't expand to blocks containing constants.
But I do the resolve pass doesn't backtrack so normal code can only refer to constants lexically before it because otherwise the names wouldn't bind.
So its the worst of both worlds.
Also expressions that disappear because of constant folding and shouldn't even be compiled,
still have thier constants hoisted to the function so you can't use that for platform specific stuff which is sad.
I want `#[cfg(whatever)]` to just be a normal comptime `if`.  
(end plan)

- removed modules. easy. only a toy test relied on them.

doing scan ahead in scope: (end result will be can bind to later declarations but stil generally can't use them because compiled in order).

- needed to move `const FatExpr: Type = @builtin("FatExpr");` up above `#include_std("compiler_macros")`.
  I guess it worked before because the builtin identifiers get checked if a name is unresolved but now it gets resolved dispite being declared later,
  but they're stil evaluated in order so it doesn't exist to get closed over?
- Then my `const T = T;` hack in comptime functions that fixed some old renumbering bug (i think), breaks now because the value T binds to the new declaration T.
  But i seem to have fixed things enough that sometimes making the arg constant works without the extra binding (even if still seems redundant if the func is comptime but since inner constants need to refer to it, makes sense).
  Thats enough for half the tests to work.
- switching memcpy to a trailing lambda instead of const binding make another quarter work. need to go back and fix that.
  it can't find value for the T in fn set's signeture?
- now a bunch of no stomp assertions are triggering in comptime functions cause I took out the rebinding hack. forgetting to renumber somewhere?
- adding renumber to emit_comptime_call broke everything. had to add handling for functions. was forgetting to remap function names and closed vars.
  so it couldn't find like outer comptime args (or anything at all at one point).

ahhhh such painful mistake (err below).
When compiling an if, I require the args to be functions but I always inline the call, so its a problem if I try to compile twice, cause the second time it wont be a call.
(interestingly that means if branches always have thier own constants lists so what i said before about not being able to do platform specific stuff was wrong).
But I originally solved the problem by `arg.ty = Unit` and then if it was unit instead of unknown at the beginning, I'd know we'd done it already and just return.
But because I'm dumb, i'd always return unit as the type of the if expr. Because I did that for while loops and it made sense because they don't return a value.
and apparently only now with my ordering changes, you hit an error in a type infer somewhere and end up trying to compile and if that returns a value twice.
so you get a sanity ICE because it says its a unit but last time you were hear it was some useful type.
now my fix is check if its a function type and if not, just typecheck the branches are the same value.
which fixes my current tests so yay, scan ahead works.
HOWEVER ITS WRONG! now you can't have an if expr that returns a function. TODO: write a test for that and actually fix it!
maybe just add an extra marker to the end of the macro args that its a value if not a function if. or change the macro to ifx.
theres no real reason it has to be functions as arguments, I just think thats more consistant because one of them isnt going to be evaluated.
could have the macro !if just be expressions and generally call the function if that takes lambdas once type inference is more reliable.
now that macros are a more normal part of the language, its not that weird for an argument to not be evaluated.

```
sanity ICE. Type check expected Ty2 = Unit but found Ty465 = { Some: i64, None: Unit}!enum
 --> main_file:84:71
   │
84 │         (self.is_some(), fn() O = then(self&.Some[]), fn() O = else())!if
```

So the point I'm at now is that you can resolve to constant names in the same scope but lower down.
However, its not useful because I still evaluate them in order, without any clever dependency chain analysis.
So you still mostly have to have your code strictly ordered.
Even functions which would almost work because body isn't compiled until needed can't call out of order because the declaration needs to close over the overload set which might not exist yet.

## improving macros (Apr 21)

Syntax sugar for !quote/!unquote goes a long way.
Now its not too bad to put the whole logic in an unquote so I can use that to make lexical scope work instead of @with_var which was really annoying.
TODO: Still can't do constants because of hoisting tho which is sad.

from a long time ago:
// TODO: dont do the builtin ones this way. put them in thier own function and expose them as @ct @comptime_addr() @call_conv(RustPrefixMacro) === fn name(&mut self, arg: FatExpr, target: FatExpr) -> FatExpr;
// then you don't have to eat the these checks every time, you just get there when you get there.
// plus it makes the transition to writing them in the language smoother. until then, the only cost is that it becomes a virtual call.

started doing that. did it for @enum.
can't do it for @as yet because I use that in quoted things without @literal so it might not be closed over but I can't invoke an expression as a macro yet.
I can't decide if the destinction between var resolution and captured const values makes any sense.
Ideally the vars would always be unique so maybe the extra level of dynamic scope is just because I wasn't rigorous enough with renumbering originally and now its hard to reason about.
The current thing means a raw identifier is never useful sincei t can't be from the caller's scope (wouldn't resolve) and can't be from the macro's scope (doesn't capture).
But its applied inconsistantly if caller they happens to have already captured it, you won't get an error.
I guess its the same problem as rust letting you use relative import paths in macros so I end up needing to import stuff when I add InterpSend to a new file becuase I'm too lazy to actually fix it.
added @(e) for comptime overload set, but its still a bit broken. works for as so at least its slightly more consistant now.
feels like it made the compiler like 10% slower which is sad but also not worth bothering about until i fix the thousands of redundant ast serilizations.

Really need to stop doing the stupid export_ffi wrappers. got confusted by pasing the wrong macro impl and of course args matched cause they're all exprs so error message wasnt obvious.

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
Fuck, it's on a function labeled `// TO-DO: this needs to be an error`.
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
