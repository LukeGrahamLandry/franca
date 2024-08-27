## debugging on x86 (Aug 26)

v2;  
so tuple2 is getting the right type args but it segfaults if i try to print types.len so comp_ctx ptr is garbage?
yeah the hack number im passing to emit_bc_and_aarch64 is not the one i get out, but it is on arm.
ooooooooo kkkkkkk now it seems to be working....
maybe i didn't recompile comptime_cranelift for x86 since i changed how comptime_ctx_ptr was passed...
i feel like i did.... but i must just be super dumb.
but anyway now i can run mandelbrot on franca2 x86.
v1 doesn't work still but that makes more sense because it has to deal with matching rust's c abi.

---

v2 can't `run_tests -- cranelift`. maybe `DirEntType.File` has a different value on x86 macos libc?
no `clang libc_constants.c -target x86_64-apple-darwin` gives the same results and file says `a.out: Mach-O 64-bit executable x86_64` so it seems to have worked.
but my basic_libc walk_dir test works on arm with v2 but not x86.
different DirEnt layout? seems like no.
but cross compiling `franca examples/default_driver.fr test tests/basic_libc.fr -aot=llvm -x86` also doesn't work so thats definitly the problem.
`println(entry.name().len());` gets garbage lengths.

what the fuck?? ok so i can write a c program that uses readdir_r and works, but if you look at the llvm ir clang produces,
its calling `readdir_r$INODE64` on x86. on arm it just calls `readdir_r` like you'd expect.
and sure enough if i manually edit the ir i produce and call `readdir_r$INODE64` it works on x86.
how the fuck am i supposed to know that??
oh damn if i google that string its just a thing people know,

- https://www.reddit.com/r/Compilers/comments/fuosy2/psa_link_name_for_readdir_on_64bit_osx_is/
- https://github.com/rust-lang/libc/issues/414

and if i look in my dirent.h

```
int readdir_r(DIR *, struct dirent *, struct dirent **) __DARWIN_INODE64(readdir_r);
```

and then in cdefs.h

```
#define __DARWIN_INODE64(sym)           __asm("_" __STRING(sym) __DARWIN_SUF_64_BIT_INO_T)
```

and

```
#  if __DARWIN_64_BIT_INO_T
#    if __DARWIN_ONLY_64_BIT_INO_T
#      define __DARWIN_SUF_64_BIT_INO_T /* nothing */
#    else /* !__DARWIN_ONLY_64_BIT_INO_T */
#      define __DARWIN_SUF_64_BIT_INO_T "$INODE64"
#    endif /* __DARWIN_ONLY_64_BIT_INO_T */
#  else /* !__DARWIN_64_BIT_INO_T */
#    define __DARWIN_SUF_64_BIT_INO_T   /* nothing */
#  endif /* __DARWIN_64_BIT_INO_T */

```

we need to appritiate reddit bro for a moment

```
// https://github.com/peterdelevoryas/mylang/blob/master/src/llvm.rs#L52
for func_decl in &module.func_decls {
      let lltype = type_bld.func_type(&func_decl.ty);
      let mut name = func_decl.name.deref().to_string();
      name.push('\0');
      let mut link_name = name.as_ptr() as *const i8;
      if cfg!(target_os = "macos") && name == "readdir\0" {
          link_name = "readdir$INODE64\0".as_ptr() as *const i8;
      }
      let llfunc = LLVMAddFunction(llmodule, link_name, lltype);
      llfuncs.push(llfunc);
  }
```

thats funny.
ok so this is just how life is i guess.
now i need to make a better #import that lets you set a link name.

---

now back to trying to get v1 to work;  
changed the name of the self hosted lib file so it doesn't collide with the rust one and that got rid of `ld: warning: ignoring file 'target/x86_64-apple-darwin/release/libfranca.a[19](libfranca.o)': found architecture 'arm64', required architecture 'x86_64'`
but that didn't help.
if i compile the rust with `TRACE_CALLS = true`, it panics on `Expected function to have scope` instead.
so thats interesting.
tho arm fails a safety check with that so maybe its just all fucked.
maybe x86 doens't have the same field alignment rules? but sizeof(Func) is 448 on both.
actually maybe that happens to be sorted to not have obvious padding.

```
pub fn main(){
    #[cfg(target_arch = "x86_64")]
    println!("sizeof(A) = {}", core::mem::size_of::<A>())
}
#[repr(C)]
struct A {
    a: i64,
    b: bool,
    c: i64,
}
```

on rust playground says 24 tho so that seems to be normal.

- make_and_resolve_and_compile_top_level print stmts, they seem reasonable
- printing names of functions as they're added also panics on `Expected function to have scope`.
- compiling rust debug instead of release makes it `assertion failed: func.get_flag(NotEvilUninit)`

so like everything's just confused. and it changes if you try to observe it.
feels a lot like the instruction cache flushing problem i had a long time ago but the whole thing we learned there x86 doesn't do that.

---

cleaning up some todos

- (run_tests) @assert
- (mandelbrot) fix += with new place expressions.
- a bunch of places with hardcoded exe paths from when i used execv instead of execvp

## supporting x86_64 (Aug 25)

x86 inline asm because the compiler uses it for comptime c calls. just passing a string to llvm for now.

- magic incantation `.intel_syntax noprefix` from https://stackoverflow.com/questions/66532417/how-to-inline-assembly-with-clang-11-intel-syntax-and-substitution-variables

bringing back cranelift because i want the compiler to work on x86_64 but i dont want to deal with it yet.

- int intrinsics. failing 50
- switch. failing 35
- fcmp. failing 23
- float casts. failing 11
- clone stack before each case of switch. failing 1, but its the runner_segfault that always fails.

fixed runner_segfault on old.

now to make it actually run on x86 i get the thrilling job of hunting down everywhere i was sketchy with abi stuff.

v2 dying in tuple_of after a call_dynamic.  
v1 dying in platform_mmemmove after hoist_constants

- put_constant taking by value instead of ref. that didnt help but probably will eventually.
- call_dynamic_values took a slice as an argument. converted to ptr, len.
  that moved the problem, only for v1 not v2, which makes sense.
  now v1 isn't crashing, it just thinks theres no overloads for bootstrap_compiler_environment which is kinda worse cause you don't know where to look.
- make_and_resolve_and_compile_top_level, unquote_macro_apply_placeholders, insert_owned, and tuple_of took slice arg. didnt help

## hack overloading some more (Aug 24)

symptom: `arr: Array(i64, 3) = init(@slice (7, 8, 9));` choosing the overload for `Array(i64, 5)`
It works with the arg being i64 but not []i64 or \*i64,
so i think its about suspending in the argument type.
so the answer that fixes this is to not continue after infer_arguments suspends in resolve_in_overload_set
but that makes it segfault on run_tests.fr

can make it work by taking out hte block where we infer args or continue which should be fine because you do it one arg at a time later,
but that makes compiling run_tests llvm path segfault.
also broke overloading_cycle which i guess im less attatched to cause the old one couldn't do it.
and apparently the new one only could by randomly guessing.

---

aot emitting forwarding functions for redirects brings llvm to parity with aarch64.
still kinda feels like a hack becuase i didn't need that before.
but its certainly nicer if compilation order matters less

## repl (Aug 23)

have to do some fixing up of the tree to make runtime vars work nicely.
you cant just compile into a function because you don't have all the statements that might want to reference your variables.
you can't just make a new function, append the statement, and rerun every time you get a new one because what if the print or access the file system or something.

## gui (Aug 22)

initial gui for introspecting the compiler.
lets you click through functions, overload sets, and constants.
not super helpful for me yet because you can't use it if there was a compile error.

## fixing aot on new sema (Aug 21)

- resolve named args. mandelbrot works now. can't do a single named arg yet.
- (old) problem after making neg #fold because i write fmt_hex for llvm before i had a shift_right_logical so i was using div and it only worked when the high bit was 0
- implement check_for_new_aot_bake_overloads
- forgot to call created_jit_fn_ptr_value based on TookPointerValue in dispatch.
- TODO: deal with execv vs execvp

> failing 11 + 175

now problem is it doesn't fold and thinks we need GetCompCtx. like calling size_of in arena.
Was only folding when `!self.dispatch.enclosing_function.is_some()` to prevent recursing but thats not what you want.
in old sema you could just mark expr.done because you know you only get there once but now i have to be able to suspend.
skip fold when we're in a function made by imm_eval helps a bit.

> failing 9 + 174

but now llvm not failing on GetCompCtx, tho its still `!!!! failed to emit speciai function: size_of` which is a bad sign.
but the actual error is i64 vs ptr of last argument to new_arena_chunk, passing a const null pointer, so thats a `.None`.
so the problem is my new contextual_field of tagged becoming a value instead of a structliteralp, and then emit_bc gets the wrong prims for it.
adding :tagged_prims_hack to emit_relocatable_constant_body fixed a bunch. mega ugly but progress.

> failing 9 + 32

always fold functions created by imm_eval so you never add them as a callee to a runtime function.
hard to think about and broke the `floats` test because of a now missing coerce_const?
but fixed a bunch and type errors are less scary than an incorrect callgraph.

> failing 10 + 24

now the problem is with redirects.
i can make it better by if the target is special, handling its body right there and pulling forward the body if its an intrinsic.
breaks the first case in `deduplicate_functions` test tho.
I think things aren't always getting added as callees correctly?

> failing 11 + 14

## fixing overloading (Aug 20)

- rust program with wasmtime and some libc shims to run most tests as wasm.

> failing 32

HACK.  
just say no type hint for the first argument when resolving overloads.
thats good enough to not incorrectly coerce a constant and then fail to match on a runtime second arg for the binary arithmetic which is what it all ends up being.
eventually i'll need something more robust but its not like the old version worked super well either.

> failing 26

regression add a @as for `mask: u32 = 1.bit_not()`. since thats a single arg, im now skipping giving the literal a type hint,
and then since i don't have i128 anymore i think its a negative number and don't let it coerce?
also wierd ness about if my new thing is doing #fold enough.

> failing 23

had a TODO when you suspended after removing some of const args and then circled back.
for something like `get_field_ptr :: fn($S: Type, self: *S, $f: *Field) *f[].ty #generic`,
you'd replace the first argument and suspend on the second.
arguments are removed from the cloned function as you go because later ones might refer to the bound values,
but we leave the arg_expr alone until the very end.
so now when you try again, you know how many args have been removed from the function, and just skip over that many constant args before doing more processing.

> failing 17
> failing 15

exec_task.Jit calls copy_inline_asm fixes all the #asm tests.

> failing 10

## procrastinating fixing overloading (Aug 19)

- track stack depth so more nested early returns work
- struct field default values, contextual fields
- allow DeclVarPattern with no name because @match can make them
- fnptr -> rawptr coercion
- llvm target wasm. hack around 32 bit pointers in constants (i still just pretend everything is 64 bit and pad const arrays with zeros).
  js shim of a couple libc functions to run mandelbrot in the browser.

## debugging new sema (Aug 13)

variables declared in quote expressions (like in @match) don't get renumbered (also didn't in old sema),
so new ones are seen as the old type if you lookup in the global thing what the type of the variable is.
i don't understand why it worked before since it was using the same `get_var_type`.
wait, nvm, unquote_placeholders renumbers at the end, so i was just logging too early (inside the @[] expr),
and seeing the template var names. So the new one is indeed getting new vars each time.
ok but the new one isn't renumbering all the way into the switch expr,
the `callable` is always referencing the old `arg_ptr` somehow.
which again i don't understand because the old one also calls into the self hosted renumber_expr.
The problem was not cloning the arguments of unquote_placeholders so the first renumber would
mutate the template of a quoted expression was used in an unquote, and then subsequent renumbers wouldn't see it. :double_use_quote

##

format_into.
commenting out the expr.done shortcircuiting works so we know its something wrongly being marked done.
the problem is that im leaking a loss of dispatch.enclosing_function somewhere.
hackily setting it back every compile_expr call lets it get further. still need to track down the actual mistake.

## (Aug 6)

- crippling bug with `expr[] = (Tag = self.box(expr[]));` where you have to make a var for the self.box(...) or it segfaults

##

- thinking about how to do #generic without forcing you to mark it and letting the expressions compose better.
  need to move argument typing to the bind_const loop. added a ResultType that holds a tuple so you can have partially known args and pass that down.
- but first, tuples with fields should be easy.
- back to consts. so just delaying infer_types and doing it one arg at a time as enough to make it work.
  so the remaining problem is that you still have to precompile the argument
  because you want to know its type in const_args_key to check if its a tuple, but that can just be optional.

## debug info (Jul 31)

- lldb doesn't see it. oh, need to connect the !SubProgram with the functions?
  `inlinable function call in a function with debug info must have a !dbg location`,
  complaining again is good i guess. adding that just segfaults tho...
  maybe its https://github.com/llvm/llvm-project/issues/92724
  thats the function its crashing in tho it doesn't show up multiple times in the stack trace,
  idk if that means its not recursing or they just don't want to spam you.
  oo i bet this one https://github.com/llvm/llvm-project/issues/59471
  yup, you just also need it for DILocalVariable, same segfault.
  so now its back to the state of `warning: no debug symbols in executable (-arch arm64)`
  ok that goes away if you put a real filename in the DIFile.
  it doesn't have to exist, you just can't have the directory be an empty string or it swollows it silently.
  so now in clion, i can see the functions in the stacktrace have my nice names instead of the mangled names.
  i can't see the variables tho. but `frame variable` in lldb lists them (which it didn't before), so its just clion that hates me now.
  in lldb if i break in main and then step i can see the value of the variables.
  i told it pointer so i just see 8 bytes always but i can see `max_steps = 0x000000000000002d`
  which is 45 which is correct. so the value there is whats behind the pointer, not the pointer that i passed to dbg.declare.
- more new sema work
  // Since were evaluating in const context, any functions that are called in the expression weren't added to anyone's callees.
  // So we want to say we need to recompile the expression, adding to callees of the lit_fn we're about to make.
  // I think this is not enough, and we need to deeply clear done?
  // so really we just shouldn't have tried to compile outside a function context! -- Jul 31
  // it works without clearing done here if you remove :the_compile_above, but you kinda want to do that for GetVar on a const
  // i guess that should just be handled in quick_eval and just deal with the occasional redundant work.
  // changing that should make us more robust to "ICE: Tried to call un-compiled function."
  removing the compile attempt brings lit_fn up to 24 but adding a quick_check for already compiled constants brings
  it back down to 5 so it's fine.

## tiny tests on new sema (Jul 29/30)

- got it to the point where it can load a test file and use the existing parse/scope/bc/asm systems.
  so thats very pleasing because now there can be a quick feedback loop as new features are implemented.
- addr/deref
- call zero arg functions, needed to track callees in the new dispatch system
- early return

## Switch (Jul 27/28)

Added a Switch ast node and bytecode instruction for @match to use instead of a chain of ifs.
The thought is that llvm has a switch instruction and maybe it would be faster if i just told it what im doing,
instead of it having to do work to figure out my chain of ifs.

It seems llvm takes about the same amount of time, but it does make my part go from 455ms to 425ms, which is interesting.
I guess we've learned that my thing where blocks are functions that then get inlined so my old match was kinda copying out this
whole linked list kinda thing and then collapsing it all down again, was dumb and if i just have list of expressions its faster.
(note: it doesnt seem to mater which version you're compiling with, its which version you're newly compiling, its just a library change in @match).
I have mixed feelings about adding redundant things, like it would be conceptually simpler if you just have ifs.
but also 5% for like 80 lines extra (once i remove the old @match impl) is pretty damn good.
and it kinda is easier to think about if you ever have to look at the ast for debugging the compiler if it matches more closely with whats actually happening.

- @match on enums (not only for @tagged-s), down to 415ms.
- maybe I'll try making @if at least not expect closures so insert the call in the code instead of in the compiler.
  that takes it down to 405ms. @loop remove extra lambda makes it 395ms. and those changes were nice because they deleted code.
- TODO: qbe inst_switch, use Switch node for @switch

## (Jul 25)

- ask the compiler which safety checks are enabled. i have some bug with order of compilatio nwhen disabling debug_assertions on the compiler.
- finished porting bloat.fr from size.py
- fixed fork_and_catch to poll the pipes so it doens't block on big outputs.
- discovered how fast you clear the pipe is the main thing that determains how long it takes to disassemble.
  bloat.fr running objdump on the compiler takes 430ms jitted and 230ms llvm-Os.
- wrote a generic example driver program so its easier to just run random programs with different backends.

## Admiting intrinsics are special (Jul 24)

The currently basic math like add is defined as just functions with inline assembly.
Which i think is cool because it means you can click into operators and see what they do.
It also forces making the inline asm language feature good because its tested for absolutly everything which is good.

But its perhaps noteworthy that almost every real language doesn't do this, perhaps they know something I don't.

- It makes a weird dependency so some of the inline asm stuff needs to make sure not to do anything fancy
  because most stuff can't be compiled yet (which is why I need two versions of @bits).
- It's got to be slower that i have to compile that stuff for every single program. Like its not that much work,
  but if it were just already done it would be free.
- If you break anything along that path its a nightmare to debug because you can't really make a minimal test case because there is no action you can perform.
- I end up typing out a bunch of dumb strings (llvm ir for example, instead of just templating in the one operation).
- It generates dumber code. My asm backend doesn't know you can add any registers other than x0 and x1.
  It gives the llvm backend more work to inline it all probably, like its a super easy choice to inline the one add,
  but surely it would be faster to have it already be done. I wonder if im wasting some optimisation juice on that.
  I wonder if assembling the output of qbe is so slow because it wasn't expecting to resolve that many symbols,
  and is tuned to most instructions actually being instructions, because otherwise i don't see how it could possibly be slower than llvm doing optimisations.
- I want to get rid of the @BITS vs @bits distinction, having a file that looks like user code but you actually can't edit without
  careful thought about internal compiler implementation details. is a bit silly.

So far converted int operations. Didn't make assembling much faster. Still worth it tho.
Maybe im having this guy's problem and clang just sucks at being an assembler `https://github.com/llvm/llvm-project/issues/68445`.
Older clang takes 1.5 seconds instead of 2 seconds which is interesting...
i was hoping for actually gcc

```
$ gcc --version
Apple clang version 14.0.3 (clang-1403.0.22.14.1)
Target: arm64-apple-darwin22.2.0
Thread model: posix
InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
$ clang --version
Homebrew clang version 18.1.6
Target: arm64-apple-darwin22.2.0
Thread model: posix
InstalledDir: /opt/homebrew/opt/llvm/bin
```

I also tried real gcc and thats 1.4 seconds

```
$ /opt/homebrew/Cellar/gcc/14.1.0_2/bin/gcc-14 --version
gcc-14 (Homebrew GCC 14.1.0_2) 14.1.0
Copyright (C) 2024 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

So thats enough faster that it makes qbe just slightly faster than llvm.

## Playing with Qbe (Jul 23)

- discovered one bug with llvm tests failing grouped but passing individual.
  when entry points get deduplicated, i don't make shims or call the deduplicated version.
  TODO. but also im pretty sure that was happening before too.
- XProtextService sucks infinite cpu. thank you internet the magic incantation is `spctl developer-mode enable-terminal`
  and then in settings you can see the developer tools tab in privacy/security and then you can add other things (like other terminals),
  and then they're allowed to run new programs without being crippled. that takes running the tests on the llvm backend from 5.5 seconds to 1.7 seconds.
  so thats nice.
- i bet qbe never passes an opque thing in regisers even if its 16 bytes.
  yeah, saying pairs are a `{ l l }` made the temp function stop touching x8 (indirect return).
  that fixed it jumping to null when trying to call through alloc vtable.

```
qbe bug? docs say im supposed to use fake types for calls but this loses the argument.
making it w works. but without -NDEBUG it hits an assertion so maybe the docs are lying?
Assertion failed: (v->t[0] == T.rglob), function spill, file spill.c, line 507.
yeah apparently just use w for everything.

  function l $trivial(){
  @start
  %v1 =l call $something_important(ub 123)
  ret %v1
  }
.text
.balign 4
\_trivial:
hint #34
stp x29, x30, [sp, -16]!
mov x29, sp
bl \_something_important
ldp x29, x30, [sp], 16
ret
/_ end function trivial \*/
```

- hangs on test `buckets` but its something wrong with my fork_and_catch

## (Jul 21)

- made typed ast nodes to replace all the SuffixMacro ones, which i like more cause there's less junk code about decoding the arguments,
  cause you just already know the answer.
  but sadly its slower to compile itself. (and also broke the lox test, todo).
  i wonder if the slower is just because of removing #compiler_builtin_transform_callsite so there's more indirection.
  old 390ms -> new 430ms, 10%.
  yeah cause making single arg if function call macro instead of the two arg function brings it to 400ms.
  so extra const arg function expanding really hurts.
  doing @loop directly in fn while gets me back to 390ms.
  so i guess thats good enough and then change is fine now, but its a bit of a bad sign for my avoidence of builtin things,
  that implies it could be similarly 10% faster if I just had syntax for `if` and `while` like normal people instead of most code going through an extra function expansion.
  lox was fixed by adding a semicolon at the end of a loop body so it didn't try to infer the type of the last expression.
  because now it wants to typecheck before noticing its a loop.
  which is a bit yucky that i changed behaviour but probably fine, dont really care.
- made it a lex error to have a string literal split across multiple lines
  so typos don't have completely incomprehensible errors
  where you can't find any later functions cause they're all in the string
- fully got rid of 'quoted' syntax in favour of @{ quoted }

## deduplication (Jul 20)

- it seems pretty effective. first attempt was ~17% less ir and made llvm take ~8% less time
- trying to do it for jit too made aot try to call non-existant functions. i thought i had to complicated seperate redirect tracking,
  but just adding the redirect to pending in llvm/emit_special seems to work.
  i don't trust it tho, i think it just happens to work because i don't have a test that ends ip calling something in jit and
  then needing to include it as aot but needing to emit differently but the body has already been replaced with the redirect so you can't notice the problem.
- TODO: i reintroduced sometimes broken const temp areana base_size i think?

Moving type storage to self hosted.

- a bunch of pain about when you have to extend_options on the TypeExtra
- typo-ed @tagged starting at size 8 and fold(max) on fields instead of 8 plus fold(max) on fields
- speed is the same it seems? generates ~8% more llvm ir (cause theres more code in my language now) and takes ~8 longer to compile itself.

## Jul 19

- temp alloc had the wrong base_size because baked constants weren't including tagged padding.
  didn't matter cause its just virtual memory but thats one less lurking bug.
  while debugging that found another missing \_\_clear_cache when calling into the compiler for dyn_bake.
  at some point i should just give up and write an interpreter.
  tho i could also try to do something fancy where every time i return or call a function that might be jitted,
  check the target address against the mmapped code segment and only clear cache if its in there and new or something.
  i guess returning should never be a problem? how can you be returning to something that wasn't compiled yet.

// TODO: (maybe) instead of storing TypeId of vars, store size+align.
// then don't throw away fnbody after use, keep them in a hashmap to deduplicate generics.
// like List(\*T) will often generate the same code even with different types.
// can keep a running hash as you construct the bc so maybe pretty fast to discard non-matches.
// have that as a build option so you don't have to take the hit for debug builds if you don't want to.
// but it actually might make it faster in general because it means giving less stuff to llvm which is 90% of the time.
// I kinda want to wait for emit_bc and bc_to_asm to be self hosted tho cause then its less awkward to change things.
// Note: you wont get all the matches until you really deduplicate because they might call different functions that actaully generate the same code,
// so have to have a thing like redirects where if checks if a call has been deduplicated. -- Jul 5

Did the first part of that, now vars are just size + align.
That's also kinda nice cause it decouples the bytecode from the rest of the compiler more,
so it might be easier to try to import wasm or something to bytecode as a way to test that backend eventually?

## debugging ported aarch64 (Jul 17/18)

- its a little too easy to typo a `:=` inside a scope. maybe should think about that.
  or at least warn if a variable is shadowed and also unused.
- had an offset by -4 not changed to -1 now that i use `*u32` for instructions.
- ok we segfault in bootstrap_compiler_environment. trying to call debug_print_int as the first thing does run
  but prints twice and the second time is the wrong number.
  and it sanely prints once when not using the new backend so its just totally messed up somehow...
  and im like doing a lot of calls with the dispatch table but the old version had direct?
  oh im doing every call twice because i had `return;` and didn't call it in one of the cases in branch_func.
- aaaaaa i bet im just miscompiling the code that does the inline assembly and thats why im getting garbage.
  because i changed the brk number at the end of the function and turned off BASIC_ASM_INLINE and the op function im crashing on
  was emitted by the rust code which means it was a copy_inline_asm. ok thats reassuring.
  can narrow it down by finding one function that doesn't work and using the old asm for all the rest.
  slice eq. hangs forever. if i debug_log_int(i) in the loop its always 0.
  heck. in the old version my ldr_uo/str_uo took a u2 size so the could be used for 8/16/32/64
  but in the new version i just called the ones that took Bits (u1, for 32/64) but still passed the u2.
  so what we've learned is i should fix type checking for my little types, not just allow casting any i64 silently,
  and then start using the little types more throughout the jit code.
  and have range checks. i long time ago i took them out from shift_or_slice because you got junk reading a u16 as a i64 or soemthing,
  and i just masked it off. perhaps i should be more strict about just letting seemingly harmless bugs exist in the universe.
- ok now all but 3 tests pass using new asm for all functions.
- huh found a place in the old one that used register_type instead of Bits.X64 for add_im on a pointer which is wrong but happened to work apparently
  thats scary
- typoed a put_jitted_function in flush_callees, saving it to `f` instead of `from`. that fixed 2/3
- funny that the only failing one is write_empty_module which i added yesterday. lucky i did that i guess. again... scary.
  if i hadn't got bored of this being broken and done something else it would have been silently broken for longer.
  ha, the compiler works with new asm, so its just this one test.
  tho now i cant make the test work with the old asm and it seems like jsut a missing overload instantiation... maybe im going crazy....

// this is slower than the old version. i suspect because of \_fold_shift_or,
// i can see in objdump llvm doesn't inline it away and its calling brk,
// which is doing loops and shit when it could just be a single or.
// need to do more of the work in the macro like i did originally so it just expands out into math that's easy to optimise.
did that ^. now the function for the emitting brk is only 4 instructions... which is never called because llvm always inlines it.
fantastic.

## Jul 13

- converted my comptime ffi call asm to my language.
  can compile itself but comptime_float_call fails.... but not in the RustRover debugger, unless no_fork... so thats fun.
  thats really creepy, i just had the registers totally wrong.

##

```
[
  {
    "label": "Franca Compiler",
    "command": "cd /Users/luke/Documents/mods/infered/compiler && franca first.fr && clang++ /Users/luke/Documents/mods/infered/target/aarch64-apple-darwin/release/deps/libfranca.a target/libfranca.o -o ../target/release/franca_new && mv ../target/release/franca_new ../target/release/franca",
    "description": "compiler",
    "use_new_terminal": false,
    "allow_concurrent_runs": false,
    "reveal": "always"
  }
]
```

## Jul 9

- self hosted emit_bc is about 3% slower (170ms -> 175ms) at building the compiler.
  note that to be fair you have to build the non-self hosted version with the self hosted version
  because there's also just ~50% more code now.
  and pleasingly it seems the front end times are approx. linear so far.

##

// - did: @match/@switch on enum names so you don't have to say @case in switch.

## Jul 6

- flow result type through function literals + constants so you don't have to put type annotations on @match to use dot field of enum/tagged.
- fix bad index when multiple constant args in #generic

## Jul 5

- stopped hardcoding lib path
- figured out that llvm will keep function names if i don't mark them private.
- made const lookups use a hashmap. made fn find (in scope.fr) go from 19/105 to 0/82 samples.
  need one in every block, not just every scope or you get the wrong answers.
  really smarter would be only start using it when theres a few constants because i suspect all the win is from the top level where all the code in the universe exists and its all constants.
  so i just make it not be n^2.

## Jul 4

- something corrupting the tag when trying to return a `Result(Str, Str)`.
  Making it `Result(Ty(Str, i64), Str)`, kinda seems to fix it but no now the payload string is disappearing.
  Hmmm, my destructuring was leaving the pointer on the stack an extra time, kinda distressing that so much worked like that,
  but easy to fix. I suppose since its a statement, you mostly don't care because you're not flowing into an expression, you just end up with extra junk at the end.
  TODO: test that catches that mistake.
  That also fixed `// something super sketchy happens if you try to use destructuring here. its way slower all the grouped ones are failing but all the singles still pass. -- Jul 1`.
  so thats reassuring.

## Jul 3

- caching slices by addr saves 47/369 KB on libfranca.a (with -O2) (including emit_debug_info not using cache yet, tho with trace off so might not be included by llvm).
- interestingly -O2,-Os,-O1 are make frontend time all approx the same and they're all ~6.5x faster than -O0.
  but -Os makes a binary ~17% smaller and saves about half a second of llvm running on my thing.

## Jul 1

- made maybe_direct_fn handle it being a Label gracefully.
  remove resolve_function because its a copy paste of maybe_direct_fn, it just only handles named constants which isn't what you want.

> I think its bad style to call something that dispatches when we know which branch it will take,
> it feels harder to think about because the other implementation might drift.
> // TODO: go through an see if applying this elsewhere makes it more clear?

##

- forgot to clone after expanding PendingParse cause not going through the rust side anymore.
  symptom was confusing getting same value every time for Option(T)'s arg.
- now deep enough stack that it tried to free the stack trace's constant memory into libc free. had to not do that!
- the self hosted scope now works but makes it 5-10% slower maybe. need to give llvm function names so i can use a profiler.
  i wonder if its just that the scope stuff uses a lot of hashmaps and my hashmap is 150 lines and [rust's is 4000](https://github.com/rust-lang/hashbrown/blob/master/src/map.rs) (not including comments!).
  which feels like they could be doing a lot better than me.
  tho if i make sure ids are unique i could make it a vec and just use way more memory because of constants that aren't vars.

## self hosting feature flag (Jun 23/24)

problem with running parser on llvm is pool returns ?u32.
currently i say thats {i64, i64} but it tries to create one from {i64, i32} which doesn't typecheck.
cause i return small enums by tuple not bytes.
need more complicated resolution of which prims to use based on the varients.
easy cases are when the same or all but one are unit or all ints or all floats but then need to upcast sizes.
only matters when theres one or zero cause otherwise you pass by reference.
mixed ints/floats make sense that you can't do cause it probably wants to pass in registers and then you cant know which are real without checking the tag which is hard to think about.
rustc just uses `[i8 x _]` in that case which i have to match anyway for now since i need to call between the languages.
mainly just a hack solution that's good enough for ?u32 (which is an easy case), would be fine for now.
when i get more serious have abi tests that compare to rust's repr(c), but thats a little dumb until i stop forcing i64 for the tag.

- i think calling convention for span is messed up, for now hack returning as tuple of i64 on rust side
- have to use non-null pointer for empty vec.
- hard to move include_std handling to scope.rs because parser isn't used anymore. eventually want to move it to driver.
- forgot to add flags to pool in init was just doing on parser test
- forgot to lex float.
- args needs to not just read expr for type after : to handle :=
- capitialisation of flags enum so the values match the rust one
- off by one for binary literals
- if_empty_add_unit so overload arity didnt match (first thing it hit was fn ret).
- horrible special case @return -> `__return`

that seems to make it work on first??!!! fucking great success.
not tests tho? `Undeclared Ident assert_eq`.
its printing `=== Aarch64 Jit ===` so its getting into the driver.
wanst adding back the stmt if it had annotations but ended with semicolon when parsing block
which i now use for top level stmts so wasn't getting it for injecting include core.

- fixed lex float
- oh one problem with release mode is the debug typeid bit but i hardcode them in the self hosted parser based on whatever it was compiled with.

removed magic `@return` and now return rebinds every `=` function and `local_return` rebinds on capturing functions.
which is still a bit confusing but probably better.
but i achived that without using the old parser, so thats a good sign that its at least somewhat safe to remove.
it was definitly more fiddly than it would have been before self hosting.
some confusion about needing to rebuild an extra time because driver_api (which you need to change to add shared Flag value),
is built into the rust exe. so should make that just a normal part of the compiler? hard to think about.
I guess you want included like now but the self hosted compiler build against the new driver_api, not its own compile time copy.

Added coerce_constant for fn->fn_ptr. A bit of a hack because you want to insert !fn_ptr instead of just mutating the value,
because you want to delay needing to create the assembly of the function. using an expression defers it and lets the backend do the fix-up.

I'm doing this to eventually get rid of !macros so its easier to parse 'if !cond' but also
its pretty dumb to not implicitly cast a constant function to a function pointer.
It's not ambigous and it does the type check.
Similarly, most other uses of ! are left over from when my type system wasn't expressive enough and i needed lots of hooks into the compiler.
The few that are actually magic (!if, !slice) I can make into @macros now that i have better bootstrapping system for them.
Those are really thier own ast nodes, so perhaps I should treat them as such.
`if` you always call through the function so its chill.

## the parser grind (Jun 21/22)

- stmt var decl
- tuples, struct literals
- calls, dot calls
- assign, update-assign
- macro, quote, unquote
- trailing lambda
- prefix type ops, bang macros, bit literals
- fn exprs and stmts

## debug x86 (Jun 20)

fucking wikipedia lied to be.
system v x86 cc does NOT replace large arguments with a pointer (which sounded reasonable cause thats what arm does),
it just copies them onto the stack. oh an also does it right to left apparently.
this one knows the truth https://www.uclibc.org/docs/psABI-x86_64.pdf  
and godbolt agrees.

i think when farm_game x86 was working before its because i was still using flat calls for macros specifically, and sokol always passes large args by pointer.
cause i can still compile on arm, produce llvm, and then cross compile that for x86 and it works
(other than warning about overriding the module target triple because i hardcode it in the ir but activity monitor says the exe is Intel so its fine).

thats much more of a pain in the ass to deal with cause i have to know the size of the things behind the pointer.
i was so confidently like hey i dont need libffi it doesn't do much for me but maybe arm is just simple.
aaaaa i dont want to do itititititititi. fuck.

i also have to start giving llvm byval attributes on args so i can do x86 ffi.

## (Jun 19)

- have an overload set for changing how constants get baked so don't need a special case `__save_cstr_t`
  to tell the compiler that it needs to walk until the null terminator when deciding what to put in the aot exe.
  TODO: should have slices and lists use that too. lists don't need to keep thier unused capacity
  and should change thier allocator maybe? (its a problem if something tries to call free on readonly memory).

## parsing (Jun 18)

- self hosted math ops parsing and ast logging.
- allow `for x {| .. }` without the extra brackets
- sometimes convert struct types to arrays when large and all same types.
  should do more stress tests for weird generated code, like my swift parser generator problem.

## give up and use llvm (Jun 15/16/17)

- empty block. oh but also same label twice. switched to the block before checking if already did that one.
- they have pair struct literals but only for constants so you have to painfully insertelement to return a pair.
- all the ones where it was just silently crashing was because i forgot to do tailcalls and was just trapping because emitter was supposed to have returned

// :PushConstFnCtx
// make sure things that get called at compile time don't get added to the call-graph.
// this allows aot backends to just blindly walk the call-graph and not emit loads of garbage empty functions,
// for things that returned types or added to overload sets. extra important because they might use Bc::GetCompCtx, which can't be compiled.
// this is special because of check_quick_eval, so we're not always in the context of a new function that will be thrown away after the expression is finished.

- sret needs to be added to the callsite too, not just the declaration, which is fair enough.
  I think it only matters for calling through function pointers.
  clang does it for both tho so playing it safe.
  can see the problem in the debugger becuase it was doing `blr x8`
- dont emit const slice as load of ptr to slice. ~2700 -> ~1200 emitted constant definitions for compiler/first.fr
  that broke please_be_the_same which has a comment "sketch allignment, its fine if you break this test when emitting exes",
  so... thats a win i guess.

## distrust qbe (Jun 15)

I'm not sure i trust qbe, like ok i want to return two ints.

```
type :pair = { l, l }
export function :pair $get_two() {
@start
    %out =l alloc8 16
    %snd =l add %out, 8
    storel 123, %out
    storel 456, %snd
	ret %out
}
```

generates this:

```
.text
.balign 4
.globl _get_two
_get_two:
	stp	x29, x30, [sp, -32]!
	mov	x29, sp
	mov	x1, #8
	add	x0, x29, #16
	add	x1, x0, x1
	add	x2, x29, #16
	mov	x0, #123
	str	x0, [x2]
	mov	x0, #456
	str	x0, [x1]
	mov	x1, #8
	add	x0, x29, #16
	add	x0, x0, x1
	ldr	x1, [x0]
	mov	x2, #0
	add	x0, x29, #16
	add	x0, x0, x2
	ldr	x0, [x0]
	ldp	x29, x30, [sp], 32
	ret
/* end function get_two */
```

and in my language,

```
fn get_two() Ty(i64, i64) #log_asm = (123, 456);
```

with my current garbage asm where i do absolutly no optimisations, you get this

```
=== Asm for Fn2352: get_two ===
	stp	x29, x30, [sp]
	mov	x29, sp
	sub	sp, sp, #0
	mov	x0, #123                        ; =0x7b
	mov	x1, #456                        ; =0x1c8
	mov	sp, x29
	ldp	x29, x30, [sp]
	add	sp, sp, #16
	ret
===
```

and the obvious correct that clang gives for this

```
struct pair { long a; long b;};
struct pair get_pair() {
    return (struct pair) {123, 456};
}
```

is

```
get_pair:                                 // @get_pair
  mov     w0, #123                        // =0x7b
  mov     w1, #456                        // =0x1c8
  ret
```

## (Jun 11/12)

> we're at 15404 loc .rs

- hashmaps said the wrong size, i was treating them as a Vec.
- i was saying align of a struct was the align of the first field, but it should be the max field align.
  which is weird cause you have to pad it anyway if you can't reorder.
- a problem i'd eventually have is rust probably doesn't define padding but i try to use Values as map keys,
  but i don't see how that could be the current problem.
  fixing that might help debug this tho.
- trying to zero_padding on Box<Func> dies somewhere, and that was the sketchy one before so maybe thats interesting.
  but you can do it after serializing so thats the same as the real problem.
  its recursing on structs but not on pointers, so the FatExpr in FuncImpl i guess?
  no, VarType. oh because i have to zero padding on the tag first maybe? no its repr(i64).

oh shit im just not at all typechecking for generics????

```
// We might have compiled the arg when resolving the call so we'd save the type but it just changed because some were baked.
// Symptom of forgetting this was emit_bc passing extra uninit args.
arg_expr.ty = new_arg_type;
```

what the fuck am i talking about....
that lets me call slice_from_tuple that expects a Ty(*T, i64) on something that's a Ty(*T, i64, i64) cause i changed it to real vec.
I don't understand why it doesn't have the constant argument type in there tho.
Oh its a tuple (T, TTT) and then i remvoe one so its (TTT) and that slots to TTT so it just happens to not need const args remove from the type in this case.
Right so for fn alloc it doesn't work.
so the comment is true, but i still need to do the typecheck somewhere!!
so i was swapping length and capacity of vectors if i try to bit cast them instead of going through my serilizer that just uses length for both for now cause i just leak everything.
that wasn't enough to fix the problem tho.

- also ptr_cast_unchecked(From = Type, To = u8, ptr = mem.ptr) instead of ptr_cast_unchecked(From = Element, To = u8, ptr = mem.ptr)

I was saying the type for Option<T> was (i64, T),
so zero_padding always tried to walk the None section if it wasn't present,
fixing that fixed doing zero_padding in box serialize but still not the real problem.

I'm just stupid and assumed vec would be in order of the fields (ptr, cap, len) but ptr is in the middle.

> for reference, `franca run_tests.fr`, just unit tests: 62 ms. before prim_sig change.

Actually, now i don't even need all the flat call shit, I'm just using it for macros because that's what one does apparently.

- stop using bounce_flat_call for COMPILER_FLAT
- update_cc wasn't setting for #ct unless you specified #flat_call or #c_call. which i wasn't doing cause #c_call is the default.
- change a bunch of `(arg, reg): (E, E)` to `arg: E, ret E`

- TODO: that broke exceptional.fr somehow (bus error. maybe just a make_exec thing?) and I haven't actually taken the win yet and removed the flat_call code.
  leaving that for tommorrow. also need to fix the single vs double arg copy-paste, now that it can easily be dynamic with the vec.

flat call served me well but it will not be missed.

## (Jun 10)

debug log says width/height can't be zero.
problem is probably that there are f32s before these fields in the struct,
and im treating those like 8 bytes so the offsets are wrong!
yup, making it pretend to be i32 instead of f64 makes that error go away and it draws text!

TODO: use for bindgen: `cargo +nightly rustdoc -Z unstable-options --output-format json`

## (Jun 8)

- let type_of see through fn ptrs so can do inline .unwrap on vtable calls.
  I think nothing relied on Program::fn_ty denying FnPtrs.

## Build script goes brrr (Jun 7)

- when returning something larger than two registers,
  excplitly make the first arg the out pointer because i don't correctly use x8 yet.
  and need to use ManuallyDrop because you can't have the rust side trying to drop uninitialized memory when it sets the result in there.
- forgot to make it exec!
- compiling driver to c and then dylib works if i hardcode the source but not if i try to read a file.
  - though it might be because i was treating Fd as i64 instead of i32 but that wasn't the problem.
  - confusion with read returning giant number that must be -1 but im not seeing it as `lt` 0,
    because my comparison functions had thier arguments emitted as unsigned ints, for now hack jsut cast them back
    because that's just why the error message was confusing, not the actual problem.
  - oh im stupid. my emitting constants doesn't know that a cstr is a special type of thing,
    so it thinks its just a pointer to one byte so I was trying to open a file called "m" instead of "main.fr",

## (Jun 6)

- just had to remove is_unit check on arg in c::declare because now there's the indirect return address sometimes.

my own setjmp/longjmp was easy... for my asm backend anyway.

for c,  
i can't just write it how i did other functions where it becomes
a `_FnXXX` that calls the libc setjmp because you can't setjmp and then return,
because now you're in a different place on the stack. maybe it would get inlined by luck on higher opt but that seems sketchy.

but i can't make clang on -O2 make it behave in a sane way (works on -O0).
i tried attribute returns_twice and i tried making all the local variables the compiler generates volatile.
also tried modifying the generated code to use libc setjump/longjmp and do it in an if() like `https://en.cppreference.com/w/cpp/utility/program/setjmp` says you have to.
but it still doesn't pass my test.
allocating the counter makes it work tho, with or without returns_twice.
so i guess that does nothing and its just more conservative about aliasing the random pointer
it got from an indirect call into the allocator, than a stack address, which fair enough i guess.
but it kinda makes using that less appealing.

I wanted to use it for the parser where there's only one way at the top to handle an error.
A question mark operator like rust's seems like more effort, since I have a weird concept of returns
so you have to like walk up the stack of blocks to find something of the right type to shortcircuit from and that seems a bit unpredictable, idk.
Could also try the `x <- try(whatever)` thing again, but I feel like that's meh.
cause you don't just want it to eat the rest of the block, it needs to work in nested expressions, and that gets hard to think about in the same way.

## (Jun 3)

my painful bug was about dropping a stack slot after dup-ing it so it got reused for another variable.

> pass by ref, arity, sig

- (parser) let you do loop captures without the extra brackets so its a bit less painful to look at.
- removed TypeInfo::(Scope, OverloadSet, Type), because they're really just unique u32.

:return_u32_pair
need to fix the crippling corruption when passing (u32, u32).
need to not assume \*8 but that's a lot of places (3) to write the annoying loop.
oh but actually, i'm only ever loading/storing 1/2 sized structs because anything else you pass by reference/copybytesttofrom,
so I don't need a whole compilicated logic for nested structs, I just need those cases to work.
using correct-ish offset instead of always 8 did quickly fix return_u32_pair but now i have to debug the other 1/4 test it broke :(
nvm, just an ordering problem, that was easy, and that fixed codemap too.
and it works on c too, so that's a good start towards using that as small step to selfhosting.

## (Jun 2)

indirect return address for large structs instead of using flat_call.

- bc_to_asm setup and dyn_c_call have a hacky +1 to arg_count to include that ret ptr.

## (Jun 1)

- I think now that i store correct sizes, i can't have my loose int casting,
  because just putting a u8 in an i64 variable stores 1 then reads 8 and my asm backend doesn't zero the stack.
  it used to work because i stored the whole register, and it works on cranelift because
  store get type info from the value not my bc instruction and i zero extend all ints from before i was tracking primitive types.
  calling zext on the digit byte fixes parsing_hex, parsing_float, and escape_characters.
  bf_interp was same problem.
  the aarch64_jit problem is also clearly the same but hard to see where cause its in an ffi macro.

## (May 31)

```
RUSTFLAGS="--emit=llvm-bc" cargo build --release -Z build-std=panic_abort,std --target aarch64-apple-darwin --no-default-features
```

If you don't have `panic_abort` you get `duplicate lang item in crate core: sized.`...
idk bro https://github.com/rust-lang/wg-cargo-std-aware/issues/56

Confusing dereference garbage pointer in c when calling alloc. just forgot to pop the fn ptr after a call.

Ok this whole backend not knowing about small primitive types thing is getting a bit dumb.

- spent ten thousand years on slice ptr-len wrong direction and then incrementing before re-pushing the pointer
- interestingly cranelift was easier to make work than my asm now.
  other than it can't arm... but x86 was fine.
- redirect function caused storing the result with the wrong type so unaligned (found in codemap with ubsan),
  (ex. add u32 redirects to i64 so it things it needs to store 8 bytes).
  but that didn't fix my problem.
- the problem with `Expected (18446744073709551615 == 255)` in basic.fr,
  was loading a byte as a (char) and then casting that to a (void\*),
  char is signed so it reads -1 and then tries to preserve that so it sign extends and you get the highest number.
  ok im kinda sold on zig's billion different names for casts now, should probably do that in my language too now that i'm getting close to non-painful generics.

## (May 30)

I think i want explicit allocators.
Can have a comptime one to use whatever arena the compiler wants ast nodes in.
Also nice for the lox example program, can have a gc allocator and still use the standard containers.
need to be able to do flat call through function pointer if i want it to return a slice,
but other than that it works for all the lib collections.

just tracking cc on fnptr was tedious but now i need to actually use it to make flat call in the backends.

fixed confusing crash that only happened on lox demo (my asm backend. cranelift's still fucked).
In bounce_flat_call!::bounce, needed to set the jitted page to executable.
Since we just called into a compiler function, it might have emitted new code and left it in write mode...
but our caller might be jitted code (like a macro), so before returning to them, we need to make sure they're executable.
this avoids a `exc_bad_access (code=2, <some code address>)`.
The problem only happened in the lox demo, not my small tests, so its fairly rare.
Need to remember for future that the address it shows is the address of memory you tried to access (not the ip when you tried to access it),
so if there's disassemblable code there, it muse be a jitting permissions/caching problem.
The frames might be wrong in debugger because it hasn't gotten to the part where it sets the frame pointer? idk.

- for emitting c, im trusting the callees list.
  problem with VERBOSE_ASSERT where list/displace weren't being seen as callees of anyone.
  needed to add callees in emit_capturing_call because for things with const args, the body will already have been compiled.
- needed to use flat_tuple_types instead of tuple_types for c fn ptr type because want to see struct fields as seperate args.
- stack buffer overflow because i was making vars that were right byte size (like 1 for a bool) and then writing to them as a u64 i think.
- something emitting a comptime pointer as a pushconstant is getting through.
  i thought it was some subtle thing about threading the right exec time through but actually
  i was emitting bool constant values as a memcpy because the backend isn't supposed to know how to load a single byte.
  cry.

## improving inference (May 29)

made it so overloading checks one argument at a time,
so it can decide which loop iter function you're calling based on just the collection,
and then infer the types for the closure, so you dont have to write the element types a billion times.
and then in promote_closure, slot in the types if they're missing, like i did in @match a while a go.
also had to carry through the requested type to each tuple element so it gets passed from the resolved function arg
all the way to compiling the closure.

gonna replace !while with !loop and implement fn while with early returns.
eventually want it to just be expressed as tail recursion but this is a good start.
it really doesn't like me writing while with a loop tho, `Missing value result.inlined_return_addr.get(&return_from)`.
Oh its because of my reducing Block with empty stmts to just the result expr and Block is the only place I hang a return label from.
So just need to check if that's been defined (which is only done by emit_captuing_call).

trying to emit shitty c.
its a problem that there's #ct functions that try to get called at "runtime".
like assert_eq-ing on a tag_value doesn't get folded.
same for some basic.fr assertions on Ty(...), i started fixing those manually before i noticed it was so common.
its because expr.as_const doesn't check tuples.

## remove #comptime (May 28)

another attempt at removing #comptime because const args should do the same thing.
const_bound_memo ends up being the same as generics_memo, if all args are const
so it maps to a function with no args that just returns a value.
tho be be exactly the same you probably need to :: at the call site.
but its mainly being used for functions that add to overload sets and return types so they're called in a const context anyway.
I'm hoping this lets me get rid of the hundred line emit_comptime_call function.
tho it might not because #generic needs to delay knowing its return type.

made const_args_key imm_eval the expression if there's only one and its not already values.
that was enough to make fn List work without #comptime. and the rest with only one argument.
same thing for macro eval. same for multi-arg case in const_args_key.
so now its only needed with #generic.
damn that was actually easy. all it needed was an extra infer_types in curry_const_args.
well that was good for morale.

i wanted to make a quick c backend, so i made a bucket array because it seemed convient to refer to types as a Str,
but then its a pain to write to because everything is hardcoded List(u8).
so maybe its time for traits (tho i guess it wouldn't even help this situation cause i need to pointer back but one step at a time).
I think I'll try to rip off Jai's $T pattern matching polymorphism stuff.
The current thing where the easy to use functions are the ones inside the generic function that creates the type is really annoying.

start with having #generic work like zig where tou can have later argument types depend on const args.
ok so the problem there is you can't renumber in the clone, because you haven't resolved the body yet,
so it still references the un-renumbred scope.
so i need to like insert a new scope in between where the arguments can go?

dumb mistake with checking flags on a mem::take-en func.

## (May 25/26/27)

now that im doing alignment shit, i dont really want to duplicate it for InterpSend::SIZE_BYTES so it just reads it from the program,
which is dumb and slower but the eventual goal is to use same layout as the rust structs so the whole thing goes away.

- was off by 8 redundantly skipping tag for de/ser Option.
- operator_index got garbage value for expr tag. needed to do correct alignment offsetting for NameFlatCallArg because (FatExpr, FatExpr) has padding.
- ok cranelift now works for 80/128 tests (with verbose_assert=false).
  but my asm says 0 for `debug_log_int(if(0.lt(4), => 1, => 0))`.
  problem was cmp_with_cond being read as (i64, i64, i64) when asm was trying to return (u32, u32, u32).
  struct coerceion now checks that byte offsets match. really the whole thing needs to go away eventually. TODO: at least make sure to finish_layout
  now my asm is same as cl.
- in deconstruct_values i wrote bit sizes instead of byte sizes :(
- `name: Symbol = part.expr.GetNamed;` doesn't work because it needs the special u32 load fn.
  but I can't easily refer to the type GetNamed.
- `missing value program.get_enum(enum_ty).\nType is not enum. (tried tag_value of GetVar)`.
  because i typo-ed when i redid builtin_const
- had to fix load_u64/store_u64 for my asm backend with non multiple of 8 offset.
- when emit_bc for ::Values, values.len != ty.stride. I think because of @as for ints doesn't actually do anything.
  for now just copy the right number of bytes but should really handle it in ::Cast somehow.
  now i fail 11, cl fails 3.
- @enum(a, b, c) has indexes (0, 4294967296, 8589934592). off by 4 bytes.
  im stupid and did align_to(offset, info.align_bytes as usize); without asigning it back to offset.
- all the rest of my asm failures are happening in @fmt for floats.
  problem with adding fn int as callee when you call it inside the macro?
  cause it works if you call it first and then also in the macro. but idk why it would work on cranelift then.
  for now just do the easy hack and TODO: actually fix the problem.
- i just want to get everything kinda working so i can make it less painfully slow!
  the new InterpFfi has to call get_info like a billion times and its ~47x slower than before i started using bytes.
  tho get_info is only 64% of samples, im hoping it just gets inlined sometimes.
- last problem is cranelift jumping into a bunch of zeros for bf_interp.
  - it happens during compilation, before running main.
    i can #log_ir of run tho, so it gets that far, it can't compile main.
  - it seems it can't compile the assert_eq on bools (if i ::println on either side of it, the second doesn't go off).
    but it can do it in other tests... so what gives??
  - and deleting random cases from the @switch also makes it compile....? this is kinda scary.
    like it works if i remove Bf.Open or Bf.Close...
    but it always compiles fn run anyway, thats somehow affecting its ability to compile the assert_eq in fn main!!!
  - it works on x86 so i guess its either an alignment thing or a instruction caching thing.
    i tried always aligning to 8 in my GlobalAlloc since i do flat_call arg/ret with Vec<u8>
    which might not be correctly aligned but that didn't help.
  - maybe its like about stack alignment? cause deleting an unused local variable makes it compile.
    tho asking for opt_level=speed doesn't fix it and i'd hope that would remove the unused things.
  - its getting to call main but it fails before doing the print.
    !! but not if i call debug_log_int. now it actively can't call println?
    can't call print either so its not the stack address thing for the new line.
    but it does work if i inline print. so its a problem with passing a string as an argument??
    it can call debug_log_str.
    but it can also call `fn pls(s: Str) Unit = debug_log_str(s);`
    can also call a copy pasted version of print... so this is all just useless and its some crazy layout thing??
  - something to do with string escapes maybe?
    but those work everywhere else.
  - like i dont have high faith that its not a cranelift bug.
    maybe we're back to just doing everything myself because its harder to debug other people's stuff.
- most of the slowdown in calling get_info a lot was because i was checking cache on ty but saving on raw_ty
- adding real u16. trying to deconstruct_values a @struct(\_0: u1, \_1: u5, \_2: u16, \_3: u2) from 26 bytes instead of 32.
  something's not emitting padding for the u16.
  ah, its `for v in values? { pls.extend(v.0); // TODO: alignment }` in quick_eval ::Tuple.

## (May 24)

- ok so now i never match against Values::One, so i can take it away, just do everything with Vec<i64>, then do everything
  with Vec<u8>, and then add back the by value varient when its small as an optimisation. thats the plan.
  got to everything being Vec<i64>, its like 15% slower than last i checked, thats fine for now.
- problem where it didn't compile f in create_slice_type if i set the type to FuncId instead of Fn(Type, Type).
  fixed by calling compile instead of ensure_compiled in emit_runtime_call and switching some checks from matches TypeInfo::Fn to also allow FuncId::get_type.
  not sure if thats the best choice. or if i should just have seperate as_literal for functions that checks the type...
  but you want to allow for ones that haven't typechecked yet.

## (May 23)

- compiling the function before folding a call to it because those didn't go in anyones callee list.
  and to avoid that recursing, it marked the expression as done so it wouldn't get added as a callee of the lit_fn,
  so it would go in pending_indirect list and get compiled at the last minute.
  dont have to worry about the arg trying to call something that's not ready because you only fold if its already a const value.

## (May 22)

- got rid of Value::(Type, Label, Unit, OverloadSet)
- got rid of some implicit casts
  becuase they get in the way of just representing all values as bytes.
  probably want to bring them back eventually but in a more explicit way where you just declare overloads for implicit_cast maybe.
  - tuple of types to type so you have to call Ty all the time.
  - overload set to Fn, now you use @resolve (only needed to change 3 tests + `__save_slice_t`).

## (May 21)

I want to be able to make terminal ui things but that kinda needs correctly sized array of bytes as a struct fields to call the terminos functions.
Its 20 bytes so i can't even hack it with bitshifting cause i don't have u32 fields either.

- removed ability to .(index) on structs, how that i have seperate PtrOffset ast node.
- made value arrays. should have them more interchangeable with tuples in the language.
  but the compiler wants to see them differently so `[u8; 4096]` isn't storing 4k TypeIds in a vec.
  also arrays let you use runtime known indexes cause they're all the same type.

## (May 20)

- have @match fill in argument types since it knows from the varient name
- want to add escape chars in strings by just having the parser emit calls to a comptime function.
  but to pass that to .char(), which you often want to do, need to save a way of saying that function should be constant folded.
- started tracking which constants are pointers so the backend can handle relocations when not just jitting.
- trying to output an executable with cranelift-object.
  the thing it gives you is a .o file and then i run clang on it to link, but i get bus error trying to call a Linkage::Import libc function.
  i tried having a function called main and i tried having a c program call one of my functions and giving that porgram + my obj to clang.
  no luck.

## (May 19)

- use bit set for func flags. make some other structs smaller
- got rid of ::F64, ::Heap. ::Bool, and ::Symbol Value varients, because the compiler doesn't care about them other than type.

## (May 18)

- want to clean up type checking / coercion stuff.
  so I want @as to be the only one that does the current loose `type_check_arg` where you can assign structs to tuples.
- @as can't just change the type on the node because `c: i64 = 0; d := @as(u8) c`,
  makes emit_bc debug check see that as a u8 load without calling fn load.
  it does work if you take that out but i don't really want to remove that check.
  but doing it on the pointer type instead of the value type makes sense to me.
- made it give the compiler access to fn Slice(T) so strings and !slice can have the right type less painfully.
- made bool take one byte. interestingly that made @bits break in emit_bc debug check
  because it was using !if instead of fn if, so the cond didn't go through the normal deref thingy so didn't get converted to a load call.
  thats pretty janky.
- i feel like its important that eq/ne/load/store know that they can call the same function for most unique types of i64 or whatever.
  so i can use types for enum tags and just forward those functions to the int versions but not say you can call fn add on an enum tag.

## zero size unit (May 17)

- in the backend you mostly need size and float_mask at the same time so put them together so its less verbose.
- want zero sized Unit so you don't spend instructions moving around garbage.
  removed places that pushed/popped a zero to Vec<i64>/v-stack.
  had to check for it in c::call because you can't just read garbage if the vec doesn't have an allocation.
  special case in cranelift signetures / intrinsics.
  last that fail are `match_macro` and `tagged_unions` trying to load a zero sized thing.
  strangly adding a check for zero size in emit_bc Deref fixes those but breaks `test_string_pool`.
  but why am i even getting there? i gave Unit a special load/store that do nothing.
  so enums are adding loads without checking overloads.
  ah, it happens if i have a Unit struct field too.
  so place exprs like that don't go through the compile deref that checks the overload.
  nice i caught that now cause it would be a problem when i tried to make small types take the right amount of space in structs.
  so thats mostly easy to fix by just not setting `.done` in compile_place_expr.
  but @tagged are still wierd because if you load the whole thing that includes padding so you have to get that off the stack before treating it as a smaller varient.
  but no, thats not true, you always access specific fields on it from memory and only load the whole enum type if you're going to store the whole enum type.

TODO: it should know Unit load/store does nothing so my backend doesn't save registers before doing the garbage call!

## compiling the compiler for blink

https://betterprogramming.pub/cross-compiling-rust-from-mac-to-linux-7fad5a454ab1

in `.cargo/config.toml`

```
[target.x86_64-unknown-linux-musl]
linker = "x86_64-linux-musl-gcc"
```

```
rustup target add x86_64-unknown-linux-musl
brew install FiloSottile/musl-cross/musl-cross
cargo build --target x86_64-unknown-linux-musl
```

ok that worked on my mandelbrot demo.

so now i have to write the c_call shim for x86.
i don't understand why it can't find my function!
is it because of changing whos doing the linking?
it works if i make it a `#[naked]` function with an asm block, but not if i make it a global_asm function...
thats so strange. its in the same module which was the problem last time.

- https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI
- https://stackoverflow.com/questions/18024672/what-registers-are-preserved-through-a-linux-x86-64-function-call
- https://en.wikipedia.org/wiki/X86_assembly_language
- https://en.wikipedia.org/wiki/X86_instruction_listings

- i have to restore the stack pointer after pushed args, callee just reads them.
- cranelift: can't resolve libcall memcpy. fair, there's no dynamic libc for it.
  so how do i tell it the fn ptr to use.
  JitBuilder::symbol_lookup_fn lets me just switch on the name and give it a pointer to my staticlly linked memcpy, cool.
- `debug_assert_eq!(addr as usize % 4, 0);` x86 has variable length encoding so does't force an alignment i guess.
- hitting a SEGSEV somewhere. i wonder if its something aarch64 inline asm that i forgot.
  yep. skipped any #aarch64 functions and now it segsevs with rip=0x1337.
  ah ptr_to_int, int_to_ptr and fn int(f64) i64.
  that wasnt the problem but still good to have fixed.
- turning off VERBOSE_ASSERT helps for now.
- `not compiled Fn286 gt`. oh for value of a constant the compiler needs to call it so need to emit the cl intrinsics as thier own functions.
  but just try a different example for now.
- its definitly trying to jump to a missing function, the address changes if I change Jitted:EMPTY.
  but wheres it getting it from??.
  even without fixing that, passing 79/112.
  tried writing the func id in the empty slot.
  for generics it seems that its trying to call `$eval_0$Fn%73Cs1b3([Unit%3Cs1b3, Unit%3Cs1b3])$` through the dispatch table.
  fuck no im an idiot, thats putting the f that extend_blanks is going up to, its actually trying to call offset,
  which makes sense that doesn't exist cause its one of the intrinsics i just skipped. sigh.
  AAAAA, i was so confused about how it was getting the ptr because i thought i took out the thing
  where it uses the dispatch table when you try to GetNativeFnPtr and it wasnt even doing that bc inst....
  but i was still embeding the dispatch ptr when trying to CallDirect to something that wasnt ready yet,
  hoping it would get filled in later, so thats why it wasnt trigging the check in get_fn.
  so problem was my mutual recursion hack meant i couldn't catch the error of trying to.
  but even with emit_cl_intrinsic, it doesn't get there.
  maybe its like its added to callees of something that doesn't end up having compile called on it before getting popped off the stack,
  and it works on aarch because the merge causes special_body to be evaluated directly?
  DAMN IT. i typo-ed the return type of offset as i64.
  ok so i should really consider reworking my overloading system to catch that sort of thing.
  at the very least typecheck my hard coded string signetures somehow. fuck.
- well anyway now all that fails is floats.fr (only comptime stuff cause mandelbrot works)
  and libc.fr/open_dynamic_library (which wont work cause im running it in blink which doesn't have dynamic libraries).

## finishing cranelift backend (May 16)

- floats on cranelift. im being dumb and not tracking types so doing a bunch of bit casts.
- have to be able to call comp_ctx functions on cranelift for macros.
  might as well just make that a normal part of the bytecode instead of special thing in the backends.
  for now just made an inst to push the ctx, but it still needs to know on the call inst to add an extra argument,
  so it didn't really make it simpler. would be good if i just actaully put that info in the function type.
- dont put a return val for sig when its Never.
- forgetting to set asm_done breaks comptime_float_call which is weird.
  but you want it in a more central place so you never forget anyway so not a huge deal?
  creepy tho. why does trying to compile the same thing multiple times make you try to call something with a different calling convention.
- when returning an if expr, it does a jump with block args, so needed to tell cranelift about them.
  thought it would be annoying because types but since i cast floats for ::ret already, i can just use ints everywhere.
  that seems to have fixed pack_i64 too.
- can't try to do a tail call from cranelift to a rust c abi function.
  and can't use cranelift tail abi if its something the compiler wants to call.
  for now i guess just giveup on that and try again later when the normal stuff works.
- need to forward declare func ids for recursion.
- thats enough for 100/116 to pass on cranelift (comptime+runtime)
- next problem is entry block args being floats instead of ints because i stopped using append_block_params_for_function_params.
  cause i want them to be ints so i dont have to deal with it but then it doesn't match signeture which it rightly complains about.
  doing that and then casting from float fixed another 6.
- mandelbrot floats mask was wrong. lol `// TODO: HACK: wrong!!!!!!! size_of. -- May 3`.
  when making the mask, i was always shifting by one for each tuple element even if it took up more slots.
  and it mostly didn't matter cause the only one where i passed a struct of floats was mandelbrot and that didn't do
  try to pass it to the compiler so it didn't matter that i wasn't using the float registers, cause both sides made the same mistake.
- the 2 mutual recursion tests need forward declare. on asm i just look it up from the dispatch table but cranelift has fancy relocation stuff so why not use it.
  but im lazy and just want to see it work first so using table for now.
- need to say right number of block params for @tagged, even tho they don't have Some(tuple_types).
- emitting an unreachable and abandoning that block after you call something that returns Never.
- fixed bc emitting StorePre of size 0 when assigning to a var from an if with a Never branch.
- thats everything working except backtrace.
- unwind on cranelift gets messed up by it doing pointer integrety things? pacibsp and retabsp.
  so when it saves the return address its infact some different thing you can trade for the return address?
  https://devblogs.microsoft.com/oldnewthing/20220819-00/?p=107020

## small types on cranelift (May 15)

- got rid of FnWip. cause i wasnt using it right with constexprs anyway.
  now just tracking var types in the global thing.
  the other thing they did was tracking callees so you could try to emit them first.
- trying to make small_types test work on cranelift.
  revealed problem of using normal loads for implicit access to vars that should be u8,
  so then really you should zero the stack slot at the beginning which i wasn't doing so i guess it worked by luck.
  new system is emit_bc never sees a GetVar, the frontend inserts `&[]` every time so it goes through the normal logic
  that might replace that with the intrinsic load/store.
  but now there's the u8/i64 types problem, so i guess i have to say zero extend on every load.
  oh they have a special uload8 that does that.
- next problem is the reverse, in my test, the store has to store 1 byte
  but then it has to read back the whole word without assuming its the same as it just stored.
  so it needs to see the value being stored as a u8 even if it started as an i64 (like a constant).
  so truncate the value first. that works.
- perhaps this shows that my bytecode should keep better track of things.
  cause it did know which was float/i64/u8/ptr/unit and im just painfully reconstructing what i threw away.
  presumably you want to help it be fast by giving it fewer redundant bit size casts to optimise out.
- not following the calling convention with x21 is creepy.
  and i was already reloading it every time cause i didn't trust, so just stop using it at all.
  had to adjust unwind.fr to use a new builtin to the compiler to get the dispatch table.
- was a pain dealing with R64 vs I64 in cranelift, but it seems i can just make everything I64 and dereference it just fine,
  so i don't understand why R64 exists.
- for using results of comparison as values, had to uextend to i64. eventually should just be a byte (or bit??)

## stop committing generated stuff & do u8 strings (May 14)

If I can't have a self-hosted compiler yet, I should at least get the benifit of being able to build from source directly.
Rn I generate instruction encoding stuff so there's a sad amount of code there that i barely use.

- cleaning up the rust side asm encoding stuff is easy.
- for the rest, start by defining a version of @bits in rust.
- fuck im trying to pass it a slice on the stack but that call is the last in the function so it tries to do a tail call.
  and the tailcall resets the stack so the rust function puts its local variables overlapping the slice i tried to pass as an argument.
  so i guess i have to say no tail calls if any of the args are pointers?
  damn lifetimes are looking like a pretty good idea all of a sudden.
  suppose its not that big of a deal cause the main thing i want them for is closure loops where the lambdas have no arguments anyway.
  should make the calling convention thing a more useful struct if im gonna be having this many combinations.
- then need to make !asm not force a slice. instead eval each int of the tuple seperately and build the vec on rust side.
  if the expr isn't a tuple, you can still return a slice like before so the language hasn't lost cool jit power there.
  had to get rid of u32 as inst return type cause it just makes everything harder cause ffi u32 things its just an i64.
  bring that back when i actually do smaller types as a first class thing.
- then can remove the stupid hack with allocating a list for the asm so it looks much neater.
- so that gets the arith but cmp still needs to call a fucntion so has to return a list.
  cant just compile first so inline fns work because then have to thread a FnWip through which is annoying.
  and even then, inlining doesn't project the args so i couldn't just poke through the ast like im doing now, would have to actually call it.

FIXED:

```
//! If you change anything here, you have to `./bootstrap.sh` again.
// TODO: some of the argument names matter because they're hardcoded in emit_rs
```

I wonder if --64fps demo would be less flashy if i did u8 properly instead of printing x8 as many nulls.
Maybe start with still spending 8 bytes of stack/struct on everything and just intercepting load/store
with calls to the inline asm functions that use the short instructions.

- so now i hackily desugar `u8_ptr[]` to `load(u8_ptr)` and `u8_ptr[] = val` to `store(u8_ptr, val)`.
  which seems to work. its doing a sad slow overload set find every time it sees one of those expression tho.
- painful debugging of requested not being unpointer-ed. really need to make my error message better.
- ok trying to make Str be Slice(u8).
  after adding some casts, only ~8 tests fail. eq is sometimes reading them like its a i64 still.
  made ints interpsend know thier bit count so string literals can be the right type.
  that made it worse.
- it gets weird with string being InterpSend cause that expects everything to be serializable,
  as a vec of ints which is odd when reading from a ptr to u8 with the wrong len.
  so hacky answer for now is i dont actually need strings to be interp send, ill just treat them as (\*mut u8, i64).
- oh ok that just worked. i thought it was broken cause 2 eq tests were crashing on oob
  but it was just my debug print that i did before the length check in the loop.
- so now the state is that slices/lists/ptrs/sizeof correctly treat u8 as bytes.
  however, they still take 8 bytes as struct fields/stack slots.
  does that mean size_of is wrong for structs containing 8 bytes? yikes.
  oh, no it doesnt... becuase size_of specially lies for u8 and uniques of it, otherwise it still does slot count times 8, so thats ok for now.

## more place exprs (May 13)

- using 32 bit indices everywhere saves 9MB (15%)
- made backends only get program and asm instead of whole compile struct so it feels a bit less messy.
- i dont actually use most of libffi, i dont need structs abi since my backend can't do it and i only need aarch64 for now.
  so removed it and replaced with tiny asm shim that just loads ints from an array into registers and calls a function.
  libffi is ~45k lines of asm/c, mine is 13 lines... i just have very simple requirements compared to what its trying to give me.
- interestingly that also fixed comptime print. so i guess i was lying to them about struct abi flattening somehow?
- discovered that when you do global_asm! and extern c to your own function, it must be in the same rust module as calls it?
  if you try to import it the linker spews a billion lines of errors.
- made `.(i)` and `[i]` work like other place expressions.
  had to add a new PtrOffset ast node that they desugar to so you can tell the tuple accesses
  in user code apart from pointer ops that everything desugars to.
- auto deref in chained place expressions (like rust).
  you still need to manually dereference for function calls (which is kinda inconsistant, might change that),
  and if the type of the first is a pointer and you want the value, like it doesnt just auto deref on assignment,
  its just a magic part of the dot operator. which is how rust does it, and feels kinda nice,
  cause you never meant to access a field on the pointer so it always guesses right.
- im suspisious that it wasn't inlining InterpSend::size cause i saw it in the profiler so i made it an associated constant.
  that seems to have made less serialize_to_ints show up but that seems crazy? like its always through a generic (its not even object safe), why wouldn't it get inlined?

## place exprs (May 12)

- made inserting #include_std stmts a bit less dumb.
- change @namespace to take a closure instead of a block so every block doesn't need to remember its scope.
  lookup field access in constants of that scope, dont care about which block for now, but eventually need to so you can do private/shadowing.
  taking off the scope tracking field from Expr::Block saves 24 bytes on every ast node.
  saves ~7MB on tests so I have 284K nodes, yikes. Stats::ast_expr_nodes_all says 34k... oh but it doesn't count clones.
- made TypeId::scope just be one int, removed local_constants and some old fields of Func
- made Enum cases a real type instead of weird contextual_fields thing in the sky.
  prep for actually checking that casts are valid values.
- replaced `tuple[index]` with `tuple.(index)` which frees up the brackets for something an overloadable operator for containers.
  the tuples one looks dumber but you'll probably never want that once i can pattern match on them properly in declarations.
  using just .index means you can't use an arbirary expr and you have to deal with it when lexing floats if you want to chain them.

// :PlaceExpr
I want less painful place exprs.

- currently `n&[] = 456` and `n = 456` generate the same code (as they should),
  but raw var access has wierd special cases as tho it needs to specially optimise out taking the pointer to it (which it then does it in the end anyway).
  so give set_deref the special power of evaling place exprs. the easy case is wrap <var> in <var&[]>.
- then assinging to a field access expr (not a []), doesn't conflict, so handle that recursively inserting &,
  so that avoids writing `a&.b[] = c` all the time instead of `a.b = c`. but doesn't help with more complex nesting or even jsut reads of the value yet.
- now !addr means eval that as a place expr but remove the deref. that still needs a special case for var!addr because there's no desugaring you can do for that.
  so `var&.field` and `var.field&` produce the same value. which might be the last unambigous change i can make on the path to being normal.
- changed to more c like syntax where `var.field` is a load and `var.field&` gets the address.
  also i think `ptr[].field&` is really dumb because you have to pretend to load the whole struct but you don't actually.

  now its much slower cause i do a bunch of intermediate addr/deref chasing.
  holy shit im stupid, no, nothing matters, i just left on my log and leak everything on every recrusive call to compile.

## cranelift (May 11)

I want x86 but I don't want to go back to the nightmares factory.

- cranelift maybe doesn't inline things cause they expect you to already have optimised wasm?
  so my llvm strat of i write the basic functions in text ir and parse that into my module seems meh.
  instead just have a few magic callbacks in the compiler that get access to an instruction builder.
- my old thing of SplitFunc for different targets is kinda confusing and annoying to deal with.
  what if instead i merge them so theres only one func for iadd or whatever, and you just pick the impl depending on the target you're emitting for.
- now with merge it means it tries to load the llvm ir funcs wven tho not using and i havent loaded them since i changed to inline asm needs to say #c_call,
  and that was unacceptibly painful to figure out cause my error messages are useless cause they get swollowed by type_of,
  so by the time it bubbles up, it doesn't have the right loc on it anymore. fuck.
  also slower! but the old thing was so dumb. TODO: this one's doing redundant work somewhere, it merges the same thing twice.

- it seems you can declare a symbol on the jit builder but not add imported functions later? which i want to for comptime_addr.
  I guess i could to them up front but like... might as well just do it through a function pointer.
- iconst never an R64 even if you're going to use it as a pointer
- it gets less far if i try to return_call_indirect tail call comptime_addr??
- the way flat_call works rn is dumb. in the prelude, it loads the args to v-stack and stores them in its own stackframe in var slots,
  instead of just treating that space as its variables. so you have to do that whole copy even if you only use one part,
  and my backend does the spilling in a deranged way where it puts them in the wrong place when it runs out of registers, cause it can't look ahead, and recopies later.
  Using that space as vars is sad because then you have to save the pointer, which is why the normal c abi is better, they have the caller just copy it to below the stack pointer.
  but I i dont want to change too much at once. so just gonna add memory reference variables to bc,
  and then it becomes a bit less painful to support on the new backend.
- FIXED: `// TODO: why can't i do this ? feels like this has gotta be a lurking bug -- May 8`
  the "x1 is not free" thing from a while ago, that happens when you remove a reg from the stack after assigned for arg:
  was because i wasnt dropping the old reg after the move.

cranelift has much better error messages than llvm (in that it doesn't just segfault if you make a mistake),
and is like 2MB extra instead of 60MB extra compiled binary.

as a side quest i tried building my mandelbrot with `--target x86_64-unknown-linux-gnu` (yes I `rustup target add x86_64-unknown-linux-gnu`),
to try blinknlights. it spewed errors. internet thinks maybe i need to install gcc.
zig `zig build-exe tail_call.zig -target x86_64-linux-gnu` just worked.
My cross compiling goals give me extra insentive to get rid of libffi so i dont need to figure out how to get thier massive pile of c to compile.

- removed some #bs that werent actually called by the rust code
- removed most of my old spammy logging. i never looked at it anyway so it was just kinda distracting to have in the middle of the code everywhere.
  really what i want instead of that is a repl-ish thing where you can have an error and then be able to look around at the compiler's internals.
  the just dumping out a bunch of files is not super useful.
  anyway I've made constants less confusing since then and now what I have truble with is the asm stuf which has a different logging system.
- finally removed last traces of my Any type from the interp.

## simple tail recursion (May 10)

- fixed next_label starting at from_raw(0) instead of from_index(0) so serialization works
- cache specilization of const args.
- did simple tail calls. needed to add hack to backtrace test so all the frames would show up.
- saved the start of the function at the beginning instead of the end so direct tail recursion works better

# simplify FnBody/cc/returns/constants (May 9)

- using Vec::with_capacity in a few places
- fixed do_flat_call_values where I was doing Vec(i64) -> Vec(Value) -> Vec(i64)
- emit asm right after bc always so don't have to do callee loop in asm.
  then it turns out you never look at any function's FnBody other than the one you're currently doing, so there's no point in saving them all.
  very nice to replace every `self.compile.ready[self.f].as_ref().unwrap()` with `self.body`.
  only .7MB but feels less complicated now.
- track exactly one calling convention on a function instead of having to reinterpret the flags every time

(start plan)
Ok my FnWip result thing doesn't make any sense. It currently just holds var types (tho eventually early return targets too??)
and its tied to functions but really there should be multiple contexts like that in a function.
Like all the comptime expressions kinda take place in a different universe.
Which is how my current make_lit_fn works but I try to skip that as much as possible because it seems dumb to make a whole function just to lookup a single constant or whatever.
And my current way of threading it through macros is a hacky unsafe thing.
You need to be able to have them inherit like scopes for my @type thing to work intuitivly.
Maybe the same thing as const values makes sense where you just do it by var in a hashmap in the sky and if you are able to refer to something you must be allowed to have it.
I think its really important that it always work if it looks like it should work lexically.
The thing where most languages closures don't capture the control flow context of thier declaration is the extream version of this problem i feel.
So for vars i think the easy choice is that global thing but how to handle the stack of lambdas you might be trying to return from.
Like you want it labeled by name but you might have nested of the same funcid and be trying to refer to the outer one,
and it would look reasonable because maybe the closer one is inside another lambda so you can't see it.
The current thing where you just hope the inner one is right will be super confusing.
So you need like a parallel system to the var scoping... but with exactly the same rules kinda.
Cause again, having it thread through a result thing means you have to recreate the path you should have had from scope.rs anyway.
Easy way i guess would be force you to declare labels on the lambda you're trying to return from.
Kotlin also lets you use the name of the function you're passing the lambda to to qualify the return.
Have to just start with the easy thing to prove it works even tho the syntax is ass.
I guess even easier is the original idea of only unqualified !return and you just assign to a variable,
but then you need to track what block you're in all the time. When really you want '!return' to just act like a variable.
could declare a fake `__return` in scope.rs? maybe that's the dumbest option (affectionate).
(end plan)

So problem with passing it into a closure (like trying to implement break for fn while),
renumber doesn't do constants cause they're like in the sky hashmap already.
but you can't just make it runtime because emit_bc needs to be const or it doesn't know where to jump to.
and i forgot closure args can't be const yet because of that same renumbering thing.

That's a very related problem to constants in macros and I fixed that my delaying when constants get hoisted out of blocks into the global map.
Quite pleasing because the old system had weird logic to pass information between scope and compiler but now you just do things in one place kinda.
Feels like it would mean compiling more copies of the same constants but that's what allows them to work intuitively.
You really want each expansion of the macro to have its own version of the constants becuase its reasonable to have different types for them like how it works for generics.

- removed no const arg check on closures and added renumber in bind_const only if its capturing. redundant but its fine fore now.
- had to fix emit_bc to allow nonlocal return to discard-ed function and then use 0 arg jump. cause while is always discard i guess?
  thats all it took for fancy_while to work.

TODO: really you dont care about closures being inlined, you just care about them not having thier own stack frame and not escaping thier declaration frame.
so represent them as labels too and then you don't have to be paranoid about calling the same one multiple times bloating your code.
Then you can allow tail recursive closures and the you don't need !while as a language builtin anymore.

## syntax tweaks (May 8)

- Made '=>' instead of '=' in function declaration mean capturing.
  So then I want to use '=> body' instead of 'fn = body' for passing a lambda as an argument.
  Also it makes @switch look more like rust.
  The problem is it means you can't have function names be part of the value which I wanted to use for targeting nonlocal returns.
  But currently its weird that functions aren't just values bound to variables,
  and that if you use that syntax it decides they're a closure so it will parse through them unlike the statement ones.
- Made name(arg = value) mean named arguments (instad of arg:value) so it looks like default argument values.
  Which also means you can tell a decl and a call apart without the fn keyword. is that a good idea?
- Allow #markers before the equals sign of the function instead of only above the decl, cause you want to allow them on values too.
  So you can say the calling convention of closures, etc.
- changed struct literal syntax to be equals signs so its like named args / vars.
  now everywhere in the language, : means type and = means value.
  except functions i suppose. maybe I should use -> instead of =.

Fix remaning tests:

- fixed early_returns. was using the result_location of the ret call instead of the thing you're returning to.
  TODO: have it work when its not pushstack
- now asserting has c_call marker if you do that so have to add those earlier in compiler.
  before i just figured it out in asm based on sizes. but now i need to change stuff in emit_bc based on it too, feels cleaner to decide in one place.
  inline asm now needs you to say #c_call.
- now only mmap is failing.

Did default values for struct fields.
Involved some hacky threading result around but really it shouldn't need the result since that just holds var types and default values are always constexpr.
Vars know thier scope anyway, could just track types there too.
But it feels a bit cringe to have every tiny thing know whats going on?

## early returns / rls bc again (May 7)

Doing early returns by having labels as a comptime value.

- keep track of number of incoming jumps so if its only one, you can just fall through,
  without spilling stack, cause you know you dont need to match value positions with anyone else.
- if a return_block only has no incoming jumps after the last expression, dont bother emitting it, just fall through in bc.
  i do that check in asm too casue i did that first but seems nicer to not even get there.
  only fixed call_if_in_unquote.

ok i have to go back to passing a result ptr because its compeletely impossible to read the asm,
cause it does these insane redundant loads where it spills to the wrong place while trying to load the whole thing to do a copy.

- made Never say it takes 0 slots which removed some weird -1s and let asm assert no flow on stack between blocks without failing more tests
- putting 16 in a register and then dereferencing it. swap_remove for storepre instead of shifting remove. cri.
- then an off by one in storepre
- moved most of the flat call abi work to emit_bc so it can emit to its arg loc and use its ret loc without copying.
  now passing 48/90.
- typo construct_struct was duping on PushStack instead of ResAddr. pass 52/90.
- impl dup non sp reg. pass 56/90
- fix 'assertion failed: target_c_call && !target_flat_call': move adding flat_call if big arg/ret from bc_to_asm to emit_bc
- flat_call PushStack tries to load but it doesnt push the ret addr first. and was loading |arg| instead of |ret|.
- for flat_call header, it was only deciding to use result addr based on ret size, not considering explicitly marked cc like in my test.
- offset from spill wasnt doing the mul 8. would affect anything not inlined with multiple returns. pass 65/90.
- result aliasing miscompilation is back. whatever. ignore for now.
- another card on the house of not tracking dominators. now i leak slots because when you do the second if branch,
  it doesn't know which are safe to reuse anymore. can do that now because result locations mean less wasted slots.
  but thats 95/101. that was the problem with @bits.

TODO: use => instead of = for capturing functions. remove need for fn keywork on values, just use that for
add to overload set stmt. fix stack slot reuse. !return targeting an overload set and it just resolves to the current function.
!return without fn arg for outermost fn. version of while loop that passes in labels for break and continue.
same by var ptr for large values for !return as !if. fix whatever debug check is going off.

## basic blocks (May 6)

The way my bc handles loops and ifs is kinda hacky and convoluted. gonna try adding the concept of basic blocks.
The general goal of this whole adventure is making it easier to think about how to add tail recursion and nonlocal returns.

The problem is that any time control flow diverges (ifs and loops),
you need the places where it rejoins to represent each v-stack slot with the same reg/slot.
Its what llvm phi instructions are for but I like my way of just both branches leave something on the stack cause its so simple to think about.

BEFORE:
ifs made a variable that each branch wrote to and then read the return value when rejoined.
while always spilled everything to stack at the beginning so both times you get to the cond block its the same,
dont have to worry about stuff getting spilled in the body so its in a different place the second time around.
I don't keep track of basic blocks so you can't really tell whats going on.

AFTER:
Split bc into basic blocks where you can't jump in/out of the middle (other than calling functions that return to the same place).
Blocks can take arguments (only used for rejoin after if), where jumping in puts in ccall reg and then the block puts back on stack.
But mostly stuff still just implicitly flows on the stack (i use that for temporaries).
I still just spill everything (other than args) at the end of each block.
TODO: be more consistant about representing block arguments? rn stuff can just flow on the stack behind you kinda.
real function args should be args to the entry block but currently are hacked in.

- failing math_still_works. JumpIf: flipped true and false branches but still tried to cbz. true needs to be right below.
  that got 68/87 working
- three problems now: `unhandled prefix in @switch`, `attempt to subtract with overflow`, `(slots as u32 - float_mask.count_ones()) < 8`
  the two are ifs returning big values that don't fit in register, need to use variable like before and floats aren't done properly.
- made ifs use var if they return something big. 81/87 but had to turn off reusing stack slots because bbs are done depth first not in emitted order now.
- 86/87. fixed nevers now that sometimes they need to push junk on stack. TODO: thats a hack. calling a Never should be a terminator like unreachable.
- broke never more so minor code change in derive makes all pass
  BUT bug call_if_in_unquote is fixed and that was really creepy so i think thats a net win and i'll call it a day.
  i'll fix Never properly eventually.

its not slower which is nice. stack slot reuse is fucked which is bad. code is more complicated but looking at the bc dump is more legible.

## (May 5)

- removed some places where it pre-interned a fnptr type since all exprs have thier type now, emit_bc shouldn't need to mutate program to make a new one.

trying to remove uses of type_of and mut_replace because they're clunky ways of doing things.
A) its a massive problem if you error in a mut_replace and try to recover cause you lose something.
b) type_of is kinda all or nothing on getting the type so its not really what you want for overload resolution.
you just need more compile errors to be recoverable so it can iteratively narrow down the types of each arg seperately.
now only type_of is for overloading so I can spare some code to fix that cause it means getting rid of the type_of junk.

- convert to @At macros: !symbol, !assert_compile_error, !type.
  tried to convert @as to !as but its still a massive hack because i use the @as syntax but wrapper macro isn't enough,
  because type_of can't just apply one layer of macro and get the type, it always tries to compile the whole thing, so theres still a speciall case.
- changed serial tests to reuse the same compiler instance and now that's faster than the fork one.

## improving enums (May 4)

all these are not big expressiveness wins, really just trying to raise morale.

- renamed rust @enum to @tagged cause it seems kinda dumb to overload the name enum.
- did @enum(name1, name2) as a lib macro so don't have to write the sequential values cause that's annoying.
- remove #enum (for generating init constructor). it was used in 1 place and didn't integrate well with treating types as values.
  still have mixed feelings, maybe i want it to be easy to add constructor without changing callsites but meh.
- remove #struct. that was used in a few more places but still felt like low value add.
  if I want that, I should just implement it in the library, it doesn't need to live in the compiler.
  so need to implement hash macros that transform functions soon.
- remove !enum, just define it as @tagged in the compiler.
- remove !struct, like above. that was used more often but i like the prefix syntax more
  and started using as soon as i had the forwarding macro anyway. it means you don't have to look all the way to the end to see whats going on.
  (maybe should rethink my & syntax then too...). only sad thing is now if you define one inline as a function return,
  you have to wrap in brackets because the lhs of an assignment could be a macro call so the parser complains.
  i think its not actaully ambigous but threading the context through seems a bit yucky. idk.
- made !size_of just be a compiler ffi function, i was already using it through a function wrapper anyway but now that doesn't have to be pointlessly specialized.
  it does mean you have to explicitly '::' it if you want it to be const known because I don't have auto folding yet.
- hack fix to ""!type not being Slice$i64, since constants are lazy now, just have the parser emit casts to a fixed identifier.
  sad that its 4 ast nodes for a string constant now (5k extra nodes in full test, ~2%, yikes) but its much less cringe than the old system where you had to manually do the cast or write a bunch of overloads.
  there was something weird about @as not resetting the type when I set the values to the expr which made it the unique type from rust String ffi.
- removed `program.intern_type(TypeInfo::Int(IntTypeInfo { bit_count: 32, signed: true })); // HACK: for ffi find_interned after moving IntType to export_ffi -- Apr 17`
  seems to work fine without now.

(PLAN)
eventually want to unify the different ways of doing @enum and maybe make the sytax be a block isntead of a pattern.
so you could mix implicit values and specific ones and specifiying the type.
Also switching to zig sytax where struct def looks like a block and then initilizing fields uses = instead of : would
be pleasing I think. have named arguments use = as well so there's symmetry.
(END PLAN)

## Finished new bc_to_asm (May 2/3)

- calling assert_eq wasn't leaving the v-stack empty at the end. lol Bc::Pop wasn't doing the right count, always 1.
- thats enough to get through 5/29, but I can't turn on COUNT_ASSERT so I don't trust that its actually working.
  yeah assert_eq(1, 2) passes :( i was replacing any eval that gave structured::const with Noop, but might still have runtime side effects.
  fixing folded ifs made that work, now 4/29 but i have more faith.
- why are so many things flat_call?
- fixing corruped types was enough to make hello world actually print the whole string!
- problem with slize size vs len. sadly i even rememeber deleting the code that got it right cause it looked weird.

Fixed aliasing result location shit so some tests changed:
// (structs) You'd expect this to be 1. It would be great if you break this test somehow.
// (structs) BUG. You'd expect this to be the same as above (b=3)

TODO: fn index doesn't work if not inline thats like something cripplingly wrong in the calling convention stuff!!!!!

- force spill before every loop fixed all except ones with floats (which i dont have yet) and parse_asm

I think my prize for getting this working will be `+=, -=, \*=, /=`. (did that, its just a macro).

Then maybe llvm ir text will get me normal operatiors.

## stack based ir (May 1)

I think my current bc format is dumb.
It tries to treat tempararies the same as variables in a weird way and uses MarkContiguous/slot_is_var to reconstruct that info.
It tracks slot types but I don't really trust that its always the raw ones and you only really care about unit/int/float, and i guess other int sizes eventually.
I think a stack based on would be easier, don't bother specifying ins and outs of each op, its just implicit flow.
Then you know temps are on the working stack and have var slots for vars.
Originally i was thinking llvm does infinite registers so i should too but there's no point if im not doing data flow optimisations-y stuff.
As long as im friendly about how I use the stack, like an if branch can't pop backwards and replace, it should be trivial to turn into llvm ir,
probably easier than the current one.

I think I'll ditch my current llvm backend regardless because thier c api is annoying.
It feels like it would be less code to just generate the text.
Maybe I want to commit to only having one backend in rust and write the rest in my language.
It doesn't work anyway and I'm not inspired to fix it cause the error message is always just SEGSEV so you can't even see the broken ir you generated.
Maybe thier cli stuff has friendlier validation, it can't be worse, and I could still link to thier c api and just call LLVMParseIRInContext if I want to jit it in my process.

So far looks like stack based bc removes a bunch of reserve slots stuff which is nice.

- confusion about the stack having addr of a stack slot vs deref of a stack slot. currently I don't have spilling.
- there's somethign weird where functions with a constant value body still get emitted asm so have to fix that later but it should work anyway
  its comptimes ones where the function decls get left in the body as declfinished so it doesn't know its safe to ignore the block body.
  oh and im keeping anything with annotations just in case but that inlcude #pub.
- lamo in my load after switching to trying to keep reg on stack instead of storing immediatly i forgot to move get_free_reg into the loop so a big store was putting all parts in the same reg (and stomping)
  that was enough to fix cases/corrupted_types.fr.
- GetVar emit_bc was just getting the address without doing a Load
- wasn't dropping x1 after flat call prelude but that wasn't the problem. I bet its that i dont restore args as free after a c_call, yeah that helped.
  oh and i was wrong about dropping x1, load() drops the ptr reg
- added spill support for when you make a call so your v-stack doesn't get stomped
  thats enough to get hello_world to run but it just prints one letter "H".
  thats very unfortunate. if its gonna be wrong it really shouldn't compile.

// TODO: dont have to do full spill for inline basic ops that only use fewer registers than args. have the annotation mean that too.

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
Because I store both the funcid requesting to be a pointer and after the addr that is the pointer as the same FnPtr type because to the user program they're the same.

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

/// The normal cargo test harness combines the tests of a package into one exe and catches panics to continue after one fails.
/// It doesn't expect you do be causing segfault/illegal instruction signals, (which is generally a reasonable assumption but less so for my compiler sadly).
/// So when a test does that it kills the whole thing instead of being recorded as a failing test and continuing.
/// Instead, I start each test in its own process so no matter what happens, it can't interfear with the other tests
/// (unless they're like trying to use the write files or something, which like... don't do that then I guess).
/// My way has higher overhead per test but it feels worth it.

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

## struct init as function (Mar 16)

// The above is a macro that expands to:
// const Person = (age: i64, gender: i64)!struct;
// fn (age: i64, gender: i64) Person = (age: age, gender: gender)!construct;
// Note, this is not a ~constructor~, its just a function that happens to construct a value of the new type.
// There's nothing actually special about the function, its just a convention that seems nice.
// I think constructors in java/c++ are a bit weird because you have access to a 'this' pointer before you've actually produced a value in a valid state.
// Instead, here you are forced the figure out all the fields you want and put them in place all at once.
// TODO: currently (a: b) === (a: b)!construct and there's nothing forcing someone to actually call your nice factory function (private fields, etc.).

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
  (dataptr: self, vtable: log)
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

## notes.txt (Feb 12)

Value Repr

A tuple is a value type. Some range of memory on the stack/heap.
Passing a tuple to a function is always conceptually a memcpy from the old location into the callees's stackframe.
Some types are Move but not Copy so passing by value invalidates the original data (Drop will not be called on the original).
Move is a semantic construction, it doesn't actually change the calling convention.
Structs have the same repr as tuples, just with syntax for accessing named fields.
All scalars (numbers, ptrs) in the interpreter take one Stack Slot, a tuple is a range of stack slots.

Variables

runtime cvar capture is by just inlining the function.
comptime var capture is by copying it into the function object (always immutable).
const means constant to the function's compilation.
comptime functions can still have let/var.
types and closures fns can only be in consts or in comptime variables.

Other Notes

// opt: pass by &const if not mutated. Have some way to assert that in the function def so you don't accidently cause copies.
// But I don't like Zig's way of forcing you to writing a whole line assigning to a new variable.
// LAMO https://github.com/ziglang/zig/issues/16343
// Maybe like rust where you just write 'mut' on the arg but don't add a keyword.
// Distinguish between wanting to mutate the caller's value (explicit pass ptr),
// and wanting to mutate your copy. Make sure you can't make the mistake of taking address to mutate and getting caller's.
// Distinguish logical mutation (like through a pointer) and reassignment (where you change what's in the stack slot)?

// Goal: I want to not need the linker.
// If you don't want to interact with other languages you should be able to run my compiler and then have a program.
// Go's compiler stuff seems pretty good, that might be a good one to steal.
// https://www.youtube.com/watch?v=KINIAgRpkDA&list=PLtLJO5JKE5YCcdJGpmMqB8L4C4VNFN7gY
// If I use their stuff, be careful about too many calls between rust and go...
// But I assume c abi is only slow if you're trying to use goroutines which I don't care about.
// - https://shane.ai/posts/cgo-performance-in-go1.21/
// - https://www.reddit.com/r/golang/comments/12nt2le/when_dealing_with_c_when_is_go_slow/
// I need to be very careful about sharing common work between similar backends if I want to pull off having so many.

// I want to keep the interpreter using fully linear types in debug mode,
// but have them happen to work out as a normal stack so you can have a fast mode that just skips the redundant stuff.
// I wonder if I would then be easy to do wasm since that's also stack based.
// I'd be fine with tweaking my bytecode a bit to make that work better.

// unify function statement and closure expressions?
// but there's a difference between one you release to the outer scope for overloading and one that's just a value you can pass around.
// might be a good idea to allow a name on closures just to let it show up in stack traces for debugging purposes.
// but then its less clear that it's different from a possibly overloading function declaration.

// Overloading and Generics: maybe distinquish between a function template and a bound function.

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
