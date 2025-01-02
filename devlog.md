## wasm (Jan 2)

- globals for stack pointer so now (static) alloca works.
  (for now i don't let frontends create thier own globals, they can just use a normal symbol in the data segment until i want exports).
- store instructions
- typecheck_failed: added check for store instruction with result

## wasm (Jan 1)

- data segment. kinda nice that relocations are just you put the number in because you know your base address is zero.
  skip a page so we don't assign something to the null pointer because that sounds confusing.
  (TODO: should i have an option insert null checks since you won't auto-crash like you would on native?).

##

- added J.switch block terminator
- experiment with the jump table being a data symbol of offsets or being a bunch of direct jump instructions.
  everything (including no jump tables, just if-chains) is roughly the same speed
  (ie. i can't tell the difference when just eyeballing the time to self-compile).
  so that's kinda disappointing. should probably take it out since it's extra code for no benifit.
- reworked opt/fold to work with J.switch. my version is a regression,
  it doesn't handle phi nodes that become constant properly anymore (added a test case: fold2.ssa).
  but at least i understand how it works now and can come back later.
- fixed @is on non-sequential enums

## (Dec 29)

- fixed problem when doing `use_map_jit_on_apple_silicon` i was calling mmap twice
  and kind-of assuming they would be next to eachother and trying to munmap them as one.
  and it looks like it should have just worked out because i said too big a size for the first one so it wouldn't try to free junk when they didn't get put adjacent,
  and i guess i don't rely on layout that much when jitting, i just always pictured it a certain way in my head.
  now i mmap the whole thing at once and then MAP_FIXED over it to change part of it to not be MAP_JIT.
  It seems that MAP_JIT and then MAP_FIXED over it is fine, but the reverse is not.
  now i can drop comptime_codegen and run with `-repeat` for longer (also fixed 60fps.fr).
  `./boot` is still flaky tho.
- fix `display` for non-sequential enums.

## (Dec 27)

- failing `@debug_assert_gt(c.canvas_width, 0.0);`, which i think is actually a miscompilation of the compiler itself when doing stuff with the constant `: f32 = 640,`
  fixed incorrectly flipping prefix for truncd but that wasn't enough.
  but now it works if you do it in a normal variable instead of in a struct default value.
- construct_struct_literal wasn't calling coerce_const_expr, it was just assuming the type of the value matched.
  TODO: really find_const should call it for you
  that gets farm_game working on amd64!
  > bad that i can't explain why that worked on arm since the little repro test i wrote fails on the old version of the compiler.

in fact, i think amd64 now works better than arm64 because of my cache coherency woes, it can run 64fps.fr.

- fixed `need to do_merges when resolving by type.`
- clean up arm emit() for some instructions with similar bit patterns

## amd64 bug fix extravaganza (Dec 26)

- jit hello world works if i got_indirection_instead_of_patches=true on both modules. so clearly i was right when i asserted `!=.JitOnly` but i still don't know why.
  problem was a call to a DynamicPatched was outputting a fixup that would never be applied.
  so it was just calling offset 0 which is just the next instruction, and it was near the end of the function, so it returned to itself and then returned again.
- `rosetta error: unexpectedly need to EmulateForward on a synchronous exception` is fun because lldb chokes on it forever so you can't see what's happening.
- ok so it must be something shim related. sudoku works when aot compiled to x64 but not when jitted on x64,
  but it works if you don't call `solve` at comptime. oh and it works if #inline `assign`.
  works also if you uncomment `is_ready := f.getcon(0);` in create_jit_shim (which is also enough to run `examples/default_driver.fr`).

hmmm,

```
R2 =l addr $F924__shim
R6 =l addr $assign__924
xcmpl R2, R6
R2 =l addr $assign__924
jfine @, @_
<snip>
---
endbr64                                 ## encoding: [0xf3,0x0f,0x1e,0xfa]
push	rbp                               ## encoding: [0x55]
mov	rbp, rsp                            ## encoding: [0x48,0x89,0xe5]
push	rbx                               ## encoding: [0x53]
push	r12                               ## encoding: [0x41,0x54]
push	r13                               ## encoding: [0x41,0x55]
push	r14                               ## encoding: [0x41,0x56]
lea	rcx, [rip - 24]                     ## encoding: [0x48,0x8d,0x0d,0xe8,0xff,0xff,0xff]
mov	rax, qword ptr [rip + 100556228]    ## encoding: [0x48,0x8b,0x05,0xc4,0x5d,0xfe,0x05]
cmp	r8, rcx                             ## encoding: [0x4c,0x3b,0xc1]
mov	rcx, qword ptr [rip + 100556218]    ## encoding: [0x48,0x8b,0x0d,0xba,0x5d,0xfe,0x05]
jne	56                                  ## encoding: [0x0f,0x85,A,A,A,A]
<snip>
```

so that sure isn't a program...
it loads from the got into rax and then uses r8 instead.
sad day, in the .addr hack i just did, i typed 0b01001000 instead of calling pack_rex_si so high registers didn't get encoded.

- using the x64 compiler to aot, i get `we guessed wrong so the array resized and all the pointers are junk`
  sometimes, with a random multiple of 4096, but when it works it gets the same hash as the arm one cross compiling which is a good sign.
  ah, trying to zero_pad_to_align to page size, and doing that starting at the pointer to the base of the array, not at zero and just using the length,
  and `commands` is at the start of the text segment so it's allocated by page_allocator,
  so when running on macos-arm, where the page size is 16k, its always 16k aligned, but macos-amd's page size is 4k.
  need to do it based on the length instead of the address.
  same thing in codesign `deal with page spread across chunks` because of the SegmentCursor.align_to at the top of macho.emit.
  TODO: i should take advantage of the smaller page size to emit less padding on that platform but for now it's not a big deal.

- avoid folding memory accesses that create `base+index+(>4GB constant)`,
  which happens when jitted code has constant addresses and then you try to access a nested array i guess?
  TODO: i think now im being too conservative with the folding. it's fine as long as you don't have both base and index.
  that's enough to self compile on amd. hurrah!

and reproducibility works with cross compiling which is amazing.
still not everything works on amd64: lox, mandelbrot, and the gui programs.
incidentally, those are all programs that use floats heavily.

- float bitcast encoding mistake. in the table we use (.F, .M) vs (.M, .F) to tell the directions apart,
  but the movd:movq always takes the float arg on the same side so one of the directions needs a swap before actually being encoded.
  that fixed lox+mandelbrot.
- did register shuffling for 2-adddress fdiv which gets farm_game to compile.

## amd64 bug fix extravaganza (Dec 25)

- yikes, unpleasent memory safety thing.
  Alias.slot is a pointer to another tmp's alias field.
  However, it can't be an actual pointer because we read it in getalias which is called from loadopt:def
  which is intersperced with newtmp calls in that pass, which cause f.tmp to be resized, invalidating any internal pointers.
  I think it works for qbe because f.tmp is PFn so free doesn't do anything and after the alias pass once you start making new tmps,
  you don't need to update the alias info so it's fine that you're pointing into an old copy of the tmps array if it resizes.
  which would apply to my temp() alloc too except that when i suspend for inlining, it copies into a new QList in libc_allocator,
  and then keeps using that at the end if i need to emit_suspended_inlinables, at which point free does do something and
  now we're at the mercy of whatever your libc's malloc decides to do.
  i think i want to just use the module's arena instead of libc_allocator anyway but still nicer to follow the rules i think.
  seems that was what was making ./boot fail most of the time so that's very good to have fixed, but -repeat still calls into garbage so more mistakes exist :(

trying to get the compiler to run on amd64 with my own backend:

- fix typecheck_failed
- in sysv-abi, zero AClass array so we don't have junk in the align field
- turn off some div folding temporarily cause i don't want to deal with it yet
- truncd
- handle call to forward reference when got_indirection_instead_of_patches for jitting
- `Assertion Failed: cannot encode offset 4641883552`, hmm, we sure are loading a constant... `R2 =l load [4641883552]`.
  oh because we're jitting so we poke in real addresses and those are larger than 32 bits so can't be done encoded in one memory displacement.  
  have to catch that in isel and copy to a register.
- for got_indirection_instead_of_patches i need to convert RMem into a load from the got instead of a constant displacement in the instruction,
  but by the time you realize you want to do that in push_instruction it's kinda to late to do it sanely.
  like now i need to pull emitting prefixes into that function as well so i can do extra instructions first?
  but then i kinda need to reserve a register to use there.
  tho so far it mostly seems to be O.Addr which is easy because it's just producing the address.
  so start with just hacking it in there and see how it goes i guess.
  that's enough for it to jit `hello.fr` to the point of `// Hello Compiler` but then it gets bored and doesn't do the rest.
  you win some you lose some i suppose.
- fixed walk_dir test. cross compiling with #link_rename means fill_from_libc didn't find things.

## (Dec 24)

- fixed compiler_gui/dearimgui_demo examples
- don't need the asm versions of call_dynamic_values anymore
- there's something very strange where it sometimes decides you can't coerce an overload set to a function pointer.
  problem was suspending in coerce_const_expr with done=true so you don't come back.
  so indeed a subtle compilation order thing that started exibiting when i started marking more things done yesterday.

## (Dec 23)

- fixed occasionally getting spliced names like `_Anon__5989Ano`.
  thread safety in intern: each bucket has a lock but they were using a shared ArenaAlloc.
  the rust people start to make some good points.
- i was calling join_codegen_thread on comptime_codegen instead of the one used for the driver,
  so if you called a shim that tried to trigger a compilation, it would get stuck.
  but maybe changing that doesn't make any sense either, because if my premise is that (we use different modules
  for the driver part and the comptime execution because we're pretending the driver is like an aot program and
  saving time outputting a binary is just an implementation detail),
  then you shouldn't be calling a function in the comptime module from the driver module.
  but if you made a function pointer value, that will be in comptime_jit and it doesn't do any remapping since it's not Aot.
  so then calling directly and calling through the pointer will be calling different versions of the same function.
  so maybe it's wrong to try to use two jitted modules on the same compiler instance.
  but also you kinda don't want got_indirection_instead_of_patches set on the driver one because that's slower.
  somehow ive revealed a massive oversight here.
- uncommenting (in compile_expr:Tuple)
  // This would make sense to me but it makes lox test call uncompiled.  
  //expr.done = done;
  saves ~50ms (1350 -> 1300 non-legacy, when i get lucky with timing).
  gets me to 777 when i cheat (old jit + use legacy but build new + precompiled driver).
  the comment was from before i had shims so now it's safe to be more careless.

## (Dec 22)

Made from_bc single threaded because i think it makes a better example if it's not sharing the CodegenWorker stuff with the new version.

Extracting -legacy. Getting rid of `opts.comptime_jit_vtable` because i think supporting multiple is more painful than it's worth,
you can still have driver programs for other runtime backends, but for the core thing that has to deal with out of order compilation stuff,
it's really painful to add any more confusion than necessary. go through and make sure all the ifs on NEW_COMPTIME_JIT are static so we don't have to include the code.

before:

```
>>> compiler configuration: context=implicit, threads=true, debug_assertions=false, nctjit=true
>>> compiled driver__1783 in [CPU Time] frontend: 160ms, codegen: R:90ms + C:25ms
>>> [CPU Time] frontend: 1268ms, codegen: 515ms
>>> 1345576 bytes of code, 87264 bytes of data.
```

after:

```
>>> compiler configuration: context=implicit, threads=true, debug_assertions=false, nctjit=true
>>> compiled driver__1950 in [CPU Time] frontend: 296ms, codegen: R:169ms + C:42ms
>>> [CPU Time] frontend: 1201ms, codegen: 434ms
>>> 1105560 bytes of code, 80176 bytes of data.
```

Now you have to pay the cost of compiling that in default_driver but you can precompile it and then we're back to how it was before but with a different connotation.
I'm considering having drivers use shims too so you don't compile big functions until the first time they get called.

the thing where run_tests hangs is because fork doesn't duplicate threads.
so when it tries to compile with NEW_COMPTIME_JIT it gets stuck in wait() forever.

## (Dec 21)

this code is pretty funny

```
if (_sapp.quit_ordered) {
    return YES;
}
else {
    return NO;
}
```

and like ok sure, maybe you're not supposed to assume you know what `__objc_yes` is,
but it would seem that's just so the compiler can tell if you meant to make a bool or an NSNumber
(https://releases.llvm.org/3.1/tools/clang/docs/ObjectiveCLiterals.html).
we can't be having 6 lines of code per line of code.

```
#if __has_feature(objc_bool)
#define YES __objc_yes
#define NO  __objc_no
#else
#define YES ((BOOL)1)
#define NO  ((BOOL)0)
#endif
```

Little vacation from compiler was nice but I'd feel a lot better
if i could extract the old backend from the core compiler
so stuff gets less insane before i risk forgetting more things.

Trying to get it to compile the tests that do `@if(@run query_current_arch().unwrap() == .aarch64) {`
is somehow super painful because of how shims interact with redirects (in this case inserted for #target_os
but probably always because there's something in vec that didn't work as a #redirect).
It should be so easy but somehow i can't do it.
Clearly I need to make that system simpler somehow.

Jit shims make fewer redundant calls to report_called_uncompiled_or_just_fix_the_problem:

```
// If the entry in __got for the target symbol is not this shim, call it directly.
// Otherwise, call into the compiler to ask for it to be compiled.
// Most calls will just go through __got directly in which case this check is wasteful,
// but it helps in the case where you created a function pointer to the shim and stuck it in memory somewhere,
// now not every single call to that has to go through the compiler and take a mutex to look at the symbol.
// It seems this doesn't make each shim only call into the compiler once (TODO: why?) but it does lower the number.
```

not measurably faster but spiritually better.

need to fix repro. it's only when NEW_COMPTIME_JIT and not -legacy which is iteresting.
so it's something about trying to use two modules in new emit_ir at the same time?
but -legacy still uses the new backend so it's specifically a frontend problem?
many programs work tho. parser specifically has this repro problem.
ah, there was a path in emit_constant that didn't go through `c.zero_padding(ty, to_zero&);  // Needed for reproducible builds!`
so thats reassuring. works again.
still can't quite articulate why it only happened when both sides used the new thing.

## (Dec 19)

- allow multiple things in index expression `a[i, j]` -> `index(a, (i, j))[]`

## (Dec 18)

- start porting sokol_gl
- added require_layout_ready(Type) so i can add offset_of
- start working on translating my language to MSL for shaders.
- added get_function_ast so you can do more powerful reflection things at comptime without wrapping everything in a macro.
- allow #annotations on struct fields.
  they don't do anything, Binding and Field just pass them through, but comptime code can read them.
  so i can use them for `layout(binding=0)` / `[[position]]` / etc.
- shader languages have the vector swizzling thing like `v.xxxx` which would be kinda nice to have so it could be lowered directly for shader translation,
  and still allow you to run those functions on the cpu if you just want to print out some numbers for debugging.
  so im tempted to let you register magic types where field accesses become macro calls,
  but maybe that gets super confusing quickly.
  did some plumbing for TypeInfo.Swizzler but the compiler doesn't call into it yet.
- (debugtext) parse font data from readable strings at comptime.

## (Dec 17)

- auto-cast float literals to f32 so you don't always need a `.cast()`
- allow underscores in literals. `1_000_000_000` is more readable than `1000000000`
- auto-cast string literals to CStr so you don't always need a `.sym().c_str()`
- infer type of const fields on structs lazily so you don't need to use `@rec` as often to use them as namespaces.
- implement link_rename in from_bc/emit_ir for walk_dir on macos x64. works with -c + clang but segfaults with my own exe. sad day.
- fix x64 emit neg acting on the wrong register. that took much too long to find. ugh.

> using a different computer.
> recall: `spctl developer-mode enable-terminal` -> Privacy Security Settings -> Developer Tools.
> makes ./boot go from 19s to 10s.

## (Dec 16)

- examples/softdraw can successfully make a window and draw stuff now that i have a jit backend that follows the abi correctly.
- new example: simple gui program that lets you scroll around a text file.
- ported sokol_debugtext. it looked intimidating but most of it was tables and c macro boilerplate.
  eventually i want to be able to compile a subset of my language into the shader languages but for now,
  i think having a seperate binary file for the precompiled shaders is less ugly than putting it in a giant byte array in the code.
  clicking into a file that's 450 lines long is much less unpleasent than one that's 4900 lines long.

## (Dec 9-15)

- my asm for call_dynamic_values is super primitive so i can't make `fn zeroed($T)` less dumb,
  so instead of that just jit a function for each FnType, that deals with moving arguments between byte arrays,
  and then i only have to implement the abi in one place. but i can't use that until i get rid of old comptime jit because it didn't follow the abi either.
- want to use new backend for comptime jit as well but that's harder
  because you need to be able to call functions in the middle of adding more to the module.
  instead of doing patches, always have forward references go through `__got` even if they're local,
  and then i don't have to worry about flushing modified code after doing a patch.
- version of `create_jit_shim` that uses new backend. again, can't use it until everything does because of abi struggles.
- new backend does more work per function so more incentive to fix how many tiny useless ones i create in `immediate_eval_expr`.
  instead of suspending on Jit after deciding it needs to be wrapped in a function, try CompileBody first and see if it decarys to a value.
  add PtrOffset/Deref to `check_quick_eval`
- trivial mistake with just using `idx := 0` every time i wanted to get a new entry instead of keeping it around
  so things didn't always get compiled in the right order. so extra confusion about which callees would be in range for a sys_invalidate_icache
- we've learned condvar wait/signal is not the same as just sleeping in a loop until a variable changes.
  i guess you're not guarenteed on what order you'll see changes on different cache lines even if you do both changes and check in a mutex?
  which is fair but then why is condvar any different? do they put some magic barrier thing in there that applies to all memory?
- forever confusion about when you have to call sys_invalidate_icache.
  i guess it has to go on the thread that runs the code, but also surely you need to make sure the thread that writes it flushes its data cache.
  so do you need to call the other one there too?
  can't just have shims get filled in later because you need to make sure the new thing gets flushed even if you don't compile anything else in between?
  memory barrier instruction seems to make it a bit better.
- another round of sprinkling #inline around. got resolve_in_overloadset_new from 740 to 500 samples
- take out add/sub 0 (in arm isel which is silly but convient)
- fix ir templates to use tmp.visit for arg index so you can disable TRACK_IR_NAMES
- use copies instead of load/store in the frontend for scalar vars that you don't take the address of.
  doesn't make it much faster but it's easier to read the generated code. could do better with more guess_can_assign.
- silly fix for `-c` with new emit_ir, just had to leave imports Pending.
- #target_os has a problem with looping calling its shim when jitting because the target doesn't get added as a callee.
  but its going to be such a pain for cross compiling if calling it when jitting puts junk in the callees list,
  maybe it needs to be a more aggressive special case.
  the worse thing is just how painful and confusing diagnosing that sort of problem is but I'm not sure how to make it less complicated.

##

- in arm isel, instead of doing fixarg and then going back later to fold address computation into the immediate of load/store,
  do a prepass. this way you don't create a bunch of extra temps that are just going to disappear later.

trying to do elide_abi_slots.

stats are -unsafe, compiling itself.

```
before:
>>> [CPU Time] frontend: 1692ms, codegen: 595ms
>>> 1420472 bytes of code, 86384 bytes of data.
removing loads:
>>> [CPU Time] frontend: 1569ms, codegen: 598ms
>>> 1311064 bytes of code, 86496 bytes of data.
```

##

- make slot promotion treat 1/2/4/8 byte blit as a load+store.
  kinda silly cause the frontend just shouldn't generate those but while prototyping its nice to not care and still get nice output.
- mistake making arena_alloc always return 0 was Block using the result ref from compile_expr instead of pulling it out of the placement.
- i bet the problem with default_driver is that im trying to use the real calling convention now.
- when a constant is Value.Small it might still be an aggragate like (u32, u32) so scalar_result should allow storing that from an int literal.
- trying to run other_main its trying to jump to 0s when calling into jitted code by the old backend.
  which looks a lot like the not calling clear_cache problem but the address is wrong.
- for macho_loader, it seems to think MAX_i64 is -1? oh cry, i said cgew instead of cgel so it was only looking at the low 32 bits and comparing as i32.
- trying to make it work for aot. its trying to import vars. oh im not calling reference_constant recursively.
  and same for mangling imported symbol names. on the plus side, this is the most helpful error message i've ever seen from the machO stuff `dyld[3164]: Symbol not found: _memmove__231`
- got run_tests to allow using the new stuff. fail 201/309.
- forgot to call check_for_new_aot_bake_overload. sympom was strings only being the first character because it thought it was just a \*u8,
  and then you'd print whatever other garbage bytes which were often invisible because unprintable.
  fail 151... fluctuates tho which is creepy.
- replace emit_constant with creating a data structure so codegen thread doesn't need to access compctx and frontend thread doesn't need to access the segments.
- randomly fails sometimes, but at least now i can look at the exe. mandelbrot is pretty small and still has the problem which is nice.
  lowering abi types wrong sometimes, less if i spam print so you know its a threads thing.
  mutex for module.types, frontend needs it the first time you reference a type, backend needs it during abi and inlining.
  got lucky for old backend because i used opaque types cached by size instead of trying to follow the calling convention.
  also wrap_with_runtime_init can't use module.scratch.
  fail 5.
- back to the old one for a moment, time's bloated cause i have an extra ~2k lines.
  finally doing #inline not makes codegen thread 650 -> 610.
  it's annoying because #inline happens before #fold so you have to manually `::` for conditional compilation.
  kinda fixed it but :InlineFoldHack.
  really should just do the special case and make sure `if !` just swaps the branches/cmp.k
- add BakedEntry.FnPtr to pending. fail 3.
- aha i remember this when i was ffi-ing with the bootstrap rust compiler.
  Span:(u32, u32) my old jit wants to pass in 2 registers but that's not what the abi says to do.
- changed a bunch of stuff in the frontend to let things be intrinsic on one backend or another with normal body otherwise.
  thought it might get slower but seems the same-ish.
- i think my current thesus is that the emit_ir is about the same speed but generates much shittier code so slower when self compiled.
  the new one compiles faster if it was itself compiled by the old one, 1450->1400.
  but then compiling itself is 1700. backend time is more similar for all 590 vs 615.
  i bet its because im actually using the abi passes now and they generate really dumb loads+stores because they happen after mem opt.

> i really need to write my own terminal thingy.
> i like that warp lets me click the mouse to move my cursor.
> i don't like that they give you a shitty banner ad until you update
> and then as free bonus with your update change the colour of the suggestions background and the boldness of the text.
> like bro please just let me run the program

## (Dec 1)

- finish emit for shared libraries
- support loading driver from a dylib instead of recompiling it everytime.
  - confusion about a `.sym().c_str()` on a runtime value that was relying on being jitted.
  - need to improve error detection for calling a #comptime_addr at runtime
    now that i don't detect it when trying to compile Bc.GetCompCtx (since that doesn't exist anymore).
    right now its just an int3/brk which is not super helpful.
  - saves 170 for default_driver.fr and now i don't need to feel bad about taking backends out of the core compiler,
    because they don't always run in the slow jit anymore.
    for example, `-aot=llvm -S` takes 1800 to jit and run or 1240 when precompiled.
- for bscopy, just calling memcpy instead of `BSOP(a, b, fn(a, b) => b);` makes codegen thread go 600 -> 550.
  im sure llvm would have noticed but i certainly don't do idiom recognition.
  backend isn't the bottleneck currently tho so it doesn't actually make it compile faster.
- asm for count_ones and trailing_zeros cause i don't like seeing them near the top of the profile.

it's sad that my asm functions have to be done through a call with the c abi where i spill all the locals first.
i can't decide if i should try to commit to making intrinsics for everything you could ever want,
or try to make real inline asm that can exist as an expression inside a function so good that it's not painful to do everything with inline asm.

letting the backend have asm inside functions might actually be easy, because rega already doesn't know very much about calling conventions,
the abi/isel stuff expresses register constraints as copies, so if i let you just have an opaque blob of asm bytes
that uses specific inputs/output registers as though it was a call. just need to have the frontend passes that expect only tmps
to leave references to registers alone. tiny change to alias, copy, fold.
that's not quite as good as c's inline asm where you can have template registers that get picked by the compiler,
but it's pretty pleasing for so little work.

## (Nov 29/30)

- frontend: slow progress on new emit_ir from ast instead of going through old Bc.
- arm: fixealways run in the slow jit anymore. d HFA abi. it was always passing >16 byte structs in memory which i learned was wrong a from soft_draw example a while ago.
- arm: fixed incorrectly transcribed udiv bits. only manifested on fold test with fold turned off which is strange.
  always run in the slow jit anymore.always run in the slow jit anymore. didn't mess up franca because i only use signed math currently.
- amd: temporary rort/byte_swap #asm impl. revealed bug with const arg + FuncImpl.Merged that im going to ignore for now.
  that gets sha256_examples working on x64 (but only with linker, not my exe... odd).
- incantation to get sane disassembler: in `~/.lldbinit`: `settings set target.x86-disassembly-flavor intel`
- amd: embarasing amount of time on storeh being an incorrect copy paste of storeb `e.a&.push(@as(u8) c.trunc());`
  instead of `e.a&.reserve_type(u16)[] = c.trunc();` so putting out one byte of immediate instead of two,
  so all the instructions later get decoded wrong, because variable width instructions are garbage.
- amd: found some only work with linker. `unexpectedly need to EmulateForward on a synchronous exception`, ive seen that before,
  odd way to spell stack overflow but go off i guess.
  do_fixups_amd64:RipDisp32 wasn't including increment when an offset got folded into the constant reference.
- amd: simple_addr wasn't always converting fast-allocs to RSlot. 7.
- amd: make old jit return result pointer in rax because new backend relies on it.
  added new test for that which llvm-aot fails but its because it optimises away the pointer comparison when the caller checks if it worked,
  it does however follow the abi correctly for the callee.
- amd: painful float negate
- arm: neg even though franca doesn't use it

## codesign (Nov 28)

ahahahaha i've defeated apple.
ive produced a binary signed so incorrectly that when you try to run it the computer freezes for ~20 seconds
and then the screen goes purple, it shuts down and restarts. garbage. i love it.

```
codesign -d -vvvvvvv a.out
a.out: code object is not signed at all
```

hmmmm, why your os crashing then friendo... me thinks thou dost protest too much.

```
objdump  --all-headers  a.out

a.out:	file format mach-o arm64
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/objdump: error: 'a.out': Invalid/Unsupported object file format
```

this thing can parse it...

```
/Users/luke/Downloads/llios-main/macho_parser/macho_parser --cd a.out
LC_CODE_SIGNATURE    cmdsize: 16     dataoff: 0x128210 (1212944)   datasize: 2497
SuperBlob: magic: CSMAGIC_EMBEDDED_SIGNATURE, length: 2496, count: 2
Blob 0: type: 0000000, offset: 28, magic: CSMAGIC_CODEDIRECTORY, length: 2460
  version      : 0x20400
  flags        : 0x20002
  hashOffset   : 92
  identOffset  : 88
  nSpecialSlots: 0
  nCodeSlots   : 74
  codeLimit    : 1215969
  hashSize     : 32
  hashType     : SHA256
  platform     : 0
  pageSize     : 16384
  identity     : foo
  CDHash       : Unavailable. Use 'build.sh --openssl' and run again.

  Slot[  0] : 29a5508ca386f6b14cca312d782f5504d78dd3acd26ae7a6ae4e9ab569ed3556
  Slot[  1] : 15ca388192bb891e2a8b6bcdd3c1249dca923824737513e53920f530cd666bfd
  Slot[  2] : 0409137b727cedb66f23b0fe974bf0caf2d8d343f70d93f2f8ae8f64942f2fed
  Slot[  3] : 6d6540fef7e20424468235786621f9b3287aa51b3b67ae51601eb8fb35c8fbd9
  Slot[  4] : a9f2009ecda80a18ec61a65356e65fab08139396338c3ca18d4bd33353d00e97
  Slot[  5] : 7b2ffc6c5bee282f59bf6961b25a4eec9b09dfcf76b400c46d7caaaabaa0b9aa
  Slot[  6] : d1ad3a340641ca99a94e26d80323625e17025254c7079da30d474af642d2f0df
  Slot[  7] : 681da381a6ef15659b872354acfae545814fd7a18166c31f6ec9cb0248ab81fd
  Slot[  8] : 0eb5903bf534fc4777184400dcae9f5dea819e657412b0261711c11c7409ca64
  Slot[  9] : a8ce78769ede445625c03e8a16d3ecd0a82588feb2337149a782c797e14cd171
      ... 64 more ...

Blob 1: type: 0x10000, offset: 2488, magic: CSMAGIC_BLOBWRAPPER, length: 8
```

maybe pageSize needs to be 4096?
no it doesn't matter, i just did the math wrong and was missing the hash for the last page or got the file size wrong and had extra hashes depending which mistake canceled out.
that seems like a slight over reaction.

pleasingly mine is faster than using `codesign` because i don't have to write out the file, exec something, have it read it, finally do the work, and then write it back again.
20ish ms that comes out of the 160 mysterious system part reported by time.

---

i think ive started triggering a thread safety problem more reliably.
hopefully its not something insane about code signing wrong and just one of the many shortcuts i took.
in the objdump diff between a good one and one that hangs, it starts with a bunch of just offsets being different,
and then the first real change is missing

```
< 100050814: 13805800    	ror	w0, w0, #22
< 100050818: d65f03c0    	ret
```

and then the infinite garbage of all the addresses on the left being different
(i should really remember to use --no-addresses, it just annoyed be cause it doesn't take out jump targets).

but anyway thats reassuring because i think its just because when you declare an asm function
it calls add_code_bytes on the frontend thread directly, which then fights with emit_func_arm64 over `m.segments[.Code]`.
yep, seems to be fixed by adding those to the queue.
I guess adding codesign stuff just make it more likely to happen because i added rotr/bswap that i haven't bothered to make intrinsics for yet.
Seems to have also fixed the rare reproducibility problem i noticed before, so thats's good.

just to be safe also delaying emitting constants until after rejoining thread instead of just near the end randomly.
but better would be doing them in the queue because then you wouldn't need as many fixups.
acutally it would have been fine until i started trying to use DataConst instead of only DataMutable becuase that's what GOT entries use.

---

TODO:  
llvm backend repro has regressed tho which is odd cause you'd think thats the easier one.
but i don't think "llvm" and "easy" have every been used together in a sentance not containing the word "not".

```
523217c523217
< @g821 = private unnamed_addr global [3 x i8] c"\C8D\B0"
---
> @g821 = private unnamed_addr global [3 x i8] c"\C8\04\DE"
523223c523223
< @g827 = private unnamed_addr global [3 x i8] c"\00\00\DA"
---
> @g827 = private unnamed_addr global [3 x i8] c"\00\00\92"
```

## (Nov 27)

- sha256 for codesign
- started stealing exports-trie from zig for dynamic libraries

## (Nov 26)

yikes, this was gonna suck when i got around to trying ctx=implicit.

```
// [snip]
mov rax, rdx  // env
// [snip]
mov rax, rdi  // callee
// [snip]
call rax
```

using arm compiler to run qbe-exe-x64

- fail 78
- lots of Safety Check Failed, comparisons messed up? fixed w=true on the setcc of xtest becuase you can't encode some registers that way. 73.
- w=false extub, 72

ahh i have reading comprehension problems. i saw:

```
1. In 64-bit mode, r/m8 can not be encoded to access the following byte registers if the REX prefix is used: AH, BH, CH, DH.
```

and interpreted that as "don't use the REX prefix when accessing a/b/c/d as a byte",
but it means the opposite: "if you don't use the REX prefix, you get bits 8-16 which is never what you want."
AH means bits 8-16 of RAX, AL means bits 0-8 of RAX.

better phrasing elsewhere:

```
1: When any REX prefix is used, SPL, BPL, SIL and DIL are used. Otherwise, without any REX prefix AH, CH, DH and BH are used.
```

- fail 17

---

Running the backend on its own thread goes from (1850-implicit or 1780-static) to 1390-implicit (all numbers unsafe).
Profiler says ~25% of the time was in the backend so that's pretty close to purely parallel. We only stall waiting on the backend 11 times when self compiling.
dynamic context pays for itself now (unless i do thread locals but maybe they're slow too, who knows, probably not cause i wouldn't have to generate code for saving the register).
Notably, because of how the queuing works, threaded builds are (mostly?) reproducible.
would get harder if i wanted more backend threads.
which is funny because single threaded builds didn't reproduce recently and i just now revealed problem was not calling push_dynamic_context enough.
sadly they're not reproducible between threaded and not threaded (both building the same program).
can print names in codegen and they're doing things in the same order tho, so not sure what the difference is.
seems threaded occasionally doesn't repro itself tho, perhaps i spoke too soon, farther investigation required.

now the flat 90ms spent loading the driver before getting to the threaded part looks more attackable,
so i could bring back loading from dylib but that means i have to make my own dylibs.
damn hard to pick which is more soul sucking between mach-o and x64 encoding.

---

- rex prefix for long sign extend beforev division. 12

## (Nov 23)

- static dynamic_context on llvm (oxymoronic)
- `#log_ir("RC")` so you can see ir without it being super spammy.
- apple_extsb only copy blocks if something changed (like simplify.fr does)
- panic handler use context so different threads can have thier own
- treat extuw after loaduw as a copy. can't always do it because you're allowed to use extuw to clear the high bits.
  maybe loaduw should always output a long?
- in spill: `dead := pass.f.tmp[t].slot == -1 && !b.out&.bshas(t);`
  // my fuse_addressing in arm isel doesn't update usage counts correctly so i end up with a lot of dead pointer arithmetic.
  // rega would remove the dead code anyway but we can skip doing some work and reduce false register pressure.
  // A less hacky solution would be to fix it in fuse_addressing instead. that would prevent us from incorrectly thinking the args are live until getting here.
  // saves ~70ms. This happens 150k times with fuse_addressing on and 6k times with it off which mostly reassures me that i understand how we get here. -- Nov 23
- lock buckets while you access symbol info to prep for threads.
- less CStr when working with symbols
- list of functions with seperate arenas so threads can pass them around.
- clear `tmp.use` before going in inline cache so you don't try to reuse memory if it gets suspended.

##

before i did some manual outlining in list and context stuff, the worst the time got was around 2250.

## (Nov 18)

isel mistakes:

- initial port, 20
- emit() call ordering for float immediates, 17
- safety check getting index past the end to do bounds checks in runmatch, 9
- hacked around miscomplication because of max_homogeneous_tuple, 3

## (Nov 14)

- relative addressing is from the start of the next instruction but i was getting the current location before encoding the displacement.
- xtest needs extra swap like cmp. need to figure out whats going on there.
- conaddr.ssa test wants to encode an absolute address in sib. TODO: does this ever happen in a non-contrived situation?
- for stubs to got, jmp not call because you don't re-push addr (its like b without l),
  and i keep forgeting that offsets are from the end of the instruction not the start of the patch like they are on arm.

could do such a cool steganography thing with the redundant instruction encodings. surely someone has that already.

## (Nov 12)

- made sequential enum use the right value size based on its declared type so const_eval_any doesn't fail debug check.
- start of ir parser so i can run tests without LINK_QBE_C
- (minimal, 58), (loadw alas + predeclare blocks + jumps, 54), (phi, 47), (header par, 35), (fix off by one on lines, 33),
  (call arg, 29), (blit, 27), (float literals, 25), (data, 21), (vararg, 18), (env, thread, fix export\n, fix cstr, 12)

## (Nov 10)

- spent so long on typoing `Indirect01 = 0b10`

## peaceful cleanup (Nov 6)

a little strange that these generate different code.

```
fn is_load(j: Qbe.O) bool =
    @is(j, .loadsb, .loadub, .loadsh, .loaduh, .loadsw, .loaduw, .load);

fn is_load2(j: Qbe.O) bool =
    j.raw() >= Qbe.O.loadsb.raw() && j.raw() <= Qbe.O.load.raw();
```

the latter has extra sxtw's (and some are on constants becuase I'm not running the fold pass),
because i don't use 32-bit cmp because I didn't want to deal with it on the old backend.
In the @is_op i extend the constants to i64 at comptime.

- move inlining before memory opts just incase it can delete anything extra.
  would help a lot more if i inlined things with aggragate arg/ret because thier abi inserts a bunch of junk copies.
- move some @debug out of lambdas in emit
- combine is_store+size_of_store (or load) by returning an option
- alignment padding between 4 and 8 byte local stack slots to help fuse_addressing.
- sprinkle #inline fairy dust on functions that look dumb in the disassembly.
  I feel its always a win if it actually ends up making to code smaller.
- use @is in the frontend
- generate lookup table for argcls at comptime
- go through some old code and use operator syntax sugar
- Trunc64To32 becomes copy:Kw instead of extuw:Kl

nets code size ~1260KB -> 1188KB, but the main thing is it's nicer to look at now.

## the line must be drawn here (Nov 4/5)

- versions of the compiler compiled by llvm.fr, from_bc.fr with my qbe, and qbe.fr with real qbe, compiling with my qbe all produce the same binary when they work
- versions compiled by either qbe have the bug but the llvm one (-Os and -O0) does not.
- often it works. if not, mostly segfault but sometimes its LexError.Unexpected.
  which suggests to me that im stomping random memory and sometimes it happens to be fine or unmapped or in the source code itself.
- it can't be use after arena_reset because that would never be unmapped.
  unless its using junk as a pointer. easy to check, still happens if I disable
  reset_retaining_capacity/reset_retaining_original/deinit.
  still happens if i disable dealloc so its not a normal use after free either.
- i can try address sanitizer on the llvm one, maybe there's some logic bug in the frontend part that just doesnt really manifest somehow, idk, cant hurt.
  need -fsanitize=address to clang (both compile and link) and also sanitize_address addtribute in llvm ir.
- asan thinks im calling memcpy with overlapping args, im not calling it from my code, its inst_call for CopyBytesToFrom uses llvm's memcpy intrinsic.
  I can use thier memmove instead and then asan stops complaining. that's promising.
  but making my qbe call memmove for every blit still has the problem. sad.
- i suppose asan might not be doing much because i mostly use my own areana allocators instead of malloc.
  making get_alloc use todo_allocator doesn't change anything.
  (tho it does make a compile error unless i change @static to zero the memory which is offputting but easy to fix and clearly not related to the main problem).
  so thats kinda a dead end.
- ok new tact. write a simplier version of rega and skip as many opt passes as possible and see if that still has the problem.
  just give every tmp its own stack slot and only use 3 registers ever.
  - special case to ignore copies to null that the other rega needs as markers after a call.
  - mistakes around phi nodes.
  - why does store argcls need to be Kw?
  - swap needs to be a special case but is only inserted by other rega so don't care
  - fold1 fails without constant folding (but it does that with normal rega too)
- ok so this is good. minimal_rega+isel+abi+convert_to_ssa passes tests and can compile the compiler, but doesn't seem to have the bug.
  or im just getting impatient because its slow as fuck.
  but it got through 1000 compiles of the empty program without crashing (before i got bored because that took almost 8 minutes).
- ok so what if i add back other passes? same as normal but this minimal_rega instead of spill+rega.
  that also seems to not have the bug. so i guess its some subtle bug in qbe's spill or rega that I faithfully ported.
  thats kinda painful.

oh hey i even noticed this before.
`// TODO : qbe uses x18 for this but i feel like you're not allowed to do that. :SketchPlatformRegister`
whelp todays the day we todo that todo. progress.

by far the most common thing that used the scratch register was swaps inserted by rega.
and i can see how randomly when you try to swap registers and one gets zeroed it could manifest as seeming to take the wrong branch or just segfaulting.
also suggests i wasnt going crazy when it happened way more often when trying to run in profiler, because maybe thats a context switch to take a sample?
so the solution to all that was just to use x17 as my scratch register instead of x18.

minimal reproduction without involving qbe at all:

```c
#include <stdio.h>
#define C 10000

int main() {
  int stomps = 0;
  for (int i = 0; i < C; i++) {
    long x = 1;
    for (int k = 0; k < C; k++) {
      asm volatile("b .bbb\n"
                   ".aaa:\n"
                   "    ret\n"
                   ".bbb:\n"
                   "    mov x18, %0\n"
                   "    bl .aaa\n"
                   "    mov %0, x18\n"
                   : "=r"(x)
                   : "r"(x));
      if (x != 1) {
        stomps++;
      }
    }
  }
  printf("stomped %d/%d\n", stomps, C * C);
  return 0;
}
```

for posterity this is on an m1 with macos 14.6.1.

## (Nov 3)

hoping the compiler constants thing also shows up in one of the tests.
setup script to diff them so its less painful.
yes! basic_libc:catch_signal does it alone, thats much more managable.
same symptoms: identical asm except for adrp+add offsets. one extra data symbol.
it's like the `handled_a_signal :: @static(bool) false;` is happening twice in the qbe version.
I guess one is IS_PANICKING and thats why it doesn't happen when i don't print something too.
add_constant is only called in walk_bc (not c/b/llvm or b/from_bc) and it happens for both llvm and qbe so its miscompiling the frontend.
oh its the deduplicating by jit_addr in emit_relocatable_constant so when its less than 8 bytes and stored inline, its deduplicating by stack slot,
and the one compiled by llvm happened to use stack space such that they were at the same place for both constants and it deduplicated them.
a bit alarming that it wasn't a problem for months but anyway we're back to reproducible builds of the compiler so thats good for morale.

oh hey i even noticed this problem before (macro_in_const_cloned test)
`// TODO: this doesn't work on llvm, presumably because bake value does the wrong thing? : FUCKED -- Jul 10`

the new backend still doesn't fully work, the occasional crashes were unrelated apparently, but at least now we know its not a
confusing miscompliation that only surfaces after multiple iterations, just a normal friendly miscompilation.

- typing f.rpo.offset(i)[] is gonna drive me insane, same for f.pred.
- with using init_default_module_dyn you finally get a speed up from fuse_addressing, around 2.45s -> 2.25s.  
  looking good for the idea of only adding optimisations that make it compile itself faster including the extra code than the old one did without.
- another peephole isel improvement: using madd saves ~10KB of instructions.
  not sure its worth it speed wise.

## (Nov 2)

farm_game is also different exes depending if using lq or lqq.
hoping thats the same problem as the compiler but don't thing so because this changes code and i think compiler is just different in constants.

```
3f30: d2800001     	mov	x1, #0
3f34: f2d33321     	movk	x1, #39321, lsl #32
3f38: f2e7f921     	movk	x1, #16329, lsl #48
3f3c: 1e270021     	fmov	s1, w1
```

vs

```
3f30: d2800001     	mov	x1, #0
3f34: f2c00021     	movk	x1, #1, lsl #32
3f38: 1e270021     	fmov	s1, w1
```

so its miscompiling emit:loadcon? no n is just different going in,
but the same in from_bc:inst_literal. so lost somewhere in the middle.
no, its different at the very beginning (`-d P`):

```

args 4596373777117347840
args 4596373778182701056
call $sdtx_pos__1050
```

vs

```
call $sdtx_color3f__1058
args 4294967296
args 5360320512
```

was because of setting f32 union field in push_literal and then reading back an i64 because when emitting i just care about the bytes.
that was interesting because it did make different binaries but both were correct compilations of the program.
because the difference was just wastefully loading the high bits of a register that you were about to truncate anyway,
so it could never be observed.

should decide what that should do.
maybe x = (i = 123) should zero the other fields but x.i = 123 should leave them unchanged?

## (Oct 31)

made the arm isel for the add instruction use an immediate when the right hand side is constant that fits in 12 bits
(instead of an extra register).
this alone makes the compiler go from 2,485k -> 2,089k bytes of code. thats insane.
runtime (safe) goes 4.52 -> 4.13 (but again some of that is just because the new version does less work, unfortunate that my only big program is the compiler itself.
it looks branchy but it generates so much less code that its faster even before accounting for it generating better code for itself.

in testing that, found confusing behaviour where it updates to my changes
without recompiling even when default_driver is using emit_qbe_included_dyn
instead of emit_qbe_included_sta so it should be using the version in the compiler.
oh! its because I'm setting function pointers in the target vtable in fill_target_arm
which gets called from the module initilizer in the driver program instead of within the precompiled part.
which is fantastic news because it also explains why there was a chunk of the backend code in the profiler that didn't have symbol names
(because they were getting jitted every time) and that chunk is ~11% of the total time which perhaps is about to get 5x faster by precompiling correctly.

using immediates for ldr goes 2,089k -> 1,737 and using for str goes to 1,407k.

those numbers are all before including vtable.init_default_qbe_module.

## (Oct 30)

Found someone on the internet to tell me how Mach-O does thread locals: https://stackoverflow.com/a/75297331

So debugging why it works when I let clang handle the offsets instead of doing everything myself.

```

_xaddr:
.word 3573752927
.word 2847898621
.word 2432697341 ; 910003FD
adrp x0, _x@tlvppage
ldr x0, [x0, _x@tlvppageoff]
.word 4181721089 ; F9400001
.word 3594453024
.word 2831252477
.word 3596551104

```

assembles to this:

```

0000000100003ebc <_xaddr>:
100003ebc: d503245f bti c
100003ec0: a9bf7bfd stp x29, x30, [sp, #-16]!
100003ec4: 910003fd mov x29, sp
100003ec8: b0000020 adrp x0, 0x100008000 <_xaddr+0x20>
100003ecc: 9100c000 add x0, x0, #48
100003ed0: f9400001 ldr x1, [x0]
100003ed4: d63f0020 blr x1
100003ed8: a8c17bfd ldp x29, x30, [sp], #16
100003edc: d65f03c0 ret

```

the important thing is that (3 words, adrp, ldr, 4 words) assembled to (3 words, adrp, add, 4 words).
becuase we just know that the `@tlv` means do something totally different from what the instructions say.
So my blindly doing the instruction encoding for ldr when its a thread local was wrong.
So when qbe outputs assembler text, it has a special case to use ldr instead of add if its a thread local,
and then the assembler has a special case to treat ldr as add if a thread local.
this all seems very sane and normal to me.

That's enough to pass the one qbe thread local test when outputting exe. still have to deal with relocatable obj.

## (Oct 28)

- `farm_game` doesn't work with new backend. asserts on `_start_canary` being nonzero after calling sglue_environment.
  i can't recreate the problem with just my language so its either we disagree with clang about size of the struct or its a calling convention thing somehow.
  oh heck, thats crippling. in from_bc:emit_bounce_fn i was treating the indirect ret addr as first arg (link it is inside a bc block),
  but it actually has to be a blit to a qbe return instruction. so it was just using whatever junk happened to be in x0 instead of x8,
  which i was the address of the whole SgDesc.
  but on the plus side the compiler now works on new backend now!
  at least for hello world.
- ok this is getting confusing. we need some notation.
  f_l = the normal one compiled by llvm.
  f_lq = compiled by f_l with the new backend
  f_lqq = compiled by f_lq with the new backend
  f_lql = compiled by f_lq with llvm
- the take away i think is that the new backend cannot compile itself but it can compile the old backend.
  but luckily for me the broken f_lq also fails tests. ie. `push_it`.
  so now i have two binaries for that test that should be the same and one segfaults, and i can diff them?
  thats creepy.
  the new one does

  - `cmp XX, #0, lsl #12` instead of `cmp XX, #0` for cbz
  - `mov XX, #YY; add	XX, x29, XX, uxtx` instead of `add XX, x29, #YY` for addr of slot

  which is interesting because f_lq is clearly broken since its different,
  but those instruction sequences do the same thing.
  ok thats still too big tho lets try something smaller.
  only one of the qbe tests fails with the broken compiler.
  oh shit which one changes tho. was collatz then rega1.
  so we've produced UB. thats fun.
  cry.
  `c.bits.i < 1.shift_left(12) - 1` is not at all the same thing as
  `c.bits.i.bit_and(1.shift_left(12) - 1) == c.bits.i`. (off by one. negatives are handled on different path anyway).
  and that was the only problem.

## emit mach-o reloc (Oct 27)

- string escapes, 1
  and that last failing qbe test is for thread locals which i haven't got to yet.
- pretty embarassing that in run_tests for qbe-exe i forgot to actually run the resulting binary...
  but they did still all pass so thats a good sign for the qbe tests being comprehensive at least.
- the my language tests are all exes that don't link something else,
  but i can still have them output a relocatable object and link it against nothing to test that i get the relocations right.
  failing 250/287.
- DataAbsolute relocs and track when data contains a local pointer, 52.
- add to local_needs_reloc in do_jit_fixups so you handle when code references data, 34.
- for data pointer reloc, the value in the data is added to the symbol address so it needs to be 0. all run_tests pass!
  that took so long to figure out.

## emit mach-o reloc (Oct 26)

- for relocatable object you can't have headers included in `__TEXT`?
- `ld: Assertion failed: (addr >= lastAddr && "function addresses not sorted"), function FunctionStarts`
  means your `__text` offset was high like it has to be when you include the load commands.
- single func relocatable. fail 28/58
- use correct offset, 23
- adrp+add import reloc, 13
- call reloc, 10
- skip `__got` data relocs because you just emit for each use instead, 8
- data segment and reloc when code references data, 5

## (Oct 25)

- print the most negative number
  - oh hey i even noticed this before `// assert_eq(MAX_i64 + 1, MIN_i64); // TODO : llvm backend chokes on this.`
- emit constant of a null CStr
- switch on u32
- aaa global_module is a different variable between the precompiled and the normal.

## emit mach-o exe (Oct 24)

- i was hoping i could use `https://lief.re/doc/latest/formats/macho/python.html`
  to start with a working exe and remove parts until it broke.
  but just doing nothing:

```

import lief
import sys
path = sys.argv[1]
app = lief.parse(path)
app.write(path)

```

breaks the exe. so either im stupid or they're stupid but either way it doesn't help me.
it seems they unset the N_EXT bit in LC_SYMTAB entries?
ahaaaa im dumb! its because they're signed so i have to run codesign -s on it after changing any bytes.
which means by previous experiments with editing the flags of a section to be zero and that breaking the exe,
didn't mean i need to have sections, it just means changing bytes invalidates the signeture.
codesign still isn't enough to make my program work tho.
`codesign -s "Luke Graham Landry" hello.o -f`
i hate this so much.
i know more than i ever have before. not a meaningful statement but makes me feel better. its fun because its always true.

// TODO: real ones have the file offset of `__TEXT` being 0 so it contains all the load commands too?
// and then section `__text` is just the code. do we need to do that? (we dont now). why would that be the case?
// ooooo maybe its becuase you want to declare a symbol `__mh_execute_header` so your program can
// reference that and read its own exe file like for debug info, etc.
ok so doing that ^ (without `__mh_execute_header`, just making `__TEXT` have file_offset 0 and include commands),
made it go from `killed` to `main executable failed strict validation` which is slightly better i guess.
and that was just becuase i was still doing the add_data_file_offset thing and redundantly adding the size of commands again
to all the offsets but now since its in TEXT its accounted for automatically. so thats an improvement i guess.
now its `bus error`. which suggests that its actually running.
yeah, i can run it in lldb now and it crashes trying to load from `__got` in a stub.

aaaaaa problem was you're not allowed to have a segment's address_size different from size???
that can't be true but it sure breaks when i change that.
maybe you're supposed to explicitly have a section that says the rest is zero filled?
no that doesnt work.
clearly im wrong because linkedit doesn't have page multiple size and doesn't have size==vmsize.

ok one mystery solved.
chained_starts_in_offset.segment_offset is actually the offset in virtual memory from the first intersting segment (end of `__PAGEZERO`).
before i thought it was the offset in the file.
its just a fun coincidence that normal binaries made by clang have thier segments packed together in virtual memory so those numbers are the same.
and this dump to text thingy (which i was using because i couldn't get objdump to give me the information i want)
`https://github.com/qyang-nj/llios/blob/main/macho_parser/sources/chained_fixups.cpp#L126` gets it wrong too,
and segfaults if you have your sections spaced out (unless you use segment_offset incorrectly).
however, `llvm-objdump --chained-fixups --macho` doesn't bother tryingto give you that information and doesn't crash
(it just tells you the imports table and shows you the page_starts without showing the rest of the chain).
oh and look at that they even have a comment with the information i needed: `https://github.com/llvm/llvm-project/blob/main/llvm/include/llvm/BinaryFormat/MachO.h#L1086`
if only i saw that 8 hours ago.

so recap of things we learned:

- `__TEXT` is required to have file offset zero (even if you don't define/reference `__mh_execute_header`)
- you need to run codesign.
- you cant have segment virtual size greater than file size unless file size is zero. you can space things out with gaps in between tho.
- segment size needs to be a multiple of a page, except for linkedit?
- you don't need sections except to make objdump disassemble the right places.
- you don't need LC_SYMTAB, LC_DYSYMTAB, UUID, min os version

```
import lief
import sys
path = sys.argv[1]
app = lief.parse(path)

import os

app.remove_symbol("__mh_execute_header")
app.remove_symbol("_main")
app.remove_command(5) # exports trie
app.remove_command(8) # uuid
app.remove_section(segname = "__TEXT", secname = "__unwind_info")
app.remove_section(segname = "__TEXT", secname = "__cstring")
app.remove_section(segname = "__TEXT", secname = "__stubs")
app.remove_section(segname = "__TEXT", secname = "__text")
app.remove_section(segname = "__DATA_CONST", secname = "__got")
app.remove_command(12) # function starts
app.remove_command(8) # build version
app.remove_command(8) # source version
app.remove_command(10) # data in code
app.remove_command(5) # LC_DYSYMTAB
app.remove_command(5) # LC_SYMTAB


app.write(path)
print("replaced exe.")
os.system('codesign -s - "' + path + '" -f')
```

## (Oct 23)

- damn. solved that in like 30 seconds this morning.
  each bl to an import was a different place. each getting thier own stub.
  that wasn't actually the problem yet but it would have been confusing.
  mistake was in init, i was make_segment in the wrong order (constant and mutable data flipped).
  so that makes mandelbrot work.
- for f_vtable, needed to fix rebase. target was using file offset to patch instead of virtual offset to target.
- cant run normally tho. killed.
  lldb helpfully tells me `error: Malformed Mach-o file`.
  i tried to open it in xcode's debuger incase thats different and can't even select the file which is funny. it knows its broken.

## mach-o loader (Oct 22)

is `__got` guaranteed have sequential chained fixups for all the imports?
maybe you're supposed to use that instead of doing the hacky some_extra_memory thing.
but if that were the case, you'd think they'd pre-setup `__stubs` to load from there instead of
needing to patch them which i think you do unless im misinterpreting.
oh yeah, i just got confused by the output of objdump.
example:

```

000000010000be2c <**stubs>:
10000be2c: b0000010 adrp x16, 0x10000c000 <**stubs+0x4>
10000be30: f9400210 ldr x16, [x16]
10000be34: d61f0200 br x16

```

it says its to `__stubs` but 0x10000c000 is the address of `__got` in that binary.

so now the loader it good enough to run itself and the compiler.

## mach-o loader (Oct 21)

- since im doing import patch in place before moving, the offset i calculated for adrp is wrong.
  then also had to make sure the fns array was close enough to the code that it could reference that far so mmap them together.
- whatever version of objdump is on my computer, -r doesn't show the entries in ChainedStartsInSegment.
  and i cant find an argument that does. `objdump --chained-fixups --macho` sure doesn't.
  i can only assume their goal is to make life as confusing as possible to reduce the competition.
  ah different program for some reason: `otool -fixup_chains`,
  or `dyld_info -fixups` also works.

## (Oct 20)

- caching the `primitives` slices doesn't make it faster so don't bother.

## (Oct 16)

- emit: for extuw, need to `mov	w0, w0` (with Bits.W32) to clear the top bits. even tho the class of the instruction is Kl because thats the output.

## (Oct 15)

- stop being lazy and make a macro to make else-if chains less painful.

## (Oct 14)

- improve tracy zones so you can actually see the dependency hierarchy.
  previously it was just flat because of how i unroll the recursion.
- add comptime resolve_overload and use it in format_into. 435->410.
- don't care enough to figure out what's making cranelift flaky.
  My own x64 supports the whole language now so i don't rely on it anymore.
  And somehow it's 730ms to run c_bindgen vs 530 for my old unoptimising and 120 for new qbe.
  so there's no point if its worse than doing nothing. (might just be heavily punished by not inlining anything but my old one doesn't either so that seems fair).
  tho calling franca_comptime_cranelift_flush less often gets it to 650ms so maybe im just forcing a lot of indirect jumps and being unfair.
  but thats still slower. i want to like it but it seems more trouble than its worth for me.

## (Oct 13)

- so long on wrong sp encoding for epiloge with large stack frame.
  should have been obvious because it was bus-error-ing between the last line of a function and the next line in the caller.
  (still cant run the frontend on new backend but it gets farther)
- get backend to compile itself. had to fix #redirects of i64->i32 because it treats those as different types.
- made walk_bc do callees first so it can inline intrinsics more reliably (and qbe gets things in a better order).
  that gets c_bindgen to 120ms. so thats like 30% slower than llvm, thats pretty damn good. with -unsafe tho llvm goes 90->40 but mine only gets to 110.
  so still a long way to go. maybe inlining things with branches would make a big difference. if just single block functions was a 115% improvement.
  presumably im also generating pathologically stupid but slightly bigger functions.

- renumber doesn't work if you have out of order constants because it just assumes they're from an upper scope.

## (Oct 12)

- can trivially recreate the c_bindgen problem with enum_names_constant.
  but it works if i use old emit_qbe to make the text and then parse that so its not my emit asm.
  it's the way i produce the constants in from_bc.
  unlike the other baked constant tests it still fails when run one at a time.
  ugh, in AddrOf i was putting its own id instead of the target id.
- the parser_doesnt_crash safety check fail is during compilation.
  in emit_func_arm64 on parse_block_until_squiggle.
  oh interesting, it's just more code for that test than fits in my 256k segment.
- using adrp for stuff and bigger segments also fixed the broken constants tests.
  so i was probably fucking up the offseting math with adr and in my too many hours fixing it got it right.
- did patching instead of the indirect table. its a lot faster (only noticable without inlining).
  trampolines if its too far because its imported from a dylib, which nicely matches what you have to do with \_\_stubs for aot.
  its just a bit of a pain for jitting becuase now that has to be executable so you can't just poke an address in like with the data table.

## (Oct 11)

- i think my adding indirection for direct calls really hurt. `c_bindgen sokol` on mine is 330ms instead of 260 with real qbe.
  but adding inlining of tiny functions gets it to 200ms on both. which supports the theory that i added call overhead.
- had to add RType as a pass through getinlref for when you inline something that contains another aggragate call.
- inlining fixes walk_dir and breaks bus_error_baking_constant/baked_constant_tagged_padding.
- TODO: mine messes up string constants. the word struct in c_bindgen is random gargbage.
  but that happened without inlining and doesn't for real qbe so its a problem with my asm emit.
  but perhaps thats also causing the new breaks in tests related to constants and the inlining just revealed a problem there.

## (Oct 9)

- ported test runner script
- working on inserting casts for llvm backend
-

##

- side tangent of i'm stupid. lets play can you spot the mistake...

```

fn println(i: i64) void = {
mem: List(u8) = list(i.div(10).abs().add(2), temp());
i.display(mem&);
mem&.push_all("\n");
print(mem&.items());
}

```

ok we're doing some printf(A) driven development.
that's where i decide the resolve_overload loops are too slow so i put a println("a") in there and count the lines, and then change something.
its like a sampling profiler but instead of being good it's inconvient and slow as fuck.
note also that I have to redirect the output to a file because my terminal clamps the output to 100k lines.

- when i infer_type on each arg of the pattern i wasn't savingthe finished tpye even if all were successful.
  so if an overload was never taken i'd try to reinfer it every single time you look in that overload set.
  so now save it in finished_arg if all were known.
  before 5000k, after: 23k. amazing. 470ms -> 440ms
- comptime zeroed() gets to ~430ms but can't do that until i can do float comptime calls more reliably.
  ah but i can cheat and do it by pointer.
  saved half the samples in coerce_const_expr (in real profiler)

##

- so long on a `:=` vs `=` in a loop. perhaps i was wrong.

## (Oct 6)

- pretty ironic to not be able to incrementally port abi.c because functions pass structs by value and my old backend doesn't follow the abi correctly.

##

- setup default_driver test and its always failing the third one it tries.
  heck it was just getting confused by compiling twice becuase i pasted wrong in run_tests, 15.
  and then i can just recreate the module every test in default_driver too for now and that works.
  and after that i can add back the redeclared symbol check and it doesn't fire.
- from_bc gives me the inline asm, 8. and that's just as many as passed with aot.
  but in 4 seconds instead of 11 seconds. there's gotta be something wrong with clang's assembler.
  (and thats still with the new backend running jitted on the old unoptimising backend so it should get faster with release mode).
  ~200ms of that was emitting the unneeded asm text.
- fixed minimal_inline_bc not setting arg_prims on its block
- the loop cost stuff is creepy because the program still works if you get it wrong. its purely an optimisation.
  so finally did shitty padding for formatting so i can make exactly the same output as qbe when it does `%s-10` and check it that way.
- spill mistakes:
  - typoed extra & of a pointer passed to a function and apparently i don't typecheck that sometimes, thats a massive problem.
  - wrongly redeclared a variable
  - translated i++ to i+1
    // bf_interp fails with mine! tho not different answers. it segfaults in the compiler.
    // and thats the only one. so thats strongly in favour of going and finding more tests.
    // and not getting rid of the old backend, just turning it into an example so i have it as an extra test because its such a large proportion of the code i have.
  - problem was le vs lt in sort.

## (Oct 2)

- you can't ask the linker to adrp you to a dynamic import? that makes sense, the loader puts it somewhere random and you have to look it up in a table.
- had adr encoding high low flipped
- ok that worked much better than expected. franca tests are failing 57/289. and i can still run in text mode that mostly works.
- put jit addr for constants in the table, 45
- started doing emit constant properly and poking in references, 33

## (Oct 1)

- (big imm, cset, 37), (float ldr/str, 35), (slot ldr/str, 34)
- off by one in big imm loop, 31
- much pain with sp encoding, 29
- ugh stupid mistake where i patches the asm bytes in memory but for im still emitting the text unbuffered so all my oforward jumps were brk, 24
- (rem/urem, 21), (swap, 19), (int ext, 16)
- aaahhh im so dumb. jumps were randomly off by 8 or 16 bytes sometimes because i was just emitting text for calls so the offsets in my "jit" code were off, 10.
- sometimes objdump is ass and you have to say -D instead of -d? what? and they're the same, why doesn't mine work.
  ohhh its ones that happened to have a format string data before the code and i wasn't putting out a `.text` directive after so it switched me to data,
  and then objdump throught my code wasn't something it should try to disassemble, and the test failed because it wasn'tmarked executable.
  ok thats fair, my bad ig, 7.
  oooo! and thats also what was making most of my franca tests fail (cause main was at the end after data) so now thats 255 -> 27 too. amazing.
- painful float cc, 5 + 27.
- cast=fmov, 2 + 23
- a billon d/s<->l/w s/u, 1 + 12
- fixed emit ordering for Thr fixaddr isel, 0 + 12
- sltof, 0 + 10
- 0 + 8
- filluse first try.

## (Sep 30)

- intermediate step so i can still use thier test scripts: do the asm myself but emit it as `.word xxxx` for an external assembler.
  also lets me delay dealing with patching calls which is nice.
- flipped a few of my encodings when the arg registers are interchangeable because its easier if the disassembly of mine matches what real qbe does.
- (math/load int/copy, 50), (get global addr, 47), (load args backwards, 46), (call, 45), (cond jmp, 41)

## (Sep 29)

- TODO: fix opsies when trying to match on a non-sequentual enum. fails safety check
- pass: instruction selection
- wasted so much time on a flipped condition in `imm`
- implemented sign extension properly on my old arm backend becuase i need it for interacting with qbe's 32 bit integers.
- added ability to run qbe's tests and caught blit mistake related to ^

##

- off putting that i accidently flipped the conditions in simpl div

## (Sep 26)

- store has no result type so the instruction must have .Kw
- forgot to set nunion to 1 and it caused a 0 opcode somewhere. took so long to figure out but can't really complain, like yeah thats my bad, don't do that.
- i was still using qbe parsing for TestRunnerMain so at the end, it would free my `typ` and thats why only the first test case would work every time.
- a bunch of `error: assembler local symbol 'Lfp0' not defined` because i wasn't calling `target.emitfin` for it to output float constants it made in the backend.
- insane behaviour where it looped forever (at runtime) if you tell it store returns a value instead of using QbeUndef.
  which again fair enough i guess. my builder should warn you about that. im surprised typecheck doesn't.
- made TestRunnerMain return 0 which made more pass. i guess many tests just happened to leave 0 in x0.

## (Sep 25)

- my c_bindgen emits both fields as the whole backing type for bit fields. that was confusing. TODO: fix // :BitField

## (Sep 24)

- so like its gotta be https://github.com/llvm/llvm-project/blob/3fbf6f8bb183ad8b9157e50c442479f4ca7a9b8d/llvm/lib/MC/MCDwarf.cpp#L630
  and clion warn try to manually select a file that the old name was "" so that matches.
  but where the fuck is it getting that from. the empty string does not appear in my ir.
  clang sets `source_filename` at the top level which i don't but that doesn't fix it.
  At least i can use `objdump -d -l a.out` to check if its working instead of running it in lldb every time.
  ah problem was my DISubProgram didn't have `file` field... so whats the scope field for then??? because thats also a file.
  TODO: write a test with that objdump thing that you still get debug info.
- the compiler works on qbe which is pleasing. still useless because clang assembling qbe's output takes so long,
  but its really nice to not rely on llvm as our only way of doing an AOT build.
  (runtime of output code) qbe is 5x slower than llvm release mode.
  oh shit i take back everything bad i ever said about qbe.
  targetting x64 clang can assemble its output in under 380 ms.
  compared to arm where it takes 3900 ms which i was complaining about.
  assembly size: x64=5.3MB, arm=8.2. so like its bigger but its not 10x bigger!
  what could clang possibly be doing.
  so for x64, qbe+assemble takes 30% longer than llvm debug mode but runtime is twice as fast.
  so thats like a totally reasonable option.
  I should really make a table of times.
- considering hooking into qbe and replacing the parser with just generating thier in memory data structures,
  and the assembly emit with my own machine code emit. i find thier code incomprehensible so want to understand it,
  working on making c_bindgen not choke on thier anon enum struct fields.

## (Sep 22)

- made most of the wasm tests work again, at some point it started generating calls to memcpy. hack: just had rt provide it.
  it must have been the change from calling memcpy to defining one the takes advantage of alignment but you'd think that would have the opposite
  i guess i don't know what `@llvm.memcpy.p0.p0.i32` does.
- (bf2bc) fixed the arm encoding problem when IncPtrBytes is negative so now i can disassemble it but it still infinite loops
- ah printing something in the loop changes its behaviour so it must be a register allocation bug?

## (Sep 21)

- always use shims for jitted function pointers.
- added a new type of inline asm comptime function where you just fill in my bytecode format.

bf2bc

- needing the loads/stores for phis is annoying
- to work on llvm i needed the entry block to have the right arg types
- llvm cares about i64 vs i8
- llvm doesnt jsut compare to 0 it wants 0 or 1 exactly.
- works on x86/llvm/qbe but not arm

## (Sep 20)

out of ~500ms, resolve overloads of `if`, `eq`, and `display` are each 50ms.
so the root problem is my linear scan overloads thing, but can probably make it better by not instantiating those
inside generics as much.
I got rid of `::if_opt(T, bool); ::if_opt(T, T);` in fn Option and now if takes `25ms`.
So like, its dumb to create a situation where im deciding how to write code to make it compile faster,
but also its so easy to make the standard library stuff not pessimize speed so maybe thats worth it.
The `display` is probably becuase of ::enum which i use a lot but don't print by super often.
I bet `eq` would get a lot better if DeriveEq could create the sub-functions it needs for fields without
adding them to the list you scan every time you type `==`, because there's a bunch where i only manually call the top of the hiarchy.
Really i just want to be able to have generics that don't add to overload sets until you need it.

- eek! noticed i had added get_build_options to the vtable twice. add error for conflicting field names.
  same for function args because it feels weird to just have the later one shadow.
  TODO: but for function args i should allow multiple `*` if you just want to discard.
- shims for better errors when missing libc

## (Sep 19)

- update qbe backend, implement switch, make walk_bc usable from driver, enable -aot=qbe in default_driver.
- fixed stupid typo in flags for open_append/open_trunc
- actually show lex error reason
- implemented stack arguments on x64 when you run out of registers (>6 ints).
  i still don't actually implement the c abi correctly because you're supposed to pass structs in xmm registers sometimes.
  also since then some of the functions have progressed to having >8 args which the aarch64 backend can't handle.
  maybe i should just make you use a struct. or maybe slices should be passed by pointer if there's too many.
- fixed silent exit(1) if you hit an error at top level of the driver.
  TODO: figure out how to print more info since you don't have the compiler instance at the point where you decide you want to panic on it.
- make resolving by type without an expression less insane.

## (Sep 18)

- added syntax for short-circuiting `or` because its dumb to only have `and`.
  kinda nice that you can use it like zig's `orelse` operator.
- fused a few operations in sema so its less clunky to interact with values of known type.
- started trying to call get_or_create_type everywhere so i can get rid of tagged indices
  because it spiritually annoys me that i have to encode larger immediates all the time,
  and im pretty sure its never caught a bug since before i got rid of the interpreter.
  can't quite do it yet. maybe im missing some but also im a bit afraid of my hashing Values by bytes.
  did get rid of tags for scope/overloadset/funcid tho.
  there's also some `==` that don't take self parameter and use the constant void/Never/etc.
  ah i was just missing an iteration that removes the tag bit in get_or_create_type.
  so that works now and my indices are small.
- made the primitives stuff a bit less painful by hackily having secret (P64, P64) before the array so you can just offset backwards for `#ct` and indirect return.
  its slightly less ugly than the old thing, and less code, and saves 10ms self-compiling. :ConfusingPrims
- made the parser more strict about semicolons after declarations.
- finally fixed field_ordering

## more more linux (Sep 17)

added ability to ask an allocator if it owns a pointer so now expr log doesn't crash on trying to print garbage
if its not in the compiler's areana or on stack. so i can see the place where the garbage number is.

// : FuckedJunkPointerExpr
tried run_tests with cranelift on linux but `parser_doesnt_crash` still hits the `bad expr ptr` (before the cranelift type error on that test that happens on mac).
its definitly using cranelift because it passes the tests that `[TODO X64]` normally.
that seems pretty conclusive that the problem isn't my x86 asm backend.
so i guess i don't need to bother setting up compiler/first to make sure it uses cranelift for jit.

// :ClearCacheHack
hmmm inline_asm_jit:manual_mmap tries to call uncompiled on real linux but works on blink if i smuggle uname.
thats concerning. its not just -static because that still fails, tho with different args to the uncompiled and after trying to link `malloc` which it doesn't on blink.
blink definitly is running the x64 code, it breaks predictably if i change the numbers there.
the non-static linked problem where the args are pointers at least is x86 glibc not giving you an empty `__clear_cache`.
and the static linked one (idk if its from musl or zig) has a noop impl of that.
and i wasnt falling back to smuggled libc if found a dynamic one but it didn't have the function.
but my binary dynamiclly linking glibc has a static copy of a noop \_\_clear_cache,
so i guess its fine if you lie and say it exists at comptime because the linker magically fixes that....
idk, the non-hack solution would be have `#target_arch` like `#target_os` (rn you can only do that with asm),
but until there's cases i need that other than this one function + unimplemented asm tests, i don't really want to bother.

> self compile can't cope if the smuggled uname uses the other os type from the compiler target because it tries to redeclare the symbol in the llvm ir.
> for now i just hack around it with manual #redirect.

I broke `-unsafe -keep-names` but only on arm so its probably a `__clear_cache` thing.
I really need to start running more combinations of tests in github actions.
but.. only if i call `0.rawptr_from_int()` instead of `rawptr.uninitialized()` as the ignored argument in alloc.
TODO: should fix the root problem

also last commit needed boot update becuase i moved ast_alloc to the compiler.

TODO: can't dlsym `dlopen` from the glib .so file, i assume they have it sepereatly because you also have to pass `-ldl` to clang to link (but not zig cc because its magic and knows).

## more linux (Sep 16)

Fixed the looping on `_NS*` functions on linux.

// : FuckedJunkPointerExpr
now when compiling itself its segfaulting on 590072672032938 in compile_expr_inner.
i feel thats not even a pointer becuase it doesnt change if i turn aslr back on but all the other numbers passed into
compile_expr_inner do.
bindings.len == 1 case of decl_var_pattern
this is the one that crashes on linux and its the first time it runs
this is valid but its subexpr Block.result is a garbage pointer.
and the block loc is `case_access = @{ @[case_access]& };` so thats compelling that it would be a strange one.
ok good it cant do tests/front.fr or requested_type_through_match either.
so its not something crazy about self-compiling, its just @match is broken.

i went through and fixed readdir stuff so now run_tests works on linux.
hmmmmm now requested_type_through_match passes. but still not front or the compiler itself.
but the number is different, and its different between compiler and front but then consistant within one.
589312462821369, 826016700493633. and the compiler one is still very close.
10000101111111101000000000000000100001011111111001
10111011110100001000000000000000101110111101000001
it doesn't have the fancy bit set for one of my indexes so its not just reading 64 bits instead of 32 from one of those being treated as a pointer.
lox has 698683805039472. the consistant per program + compiler binary is creepy.
i can recompile and add a check if its exactly that number and it fires.
I can `@println("comptime: %", FatExpr.int_from_ptr(case_access.expr.Block.result));` in macros.fr and its not the same number.
hmmmm, run_tests.fr has the same problem and just happens to not try to dereference that subexpr??
it only crashes if doing my debug printing the tag in the compiler.
hmhmmmrmrm lox works now... well later crashes for a different reason.
so im just like doing UB then, its not a sane mistake.

the compiler still generates exactly the same llvm-ir when run back to back so confusion about its behaviour isnt as easy as uninit comptime memory.
tho running on arm and x86 give different (same target).
oh thats just it calling get_comptime_arch in go_build_yo_self.
fixed that and now arm-macos and x64-macos generate same code for linux which is reassuring.
all 3 targets can build identical bf.fr which is extreamly pleasing.
interestingly, lox build is not reproducible (even on the same system)...
which is the program that tends to be super fragile about freeing a garbage pointer...
what a coincidence that its the one that has a comptime pointer in the ir...
and its build is reproducible on my linux where i turned off the aslr.
it was the null_rule_addr constant becuase i called int_from_rawptr so it didn't know it had to be relocated. thats a footgun.
now its reprodicible on all 3. alas too good to be true, its still unreliable to actually run on linux.

forgot to do X86AsmBytes (the compiler itself uses X86AsmText because it was done before inline asm was implemented on x64)
so llvm-aot tests didn't work on llvm. that fixed math_ops/branching.
the ones that do a comptime arch check becuase they're not implemented yet don't work because im cross compiling.

did: i want to hack in using compiler impls instead of libc so it can run in blink and see if same problem.
yes same problem.

## linux (Sep 15)

so a thing that was sketchy was calling allocators with 0 lenght,
adding a zero check and just returning null in the general alloc broke the compiler,
made it unwrap in poll_until_finished.
oh probably the resetting temp before any allocations and i just happened to get lucky with a zero length one before.
yeah, just had to fix coconut.jpg.
that wasn't the actual segfaulting problem, it just surfaced when trying to debug by changing allocators.

oooo lox test also fails on linux in a vaugely similar way, thats nice.
but i think its differnt :( cause it changes if i don't free in libc_allocator_fn but compiler doesn't.
tho... only if you remove a few tests, just not calling drop still crashes.
the only libc functions it calls are (abort, write, memcpy, munmap, mmap, malloc, free).
I already did different mmap flags number 34 (linux) vs 4098 (macos).
i feel like the others can't be different.

its super suspisious that the place in the compiler that's crashing is tuple_of
where it was being super confusing a long time ago
(i think it was when i tried x64 with rust).

aaaa fucking stack alignment, same place as my old problem god damn it.
and i saw it using the 16 byte load on the instruction it was faulting on `movups	xmm0, xmmword ptr [rbp - 480]`.
and in blink it even gave the right faulting address but real linux said 0 and i assumed i was just confused somehow.
aaa.

now looping trying to get `malloc` but that makes since becuase i hardcoded the macos libc path.

i think musl dlopen can't cope with glibc .so? fair enough i suppose.
gah, no they just dont do that if you statically link.

- https://www.openwall.com/lists/musl/2012/12/08/4
- https://musl.openwall.narkive.com/lW4KCyXd/static-linking-and-dlopen
- TODO: i bet zig doesn't hate me personally, try their thing https://github.com/ziglang/zig/blob/master/lib/std/dynamic_library.zig#L14

glibc mmap gives invalid file descriptor for -1??
strace says
`mmap(NULL, 1048576, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_EXECUTABLE, -1, 0) = -1 EBADF (Bad file descriptor)`
ooooohhh they have different numbers!!! MAP_EXECUTABLE instead of MAP_ANONYMOUS...
how can that be i thought mmap just did a syscall?
with musl in blink i have the number right
`I2024-09-15T17:47:06.685846:blink/strace.c:778:52190 (sys) mmap(0, 0x1000000, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) -> 0x2168409d0000`
oh but sometimes i have it right `mmap(NULL, 16777216, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7fffe63de000`
so is that a calling convention thing or maybe that one was done by the libc not my code.
jit.fr always mmaps writable and then later mprotects to exec, so im never doing that on purpose.
and `MAP_EXECUTABLE` isn't the same as `PROT_EXEC`(the former says `This flag is ignored.`).
hmmm `MAP_EXECUTABLE = 4096` so like its gotta just be calling the macos version instead of the linux version in #target_os.
fuck. yeah i forgor that i `comptime_os: Os = .macos,` dumbass.

todo: it might not find dlopen because its actually `dlopen@@<some insane garbage>` like apples's opendir.

## (Sep 14)

- ok the thing where mmap was failing in qemu must be because MapFlag.Anonymous needs to be 32 instead of 4096.
  and also (not the problem yet but will be) uname struct is a different size, so its really not just the args stuff,
  i need a general system for different impls on different targets.

there's gotta be a better way than typing

```

mkdir [mount point]
mount -t 9p -o trans=virtio share [mount point] -oversion=9p2000.L

```

every time, i tried the `/etc/fstab` thing but it didn't seem to work.
how the fuck do i make it not reinstall every time.

## linux (Sep 13)

ok i know `_NSGetArgv/_NSGetArgc/_NSGetExecutablePath` wont exist but i should be able to get the rest to work.
compiling with `-target x86_64-linux-musl` to make a .o file and then using `zig cc f26bUYh.o	-static -target x86_64-linux-musl`
(becuase idk how to put musl somewhere clang can find it),
it also complains (`ld.lld: error: undefined symbol:`) about the assmebly functions `arg8ret1__1000/arg8ret2__1001/arg8ret_struct__1002/arg8ret1_all_floats__1003`,
i tried this guys gcc https://github.com/FiloSottile/homebrew-musl-cross, same thing.
ah thank you internet https://stackoverflow.com/questions/26051946/problems-compiling-assembly-file-error-undefined-reference-to-function-name
you just don't put the `_` prefix on linux.
manually changing that in the llvm ir makes it only complain about the `_NS*` as expected.

## more more more x64 (Sep 12)

- float add/sub/mul/div is very sane, no comment, they must have added it later.
  tho im not sure if im supposed to be using the VEX versions that im avoiding because i don't want to deal with another encoding.
  im not remotely at the level of optimisation where a false data dependency on the top of the simd register would be measurable,
  so its probably fine for now...
- x64 CVTSD2SI rounds to closest instead of towards zero like the arm one does.
  the internet says "When the conversion is inexact, the value returned is rounded according to the rounding control bits in the MXCSR register."
  i'd really not have magic global state that decides rounding behaviour.
  looking at cranelift's disassembly (which does the rounding i want), it says `cvt_float64_to_sint64_sat_seq %xmm7, %rsi, %rax, %xmm6`,
  which... drum roll please... doesn't exist 🎉🎉.
  googling `cvt_float64_to_sint64_sat_seq` has one result and its a cranelift test. https://cocalc.com/github/bytecodealliance/wasmtime/blob/main/cranelift/filetests/filetests/isa/x64/fcvt.clif
  ah of course, https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/codegen/src/isa/x64/lower.isle#L3680
  silly me, i want cvttps2dq not CVTSD2SI, but no thats a vector, the second t is trucate so cvttsd2si? hehe ye https://www.felixcloutier.com/x86/cvttsd2si
  why do we even have an instruction for "eh fuck it whatever rounding the last guy wanted is probably fine".

## more more x64 (Sep 11)

- x64 jump if. setcc and load byte don't zero the register (TODO: there's a move with zero for the latter at least).
- 2227 but unreliable. lox test fails most of the time, but i've seen it work.
  and it almost always works if you run it alone (and never fails the way it does when run through examples).
  but i think its cranelift getting stressed out by a lot of functions existing?
  becuase if i comment out the part where i flush_cranelift after every time i use my backend, a bunch of tests fail
  (with uncompiled as expected), but then the lox one works every time.
  and that matches my previous impression of it getting more fragile the more you call finalize_definitions,
  (and i was calling it more now that more used my backend).
  and now that most use mine, flushing after using cranelift is more reliable.
  so i choose to belive this will fix itself when i can stop using cranelift all together.
- switch, 2255
- div. they make this a massive pain. it always outputs to rax, and puts the remainder in rdx (which would be nice if i exposed div_mod).
  and the divisor has to go in rax but its 128 bit with the rest in rdx so you have to sign extend if you just want 64 bit division.
  2263
- in_reg unspill, 2291
- load/store I32, just set w=0 and then its the same as I64.
  2891
- bit shifts. can only use rcx as the shift amount, because they hate me personally i guess.
  2956
- load/store 16bits. 2967
- call without link. 3800
- trunc. 3903

## more x64 (Sep 10)

- 683
- fix dumb load bug, was using addr twice.
- 885
- enabling stack_to_ccall_reg for more args breaks in new_arena_chunk but mostly works if you add a debug_log_str call.
  so at least thats a classic spilling bug.
  yeah missing a ensure_specific_free.
- 1284
- allow indirect ret
- 1329
- CopyBytesToFrom (simple version where its always a call)
- 1376
- intrinsics: int cmp (with setcc), nop zext/int<->ptr.
  i'd messed up the condition codes because i didn't have a check for declaring conflicting enum values.
  and then in the encoding test it picked the first name (because that matched the index) so it thought it was fine,
  and just ran redundant tests.
- 1657
- intrinsics: int add/sub/mul
  mul is insane and can only use rax but imul is all i actaully use for now.
- 1736
- CallFnPtr 1764
- bitwise and/or/xor only 1771

## (Sep 9)

- x64: setup stack frames. indexing down from rbp this time. had to go back and get rid of u16 everywhere.
  attempt load+store.

## (Sep 8)

- working on calling objective-c to make a window with the raw macos apis.
  very very confusing time with strange warnings about passing the wrong flag values but with numbers i wasnt using.
  problem was that i was passing a struct of 4 f64 as a pointer to memory instead of in 4 registers. (arm).
  because i can't read apparently and assumed the calling convention was the same as ints (only reg if <2).
  so that pointer was being seen as the first flag int and it was using whatever garbage happened to be in the float registers for the rect.
  note: its not an objective-c thing, i have the normal c-abi wrong.
  :float_struct_cc still need to actually fix that.
- x64: handle calls to known address with no args/rets.
  jump distance is from the end of the instruction! so you need to know how big the instruction is when writting the offset.
- working on :trivial becuase it annoys me.
  for functions like List/Slice, it doesn't realize they're just a value because thier body becomes noops.
  changed the criteria for a block result replacing the whole block so it does it for noops nbot just empty.
  and do it even if that block has a return label if the result is just a value since it can't possibly early return.
  so now i think at least it doesn't actually call them,
  but it still jits them because to compile the body it suspends on Jit.
  i was hoping i could just catch those trivial ones in emit_bc_and_jit and not bother since they'd be snatched anyway but it didn't make much difference.
  it was like 100 not 600 and it didn't make the 600 go down.
  i guess you still end up with calls to Option which isn't .SyntheticImmEval or .ComptimeOnly.

## (Sep 7)

- store PrimSig in a seperate array from the bc so the instructions are less chunky.
- fixed not checking switch_payloads when deduplicating and made a test for it

- such a painful time on -spam now tries to get_info too soon so i didn't notice that the error was different when i tried to debug it.
  real problem was just an extra == .Cranelift check but uuuuughghhgh im stupid.

- did the plumbing to get fuctions jitted with my own x64 backend.
  currently any functions that just push constants and return 0/1/2 (no args, no vars, no calls, etc),
  are done by me and the rest are given to cranelift.
  kinda embarrassing that my:cl is 617:3391 already (when compiling the compiler). so 15% of the functions i jit are trivial.
  :trivial

## (Sep 6)

- discovered the modrm/sib tables aren't actually that much information

## tiny cleanups (Sep 5)

- store a vec for debug loc of each instruction on BasicBlock
- move a few fields only needed during building from FnBody to EmitBc
- don't store ?TypeMeta, have is_sized field instead (padding was already there), save tag space. TODO: would be nicer to have niches in the field padding.
- track name when assigning type to constant so can print ie. `EmitBc vs FnBody` insatead of `[50 lines of garbage ommited]`.
  still pretty big message because it tells you all the arguments to a call instead of just the problem one.

> i might be going crazy but... mandelbrot test runs faster in warp if i spam scoll down !!!!!!! i love it???
> perhaps relates to my discovery before with bloat running objdump faster when i compiled in release mode because i could empty the pipe faster

## (Sep 4)

- infer arg type for #x86_bytes function
- x86 branch encoding jit examples

improving some error messages.

- added parse error for lone identifier instead of treating it as beginning of quick expr and then getting confusing sema errors.
- fixed losing error in resolve (missing @try) so you always hit a GetParsed in sema and reported the wrong location.
  should make driver programs powerful enough to add warnings for that sort of thing.
- overload not found error show the types
- fixed missing type check error for get_variant_ptr `if you pass a pointer as $t it still tpyechecks !!! : FUCKED -- Jul 16`
  I was passing arg_ty instead of arg_expr.ty for arg_ty_found of bind_const_arg.
  // TODO: really immediate_eval_expr should do more strict type checking.
- returning more sturctured errors so i can write tests for them, and so i can delay the slow formatting the error message until we know its actually going to be printed.
  this is gonna take a billon years, sema still does `@err` 133 times.
  started with what seems like the most common ones (ExpectedCompileError, TypeMismatch, CoerceConst, InvalidField).

wrote my own json lexer for c_bindgen instead of using wuffs.
processing time for c_bindgen sokol is ~70ms for wuffs and ~95ms for mine (both aot).
the json.fr for both versions is the same amount of code tho (not counting bindings/wuffs).
mine doesn't do much error handling or support string escapes (tho my old couldn't cope with escapes either).

// TODO: examples/llvm_mc_repl.fr so i don't have to use the python thing
// TODO: example image viewer to use an actually interesting part of wuffs because i still think it's cool
// TODO: nicer imports system. json.fr shouldn't clutter the namespace, most programs won't use it.
// #file_scope, better namespaces in general.

## (Sep 3)

- allow `:=` for default with inferred type on first field of sturct
- init all default fields with `()` literal
- allow empty struct when you just need to implement a trait.
  which is kinda dumb but its even dumber if you can't do it.
- trying to clean up building the compiler.
  ok the problem statement is that when repl wants to depend on the compiler it has to copy paste a 70 line driver program.
- `Tried to call un-compiled function` when coerce const overload set to runtime fn ptr (but not fid -> fnptr).
  just needed to call compile_fn_ptr.
  that got running repl on jit a bit farther but its still calling uncompiled.

started x86_64 encoding
//! The more fun game is `from pwn import *; context.arch = "amd64"; print(asm("mov rbx, rcx"));`
//! and then slowly figure out how to map that back to the tables.
//! important to note that it prints normal ascii things as characters instead of in hex because we like confusion!

```

from pwn import \*
context.arch = "amd64"
def show(s):
print(" ".join(("{: 8x}".format(x) for x in asm(s))))
print(" ".join(("{:08b}".format(x) for x in asm(s))))

```

- expand macros in imm_eval without function context (@FnPtr is common).

also (with new using -unsafe) its now faster than the old compiler (~520ms vs ~580ms).
safe its still 780 tho which is sad.

- replacing if with @if in a few places that get instantiated a lot makes SemaOverloads go from 10k to 6k.
- the step where you do type_of seems to never make progress if the type is funcid so can skip it. that might be a big fragile.

## (Sep 2)

it feels like i should be able to get cranelift passing everything on aarch64 before dealing with x86,
but it seems super unreliable. a different subset of my tests fail every time i run.
ocassionally all except multistack work so i suspect thats the only one that's actually broken.
i must be doing something wrong.
A common crash is this
https://github.com/bytecodealliance/wasmtime/issues/8852
"I just noticed that the assertion failure is unrelated to StructArgument. [...] I'm not sure when this assertion fires though."
but thats not the only one.
i don't remember it being this flaky before tho.
maybe calling finalize_definitions on every function stresses it out.

debug mode revealed `FunctionBuilder finalized, but block block_ is not sealed`, which i can fix with builder.seal_all_blocks at the end.
still super flaky.

in other news, added back zero_padding which fixed the easy case of reproducible builds (run the compiler twice back to back and the checksum of the binary should be the same both times).
// TODO: should add a test for that

## cleaning up

- update go-build-yo-self to create v2
- remove padding in ExportVTable
- make enums in driver_api zero indexed
- move Flag (fixed ident) out of driver_api and removed unused values
- stop tagging things #compiler, remove some now unneeded functions.

## faster! (Aug 30)

the old one was ~580ms safe. at the very least i should be able to match that unsafe.

(times are -unsafe).

another like 10% is the old resolve_in_overload_set because every single macro call goes through resolve_by_type.
inline_cache takes us from 900ms to 800ms.
can do a hash table in resolve_in_overload_set_new and get to somewhere around 770-730.
but its kinda clunky because you need to be constantly checking if you know all the types for the parts and then creating a tuple of them.
and its hacky because i can only get it to work if i don't add to table when any part.is_const.
becuase of the coercion stuff i assume but it feels too fragile.
so im just doing the simpiler inline_cache that makes macros not crippling and takes like 10 lines of code.

misc, got to around 750.

theres a whole seperate tree where its doing check_for_new_aot_bake_overloads thats 10% of the time.
thats just a bunch of extra work thats flat with number of types in the universe when doing AOT, not with number actually used.
so instead just collect from the overload set up front but only compile+jit once you actually need to emit a constant of that type.
doing custom constant bake functions lazily got to ~680.
and the whole emit_bc is now ~5%.

that one was a bit less satisfying because this fix would speed up the rust version too.
but it was a pain to do back when everything was rust and i didn't want to just turn off the borrow checker.
so maybe it counts as an advantage of this langauge.

but anyway thats like 17% slower than the old compiler. getting there.
the correct thing to do would be fix whatever the crazy exponential overloading problem is thats taking half the time,
but i kinda want to see if theres any other low hanging fruit first.

## it compiles itself! (Aug 29)

its like 3x slower.
the first number i remember was ~2200ms safe.

with -spam you can see `decl_var as_int%100600` 71 times. so i think a done flag on FatStmt will help a lot.

oh damn, turning off debug assertions goes from 1800 ms to 1580 ms, thats the combination of saved time and not having to compile them.
using that to compile a version with debug assertions + indexing takes ~1650 ms.

done flag on FatStmt gets down to (unsafe 1400, safe 1780) and brings spam output from 4396386 lines to 2033850 lines.

fixing .TookPointerValue (unsafe 950, safe 1300). my hashtable of integers is really fucking slow.

##

seems you no longer need this.

```

// If you don't do this at all, you loop (TODO: why? gets stuck on EvalConstant:i64).
// But if you @check it, you'll error out on things that need a type hint.
// We do a real typecheck after dealing with the const args so its probably fine.
// TODO: compiling it at all here (check or not) has to be wrong tho,
// because you don't want things in a const arg expr to be added to runtime callees.
\_ := self.compile_expr(arg_expr, arg_expr.ty.want());

```

a couple mistakes in the compiler code that were incorrectly allowed by the old sema. thats kinda cool.

- `self.put_token((BinaryNum = (bit_count = bits.trunc(), value = total)));`
  with value wanting u64 but total being i64 used to work without .bitcast() but shouldn't
- `out.push((Num = (i, .I64)));` where `Num: @struct(value: i64, ty: Prim),` used to work but you should need field names.

---

- forgot to call get_or_create_type for u8 in eval_str

##

remove some unneeded sema_regression changes.

- (macros, bits) didn't used to need the annotation.
- (mandelbrot, mul) this should be able to be #inline and still get its type hint.
- (panic, panic) i used to just call unreachable() here but now i can't cope with mutual recursion
  even when fixed, this should still actually abort() because if the hook returns we're in a fucked state. -- Aug 6
- (arena, ::) old sema didn't need the type annotation! its picking the wrong overlaod now
- (hash, StupidAddHashable) used to work without `: Type`
- (run_tests, run_llvm) shouldn't need to be constant :runner_segfault

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

int readdir_r(DIR _, struct dirent _, struct dirent \*\*) \_\_DARWIN_INODE64(readdir_r);

```

and then in cdefs.h

```

#define **DARWIN_INODE64(sym) **asm("\_" **STRING(sym) **DARWIN_SUF_64_BIT_INO_T)

```

and

```

# if \_\_DARWIN_64_BIT_INO_T

# if \_\_DARWIN_ONLY_64_BIT_INO_T

# define \_\_DARWIN_SUF_64_BIT_INO_T /_ nothing _/

# else /_ !\_\_DARWIN_ONLY_64_BIT_INO_T _/

# define \_\_DARWIN_SUF_64_BIT_INO_T "$INODE64"

# endif /_ \_\_DARWIN_ONLY_64_BIT_INO_T _/

# else /_ !\_\_DARWIN_64_BIT_INO_T _/

# define \_\_DARWIN_SUF_64_BIT_INO_T /_ nothing _/

# endif /_ \_\_DARWIN_64_BIT_INO_T _/

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

pub fn main(){ #[cfg(target_arch = "x86_64")]
println!("sizeof(A) = {}", core::mem::size_of::<A>())
} #[repr(C)]
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
This is free software; see the source for copying conditions. There is NO
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
%v1 =l call $something*important(ub 123)
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
/* end function trivial \*/

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
.globl \_get_two
\_get_two:
stp x29, x30, [sp, -32]!
mov x29, sp
mov x1, #8
add x0, x29, #16
add x1, x0, x1
add x2, x29, #16
mov x0, #123
str x0, [x2]
mov x0, #456
str x0, [x1]
mov x1, #8
add x0, x29, #16
add x0, x0, x1
ldr x1, [x0]
mov x2, #0
add x0, x29, #16
add x0, x0, x2
ldr x0, [x0]
ldp x29, x30, [sp], 32
ret
/_ end function get_two _/

```

and in my language,

```

fn get_two() Ty(i64, i64) #log_asm = (123, 456);

```

with my current garbage asm where i do absolutly no optimisations, you get this

```

=== Asm for Fn2352: get_two ===
stp x29, x30, [sp]
mov x29, sp
sub sp, sp, #0
mov x0, #123 ; =0x7b
mov x1, #456 ; =0x1c8
mov sp, x29
ldp x29, x30, [sp]
add sp, sp, #16
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

get_pair: // @get_pair
mov w0, #123 // =0x7b
mov w1, #456 // =0x1c8
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
84 │ (self.is_some(), fn() O = then(self&.Some[]), fn() O = else())!if

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
const log: Fn((Ty(VoidPtr, bool), Str)) = log; // Somewhere to hang the type annotation to resolve the overload.
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
stackPointer: VoidPtr, // fp
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

```

```
