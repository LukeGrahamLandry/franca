
- crack down on smurf naming conventions (ie import("@/lib/alloc/debug_alloc.fr").DebugAlloc is kinda dumb). 
- real error handling for lib/sys/posix.fr. need to be able to remap the errno values to something consistant. 
- i've lost 20ms of speed. i should really make something that automatically times it every commit. 

## tooling for debugging

- sampling profiler with setitimer
- something where you can mark some memory as an artifact with a name and a type,
  like when you generate code (like compiler/test.fr/gen_full_test_program) or a module 
  (like backend/opt/simplify.fr/static_memmove), and set an envvar to have 
  the compiler dump it all for debugging. so you can look at all the extra stuff 
  that when into your program that doesn't have a real home and error messages could 
  point to that as a source location. 
- strace from my syscall wrappers
- DebugAlloc
  - do something about the name resolver for a jit module needing to live all the way to DebugAlloc.deinit to give nice stack traces. 
    you can't just leak the whole module because then you get junk leak reports. 
    instead of pop_resolver of the module, have an option to finalize the debug info into the same flat structure as for aot and keep that. 
    it would be nice if drop() were generic over the message sent to the allocator so you could mark the whole tree of memory as 
    ignore_leak without rewriting the thing. like getting a billion leak reports because you forgot to drop(QbeModule) is much 
    less useful than if it just showed one. 
  - make it easier to toggle on and off. should have a consistant system for all the configs like this. 
  - memory isn't the only resource. i should have something similar for file descriptors and lambdas that you're not supposed to return
    from like walk_directory. maybe just allocate some junk memory, pass it around, and deallocate at the end and then it doesn't need any extra work. 
    (extra dumb to waste a whole page for that tho if i keep it coupled to the unmapping in debugalloc)
  - extend the crash_report on fault to lookup the address you crashed on (the memory access not just the ip)
    and report where it was allocated if possible. 
  - make it more tunable in case it's too unbarably slow. like maybe you don't want to give each allocation its own page, 
    just protect the page when all allocations on it have been freed. 
  - seperate event_counter per thread as well. 
  - sort the leak report by event counter so it's easier to diff. rn it's in hash order of aslr address which is unhelpful.
- should log when the slow things are turned on so i can't forget to turn them off. 
- need to give better source location for #inline and @macro. each ip can have a stack of locations. 
- SLOW_PROTECT_ARENA doesn't work with SLOW_DEBUG_ALLOC because the protect forces it to be leaked. 
- tests for all this somehow. need to make configuring it more programatic. so choosing a different allocator at top level
  (like how DebugAlloc works) is more convient than just checking the constant in the implementation (like how ArenaAlloc works). 
- nicer interface for bloat.fr, make you a pie chart of function size by file or something idk but the list is kinda unreadable. 
    
## import_c

- import_c add a test for the incorrect include guard thing i fixed on jul16
- import_c faults on function without parameter name: `int aaa(char*) { return 1; }`
- import_c/cc.fr searches include path for the starting file before your current working directory which is super confusing
- turn off Qbe.Fn.track_ir_names in import_c
- :ReWalkTokens
- what is the deal with load_opt::def taking so long for import_c
- implement _Atomic in import_c
- import_c: `__constructor__, __aligned__`
- import_c, get rid of :BoundsPadding
- import_c: if you call something at both comptime and runtime it needs to redo the 
c frontend work because of `#ifdef ARCH/OS`. C also lets you have different function/struct
bodies on different targets which i don't deal with well. 
- for import_c, decide when to reset temp() and audit all the allocations in tok/pre
- import_c/ffi has a big comment

### !! BROKEN !!

- wuffs/gif.c fails at random
- there have been very rare failures in my lua tests for a while
- soft_draw.fr crashes when you quit the program
- spurious failure of import_c/tests/wuffs.fr on macos-x86_64 
  (failed in github actions and then worked when rerun with no change)
- random failures in import_c
  - `panic! tried to unlock someone else's mutex`
  - `16000panic! recursing in new_arena_chunkmemory order problem`
  - `panic! use_symbol but already locked by this thread=1`
this eventually reproduces with the slow memory debugging stuff turned on. 
```
for i in $(seq 1 1000);
do
    echo $i
    ./target/f.out examples/import_c/test/test.fr || { echo "fail"; break; };
done
```

## import_symbol / weak

TODO: my @import_symbol always does a weak symbol even though it in the macro body i say weak=false.
TODO: test that makes weak and non weak of symbols i know don't exist and makes sure the dynamic loader crashes correctly. 
TODO: tests that @import_symbol give you null for a missing weak symbol (instead of address of a stub like when you #import a function), 
      it works right now but is fragile. 
TODO: fallback to the syscall version automatically if a weak symbol is unavailable
TODO: @import_symbol of non-existant throws TraceTrap at comptime
TODO: be consistant about spelling: zeros or zeroes

## COMPILER BUG 

- lots of crashes if you have an overload set somewhere with the same name as a constant and then try to use the constant. 
- #use is ordered
- tests/todo
  - a.fr: first_ref_os, const_field_unordered
  - b.fr: multiple prefix calls `float intcast 123`
          (might even be a parser bug? always a surprise when that's the problem)
  - c.fr: first use of constant is fieldaccess?
  - d.fr: OverloadingConfusedCoerce
  - e.fr: #use inside a block prevents lookups that escape to the outer scope
  - f.fr: missing ; should be disallowed
  - g.fr: bad performace of linked list of `body = @{ @[body]; @[next]; }`
- repro doesn't work accross linux <-> macos
- `@debug_assert(macos.common().valid, "not valid");` compiles 
and gives you junk (that's not 0 or 1) when `common()` uses `#unsafe_noop_cast`. 
rn you're supposed to need a redundant `[]` in a call like that. 
- fix the infinite loop when a constant references itself
- `@struct(a: A #use = (),);`
- // TODO: don't segfault if you get a compile error inside a jit_shim. 
- import_c: tests/todo/b.c
- same string constant as Str and CStr :MiscompileCStr
- :ConstInFuncOrLoseTemp
- literal for a 64 bit integer with the high bit set shouldn't need a bit cast 
- `fn vec2(x, y) = (v = (x, y));` miscompiles if inlined but i can't reproduce it in a simple test. 
- :UseDoesntWork
- :BitFieldsCompileError there's places in Qbe and Incremental where it won't let you have 
a field of type @bit_fields or you get:
```
panic! compiler/main.fr:2:1
#include_std("lib/core.fr");
Compile Error: 54 matching options for index

TODO: end of loop. still too many options for 'index'
```
- // TODO: only the first element of the @slice in unquote_placeholders is getting typechecked that it wants FatExpr not *FatExpr? 
- #inline returning Never doesn't remember that it returns Never
- there's still a compilation order problem with inlining intrinsics. look at copy_bytes(). 
- :ThisIsNotOkBecauseMemoryWillBeReused

## 

- #log_ir should fire multiple times for functions with $const parameters (ie. native_isel)
- shouldn't be able to typo a name as easily. like S :: import_module{enqueue :: enqueue_task}); then S.enqueue_task will get you the wrong one and be slow. 
- why was the quicksort wrapper trying to be emitted for import_module (when not marked #fold)
- fix my blink patch so it applies and i don't have to remember to type `/Users/luke/Downloads/franca_target2025_jun12/blink/o/blink/blink` 
  every time i want it to not be super slow. 
- get compilation order dependence under control!!
- fix callgraph sorting to improve inlining. like make sure the ge/le in lex_int/is_ascii_digit are inlined 
- make AsmFunction get an inferred name
- @bit_fields in incremental.fr don't work inline in the structs
- use HashMap.get_or_insert more
- #ir tries to ignore zero-sized params but not if they're first which is sad
- :BrokenCGeneric i think erroring on conflicting `_Generic` cases is correct but you're supposed to treat `long` and `long long`
as different types even when they're the same size. 
- "need to be consistant about how to handle modules like this that don't actually compile anything"
- fix examples/terminal.fr -jit so it doesn't freak out about nested compiler contexts because of the repl
  (get rid of EASY_GRAPHICS_IS_JITTING_HACK)
- reduce disk usage bloat. rn fetch_or_crash stores the unzipped thing and the zip file so you have everything twice. 
many of the things i use i don't need everything from so it could make you tell it which files are important and 
delete the rest. thing to think about is that you want to union those between different projects that depend on 
different subsets of the same resources. 
- run tests in landlock so it's less scary to miscompile something that's doing random syscalls
- remove from the language:
  - make is_linking_libc() not foldable
- document #weak: docs/(annotations.md, imports.md)
- deal with `NOSYS`
- apple has different c variadic abi. that's pretty unfortunate for my grand plans. 
  it feels pretty invasive to have the backend compile two versions of anything variadic. 
- get rid of query_context_is_implicit
- make #log_asm work for the #asm replacement 
- experiment with outputting even more info in .frc and an lsp that reads it back. 
- zero parameter syscall

// TODO: use this for stuff
//fn source_location(arg: FatExpr) FatExpr #macro = 
    //@literal arg.loc;

## backend 

- macho/emit.fr/emplace_fixup() allow negative offset for DataAbsolute of dynamic import in .Exe
- arm64/emit.fr/loadaddr_bits() allow large offset in .Relocatable
- rm64/emit.fr/fixup_arm64(): offset from dynamic import
- isel5.ssa -w: fails with inlining disabled
- (mem3.ssa, isel5.ssa) -w: fails when constant folding is disabled. 
  `IR failed typecheck: invalid type for first operand RTmp:144 of add in block @4`
- "you really want to lock the module in case there's another thread trying to compile into it"
- rv64/isel: fuse cmp+jnz
- error prone that the other isels have overloads called fixarg,fixargs etc and i rely on the types being different. 
hould just make them local constants in each file like they are here in riscv
- factor out the top level ARCH_isel(), they're all the same shape
- test for phi of stack slot? 
- rv64 address+offset folding like i did for arm. they can probably share? 
- remove redundant extension for `b := a & 31; c := extub b;`
- i don't like that direct to exe vs to frc_inlinable then to exe give different binaries. 
- be able to output frc/frc_inlinable in the same module as compiling normally so you don't have to do two passes over things to cache it
- fix those two ^ and then compiler/test.fr can create all at once and assert that they make the same exe instead of running them all
- elf: don't include names for local symbol with DataAbsolute relocations when exe_debug_symbol_table=false,
  and do include all symbol names when exe_debug_symbol_table=true. 
- test that function names don't appear in the binary when exe_debug_symbol_table=false and do when true.
- test that makes a dylib
- macho exe_debug_symbol_table doesn't work i clion profiler (dtrace?)
- make sure the string "franca" doesn't show up in binaries. ie `__franca_aot_debug_info` symbol offends me
- i want the module made for static_memmove in opt/simplify.fr to not include 
`__franca_base_address` and `__franca_aot_debug_info`
- debug assert that all tmps have a definition in rega. 
  (especially because @emit_instructions doesn't catch it)
- turn off ASSUME_NO_ALIAS_ARGS in arm/abi. it doesn't help much and it would be better to do the hard thing 
  and figure out how to make UNSOUND_SKIP_EXTRA_BLIT in emit_ir work. 
- arm relocatable import: relocation to fold symbol offsets into the adrp+add instead of wasting an extra instruction :SLOW
- fixup2.ssa: "this should work without the indirection, i just don't handle folding the offset into the constant correctly."
  i think that's fixed now. add a little test of that too
- "this works here but not in it's original place under f()."

## don't rely on libc

- import_c/tokenize: strtoul, strncasecmp, strtod
- import_wasm/run.fr: 
  - snprintf
  - (because .ssa test calls it): getchar, strcmp, memcmp, memcpy, strncmp
- prospero: atof
- fetch_or_crash: stop exec-ing random shit! that's even worse than depending on libc!
- examples/bf: putchar, getchar
- (epicyles, geo): fmod
- (graphics): cosf, sinf
- dlsym, dlopen, dlclose

## linux 

- linux fault-na.ssa need to do the signal struct (rn it's skipped in backend/meta/test.fr)
- :TodoLinux CLOCK_REALTIME
- repro doesn't work cross compiling from linux to macos,
but dump_macho.fr and objdump -d say they're the same (for mandelbrot_ui.fr at least). 
so something in the data i guess? 
- can't cross compile from (macos) to (linux -syscalls) if you call 
  write_entire_file at comptime because it wants gettime (for atomic file name, bleh)
  which i havn't transcribed syscall for on macos. will be fixed when i make prefer_syscall
  not a constant which i want to do anyway for reusing frc_inlinable when cross compiling. 
  (same for linux-sta -> linux-dyn)
- elf Dynamic
- how are you supposed to ask for page size? blink wants 64k instead of 4k. 
- standalone import_c/cc.fr and meta/qbe_backend.fr can't make statically linked binaries because the `_init` is written in franca
- if macos {  // todo: "why does this work on my linux but not github's linux"
  (tests/sys.fr using elf_loader.fr on a dynamic executable)
- elf_loader.fr doesn't work on linker output: `panic! not divisible by page size`
- lldb doesn't have symbol names or let you set break points. do i need to do dwarf stuff?
  (it kinda works in gdb!)
- decide what to do about entry point without linker (need to align stack always + 
  static relocations or call `__libc_start_main`), but franca_runtime_init is 
  written in franca (same problem for cc.fr). could do like staticmemmove and compile 
  to module embedded in the compiler and spit that out on demand. 
- once is_linking_libc isn't #fold, if you have a static compiler and want to link a libc thing
  and notice that the path to dynamic loader is valid, 
  it would be cool to try to do something where you morph by poking in the Dynamic header
  and reexecing the compiler to get it. 

```
* thread #1, name = 'q.out', stop reason = signal SIGBUS: illegal alignment
    frame #0: 0x0000000001016b78 q.out`impl2__7041 + 224
q.out`impl2__7041:
->  0x1016b78 <+224>: cas    x17, x0, [x2]
```
- shared libraries (backend/elf/emit.fr)
- shader translation for the gui examples
- non-amd64 support
  
## amd64

- amd64: `std/json        cc      FAIL test_wuffs_strconv_parse_number_f64_regular: "-0.000e0": have 0x0000000000000000, want 0x8000000000000000`
- amd64: examples/import_c/test/test.fr
- import_c can't compile lua targetting amd64 (makes an exe but it crashes immediately)
- import_c/test/ffi.fr: panic! sysv abi does not support variadic env calls
- finish amd64/isel/NEW_ADDR_FOLDING
- be less strict about amd64 address folding when there's a large constant pointer (which is valid when jitting)
  - idk, i might have done this already? 
- CLOCK_REALTIME
- integer division by zero traps. so cross compiling isn't totally transparent. 

---

- examples/terminal.fr: does rosetta2 land's version of MSL have different rules??? fuck!
```
METAL_SHADER_COMPILATION_OUTPUT: program_source:15:14: error: 'texture' attribute only applies to parameters, global constant variables, and non-static data members
    T4 tex [[texture(0)]];
             ^
program_source:16:14: error: 'sampler' attribute only applies to parameters, global constant variables, and non-static data members
    T5 smp [[sampler(0)]];
             ^
program_source:22:39: error: invalid type 'T6' for input declaration in a fragment function
fragment T7 main0(T3 v1 [[stage_in]], T6 v2) {
                                      ^~~~~
program_source:25:8: error: call to deleted constructor of 'T4' (aka 'texture2d<float>')
    T4 v3;
       ^
/System/Library/PrivateFrameworks/GPUCompiler.framework/Versions/32023/Libraries/lib/clang/32023.619/include/metal/__bits/metal_texture2d:2346:3: note: 'texture2d' has been explicitly marked deleted here
  texture2d() thread = delete;
  ^
program_source:26:5: error: constant sampler must be declared constexpr
    T5 v4;
    ^
    constexpr 
```

## Quest Lines

- compiler: add a defer expression that lets you run cleanup from a closure even if you jump out past it
- wasm:     get all the ssa tests passing verifier
- linux:    make all the tests pass with -syscalls
- external: make it not a 100 line copy-paste to setup a driver that links an object file

## Caching

- caching only happens if the file path ends with .fr
- crash backtrace with source location
- keep caches for multiple targets at once?
- need to be careful if start caching both main+driver from one source file
- make it work for fetch_or_crash. like if you import a c thing, the hashes of 
those c files should go in your .frc file. it's tempting to just use the hash of 
the one zip file for the resource but i think being able to printf debug by just 
editing your local copy of the c files is important. 
- api for import() of a string that you want to involve in the cache file. 
rn it just assumes it's generated from your input files so can be safely ignored. 
- have examples/default_driver and graphics/easy do it for main(). 
but then need to deal with including build options in the cache (like -unsafe, -wgpu, ENABLE_TRACY) 
- do automatic caching for big comptime things (like import_c'include)
- now that import_c is always serialized, just need to add `dep`s and then it can be cached separately from the rest of your program. 
- allow smaller compilation units. like not making you recompile the backend when you work on the frontend. 
- test that .frc files repro and that you can pass them directly
- dump_bin: print segment.MachineCode as something qbe_frontend.fr can parse so it can round trip
- clear cache before tests just in case
- whether the host compiler was built with`-syscalls` needs to go in the cache file :CacheKeySyscalls
- caching an invalid thing i think? if you Compile Error: we hit a dynamicimport ('puts_unlocked' from 'libc') with no comptimeaddr for jit
(happening with import_c)
- persist #noinline

next impressive advancement: 
make this work well enough that i don't need to re-export the backend functions in the driver vtable. 
`enqueue_task, codegen_thread_main, drop_qbe_module, finish_qbe_module, run_qbe_passes, init_default_qbe_module, emit_qbe_included`
i want them to go through sema exactly once if you compile the compiler and then use it to run a driver that compiles a program that uses the backend. 

the current state of import_module() is that if you change anything it's faster to just use FRANCA_NO_CACHE=true,
but now it's spilt into pieces that it's clear how to parallelize. 
even when cached, need to store post-regalloc as well or it's kinda pointless. 
gets to the point where it's just blocked waiting for the backend thread.
should allow more backend threads but need to be careful about order because i want repro. 
also it's cripplingly broken because it doesn't track buildoptions so it's disabled in meta.fr for now. 

### FrcImport 

-  run inlining and if something changes redo the other early passes as well
- someone needs to detect undeclared symbols
- pre-regalloc is almost target independant but it doesn't quite work 
  - because of apple_extsb
- make it work with did_regalloc if you know you only care about one architecture
- check arch in import_frc() if did_regalloc 
- need to merge deps if we're generating a cache file? 
- import_c: local declaration of import doesn't work because it doesn't get a name in root_scope
- fix import_c/ffi.fr needing to compile the whole backend now and then time stuff to make sure this way isn't way slower than the old way. 
- source locations
- remove DISABLE_IMPORT_FRC. need to deal with scoping when you import(@/compiler)
- import_c/ffi.fr: make forward declaration of an import work in local scope (rn missing type info in root_scope so can't use it)
- support symbol without type info as a rawptr that you have to cast to use
- if im going to keep FTy.Tag.AbiOnly, need to write a test that actually uses it with aggragate returns. 
- It needs 3 pieces of information: exports, callgraph and bodies, but there's 
  no need to supply those at the same time. we want to enable the different frontends 
  so run in parallel whenever possible so maybe allow splitting the type info from the code?
- type/symbol alignment
- replace sym.imp.temporary_funcid with a sane import system so modules can be reused between CompCtx-s
- "making everyone remember to do this is kinda lame"
- env var for saving out all the files so you can inspect them, 
like what i do with a.frc but without recompiling the compiler. 
similarly allow #log_ir that persists on a module so you can debug. 
- make mutating imported data at comptime get baked into the program like normal (instead of just the initial version)? 
tho rn it's a bit order specific so the way import_frc works is probably a valid ordering you could get with normal franca code as well. 
so maybe that whole system needs a bit of a rework. like maybe waiting and do all baking at the end would be less creepy.
- if you compile with -debug-info, dbgloc instructions have offsets into the source codemap which will be wrong if moved to another module. 
- store multiple things for the same file path. (frc_inlineable) + (frc for each target) and choose the best one. idk if it's better to have 
  them all in one file or with different extensions. 

- non-u32 enums
- don't output frontend type info for internal functions
- type names
- unify imported types
- unmangled function names
- non-i64 @tagged. doesn't really matter because i don't allow it anyway but should assert on it just in case. 
- import_module 
  - don't hardcode `exports` as a magic symbol
  - include deps and chech thier hashes
  - include build options somehow
  - add back !did_regalloc check
  - the things where i just assume something is a One(Sym) feels kinda bad. should have a special tag somewhere maybe? 
  - be able to save both before and after regalloc. need before so it can cross compile (works) and inline (eventuall). 
    but regalloc is the slow part of the backend so it's sad to do that everytime. 
  - be able to compile modules in parallel. right now it's much slower the first time but helps if you're compiling many programs that use the same thing. 
  - needing to explicitly declare which functions you're exporting is really annoying
  - now that you'll be having more modules, you really want to cache the normal comptime stuff automatically. 
    like you can't have every compilation unit eat 15ms on @syscall, @match, etc
  - use the multiple scopes thing
  - every module has it's own copy of lib/runtime/etc stuff which is terrible, especially for statics, they really need to share 
    or everything's confusing, like OS, RESOLVERS, really need to only have one. 
    the functions being duplicated is also very sad but not as fundamentally crippling. 
  - when a module depends on another it shouldn't pull all the code into itself and then cache that, it should just reference 
    the other module. like (qbe_frontend->(import_c, backend), import_c->(backend)), you'll have 3 copies of the backend code on disk. 

- i want the apis for importing .frc files to be as convenient as dlopen. 
  i like the idea of importing things as source and transparently using the cached thing if the hash hasn't changed.  
- track envvars you observe (ie. ENABLE_TRACY). 
- `foo :: 0; bar :: @struct { foo :: foo + 1; };` should be legal. 
  it's so garbage that you have to think of synonyms when declaring exports for import_module()
- probbably don't want each module to have it's own copy of builtin_memove_symbol, 
  but that applies to lots of lib stuff too. like shouldn't have many copies of 
  syscall wrappers, arena_alloc, etc. 

## Unfinished Examples

- epicycles: make it actually trace the correct path
- lambda: imporove diagrams and implement evaluation
- import_wasm: make it usable for libraries written in other languages

### Terminal

- crashes if you `cd "";` twice in the repl
- tab to autocomplete a file path 
- escape codes: colour, move the cursor, clear the screen
- send input to stdin
- raw input mode
- catch faults/panics from the repl
- option to reset the repl since it leaks memory indefinitely 
- unicode characters
- && to run two commands
- make it compile faster when ENABLE_FRANCA_REPL
- multiline franca commands
- restore session when you restart it
- jump around past commands like warp
- search
- environment variable syntax
- tell child programs that im a terminal. ie. ls gives me one column instead of two.
- if something's outputting invalid utf8, switch to hex view,
actually that's a bit too agressive but certainly stop processing ANSI escape codes. 
- rebake the texture for dynamic font size so i can handle when you move between monitors with different dpi correctly
- scroll when you resize to lock the bottom position instead of the top
- tabs
- drag a tab out to make a new window
- auto-scroll if you put the mouse at the edge of the screen while highlighting to grow your selection
- undo

## Graphics

- https://github.com/floooh/sokol/commit/50bbbe4521af356c3b0879e1d46e30114feb4e6b
- finsih webgpu
- finish gfx-metal. need to make sure everything is reachable
- example program that tries to use all features
- finish gfx validation and enable that based on DebugAssetions
- clean up defaults
- dearimgui backend
- support x11 / opengl / glsl
- support web (depends on finishing wasm backend), don't use emscripten
- revive compiler_gui (try to export all info to the driver so it can run in comptime)
- provide the same c api as sokol so can test with their examples? 
- translate a more serious subset of my language into the shader languages
- think about fancy logging system instead of @debug_log in `graphics`
- inspection gui like sokol_gfx_imgui
- i enjoyed https://sotrh.github.io/learn-wgpu/, an implementation of that would be a good example program
- replace cimgui
- https://github.com/id-Software/DOOM
- implement #trace
- test that runs all the interactive things in sequence
- geo: `// TODO: the controls feel really bad. you can like lose key presses.`
make sure that's not something i broke (i think it was always like that, 
just a problem with how that program is doing directions not with the app lib)
- generate the shader desc structs instead of pasting them
- :LazyMagicNumbers
- :DEPTH (which is also for msaa)
- bindgroups_cache 
- comptime thing to generate SgShaderDesc

## stuff i broke

- `./q.out -t wasm32 -o target/out/q.wasm -cc backend/test/abi8.ssa -d AI`
%�%.104 =w pop
- fast memcpy (need to deal with fallback when not linking a libc)

## Introspection 

try to mark places that rely on running inside the same compiler context and not from a driver 
since i probably want to precompile these eventually and will need to deal with non-fixed
types, etc. should also document that distinction more thoroughly. 

the big examples are import_c, import_wasm, shader compiler, objc bindings, backend ir parser. 

kinda sad to need to recompile the whole c compiler from scratch 
any time you want to compile a program that want's a nice font. 
makes my start up time look even worse than it actually is. 

The alternative school of thought is that i could go full linker and be able to 
give types a stable identity accross compiles and patch out the type constants 
so you could cache code that runs at comptime without it needing to do everything 
through the ImportVTable explicitly. but that feels a bit too wishy washy to me? idk. 

## language consistancy

- :NoInlineImport
- ._N and .len on Array
- #use field in guess_type for #where
- make auto deref always work (you shouldn't need to `[]` for constants or returned structs)
- compiler/values.fr has a big comment
- make namespacing nice enough that i can have less stuff loaded in every program by `core.fr`
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
- @rec and #generic suck
- reporducible builds depend on compilation order because of var id (like if you iterate a scope) 
or function ids which end up in your binary. this is actually fine currently because 
the frontend is single threaded but maybe it's not a great idea in the long term. 
- don't evaluate the spread value of `..` multiple times
- make `..` work when it's redundantly spreading a single item (rn it's an error)

## library robustness

- dynamiclib is kinda broken because i rely on OS being set by runtime_init
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
- make sure all the allocators respect required alignment 
- catch multiple branches with the same switch value.
- (cap, alloc) args are swaped between list:List and init:RsVec
- `[CPU Time]` on macos-x64 is wrong. (different libc magic numbers? clang libc_constants says no). // :WrongClockTime
- maybe have a `push_compiler_error_context` and `pop_compiler_error_context` for macros so @fmt could easily add a message like `while evaluating format string`?
- is :jnz_is_Kw really what i want the semantic to be?
- RSlot overflow
- default arg values (any const expr and inject at callsite? or based on other args so generate shims or multiple entry points to the function)
- clean up what goes in lib/build.fr vs lib/sys/fs.fr
- make fetching dependencies (ie. lua for testing import_c) not embarrassing
- implement examples/testing.fr/fetch_or_crash() with import_c (wuffs and libcurl) instead of exec-ing shit
- fetch_or_crash hashes are of the compressed file which is garbage. will break if github changes compression level or whatever. 
- use import_c for the parts of sokol i haven't ported yet (can't for the mac stuff because thats objective c)
- shouldn't have `alloc` be the nice name, it should be `alloc_uninit`
- :ThisSureIsABigBlit
- make import_wasm not crash on implicit return 
- it would be nice if the backend did a bit of typechecking when you were 
targetting wasm so it could give you the errors instead of producing a program 
that fails the wasm verifier
- bake for list/hashmap need to get rid of uninit memory
- List.shrink_to_fit for places that i push and then return a slice so you can free it on allocators that don't track size

## cleanup 

- backend: unify SymbolInfo.library with the old name$module i was using for wasm .ssa tests
- ImportVTable: implement add_to_scope with add_expr_to_scope
- make comptime.fr exporting stuff less painful 
- sema needs to get simplified. 
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
  - graphics/web/webgpu.fr which is extra bad because the abi isn't stable so it's useless
  - if i ever get serious about using tcc for anything, we can't be having thier lib/atomic.S
- extend hacky_incremental into something that can be used seriously 
- as an extension of argparse it would be cool if all the demo programs could be both 
and exe and a dylib so if you want to run from cli it parses to a struct and calls the impl,
but if you're calling from a program you can import the struct, dlopen the thing, 
and not need to serialize the arguments to a string. 
- seperate out the platform specific fields of QbeModule
- finish curing stds

## tests

- dylibs
- pending repros: uninit_stack_slot, typchk_unquote
- have a test where you force inline everything that's not recursive to stress test the backend dealing with large functions.
- compiler/test.fr run for jit as well
- automated test that builds are still reproducible (including with -debug-info which doesn't repro currently)
- fix the test programs to not all write to `./a.out` or whatever so they can run in parallel.  
- test compile error for conflicting #use
- compile all the examples in run_tests: toy
- repro doesn't work when you do `-repeat`
- more calling convention tests between jitted code and c.
- have one command that lets me run the tests on all targets
- tests for failing progeams. ie. panic backtrace
- auto test repl. 
- compiler/tests.fr stops when something doesn't compile but it should show which other tests passed like it does for runtime failures
- make it clear that you can't do this: `franca self.fr && ./a.out driver.dylib run b.fr`. 
it doesn't like that you stomp a.out, default_driver:run should pick a unique path probably. 
or just default to jitting and force you to enable aot by specifying an output path. 
- sort the array of test files so you can always diff the output without hoping the file system iterates in a consistant order 
- think about how to test the gui programs more convincingly than just that they produce a binary
- test using import_(c, wasm)/ffi from a precompiled driver to make sure they're not relying on being in their own compilation context 
- test crash stack traces
- make the crash examples work without needing to set the env variable / run jitted
- TODO: i should probably be making tests for error locations
- arm-linux github actions
- run rv-hello-world in libriscv (and x86_64 in blink too maybe)
- instead of hardcoding `clang`, use env var CC or something more sane.
  could try clang and fall back to a self-compiled tcc if it's not available. 
- make it easy to skip any tests that have external dependencies

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
- `pattern match on non-tuple but expected % args` should show the callee as well
- when it tells you a #import is null at comptime, say whether the library string was registered. 
(ie. did you probably typo the lib or the func).
- integer overflow of a literal
- contextual field not found should show the type's declaration location 
- "arity mismatch" should check if last aparameter is CVariadic and suggest @va
- "arg type mismatch after removing const args" error message needs to tell you the types
- i need to be able to easily identify the Anon generated functions. 
  try to be smarter about giving them a name. like for const eval of a `::` var it should just be the name on the left. 
  use FnDef.line and make it more elegant somehow, idk 
- give data symbols readable names now that they show up in symbol table

## language decisions

- decide what the policy on not having a bti instruction on AsmFunction should be. 
  need to tell the frontend that it can't be shimmed without a patch. 
  should i just insert them for you? that's probably bad if you did it on purpose or 
  are counting instructions to tail call into it or something interesting like that. 
  should detect trying to create a function pointer to it then. 
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
- make writing macros easier
  - make it easy to have to dump out all the macro expansions so you can see what's going on 
  - have the inverse of `@[]`, you want to be able to describe the structure you're 
  expecting as an argument and pick bits out
  - allow attatching a parent scope to an expression so you can split a macro into 
  sub-fuctions without nesting them lexically (see backend/meta/template for a painful example)
  - somehow preserve the original structure so your program can know the types of stuff 
  and also the shape of code at the same time. currently sema stomps on the nodes. 
  (see graphics/shaders for a painful example)
- more stuff with the spread operator `..`,
  - in a call: maybe force that for varargs? or have it mean to look for default arguments in the function? 
  both of those would maybe feel better if they just worked but maybe you'd rather 
  have it be opt-in since it makes the overload resolution situation even more expensive. 
  - in a structliteralp: update syntax? but it would be so easy to just write a macro like `@objc_set` for normal structs.  
  - as a binary operator: give a new overload you could use for indexing like `arr[a..b]`
- give macros access to the requested type
- fomalize init from literals? like implement `Expr::Tuple` -> `Array` in "userspace"?
but that's really going towards crazy town (cough swift/c++ cough)
- formalize something for running code at the end of compilation 
(instead of using hacks like set_self_hash)
- probably shouldn't let you access root scope through a namespace. 
```
a :: 123;
A :: @struct {
  b :: 456;
};
@assert_eq(A.a, 123);  // kinda weird
```
- tail calls
// TODO: destructure through a pointer? 

## demos 

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
- tcc at comptime. use their assembler for AsmFunction

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
- this would be much less messy
```
#use("@/examples/import_c/ffi.fr");
Stb :: fetch(
  "https://github.com/nothings/stb/archive/f0569113c93ad095470c54bf34a17b36646bbbb5.zip", 
  1754150, "b62be3009cb9d4b6385bd4854bddf72d8e91f4e19d1347d6cf9589b19f70d45d",
);
StbTrueType :: include { C |
  C.define("STB_TRUETYPE_IMPLEMENTATION");
  C.include(Stb, "stb_truetype.h");
};
```

##

- Wasip1Libc
- things that should be cleaned up next time i :UpdateBoot
  - :DumbNameAlias NoMangle
  - AbiHackSpan
  - `@rec` in backend/ir.fr and wasm/instructions:Wasm
  - use sqrt/min/max
- fix overflow when lexing large float literals
- show const fields better in log_type
- less verbose if let/let else? think about how to express with a macro.
- i still think wasi's kinda dumb but running the compiler in a browser would be very pleasing.
  tho i could just use blink https://trungnt2910.com/blink/blink.html
- be nicer about warning invalid arguments in examples/default_driver and compiler/first
- parse zig headers and generate extern declarations (like examples/c_bindgen). then i could use bits of thier standard libary.
  they have a bunch of fun stuff that doesn't use use comptime in the api and im not interested in writing myself:
  http, hashing, random, zip, magic numbers for wasm/elf/dwarf/macho
- should make it easy to build rust/zig projects from your driver program since they can generally cross compile well and have libraries you might want.
- do i want to expose the idea of different libcs? musl vs glibc vs cosmo

// TODO: derive ne from eq so you can use != more often. 
// TODO: better error message for *T vs **T, should say dereference not unsafe cast. 

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
- c style variadic functions
