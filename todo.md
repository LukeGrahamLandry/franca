
- fix the spam from examples/web/build.fr, clearly this isn't true because it builds. 
  `ambigous overload for void -> void;0 matching options` 
- import_c `typedef void V; void foo(V) {}` Assertion Failed: cls(TY_VOID)
- this should work on wasm too. 
```
export function w $main() {
@start
    dbgloc 9876543210, $__this_symbol_doesnt_exist__
    ret 0
}
```
- qbe_frontend.fr: parse `retc %s.64, :T3`
- TODO: for the hare urls, they do let you just download a tar file:
  https://git.sr.ht/~lukegrahamlandry/franca/archive/5671fd344d80207427f37d391435fa986839bcb3.tar.gz
- use peek_cstr everywhere i `s: []u8; s: CStr = (ptr = s.ptr); s := s.str();`
- find someone who will give me ci that runs on riscv
- delay evaluating top level code when import()-ing a file?
  like if you try to `@run println("a")` in alloc/arena.fr it's "Poison expression InProgressMacro."
  either make it work or make top level code a hard error. 
  don't just have magic files that give an insane message if you try there. 
- setting FRANCA_MORE_CACHE=1 also needs to invalidate the FrIncr'files_unchanged
- when import_c/ffi makes a cache file it needs to include itself as a dep!
-     very concerning that boot with USE_VM doesn't work when -unsafe and 
      opt passes somehow doesn't work specifically for replace_switches (had to skip it to get the measurement). 
      doesn't matter because its going to be rewritten anyway
      but it might be revealing an actual bug so should investigate further
- make it more sane to jit a franca program from another directory
  - don't just always shit a "traget" dir in the current dir
  - be consistant about always checking the franca lib root dir then the current dir. 
    ie. `franca examples/sudoku.fr` works anywhere you run it but `franca examples/chess/gui.fr -jit` 
    doesn't because include_bytes doesn't fallback to the root dir. 
  - make sure the inverse works too. you should be able to write a .fr script that doesn't live in the root dir
- go through and add cases to check_opt.fr for everything in backend/opt
- should error if you try to make a bake_relocatable_value which will never be called because the type doesn't contain pointers
- deal with Crash'hook_backtrace();
  - when aot it gives you more information 
    but when running jitted it replaces the compiler's one 
    so you have to keep toggling back and forth which is ass. 
  - and you don't want to just not request_backtrace_on_signal
    if __franca_aot_debug_info is null because they might 
    be going to add thier own later (like the compiler does).
- actually use hash_archive !!!
- tests/external/tcc.fr
  - fix my arm-linux abi
  - bootstrap on amd/rv
  - skip fewer tests (import_c is missing some features)
- tests/external/wuffs.fr uses thier committed generated c code. 
  do i care enough to bootstrap a go? probably. the more import_c tests the merrier. 
  https://github.com/golang/go/tree/release-branch.go1.4/src/cmd/dist
  its only 1.4 -> 1.17 -> 1.20
- replace all the `start := getcwd(); chdir(foo); work(); chdir(start);` 
  with `push_cwd(foo) {| work(); };` = `start := open("."); chdir(foo); work(); fchdir(start);`
- #reexport something with main doesn't get picked up by find_unique_func. 
  because find_in_scope doesn't recurse. idk if i want that to work. 
  need to decide how get_constants should work because they should stay consistant. 
- nightmare hour: don't hardcode "./target" everywhere
- i'm not sure what to do with open_temp_file. is it better to use the TMPDIR (/tmp or whatever) 
  so the os knows it can discard them at some point 
  or to have a blanket policy of "i never write outside the ./target directory"
- it's dumb that my opt/promote is defeated by add 0 
  so if your frontend is lazy and always emits the add for struct fields,
  i won't promote single field structs to scalars early. 
- sort the yes-deps tests that need clang vs ones that just need to download something but then use only my compilers. 
  - sort the ones that need libc out of the no-deps tests
- wasteful that i actually produce 0xDEADDEAD in a register when 
  there's a codepath with a stack slot used before defined and can't prove its unreachable. 
  but if you don't do that its crazy town where uninit memory can have a different value every time it's observed. 
- deduplicate identical basic blocks
- the hare test runner is much prettier than mine
- allow #fold on a single argument and have it specilize the function if that argument is constant. 
  use that to make println("string literal") not do two syscalls. 
  need to be able to check if an expression is constant. 
- run the basic tests in @run as well
- things i broke when making @enum and @tagged auto derive
  - // :TodoLostTypeName 
  - // HACK: @rec
- more tests: https://harelang.org/documentation/extlib.html, https://harelang.org/project-library/
- arm has ldr(base reg + shifted offset reg)
  should match that in isel (good for indexing arrays)
- less trash error message for missing `=>` in switch prong than
```
panic! lib/macros.fr:381:55
                        body = @{ @if(@[cond], @[action](), @[body]) };
Compile Error: not callable V:()
```
- fix tests that depend on order they run in for binaries to be compiled. 
  (ie franca examples/import_c/test/test.fr -w target/w.out).
  they should always compile the thing themself so its easy to test multiarch/orb/whatever. 
- `Compile Error: redeclared constant % as overloadset` 
  means you can have things that work if one is main program and imports the other but not the reverse. 
  because `foo :: fn` in the imported thing will be scoped correctly but `fn foo` will be lifted. 
- bloat2: don't count bss. rn EXTRA can be negative. 
- add a test that tries to bake a bunch of stuff in the general allocator to catch the mistake i made with panic_on_volatile_bake more reliably. 
- tests that are supposed to panic_on_volatile_bake
```
main :: fn() void = {
    foo :: @ref 123;
    bar :: temp().boxed(i64, 456);    
    println(foo[]);
    println(bar[]);
}
```
- without MarkNotDone in for lit_fn, its sketchy if you use something 
  that's #avoid_shim inside an @run block (like CVariadic). 
  maybe it's fine. the workaround is just wrap it in `(fn=())()` 
  which is dumb but since it never comes up who cares. 
- i really need to make the zero value of Option be None
- should have a thing for getting the current scope. 
  get_constant doesn't recurse up parents. 
  ex. builtin_macro would like to eval a symbol as though it were written in the source.
- each build_for_graphics program gets a new 300k cache file for its driver. 
  - also it shouldn't statically call make_exec because that compiles the whole output_wasm_module_jit
- add wasm binary to repro. really should just include hashes of everything that's expected to be platform independent in the build artifact. 
- something that shows you unreferenced constants across compilation units. 
  also let driver do something less cringe than get_tagged.
  so maybe add back compiler event hooks
- make backend/meta/test.fr without -bin not rely on #! / franca symlink
- there's a bunch of problems with how i do debug-info but i kinda don't want to fix them 
  because i should throw it all away and use dwarf instead so i can use other people's debuggers. 
- for debug info in exe via frc, 
  headers&payload part need to be done when doing the exe because they have machine code offsets
  but the source&files need to be in the cache file from the frontend. 
  so seal_debug_info needs to be split into two. 
- extension of that is that the embedded source doesn't have the same reachability thing 
  as the code so if you use a big comptime thing, the functions won't be in aot binary
  but the source might if you ask for that. maybe that's what i want because i like the idea 
  of being able to recompile from that but it's a bit counter intuative. idk. 
- better error message than "failed to guess type" if you do `@print("%", fmt_hex(undeclared_variable.foo.bar));`
- add a way to -d log all the comptime code from the driver. rn it only affects the runtime module. 
- mangle symbols in a more stable way than fucking sequential ids. 
  the current way is fast but makes diffing them a pain in the ass.
- things that are inlined and whose body contains a single call still don't reliably show in backtrace
- get rid of comptime in tls. 
  also have to get rid of the places i was lazy and call current_comptime() directly. 
  (@unwrap,@err,make_error,set_type,log_it)
- os: use envvars instead of `nocache on` so it gets inherited properly and this:
  `franca examples/os/build.fr -vzf -append "nocache on;compiler/main.fr"`
  behaves the same as on native. add logging init_self_hosted so you can tell. 
  it should loop repeatedly compiling itself. similarly in web demo, should inherit cli args the same way.  
- fill_pending_dynamic_imports should care which lib the symbol comes from.
- compile_cached should use comptime_libs not fill_from_libc.
  but that's the codepath where you don't make a CompCtx, so .frc needs to tell you where to find the dylibs?
  usecase is letting graphics programs be cached. 
  probably better to have easy.fr use some api that lets you add libs like wasm imports do
- instead of printing pointers in @trace, print thread local allocation index and offset so it changes less in diff?
- better error messages when you have an unfilled fixup on wasm. 
- please please cleanup the got_lookup_offset stuff for wasm
- (when jitting) FRANCA_BACKTRACE from host in import_wasm show both wasm and normal function names.
  need to be able to inherit the resolver from the compiler. 
- `f :: @as(rawptr) fn() = println("A");` you don't get an inferred name so it doesn't show nicely in trace. 
  (the confusing example was report_called_uncompiled_or_just_fix_the_problem)
- removing pare broke ENABLE_GVN=false `compile_expr__1691 Wanted alias for RTmp:1849`
- prospero doesn't work in wasm because my atof doesn't support `_1069 const 4.76837e-07`
- add a test for the shallow force_default_handling bake
- deal with the webgpu wasm imports having hacky thing that doesn't work with caching in an attempt to make dawn.json optional
- the thing where when you dot access a scope and it doesn't have that constant it recurses up all the way to the root scope is confusing. 
- better error message if you try to bake an allocator with a data pointer. 
  should also be more careful about passing around the source location 
  and keeping track of the tree of types you saw before the error. 
  its a bad sign that every time i make that mistake 
  i have to add spammy logging and recompile the compiler. 
- wasm: if no stack frame don't load, sub 0, and store sp. test with env.ssa/main. 
- also, import_wasm of env.ssa/main, wtf are we doing man
```
// should use LocalTee
LocalSet(3);
LocalGet(3);
// wasteful and also generates dumb asm. should fix both
I64_Const(42);
I32_ceql();
I32_EqZero();
// stack thing again
LocalGet(0);
I32_Const(0);
I32_add();
LocalTee(0);
GlobalSet(0);
---
%wasm.92 =w ceql %getL3.91, 42
%eqz.93 =w ceqw %wasm.92, 0
---
cmp	x0, #42
cset	w0, eq
cmp	w0, #0
cset	w0, eq   
```
- bake_relocatable_value for incremental.Header and runtime.Instance
- produce a benchmark that is helped by aligning things in qring.fr to cache line size
- interactive_read_line prints garbage spam when the line is wider than your terminal
- make sure not to keep sending events after a graphics program panics
- don't hardcode pixel format in init_browser_wip_frame
- need more printf to run the import_c and ssa tests
- simple standalone wasm web example (without workers) to show you don't need to do my whole os-userspace thing 
  (maybe provide same api as my old ChessBot would be an easy thing that isn't totally trivial)
- make a driver demo that gives warnings for unused variables
- should default to -keep-names so you get nice stack traces. 
  have -strip be explicit if you want small binaries. 
- elf: fix the first 0x4000 (commands_size_guess) of text section (at least when Relocatable probably also exe)
  being marked as executable but being elf header junk, etc.
  so objdump tries to disassemble it and you get something confusing. 
- elf: names for the symbol import stubs that show in objdump
- Terminos/tcsetattr confuses me. 
  theres like a parallel thing for configuring if you're going to do a blocking read or poll
  if the file descriptor feels in its heart that it's a terminal? 
  im imagining stdin/stdout just being pipes and it doesn't matter if the other end of your pipe 
  is a file or a process or a "terminal". maybe the idea is its a different thing when we're
  pretending theres a device driver on the other end. so a tcsetattr should correspond to 
  setting some magic bytes in the uart device memory. 
  - https://man7.org/linux/man-pages/man3/termios.3.html
  - https://man7.org/linux/man-pages/man2/TCSETS.2const.html
- elf_loader set auxiliary vector and use it to get the right page size for page_allocator
  - https://man7.org/linux/man-pages/man3/getauxval.3.html
- sad redundant work:
  - it runs the backend twice when something is used at comptime and runtime,
    even if both are the same architecture, and even the early passes where i carefully kept them target independent. 
    (easy example: fill_tree and codes_from_lengths)
  - inline_for makes it re-sema even when thats not helpful. 
    sha256.update would be better off if i could just generate the ir once 
    and require the backend to unroll the loop. 
- `u1, u2, v1, v2 : Ty(f32, f32, f32, f32) = (0.0, 1.0, 0.0, 1.0);  // TODO: allow this :compiler`
- using @switch on an enum instead of @match is an easy footgun because it expects @case(.foo) instead of fn foo() and then you'll be confused
- Type Error when calling a function ugently needs to show both the call site and the declaration site
- don't just crash at runtime when you `import_c/cc.fr -r`
  and try to call a function that was forward declared but not linked against
- make stack trace debug info work accross multiple compilers. it needs to go in GlobalFrancaRuntime
  (test with crashing in examples/repl.fr when running it as a driver)
- `f :: fn() = ` isn't getting an inferred name (report_called_uncompiled_or_just_fix_the_problem)
- make it easy to run all the tests with qemu-user
  - -L /usr/x86_64-linux-gnu
  - document what needs to be installed: qemu-user-static, libc6-amd64-cross, libc6-riscv64-cross
- more stack trace improvements
  - macros don't always get a block. 
    my hack is manually calling .with_loc in @assert/@panic but it would be better if it could "just work"
  - another source of confusion is that for comptime code, source location doesn't come from 
    the fake debug info, it parses the string and finds that function call in the ast. 
    make that work with #inline too. 
  - allow hook_backtrace only if not already hosted
  - give a franca_aot_debug_info when running directly (`franca crash2.fr`) so it works if you do your own hook_backtrace. 
  - tests for backtraces
  - programatic access to the data from resolvers instead of just a string. 
  - disassembly annotated with the debug info
- life would be better if i tracked line numbers instead of byte offsets. 
  also that would make it trivial to move them between .frc modules. 
- you need a trace of what was being compiled when you get a compile error. 
  like the chain of references so you know why you're trying to compile that function. 
  - wip: kinda works but often i pop off all the errors because they're allowed when doing overload signetures (which is stupid). 
- replace import_c/includes.fr with a .frc module generated from the franca libc bindings. 
  - wip: only did it for string.h so far
- better apis for working with paths and temporary files
- use `#macro #outputs(T)` to give a more sane error message for `x: i64 = @is(foo, .A, .B);` etc. 
- improve interactive_read_line

---

- crack down on smurf naming conventions (ie import("@/lib/alloc/debug_alloc.fr").DebugAlloc is kinda dumb). 
  could steal what zig does where a file is a struct. but that always annoys me because it makes you 
  pick one blessed struct to have the top level fields when the rest of your file is a namespace which looks odd. 
- real error handling for lib/sys/posix.fr. need to be able to remap the errno values to something consistant. 
- i hate the vscode/zed extension build junk being in here. each are 300 lines of dependencies. 
- `@inline` at the callsite 
- need an option to make `@safety` assertions give you more information. it's hard without a runtime bootstrapping 
  step because ie. `fn index([]T)` needs to get compiled really early before you can do anything. 
- do something for detecting if you discard a Result without unwrapping it
- the pattern of interning things with a List(V)+HashMap(V, index) is common. 
  should make the map give you a way to not store the keys twice. 
- i need a way of testing the global debug settings in core.fr. 
  maybe -DFOO=true passed to default_driver could replace a declaration like `FOO :: false`
- need to auto-test static linux. i wonder if you can landlock away `libc.so` to make sure you can't cheat 
  (answer: yes! easy way is to just use bubblewrap)
- create a guard page when allocating a new stack to spawn a thread
  and have the backend do the probing thing where you read a byte every page when you have a large stack frame so you can't miss the guard page. 
- since do_codegen tries to do tracy stuff, FRANCA_TRACY=1 doesn't work if you try to run something 
  that uses import_c not through default_driver. ie. `FRANCA_TRACY=true ./trace.out examples/terminal.fr` 
  crashes but `FRANCA_TRACY=true ./trace.out examples/default_driver.fr build examples/terminal.fr` is fine. 
  relatedly, since that's not included in cache invalidation, it's broken if you turn on import_module caching. 
- allow trailing lambda to be passed as a function pointer but still infer types of arguments. (ie. when calling run_tests_main_threaded)
- set a good example; don't have tests that rely on layout of codegenentry. use the functions on the vtable. i think import_c/test/test.fr does this wrong
- always zero struct padding when baking constants (even when behind a pointer and even when the struct contains no pointers). 
- can't boot/strap.sh with FRANCA_NO_CACHE=1
  - :UpdateBoot
- document `store v, [Sxxx]` vs `store v, Sxxx` on amd64
- extend the cross repro tests to all the example programs. not just the compiler. maybe just add a file with hashes of binaries to the released artifact. 
- I need to improve @enum for bit flags so i can use that in posix.fr so it doesn't suck as much to call mmap. 
- i think bake_relocatable_value always gets a jit shim which is a bit wasteful. 
  happens because get_custom_bake_handler just calls vtable.get_jitted_ptr which 
  is the same thing handed out to user code for builting vtables so it delays compiling hoping 
  you wont actually try to call it (which is often the right choice, like for objc bindings, etc.). 
- everywhere i ENABLE_ENABLE_TRACY is garbage. i need a better pattern for that. 
- make #export work on `foo :: fn() #` instead of only `fn foo() #`
- at some point i have to go through and make error handling not suck as bad. 
  there's a bunch of places i do `if thing_should_work then do_thing else not_that` instead of 
  `do_thing catch no_that`. file_exists -> open_file is a common one. 
  twice the syscalls and you produce toctou problems for no reason. 
  the reason it's annoying to fix is you want to know why something failed not 
  just that it failed so then i need to deal with remapping errno values for the different targets. 
- repl.fr doesn't work at comptime (`main :: fn() = @run { ...`) 
  on arm-macos because it needs to call apple_thread_jit_write_protect.
  should allow not writing to exec memory and call a tiny bit of aot code to do the copy when you're done each function. 
- #log_ir needs to do something in declare_alias
- stop doing superstitious fence()
- it would be nice to have a good assembler for AsmFunction. 
  the current thing is just enough more verbose that it looks confusing. 
  whatever it is has to stay just a user space comptime thing tho, not part of the compiler. 
- might be able to get rid of is_wrongly_illegal_instruction now, but maybe it's safer just to leave it for good luck. 
- remove implicit dependencies in the tests.
  ex. some of the tests/extenal depend on import_c being compiled at target/cc.out
- use O_EXCL for fetch_or_crash, write_entire_file
- be more structured about the random temporary files in tests. 
  i don't care about the names so should use mkstemp or whatever 
  so you can run the tests twice on the same file system at the same time. 
- stop using environment variables so much. they annoy me. 
- run_qbe_passes_common takes 170ms on wuffs_jpeg__decoder__load_mcu_blocks_for_single_component_smooth

## things i don't autotest

- many i compile in ci but don't run
  - run_tests.graphics_demos (@/graphics)
  - run_tests.deps_compile_only
  - run_tests.dylib_compile_only
  - tests/compiler.compile_only
  - examples/os/host/vzf
  - examples/aoc/2024
- some i don't even compile in ci
  - examples/aoc/2025
  - examples/os/bin (i.e. doom)
  - examples/toy
  - tests/external
    - raylib
    - wasm_spec
    - todo/musl
    - todo/coremark
- that import_wasm works on wasm modules generated by other compilers
- anything on riscv
- wasm verifier (i run some of the wasm tests in my import_wasm but that's less strict than real browsers about runtime "signature mismatch")
- anything about error message quality / source locations / backtraces
- anything about compilation speed
- static linux binaries
- cross compiling
- that my relocatable object files work (other than the .ssa tests)
- that my dynamic libraries work (other than trivial tests/exe/dylibs.fr)
- that all the tests would work if run in the comptime jit (won't because of apple_thread_jit_write_protect)
- reproducible builds of programs other than the compiler
- all the slow DEBUG options in core.fr
- serious use of backend env parameters since i don't use them for tls anymore
- FRANCA_MORE_CACHE
- anything about cache invalidation
- that all the tests run in bubblewrap sandbox (rn i autotest is just hello world)
- boot/src/driver.fr
- things in lib/encoding are used by the autotest so won't silently break
  but don't have standalone tests so if it breaks it will cause a confusing failure elsewhere. 

## remaining nondeterminism

- exec has the same schedualing problem as threads but is harder to fake 
- aslr for where the executable is loaded and what addresses mmap gives back
- clock_gettime

> (note) solutions for others: SLOW_USERSPACE_THREADS, NEVER_USE_LIBC_ALLOC

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
  - leak checking for page_allocator
- should log when the slow things are turned on so i can't forget to turn them off. 
- need to give better source location for #inline and @macro. each ip can have a stack of locations. 
- SLOW_PROTECT_ARENA doesn't work with SLOW_DEBUG_ALLOC because the protect forces it to be leaked. 
- tests for all this somehow. need to make configuring it more programatic. so choosing a different allocator at top level
  (like how DebugAlloc works) is more convient than just checking the constant in the implementation (like how ArenaAlloc works). 
- nicer interface for bloat.fr, make you a pie chart of function size by file or something idk but the list is kinda unreadable. 
- compiler bug:
```
// fn(buf_ptr: i64, buf_len: i64) {
// this should work: 
// bufs := bit_cast_unchecked(Ty(i64, i64), [][]u8, (buf_ptr, buf_len));
// and this shouldn't need the @run to avoid `cannot .Aot .ComptimeOnly F47 (fn operator_star_prefix` :compilerbug
bufs: [][]u8 = (ptr = bit_cast_unchecked(i64, @run(*[]u8), buf_ptr), len = buf_len);
```

## import_c

- the line numbers in my error messages are wrong sometimes!
  shows the right text tho so its not a massive deal but kinda cringe 
- make sure my detect_include_guard is working on all the system headers 
- i get "unterminated conditional directive" on apple's unistd.h without -D_POSIX_C_SOURCE=200809L
- C23: `__has_c_attribute`, allow `[[]]` instead of `__attribute__`
- import_c add a test for the incorrect include guard thing i fixed on jul16
- import_c/cc.fr searches include path for the starting file before your current working directory which is super confusing
- turn off Qbe.Fn.track_ir_names in import_c
- :ReWalkTokens
- implement _Atomic in import_c
- :AttributesNotYetImplemented
- import_c: `__constructor__, __aligned__`
- import_c, get rid of :BoundsPadding
- import_c: if you call something at both comptime and runtime it needs to redo the 
c frontend work because of `#ifdef ARCH/OS`. C also lets you have different function/struct
bodies on different targets which i don't deal with well. 
- for import_c, decide when to reset temp() and audit all the allocations in tok/pre
- import_c/ffi has a big comment
- i want import_c/ffi.fr to be able to use threaded=true but compiler flaw: 
  interaction between spawning threads at comptime and jit-shims is super broken 
- hoist the enter_task call so temp() can be reset more often
- either do tokens lazily or try doing it on another thread
- need more control over exports. currently any frc file has all the libc stuff you included reexported. 
  which will be confusing if you try to #use it in franca
- when including a .frc but also outputting a .frc, don't copy everything into the new file, 
  just have it as imported symbols that reference the old one. 
- :BrokenCGeneric i think erroring on conflicting `_Generic` cases is correct but you're supposed to treat `long` and `long long`
as different types even when they're the same size.
- generate c headers from .frc files
- option to use system headers instead of my builtin ones
- make it good enough to compile impressive things
  - blink
  - musl
  - git
  - linux kernel
  - sqlite
- :AsmNotYetImplemented
- for inline assembly, i think it would be easier to give up on ExprLevelAsm
  and use someone else's assembler to compile the `asm` blocks into an elf file 
  and then just call them as functions. they give you clobber lists so you just have 
  to put some code on either side to translate that to the normal abi. 
  probably have to deal with more types of relocations. 
  are you allowed to jmp between asm blocks? 

### !! BROKEN !!

- self compile in blink on (arm-macos and arm-linux) on github actions seems to hang forever sometimes? 
- ./boot/temporary/macos-amd64.sh with SLOW_USERSPACE_THREADS=true
```
pragma-once.c                           [ok] 

All is fine! (passed 35 tests)
1.fr                                    note: run with `FRANCA_BACKTRACE=1` environment variable to display a backtrace%     
```
- import_c can't do SLOW_USERSPACE_THREADS when run directly on a compiler that was build with real threads because of "using host backend"
- can't run tests/c.fr at comptime (@run main() hangs)
- tests/exe/sys.fr might not work with SLOW_LEAK_ARENAS=true?
- i've seen this a couple times on actions:
```
>>> unpacking [target/franca/deps/fonts.zip]...
Assertion Failed: failed to read file 'target/franca/deps/fonts/ttf/JetBrainsMonoNL-Bold.ttf'
```
i think it's just a race where it gets confused if two programs try to cache the same dependency at the same time. 
- `./target/f.out tests/exe/wasm.fr` doesn't work with SLOW_MEMORY_DEBUGGING=true
- `franca examples/os/build.fr -vzf -smp 2 -append "nocache on;spawn kaleidoscope;kaleidoscope;"`
  it doesn't like two at once? (with -smp 1 it works but is SUPER slow which should also be fixed)
- TODO: i broke compiler/test.fr running examples/repl.fr (Cached) on riscv
- spurious failures
  - (repro) diff target/release/franca-linux-arm64-sta a.out 
  - TODO: i broke repro. only of linux-rv64-sta and the only difference is two instructions swapped:
```
00154490  93 02 05 00 13 83 05 00  b3 84 62 40 93 03 00 00
00154490  13 83 05 00 93 02 05 00  b3 84 62 40 93 03 00 00
```
it seems to work most of the time? so thread ordering problem?


## import_symbol / weak

TODO: my @import_symbol always does a weak symbol even though it in the macro body i say weak=false.
TODO: test that makes weak and non weak of symbols i know don't exist and makes sure the dynamic loader crashes correctly. 
TODO: tests that @import_symbol give you null for a missing weak symbol (instead of address of a stub like when you #import a function), 
      it works right now but is fragile. 
TODO: fallback to the syscall version automatically if a weak symbol is unavailable
TODO: @import_symbol of non-existant throws TraceTrap at comptime
TODO: be consistant about spelling: zeros or zeroes

## COMPILER BUG 

- #use is ordered
- tests/todo
  - a.fr: first_ref_os, const_field_unordered
  - d.fr: OverloadingConfusedCoerce
  - f.fr: missing ; should be disallowed
  - bad performace of linked list of `body = @{ @[body]; @[next]; }`
- fix the infinite loop when a constant references itself
- `@struct(a: A #use = (),);`
- // TODO: don't segfault if you get a compile error inside a jit_shim. 
- import_c: tests/todo/b.c
- same string constant as Str and CStr :MiscompileCStr
- :ConstInFuncOrLoseTemp
- literal for a 64 bit integer with the high bit set shouldn't need a bit cast 
- :UseDoesntWork
- :BitFieldsCompileError there's places in Qbe and Incremental where it won't let you have 
a field of type @bit_fields or you get:
```
panic! compiler/main.fr:2:1
#include_std("lib/core.fr");
Compile Error: 54 matching options for index

TODO: end of loop. still too many options for 'index'
```
- #inline returning Never doesn't remember that it returns Never
- there's still a compilation order problem with inlining intrinsics. look at copy_bytes(). 
- :ThisIsNotOkBecauseMemoryWillBeReused

## random failures

- failed to read file 'target/franca/deps/fonts/ttf/JetBrainsMonoNL-Bold.ttf'
  unpacking race probably

## 

- put more stuff in read only data. 
  - maybe have @static and @mut_static. same for @const_slice. track that in PageMap? 
- #log_ir should fire multiple times for functions with $const parameters (ie. native_isel)
- shouldn't be able to typo a name as easily. like S :: import_module{enqueue :: enqueue_task}); then S.enqueue_task will get you the wrong one and be slow. 
- why was the quicksort wrapper trying to be emitted for import_module (when not marked #fold)
- get compilation order dependence under control!!
- fix callgraph sorting to improve inlining. like make sure the ge/le in lex_int/is_ascii_digit are inlined 
- @bit_fields in incremental.fr don't work inline in the structs
- #ir tries to ignore zero-sized params but not if they're first which is sad
- "need to be consistant about how to handle modules like this that don't actually compile anything"
- reduce disk usage bloat. rn fetch_or_crash stores the unzipped thing and the zip file so you have everything twice. 
many of the things i use i don't need everything from so it could make you tell it which files are important and 
delete the rest. thing to think about is that you want to union those between different projects that depend on 
different subsets of the same resources. 
- document #weak: docs/(annotations.md, imports.md)
- deal with `NOSYS`
- make #log_asm work for the #asm replacement 
- experiment with outputting even more info in .frc and an lsp that reads it back. 

## wasm

- remove all the TODOWASM (ifdefs/comments)
- let frontends directly provide signeture for imports since they probably know instead of only trying to infer from callsites. 
  (currently emit_ir just always outputs a shim with a direct call which works but is hacky and not something i'd be proud to explain). 
- generate better code (see comments in wasm/isel.fr)
- (see comments in wasm/abi.fr)
- finish PromotePointers in import_wasm/run.fr
- make AsmFunction not suck
- refactor output_wasm_module_jit so it shares more code with the aot version
- it's tempting to expand into allowing the jitted module to be reused instead of hardcoding the first_export. 
  that needs data relocations for function pointers, at which point i should just give up and follow the convention other tools use. 
  - https://github.com/WebAssembly/tool-conventions/blob/main/DynamicLinking.md
  - https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md
- make import_c work without hacks
  - setjmp/longjmp with exceptions
- import_wasm working in wasm would be cute. 
  - dont reserve giant virtual memory
  - don't depend on libc (import_wasm/run.fr/Exports for the .ssa tests)
- stack traces
- make the wasmtime version work
- add a .ssa test that tests dynalloc with a deeper callstack
- in wasm/make_exec use debug_out when dumping module so you can redirect it
- make sure `::@as(rawptr)(fn() void = ())` gives you the got_lookup_offset not the junk jit_addr. 
- pass the tests
  - cast.c, float.c, literal.c, fpcnv.ssa: `e` in float literals
  - macro.c: `__FILE__; 1 != 0; ends_with(main_filename1, "test/macro.c")`
  - varargs.c, function.c: vsprintf
  - pragma_once.c: `redefinition of function 'assert'`
  - abi5.ssa, abi6.ssa, abi8.ssa: the case in my hacky printf that calls snprintf
  - echo.ssa: `$main(w %argc, l %argv)`
  - vararg1.ssa, vararg2.ssa: vprintf
  - compiler/main.fr jit: `wasm-jit $__franca_aot_debug_info should have known got_lookup_offset`
- add the small franca tests to the web demo
- add the import_c things with dependencies to the web demo
- web demo: hare, wuffs, more nontrivial c programs
- use dump_wasm to disassemble one function at a time (like i do with llvm-mc) 
  instead of only all at once in output_wasm_module_jit
- improve import_wasm until it can cope with bootstrapping zig1.wasm
- kinda lame that web/demo.fr has to special case the graphics programs instead of drivers working properly
- examples/terminal.fr stbtt__run_charstring panic! missing br_if target @120 -> @122
  - ugly=false
- examples/terminal.fr: Uncaught RuntimeError: function signature mismatch
  - ugly=true. regression! this used to work. (in browser) 

## import_wasm 

- speed it up! go back to f15cf8315db7d0040f4a3ebf017637c5273ae00b make it that fast again. 
  the two big changes were:
  - intercepting calls to use examples/os/user/libc
  - import_wasm generating pic and passing around the runtime context to allow threads. 
    made it slower to access memory, globals, and imports. 
  `./target/w.out examples/web/target/demo.wasm -- -lang franca -file examples/kaleidoscope.fr`
  f15cf83-importwasm: 2676  
  0196148-importwasm: 5359  
  current-importwasm: 3197  
  current-importwasm: 2904  

convert.fr:  
// TODO: this could be hella faster: 
//       - there's a wasteful prepass that splits up bits of the module into a big data structure. 
//       - the parser+wasm->ir could be on a different thread from the ir->asm 
//         (i have the machinery to do this already because the Franca compiler works that way)
//         + you could compile multiple functions in parallel (but that would need more work).
//       - the number of bounds checks the parser does per byte is a bit sad. 
//       - could inline more if we pre-sorted based on callgraph,
//         but maybe the expectation with wasm is that the first compiler did that already?

// TODO: validation
// TODO: support aot. needs extra work because of import_wasm/runtime.fr
// TODO: provide standard c api so could run other people's tests? https://github.com/WebAssembly/wasm-c-api/blob/main/include/wasm.h

## backend 

- better error messages for abusing FPad. also for lying about Typ size in frc file (giving too many fields).
- tests/todo/apple_arm_subword_abi.ssa
- macho/emit.fr/emplace_fixup() allow negative offset for DataAbsolute of dynamic import in .Exe
- rm64/emit.fr/fixup_arm64(): offset from dynamic import
- isel5.ssa -w: fails with inlining disabled
- (mem3.ssa, isel5.ssa) -w: fails when constant folding is disabled. 
  `IR failed typecheck: invalid type for first operand RTmp:144 of add in block @4`
- "you really want to lock the module in case there's another thread trying to compile into it"
- rv64/isel: fuse cmp+jnz
- test for phi of stack slot? 
- i don't like that direct to exe vs to frc_inlinable then to exe give different binaries. 
- be able to output frc/frc_inlinable in the same module as compiling normally so you don't have to do two passes over things to cache it
- fix those two ^ and then compiler/test.fr can create all at once and assert that they make the same exe instead of running them all
- elf: don't include names for local symbol with DataAbsolute relocations when exe_debug_symbol_table=false,
- macho exe_debug_symbol_table doesn't work i clion profiler (dtrace?)
- debug assert that all tmps have a definition in rega. 
  (especially because @emit_instructions doesn't catch it)
- arm relocatable import: relocation to fold symbol offsets into the adrp+add instead of wasting an extra instruction :SLOW
- apple has different c variadic abi. that's pretty unfortunate for my grand plans. 
  it feels pretty invasive to have the backend compile two versions of anything variadic. 
  but most functions don't use varargs so those could be rega-ed once for linux+macos when compiling for all targets together. 
- experiment with leb128 for indices in .frc modules. 
need to be careful about the refs which have tags in the high bits so won't leb well directly. 
- why does llvm-mc disassembler arm think my cas is invalid instruction encoding? (check with cas.ssa -d D). 
  objdump thinks it's fine and it clearly runs correctly. 
- TODO: harec/src/gen.c: `[arm64/emit.fr/fixup_arm64] offset from dynamic import builtin_type_nomem+4` but would work when done as one compilation unit.
  when doing .Relocatable, i shouldn't complain early because it might be local. need to leave it for the linker to deal with. 
- mem3.ssa fails without opt/load.fr/loadopt()
- some instructions don't have a .ssa test: float(sqrt, min, max, swap, truncd), int(extsb, extsh, extuh)
- support fini/init sections in elf (and macho i think has a special load command for them)
  - needed for import_c constructor attribute
- easy way to expose c api when it can't pass the environment pointer. 
  sadly might have to reimplement thread locals. (which would be nice anyway so i could support more qbe languages)
- make @emit_instructions not as painful to read. 
  give placeholders names instead of numbers (and pass them as Expr::StructLiteralP instead of Expr::Tuple). 
  or maybe allow tuple if the expressions are just a single variable and use that for the name as well. 
  or maybe if you're just using it once, have `@{escapes}` where that string gets passed
  to the macro and becomes an Expr::GetParsed so you don't have to give things names. 
- document RSlot. t.slot=-1 for None, but S-1 means start of vararg save area
  and other negative S means par passed on stack, 
  and the number is stored as -(off+2) so it doesn't collide with the special -1. 
  qbe stores it at 4 byte granularity but i don't because i wanted elide_abi_slots to work on smaller types 
- get rid of gvm.fr/sink. but first need to make rega do better live range spiltting in large functions. 
- i wish i could get rid of the parts of fold.fr and copy.fr that were replaced by gvn,
  but it feels wrong as long as gvn is still slower. like maybe this is dumb and i should go back to the old way. 
- :InverseIsUnsoundForNanCmp on non-arm?
- why go i get faster core mark with my import_c than my qbe_frontend on cproc's ir? 
  differences i see in the ir:
  - i insert redundant cnew 0 before jnz
  - cproc inserts redundant extub after loadub
  - i insert unused loads
- register allocation
  (problems / confusions, either way i should make it more obvious that it's doing the right thing)
  - only one hint per temp so bad code for very large functions. 
    - similarly spill cost is global per temp so if it's only used 
      in one code path but a lot it's still treated as important everywhere else
    - need to do better live range splitting. it does some, a value can live in a 
      register for a bit, then get spilled, then reloaded to a different register,
      but its always trying to get to the same one.
      which is probably why getting rid of wait lists was an improvement,
      in big functions the hint is wrong a lot of the time. 
  - rega() inserts spills to meet register constraints (call/phi parallel moves). 
    spill() decides which temps get spilled before knowing which ones have to end up in a specific register
  - each temp gets its own spill slot so i think phis might have an argument reloaded 
    just to spill it back to the phi's dest. should put phi args in the same spill slot 
    if its spilled on both ends? temps can overlap tho so thats another place where 
    you want to be able to split them. rn they can only have a single stack slot. 
- be drop in qbe replacement for cproc. its string literals use `\012\000` instead of `\n\0`
- wasm elide_abi_slots
- detect mutable variable that's used undefined in a loop. 
  its annoying because maybe you want to allow it if the user knows the path that leads to it being undefined is unreachable. 
  but if it can't possibly be defined that should probably be an error. 
```
// TODO: if they don't do it already: 
//       - track known truthyness per block (based on jumps in dominators)
//         to remove redundant checks (like bounds checks). that use kinda relies on inlining tho.
//       - jmp on the not (xor -1) of a cmp should just flip the cmp. 
// TODO: don't bother emitting nops (when copying with emiti() anyway)? is avoiding a branch worth your blocks using extra memory
// TODO: :Explain :UnacceptablePanic :SketchyIterTargets :nullable
//       :MakeATestThatFails places where i don't know why qbe did something and all my current tests work if you change it. 
//                           so try to make a test case that requires qbe's behaviour or remove the extra code. 
// TODO: look at the asm and make sure im not uxtb before every cmp #0 on a bool. have to fix the ir i generate.
//       do i need to be able to express that a function returns exactly 0 or 1?
//       once i expose w ops to the language i can just generate those instead of l for working with bools and then it should be fine?
// TODO: dead store elimination would make small examples generate tighter code. 
// TODO: don't do extra copies of arg/ret when just passing through to another call.
// TODO: ir test that uses opaque types
```
- elf can have relocations in bss so should use that for export_ffi_data instead of having 12k of zeroes
- https://c9x.me/git/qbe.git/commit/?id=73f0accb45f80d697e054ee95e9c82adbc512c99
  i already had a debug_assert for that problem but should actually fix it. 
  need a non-insane repo case. 
- should run the tests with inlining disabled as well. 
  a bunch of stuff just compiles to nothing. 
  currently some i try to order the functions in the ir to prevent it but that's fragile. 

## backend symbols rework

- 
  tho it's also pretty dumb for every compilation unit to have its own copy of builtin_static_memmove, 
  but if anybody doesn't have it, you're suddenly in linker hell where you need to find a compiler_rt 
  or some shit every time you try to do anything and the errors are always inscrutable. 
- make sure lnk.export is being respected (and implement it for data as well). 
- you don't necessarily want to inherit export-ness of symbols when including a frc module. 
  that's something wasm got right i feel. each module has some imports and some exports but the 
  names are arbitrary and it's possible to specify your own mapping on how they get filled when loading a module. 
  so you don't have to do the flat-namespace-prefix-all-your-symbols junk that usable c libraries have to do. 
  you need at least two layers of namespaces so you can import two different libraries that export a symbol with the same name. 
  but you also need to allow choosing the unmangled real symbol name for working and playing well with others. 
- unify SymbolInfo.library with the old name$module i was using for wasm .ssa tests
- the weak_imports shit for wasm is garbage. have like vtable.fill_import(library, name, funcid) 
  that you call at comptime to rebind an import to a local function. then you can just have another 
  entry point / driver for targetting wasm that stomps over the weak symbols you don't want to fill normally. 
- allow the same symbol name in different libraries. you should be able to actually use them for namespacing. 
  but also keep the ability to have a named import without declaring where it's from? 
  maybe a special default library name and you fix that for yourself by calling vtable.fill_import later? 
- be consistant about how weak symbols work. it would be nice guarentee the address is 0 if it's not available, 
  but that prevents you from handing out the address of a jit-shim or got-shim before you know if the import will get filled. 
  so im not sure what to do about that. 
- make the mangled name of copy_bytes in simplify.fr/static_memmove not change so the generated module 
  is identical regardless of which compilation context you're in 
  (not a repro problem, just an extra source of confusion that doesn't need to exist). 
- make sure the string "franca" doesn't show up in binaries. ie `__franca_aot_debug_info` symbol offends me
- i want the module made for static_memmove in opt/simplify.fr to not include 
  `__franca_base_address` and `__franca_aot_debug_info`
- test that function names don't appear in the binary when exe_debug_symbol_table=false and do when true.
- be consistant about keeping a list of symbols that might be interesting instead of doing for_symbols multiple times at the end of a module. 
- try to deduplicate filling patches. currently there's a lot of places in emit where you look at symbol.kind 
  and maybe patch immediately if it's ready and maybe save a fixup for later depending on m.goal. 
  it would be much more sane if every action was described by a patch 
  and you just had one function to apply it now or save it for later as needed. 
  the hard thing about that is sometimes the immediate version is more efficient but doesn't make sense 
  as a delayed patch so you want to only fall back to the slower thing if needed. 
- make sure im not redundantly calling clear_instruction_cache. 
  for pending_immediate_fixup_got, i think i might be doing it after the function, 
  then doing patches, then doing it again. so you flush the whole function twice whenever it calls anything. 
- need to be consistant about a place to do stuff at the very end of emitting a module. 
  it's probably emit_suspended_inlinables but it's confusing what Target.finish_module is supposed to do for you. 
  and it's confusing whether make_exec should do something or just assert that you're in a mode that doesn't need it. 
- at the end you should make always sure there are no strong imports that haven't been patched. 

## riscv

- rnez/reqz aren't real instruction in riscv so i fix them to be ult R0 in isel,
  so remove those ops? so remove them? but now i use them in wasm/isel. 
  not sure if its more sane to move the wasm cmp zero checking to emit or let rv use the fake instruction and lower it in emit. 
  i guess there's no winning, one of them can't be done in isel if i don't want to having boring instructions. 
- to make -cc work with static linking, import_c needs to not make weak symbols
- llvm-mc doesn't disassemble float instructions
- isel address folding into load/store like arm
- isel replace cmp+jnz with bcmp (they don't have a flags register)
- (fixup1.ssa, fpcnv.ssa, encoding.ssa) work in qemu but not libriscv. report bugs? 
  - fixup1 and encoding work with --single-step but not without
  - also cas.ssa but there it has an expcilit error message for not supporting that extension instead of just giving the wrong answer like the others
- call local symbol directly without producing it in a register first
- trampolines for imported symbols
- tests/
  - intrins, inline_asm_jit (AsmFunction)
- clang's binary does something with `__global_pointer` in start. do i have to do that?
- ssa test that uses the constant 9223372036854775807
- `thread backtrace` doesn't work in lldb. is my stack layout wrong? 
- riscv_flush_icache syscall vs FENCE.I instruction for clear_instruction_cache
- :TodoRiscv
- panic! TODO: copy slot float

## don't rely on libc

- import_c/tokenize: strtod
- import_wasm/run.fr: 
  - (because .ssa test calls it): snprintf, strncmp
- prospero: atof
- (epicyles, geo): fmod
- (graphics): cosf, sinf
- dlsym, dlopen, dlclose

## hare

  - be a drop in replacement for qbe that harec can use. need to deal with the fact that i don't output text assembly. 
    just provide a script that you can use as hare's assembler that dispatches to a real assembler if it's actually text? 

## longevity

> i want to be confident i could abandon it for 20 years and still be able to run my little examples

- vendor the things i care about: stb(truetype, image, image_write), jetbrains mono font
  - get sdf data out of that and generate my own test data for prospero.fr
  - generate my own laz data to render with geo. im sure there are fun shapes that can be created with little code
  - teminal.fr, view_image.fr, import_wuffs/test.fr (wuffs input programs)
- the graphics stuff is a bit of a dumpster fire
  - slow software rendering implementation as documentation of what it's supposed to do
- make sure everything works just using posix libc stuff as a fallback (syscalls and private apis need to be optional)
  - (openat, ioctl) use varargs
  - glibc fstatat somehow sucks 
  - macos: now sure if (ulock_wait, ulock_wake) are supposed to be public. have an option to use something from pthread
- make the platform detection in franca_runtime_init less hacky
- support (free, net, open)bsd
- the bootstrapping system can't be committing a macos-arm binary. linux-amd seems more old / emulator enthused, could use blink, etc. 
- report all test failures instead of stopping on the first one
- i want the transcribed magic numbers for syscalls, sys struct layouts, instruction encoding, object formats, etc. to be more auditable. 
  maybe make it structured consistantly enough that i can generate a c program that asserts everything matches
  for a certain target when compiled by a normal c installation. for instruction encoding, maybe do more like tests/exe/x64_encoding.fr. 
- output qbe text ir that it can parse so i can test my stuff without my backend implementation 
  (doesn't help for comptime because qbe can't jit)

## linux 

- :TodoLinux CLOCK_REALTIME
- :TodoMacosSyscall
- how are you supposed to ask for page size? blink wants 64k instead of 4k. 
- elf_loader.fr doesn't work on linker output: `panic! not divisible by page size`
- if you have a static compiler and want to link a libc thing
  and notice that the path to dynamic loader is valid, 
  it would be cool to try to do something where you morph by poking in the Dynamic header
  and reexecing the compiler to get it. 

```
* thread #1, name = 'q.out', stop reason = signal SIGBUS: illegal alignment
    frame #0: 0x0000000001016b78 q.out`impl2__7041 + 224
q.out`impl2__7041:
->  0x1016b78 <+224>: cas    x17, x0, [x2]
```
- shader translation for the gui examples
- mprotect .ConstantData segment after applying relocations

## amd64

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
- linux:    make all the tests pass with -syscalls
- external: make it not a 100 line copy-paste to setup a driver that links an object file

## Caching

- caching only happens if the file path ends with .fr
- crash backtrace with source location
- keep caches for multiple targets at once?
- need to be careful if start caching both main+driver from one source file
- have examples/default_driver and graphics/easy do it for main(). 
but then need to deal with including build options in the cache (like -unsafe, -wgpu, ENABLE_TRACY) 
- do automatic caching for big comptime things (like import_c'include)
- allow smaller compilation units. like not making you recompile the backend when you work on the frontend. 
- test that .frc files repro and that you can pass them directly
- dump_bin: print segment.MachineCode as something qbe_frontend.fr can parse so it can round trip
- clear cache before tests just in case
- caching an invalid thing i think? if you Compile Error: we hit a dynamicimport ('puts_unlocked' from 'libc') with no comptimeaddr for jit
(happening with import_c)
- persist #noinline
- decide the policy on mutating the ir module. 
  some of the passes mutate in place and emit() does when fixing arg even if you saved after rega. 
  so i guess if the module needs to be reused it needs to copy the blocks when loading. 

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

this shit with the magic `@/!/` prefixes is stupid. 
just have a struct with an enum field.
(or even just know to check recursive deps if it starts with the frc magic, 
you have to read the file anyway to check the hash so there's no extra information). 
the only difference rn is whether you have to include the target/franca/cache/ part of the path.

### FrcImport 

-  run inlining and if something changes redo the other early passes as well
- someone needs to detect undeclared symbols
- make it work with did_regalloc if you know you only care about one architecture
- check arch in import_frc() if did_regalloc 
- need to merge deps if we're generating a cache file? 
- import_c: local declaration of import doesn't work because it doesn't get a name in root_scope
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
    or everything's confusing, like RESOLVERS, really need to only have one. 
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

## wuffs

- less hacky handling of types (emit_type.Field should not special case the base module)
- implement the rest of the language
  - output constants: arrays
  - suspend when a callee suspends
  - correct abi for large values (parc/argc) and handle assigning them to fields correctly with a blit
  - choose (function pointer typed fields and methods that dispatch to them)
  - Builtin: iterate, io_bind, io_limit, io_forget_history
  - Generic: slice, array, table
  - io_buffer methods don't exist in the c code (they're intrinsics in original wuffs)
- output .frc with frontend types for function signetures
- compile their programs and pass thier c tests (ie. provide same abi as original wuffs)
- less verbose wrappers for calling from franca
- better error messages in emit.fr (like for type errors, etc). 
  need to track location info on the nodes in parse.fr. 
  keep stack of location information and pop from it in Parser.new_node
  just have backwards from a Node be the global offset in the source code. 
- proofs / safety checking 
  - track min/max interval on values
  - assert and loop invarients
  - easy: check visibility / effects
  - check for recursive types / functions
- multithread the frontend. 
  parse and emit should be able to happen seperatly for each module.
  with the latter waiting for any used module to be public_ready=true
- make print.fr output it well enough that it can be re-parsed (including by original wuffs) 
- splice comptime franca code in to generate constants every time you build 
  so you don't have to paste giant tables of numbers generated by a go program that you run seperatly. 
- a demo where you feed it one byte at a time and have a ui that shows how fields/vars in the struct change. 

## Unfinished Examples

- epicycles: make it actually trace the correct path
- lambda: imporove diagrams and implement evaluation
- import_wasm: make it usable for libraries written in other languages

### Chess

- experiment with New Types for index/rank/file/mask so you can't mess up the unit conversion
- have Slider :: @enum(Rook, Bishop) instead of functions with isRook: bool parameters
- regressions (unported):
  - Learned.Weights
  - search: followCapturesDepth + lookForPeace, 
    but i had it turned off in the old codebase so clearly i was doing it wrong. 
    debug / make sure it actually wins more games
  - seach: uci reporting info/pv/time etc. 
  - search: time limit
- i don't think lastMoveWasRepetition works. gui doesn't stop after repetitions. 
- use ZOID_CASTLE_START
- Move would fit in 16 bits. 6 per square leaves 4 for promote+action. 
- gui
  - play against uci engine
  - promotion
  - bit board overlays like the old one (and ui to choose which to show)
  - run search on a seperate thread
  - ui for turning off the bot and choosing which colour to play

### Terminal

- should seperate the text editing model part from the rendering. 
  i need to make it more structured anyway to allow undo. 
- tab to autocomplete a file path 
- escape codes: colour, move the cursor, clear the screen
- send input to stdin
- raw input mode
- catch faults/panics from the repl
- option to reset the repl since it leaks memory indefinitely 
- unicode characters
- && to run two commands
- multiline franca commands
- save cmd history to file as well (not just output) so up/down work across restarts (and maybe auto save)
- jump around past commands like warp
- ui for canceling search
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
- factor out a text widget so its not a big deal to make other gui examples that want a bit of text input. 
- paste containing new line in middle of line shift the end 
- allow limiting scroll back so it doesn't eat memory forever. 
  maybe an option to have it dump to a file when it scrolls off to far so it doesn't delete information. 
- triple clicking past the end of a line should count as clicking on the line and select it all
- make highlighting feel less bad. 
  currently you have to be careful not to drag up past your starting height or you loose it. 
- add some margin on the left so it feels less cramped and its easier to click on the beginning of a line. 
- backspace should delete the whole selection instead of one character
- cmd+x to cut
- left/right arrow key should move to that side of the selection and unselect
- cmd+a to select the whole line
- poll() only gets 81920 bytes per frame.
  the BigOutputBuffer isn't enough, sometimes it doesn't get a full 64k before the next read. 
  if you cat a gigabyte file it takes like 3 minutes
  but its 5 seconds with the poll loop condition changed to `(len != 0 && (len < space || realloc))`.
  (which is still slow but better. and also wrong because then its a blocking poll 
   until the process doesn't get a turn to run between reads tho its fine for my terminal because it has realloc=false)
- let you get out of lock_to_bottom while a process is spamming output
- scrollbar feels bad because i stop sending events when the mouse goes off the window

## Graphics

- offsets on storage buffers (they added it in Resource View Update)
- https://github.com/floooh/sokol/commit/50bbbe4521af356c3b0879e1d46e30114feb4e6b
- tests that reach discard(), compute shader dispatch(), and create(Attachments)
  - finish gfx-webgpu
  - finish gfx-metal
- example program that tries to use all features
- finish gfx validation and enable that based on DebugAssetions
- clean up defaults
- dearimgui backend
- support x11
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
- this happens occasionally: (but i have to redo the whole wgpu shit anyway because just yoloing a specific version of libwebgpu_dawn.dylib is not acceptable) 
  - seems fixed. probably by -jit always using ExecStlye.AOT so baked happen normally (even tho it doesn't actually produce a binary)
```
franca examples/mandelbrot_ui.fr -wgpu -jit
panic! graphics/web/webgpu.fr:1221:34
wgpuDeviceCreateCommandEncoder :: fn(device: Device, descriptor: *CommandEncoderDescriptor) CommandEncoder #import("webgpu");
Compile Error: we hit a dynamicimport ('wgpuDeviceCreateCommandEncoder' from 'webgpu') with no comptimeaddr for jit
the frontend should make sure this doesn't happen. 
TODO: this happens when compiling targetting libc from a compiler built without libc on linux
```
- web
  - safari
  - remap key codes (keytable)
  - modifiers
  - make test programs that force me to implement the rest of the wgpu api surface
  - a bunch of the bindings im writing manually in gfx.js could be generated
  - do the rest of the events in app.js
  - all the app commands are stubs
  - might want to limit the scrolling so the scale of the numbers is the same as native. 
    ie. i had to clamp mandelbrot_ui so you can't overshoot and that isn't a problem in the macos version. 
  - make demo.fr work in import_wasm using dawn for graphics. 
  - the franca side should call requestAdapter/requestDevice/configure so it can pass arguments. 
  - make the js code less messy and make it less painful to integrate into your own project. 
    ideally i could just spit out a wasm file and a js file and it would just work. 
  - its cringe that im pre-generating webgpu.g.js and webgpu.g.fr instead of doing it in the browser like everything else. 
  - be able to precompile the graphics library so the demo isn't as slow
  - make downloading one compiled for aot macos graphics work in the demo and give sane error if you try for linux

## stuff i broke

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

- `@Fn` vs `@FnPtr` vs `FuncId` is more confusing than necessary. 
  it would be nice if @Fn was always const and @FnPtr was always runtime and they were the same type. 
  because then assigning a const @Fn to a runtime variable would give you something callable without typing out the signeture again. 
  but they have different representations and you can have a the other combinations they're just not useful. 
- :NoInlineImport
- ._N and .len on Array
- #use field in guess_type for #where
- make auto deref always work (you shouldn't need to `[]` for returned structs)
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
- still allow coerce to c string if there was a `\` escape.
- this should work
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
- @rec and #generic suck
- reporducible builds depend on compilation order because of var id (like if you iterate a scope) 
or function ids which end up in your binary. this is actually fine currently because 
the frontend is single threaded but maybe it's not a great idea in the long term. 
- don't evaluate the spread value of `..` multiple times
- make `..` work when it's redundantly spreading a single item (rn it's an error)

## library robustness

- dynamiclib is kinda broken because i rely on tls being set up by runtime_init
- ask the os for the correct page size
- :TodoLinux :HardcodeOs
- cure remaining stds
- stop using libc for random shit like float math or number formatting. 
- there's a bunch of library-ish / style things that need more work to get to the pointer where
the right/fast/safe/whatever thing to do is also the easy thing to do. 
  - make number casts less annoying and less error prone 
  - do a pass at trying to replace pointer<->int casts with some higher level thing
  - be consistant about using rawptr when it's a pointer (not i64)
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
- implement examples/testing.fr/fetch_or_crash() with import_c (libcurl) instead of exec-ing shit
- fetch_or_crash hashes are of the compressed file which is garbage. will break if github changes compression level or whatever. 
- bake for list/hashmap need to get rid of uninit memory
- List.shrink_to_fit for places that i push and then return a slice so you can free it on allocators that don't track size

## cleanup 

- ImportVTable: implement add_to_scope with add_expr_to_scope
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
  - some of the qbe ssa tests are generated: strspn, strcmp, mem3, cprime, abi8
  - if i ever get serious about using tcc for anything, we can't be having thier lib/atomic.S
- as an extension of argparse it would be cool if all the demo programs could be both 
and exe and a dylib so if you want to run from cli it parses to a struct and calls the impl,
but if you're calling from a program you can import the struct, dlopen the thing, 
and not need to serialize the arguments to a string. 
- seperate out the platform specific fields of QbeModule

## tests

- have a test where you force inline everything that's not recursive to stress test the backend dealing with large functions.
- compiler/test.fr run for jit as well
- automated test that builds are still reproducible (including with -debug-info)
  (currently i only do it for the compiler via `run_tests release` in ci but should do it for all the programs)
- fix the test programs to not all write to `./a.out` or whatever so they can run in parallel.  
  (including cross for different arches at the same time)
- test compile error for conflicting #use
- compile all the examples in run_tests: toy
- repro doesn't work when you do `-repeat`
- have one command that lets me run the tests on all targets
- tests for failing progeams. 
  - panic backtrace
    - including inferred function names
  - compile error locations
  - `franca examples/import_c/cc.fr a.c -r` with a.c being the empty program
    should say "missing function main()". previously it was segfaulting because get_addr never returned None. 
- compiler/tests.fr stops when something doesn't compile but it should show which other tests passed like it does for runtime failures
- `default_driver.fr run` should jit instead of stomping a.out and exec-ing it. 
- think about how to test the gui programs more convincingly than just that they produce a binary
- test using import_(c, wasm)/ffi from a precompiled driver to make sure they're not relying on being in their own compilation context 
- make the crash examples work without needing to set the env variable / run jitted
- instead of hardcoding `clang`, use env var CC or something more sane.
  could try clang and fall back to a self-compiled tcc if it's not available. 

## error messages

- `contextual field % not found for % message` for enums shows the `declared here:` location for the raw type instead of the enum tpye
- errors don't show multiple locations (like conflicting overloads should show the problem)
- `Type Error` should tell you where the difference is! and use the right names for enums, etc.
- `place expression expected pointer dereference` when you forget a `[]` after a call and field access should tell you that.
- detect if you try to do the jit thing with the backend library from jitted code on macos instead of just `bus error`-ing
- bring back nice error message for trying to call a #ct at runtime
- `Illegal place expression: GetNamed` is not as helpful as "tried to assign to undeclared variable %"
- need chained compile errors.
  - maybe loc should be a specific token so you don't highlight the whole expression just to complain about a function signature
  - macros should show both expansion site and code that generated it.
- fmt_error wip_tasks stack doesn't work when you get "failed compile foo" 
  in main_thread_pump because of failed overload resolution. 
  probably anything that politely returns error all the way up (instead of poll_in_place)
  since i pop the stack when poll_until_finished returns an error. 
- better error message from backend if you forget nunion=1 on a struct
- `pattern match on non-tuple but expected % args` should show the callee as well
- when it tells you a #import is null at comptime, say whether the library string was registered. 
(ie. did you probably typo the lib or the func).
- integer overflow of a literal
- contextual field not found should show the type's declaration location 
- "arity mismatch" should check if last aparameter is CVariadic and suggest @va
- "arg type mismatch after removing const args" error message needs to tell you the types
  and index of the argument. 
- i need to be able to easily identify the Anon generated functions. 
  try to be smarter about giving them a name. like for const eval of a `::` var it should just be the name on the left. 
  use FnDef.line and make it more elegant somehow, idk 
- give data symbols readable names now that they show up in symbol table
- error instead of hang on recursive inline
  and recursive constant declaration

## language decisions

- decide what the policy on not having a bti instruction on AsmFunction should be. 
  need to tell the frontend that it can't be shimmed without a patch. 
  should i just insert them for you? that's probably bad if you did it on purpose or 
  are counting instructions to tail call into it or something interesting like that. 
  should detect trying to create a function pointer to it then. 
  - for now i do add the instruction so i can use it in check_debug_info
- make #macro less of a special case? it would be nice if it could participate in overloading on some arguments. 
so you could say `fn fmt(out: *List(u8), template: Str, arg: FatExpr #macro) FatExpr`. 
then you need a new way to express that you need `template` to be constant in the program but 
it can be runtime known in the body of the fmt macro. 
- #generic is painful
  (the type annotations can only use previous const parameters, not later ones)
- do i want to represent different calling conventions in function pointer types? 
  currently i've solved it by only supporting one calling convention 
  and if you want something else you use an AsmFunction to translate. 
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
  nicer syntax than `{|\n continue :: local_return;\n continue(); \n }` might be `{|'continue\n continue(); \n }`
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
- tls(.leak_allocator) could just always be an arena instead of wasting general_allocator's time
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
- https://andrewkelley.me/post/string-matching-comptime-perfect-hashing-zig.html
- fix examples/compiler_gui
- rust format! macro. they have format_args! builtin to the compiler which is kinda funny
- make graphics/shaders translation support a more interesting subset of the language 
- profiler gui 
  - samply solves my problem but it would be nice to have something basic that works without installing random stuff
- something that generates point clouds / LAZ files so you can use the geo demo without 
needing to go find some data in the right format (and without me including a blob for it)   
- http://www.xmailserver.org/diff2.pdf
- https://spidermonkey.dev/blog/2025/10/28/iongraph-web.html
- runner for 
  - https://github.com/ertdfgcvb/play.core/tree/master/src/programs/demos
  - https://github.com/bellard/quickjs

### assembler

- tcc at comptime. use their assembler for AsmFunction 
  - amd64/rv64 only
- use https://luajit.org/dynasm_features.html for AsmFunction 
  - amd64/arm64 only but there's a pr for rv64 https://github.com/LuaJIT/LuaJIT/pull/1267

### mimic

- it would be cool to provide the same abi as tcc 
  and see if i could get libriscv to use me as it's c compiler for binary translation. 
  looking at the .c it generates for hello world, 
  i need to add support for: `__attribute__ (visibility, constructor, used)` and 
  `__builtin_(expectbswap32, bswap32, clz, clzl, popcount, popcountl, fminf, fmin, fmaxf, fmax)`
- i like the idea of providing the same api as luajit's c ffi but on the normal lua interpreter
  - can i do better than other people?
    - https://github.com/q66/cffi-lua
    - https://github.com/zhaojh329/lua-ffi
    - https://github.com/facebookarchive/luaffifb
- ditto libffi or dyncall. that's probably a great source of calling convention tests. 
  do the same as call_dynamic_values where i just jit a new function for each signeture,
  compile thier tests with clang but against my version implementation of the ffi library 
  and hopefully they've done the work of finding edge cases that i can test against my backend.  

## make it not suck

- `#[derive]`, hash, print. should be fixed by #where (but now im afraid that might be super slow because of the #align fiasco)
- numeric stuff sucks because of all the casting and it's bad about letting constants/literals coerce correctly
- comptime feature flags like use_threads, graphics::backend, os/arch.
need to support them being different at comptime and runtime in a uniform way. 
- using foreign libraries. as soon as you need to write your own driver it's super annoying. 
- nested error messages. need to be able to show more than one source location. 
- keep reducing the amount of stuff in the prelude 
- allow adding your own fields to tls
- i really need to output debug info
- nested array syntax is kinda ass. `Symbol.Array(2).Array(3)` and `Array(Array(Symbol, 2), 3)` 
is much worse than zig's `[3][2]Symbol`, even c's `Symbol the_name_in_the_middle[3][2]` 
has the numbers in the right order at least. rust has my problem too `[[Symbol; 2]; 3]` but nicer special case syntax for it. 
  - could just... swap the argments. idk why i made such a big deal about it. 
    Array(3, Array(2, Symbol)) is an (array of 3 (arrays of 2 (symbols))).
- bit flags, @tagged abi. see sys/linux/LandLock
- this would be much less messy
```
#use("@/examples/import_c/ffi.fr");
StbTrueType :: include { C |
  Stb := Foreign'stb();
  C.define("STB_TRUETYPE_IMPLEMENTATION");
  C.include(Stb, "stb_truetype.h");
};
```

##

- things that should be cleaned up next time i :UpdateBoot
  - AbiHackSpan
- fix overflow when lexing large float literals
- show const fields better in log_type
- less verbose if let/let else? think about how to express with a macro.
- parse zig headers and generate extern declarations (like examples/c_bindgen). then i could use bits of thier standard libary.
  they have a bunch of fun stuff that doesn't use use comptime in the api and im not interested in writing myself:
  http, hashing, random, zip, magic numbers for wasm/elf/dwarf/macho
- should make it easy to build rust/zig projects from your driver program since they can generally cross compile well and have libraries you might want.
- do i want to expose the idea of different libcs? musl vs glibc vs cosmo
- eventually it would be nice to send codegen of large subgraphs to another thread, like when compiling 
  import_c it would be better to do the whole thing at once instead of forcing each function to be jitted eagerly.
  but even then you want the frontend thread to be able to progress on other functions while it waits. 
  my previous attempt stalled on most functions (in wait_for_symbol)
  so it was faster to move all codegen to the frontend thread so there was no context switching overhead. 

// TODO: derive ne from eq so you can use != more often. 
// TODO: better error message for *T vs **T, should say dereference not unsafe cast. 

- write f64 to string (better)
- derive eq/display for unique types. 
- derive recursive drop/default.
- nicer switch over incomplete enum. 
- jump table switch, mine is gonna be so slow. 
- need smarter overload resolution so you can use enum contextual fields more often. I hate typing the name a bunch. 
- multiple cases with one handler in switch. 
- expand macros in switch

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
- https://llvm.org/docs/CoverageMappingFormat.html https://llvm.org/docs/LangRef.html#llvm-instrprof-increment-intrinsic
- good error message for accidently using ' as character literal.
- embeding other languages would be a good demo of the comptime/meta programming stuff.
  - need a raw string syntax that passes it to a macro (like nim?)
  - lualit would be cool cause they have c abi stuff and dynamic language so notably different from mine.
- annotations should be powerful enough to derive recursively eq/clone/drop/hash based on fields.
  - need to let you access the default even if you override.
    like adding extra drop logic shouldn't mean manually dropping all fields.
- maybe anon struct literals with inferred type should be fine.
- enum bitset
- quick union types: fn Enum(Arr(Type)) Type; so Enum(A, B, C) === (A: A, B: B, C: C)!enum;
  - if you had all the tags unique (use TypeId) instead of starting at 0, you could make it free to convert to a super set.
    that probably gives up u8 or niche tags but even u16 is enough that its fine.
- user defined operators so you can pick what meaning of &T makes sense for you (per module)
- function that take a slice of args called like variadic functions.
- u32/u16 pointers as indexes into per type arrays. deref trait so that can be a library feature? want to be able to toggle easily not at every use so can benchmark
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
- https://btmc.substack.com/p/tracing-garbage-collection-for-arenas

## behaviour considered disrespectful

- accessing the wrong tagged field
- holding a pointer to an tagged member while changing its tag
- read off the end of an array
- div(0)
- constructing a slice with a bad length (can trick someone else into breaking the rules)
- escaping pointer to stack
- double free, use after free
- holding pointer accross a collection resize (common case of ^)
- void pointer cast
- AsmFunction
- reading from uninitialized memory
- init from small union field then read from large.
- c style variadic functions
- nan bit pattern for any instructions other than load/copy
