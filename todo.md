
- `u1, u2, v1, v2 : Ty(f32, f32, f32, f32) = (0.0, 1.0, 0.0, 1.0);  // TODO: allow this :compiler`
- using @switch on an enum instead of @match is an easy footgun because it expects @case(.foo) instead of fn foo() and then you'll be confused
- Type Error when calling a function ugently needs to show both the call site and the declaration site
- DONT EXEC UNZIP IN examples/testing.fr
  apple's unzip clearly doesn't work...
  ```
  /Users/luke/Downloads/wuffs-b1174882799a6d39796a14c9b28fb4977144a480/test/data/non-ascii/++.txt:  write error (disk full?).  Continue? (y/n/^C) n
  fchmod (file attributes) error: Bad file descriptor
   (warning) cannot set modif./access times
            No such file or directory
  warning:  /Users/luke/Downloads/wuffs-b1174882799a6d39796a14c9b28fb4977144a480/test/data/non-ascii/++.txt is probably truncated
  ```
  it worked before and the debian one works in orb on the same machine, the disk clearly isn't full. 
  this is why we shouldn't just be trusting the system to have random shit preinstalled. 
  stop being lazy!!
- don't just crash at runtime when you `import_c/cc.fr -r`
  and try to call a function that was forward declared but not linked against
- examples/count.fr total lines is wrong
- seperate all the tests that exec stuff or download stuff
  and make run them seperatly. 
  it's not acceptable that if i update macos and fuck up my clang, my normal tests fail. 
- make all the tests pass on linux
- fake assembler to be drop in for hare
- real test for dynamic libraries
- lite version of franca_runtime_init when running drivers so theres a sane place to make sure OS gets set
- make stack trace debug info work accross multiple compilers. it needs to go in GlobalFrancaRuntime
  (test with crashing in examples/repl.fr when running it as a driver)
- `f :: fn() = ` isn't getting an inferred name (report_called_uncompiled_or_just_fix_the_problem)
- autotest all the stuff from the web demo in import_wasm 
- make it easy to run all the tests with qemu-user
  - -L /usr/x86_64-linux-gnu
  - document what needs to be installed: qemu-user-static, libc6-amd64-cross, libc6-riscv64-cross
- more stack trace improvements
  - macros don't always get a block. 
  - another source of confusion is that for comptime code, source location doesn't come from 
    the fake debug info, it parses the string and finds that function call in the ast. 
    make that work with #inline too. 
  - including every function definition in the trace as INLINE is confusing. 
  - allow hook_backtrace only if not already hosted
  - give a franca_aot_debug_info when running directly (`franca crash2.fr`) so it works if you do your own hook_backtrace. 
  - tests for backtraces
  - programatic access to the data from resolvers instead of just a string. 
  - disassembly annotated with the debug info
  - annotate functions to skip dbgloc for. rn, `if` gets 3 entries in the trace which is useless. 
    kinda like rust's `#[track_caller]`
- life would be better if i tracked line numbers instead of byte offsets. 
  also that would make it trivial to move them between .frc modules. 
- you need a trace of what was being compiled when you get a compile error. 
  like the chain of references so you know why you're trying to compile that function. 
- replace import_c/includes.fr with a .frc module generated from the franca libc bindings. 
- run all the tests on linux
- enable caching of the .frc between -syscalls and normal. (they're one byte different)
- better apis for working with paths and temporary files
- use `#macro #outputs(T)` to give a more sane error message for `x: i64 = @is(foo, .A, .B);` etc. 

---

- crack down on smurf naming conventions (ie import("@/lib/alloc/debug_alloc.fr").DebugAlloc is kinda dumb). 
  could steal what zig does where a file is a struct. but that always annoys me because it makes you 
  pick one blessed struct to have the top level fields when the rest of your file is a namespace which looks odd. 
- real error handling for lib/sys/posix.fr. need to be able to remap the errno values to something consistant. 
- i've lost 100ms of speed. i should really make something that automatically times it every commit. 
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
- create a guard page when allocating a new stack to spawn a thread
  and have the backend do the probing thing where you read a byte every page when you have a large stack frame so you can't miss the guard page. 
- since do_codegen tries to do tracy stuff, FRANCA_TRACY=1 doesn't work if you try to run something 
  that uses import_c not through default_driver. ie. `FRANCA_TRACY=true ./trace.out examples/terminal.fr` 
  crashes but `FRANCA_TRACY=true ./trace.out examples/default_driver.fr build examples/terminal.fr` is fine. 
  relatedly, since that's not included in cache invalidation, it's broken if you turn on import_module caching. 
- allow trailing lambda to be passed as a function pointer but still infer types of arguments. (ie. when calling run_tests_main_threaded)
- set a good example; don't have tests that rely on layout of codegenentry. use the functions on the vtable. i think import_c/test/test.fr does this wrong
- always zero struct padding when baking constants (even when behind a pointer and even when the struct contains no pointers). 
- why don't lldb/gdb like my linux binaries?
- not all the tests pass with FRANCA_NO_CACHE=1
  - examples/import_c/test/test.fr
  - examples/import_wuffs/test.fr
  - examples/repl.fr
- not all the tests pass with import_module caching enabled
- can almost enable cacching when -syscalls 
- document `store v, [Sxxx]` vs `store v, Sxxx` on amd64
- add a test for #discard_static_scope now that i gave up on scc. 
- extend the cross repro tests to all the example programs. not just the compiler. maybe just add a file with hashes of binaries to the released artifact. 
- include_bytes should work like import() in that it invalidates the cache if the file changes 
  but instead of giving you a ScopeId just give you the bytes. 
- I need to improve @enum for bit flags so i can use that in posix.fr so it doesn't suck as much to call mmap. 
- i think bake_relocatable_value always gets a jit shim which is a bit wasteful. 
  happens because get_custom_bake_handler just calls vtable.get_jitted_ptr which 
  is the same thing handed out to user code for builting vtables so it delays compiling hoping 
  you wont actually try to call it (which is often the right choice, like for objc bindings, etc.). 
- everywhere i ENABLE_ENABLE_TRACY is garbage. i need a better pattern for that. 
- compile error if you try to bake something that is not in ast_alloc 
  because it's so easy to accedently type foo :: read_entire_file(temp(), path),
  and then you waste so much fucking time. 
  or is it better to just make a copy? 
  really i should nail down the semantics of when the bake snapshot happens. 
  (all at once at the end of compilation probably?). 
- make #export work on `foo :: fn() #` instead of only `fn foo() #`
- at some point i have to go through and make error handling not suck as bad. 
  there's a bunch of places i do `if thing_should_work then do_thing else not_that` instead of 
  `do_thing catch no_that`. file_exists -> open_file is a common one. 
  twice the syscalls and you produce toctou problems for no reason. 
  the reason it's annoying to fix is you want to know why something failed not 
  just that it failed so then i need to deal with remapping errno values for the different targets. 
- `FRANCA_NO_CACHE=1 ./q.out examples/import_wuffs/test.fr`
  > Assertion Failed: need_reify on threaded codegen of wuffs_base__io_reader__read_u8__46298
- repl.fr doesn't work jitted with FRANCA_NO_CACHE=1 on arm-macos because it needs to call apple_thread_jit_write_protect.
  should allow not writing to exec memory and call a tiny bit of aot code to do the copy when you're done each function. 
- #log_ir needs to do something in declare_alias
- stop doing superstitious fence()
- it would be nice to have a good assembler for AsmFunction. 
  the current thing is just enough more verbose that it looks confusing. 
  whatever it is has to stay just a user space comptime thing tho, not part of the compiler. 
- might be able to get rid of is_wrongly_illegal_instruction now, but maybe it's safer just to leave it for good luck. 
- compiling tests/collections.fr/removing alone fails:
  (Compile Error: Poison expression Argument. probably not riscv related, just that i don't normally run one function at a time. :compilerbug)

## remaining nondeterminism

- exec has the same schedualing problem as threads but is harder to fake 
- aslr for where the executable is loaded and what addresses mmap gives back
- clock_gettime

> (note) solutions for others: SLOW_USERSPACE_THREADS, NEVER_USE_LIBC_ALLOC
> TODO: should make docs/debugging.md and document strategies for narrowing down bugs. 

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
    
## import_c

- C23: `__has_c_attribute`, allow `[[]]` instead of `__attribute__`
- import_c add a test for the incorrect include guard thing i fixed on jul16
- import_c faults on function without parameter name: `int aaa(char*) { return 1; }`
- import_c/cc.fr searches include path for the starting file before your current working directory which is super confusing
- turn off Qbe.Fn.track_ir_names in import_c
- :ReWalkTokens
- what is the deal with load_opt::def taking so long for import_c
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
- now using enter_task instead of c'enqueue_task but that means it will compile slower. 
- why does threaded=true not work when running jitted sometimes (random mutex failures)? 
  there shouldn't be jit-shims if it's not at comptime so that theory doesn't explain it 
  although it seemed to have similar symptoms. 
- need more control over exports. currently any frc file has all the libc stuff you included reexported. 
  which will be confusing if you try to #use it in franca
- implement the rest of import_cache_file: variadic, union/enum/pointer
- extend test/ffi.fr to have simpler usage of #include a .frc file 
  (currently only used by import_wuffs which has a lot going on)
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
- :AsmNotYetImplemented
- for inline assembly, i think it would be easier to give up on ExprLevelAsm
  and use someone else's assembler to compile the `asm` blocks into an elf file 
  and then just call them as functions. they give you clobber lists so you just have 
  to put some code on either side to translate that to the normal abi. 
  probably have to deal with more types of relocations. 
  are you allowed to jmp between asm blocks? 

### !! BROKEN !!

- self compile in blink on (arm-macos and arm-linux) on github actions seems to hang forever sometimes? 
- soft_draw.fr crashes when you quit the program
- ./boot/temporary/macos-amd64.sh with SLOW_USERSPACE_THREADS=true
```
pragma-once.c                           [ok] 

All is fine! (passed 35 tests)
1.fr                                    note: run with `FRANCA_BACKTRACE=1` environment variable to display a backtrace%     
```
- import_c can't do SLOW_USERSPACE_THREADS when run directly on a compiler that was build with real threads because of "using host backend"
- `./q.out examples/default_driver.fr build compiler/main.fr -o q.out -d t` very rarely prints junk
- `FRANCA_NO_CACHE=1 franca examples/import_c/test/test.fr` 
  dies in init_codegen_worker. spawning threads from a shim doesn't work? 
- tests/exe/sys.fr might not work with SLOW_LEAK_ARENAS=true?
- i've seen this a couple times on actions:
```
>>> unpacking [target/franca/deps/fonts.zip]...
Assertion Failed: failed to read file 'target/franca/deps/fonts/ttf/JetBrainsMonoNL-Bold.ttf'
```
🤡 which is extra plausible because i'm not even checking that unzip was successful because it complains about weird filenames in wuffs. 
all the more reason to continue sequestering the tests with dependencies. 
i think it's just a race where it gets confused if two programs try to cache the same dependency at the same time. 
- similarly i think write_entire_file doesn't work at all. 
  should probably have more targetted tests for library stuff like that. 
  `failed to write 4935560 bytes to 'target/franca/cache/__backend_meta_qbe_frontend_fr.frc'`
  when running backend/meta/test.fr without -bin so it recompiles and tries to cache on multiple threads. 

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
- rarely:
```
orb ./boot/temporary/linux-arm64.sh
mkdir: cannot create directory ‘target’: File exists
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 1001k  100 1001k    0     0  5822k      0 --:--:-- --:--:-- --:--:-- 5858k
panic! backend/lib.fr:1171:58
find_ip_in_module :: fn(m: *QbeModule, addr: rawptr) ?Str = {
Parse Error: unterminated block (mismatched '{')
```

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
- make AsmFunction get an inferred name
- @bit_fields in incremental.fr don't work inline in the structs
- #ir tries to ignore zero-sized params but not if they're first which is sad
- "need to be consistant about how to handle modules like this that don't actually compile anything"
- fix examples/terminal.fr -jit so it doesn't freak out about nested compiler contexts because of the repl
  (get rid of EASY_GRAPHICS_IS_JITTING_HACK)
- reduce disk usage bloat. rn fetch_or_crash stores the unzipped thing and the zip file so you have everything twice. 
many of the things i use i don't need everything from so it could make you tell it which files are important and 
delete the rest. thing to think about is that you want to union those between different projects that depend on 
different subsets of the same resources. 
- run tests in landlock so it's less scary to miscompile something that's doing random syscalls
- document #weak: docs/(annotations.md, imports.md)
- deal with `NOSYS`
- make #log_asm work for the #asm replacement 
- experiment with outputting even more info in .frc and an lsp that reads it back. 

## wasm

- remove all the TODOWASM (ifdefs/comments)
- wasm instruction for wait/wake/fence
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
- a page_allocator that can reclaim
- do the libc replacement stuff with (unimplemented) vtable.fill_import. 
  - so you can have a layer that's wasm aware and exposes a tighter interface to the host. 
  - rn most of the Exports in run.fr are just to make the ssa tests work without changes, 
    not because they really need to be provided by the host. 
  - maybe this should be wrapped up in getting official about exposing libc api from a .frc 
    since i want to do that for static native binaries anyway. 
- make import_c work without hacks
  - setjmp/longjmp with exceptions
- import_wasm working in wasm would be cute. 
  - dont reserve giant virtual memory
  - don't depend on libc (import_wasm/run.fr/Exports for the .ssa tests)
- what to do about file system
    - can't decide if i want drop in posix-ish like wasip1 or declare that it's better
      for programs to make themselves embedable and the compiler shouldn't be opening files
      anyway, you should pass in a vtable of how to get imports when creating the compiler, 
      so you just have two different entry points. 
- syscall wrappers support wasip1
- web demo that runs all the tests
  - make it run in node in actions
- stack traces
- make the wasmtime version work
- add a .ssa test that tests dynalloc with a deeper callstack
- in wasm/make_exec use debug_out when dumping module so you can redirect it
- make sure `::@as(rawptr)(fn() void = ())` gives you the got_lookup_offset not the junk jit_addr. 
- for examples/web, instead of `url?v=version` the sane thing is to just put content hash in the file name
- pass the tests
  - cast.c, float.c, literal.c, fpcnv.ssa: `e` in float literals
  - macro.c: `__FILE__; 1 != 0; ends_with(main_filename1, "test/macro.c")`
  - varargs.c, function.c: vsprintf
  - pragma_once.c: `redefinition of function 'assert'`
  - abi5.ssa, abi6.ssa, abi8.ssa: the case in my hacky printf that calls snprintf
  - echo.ssa: `$main(w %argc, l %argv)`
  - vararg1.ssa, vararg2.ssa: vprintf
  - compiler/main.fr jit: `wasm-jit $__franca_aot_debug_info should have known got_lookup_offset`
- threads. it's a pain in the ass tho. 
  - need cors to use sharedarraybuffer which limits the environments people can use your thing in.
    (no github pages, no `python3 -m http.server`)
  - no shared tables so have to keep them in sync manually so need to keep a list of every
    time you load jitted code or do a table assignment and replay them when you span a new thread. 
- web demo: hare, wuffs, more nontrivial c programs, webgpu
- use dump_wasm to disassemble one function at a time (like i do with llvm-mc) 
  instead of only all at once in output_wasm_module_jit
- improve import_wasm until it can cope with bootstrapping zig1.wasm

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
- test for phi of stack slot? 
- remove redundant extension for `b := a & 31; c := extub b;`
- i don't like that direct to exe vs to frc_inlinable then to exe give different binaries. 
- be able to output frc/frc_inlinable in the same module as compiling normally so you don't have to do two passes over things to cache it
- fix those two ^ and then compiler/test.fr can create all at once and assert that they make the same exe instead of running them all
- elf: don't include names for local symbol with DataAbsolute relocations when exe_debug_symbol_table=false,
  and do include all symbol names when exe_debug_symbol_table=true. 
- test that makes a dylib
- macho exe_debug_symbol_table doesn't work i clion profiler (dtrace?)
- debug assert that all tmps have a definition in rega. 
  (especially because @emit_instructions doesn't catch it)
- turn off ASSUME_NO_ALIAS_ARGS in arm/abi. it doesn't help much and it would be better to do the hard thing 
  and figure out how to make UNSOUND_SKIP_EXTRA_BLIT in emit_ir work. 
- arm relocatable import: relocation to fold symbol offsets into the adrp+add instead of wasting an extra instruction :SLOW
- fixup2.ssa: "this should work without the indirection, i just don't handle folding the offset into the constant correctly."
  i think that's fixed now. add a little test of that too
- "this works here but not in it's original place under f()."
- apple has different c variadic abi. that's pretty unfortunate for my grand plans. 
  it feels pretty invasive to have the backend compile two versions of anything variadic. 
  but most functions don't use varargs so those could be rega-ed once for linux+macos when compiling for all targets together. 
- experiment with leb128 for indices in .frc modules. 
need to be careful about the refs which have tags in the high bits so won't leb well directly. 
- why does llvm-mc disassembler arm think my cas is invalid instruction encoding? (check with cas.ssa -d D). 
  objdump thinks it's fine and it clearly runs correctly. 
- TODO: harec/src/gen.c: `[arm64/emit.fr/fixup_arm64] offset from dynamic import builtin_type_nomem+4` but would work when done as one compilation unit.
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
- cmpneg is unsound for floats so if a cmp gets folded into a jnz it can behave wrong for nans. 
- don't just disable elide_abi_slots when the function has variadic parameters. 
  im not convinced i understand the aliasing mistake. 
  it must be that somehow it uses am address that goes past the save area? 
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

## backend symbols rework

- prefer_libc_memmove being seperate from `link_libc` is kind of a hack to make harec static binaries work. 
  but really link_libc is conflating two things, are you allowed to import thing 
  (or should any weak symbols be treated as missing) vs is the compiler allowed 
  to assume a memmove implementation exists with that name and can be imported. 
  those are obviously different: harec wants seperate compilation units with imports 
  between them but does not link a libc that exports a memmove symbol. 
  hopefully this can get cleaned up as part of a symbols rework.  
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
- isel fixarg is doing redundant sign extensions
- isel address folding into load/store like arm
- isel replace cmp+jnz with bcmp (they don't have a flags register)
- (fixup1.ssa, fpcnv.ssa, encoding.ssa) work in qemu but not libriscv. report bugs? 
  - fixup1 and encoding work with --single-step but not without
  - also cas.ssa but there it has an expcilit error message for not supporting that extension instead of just giving the wrong answer like the others
- call local symbol directly without producing it in a register first
- trampolines for imported symbols
- tests/
  - exceptional (jump.fr)
  - multiple_stacks, intrins, inline_asm_jit (AsmFunction)
- clang's binary does something with `__global_pointer` in start. do i have to do that?
- ssa test that uses the constant 9223372036854775807
- `thread backtrace` doesn't work in lldb. is my stack layout wrong? 
- riscv_flush_icache syscall vs FENCE.I instruction for clear_instruction_cache
- :TodoRiscv
- ./target/f.out backend/test/folding.fr: panic! TODO: copy slot float

## don't rely on libc

- import_c/tokenize: strtod
- import_wasm/run.fr: 
  - (because .ssa test calls it): snprintf, strncmp
- prospero: atof
- (epicyles, geo): fmod
- (graphics): cosf, sinf
- dlsym, dlopen, dlclose

## hare
  - enable the tests that use testmod
  - fix 09-funcs so it gets as far as the assertion failing because i don't do init/fini.
  - compile harec with import_c
  - pass the library tests as well
  - don't forget ASSUME_NO_ALIAS_ARGS might break things
  - why isn't clang giving me a static binary?
  - be a drop in replacement for qbe that harec can use. need to deal with the fact that i don't output text assembly. 
    just provide a script that you can use as hare's assembler that dispatches to a real assembler if it's actually text? 
    (needs to pass -force_static_builtin_memmove to qbe_frontend. tho maybe that should be the default). 

## longevity

> i want to be confident i could abandon it for 20 years and still be able to run my little examples

- vendor the things i care about: stb(truetype, image, image_write), jetbrains mono font
  - get sdf data out of that and generate my own test data for prospero.fr
  - generate my own laz data to render with geo. im sure there are fun shapes that can be created with little code
  - teminal.fr, view_image.fr, import_wuffs/test.fr (wuffs input programs)
- the graphics stuff is a bit of a dumpster fire
  - give up on native webgpu? 
  - slow software rendering implementation as documentation of what it's supposed to do
- make sure everything works just using posix libc stuff as a fallback (syscalls and private apis need to be optional)
  - (openat, ioctl) use varargs
  - glibc fstatat somehow sucks 
  - macos: now sure if (ulock_wait, ulock_wake) are supposed to be public. have an option to use something from pthread
- make the platform detection in franca_runtime_init less hacky
- support (free, net, open)bsd
- the bootstrapping system can't be committing a macos-arm binary. linux-amd seems more old / emulator enthused, could use blink, etc. 
- seperate out the tests that download things and run them as an extra thing at the end. so if they disappear it's not a bit deal. 
  like it's nice to test import_c on lua,tcc, etc. but it doesn't matter for the main franca stuff if you can't run those tests. 
- fix the non-deterministic test failures
- report all test failures instead of stopping on the first one
- i want the transcribed magic numbers for syscalls, sys struct layouts, instruction encoding, object formats, etc. to be more auditable. 
  maybe make it structured consistantly enough that i can generate a c program that asserts everything matches
  for a certain target when compiled by a normal c installation. for instruction encoding, maybe do more like tests/exe/x64_encoding.fr. 
- output qbe text ir that it can parse so i can test my stuff without my backend implementation 
  (doesn't help for comptime because qbe can't jit)

## linux 

- do i have to use header.type=ET_DYN on mt executables to get aslr? (it's what clang does if you pass -pie)
  does that replace Elf.MYSTERY_SPICE?
- linux fault-na.ssa need to do the signal struct (rn it's skipped in backend/meta/test.fr)
- :TodoLinux CLOCK_REALTIME
- can't cross compile from (macos) to (linux -syscalls) if you call 
  write_entire_file at comptime because it wants gettime (for atomic file name, bleh)
  which i havn't transcribed syscall for on macos. will be fixed when i make prefer_syscall
  not a constant which i want to do anyway for reusing frc_inlinable when cross compiling. 
  (same for linux-sta -> linux-dyn)
- how are you supposed to ask for page size? blink wants 64k instead of 4k. 
- standalone import_c/cc.fr and meta/qbe_backend.fr can't make statically linked binaries because the `_init` is written in franca
- if macos {  // todo: "why does this work on my linux but not github's linux"
  (tests/sys.fr using elf_loader.fr on a dynamic executable)
- elf_loader.fr doesn't work on linker output: `panic! not divisible by page size`
- lldb doesn't have symbol names or let you set break points. do i need to do dwarf stuff?
  (it kinda works in gdb!)
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
- shader translation for the gui examples
- mprotect .ConstantData segment after applying relocations
- readelf: Error: File contains multiple dynamic string tables
- linux amd: consistantly fails in actions but not in orb rosetta. 
  - wuffs/(png, jpeg, deflate) 
  - import_c/tests/(macro, varargs, function, attribute, usualconv)
- some memory corruption thing the first time you run after fixing a compile error (so not when cached). 
  ```
  panic! lib/sys/threads.fr:146:5
  #use("@/lib/sys/sync/atomics.
  Parse Error: Expected begin expression, found UnterminatedStr
  ```
  happens in many different places. 
  (orb arm is the one i mostly use but it might happen elsewhere as well)

## amd64

- amd64: `std/json        cc      FAIL test_wuffs_strconv_parse_number_f64_regular: "-0.000e0": have 0x0000000000000000, want 0x8000000000000000`
- import_c can't compile lua targetting amd64 (makes an exe but it crashes immediately)
- import_c/test/ffi.fr: panic! sysv abi does not support variadic env calls
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
  - dump_bin: won't be able to parse back if symbol names have spaces in them
- clear cache before tests just in case
- whether the host compiler was built with`-syscalls` needs to go in the cache file :CacheKeySyscalls
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

### FrcImport 

-  run inlining and if something changes redo the other early passes as well
- someone needs to detect undeclared symbols
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

- once perft works, print a mnps number in the main run_tests so i notice if it gets 3x slower via compiler bug
- experiment with New Types for index/rank/file/mask so you can't mess up the unit conversion
- have Slider :: @enum(Rook, Bishop) instead of functions with isRook: bool parameters
- regressions (unported):
  - Learned.Weights
  - search: followCapturesDepth + lookForPeace, 
    but i had it turned off in the old codebase so clearly i was doing it wrong. 
    debug / make sure it actually wins more games
  - seach: uci reporting info/pv/time etc. 
  - search: time limit
- use ZOID_CASTLE_START
- gui
  - play against uci engine
  - promotion
  - bit board overlays like the old one (and ui to choose which to show)
  - run search on a seperate thread
  - ui for turning off the bot and choosing which colour to play

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
- tests that reach discard(), compute shader dispatch(), and create(Attachments)
  - finish gfx-webgpu
  - finish gfx-metal
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
- this happens occasionally: (but i have to redo the whole wgpu shit anyway because just yoloing a specific version of libwebgpu_dawn.dylib is not acceptable) 
```
franca examples/mandelbrot_ui.fr -wgpu -jit
panic! graphics/web/webgpu.fr:1221:34
wgpuDeviceCreateCommandEncoder :: fn(device: Device, descriptor: *CommandEncoderDescriptor) CommandEncoder #import("webgpu");
Compile Error: we hit a dynamicimport ('wgpuDeviceCreateCommandEncoder' from 'webgpu') with no comptimeaddr for jit
the frontend should make sure this doesn't happen. 
TODO: this happens when compiling targetting libc from a compiler built without libc on linux
```

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
- make fetching dependencies (ie. lua for testing import_c) not embarrassing
- implement examples/testing.fr/fetch_or_crash() with import_c (wuffs and libcurl) instead of exec-ing shit
- fetch_or_crash hashes are of the compressed file which is garbage. will break if github changes compression level or whatever. 
- use import_c for the parts of sokol i haven't ported yet (can't for the mac stuff because thats objective c)
- bake for list/hashmap need to get rid of uninit memory
- List.shrink_to_fit for places that i push and then return a slice so you can free it on allocators that don't track size

## cleanup 

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
  - some of the qbe ssa tests are generated: strspn, strcmp, mem3, cprime, abi8
  - graphics/web/webgpu.fr which is extra bad because the abi isn't stable so it's useless
  - if i ever get serious about using tcc for anything, we can't be having thier lib/atomic.S
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
  (including cross for different arches at the same time)
- test compile error for conflicting #use
- compile all the examples in run_tests: toy
- repro doesn't work when you do `-repeat`
- have one command that lets me run the tests on all targets
- tests for failing progeams. ie. panic backtrace
- compiler/tests.fr stops when something doesn't compile but it should show which other tests passed like it does for runtime failures
- make it clear that you can't do this: `franca self.fr && ./a.out driver.dylib run b.fr`. 
it doesn't like that you stomp a.out, default_driver:run should pick a unique path probably. 
or just default to jitting and force you to enable aot by specifying an output path. 
- think about how to test the gui programs more convincingly than just that they produce a binary
- test using import_(c, wasm)/ffi from a precompiled driver to make sure they're not relying on being in their own compilation context 
- test crash stack traces
- make the crash examples work without needing to set the env variable / run jitted
- TODO: i should probably be making tests for error locations
- instead of hardcoding `clang`, use env var CC or something more sane.
  could try clang and fall back to a self-compiled tcc if it's not available. 
- make it easy to skip any tests that have external dependencies

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
- error instead of hang on recursive inline

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
- https://andrewkelley.me/post/string-matching-comptime-perfect-hashing-zig.html
- fix examples/compiler_gui
- rust format! macro. they have format_args! builtin to the compiler which is kinda funny
- make graphics/shaders translation support a more interesting subset of the language 
- profiler gui. it's silly that i have to open CLion just for it to run DTrace and draw a graph
- something that generates point clouds / LAZ files so you can use the geo demo without 
needing to go find some data in the right format (and without me including a blob for it)   

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
- eventually it would be nice to send codegen of large subgraphs to another thread, like when compiling 
  import_c it would be better to do the whole thing at once instead of forcing each function to be jitted eagerly.
  but even then you want the frontend thread to be able to progress on other functions while it waits. 
  my previous attempt stalled on most functions (in wait_for_symbol)
  so it was faster to move all codegen to the frontend thread so there was no context switching overhead. 

// TODO: derive ne from eq so you can use != more often. 
// TODO: better error message for *T vs **T, should say dereference not unsafe cast. 

/* List of grievances. 
- write f64 to string (better)
- derive eq/display for unique types. 
- derive recursive drop/default.
- nicer switch over incomplete enum. 
- jump table switch, mine is gonna be so slow. 
- passing arch overload set to typed Fn 
- need smarter overload resolution so you can use enum contextual fields more often. I hate typing the name a bunch. 
- debug state stack should include eval const ident. 
- multiple cases with one handler in switch. 
- expand macros in switch
*/

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
