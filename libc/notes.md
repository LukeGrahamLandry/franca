
--- 

- `__stdio (mutex), [__stdout, __stderr, __stdin] (*FILE)`
- thread/tls.h: 
  - see ape/specification.md. 
  - wants to reserve x28 on arm (which is callee saved) so it won't work 
    in a callback from external code (like when i want to do macos gui stuff)
  - tls is a *CosmoTib
      - tib_ftracer (runtime/ftracer.c)
          - only for `--ftrace` 
          - shorted depth of callstack (so how many frames before main()?)
          - last fn addr so you don't spam on recursion?
      - tib_garbages (mem/gc.c)
          - some gcc extension to let you defer() a function to call when you return 
          (by rewriting stored return address). also called in pthread exit. 
      - tib_atexit (runtime/cxa_thread_atexit.c)
          - destructors for globals in `__cxa_finalize`
      - tib_ptid 
        - stashed from sys_gettid
        - used for MUTEX_SET_OWNER in pthread_mutex_lock.c
      - tib_pthread is a *PosixThread which is handed out to user code as a pthread_t
      - tib_syshand the real pthread_t from dynamically linked pthread_create
    - you give that tls allocation to apple in CloneSilicon too tho, so do they actually look at it 
    or is it just a fixed size header we have to stay in before `.tdata, .tbss` (see thread/mktls.c)a
- `__constructor__` can poke stuff into the startup function
  - calls/getrandom.c, calls/rdrand_init.c
  - calls/getloadavg-nt.c
  - calls/finddebugbinary.c
  - calls/program_invocation_short_name.c
  - calls/unveil.c
  - intrin/armlse.c
  - intrin/isrunningundermake.c
  - intrin/mmap.c
  - intrin/nocolor.c
  - intrin/sig.c
  - intrin/unsan.c
  - intrin/x86.c
  - log/countbranch_report.c, log/countexpr_report.c
  - log/logfile.c
  - proc/exec-sysv.c
  - proc/getppid-nt.c
  - stdio/ecvt.c
  - stdio/fflush.c
  - stdio/stderr.c, stdio/stdin.c, stdio/stdout.c
- stdio is almost not a special cosmo thing. 
  so could almost use someone elses but the cosmo one sure looks simpler than musl, 
  cosmo just trusts that their own mutex is good enough and musl like inlines it again as a fun bonus?
  - only dirstream, getentropy have IsWhatever() 
  - ctermid has v_ntsubsystem
  - have to agree on where errno lives

i'd love to know what nexgen32e means. vaguely assembly related? 
it doesn't mean amd64 cause there's arm64 code in there too. 
but maybe it does anyway:?
```
static int GetElfMachine(void) {
#ifdef __x86_64__
  return EM_NEXGEN32E;
```

--- 

- they still use `#ifdef arch`, including in structs which is unfortunate. 
even public ones: sigcontext. so it doesn't completly free me of needing to 
sema twice to get different field offsets but it's still an improvement
- there's a bunch of special cases for their own stuff
  - zipos (tho that's kinda cool, i probably want to keep that)
  - an error message that suggests you try a different version of redbean
  - sys_execve execs `/usr/bin/ape` if the bytes in the file start with thier initials
- heavier runtime than i expected (tho the trace ones are optional)
  - `--strace` is just they call ~fprintf when it's turned on 
  - `__morph_begin` is pretty invasive
    - `--ftrace` is the compiler leaves nops at the begining of functions 
    and then the runtime reads its own elf headers to get find all the functions 
    and rewrite them to call ~fprintf so it has to call pthread_jit_write_protect_np
    - tls on amd64 starts at __executable_start and looks for the code sequence 
    it knows the compiler generates for tls accesses and rewrites them. 
  - they have to un-leb the syscall numbers before calling your main()
- they still depend on a blessed xcode to compile ape-m1.c to fill a Syslib struct 
with apple's blessed function pointers. 
  - which i fine, im not as excited about the single exe thing as they are. 
  i'd rather just keep doing mach-o like i do now. but that also means 
  i have to replace any of thier runtime stuff that tries to read the elf headers. 
- dlopen() isn't supported on x86-64 MacOS
- they use inline assembly which my import_c doesn't support. 
  - tho they stole chibicc just like i did and they have some `!defined(__chibicc__)` 
  so maybe i could steal their changes to make that work. 
  - most of it is just for doing syscall/svc
- `tinymath` is musl and arm's stuff
- they've got a bunch of extra stuff that isn't libc in the folder called libc. 
i guess it's probably good stuff, so maybe that's what i want, but i'd rather it be 
sorted differently. 
- they've kinda already done the whole thing i want to do with this project. 
like take a little self contained slice of interesting things in the universe
that you can cross compile to all the sane targets. maybe that's a bit disheartening. 
  - their "build from source" instructions starts with downloading a 1.3GB personal copy 
  of clang so that makes me feel a bit better. 
- they've aggressively bought into the tiny files compilation model.
like one per libc function so it can build one .o file per libc function.]
  - heh, i don't need cached compliation, i need cached concatenate the files so you don't have to reopen them all
  - you have to scroll past reincluding things so many times. 
  the lines in lib/(.h+.c) files are (1217+9380)/(46828+123927) = 6% `#include` 
  (counted after collapsing most of the licence headers). 
- they do `_weaken` and `__static_yoink` a lot which looks 
kinda messy but is a good way to solve the bloat problem (like as 
you add new runtime stuff, you need to add support code for it 
elsewhere and then you're stuck doing that whether you use the runtime
or not) which i've thought about didn't have a solution for. 
- the math stuff has hella tables of magic numbers

--- 

:CountingCosmo

```
14.323 total
------
 1.825 str/unicodedata.txt
 1.705 2020 license header
 1.171 testlib/moby.txt
 0.707 2022 license header
 0.518 2021 license header
 0.377 2023 license header 
 0.323 the rest of testlib
 0.290 isystem (remapping thier files to normal includes)
 0.287 2014 musl license header
 0.171 libc/sysv/consts/*.S (gen consts.sh)
 0.116 2024 license header 
 0.087 arm license header
 0.056 2020 musl license header
 0.040 sysv/calls/*.S (gen syscalls.sh)
 0.050 sysv/(errfuns.h, errfuns/*.S) (gen errfuns.sh)
 0.050 vga/vga-font-default.c (1235 bytes of font data)
------
 6.873 the rest
```

- 206716 lines of libc/(.h/.c) =
  -  34582 lines of license headers / ascii art / `/*-*- vi:`
  -  14868 lines in the form `#define NAME TOKEN\n`
  -  10823 lines in the form `#include TOKEN\n`
  -  14778 lines blank
  -  28184 lines comment
  - 103481 lines the rest
