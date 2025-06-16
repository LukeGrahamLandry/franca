# Caching (.frc files)

When you pass a file to the compiler `franca filename.fr`, it will be compiled into a `.frc` file 
and then that will be compiled to native machine code (in memory, not an exe) and run (the entry 
point is a function called `driver` or `main`). The intermediate `.frc` file is saved at 
`{FRANCA_TARGET_DIR || target/franca}/cache/filename_fr.frc`. The next time you try to run 
`franca filename.fr`, the cache is checked before compiling. If the source files on disk 
have not changed, the cached file will be used instead of recompiling. 

- If you run with the `FRANCA_NO_CACHE` environment variable set (value doesn't matter), 
cache files will not be read or written. 
- You can compile targetting `.frc` explicitly by passing `-frc` to 
`@/examples/default_driver.fr` or `@/backend/meta/qbe_frontend.fr`. 
- If you pass a `.frc` file to the franca compiler directly it will be executed WITHOUT 
checking that the source files haven't changed (passing `*ImportVTable` like a precompiled dylib driver). 
- You can pass a `.frc` file to `@/backend/meta/qbe_frontend.fr` to compile it to native code 
(again WITHOUT checking source files). 
- You can run `franca backend/meta/dump_bin.fr filepath.frc` to inspect a file as human readable text.
- The cache files are target specific, they are not intended to be a portable thing you distribute. 

## current limitations

- I don't do fancy incremental compilation. The whole thing is recompiled if 
any byte of any input file changes. However, this is the start of a logical progression 
if I decide that's what I want. 
- This caching is only applied to the initial CompCtx. If your program is a driver that 
builds another program, the real program will still be rebuilt from source every time. 
- I use a slow hash function (sha256 without the magic cpu instructions for it). 
I spend approximately the same amount of time hashing the input files as (decoding the 
cache file and generating machine code for it). Should probably use a different one or 
just trust the file system's last modified dates.
- I can't quite just mmap the binary ir. Half the time is waste. 
- This is not a stable api for interacting with the compiler. I don't care about being able to 
load older versions of the format. Anything with a different version number is rejected. 
It's just an implementation detail that makes it faster to run larger franca programs directly. 
- There's too much of a speed hit on a cache miss compared to FRANCA_NO_CACHE=true

Generated code (like strings passed to `import()` beginning with `{`) are ignored when 
deciding if a cached file can be used. The assumption is that they are deterministically 
generated from your real source files. Since they're not backed by a file, there's no where 
for the compiler to check if they would have changed, it would have to recompile your program 
to rerun all the comptime code which defeats the whole point of the caching. The same applies 
to things that interact with compiler apis like `@macros` and import_c. If your c code `#include`s 
something, that file is not part of the cache validation. 

## contents of a .frc file

See `@/backend/incremental.fr` for the exact binary layout and some helper code for loading/storing it. 

The format is very simple. It's just some arrays of fixed size structs prefixed by a header 
that tells you the size of each array. The main categories of data are: hashes of input files, 
the IR of the program (target specific; after register allocator but not machine code), and 
blobs of bytes for the data segment. 

The IR is a bit wasteful (of disk space). On macos-arm64 `@/compiler/main.fr` (with `-unsafe`) 
produces a 1.1M executable or a 4.9M cache file. However, they are a very direct binary 
dump of the backend's internal ir format so they are quite fast to load (slower than loading 
a .dylib but faster than linking a .o). 

## frc_inlinable

Instead of saving just the minimal ir right before machine code generation, it can be more useful 
to save before any target specific passes are run (so it's still in ssa form, abi agnostic, etc). 
The format can also include more detailed type information for a compiler frontend to use: field 
names, enum constants, pointer types, symbol aliases, etc. The sort of information that would be 
in a c header file. The `import_frc()` function can load these higher level modules as a `ScopeId` 
you can access like normal franca code. The best example usage is `examples/import_c/ffi.fr`, it 
compiles a c program into a single binary .frc blob to be loaded by the compiler. This form takes 
more disk space and is slower to load than the post-regalloc-ir, but it colocates interface and 
implementation so you don't have to write seperate bindings and it maintains the ability to be 
cross compiled (and thus used transparently at comptime). 
