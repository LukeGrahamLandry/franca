
See `../annotations.md` for info about builtin tags you can put on functions. 

```
#asm #x86_bytes #aarch64
#linkrename
#target_os
#libc
#import
#syscall
#ir
#intrinsic
#comptime_addr
#redirect
#macro
```

- import() and #use
- add_comptime_library
- @import_symbol
- lib/dynamic_lib.fr open()/get() -- same as dlopen/dlsym
- import_c/ffi.fr/include()
- add_to_scope, intern_type, intern_func
- using the backend library to insert ir directly
- fetch_or_crash

Talk about doing stuff from within comptime vs through a CompCtx. 

Talk about jit shims. Why the compiler needs to know if a symbol is a function or a data variable. 
Mention "tried to call uncompiled function" errors.
