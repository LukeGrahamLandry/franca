
This is a semi-arbitrary subset of libc. 

---

For many of my test programs (import_c/test/.c, backend/test/.ssa) 
it's useful to be able to dynamically link against a libc 
and still target platforms that don't provide a native libc 
(wasm, my operating system, any time you want a static binary). 
My programs only need a tiny subset and I care more about simplicity than performance. 

I could have used musl for the platform independent parts 
but I'd still have to port the io parts for non-linux targets. 
I also care about exposing the same ABI on all targets (both architecture and os), 
because it makes my compile time execution when cross compiling much easier to implement 
(i can run sema once and use it for all platforms). 
It also means I can cross compile without figuring out how 
to install a new copy of all the headers for each target platform. 
I couldn't find another libc that doesn't need to know the target at compile time 
(even cosmopolitan is only os generic, it still has ifdefs based on architecture). 
I also have low respect for the compliation model that involves make and a billion tiny object files. 

(WIP: currently the fixed abi thing is true for the franca interface, lib/sys/posix.fr, but not the c one)

---

Each .fr file here corrisponds to one .h file in a classic c distribution. 

Annotations:
- #static are things that don't access any global libc state (even transitively). 
  It's always safe to inline their whole callgraph into each compilation unit. 
  Examples: most of string and math
- #inline are trivial wrappers of other libc functions. 
  Like as #static but non-transitive. 
  Examples: malloc calls calloc
