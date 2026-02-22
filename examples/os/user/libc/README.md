
This is a semi-arbitrary subset of libc. 

---

For many of my test programs (import_c/test/.c, backend/test/.ssa) 
it's useful to be able to dynamically link against a libc 
and still target platforms that don't provide a native libc 
(wasm, my operating system, any time you want a static binary). 
My programs only need a tiny subset and I care more about simplicity than performance. 

(WIP: currently the fixed abi thing is true for the franca interface, lib/sys/posix.fr, but not the c one)
the impl folder is for my operating system

---

Each .fr file here corrisponds to one .h file in a classic c distribution. 

Annotations:
- #static are things that don't access any global libc state (even transitively). 
  It's always safe to inline their whole callgraph into each compilation unit. 
  Examples: most of string and math
- #inline are trivial wrappers of other libc functions. 
  Like as #static but non-transitive. 
  Examples: malloc calls calloc

(WIP: i don't do anything with #static or #inline yet)
