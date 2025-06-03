
## What's in **a** libc

- startup: tls, relocations, constructors
- pthread: allocate stacks, sync primitives (mutex, cond, rwlock)
- syscalls: magic numbers/structs for talking to the operating system
  - io: open/read/write
  - file system: unlink/mkdir/stat
  - signal handling
  - futex, clone
  - time
  - termios (ioctl)
- stdio
  - buffered file structs w/ variadic helper functions 
  - opendir
  - temp files
  - random numbers
- networking
- jmp: set, long
- dynamic library loading: dlopen, dlsym
- formatting: numbers <-> strings
- a memory allocator: malloc/free
- memory manipulation: cpy, move, set, cmp
- math: trig, log
- null terminated string manipulation
- character grouping: isalpha, tolower, etc, 

## What's the problem

## What's in **this** libc
