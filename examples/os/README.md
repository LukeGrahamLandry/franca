
the beginnings of an operating system. 
currently it's good enough to run the franca compiler on qemu-system-aarch64's virt machine.
it also runs doom (i don't have a keyboard driver yet so just the intro render)

## implemented

- interrupt handling
- pl011 uart (print to serial port)
- enable mmu
- over commit virtual memory
- load elf executables
- spawn concurrent threads
- timer for preemption
- futex wait/wake
- signal user space on fault
- minimal shell
- read device tree to get addresses for io devices
  - pci device discovery
- virtio device drivers
  - console
  - fs (FUSE)
  - gpu (2d framebuffer for software rendering)
- virtual file system in userspace 
  - makes it easy to port programs that expect a unix-like everything-is-a-file interface
- a subset of libc

## incomplete

- mprotect
- multiple address spaces
- multiple cores
- sleep
- block device
- persistant file system
- exec and capture stdout
- thread groups and clean up resources when all exit
- dlopen
- ptrace
- swapping
- keyboard 
- mouse
- audio
- network
- test on real hardware
- support riscv and amd64
- gpu
- check for needs_reset flag when waiting for virt queue
- make volatile pointers less painful in my language
- accel-hvf and vzf use 100% cpu when spinning in wfi
- vzf: sometimes you don't get the `>>>` prompt until you hit a key
- why does mmap die sometimes??
- replace the web demo with the userspace part of the os so i don't have two hacky subsets of libc
- be able to spawn a thread that's disallowed from using specific libc functions. 
  ie. my compiler's codegen thread needs mmap+futex+exit and nothing else
- rest of the libc functions needed for all my tests:
```
   "unlinkat", 
   "wait4", "pipe", "mkstemp", "dup2",
   "renameat", 
   "readlinkat", "mkdirat", 
   "sigaction",
   "localtime_r", 
   "snprintf", 
   "ppoll", "execve",
   fmodf, fmod, sinf, cosf
```
- compile with import_c and mount it in the vfs:
  https://github.com/libfuse/libfuse/blob/master/example/hello_ll.c 
- generate things from examples/os/user/libc
  - examples/import_c/include.fr
  - generate examples/import_wasm/run.fr/Exports/env from examples/os/user/libc
    (harder because you need to deal with offsetting pointers)
  - examples/web/demo.fr/for_exports
    (will probably be made redundant by doing all the web stuff with my os userspace)
- jit libc functions lazily, don't just include them all in init. 
- usb driver
- keyboard input for doom
- other clock types. real time, thread time
- when you rename a directory its `..` entry needs to change
- `..` out of a fuse file system into the normal one
