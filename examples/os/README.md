
the beginnings of an operating system. 
currently it's good enough to run the franca compiler on qemu-system-aarch64's virt machine.
it also runs doom (i don't have a keyboard driver yet so just the intro render)

```
franca examples/os/build.fr -vzf -append "spawn doom;" -fetch-doom -smp 2 -graphics
```

Many new operating systems exist because their authors have 
a grand unified theory of security or interprocess communication. 
This one does not, so maybe operating system is the wrong word for it 
but I don't know a better one. It's just a program that allows other programs 
(that were written with the assumption that there is an operating system) to work.
Currently all user code runs in the same address space 
and the file system just exists in memory so most libc things 
that would usually be syscalls are just normal functions. 

For debugging you can also run the userspace part without the kernel 
(run examples/os/host/user.fr for linux or examples/web/build.fr for browser). 
The later is used (as an emscripten alternative) by the 
[franca playground website](https://franca.lukegrahamlandry.ca).

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
- use multiple cores

## incomplete

- MemDir ref counts / shallow copying children is very sketchy
- mprotect
- multiple address spaces
- block device
- persistant file system
- clean up resources when main thread in a group exits
- wait, exit_group vs exit_thread
- posix_spawn an actual program not just a stub that prints something useless
- don't busy wait in poll
- don't recycle tid so fast
- @trace into its own pipe so it doesn't stomp on your processes output. 
  also needs to be thread local since i toggle it in the shell
- sigaction api
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
- vzf: sometimes you don't get the `>>>` prompt until you hit a key
- why does mmap die sometimes??
- be able to spawn a thread that's disallowed from using specific libc functions. 
  ie. my compiler's codegen thread needs mmap+futex+exit and nothing else
- rest of the libc functions needed for all my tests:
```
   "unlinkat", 
   "mkstemp",
   "readlinkat",
   "sigaction",
   "localtime_r", 
   "snprintf", 
   "execve",
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
  - pthread_getcpuclockid is a stub rn and clock_gettime is always system uptime. 
- when you rename a directory its `..` entry needs to change
- `..` out of a fuse file system into the normal one
