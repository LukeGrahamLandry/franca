
the beginnings of an operating system. 
currently it's good enough to run the franca compiler on qemu-system-aarch64's virt machine.

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

## incomplete

- mprotect
- multiple address spaces
- multiple cores
- sleep
- block device
- file system
- exec and capture stdout
- thread groups and clean up resources when all exit
- dlopen
- ptrace
- swapping
- frame buffer
- keyboard 
- mouse
- audio
- network
- test on real hardware
- support riscv and amd64
- gpu
- check for needs_reset flag when waiting for virt queue
- call virtualization.framework from my language and bundle the whole thing into one exe
- support entitlements in my codesign
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
   "closedir", "readdir", 
   "opendir", "sigaction",
   "strtod", "localtime_r", 
   "snprintf", 
   "ppoll", "execve",
   fmodf, fmod, sinf, cosf
```
- compile with import_c and mount it in the vfs:
  https://github.com/libfuse/libfuse/blob/master/example/hello_ll.c 
