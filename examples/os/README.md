
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

## incomplete

- mprotect
- read device tree
- multiple address spaces
- multiple cores
- shell
- sleep
- block device
- file system
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
