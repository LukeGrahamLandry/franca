
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
- virtio console in/out

## incomplete

- mprotect
- multiple address spaces
- multiple cores
- sleep
- block device
- file system
- shared file system (ex. VZVirtioFileSystemDevice)
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

---

- non-blocking virtio queues
- virtio-console input handle more than one byte so paste works
- keep track of how mmio space is allocated to bars so once i have multiple they don't overlap
- move device drivers to user space. 
- call virtualization.framework from my language and bundle the whole thing into one exe
- support entitlements in my codesign
- make volatile pointers less painful in my language
- accel-hvf and vzf use 100% cpu when spinning in wfi
- vzf: sometimes you don't get the `>>>` prompt until you hit a key
