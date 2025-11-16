
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
- pci device discovery

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

- finish virtio queues
- virtio interrupts so i can get input again
- put kprint in a vtable so it can use virtio console once thats set up
- allow choosing console device. so support both pl011 and virtio. 
- keep track of how mmio space is allocated to bars so once i have multiple they don't overlap
- move device drivers to user space. 
- call virtualization.framework from my language and bundle the whole thing into one exe
- support entitlements in my codesign
- make volatile pointers less painful in my language
