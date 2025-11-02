
this makes a hello world as an elf file that can run in  
`qemu-system-aarch64 -machine virt -cpu cortex-a57 -kernel`  
eventually it would be cool if it was enough of an operating system to host the franca compiler. 

## Resources

- https://wiki.osdev.org/QEMU_AArch64_Virt_Bare_Bones
- https://krinkinmu.github.io/2021/01/04/aarch64-exception-levels.html
- https://krinkinmu.github.io/2021/01/10/aarch64-interrupt-handling.html
- https://lowenware.com/blog/aarch64-gic-and-timer-interrupt/
- https://krinkinmu.github.io/2024/01/14/aarch64-virtual-memory.html
- https://grasslab.github.io/NYCU_Operating_System_Capstone/labs/lab8.html

Arm's actual docs are often very useful (more so than random people's blogs) 
but you have to already know the magic words to search for. 
Also they're helpfully spread across many pdfs and links to their website often die, 
so good luck with that. 
