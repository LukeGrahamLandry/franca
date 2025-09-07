
> stuff i have to remember to update when adding a new target environment

## adding an architecture

- driver_abi.fr/Arch enum
- backend
  - abi: RCall, par, arg, call, ret, vastart, vaarg
  - isel
    - flag ops: cmp+jnz, sel
    - emulate unsupported instructions
    - fold memory access
  - emit
    - setup a stack frame
    - new types of relocations
    - UnfilledGotAccessStillNeedsPatch
  - bits: instruction encoding
  - elf: dynamic loader path and e_machine magic number
  - set regalloc masks in ir.fr/Target 
  - arch_name for llvm_mc_dis
  - test/asm-arch.ssa
- AsmFunction 
  - plumbing for the frontend
  - lib
    - entry point: fix_stack
    - jump.fr: setjmp, longjmp
    - process.fr: current_arch
    - atomics.fr: fence
    - crash_report.fr: trace_start
    - linux.fr: perform_clone
    - syscall.fr: perform_syscall
  - tests
    - inline_asm_jit.fr
    - intrins.fr
    - multiple_stacks.fr, elf_loader.fr: call_in_stack
- lib
  - syscall.fr: linux syscall numbers
  - posix.fr: flags and struct layouts (i.e. mmap, stat, etc.)
  - signal handling struct layout
  - context.fr: do_relocations_static_linux magic number for absolute relocation
- what are the instruction cache coherence rules for jitting? 
  - process.fr: clear_instruction_cache
- find a nice userspace emulator for running tests
- there is lots of code that assumes little endian and valid unaligned accesses so good luck with that 
