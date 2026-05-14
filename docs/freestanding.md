# Being Imported / Freestanding 

Most franca code expects some environment setup to be done before being called. 
Mainly that the thread's callstack will be allocated with a specific alignment (franca_required_stack_bits) 
and a StaticTls will be initialized at the base of it (contains an allocator, panic hook, saved env/cli args, etc.). 
(see also ./debugging.md##Thread Locals <!-- TODO: move that info here and reference from there? -->). 
More generally most programs expect a unix-y file system to be available. 

The main point I'm trying to make with the following list of examples is that 
there's always some host that runs your program in some environment 
that exposes some api for interacting with the outside world. 
Different libraries have different levels of dependence on that environment. 
You can always run code in an environment it was never intended to target 
by "just" implementing the apis it expects using the apis the target provides. 

## Examples

- lib/context.fr/franca_runtime_init  

If you only need macos/linux executables where the main() function is written in franca, 
you never need to think about this. When you build an executable with examples/default_driver.fr 
the compiler will adds a bit of setup code that runs before your actual program. 
That setup code is also written in franca, it mostly just saves some arguments from the dynamic loader, 
swaps to a newly allocated stack with the right layout for StaticTls, and calls your main function
On linux, it also detects whether a libc was linked and if not does elf data relocations itself 
and sets a flag for the rest of the sys library to make direct syscalls instead of using libc.

- <https://git.sr.ht/~lukegrahamlandry/rustc_codegen_ferb/tree/a8106f9db78bcebca7df071174b3aec5f11777e2/item/franca-sys>
  - build.rs that passes through target arch,os so cross compiling works when building an object file. 
  - exports.fr that swaps stacks before calling other franca code so my weird way of doing tls works

Sometimes you don't want the franca code to own the main entry point of your program. 
For example if you write a library in franca and then want to call it from another compiled language. 
After compiling a static/dynamic library, other languages can call franca functions 
by following the extern c abi. However, franca_runtime_init won't have been called 
so extra care is required before StaticTls can be used. 
Once the boundary is figured out, the franca code can make syscalls or use libc as normal. 

- tests/external/wasm4.fr/build_franca <https://wasm4.org/play/mandelbrot>
- examples/chess/web.fr <https://lukegrahamlandry.github.io/ChessBot>

A more extreme version of that is a wasm module that can only use 
specific host imports because it needs to be embedded in someone else's program. 
A common shape of program is a web interface that calls into wasm for more efficient raw computation. 
So the franca side might not need access to a whole unixy environment.  

- examples/(web, os/host/web.fr) <https://franca.lukegrahamlandry.ca>

Similarly, the web playground needs to run the compiler in wasm and I want most of my programs to work without being manually ported. 
It needs to run in a web browser so it can't assume a unixy file system or libc or syscalls, 
but I do control the host code so it can give whatever exports I need 
(ex. spawning threads, jitting new wasm modules, graphics with webgpu, event handlers). 
Files are just data in linear memory, syscalls become function calls, processes are web workers. 

- examples/os/(build.fr, kernel/start.fr, user/init.fr)

You can also write operating systems in franca. 
That adds another layer of setup (interrupt/fault handling, page tables, reading device tree, io drivers, smp, etc.) 
on top of whatever api user space expects (file system, threads, exec, etc.). 
I have an example for aarch64 that can run most of my console based programs (including the compiler itself). 
I've only run this in qemu's vert machine and apple's virtualization framework 
but the general technique applies to real hardware as well (you'd "just" need to write different drivers for it). 
