
## big programs

- import_c: a C11 compiler using the franca backend
- import_wasm: a (WIP) web assembly runtime using the franca backend
- os: bare metal aarch64. context switching, virtio drivers, and a subset of libc. 

## @/graphics (examples/gpu/)

> macos / web only

- terminal: minimal terminal emulator that can exec processes and show thier output.
- chess/gui: play chess against the machine
- stackie: a clone of https://c50.fingswotidun.com
- mandelbrot: interactive mandelbrot set but fast because gpu
- viewer: open image files
- farm_game: draws some rectangles on the screen
- app_events: open a window and display all the (key/mouse/etc) events received 
- multiplexer: switch between multiple different running graphics programs
- hello_triangle: everyone's favourite program
- trivial_compute: add arrays on the gpu
- geo: decompress a LAZ file and render the point cloud
- **unfinished**: depth_test, epicycles, lambda

## languages

- prospero: render CSG images by evaluating expressions with optimisations based on interval arithmetic
- kaleidoscope: jit for a tiny expression langauge (based on an llvm tutorial but using my backend instead)
- bf: include a brainfuck program in a franca program with various flavours of meta-programming. 
  - interpreter, franca source, c source, ir, wasm
- lox: bytecode vm for the language from craftinginterpreters.com (only ch 14-22 so far)
- minijinja: interpret a tiny subset of jinja templates
- **unfinished**: import_wuffs

## formats

- dump_macho, dump_elf, dump_wasm: inspect executable files
- macho_loader: mmap a mach-O executable and run it (just like the dynamic loader would)
- elf_loader: ditto but for executable-and-linkable-format
- show_tar: list the contents of .tar/.tar.gz/.zip files
- dump_devicetree: decode the binary format bootloaders use to tell operating systems about hardware

## small programs

- default_driver: a driver program that can be used to build other franca programs 
- 60fps: interactive ascii mandelbrot (recompiled every frame)
- repl: a franca repl using the comptime jit.
- turing_art: randomized turing machines draw on a colourful canvas (w/ ansi terminal escapes)
- c_bindgen: parses clang's ast to generate franca function signetures (cli:clang) (superseded by import_c)
- bloat2: shows space per function in mach-o/elf/wasm binaries
- sudoku: solve puzzles 
- mandelbrot: prints an ascii art mandelbrot set fractal
- raw_terminal: take input without blocking and clear the terminal
- count: counts the number of code lines in a franca file.
- aoc: first half of advent of code 2024/2025
- check_source: scan a directory for confusing unicode characters and long lines 
- some tiny reproductions of strange behaviour in examples/toy/

### deps

- soft_draw: a minimal graphical mandelbrot program for macOS
- bloat: parses the output of objdump and reports the space taken by each function (cli:objdump)
- view_image: print a jpeg of the mona lisa to the terminal by importing a c library (lib:wuffs)
- ascii_table: uses stb_truetype and stb_image_write to generate a simple image
