- import_c: a C11 compiler using the franca backend
- import_wasm: a (WIP) web assembly runtime using the franca backend
- lox: bytecode vm for the language from craftinginterpreters.com (only ch 14-22 so far)

- prospero: render CSG images by evaluating expressions with optimisations based on interval arithmetic
- kaleidoscope: jit for a tiny expression langauge (based on an llvm tutorial)
- dump_macho, dump_elf, dump_wasm: inspect executable files
- macho_loader: mmap a mach-O executable and run it (just like the dynamic loader would)
- default_driver: a driver program that can be used to build other franca programs 
- 60fps: interactive ascii mandelbrot (recompiled every frame)
- bf: include a brainfuck program in a franca program with various flavours of meta-programming. 
  - interpreter, franca source, c source, ir, wasm
- repl: a franca repl using the comptime jit.
- hacky_incremental: call a function in a loop and hot load recompile it when the file changes
- view_image: print a jpeg of the mona lisa to the terminal by importing a c library (lib:wuffs)
- turing_art: randomized turing machines draw on a colourful canvas

- app_events: open a window and display all the (key/mouse/etc) events received 
- mandelbrot_ui: interactive mandelbrot set but fast because gpu
- farm_game: draws some rectangles on the screen
- terminal: minimal terminal emulator that can exec processes and show thier output. (lib:stb_truetype)
- geo: decompress a LAZ file and render the point cloud
- soft_draw: a minimal graphical mandelbrot program for macOS

- c_bindgen: parses clang's ast to generate franca function signetures (cli:clang)
- sudoku: solve puzzles 
- mandelbrot: prints an ascii art mandelbrot set fractal
- raw_terminal: take input without blocking and clear the terminal
- bloat: parses the output of objdump and reports the space taken by each function (cli:objdump)
- count: counts the number of code lines in a franca file.
- aoc: first half of advent of code 2024
- check_source: scan a directory for confusing unicode characters and long lines 
- ascii_table: uses stb_truetype and stb_image_write to generate a simple image

## unfinished 

- depth_test
- epicycles
- lambda
