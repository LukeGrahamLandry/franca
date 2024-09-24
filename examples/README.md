## simple

- mandelbrot: prints an ascii art mandelbrot set fractal
- raw_terminal: take input without blocking and clear the terminal
- lox: bytecode vm for the language from craftinginterpreters.com (only ch 14-22 so far)
- bloat: parses the output of objdump and reports the space taken by each function (cli:objdump)
- c_bindgen: parses clang's ast to generate franca function signetures (cli:clang)
- count: counts the number of code lines in a franca file.

## comptime

> These don't require recompiling the compiler because they just use the comptime apis we provide.

- default_driver: a driver program that can be used to build other franca programs (cli:clang, cli:qbe)
- 60fps: recompile mandelbrot every frame
- bf2bc: translate brainfuck to franca bytecode at comptime and include the results as callable functions in your program.
- qbe: an alternate AOT backend that can be used with default_driver (requires https://c9x.me/compile/ installed)

## bundles a copy of the compiler

> These are alternative main functions for the compiler.
> In an ideal world the driver apis would give you enough information to implement these in "user space"

- repl: a franca repl using the comptime jit.
- compiler_gui: uses the compiler as a libary to display introspection about a program (lib:sokol, lib:dearimgui)

## gui

- farm_game: draws some rectangles on the screen with sokol (lib:sokol)
- dearimgui_demo: calls ImGui::ShowDemoWindow (lib:sokol, lib:dearimgui)
