- hello: prints hello world
- bf: translates brainfuck to c
- mandelbrot: prints an ascii art mandelbrot set fractal
- raw_terminal: take input without blocking and clear the terminal
- lox: bytecode vm for the language from craftinginterpreters.com (only ch 14-18 so far)

- bloat: parses the output of objdump and reports the space taken by each function (cli:objdump)
- farm_game: draws some rectangles on the screen with sokol (lib:sokol)
- c_bindgen: parses clang's ast to generate franca function signetures (WIP) (cli:clang, lib:wuffs)
- default_driver: a driver program that can be used to build other franca programs (cli:clang, cli:qbe)