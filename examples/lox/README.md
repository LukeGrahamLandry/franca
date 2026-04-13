predecessor of this code: https://github.com/LukeGrahamLandry/Lox
based on Crafting Interpreters by Robert Nystrom (https://craftinginterpreters.com).
https://github.com/munificent/craftinginterpreters/tree/4a840f70f69c6ddd17cfef4f6964f8e1bcd8c3d4/c (MIT)

Following that book was how I wrote my first compiler so it seems cool to use that as a test project for my new language. 
see also: tests/external/lox.fr

## vm

> franca examples/lox/lox_main.fr a.lox

this is a bytecode vm for Lox: a dynamically typed, object oriented language. 
It's a pretty uninspired port of clox (the second implementation in the book). 
Added extensions to run loxlox: u16 constants and getc/ch/exit/print_error natives. 

## aot 

> franca examples/lox/aot.fr -i a.lox -o a.fr && franca a.fr

This is a fun addition that's not in the book. 
This compiles a lox program to bytecode and then to franca source code. 
It's a very naive mapping. Most opcodes are just a call to their vm implementation 
(all values are still on the vm's stack, not lifted to franca variables). 
Currently I don't preserve the source cfg. Jumps are represented as `loop(=> @switch(ip) { ... });`. 
However `ip` only needs to be reified for jump instructions, flat code avoids the interpreter's 
dispatch/decoding overhead. The extra bonus is that the Opcodes'FOO functions are #inline so 
it gives the backend some opportunity to remove junk across instructions.  
