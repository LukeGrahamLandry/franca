since the compiler is self hosted, you need an old version to build a new one. 
it is built by `compiler/first.fr -unsafe -boot-only`.
this copy has some safety/error handling removed to make it smaller, so you probably want to rebuild locally without those flags. 
