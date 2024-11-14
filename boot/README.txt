since the compiler is self hosted, you need an old version to build a new one. 
it is built by `compiler/first.fr -unsafe -boot-only`.
this copy has some safety/error handling removed to make it smaller, so you probably want to rebuild locally without those flags. 

note also that this is not the lastest version of the compiler.
it's just an arbitrary old one that is new enough to understand the code for the lastest compiler. 
it doesn't need to be updated to support a new language feature until the compiler itself uses that language feature. 
