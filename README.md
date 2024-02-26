# Franca

- almost every function can run at compile-time.
- no seperate build config, everything is done with comptime execution.
- comptime functions can manipulate and create types.
- macros are functions that directly manipulate the ast  

## Tradeoffs

I strongly believe in conservation of misery so if there's a bunch of stuff I like about this language
there must be propoertional terrible things or I'm probably just lying. 

## Progress

- `src` contains the frontend and the interpreter used for comptime execution. 
- The programs in `tests` and `lib` actually work on the existing interpreter. 
- The programs in `plan` are just trying out how potential language features feel. 
