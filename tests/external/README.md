these are tests that use my tooling with other people's programs. 
so they download a bunch of random stuff by execing curl, git, etc. 
i think it's cool to demonstrate that my programs can be useful to other people, 
but i want to keep a clear separation between (the part of franca that's self contained 
programs where i understand all the code) and (the part where you suddenly have 
a transitive dependency on the whole internet so the tests fail when cloudflare has an outage).

c programs to test import_c:
- lua: interpreter written in c 
- luajit
- wuffs: langauge that compiles to c
- tcc: c compiler written in c (that i can bootstrap with my c compiler) + their tests
- qbe's minic/miniyacc: c compiler that generates a couple of the committed .ssa tests
- bubblewrap: linux sandbox
- clox: bytecode vm
- raylib: "simple and easy-to-use library to enjoy videogames programming"
- curl: "command line tool and library for transferring data with URLs"
- janet: "a dynamic language and bytecode vm" 
- hare: "a systems programming language designed to be simple, stable, and robust"

compilers that output qbe ir as text for qbe_frontend.fr:
- hare

wasm programs to test import_wasm:
- wasm_spec: the .wast spectests (most of which i fail currently)

### sadly

- (bubblewrap, raylib, curl) need system headers
- wasm_spec needs wast2json
- wuffs downloads their precompiled c code instead of building it from source (need to bootstrap a go)

but the rest should Just Work (tm)
