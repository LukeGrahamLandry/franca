## Sema

- remove the need for forward declarations
- module system: want to be able to seperate things to help lsp.
  - hard to think about how that should interact with wanting to use global overloads as traits.
- nominal type-checking
- clean up tracking backend specific function bodies
- transitive function annotations (@env, @ct, eventually @async) where you get one if you call someone that has one.

## Replacing Interp

- give executors access to the compiler so dont need the hacky message passing with errors
- convert bytes + type back to Values

## Backend

- get llvm backend to parity with aarch64
- llvm output an executable
- using mir for compiling c dependencies would be cool
- figure out if llvm-sys statically links itself and if it can cross compile
- reference counting and deduplication of heap constants
- be able to serialize asm for any function so can cache macro handlers. is it faster to hash source than recompile?
- explicit uninit vars to make asm allocate slots
- trying to call print on asm tries to call builtin alloc somehow
  I think the problem is that you need two versions of things that are called at both comptime and runtime when not for the same architecture.

## Ui

- repl
- pass source as cli arg
- support shebang line

## Lsp

- factor out lsp dispatch
- more tolarant parser. Need to be able to represent holes in the ast.
