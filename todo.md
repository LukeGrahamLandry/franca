## Sema

- remove the need for forward declarations
- Bc::Index for accessing tuple/struct fields instead of using raw_slice because it seems like a useful semantic difference and makes it easy to use GEP.
- module system: want to be able to seperate things to help lsp.
  - hard to think about how that should interact with wanting to use global overloads as traits.
- nominal type-checking
- clean up tracking backend specific function bodies

## Backend

- get llvm backend to parity with aarch64
- llvm output an executable
- using mir for compiling c dependencies would be cool
- figure out if llvm-sys statically links itself and if it can cross compile

## Ui

- repl
- pass source as cli arg
- support shebang line

## Lsp

- factor out lsp dispatch
- more tolarant parser. Need to be able to represent holes in the ast.
