Summary of libraries provided with the franca distribution but not implicitly  
available to every program (you can access them with `import()` or `#use`). 

## lib

- collections
  - map: hash table. it's the type with a flat array (no linked lists of buckets). 
  - bit_set
  - deque: dynamic array that tracks start/end so it's fast to push/pop from either end.
  - enum_map: an array with named indices
- alloc
  - fixed_block
- bit_fields: pack values into an integer with less error prone bit shifting. 
- crash_report: dump a stack trace on panic or fatal signal. 
- leb128: variable length integer encoding (used by wasm and dwarf). 
- json: parse a json string. (mostly) non-allocating iterator style. 
- tokenize: skip whitespace in a string. 
- sys
  - sync
    - mutex
    - atomics
    - futex
  - threads
  - terminal: cli colours, interactive input
  - subprocess: launch other programs
- dynamic_lib
- jump: dynamic nonlocal returns (setjmp/longjmp)
- sort
- args: very simple/limited cli argument parsing by reflecting on struct fields 

## backend

The main interface provided here is `backend/lib.fr`: a cross platform compiler backend.  
The rest of these are mainly for internal use but might also be useful for other projects. 

- macho/bits
- elf/bits
- wasm/bits
- amd64/bits: (useful for writing #asm functions)
- arm64/bits: (useful for writing #asm functions)
- llvm/target: WIP
- meta
  - parse: read strings of qbe-style ir (see meta/qbe_frontend.fr for example usage). 
  - template: use parse^ at comptime to generate code for emitting blocks of ir patched with runtime parameters. 

## compiler 

- ast_external
- worker
- codemap
- pool
- lex

## graphics

WIP
