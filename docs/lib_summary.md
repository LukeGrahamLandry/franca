Summary of libraries provided with the franca distribution but not implicitly  
available to every program (you can access them with `import()` or `#use`). 

## lib

- collections
  - map: hash table. it's the type with a flat array (no linked lists of buckets). 
  - bit_set
  - deque: dynamic array that tracks start/end so it's fast to push/pop from either end.
  - enum_map: an array with named indices
  - bucket_array: dynamic array with pointer stability
  - page_map
- alloc
  - arena
  - fixed_block
- bit_fields: pack values into an integer with less error prone bit shifting. 
- crash_report: dump a stack trace on panic or fatal signal. 
- tokenize: skip whitespace in a string. 
- sys
  - sync
    - mutex
    - atomics
    - futex
  - threads
  - terminal: cli colours, interactive input
  - subprocess: launch other programs
  - fs: interact with files
  - process: get information about your execution environment 
- dynamic_lib
- jump: dynamic nonlocal returns (setjmp/longjmp)
- variadic: call/define variadic functions that follow the system extern-c abi (equivalent to stdarg.h)
- sort
- args: very simple/limited cli argument parsing by reflecting on struct fields 
- mix
  - sha256
  - random: `xoshiro256**`
- encoding
  - leb128: variable length integer encoding (used by wasm and dwarf). 
  - base64
  - json: parse a json string. (mostly) non-allocating iterator style. 
  - json_reflect: generates functions that convert between structs and json strings

## backend

The main interface provided here is `backend/lib.fr`: a cross platform compiler backend.  
The rest of these are mainly for internal use but might also be useful for other projects. 

- macho/bits
- elf/bits
- wasm/bits
- amd64/bits
- arm64/bits
- rv64/bits
- meta
  - parse: read strings of qbe-style ir (see meta/qbe_frontend.fr for example usage). 
  - template: use parse^ at comptime to generate code for emitting blocks of ir patched with runtime parameters. 

## examples

- import_c/ffi: compile c code and call it without writing bindings
- import_wasm: load web assembly modules at runtime or comptime. 
- chess
  - moves: board representation and legal move generation
  - uci: parse/write Universal Chess Interface commands/responses

## compiler 

- ast_external
- codemap
- pool
- lex

## graphics

> WIP. macos-aarch64-metal only. 

- gfx: 3D-API abstraction layer (platforms: Metal, wip: WebGPU)
- app: open a window, get a 3D-context, handle input events (platforms: MacOS)
- gl: OpenGL 1.x style immediate-mode rendering API
- debugtext: a simple ASCII text renderer using vintage home computer fonts
- shaders: translate a subset of franca to MSL/WGSL
- vec: 2/3/4-D linear algebra
- easy: reduce boilerplate for small example programs
- replay
