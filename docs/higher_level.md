# Higher Level Features

There are many features of other languages that mine lacks. 
All general purpose languages are equally powerful, it's just a matter of verbosity. 
This is an incomplete list of features that I haven't found to be worth the implementation 
complexity or performance cost for the type of programs I've been writing thus far. 
If you write different types of programs than I do, you may disagree and not enjoy my language. 

- There is no async or coroutines
  - if you want to do two things at once, spawn a thread. 
  - stackful green threads can be done manually with lib/sys/jump.fr (setjmp/longjmp like). 
    see lib/sys/sync/green.fr for an example. 
- There is no exception handling (throw/try/catch/finally)
  - most errors are not exceptional and should be represented as values (Result(T, E), ?T, etc.).
  - very coarse stack unwinding can be done with lib/sys/jump.fr (setjmp/longjmp like) 
    but it doesn't compose in a sane way so shouldn't be exposed as a public abi. 
  - use a signal handler if you want to catch hardware traps. again, they don't compose in a sane way. 
- There are no interfaces/traits/typeclasses
  - manually desugar it to a struct of function pointers. 
- There is no dynamic typing
  - if you actually want interfaces with dynamic dispatch, see above
  - if you want polymorphism but the code is only used with concret static types, see docs/generics.md
  - for most code there's enough type inference that you don't have to write types on your locals
- There is no inheritance
  - manually desugar it to casting a pointer to a struct as a pointer to its first field. 
- There are no dynamic/uninlined capturing closures
  - manually desugar it to a function with a data pointer parameter. 
- There is no garbage collector
  - manually deallocate your memory when it's no longer needed. 
    arena allocators make this very easy when your data has a predictable lifetime. 
  - you could use a conservative non-moving gc if you try really wanted to (just like you could in c). 
    i don't provide one and it will very likely be slow 
    (because it won't be able to use most of the fun tricks for high performace gcs)
  - you could manually do reference counting but you'll end up with lots of error prone retain/release (or whatever) calls
- There are no destructors 
  - see above. 
- There is no borrow checker. 
  - see above. simply do not make a mistake. 
- There are no constructors
  - write a function that returns a value. give it a meaningful name. 
- There are no methods
  - write a function that takes your struct as the first argument. 
    you can use dot call syntax and overloading allows reusing the same name with different types. 
- There is no message passing
  - write a function, pass it some data, switch on the data. 
- There are no "references"
  - use a pointer.
- There are no private fields/functions 
  - if you don't want to call something then don't call it and it won't be called.
- There are no volatile pointers
  - see the docs/annotations.md section about #noinline.
- There is no advanced pattern matching
  - for the simple case of accessing the active case of an @tagged type, use @match.
  - when you want more control flow, use a bunch of nested if/@match/@switch statements.
- There is no SIMD
  - if you actually cared about maximum performace you'd have to write it in assembly anyway
- There is no package manager
  - if you just want to download a specific zip file of a someone else's code, see examples/fetch.fr
- There is no lsp/ide/linter/(test framework)
  - type the program into the computer and then run it
- There is no "homoiconicity"
  - that's a nonsense phrase that means nothing. 
  - every programming language with a self hosted compiler (of which mine is one) 
    has its programs represented by a data structure in the language. 
  - code is data on every computer that presents a von neumann architecture 
    (which is any that you could reasonably buy).
  - if it makes you feel better you're free to put extra parentheses around any expression. 
