
## general closures

- i did a special case for if but need to do that for all. 
  resolver does it right but need to be able to inline call into same result. 
- painful debugging but made logging a lot better
- want to track captures so know which need inline. 
  was confused by it thinking almost everything was a closure but it's finding the type constants in the outer scope. 
  i guess constants never need to be treated as captures since you just inline them into the function anyway. 

## scope (Feb 7)

- do arg vars based my new thing
- switch the look ups on GetNamed to GetVar, etc. 
- allow shadowing 
- now that vars are numbered, how to do inlining becomes obvious. 
 i ended up just doing it on the bytecode tho but might want to renumber the ast as well later? 
 - forgot to resolve types in functions/vars which was very confusing when tried to switch builtins to being definied as constants in a prelude. 
- putting a bunch of cfg checks in logging functions (mostly in parse.rs) makes it like 2x faster (release mode) 
  so i guess it wasnt optimising out my big recursive traversals even though the log macros did nothing. 
  
 
## Interp (Feb 1)

Having a linear type system seems very debuggable. 
Every computation is put in a new stack slot and replaced with a Poison when passed to a function. 
Every time you read a value, check if its poison. 
Im sure this is super slow tho. 
Pretty cool tho, already catching stupid things like forgot to implement add, just left builtin body empty, lol. 

## Tree Sitter (Jan 28)

I need to figure out what the `•` symbol precicly means in error messages. Brackets not in quotes are like s-expression where the first thing in the list is the name of the production and the rest are its arguments. I think `•` is a sequence separator. So `(call_expr _expr  • tuple)  <-> call_expr(seq(_expr, tuple)`

## Side Tracked (Jan 27)

I'm really annoyed by how slow swift's type inference is so i want to try to write a simple version so maybe ill understand it better. 

cool how you kinda build up tools for starting projects quickly. 
can snatch the lexer from my relational algebra project, tho i guess normally you'd just have a generator for that or whatever. 

string pool. 
idk if lifetimes are gonna get too annoying. 
`*mut str` eq+hash uses the address value, not the str hash impl.
fixed that with a wrapper that does the unsafe deref. 
tried to make it generic over anything that was already hash+eq but couldn't figure out how to express the bound that `&T: Hash`
actually no, thats not the problem, its that you need the `T: ?Sized` on the impl as well as the main struct generic. Similarly, you can't derive Copy/Clone 
because it implicitly adds that bound. 


