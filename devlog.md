
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


