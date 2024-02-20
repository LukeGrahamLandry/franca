
## serializing asts

- fix logging of recursive types
- my InterpSend for option always said None cause im dumb
- need to make sure everything has a staticly known serialized size. so padding for enums and vecs as allocations. 

## the road to partial evaluation (Web 19)

Need to track which values are const known to make it easier to deal with baking closures passed as arguments to other functions. 

- resolve_function was typechecking the arg before checking if the func var was just a GetFn instead of an OverloadSet
- sad day remove_named calling Vec::retain with flipped condition so i was removing all args accept the one i bound. 

## sneaky swapping for mutation (Feb 16)

Previously I was cloning a bunch of stuff to deal with all the asts being in the program struct. 
As a seperate adventure, I want to work towards baking all the comptime work into the ast so I can write other backends 
that just have to handle the translation for runtime. So it would be really nice to mutate the ast as I emit code for them.

I suppose I've jsut invented a refcell with extra steps. 

## reworking constants

- each compiled Func needs to track which consts it closed.
 those were the state when you saw its declaration so you can use them when you go to compile its body which might be later. 
- i knew it would be slower but its like a bit unacceptable. 
 
##

Trying to ensure_compiled preemptivly instead of jitting so it can be more like real compilation,
but that makes it pass ((a, b)) isntead of (a, b) to assert_eq. But really its just making 
inline calls actually happen for builtin wrappers which is what i was trying to do anyway.
so we've just learned inlining passes arguments wrong somehow. doesn't unbox them. 
Fuck, it's on a function labeled `// TODO: this needs to be an error`.
distressing that inlining changed behaviour tho. 

## web

static LIB: &str = include_str!(...)
I think I don't want a const because I don't want it inlined at every use (if there were ever more than one).
Do they really do that?

Looking in devtools profiler, print is as long as whole parsing. Need to buffer it. 
TODO: why does opening the devtools profiler make the loc/sec better? is my counting fucked up somehow?

## structs

I need to let expected type flow down the tree so it will treat your map literal as the type of your variable. 
Don't want to deal with a generic map thing that i then convert rn.

## generics

## 

I really like the Zed thing where you get a file of all the errors. 
It's great for mass changes like adding an argument to a function when you're always in a context where you can trivially get the arg anyway. 
Tho sometimes the same place shows up twice which is unfortunate. 
Same err from rustc and rust-analyzer? 

## parser rewrite (Feb 10)

Normal assign left can be a tuple for pattern match-y assign multiple at once (like rust/python).
But that means you can't put an annotation there because its ambigous if the tuple is names or a call on the annotation?
@a    (x, y) = z;   but no, because @a(x, y)   = z; is illegal so what's the problem. 

I gave up. Maybe its just a skill issue but its so easy if you just write the boring code. 
There's no way the parsing is the slow part. I need to have error messages. 
I don't understand why they wouldn't tell me what tokens it was expecting. 
As bonus with the privilege of error messages you also get to trivially target wasm without figuring out how to ask the c compiler to do it. 
It seems like it wont really be that much more code than the hoops I was jumping through to get a useful ast out of thier parse tree.
Maybe I should have tried to find something that genreated types off the tree sitter thing but the only one i saw seemed to 
generate a ridiculously big amount of code. 

## general closures (Feb 8)

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


