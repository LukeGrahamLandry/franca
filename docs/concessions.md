# Concessions

there are several language choices, where the benifit is high enough that i can't take them out 
but the downsides are also high enough to be uncomfortable. often these situations materialize 
in places that seem different from what many other languages do, it's almost like other people know what they're doing. 
but hey i work at the learning factory not the industry factory so that's not necessarily a bad thing. 

Mar 24, 2025. 

## jitted comptime execution

- but its super wasteful for small functions that only run once. 
i don't super want to be in the business of writing a tiered jit at this stage.
- writing an interpreter that lets you do everything that native code can seems like a massive pain. 
- it limits the compiler to running in environments that behave like real computers. 
like wasm is a pain, i think creating a new module every time i add a function is not super feasable. 
- this also plays into the unsafety thing, any random library can run aritrary code at comptime and that code 
is real native code that can do whatever it wants. but also any random library can run arbitrary code in your program 
anyway so it's not really that different. 

## spending a register on dynamic scope

- don't need to do thread locals the normal way which i think you can't do dynamically 
so i'd need to do my own thing for jitting anyway 

- i use a weird calling convention
  - means you can't just be a normal function that c can call anymore. 
  someone needs to smuggle the magic pointer around (and the franca side has to set it as the blessed environment). 
  - wasteful shuffling the register since you can't trust the callee to give it back
  
- clearly the next step is to make it easy for any code to add new fields to that environment struct. 
  - if you let them be different at runtime/comptime then field offsets can't be const anymore.
  they'd be delayed to emit_ir and you'd have to be careful not to write code that looks at them too closely. 

## bake_relocatable_value

- it's super nice to be able to generate your data at comptime and not worry about how to serialize it into array literals
- the ordering gets confusing if you try to use a value at comptime after you've already baked it. 
if you try to have a constant that gets initialized gradually and then frozen, you have to be super careful 
that it's ready before the first time the compiler notices it's reachable from AOT code. 
which means i can't change compliation order without breaking programs. 
- aliasing the same memory as different types is considered disrespectful. 
like what if you see it as a Str and then a CStr and want to mutate the bytes expecting them to be aliased like they were at comptime. 
- i need to handle internal pointers and keep them aliased. currently they'll be emitted as a seperate constant. 

## #where

- needing to manually instantate templates kinda sucks and forces complication to be more strictly ordered than i want.
always passing the type as a const arg is kinda verbose. 
- i dont want to add a seperate language of traits on top of the normal language. 

## #use field

- it only feels good if it works everywhere so you any code that wants to do reflection is forced to think about it. 
like you should be able to @match on a struct that #uses a tagged, so that ends up with another 
copy of the code to walk through the fields that needs to be kept in sync with what the compiler does. 
- it feels wrong to put in a bunch of extra code that doesn't give you any new power, it just saves typing the field names a few times. 
but as soon i find myself choosing how to structure my program based on which will have the least annoying typing,
it starts sounding pretty important to make things less painful whenever possible. 

## driver_api

- there's not enough separation between the compiler's internal data structures and what gets sent to comptime code (ie. for macros). 
which makes it super hard to change the data structures. 
- there's no stability in what shapes of trees your macros will see and the compiler rewrites them as it does sema 
so you can't see the code you wrote and the inferred types at the same time. which is in fact probably the main information you want. 

## global overload sets

- it sucks that the decision of a function being usable as an overload set is made at the declaration site. 
so if you want to use someone else's code, you better hope you like thier style or it's gonna suck. 
and it being based on the declaration syntax also gives you a changing-dots-to-arrows-esc problem. 
- if a library adds an overload where all the parameters are common types then it's easy to collide. 
but having something like the orphan rule makes the common case slightly more annoying. 

## non-incremental

- the other thing is that my only motivation for making the compiler faster is impatience. 
i'm willing to put up with waiting about a second for my program to compile so that's how long it takes. 
when it was 8k lines it took about a second to compile, now it's 30k lines and it takes... about a second to compile. 
it drifts over time and when it gets unbearable (like 1.3s) you spend a week making it fast again. 
im concerned that if i cheat and make it twice as fast by doing half as much work it would just rot back to taking about a second. 
because that's how long i will accept. 
- i also don't believe in a compilation model that involves someone else's linker. 
like if you wanted to make a game run faster and your suggestion was to:
  0. take every 20x20 square of pixels and save any information needed to render that to disk 
  1. start a new process per square, read back the files, do whatever rendering work, and then write the result out to a new file 
  2. start a new process to read all those images back again and stitch them together into your final frame
  3. claim thats faster because all those processes could run in parallel  
... you're fired right? so why is that how you're supposed to write compilers? 
just to be clear i acknowledge that a toned down version of that description has some overlap with what a gpu is, 
im not anti-threads-existing, i just feel we could perhaps communicate in a way that involved sharing information 
instead of trying to transmit it through magnets. 
presumably there's some sane middle ground where i wouldn't have to recompile everything because you change one character in a string literal, 
but i just want to be very careful not to start smoking crazy pills. 

## no llvm

- llvm provides a valuable service. however, i don't think anyone that has actually used it would argue that it sparks joy. 
it's big enough that i could measure the extra time it takes the dynamic loader to launch my compiler (before it gets to main()!) 
even without actually calling into llvm to do anything... imagine how slow it is if you actually use it for anything. 
so if i don't want to link against it and i don't care enough to generate bitcode, that leaves generating the text ir... which sucks. 
anything that involves mushing strings together is not the way to go. 
- its more code than i will write in my lifetime and it doesn't even do all the stuff you want. 
like you have to do a bunch of c abi stuff yourself. so you're paying the complexity cost of a bunch of stuff you don't want 
and in return you still have to do a bunch of stuff yourself. 
- it is however very sad to have to say yeah i like my language but programs written in it are 
twice as slow as they should be because llvm makes me sad and i don't want to touch it. 
- but on the other hand, the main benchmark i care about is how long the compiler takes to compile itself, 
and using llvm is a net loss on that metric. 
- the sane answer would be to have mine for dev builds and thiers for release builds but did i mention it does not spark joy. 
i did start down the path of adding back the ability to output thier ir but like... ugh, i don't wanna work on it man, so here we are. 
- my backend generates worse code and was a lot of work and occasionally you get to debug very unpleasent bugs,
but every time i work on it i feel like i learned something which is the whole point of this project. 
if i just wanted a compiler... there are already compilers.
- also there are things i want to do that require more control than i know how to ask llvm to give me. 
like the stuff with running code in a module while adding new functions and being able to have shims 
that let you call functions that aren't compiled yet and break out into the compiler to fill them in as needed. 
im assume it's possible to do that with llvm but i don't feel i have a sane path towards understanding it enough to make that actually work. 
- in conclusion, i don't want to go to my fun project every day just to sell another piece of my soul to Chris Apple 
in exchange for a compiler that doesn't solve my problem anyway. 

## wrapping main

- it feels so invasive but it's clearly the right choice (and everyone does it too, yes even c)
- on macos you can almost get away with not doing it because they force you to use libc hard enough 
that the loader does lots of stuff for you before even calling the exe's entry point. 
but on linux you need to align the stack and call into the libc setup function so you've already lost. 
- but as soon as you lean into doing it and decide you're allowed to stick some extra code on everyone's program,
a lot of problems just disappear.
- need to setup the env register, temp allocator, etc
- track cli arguments
- you end up with some section of code at the beginning where you need to carefully remember which language 
feature can't be used yet. which is error prone and if you break something along that path, nothing works at all 
so you can't debug it on simpler programs. 

## unsafety

- i certainly go to memory safety jail but like are we sure that's the source of most of the bugs? 
like a gc doesn't help me not typo the bits when i transcribe them from the ISA spec. 
the worst of my problems were probably things with not following the rules 
about flushing the instruction cache and memory barriers in my mutex. 
but the problem there is me misunderstanding the rules and having trouble 
finding a resource that goes more in depth than "the jit compiler is a primitive provided by the operating system" or whatever. 
there's no amount of letting the machine decide when to call free() that would help that, it's not just remotely the right threat model. 
interesting that this is entirely the opposite of my opinion on static type checking. 
- certainly my opinion is shaped by the only nontrivial program ive written being one 
that is so short lived it's fine to just leak all the memory and reclaiming anything at all 
is an optimisation because it irritates me to see a lot of page faults in the profiler. 
- but i also feel like it's just taught in a silly way. like if you think of programs as needing to call malloc 
once every time you need some new space and remember to call free the moment you don't need it anymore. 
that's like a lot of remembering and im pretty bad at remembering so like i cant do it. 
but then... just don't do that? like as soon as you start grouping things by lifetime into arenas that you can 
just discard all at once, some large subset of the memory management becomes trivial. 
and im pretty sure most programs have mostly stuff that lives a super short time or a super long time. 
like that's what the garbage collecter people think too right? my impression is that the serious ones are generational. 
which is more fine grained than my temporary vs forever split but it's the same core idea. 
- im much more interested in things that provide stronger proofs about program behaviour or 
a capability system so you could do things like guarentee some library never phones home 
(which presumably could only be enforced by adding more safety rules). 
