These are intended humorously but there's a kernel of truth to them.  
There is no **tHe HeAp**, there is no **tHe StAcK**, there is no **tHe LiBc**, 
and there is no Queen of England. These words have the unfortunate 
trait of being information I already know phrased in a condecensing way 
that would really irritate me if I hadn't written them. So if you share
my personality deficiencies, maybe skip this one. 

## a memory allocator 

Languages that admit memory exists as a concept generally provide a way for your 
program to ask for more of it as needed. Sometimes that is described as allocating 
on **tHe HeAp**. That phrase is misleading. They don't mean that the memory management 
strategy uses a Heap data structure, they mean there is some memory in a pile and when 
someone wants a piece they can find one in the pile.  

## a thread's callstack 

If you say the phrase "The Hardware Stack" and think that is a special physical object, 
you are an idiot, sorry. Your thread's callstack is just whatever region of memory the 
stack pointer happens to point at. For that matter, the stack pointer isn't special 
either, it's just a register that everyone who follows a certain ABI has decided 
to use for pointing at your thread's callstack. Although that argument is made less 
convincing by the fact that both arm64 and amd64 have special instruction encodings 
for referencing the stack pointer. But that's them saying hey if everyone agrees to 
follow certain patterns when using this register, we can give you some useful instructions 
for doing that, it doesn't mean it's actually a special region of memory to the CPU. 

Allocating on the stack is not fast because of some physical property of that memory
region. It's fast because there's (approximately) no overhead in tracking when to release 
that memory (you just reset the stack pointer when the function returns) and because the 
top of it is used so much that it will probably always be in L1 cache or whatever. 

As a sanity check: 
- each thread has it's own callstack so clearly there isn't only one of them
- you can start (approximately) as many threads as you want so clearly 
callstacks are a resource that can be created without buying an extra CPU

## a libc

The C programming language's standard library is a mouthful so it's shortened to "libc". 
By historical accident, libc became a standard-ish way to communicate with your operating 
system. So much so that some systems (such as BSD descendants like macOS) provide a libc 
as the only stable abi (no stable syscall numbers, etc). This creates a strange situation 
where there is one name for something that contains core ideas like "mmap: perform the mmap syscall" 
and useless junk like "gets: read from stdin in a way that is actually impossible use safely" or
"strtok: try to split a null terminated string by sticking extra null bytes in there even though 
nobody wants to represent strings that way". 

On the other hand, linux has a stable syscall abi. Anyone can write a libc using those syscalls 
and it's just as likely to continue working as whatever libc you've chosen to believe is blessed. 
On windows, I think kernel32.dll is the one blessed interface (skill testing question: what's the 
32 mean? it sure isn't the number of bits in a pointer anymore), but the api it provides isn't libc. 
Microsoft doesn't give a shit about getting the POSIX stamp of approval. So people who wnat to run 
thier unix-y programs without porting them can use someone else's libc implemented by calling 
into kernel32.dll (i think that's what Cygwin is?). 

Thinking of your libc as part of your operating system is refering to it with more reverence 
than it deserves. It's just some code that someone else wrote that provides a specific api. 

## arm64 and amd64 

For some reason it's nontrivial to figure out the right names to call different CPU architectures. 
The more technically correct names might be aarch64 and x86_64, but it's hard to tell. 

To make myself feel better, here are some retoactive reasons I like the other names: 
- aarch64 is hard to say, you have to put like a vocal underscore between the two As?
- amd made a 64 bit version of x86 before intel (while intel was sitting in the corner 
eating paste with itanium) so it's not unreasonable to put their name on it. 

## The Optimisation Formerly Known as LTO

Some compilers split programs into very small compilation units and then link those parts 
together at the end to create your final executable. The thoery is that the only way 
to be fast is to have parallelism and the only way to have parallelism is to spawn a 
bunch of processes with seperate address spaces that do a bunch of redundant work. One 
disadvantage to this approach is that each compilation unit is optimised independently 
and only the dumb linker at the very end can see the whole program. So to claw back the 
ability to produce fast programs, you move the optimization smarts into the linker. So 
instead of outputting object files that have machine code inside, you stick some intermediate 
format (perhaps llvm-ir) in there instead that has more useful information and can be converted 
into machine code later by the newly intelligent linker. Now you've carefully designed your 
compliation model around the idea that your backend can be single threaded because you'll get 
speed by having lots of them and then made sure to only have one of them because that enables 
more optimisations. 
