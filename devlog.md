
## (Nov 29)

- adding a new field to FileVTable after the slice of ioctls didn't work because get_vtable
  was using the field's index instead of calling offset_of so relied on them all being a single pointer
- converting vzf wrapper to my language. 
  - `*** Terminating app due to uncaught exception 'NSInvalidArgumentException', reason: '+[VZFileHandleSerialPortAttachment initWithFileHandleForReading:fileHandleForWriting:]: unrecognized selector sent to class`  
    because all the init methods aren't static constructors. you have to alloc it first
  - now i get to `The process doesn’t have the “com.apple.security.virtualization” entitlement`, great
  - backend/macho/emit needs to support entitlements
  - main mistake was output_type.reserve wasn't giving zeroed memory because chained is in temp(), i was assuming its in page_alloc
  - but it only works if i reserve 10 slots, oh because you're supposed to index backwards and 
    the index i need is 5 so 10 made it the middle? thats why codesign -d prints the indices with a negative sign. 
  - i was hoping i could have a dylib with `com.apple.security.virtualization` and open it from an executable without it, but it seems that doesn't work. 
  - vzf.fr: make an nsapp to give the vm a view
- finally fixed the "invalid system call" when exiting examples/soft_draw.fr because i made the same mistake in vzf.fr but its obvious since i just wrote it

## (Nov 28)
  
- finished off the dirent stuff 
  - for fuse directories you have to give a different op for open/read/release which is a bit annoying
  - 10 stars for https://github.com/zargony/fuse-rs/blob/39fde4a5c47ce370d228ac190f950bd835db7f47/src/reply.rs#L572
    that would have taken me so long if i hadn't thought to cheat when i was getting garbage values. 
- writable MemFile, MemDir/(mkdir, rename, CREAT, TRUNC).
  that lets me turn on frc caching without a shared directory. 

## (Nov 27)

- usb
  - i am the stupid, need to enable the pci.header, now the qemu one gives sane numbers, vzf still says 32 bit tho. 
  - https://www.intel.com/content/dam/www/public/us/en/documents/technical-specifications/extensible-host-controler-interface-usb-xhci.pdf
    clearly i need to write my own pdf viewer that doesn't respect the fucking type in a password 
    to unlock copying the text from our dumpster fire garbage pdf, what the hell. 
    chrome silently doesn't copy and preview has a popup. like why bro. 
  - it seems to me that the end state is just a ring buffer queue thing 
    that they've somehow made super complicated. so maybe this is actually achivable, 
    like all the insane shit seems to be thier equivalent of my Virt.Queue 
    and once i can do that, talking to the actual devices might not suck?
  - 4.2,4.3 have a list, hey there's only 7 pages of stuff i have to do, how hard could it be 
  - i think the sane thing is to do it gradually so i don't lose my mind

vfs
- store file position on FileHandle directly instead of in the data
  and have the vtable expose pread/pwrite interface instead of read/write
- for now, store `.` and `..` explicitly in MemDir, feels a bit wasteful? but i guess you need `..` at least. 
- started working on opendir
- hack in fix_errno because i don't do errno in my libc yet
  so if opendir returns null, the SysResult.Err ends up as zero and treated as a success
- there's something about the dirent api that just turns my brain off, idk, it takes so long every time i touch it 

framebuffer
- i want to work backwards in the chain of doom forks. 
  the one before doomgeneric is maximevince/fbDOOM, which needs linux frame buffer interface at /dev/fb0. 
  might as well use that as an excuse to setup ioctls and then i can see if i can find any one else's 
  programs that draw on a framebuffer that might just automatically work. 
- spent far too long on new doom looking like choppy garbage. 
  probably was i was giving it a gettimeofday where the seubseconds were ns instead of us. 
  timespec and timeval are not the same!

## (Nov 26)

- sad waste of time becuase get_constant doesn't type check and just treats it as a func id
- bit of a slog to get all the bits of libc doom uses, mostly printf. 
  easier to just copy the framebuffer on a different thread so both can think they own the main loop. 
  it works tho!!!! great success. so nice of them to make it play a little video 
  even tho i can't do input yet so i can tell it can actually draw stuff. 
- os/build.fr takes 1350ms.
  compile the kernel/start and user/init in parallel and just concat together to make the final image. 
  (very glad i remembered symptoms of breaking adrp and getting a different place for the kernel struct every function). 
  only got it down to 1180ms, still that's 14%, not bad. 
  kernel is tiny, that part takes 150ms (wall time on its own thread).
  tho im comparing it to compiler/main.fr with -unsafe (which is 880ms),
  doing init.fr unsafe gets it to 986ms which is more respectable. 
- i fear im going to regret trying to use usb instead of virtio-input.
  - adding keyboard+mouse in vzf adds (4203 Apple Inc, 6662 xhci_hcd), 
    and `-device qemu-xhci` adds (6966 Red Hat Inc, 13 QEMU XHCI Host Controller) to pci. 
  - i can map the bar but all the numbers in there are 0xFFFFFFFF which doesn't make any sense,
    and on vzf it has the flag for 32 bit which doesn't make sense because the pci base address
    itself is more than 4gb so it wont fit. 
    made sure to align the offset to the size, didn't help. maybe it's not always BAR0?

## (Nov 25)

- pass argc,argv,envp correctly in elf_loader and use that to pass the device tree to user space. 
- use bootargs to run something on startup 
  so when im testing something specific i don't have to type the command every time. 
- catch panics and mostly recover to the shell
- more fuse. 
  - rename, awquard because there's two dirs so you have to make sure they're on the same file system
  - O_CREAT, seems its not magic in OPEN, i do a seperate CREAT call when the OPEN fails. 
  - mkdir i don't understand, its returning -74 but still making the directory
  - thats enough to enable frc caching. 
    works for sudoku but not kaleidoscope, hangs trying to read the file. 
    its not about a page being uncommitted because v_to_p does that. 
    works if you read it in small chunks, i guess theres a max read size (and 3449466 is too much)? 
    there's max_readahead, maybe thats what that means? 
- log all the functions i need for doom
- start splitting libc up by which header file has that function. 
  use those instead of well_known.fr for web demo. 

## (Nov 24)

- problem statement is that sometimes i do a syscall and it just doesn't 
  happen so it looks like the return value is the first argument. 
  - only on vzf. qemu-hvf, which you'd think would be the same, seems to work fine. 
  - manifests as: mmap returns null, munmap doesn't return 0, redirected openat returns 
    the directory as the file so you try to close it twice. 
  - with strace on it happens every time, but it was happening before at startup 
    and i just had to run it a few times to make it work
- tried TLBI_VALE1IS, just because that something i know im supposed to do, doesn't help. 
  made qemu-tcg slower so for now i'll do it less. 
  i assmue doing it at the beginning doesn't matter, 
  probably doesn't cache the emptiness of all the memory ive never used?
- its nothing about preemption. if i never init_timer, it still happens. 
- oooo but it doesn't happen if i don't enable the virtio-console interrupt. 
 - works if i set VIRTQ_AVAIL_F_NO_INTERRUPT in `q[1].driver.flags`,
  - so problem is when im printing something it sends an interrupt while im spinning 
    in StdoutFile.write calling poll. 
  - really i should be using the other interrupt instead of spinning 
    but the point is it should work either way. 
  - why does getting an interrupt there make the next syscall fail?
    all the kernel does in that interrut handler is poll the queue, 
    and it interrupts are masked while its doing that so the only possible things should be 
    intterupt while its in the syscall so when it returns it immediatly polls again even tho
    thats useless or intterrupt in the loop before the syscall so the syscall is useless 
    and either of those are fine. 
  - the thing i call strace is just in the userspace libc functions, 
    even the ones that are redirected syscalls just change elr so the work happens back in userspace. 

---

- given that's a dead end, maybe i'll feel better if i put a pretty picture on the screen
- first time i need to put pointers in the virtq, just hack myself a new syscall to do the address translation. 
- i had more trouble making a view work in my swift program than i had with the actual framebuffer part. 
- to start with, just make a thread that cycles colours. 
  its really funny that you can see it slow down when i run kaleidoscope at the same time. 
  im guessing its just a problem with my timeing or sleep or context switching, 
  because if you just print how much free time there is between frames its always like 15ms 
  so its clearly not actually as slow as it looks? 
- (trying to compile doom)
- side quest to rewrite follow_path so write works
- bamboozled myself with `a = &anims[wbs->epsd][i];` being the wrong type, but its 
  just because you can't just concatenate all the files, they rely on you using different
  compilation units so they can reuse names
- was missing a done.pop in fuse_blocking so it hung after 128 requests
- need to fix "need_reify on threaded codegen" to make it work jitted,
  but not-my-os aot import_c doom gets as far as "No IWAD file was found" error. 
- simplification: don't need to generate wrappers for syscalls on the kernel side
- added `spawn` to the shell so you can run the graphics test as nonblocking.
  two reasons its not just the jitted program spawning a thread:
  - need to set_signal_handler
    - tried doing it by exporting handle_signal in libc, but threaded programs need to just work
    - instead move it to the spawn syscall. refactor so its easy to setup a bunch of state 
      for your child. maybe thats a good place to put more fine-grained capabilities eventually? 
  - need to stay alive and not block the shell, 
    but when you return from main it drops the compiler which frees the jitted code.
    still don't have a good solution for that so it stays a shell thing for now. 
- did a userspace thing where you can override your signal handler to make test_faults work. 
  got more complicated than i was hoping for, but i have to do that eventually for posix sigaction so maybe its fine. 

## (Nov 23)

- convert the stdin signal hack to use the virtq directly as a UQueue
  - bamboozled myself a bit 
    - UQueue indices were wrong because the kernel init was still giving it a buffer to write to
    - was setting bytes_written to desc.len instead of used.len
- when using pl011 instead of virtio, im not sure which side i want to do the translation. 
  rn its easier to do it in userspace because that's where i have q.poll as a vtable, 
  so its easy to just not do the syscall, but that means i need to keep the old 
  side channels around. you can't have both (virtio as the blessed interface) and (all the 
  special driver code in user space) if i'm committed to not being able to map mmio memory 
  to userspace because you can't guarantee you have enough granularity to not have them overlap. 
- the way im doing poll feels very error prone, maybe it should move into 
  the qring.Tube itself so you don't always need to treat it as though it might not 
  be consumed by a different thread and awquardly check if things will block. 
  the current way falls apart if you try to use the concurrent and parallel interfaces at the same time. 
- do stdout as queue as well, that ones's more annoying because you still 
  want to be able to debug log in the kernel so they have to share a bit. 
  for sanity reasons, i probably have to keep the simplest possible write 
  as a synchronous syscall just in case i break everything, 
  so using that when it's not virtio isn't as much of a hardship. 
  i do think making more syscall numbers different from linux is an improvement
  so you can intercept them even if they try not to go through libc. 
- making the kernel side of virtio drivers more data driven since they're all the same. 
  - wasted some time on trying to make a constant containing an array and not getting relocations.
- return ctx from handle_signal as an extra check that its nested correctly 
- being a bit less wasteful
  - for collect_fs, since i'm not actually using the hash table, store the constant data as a flat array, (saves 72k)
  - don't leak the boxed FuseFile
  - reuse file descriptors
- more work on the endless quest for the lost syscalls 

## (Nov 22)

- sys: oopsie daisy, my join(Thread, -syscalls) was wrong
  and was just (a spin lock with a race condition) if the thread hadn't already exited. 

os
- replace check_thread with tid_futex like linux does
- figure out how to read a file over fuse
- instead of doing fstatat/openat in handle_signal, put the implementation in its own function 
  and return to it, so you're back in normal land before doing any work. 
- more object-ish file system
- more structured handling of variable width fields for fuse. 
  comptime thing that converts to slice fields on the In/Out structs. 
- making reading via fuse as a vfs object was easy. 
  write is harder. err=9. tried doing FLUSH+FSYNC+CLOSE+FORGET after, didn't help.
  was because i was putting the ACCMODE in OpenIn.open_flags instead of OpenIn.flags. 
- i think lseek doesn't do anything and im supposed to track the position myself? 
  because you pass offset to read/write. 
- for baked fs, use a tree of directory handles instead of a hash map of file name/contents strings.
- extract the path traversal from openat/fstatat, so fstatat calls openat then fstat, 
  and openat walks the tree of directories one path segment at a time. 

## (Nov 21)

os: virtfs
- moved some virtio code around to prep for shared file system
- i think this is where i start needing to set DESC_F_NEXT, 
  unlike printing its not just a flat stream of bytes. 
- important thing i got wrong: you only put the first descriptor of the chain in the ring. 
- confusion about what queue number is which. queue 2 has max_size=0. 
  docs say hi=0,no=1,re=2..., but thats only if you have the flag for notifications 
  (which apple's vzf doesn't support), without that you just skip one and requests are earlier. 
- Console.app:
```
Guest issued a read to Virtio BAR region with unexpected offset: 0x8028
Instance 0 needs reset for reason: Missing Fuse::OutHeader
```
  the former is it doesn't like me trying to read notify_buf_size when it doesn't support VIRTIO_FS_F_NOTIFICATION. 
- went on an adventure to use someone's example fuse thing with the linux kernel 
  (https://github.com/cberner/fuser big fan of thier program being something that actually compiles)
  so i can see what a valid init message looks like 
  (i failed at getting it from strace which is a bit concerning but i found where to log it in the program)
  using exactly those bytes in mine still gives the same error. 
- fuck me, 40 days and 40 nights spent on not passing the flags from push_virt to push_phys. 
  clearly zig is right and i need to deny unused variables. 
  it takes my init message now tho so thats nice. this should have been so easy. 

backend: experimenting with making the queue used for CodegenShared more generic. 
- old (waiting_for b/k):  867.7 ms ±   5.8 ms   (2379 lines of strace)
- old (waiting_for b/k without push_back): 875.6 ms ±   8.5 ms
- new qring (Pos = (first, count)):  880.9 ms ±   6.4 ms  (11892 lines of strace)
- new with waiting flag: 872.4 ms ±   3.9 ms  (2160 lines of strace)
- works on my machine but super doesn't in actions which is off-putting but not surprising. 
  the problem i feel in my heart is you increment before storing the value in the array 
  so the other guy could grab it before you're done. 
- qring (claimed & committed): 871.9 ms ±   4.9 ms  (3275 lines of strace)
- still no good
- few tweaks? 878.7 ms ±  11.9 ms
- that seems to work both places. run it again to make sure. 
  i do fear i may have just inlined my mutex and there's no point to this operation. 
  but i think i can get rid of the paired atomics at least. 

os
- queue glue
- mystery of why DeriveFmt wasn't working in the kernel, 
  it was panicking at runtime instead of comptime in the default branch. 
- skip `./` in paths so jitting the compiler gets past find_std_lib

compiler: seal_debug_info when cache disabled  

## (Nov 20)

- consolidate the boot scripts. just chech the uname and run the right binary. 
- dont depend on the unzip binary, just do the little bit of wrapper around deflate myself. 
  - main mistake i made was forgetting `#align(1)`
  - now switching to macos-15-intel in ci worked, so looks like i was right that it died because 
    it has the same broken version of unzip that tahoe does. 
    had to fix that before github takes the old one away. 

## (Nov 19)

- sys static: poll, execve, 
- give a clear error if you try to rely on slice abi in one of the syscall wrappers
- wait4 was giving ECHILD because i wasn't setting a exit_signal in clone_flags when forking. 
- fixed O_arm.DIRECTORY for static opendir, the curse of people using octal
- skip the tests that are specifically about importing something when not linking libc

## (Nov 18)

- i think i just confused myself yesterday, because i never got vzf to the point of actually working before
  since i couldn't do input. so the situation where qemu worked but vzf hangs is fine because 
  thats still progress. vzf seems helped by just bumping up the amount of ram a lot, which 
  i don't understand and is probably still a bug in my thing but for now its fine. 
  also very possible that my attempt to make qemu use stdio for a virtio console and not have a pl011 one 
  is using the wrong cli arguments because i really have no idea how they work, i just try random combinations. 
  also the order in which i init the different devices is now a terrible spaghetti. 
- taking out libc bindings that i don't use
  - demote query_cli_args from sys/process.fr to a test 
    because the one in dynamic env from the entry point is always what i want
  - demote uname/Utsname to examples/toy/where_am_i.fr
- transcribe syscalls: fork, pipe
- don't try to call clock_gettime without libc on macos. 
  not super useful but its a fun trick to have the compiler technically work

## (Nov 17)

- continuing to refactor the drivers. doesn't spark joy. 
  but at least now there's a sane way to choose which console is used for logging in the kernel. 
- reason why vzf wasn't getting past turning on the mmu was "stomp mapping",
  i wasn't zeroing the memory used for translation tables. 
  i assumed all the fresh pages in the vm would be zeroed because surely it can't leak whatever
  the host had on that page last. so im a bit afraid i might be writting over something that's used elsewhere?
  hopefully its just the device tree (which is still a problem but at least i know its a problem)
- virt console:
  - wasnt wrapping around the index stored in the ring
  - now it gets to load_elf_file but not init. 
  - problem is the Desc needs physical address not virtual address
  - now in vzf it gets to printing the shell prompt
  - need interrupts for when it gets input
    - as a start just enable every intid so i can hard code it and make it work 
      before needing to figure out how to get that info out of pci. seems to be 69 on vzf and 37 on qemu. 
    - important to set VIRTQ_DESC_F_WRITE or you just get spammed with interrupts that say they wrote zero bytes
    - i think im correctly reading the pci interrupt-map,
      but i somehow completely broke everything else.   
      identity_map-ing some of the pci memory seemed to help a bit. maybe its not always before the ram.   
      problems:  
      if you do uart qemu-hfv works,  
      if you don't it panics and fails to print the message,
      but then qemu-tcg works. 
      either way, vzf hangs trying to compile sudoku  
- lib/sys: transcribe a few more linux syscalls. 
  trying to get run_tests to work statically linked. 
  - remove calls unlinkat with flag if its a directory
  - readlink doesn't exist on arm, use readlinkat

# (Nov 16)

- (what i thought was an alignment problem yesterday): i am are have stupid. 
  i was adding the addend to src instead of dest in flat_link
- started factoring out the interrupt controler stuff

## (Nov 15)

- briefly thought LinuxBootHeader.image_size being 0 made a difference to where it places device tree
  so passed that via entry point instead of header but it seems to not really, i think i just misread a number. 
- stop hard coding the relationship 512MB block step between the kernel's executable memory 
  and where user_exec runs in userspace. so now i can deal with having more than 512mb of ram. 
  get ram size from device tree. 
- lots of wasted time being confused (as with every time i touch page tables). 
  - big one is that TranslationTable has to be aligned to its size, which is up to 64kb,
    so need to be a bit careful when using a smaller page size, it doesn't just happen automatically. 
  - there's something very weird going on thats sensitive to code layout. 
    where it crashes with `ELR_EL1: 0` before doing anything in user_exec (but after "kernel ready")
    - only broken with `-accel hvf` (just qemu's normal slow mode works fine)
    - sprinkling around #inline makes it work
    - doesn't matter if linux image or elf
    - doesn't matter if i give more stack space in build.fr/emit_entry
    - just calling `int_from_rawptr(main);` in main breaks it in the same way. 
      - only difference in code there is that it emits and calls ptr_diff__335. 
        (which should be inlined but the first call never is because of the order i emit functions is dumb). 
    - real entry point being less than 11 instructions also breaks it
    - it must be an alignment thing (because flat_link doesn't space things out)
      but i don't understand why because i shouldn't have anything that needs more than 16 byte alignment,
      other than the interrupt vector and page tables which get special treatment to make sure they're aligned right. 
      and elf/emit.fr does `m.align_to(s, 16)` now so it should be fine? 
      stack_end's align doesn't matter because entry fixes it. 
- cleanup: call localtime_r instead of localtime (for import_c's `__DATE__`)
- another round of using device tree instead of hard coded magic numbers. 
  kinda makes the code shittier, idk. maybe it will grow on me. 

---

taking a break. playing with someone else's thing that uses qbe (https://github.com/kengorab/abra-lang)
- too slow to be fun: 17233ms
- BufferedWriter: 11440ms
  - libsystem_kernel.dylib.write samples: 5166 -> 31
- no gc: 8437ms
  - just leak all the memory because compilers are short-lived
  - could make it a cli flag to call GC_malloc or malloc, if i was more committed to the idea
- whats left is 730 compiler, 2255 qbe, 5303 clang (thier arm assembler is really slow as ive learned).
- qbe_frontend.fr on thier ssa is 569ms but doesn't work
  - ret without argument in `$.10.Typechecker..._typecheckIndexingArraylike` 
    (in presumably unreachable block after you return from match stmt). 
    for now just allow that and replace it with hlt in my backend. 
  - `.lambda_192.discard` has an env parameter and calls $.lambda_191 which also has an env parameter 
    but it doesn't pass it explicitly, relies on it just happening to still be in the register.
    but when i inline a call without an env argument i set it to zero. 
    just threaded env through in the discard case of compileFunctionValue. 
  - they also have a lot of `(env %__env__...) {` where its missing a comma so its just 
    part of the name instead of being variadic (same on real qbe, dot is a legal character in identifiers), 
    but those functions don't need to be variadic, so i can't really tell what the goal is. 
- using my backend: 1345ms
- oh they also have a StringBuilder which achieves the same thing as my BufferedWriter,
  they just only use it for the js backend. i forget we're in gc land where its not 
  socially acceptable to mutate a string so you're allowed to just hold on to an array of them.
  that's kinda convient. tho it segfaults so maybe not. 
- one more broken test. compiler/types.abra: `$.f4.discard(...): Wanted alias for RTmp:69 x`,
  its called with too few arguments and then gets inlined so we notice. i don't give a very good error message for that. 
  in debug mode its slightly better but not much: `ssa temporary %x.69 is used undefined in @start of $.f4.discard (no def)`
  tho their ir_compiler doesn't have that problem,  
  but can't compile itself so ive caught them in the middle of a rewrite i suppose, 
  maybe i'll just wait for that instead of spending time fixing thier old code that seems to be on the way out anyway. 

## (Nov 14)

- making page size not a constant so i can do 16kb pages
  - mistakes:
    - not shifting by the right amount in get_pointer
    - too high nbit
    - not changing tg0
    - not committing all the pages for the signal stack
  - now it works with `-accel hvf` which speeds up boot-sudoku-exit 862ms -> 171ms (5x),
    which is still a lot slower than native 73ms but a big improvement. 
    hopefully a lot of whats left is just the time to spin up a new universe. 
    yeah, for kaleidoscope it's 16666ms -> 1382ms (12x), and native is 627ms. 
    oh and im only using one core compared to 2 threads when native so it makes sense that its slower. 
- interesting source of confusion, when loading as a linux kernel image, 
  it doesn't necessarily put the device tree before the kernel so my current system will overflow into it. 

## (Nov 13)

- for sanity purposes, make sure each section is at least aligned to 16 even when .Relocatable because flat_link doesn't respect the section.align
- made queue struct not require constant queue size for arrays
- clean up my virtio code so its at least a somewhat useable abstraction
- it seems i don't need a notify after setting driver_ok but before adding to the queues, 
  the important thing is that i READ queue_notify_off again (even tho it doesn't change?). 
  if i don't do that, it just discards the first thing i put in the queue?? 
  oh maybe its just waiting for me to read anything at all because im supposed to read back the 
  status again to wait for it to agree we're ready. 
- the thing that causes `Assertion failed: (isv), function hvf_vcpu_exec, file hvf.c, line 2030.`
  with `-accel hvf` is `sys_set(.CNTP_TVAL_EL0`,
  in qemu's source: `case EC_DATAABORT: { bool isv = syndrome & ARM_EL_ISV;  [...]; assert(isv);`
  with `-d trace:hvf_unhandled_sysreg_write` it tells me 
  `hvf_unhandled_sysreg_write unhandled sysreg write at pc=0x402040cc: 0x0030f804 (op0=3 op1=3 crn=14 crm=2 op2=0)`
  idk what 0x0030f804 is, `msr	CNTP_TVAL_EL0, x0` should be 0xd51be200, but the part in brackets is right so fair enough
  but like it seems unreasonable that they wouldn't let me have a timer. 
  `hvf_sysreg_write: Guests should not rely on the physical counter, but macOS emits disable writes to it. Let it do so, but ignore the requests.`
  ok sure, just use CNTV instead of CNTP and let the hypervisor lie about the number or whatever, it seems fine. 
  could have saved a lot of fucking around with avf if i'd just investigated this in the beginning. 

## (Nov 12)

- in the device tree, pci.reg tells me where the configuration space is, 
  and then i can iterate through that and find the active ones with vendorid != 0xFFFF,
  and the deviceid is offset 0x1000 from the ones in the virtio spec? 
  looks promising in qemu, i find exactly one with id=3 when i ask for `virtio-serial-pci` 
  and zero with `virtio-serial-device`. still doesn't look like that works in avf,
  tho it does have the right vendorid for virtio, just not deviceid=3 for console. it has 66. 
  really hard to do this with one bit communication channel of did it hang or not. 
  oh wait thats for "Transitional PCI Device ID", if its not transitional (whatever that means),
  offset is 0x1040 and then apple does have deviceid=3. 
- not clear to me if cap_ptr and cap_next are supposed to be 8 bits or 16 bits. 
  maybe one is pci and one is pcie? am i supposed to manually add 0x40 to skip the header?
  maybe i thought it was hitting dohang but really the loop wasnt terminating
  i think osdev is lying to me about "The bottom two bits are reserved and should be masked before the Pointer is used to access the Configuration Space."
  because without that, it gets VIRTIO_PCI_CAP_DEVICE_CFG in qemu which it should.
  ok we've learned i get to pick an address and write it in the bar to choose where i want the mmio to work
  does it have to be one of the addresses in pci.ranges? seems like yes. 
  once again, works in qemu (after much time wasted on double adding the offset). hangs avf. 
- ayyyy at least ive achived another information channel, when i fail at it, 
  Console.app has `Not mapping BAR because address 0xffffffffdabc0000 is not valid.` 
  so i can give myself 64 bits at a time instead of 1. 
  its just a bit lossy because you don't get the message if you try to write a valid address, 
  so just set a high bit and ignore that one i guess. 
- looks like setting bar isn't the problem. the one from ranges i try to use is 0x0000000050000000 and it doesn't think thats invalid. 
  but its promising that i was looking for something called BAR and i found something called BAR. 
  maybe they just don't implement emerg_wr for some reason (and express their displeasure at me trying to use it by hanging). 
  im pretty sure interrupts start off masked (thats what the linux boot thingy says you have to do), so im not just dying over and over in the missing interrupt handler. 
- tho, a slight detour that concerns me: `orb dtc -fI fs /sys/firmware/devicetree/base`
  doesn't have a pcie node, and does have a bunch of virtio_mmio, 
  and it claims to use Virtualization.framework which makes sense to me because its not slow as fuck, 
  which implies i can ask for the type of io thats not super complicated. 
  i choose to believe its just lying and remapping them secretly. 
- if the decission is apple just doesn't have emerg_wr, then i just have to do the virt queue thing. 
  got hello world to work on qemu eventually... would you believe... doesn't work on avf. 
- yayyyy, the last thing missing was notify after changing config but before putting a thing in the queue. 

## (Nov 11)

- it seems my woes with virtualization.framework were just that i downloaded the 
  wrong files to use as a liunx kernel from clicking around in the link from their example. 
  https://developer.apple.com/documentation/virtualization/running-linux-in-a-virtual-machine
  it worked with whatever puipui-linux is, from https://github.com/Code-Hex/vz `make download_kernel`.
  so if i make it think my thing is a linux kernel, maybe it will Just Work (tm).
- https://www.kernel.org/doc/Documentation/arm64/booting.txt
  this implies that i just give it a flat file and it jumps to the beginning of it 
  with the device tree address in x0. which is almost too good to be true. 
  that seems to work, if i put the magic header with the first u32 being Arm'b(0, 0)
  it hangs and if the first u32 is 0 it crashes with `VZErrorDomain Code=1 "The virtual machine failed to start."`
  so that's promising. jumping to my entry point makes it run some code in qemu before crashing. 
  in apple's virt, it doesn't print anything because my hardcoded uart addresses don't work,
  but i can choose between shutdown, crash, hang, so its definitely running it. 
- get rid of the relocations for the syscall dispatch array
- with a bunch of random qemu flags i can ask for a new serial port that goes to a file 
  and print to it with the virtio console's emerg_wr but it doesn't work in 
  avf, perhaps because my turning on the mmu doesn't work,
  if i skip turning on the mmu it doesn't work in qemu either. 
  can fix alignment and then it works on qemu again but still not apple. 
- maybe they just call it vertio and are lying?
- ohh maybe the you find pci devices is different than mmio devices and its one of the former?
  virtio-serial-device vs virtio-serial-pci. 
  oh joy the pci spec is paywalled. maybe this guy will tell me the secrets:
  - https://michael2012z.medium.com/understanding-pci-node-in-fdt-769a894a13cc
  - https://wiki.osdev.org/PCI
  - https://elinux.org/Device_Tree_Usage#PCI_Address_Translation

## (Nov 10)

- fixed a nasty control flow bug in emit_ir
  problem was if there was unreachable code after an infinite loop without a break or continue, 
  it would be emitted to the end of the loop body instead of being discarded. 
  but that doesn't come up much because a) why loop forever and b) sema tries to discard 
  obvious unreachable code so it can only happen when its obscured by a capturing call. 
- set EL0PCTEN so i can move clock_gettime to user space
- test that makes sure some instructions, that user space shouldn't have, are correctly trapped
- sig_action return more info about the old one so you can restore it or chain them
- make interactive_read_line use blocking read instead of sleeping a random amount
  - so it doesn't suck cpu with my dumb busy wait sleeping
  - updated the examples that use start_raw to restore terminos more exactly at the end
- ive gotten it to a state where ctrl+c just makes it hang instead of exiting. 
  ah right, problem was trying to treat the signal handler as a seperate thread_index 
  and then deadlocking on the file system mutex.
- make syscalls default to returning to the same task instead of yielding the rest of the time slice. 
  - fixes the thing where pressing ctrl+c in chess/perft.fr would have to keep letting all the other 
    threads get more time before the init task got to exiting. 
    (even before i don't understand why that wouldn't be super fast, like `3*4*5ms`, each time slice seems way longer than i'd expect)
  - somehow that makes it use more memory and even if i bump it up, kalidescope never works.
    works if i reduce codegen_queue_size.
- set UCI=1 and UCT=1 so don't need to fake as much for aarch64_clear_instruction_cache
- shitty strace thing just so you can kinda see whats going on 

## (Nov 9)

- put a lock around the fake file system. 
  i only have one core rn but threads can preempt so it still matters.
- scheduler knows about futexes so it can skip threads that are waiting and save two context switches
- reset the time slice when a new thing gets scheduled 
- the curse of randomly losing syscalls is helped by making the time slice longer,
  so something's wrong with my preemption. that's concerning. 
- the `&& !(@import_symbol "exit").is_null()` in is_linking_libc was being confusing. 
  it was that import of exit that wasn't getting filled when trying to jit the kalidescope example. 
  added a todo test. 
- no need for all the hassle with add_my_imports_to_injected, 
  just pretend to have dlopen like im pretending to have the rest of the imports. silly. 
- vtables for files so i can have stdio be normal file descriptors. 
  now need to signal userspace for the uart interrupt so i can get something to read from my fancy new vtables.
  rework the calling convention for signals to get rid of the window where getting a nested signal
  is a problem while you're copying the mcontext. now have a stack, and use that as the callstack as well
  so two birds with one stone, no longer dies if you interrupt in the prelude of a function 
  (tho thats still a problem for the interrupt handler itself). 
- use my new reading powers to make a little shell that can jit programs

## (Nov 8)

enough of a shim of a file system to run the compiler on my os. 
pretending im linking libc and then just overwriting the relocations 
at runtime with my temporary implementations mostly works.
the annoying ones are openat and fstatat where using the libc ones sucks 
so i just do the syscalls directly. so did a very basic signaling thing  
so when the kernel gets an unknown syscall it just gets sent to userspace. 
to get the child program to work, fill todo_injected by reading the elf. 
without that it hits an unfilled import (which it shouldn't, should just be null, 
so thats kinda a compiler bug, it just only comes up when you do !prefer_syscalls 
while not linking a real libc so its a bit niche). also add_my_imports_to_injected 
doesn't seem to work on real linux so im wrong about which pointers the loader 
fixes to the new base address? so should fix that out of curiosity but this 
whole thing is temporary anyway so im going to wait until its more established. 

- enough of pthread for CodegenShared
- save floats in mcontext
- munmap
- in setup_virtual_memory, unmap the low addresses so null dereferences fault correctly

## (Nov 6)

- went down a bit of a dead end with trying to redo relocations in a normal program
  so i could have something that wrapped my compiler for my os and intercepted the libc things 
  as a temporary way of running it. 
  - trying to do it that way gets annoying because if you ask for 
    `-syscalls` then you just get symbols for the empty shims not the GOT slots but if you ask for libc
    (which i don't have) then the start up code dies before you get to the main program. 
  - ive also revealed a different problem, that my elf_loader gives non-zero base address
    and the way i do relocations for `-syscalls` doesn't handle that correctly.
    yeah, the xxx-in-data things in reloc.fr are 0. 
    - previously elf_loader did the right thing with `fix` so it worked if the exe asks for an interp. 
    - ive been down this road before when i was thinking about which elf type to use Executable vs Dynamic.
  - made do_relocations_static_linux work with nonzero base address,
    so now i can set elfheader.type=dynamic so the loader knows im position independant and can give me aslr. 
  - updated the sys.fr test because dump_elf having the string Dynamic no longer means its actually dynamic, 
    also run reloc.fr in elf_loader there so it can't regress. still works in blink. 
  
## (Nov 5) os

- factor out the syscall handlers
- have an array of tasks and cycle to running the next one after every context switch.
  bump up the timer frequency so its preemptive. 
  make a syscall for calling the elf loader in a new task so its like fork+exec (rn relies on there being only one address space). 
- don't allocate physical pages up front. mmap just reserves a virtual range 
  and then it gets mapped to something real the first time you fault on it.
- getting it to run kaleidoscope. 
  magic register to enable float instructions. 
  hacky thing to ignore the system instructions in aarch64_clear_instruction_cache, there's probably a way to just enable them for el0.
  fascinating, it mostly works but the output is slightly wrong. 
  (tho in a different way to how the wasm one is wrong on safari, extra fascinating)

## (Nov 4) os

i want to get an interrupt when theres input on the uart so i don't have to poll it. 
- setting the low bit of UARTCR, now `-d guest_errors` doesn't say "PL011 data written to disabled UART",
  which is encouraging because it means im looking at the right pdf. 
- eventually make someone else's example work so at least i know its possible.
```
https://github.com/google/osdemo
rustup target add aarch64-unknown-none
cargo install cargo-binutils
rustup component add llvm-tools
```
  which is not super helpful because its spread across a billion crates so you can't tell what's going on. 
- tried changing to gicv3 and using more of the system registers
  but that fucked everything and now i don't get the timer irq anymore. 
  - an example that seems to actually be by arm: 
    https://github.com/calinyara/AArch64_GIC_v3_v4_example/blob/main/src/main_basic.c
    (which of course you can't compile without getting a license for thier personal copy of clang)
  - oh suddenly works now? idk which part of this i wasn't doing for the last however many hours but 
    it seems the only important things (for gicV3 timer) are:
    - set bit 0b10 of gicd.CTLR
    - set ICC_PMR_EL1
    - set the bit for the specific irq in IGROUPR0 and ISENABLER0 of rd_sgi=gicr+0x10000
    - set ICC_IGRPEN1_EL1
  - so now im back to the point i was at this morning but with v3 instead of v2. useless. 
- if i read the current interrupts from UARTMIS manually, the bit for recieve is set 
  at the same time that the bit 33 of PENDR in gicd is set, so thats a good sign at least.
  but its not set in ENABLER? am i stupid, does set_field not work?
  yeah ok thats totally my bad im just an idiot. fuck. 
  i'd fixed a missing `*4` earlier but also was doing mod instead of div. 
  so that should have taking zero time instead of all day and switching to v3 was pointless, but here we are. 
  aaaaaaaaaaaaaaa. it works tho.
- now the thrilling task of sorting through all the random things i tried and getting rid of the ones that aren't required
 
## (Nov 3) os

- provide a thing like mmap so you can ask for new memory. 
  unlike before, don't just do identity mapping, don't care where the physical pages come from
  - mistakes with levels of indirction that get confusing because rn the behaviour when you try to write 
    to a null pointer is just that it doesn't nothing and always reads 0.
    so i had `root: *TT; TT :: (entries: *Array);` and was setting like `root.entries = bit_cast()`,
    so auto-dereferencing root which was zeroed because not initted yet and then endless confusion. 
  - when you have enough levels of page table to get to individual pages, 
    the last one's tag is 0b11 (like table entry) not 0b01 (like block entry that you use if stopping at fewer levels)
- stop trying to use the same stack for el0+el1. 
  it's the right thing to do anyway and it avoids accidentally stomping the user code's stack 
  if they weren't using sp strictly by the calling convention. 
- seperate the part where you eret out from the interrupt handler 
  and store the mcontext not on the stack so its easy to return to a different task instead. 
- reset the stack before syscall_func
- continuing the saga of how do i exit qemu from the guest. 
  aledgedly its just ask the cpu to power down, so how do i do that?? 
  `Set the CPUPWRCTLR.CORE_PWRDN_EN bit to 1 [...; isb; wfi;]` says arms website 
  but thats an illegal instruction and objdump doesn't even know about it.
  maybe shutdown sequence isn't part of the isa somehow? like is it different for every cpu
  and i need to ask about cortex-a57 specifically?
  the magic letters to google are PSCI and theres a hyperviser call that does it. 
- ifdef out most of the spammy logging, deduplicate code in kscary_log, userspace code can call print now. 
- trying to run elf_loader
  - "data abort" trying to copy the last segment into place.
    mmap wasn't aligning the size forward so was rounding down.
  - now it gets to "calling entry point" and dies on cas as an illegal instruction. 
    fair enough, ID_AA64ISAR0_EL1's Atomic field is all zeros for cortex-a57, but 0b0010 for `-cpu max`,
  - but that breaks everything else. it dies trying to write to the device memory in the interrupt handler. 
    `Taking exception 4 [Data Abort] on CPU 0...from EL1 to EL1...with SPSR 0x804023c5` PAN is set.
    fixed by setting `sctlr.span=1` (which confusingly disables it).
  - yay! elf_loader can load a real hello world elf program on my os now.

## (Nov 2) os

- the amount im using the linker for is so little that i can just do it myself until im ready 
  to think about how to make my elf emit code more general. 
- conveniently the fixed base address means i can do DataAbsolute fixups in the linker. 
- now need to be more careful about aligning interrupt_handlers.
  i guess the linker was inserting some bytes to fulfill the section's addr_align so it always started 
  4096 aligned, but im trying to do it in place so each section has to just continue 
  from the file offset of the last (and elf/emit doesn't align when type=Relocatable). 
  now it doesn't do anything at all (not even the error message for unaligned global). 
  ahhh, same applies to stack_top being 16 aligned, its fragile when you add new instructions,
  so align it in emit_entry and now it works. no more linker script. 
- emit_entry don't assume registers are zeroed
- very beginnings of handing out pages in a structured way
- discovered you're not allowed unaligned access at all while the mmu is disabled. 
  so with flat_link, its not just the special hardware statics that need larger alignment
  to be careful of, its any statics at all that contain anything bigger than a u8. 
- somehow i don't understand where the stack frame ends and trying to do more syscalls clobbers a return address. 
  i can make it work by just skipping some but i don't trust that im not just getting lucky. 

## (Nov 1) os

still trying to turn on virtual memory. 
- https://krinkinmu.github.io/2024/01/14/aarch64-virtual-memory.html
- i tried to run a few other people's examples but like you can't just compile a program,
  everything sucks and wants a specific version of rust that doesn't exist anymore or a specific 
  name for your gcc cross compiler that i don't care enough to install also because why am i cross 
  compiling from arm to arm. so once again reminded that i need to not depend on a linker for this
  so i can have a program that actually works (and is usable by people like me who have zero patience for toolchain hell). 
- one thing i was doing wrong was not aligning the translation table enough but that doesn't fix it. 
- translation table entry attr:
  "If you set `Bits[7:6]` to 0b01, which means the user can read/write the region, then the kernel is automatically not executable in that region no matter what the value of `Bits[53]` is."
  helpfully thats not fucking written down anywhere other than https://grasslab.github.io/NYCU_Operating_System_Capstone/labs/lab8.html
  that seems true, if i just use 0b00 then the el1 part works fine, and if i use 0b01 it dies and the fault address is the return instruction immediatly after setting SCTLR_EL1. 
  but i don't understand why. 
  - Armv8-A Address Translation talks about SCTLR_EL1.WXN which does that but i have it set to 0, 
    so like i guess theres another place the enforces the same thing?
  - PAN sounds vaugly similar but cortex-a57 doesn't have it. 
- ok whatever, it works if i map the same physical memory once (identity) as RW el1 only 
  and have el1 execute in that code, and then the next block of vm points to the same place, 
  RW for both el1 and el0, which el0 can execute but el1 cant (for reasons i don't understand but thats fine). 
- now i seem to have made WFI be an Undefined Instruction. 
  thats easy ive seen the bit in SCTLR, set nTWI=1. 

---

elf_loader: wasn't working on arm. 
if x0 looks like a pointer (which it will because its used to set sp),
i try to look for DRIVER_VTABLE_MAGIC 112 bytes after it, but since 
stack grows down, reading up from a newly mmapped stack reasonably could be unmapped. 
that was a dumb hacky thing anyway, just get rid of it. 
  - the better thing to replace it with might be backend/incremental.fr/Meta having two entry_syms, 
    one for the user's hosted main and one for the runtimeinit main so the caller could choose which to use.

## (Oct 30)

- look at the device tree:
```
qemu-system-aarch64 -machine virt,dumpdtb=a.txt
sudo apt-get install device-tree-compiler
fdtdump a.txt > b.txt
```
- dump_devicetree to experiment with how the bits are arranged.  

## (Oct 29) os

- save the registers in the interrupt so the stack gets restored to the right place, 
  and unexpected interrupts won't eat your data. 
- made msr/mrs a bit more readable
- https://lowenware.com/blog/aarch64-gic-and-timer-interrupt/
- achived timer by transcribing random magic numbers from someone's blog post, 
  pleasing that it works but i hope i get to the point that i can actually do my own stuff soon. 
  the beginning where you just have to transcribe the same thing as everyone else's fake os is kinda boring. 
- asking qemu for `,gic-version=3` lets me read ICC_IAR1_EL1 which maybe has the INTID 
  that tells me which interrupt happened but makes it spam forever so maybe that kills the memory mapped part 
  and i need to do everything with the system registers instead? 
  i can read GICC_IAR memory and get the right intid, but now the timer doesn't fire again. 
  ah, next row of the table (12.12 The GIC CPU interface register map), write to GICC_EOIR turns it back on again. 
- thats good progress, kinda all you need for preemption, if i had anything to preempt. 

## (Oct 28)

os
- build script with very convoluted way of doing relocations in an AsmFunction 
  so i don't need to use a real assembler just to make an extry point 
  that sets the stack pointer. recurring pain point. 
- playing with macro that injects a scope so you can have AsmFunction without #use the encoding file every time. 
- https://krinkinmu.github.io/2021/01/10/aarch64-interrupt-handling.html
- learned it puts me in el1, seems i can't read SPSR_EL1 from el1, can't control your own interrupt mask. 
  (update: must have tried to read SPSR_EL2, names are confusing. i think XXX_EL1 means for interrupts TO el1, so from userspace to kernel)
  set the special lr and eret puts me in el0 at that address, 
  then i can syscall and puts me at the "interrupt vector" 
  (whose address is just set in a special register) at el1 again. 
  registers are shared when doing that so i can pass values with the normal calling convention,
  so thats syscalls basically working. 

terminal
- when i got rid of `state := state[];` it broke the repl because i was using `raw_from_ptr(@type state,`. 
  switched to naming it directly so that can't happen again 

## (Oct 27)

- import_wasm stubs for webgpu functions so it can run examples/web/demo.fr on other programs again. 
- https://wiki.osdev.org/QEMU_AArch64_Virt_Bare_Bones
- giving a more serious attempt to having a thing run in qemu without an operating system
- failed at trying to make qemu exit at the end instead of just being there forever. 
  at least `wfi` makes it hang at 0% cpu instead of 100% cpu (which `b .` does), thats an improvement. 

## (Oct 25)

- there's a few places in sema where im cloning a Values 
  as though i have to avoid alising the bytes that a Big points to,
  but think i never mutate a Values directly like that, 
  i just replace the expr.Value.bytes field entirely with a new one. 
  clone is just a shallow copy of those bytes anyway so it can't do anything meaningful for pointers. 
  the places that are sketchy is 
  - call_dynamic_values(args_value). passes a pointer to those bytes to the trampoline from create_dyncall_shim
    but that's fine because argc does a copy. 
    - tho ASSUME_NO_ALIAS_ARGS is wrong then so extra reason to get rid of it. made a test that only fails with @run to proves it.
  - bake_relocatable_value, but again im not cloning in that so it would only work by luck if the other 
    code path caused it to always get fresh values. and it recurses through pointers since thats 
    its whole purpose and those wouldn't be cloned and couldn't be because you're allowed to 
    point at memory that isn't mananged by the compiler. really the bake operation is the clone. 
- woah, turning off ASSUME_NO_ALIAS_ARGS makes it like 15ms faster. that's strange. i guess it helps elide_abi_slots? 
- shrink OverloadAttempt. size_of(Task) 184 -> 152.
- (874.8 ms ±   6.6 ms, lit_fn = 2754, ir_ops = 475912, macro_calls = 33188, spec_const_fn = 15343, dyn_call = 40485, 888800 bytes)
- just make the struct in @run instead of using the syntax
- from_raw(Enum) is only used in one place. inline it.

## (Oct 24)

- micro comptime optimisations 
  - (969.5 ms ±   6.8 ms, lit_fn = 3075, ir_ops = 487672, macro_calls = 35816, spec_const_fn = 16104, dyn_call = 46818)
  - Fields -> make_enum_for_fields_uncached
  - enum_basic.RAW -> get_enum_raw_type
  - zeroed 
    - offensive because it gets compiled for each slice type calling empty(). 
      measuring the time in tracy for CompileBody:empty of each empty() default field value of EmitMacho struct
    - before. 226us
    - first outline `u8.ptr_from_raw(p).slice(T.size_of()).set_bytes(0);` 82us
    - then make it a macro that just makes a Values of all zeros. 8us
  - (910.2 ms ±  16.8 ms, lit_fn = 2754, ir_ops = 478884, macro_calls = 33227, spec_const_fn = 15344, dyn_call = 40458)
    - it also disproportionately helped wasm. self: 5257ms -> 3887ms, mandelbrot_ui: 833ms -> 636ms. 
      which i guess is consistant with the previous observation that jit_instantiate_module is the problem. 
    - introduced a weird InProgressMacro problem in import_wuffs,
      but the bug existed either way and its so much faster that ill just pretend its fine for now. 
  - don't always re-intern expr_pair_type

## (Oct 23)

- use an offscreen canvas to run the rendering in a worker 
  so it's convenient to use with the existing jit demo, 
  and make the event handlers more indirect. 
- for now, demo can have some more hacky glue to add the webgpu stuff to todo_injected
- automatically replace the output text box with a canvas when the program imports the graphics stuff. 
  rn doing it hackily since they all need to include_std(graphics/lib) at top level so it doesn't even need the compiler to tell you. 
- don't need the hello.html example anymore
- terminal: 
  - basic searching. no ui yet, just printing the results. 
    but running it on another thread so it can't hang on spinning wheel like warp.  
  - save/load whole output history to a file so its easier to build up a large buffer so i can make sure the searching isn't slow

## (Oct 22)

- a few more tedious bindings. 
  confused myself with R.array wanting a pointer to a pointer. 
  but now hello_triangle works in the browser. 
- mandelbrot_ui didn't work because minUniformBufferOffsetAlignment=0 in apply_uniforms. 
  need to do getLimits.
- my attempt to make it take up the whole window and be less blurry instead made it much much more blurry. 
  setviewport makes it less blurry but not fit in the screen. 
- it seems that canvas.width controls the size you get back from getCurrentTexture, 
  and you have to manually scale clientWidth by devicePixelRatio to get something sane
- did reference counting so `G.objects` doesn't grow forever
- bit of event progress: scroll, some modifiers,
- mandelbrot_ui: clamp scale so you can overshoot by scrolling really fast

## (Oct 21)

- now i need to read the structs out of linear memory and make js objects i can pass to webgpu. 
- enum names are inconsistant and painful
- the unused layouts in BindGroupLayoutEntry aren't optional pointers in the c api, 
  instead there's a BindingNotUsed value for the type field and if its that, 
  the whole thing has to be passed as undefined in the js api. 
  :JsReprLiftUndefined
- create(Buffer) mapped_at_creation :WebBufferInitHack
  - the arrayview i get from getmappedrange isn't going to be in linear memory
    so am i supposed to allocate a fake one an then copy it when you unmap? that seems like a pain.
  - can i use writeBuffer instead or does that need the data to stay live until the queue is submitted?
    no, im pretty sure it does a staging buffer for me. but then it complains that i can't use it while its mapped. 
    but its just initializing i don't need to ask for mappedatcreation because its going to be unmapped immediately anyway. 
  - that seems to work, but means i have to make it CopyDst even if that's not what you asked for, does it ever matter?
  
## (Oct 20)

- got to the point of the wasm calling webgpu wrappers so at least i can tell which ones i actually need. 
  kinda fudged the setup stuff for now, can come back later. 

## (Oct 19)

- generate code from dawn.json
- so fucking annoying that you can't use webgpu without https. 
  like `python3 -m http.server` is very convenient but then `navigator.gpu` is undefined 
  because drawing a pretty picture is considered unsafe. 
  ugh. 

## (Oct 18)

- (wasm) fix exporting a CodegenTask.Bounce
- (json.fr) dumb eating an extra character after a string so it didn't work if theres no white space before the end token. 
  added a test. 
- parse_json: structs use default field value when not provided
- got dawn.json into structs

## (Oct 17)

- failed at making prospero optimise_intervals know that mul with same arguments can't output a negative number.   
  thats clearly true so idk what my problem is :( very frustrating.   
  like maybe it's just revealing a broken thing about one of my other optimisations?   
  works if i take out the `@if(b.max < a.min) => only(1);` case for min.  
  804 -> 384.  
- use a bit set instead of `s.ints.interpret_as_bytes().set_bytes(255);` -> 310
  ditto for s.copy -> 270. 
- idiot, for one thing in only() i was doing 
  ```
  i[] = make_ins(.copy, .Kd, i.to, i.arg&[n], QbeNull);
  copy&[i.to.val()] = i.arg&[n];  // wrong if n=1 ^^^
  ```
  fixed that but still doesn't work if you set i there, but does if you don't, so then i can turn back on the min opt.
  -> 245.

## (Oct 16)

- timing `examples/toy/shasum.fr wuffs-main.tar.gz`  
  using variables instead of array for v and inline_range the loop over v:  
  588 bytes -> 8040 bytes, 90 samples -> 77 samples  
  move the w loop into the inlined v loop:  
  -> 13240 bytes, 60 samples  
  but takes 10ms longer to compile.   
  which actually becomes worth it pretty quickly.   
  `FRANCA_NO_CACHE=1 franca examples/toy/shasum.fr`: 148.7 ms ±   1.1 ms -> 135.8 ms ±   1.5 ms  
  `franca examples/toy/shasum.fr`:  93.8 ms ±   0.4 ms -> 66.0 ms ±   0.6 ms  
  which is a pleasing result because (if i aot compile mine) im the same speed as whatever `shasum` apple gives me.
  `shasum -a 256`: 59.9 ms ±   0.3 ms
  `./a.out`: 60.9 ms ±   0.3 ms
- deflate: 
  - simplifying yield_byte to just poke values into the array: decompress() samples, 283 -> 252.
  - brief attempt at sorting the low length (high probability ones) together in tree.node, 
    didn't make read_symbol faster (not including the time to sort). thought maybe it would be cool similar to examples/toy/predict.fr but no. 
  - tried reserving bits into an i64 on the struct so get_bits can be simplier. 260. (worse)
  - tried always reserving 32 bits so you can just read a u32 and not fiddle with masking it. 240. (better)
  - tried looping to always add 1 byte at a time so no unaligned read (like examples/geo/laz) does. 260.
  - meh, the middle one that was better still makes the code look more confusing, doesn't feel worth it for now.

## (Oct 15)

- the wuffs people have an example that goes bit by bit which is very very helpful.
  https://github.com/google/wuffs/blob/1e2e58cea012ea4c7553f327b63fafe196b0f9e2/std/deflate/README.md
- i can do test/data/romeo.txt.gz (the one from thier example). 
- on wuffs-main.tar.gz i get stuck after 18 bytes of output. 
- there's also https://github.com/madler/infgen which now that i understand what it means is also great. 
  im missing some of the litlen entries. 
  read_code_lengths for 16 was copying i instead of i-1. 
- now i can do wuffs/test/data/archive.tar.gz and i get through 22414 output bytes of wuffs-main.tar.gz.
  oh i just had to bump up MAX_BITS again, i don't understand why it isn't 9, todo i guess. 
  but it works now. takes a solid 15 seconds on wuffs-main.tar.gz (compared to 150ms by stb). 
- now that im sure im interpreting the bits in the right order, 
  traverse the tree correctly instead of scanning the whole thing. 
  get to use my favourite strategy to make sure i'm doing it right: 
  ```
  bit_index_start := c.i;
  new := c.read_symbol_fast(tree);
  bit_index_end := c.i;
  c.i = bit_index_start;
  old := read_symbol_slow(c, tree);
  @assert(new == old && c.i == bit_index_end);
  new
  ```
- that took samples in decompress (-unsafe) from 8564 to 227. 
  for scale, stbi__parse_zlib is 99 samples. 
- this is something where i can measure division being slower. 
  ```
  byte_i, bit_i := c.i.div_mod(8);  // 378.5 ms ±   1.7 ms
  byte_i, bit_i := c.i.udiv_mod(8);  //  369.1 ms ±   1.7 ms
  byte_i, bit_i := (c.i.shift_right_logical(3), c.i.bit_and(1.shift_left(3) - 1));  // 366.7 ms ±   2.6 ms
  ```
  udiv_mod lets backend convert it to shift. 
  becomes `366.6 ms ±   1.0 ms` if i make udiv_mod use 
  udiv+umod instead of udiv+mul so it can convert to shift+and. 
  thats cool. 

## (Oct 14)

more fixes from tests/todo
- void_to_struct_as_return
  - call_direct auto inlines the body if it's an Expr.Value without checking if func.EnsuredCompiled, 
    which is fine except if it's a unit literal `()` from the parser that needs to be coerced to a struct with all default fields. 
- w.fr is the same as undef.ssa which has been fixed.
- x.fr (typchk_unquote) problem is that @slice doesn't check all arguments
- also while im looking at @slice
  - have sema always force the arg's type to be an Array at the end. 
    and make emit_ir/compile_expr(Slice) get the count out of the type instead of looking at the shape 
    of the expression again, means it can't get out of sync with how sema counted the elements. 
  - heck but that spirals into construct_aggregate needing to allow single element array and 
    bake_relocatable_body needing to handle array itself because when i create the type with intern_type,
    it doesn't instantiate the bake_relocatable_value in `fn Array`.  
    probably not worth the hassle. 
- added arity checking for direct macro calls so you can't confuse it by passing one expression to something expecting a pair. 
  mostly didn't come up because they're normally called through overload sets so the overload resolution handles the arity checking.  
- c.fr skip_to_closing_squigle was eating the first character as though the caller hadn't already seen the `{` 
  (peek doesn't consume the token but it does advance Lex.current). 
  - i did confuse myself for several minutes when trying to add that test to front.fr 
    because i'd run it with the old compiler expecting it to fail 
    but the test recompiles the parser and the new source had the fix. 
    spent longer on that than actually the trivial bug :(
- o.fr, q.fr: 
  make sure to type check each leaf of Expr.Switch, not just pass the requested type. 
  recurring theme today is places where a multipart expression doesn't have all parts type checked. 
  - i think the fragility is sometimes i change expr.ty and then yield before checking it against the requested 
    one and that ripples out to having a different requested type when you come back? 
    should be more structured about whats going on next time i rewrite the sema stuff 
    (which will probably need to happen to get order dependence under control). 

making unpacking dependencies less fragile
- need to not rely on exec-ing as many things, start with writing my own reader for the compressed files. 
  my impression is that .zip is annoying because the wrapper format puts the 
  header in a random place and you have to start at the end and guess where it is in a fragile way. 
  so i guess i'll do .tar.gz since thats the other one that seems ubiquitous. 
  they're both deflate anyway (i assume thats the hard part) so i can change my mind later. 
  and maybe i'll change my mind entirely and use wuffs instead but thats annoying because you gotta download that somehow too. 
  probably more fun to read the rfc than just steal one so here we are for one thousand years. 
- ok the gzip part is trivial, thats nice. and i feel like you don't even need to use any of the fields, just skip them. 
  like who cares what the original file name was, the only thing im going to do is put a tar file in there, 
  and it seems the compression algorithm's always deflate, they just reserved 255 more in 1996. 
- the tar part seems more convoluted. theres like 6 different flavours of headers? 
  hopefully everyone just uses the posix one, that sounds possible. 
  the file starts with the string "pax_global_header" which i guess has more stuff in there, idk, 
  but it's shaped like a file so it seems i can just ignore it and still get all my files out 
  (for the one .tar.gz file i've tested).
- deflate
  - spent far too long on not reading the BFINAL bit


## (Oct 13)

- handled Alias in move_from_module
- made sort.fr more generic like how zig does it so you can easily use the same algorithms on parallel arrays. 
- chess
  - ported the simple version of best move search and hooked it up to the gui
  - gui: zoom in/out, show fen when piece labels enabled
  - kinda wasting time with uci supporting things i don't need and making it more convoluted but here we are. 
- maybe i've collected enough compiler bugs with the chess adventure that i should start fixing them. there's an idea...
  - zero_sized_union_member
    - is_local_scalar does it based on size_slots instead of load_op 
      so the variable declaration tries to put it in a temporary 
      but the expression tries to initialize into memory. 
      i want to get rid of size_slots, so going with using load_op everywhere 
      (which currently always treats unions as aggragates even if they could be a small scalar)
    - construct_aggregate short circuits when expected=1 and just emits the single field to the placement, 
      the thought being that structs have the same repr as thier one field. but for unions like this, 
      the field's type is smaller than the union's type so when the placement is NewMemory, 
      the variable will be allocated a smaller stack slot than it needs, 
      and when it's zero sized, EmitIr.alloca gives you back 0 as the address, because reading void is a nop anyway, 
      but then it wants to copy the 8 byte union out of the 0 byte fake allocation. 
      just don't do the fast path for unions, they're rare anyway. 
    - that was the last thing using TypeMeta.size_slots so it can go away now
  - array_spread_type_check: was actually a problem with Expr.Tuple -> Array, not the spread operator

## (Oct 12)

- 700 years messing with the weird coordinate space for debugtext
- 700 years on not setting sgl.texturing_enabled
  but now i have images for chess pieces instead of letters. 

## (Oct 11)

- added overloading check that compares struct literal names to the requested type,
  so you can have both of these:
  ```
  fn new($classname: Str) id(classname);
  fn new(it: @struct(kind: Kind, colour: Colour)) Piece;
  ```
  in a program and a call to `new(kind = a, colour = b)` is unambiguous.
- chess gui
  - draw the board
  - click to make moves
- shrink Move to 4 bytes. don't store the castling rook to/from or the french capture target, 
  they're fixed based on the move to/from, just recompute as needed. 86 -> 92 MNPS (-unsafe, cores=1).
- made perft.fr work on wasm
  - my dumb fetch_file was being extra dumb and not allowing internal null bytes. fixed that.
  - so thats a benchmark which is just compute heavy and not hurt by needing to load many many tiny modules for comptime jit on wasm. 
    (both single threaded and -unsafe. name: native ms -> chrome ms = overhead)
    - self compile: 1309 -> 5927 = 4.5x
    - perft: 73321 -> 114421 = 1.5x
    looks like the sucky way of doing jit is where most of the slow is coming from.
- macos/app.fr hellscape trying to get a menu bar in full screen
  - `NSApp.setActivationPolicy(NSApplicationActivationPolicyRegular)`  
    lets you select the window and get key events
  - `w.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenPrimary)`  
    gives you a menu bar when you mouse to the top in full screen 
    but it doesn't work if you do the activation policy as well (so no key press)
  - `NSApp.setMainMenu(new "NSMenu")`  
    instead, explicitly adding an empty menu bar makes it work and doesn't break activationpolicy
  - sadly part of my process was dear gemini-ing 
    (because if you google anything you just get idiots telling you to use the settings app)
    - "my macos application does not show the menu bar when i put the mouse at the top of the screen in full screen mode"
    - "it works if i use MTKView instead of CAMetalLayer"
    - (`[...]` show you a minimal example of a Cocoa app?) "yes"
  - its answers weren't true but its example program did compile 
    and randomly removing different combinations of lines 
    until i could make it behave like my broken thing or correct narrowed down the probem. 
  - does the lies machine still turn my brain into soup if im just using it to replace 
    apple's "chuck a zip file that opens in xcode over the wall" examples with something that's one file you can feed to clang? 
    maybe it's not worth the moral dilemma.  
  
## (Oct 10) chess

- mistakes
  - one place i was using zoidberg index instead of value. 
  - typoed a variable in knightMove and lost one
  - moved queen like rook twice instead of rook then bishop
- ported the perft-devide debugging code (which involved the uci parsing stuff). 
  very nice to have it spit out a position with 3 pieces and the illegal move im making. 
  so easy to fix things. 
- noticed i never implemented the "click for ruble" in farm_game.fr, did that. 

## (Oct 9) chess

- even more of a detour, something's fucked up about the zig version... 
  my notes say i was getting ~140 MNPS perft but now i get 44 MNPS. 
  spending 66% of the time in `_platform_memmove`. 
  where am i even calling that? lldb says getZoidberg(), wtf? 
  lol it's copying the whole array every time 
  just to read one value out of it... with -Doptimize=ReleaseFast
  fixed by explicitly taking the address of the array before indexing it. wtf. 
  idk why only that one place i index the array matters, maybe its freaked out by the index being a function call. 
  ```
  before: Thread 0 finished 165 perfts in 154040ms.  44031273 nodes / second.
  after:  Thread 0 finished 165 perfts in  44173ms. 153545771 nodes / second.
  ```
  i guess they broke that between `0.11.0-dev.3937+78eb3c561` and `0.14.1`, 
  and i didn't rebenchmark when i updated zig.
  probably https://github.com/ziglang/zig/issues/13938
  so glad i can focus on debugging my application rather than debugging my programming language knowledge.
- continuing porting movegen, found some nice compiler bugs, so this was worth it. 
- made `@switch` require a default case. dumb that i was letting you silently panic instead of documenting that you thought you were being exhaustive. 
- lots of silly mistakes with parsing fens, very error prone

## (Oct 8) chess

vacation. porting my old chess program. 

- its so sad how much easier the trick for VisitBitPermutations is than what i was doing before. 
  127 lines -> 17, and most of that is still iterator unrolling junk. 
  idk how it works but it seems to get the right answers. 
  https://www.chessprogramming.org/Traversing_Subsets_of_a_Set 
- got precalc (magic bit boards) working
- inline_for does help possibleSlideTargets.
  and then also inline_for goes by reference 
  and the compiler doesn't trust that its constant so backend doesn't hoist the load,
  so doing that manually helps even more 
  samples in that function: 253->212->184, runtime: 360ms -> 297ms.
- tho actually there's only 120832 unique possibleTargets calls 
  and for_subsets is in a deterministic order so it could be a 1MB table,
  without hashing, using your place in the iteration order as the index. 
  tho as i type that, i realize why don't i just use the old magic bitboards while searching for new ones. 
  (still, the known index thing might be an advantage over the real case where you only have the blockers mask). 
- the one where you skip the edge is harder to think about 
  (it's not just ignoring 0xff818181818181ff because its fine to move along the edge, its just when its the last square of movement)
  but helpfully the one that's easy to get rid of is the one in the loop,
  samples: -> 1. 91.6ms. now set_bytes is all the time.
  oh and the other one is what i already have to store for masking the key so move that into OneTable instead of keeping it in a seperate array. 

Things that were better in the zig version than in my language:
- bit fields that contain enums
- small tags. my @tagged always uses 8 bytes so i don't like using it
- automatically taking address when calling a method that expects a pointer
- i need to take address to index array which is verbose
- bit boards sure use a lot of &|^ that don't have syntax sugar in my language
- defer
- `inline for (directions[0..8], 0..) |offset, dir| outer: { while (true) { break :outer; } }`
- `@case("P".ascii()) => foo;` is much clunkier than `'P' => foo,`
- needing to instantate overload sets for @enum is dumb
- can't put constant fields on a @bit_fields so i end up with like Piece_EMPTY instead of Piece.EMPTY

> i can cmd+tab to the terminal and recompile before zed autosaves
> so i get the same compile error again 
> and have the error message show the text that doesn't exist in the file anymore. 

## (Oct 7)

- trying to be more structured about accessing tok.next while parsing.
  eventually i want lex/pre to be lazy instead of building the whole list of tokens up front.
- table for ascii case of is_ident(1,2). musl time: 3215 -> 3052 (-5%).
    - i also tried two longs and it was worse
- new_num_token is called 769967 times but its always a 0 or 1 for (read/eval)_const_expr replacing identifiers. 
  its only 40 samples but so easy to cache, might as well. 
- use read_punct for preprocessor directives. 
  it's only like 10 samples of equals() but not doing string comparisons makes me feel better about myself.
- pfff, getcon skipping the hash table for zero is 2984 -> 2882 (-3%)
- convert_pp_int: don't check all possible suffixes before checking the length 
  since most of the time the digits will be the whole token. 2866
- parse_logical doesn't need to bother with usual_arith_conv because the else branch already did a cast. 2843

## (Oct 6) import_c/musl

- more parsing in attribute_list
- need to allow a local symbol to be weak. 
- stub out init_tls in my test program for now.
- now it gets to the point of dying on unimplimented asm trying to do a syscall. 

## (Oct 5) import_c/musl

- make sure defines match their makefile. 589.
- fgetln: undefined temporary. 628.
  - for locals with init.all_zeros, im skipping create_lvar_init because gen_zero_var does it, 
    but when static_array_of_unknown_length like `(long[]) {0}`, it doesn't know how big it should be and never defines var.stack_slot
    and then also set ty.array_len if its missing when skipping the `{0}`. 
- oh heck, the early constant folding where i don't even emit code for dead if branch doesn't work
  because you're allowed to have a goto label in there and jump to it from the reachable code. that's sad. 944.
- `__syscall_cp_c` is defined without a prototype and then aliased to something with parameters.
  seems im supposed to just do the default argument promotions but not treat it as variadic on platforms where that has a different abi. 
  that matches what apple clang does. 1118.
- parse (and ignore for now) global asm statement.
- now i get through all the .c files
- make running on all of musl less slow. 
  - 1378 samples in tokenize_file, 325 are update_kind for keywords. 
    replace that with generated switch tree like read_punct. now 1064 samples tokenize_file. 
  - locale/iconv.c is slow. newcon is O(n^2). its 35% of the whole musl time. 
    - putting back CONMAP in getcon makes the franca compiler take 20 samples there instead of 12,
      plus i'd have to do something in mark_seen to take out symbols in case isel recreated a symbol+offset that it thought was unused which is a pain. 
    - sticking a hashmap on the c compiler makes global_variable samples go 1781 -> 119. 
  - don't box_zeroed in new_token: 190 -> 105
  - quadratic resizing of Initializer.children for array literal without specified length. global_variable 124 -> 28.
- making a libc.a from my .o files.
  `.dynsym local symbol at index 77 (>= sh_info of 70); error adding symbols: bad value`
  confused myself because emit_dat put the relocation targets in local_needs_reloc 
  even if they weren't local and things ended up on the wrong end of the elf symbol table
- can i actually run a program?
  - woes with weak symbols. `__libc_start_main` for wrap_entry_for_linux_libc needs to be strong. 
  - either:
    - `-nostdlib` and need to say -Wl,e,start to have it use my entry point (and i need to give it the entry point even when .o)
    - `-nolibc` linker still gives me a `start` until im ready to use musl's
  - now its dying trying to set `__environ`
    - didn't help: tried setting size field and STT_OBJECT for data in the symbol table
    - oh im probably trying to get the symbol from GOT because it doesn't know its not a dynamic import,
      but that doesn't work because init_libc runs before data relocations.
      no... thats what clang does too. 
    - must that i need to make all the symbols strong, 
      which gives me a billion `undefined reference to` 
      but hopefully those will go away when i do `__weak__,__alias__`
      - or maybe its tentative definitions not getting emitted? 
        they all seem to be things that are defined without a value on the right. 
        yeah im doing it wrong if you declare with `extern` and then provide the tentative one in the same compilation unit. 
        ok messed with that a bit and now all the errors are for things on the right of `weak_alias`

## (Oct 3) import_c/musl

- change `__builtin_va_(start, arg)` to expect a value not an address
- allow more cases of union declarations. 18.
- parse attribute after variable declaration. 63.
- `__builtin_va_copy` has to do something or it gets mad about reading unused slot. 79.
- "ND_LT should have known type" because not setting the type in new_binary2 when speculate=true. 372. 
  - that almost makes the early branch folding works but now it dies in emitins in the backend. 
    had to make sure hiting a return statement didn't leak a b.jmp on the old block. 

## (Oct 1/2) import_c/musl

- playing with musl, just pointing import_c at random files and seeing if they compile. 
  - to get through a bunch of arch headers, for now just parse extended asm stmts and replace with hlt
  - found a situation where i don't re-enable a macro
  - allow `if` on a float
  - cleaned up the ir for bool casts for jumps a bit
  - tre_tnfa_run_parallel: defwidthle gets stuck looping on a phi
    - running old copy first fixes it
    - maybe its not an infinite loop, maybe it's just O(a billion) 
      because loadopt is creating an insane nest of phis when theres
      a lot of tiny blocks that don't access a value and import_c 
      inflates block counts for logical expressions and CHECK_ASSERTIONS is a gigantic logical expression. 
    - but i don't understand why cproc's ir for that function doesn't have the same problem. 
      looking at a graphviz of the two cfgs, they look pretty much the same to me, 
      and they seem to lower short circuiting logic to the same sort of phis that i do. 
    - ohhhh, my lvar_initializer called gen_zero_var just in case it was a  initializer list,
      so when it was a scalar, gen_zero_var would set the stack slot to zero by doing 
      `%t =l add %var, 0; storel 0, %t` and that didn't count as is_uniform_access in promote() 
      so there were a bunch of trivial stack slots that didn't get converted to temporaries 
      and instead had to go through the compicated unification thing in loadopt 
      where it assumes the memory can escape and has to carefully trace back from each load. 
    - fixed that. lines of loadopt debug output: 1368 -> 63. 
      solved old mystery of why my import_c was always spending so long in load.fr/def. 
      sadly makes coremark drop 19159 -> 18585. same deal as sink where it was relying on that to spilt live ranges?
- save a few copies of b.ins in loadopt and inlcall.

## (Sep 30)

- when folding jnz, propagate block death down the children to reduce narg of phis in live blocks,
  self compile: previously 1119 phis got to isel with narg=1, now only 1 gets there.
  the stack.pop() loop in dedupjmp runs 2149 times, seems like a good ratio. 
- ssa/insert_phis
    - bring back the thing that lets you have a maybe undef variable. 
    - use dominators when deciding if a single def tmp is ok instead of just if all uses are in the same block. 
        - self compile, how many got to the slow case that iterates every instruction: 
          (before: 19120, after: 4380)
    - skip iterating all instructions if the block's gen bit set doesn't contain t. 
- save a fill_use() in release mode that's only for ssacheck
- maintain use info in promote() and save a fill_use()

## (Sep 29)

- don't pin constant divisor division that can't trap 
- tried replacing all the deriveindexable with something that calls items() to get a slice and then does it on that. 
  initially spends more time in overload resolution brings to wall time to 1000ms. 
  but then simplifying the real implimentations by knowing they're slices speeds it up a bit. 
  some are a bit unfortunate:
  - like just having each() go by cursoring a pointer along instead of indexing each time saves 10ms,
  - spilting out the items call into a variable so it's not overloading a nested expression helps a bit too.
  - taking out the non-slice ne overload is another 10ms
  so its 80 lines of dumb code that mostly moves time around so it ends up the same speed,
  but its moving run time to compile time so i guess its a win for programs that aren't the compiler. 
  and adds more stuff i can delete as a treat if i make overloading suck less. 
- `880276 bytes, frontend: 924ms, codegen: 428ms, 952.9 ms ±   3.8 ms, coremark: 19422`

## (Sep 28)

- without gvn: `894232 bytes, frontend: 962ms, codegen: 404ms, 988.6 ms ±   8.1 ms, coremark: 14123`
    - ifopt + smaller resolve_overloads helped that too so gvn 
      still doesn't beat backend time tho in total its helpful, 
      i think i don't want to commit to it until every number is strictly better
      since it's more source code. 
- did some reminding myself how rega works. 
  notably not all spills are inserted in spill(), there's still copies to stack slots added in rega. 
- changed my mind about late copy elim in elide_abi_slots. 
  doesn't help speed (small size improvement tho), but the important thing is it makes the ir 
  more readable which is nice if im going to be working on spill+rega. 
  - same thing: bring back the trivial dce in spill
- don't copy instructions in untouched blocks in blkmerge

## (Sep 27)

- mystery kinda solved. thats fascinating. 
  only sink if name ends starts resolve_in_overload_set_new...
  `899628 bytes, frontend: 965ms, codegen: 447ms, 994.5 ms ±   6.3 ms, coremark: 19651`
  since my language is stupid, that one function is 400 lines and 25% of the run time,
  and i guess there's some pattern that sink helps there and it goes 400 -> 350 samples. 
  - just splitting that function into smaller ones makes it faster: `967.5 ms ±   8.3 ms`. 
    so problem is im bad at big functions. probably because rega only does one hint per temporary,
    and even inserting a spill doesn't make more temporaries (ie. its not ssa after spill()). 
    so if you have a value produced early in the function and used in multiple (non-nested) loops,
    the first one that's processed will hint it to go somewhere, and it will keep using that 
    old hint for the next loop even if the situation is different. 
    and like it already sort of allows for range splitting, like if it can't get the hinted 
    one its fine with the same value being in different registers at different times, 
    but it doesn't realize that the hint has a limited time value. 
    so the reason sink helps is that its pulling things down to point of use and making new temporaries 
    for them (because its still ssa at that point), so its effectively letting 
    those different live ranges have a unique hint slot. 
- ifcblk: push to the block instead of always reallocating
- filter the debug printing `## Spill costs:` to not show dead temps so its less spam

## (Sep 26)

- qbe says 15% perf gain from gvn on "coremark" which i guess is this: https://github.com/eembc/coremark
  - use import_c on that to see if i get the same or if i made a mistake or if my other improvements dwarf it. 
  - allow passing arguments to `cc.fr -r`
  - i get -13% reported `Total time` (for fixed iteration count) with ENABLE_GVN=true, so that's reassuring, 
    it is generating better code (at least for some programs), 
    its just doing more work to do so which makes the compiler slower. 
  - CORE_DEBUG first `Initialized list:` is different for me and clang, `[0000,ffff8080] vs [0000,8080]`. 
    problem is passing `(unsigned short) nonconstant_signed_short`, to a variadic sign extends on the laod.
    add promotion to int in import_c/compile/funcall. 
- took ifopt from https://git.sr.ht/~mpu/qbe/tree/2024-11-16-gcm9+assoc-ifcv4-inlinev4
  great success, gets codegen thread time back down to what it was before gvn. 
  - before: `908144 bytes, frontend: 1011ms, codegen: 493ms, 1.041 s ±  0.007 s`
  - after: `905276 bytes, frontend: 993ms, codegen: 455ms, 1.025 s ±  0.006 s`
- tried tracking if a tmp came from a comparison (so known to be 0/1 and remember after breaking t.def)
  and convert some patterns with constant 0/1 to and/or instead of sel in ifcblk but doesn't seem to be faster. 
  sad that i don't know when a load is known to be a bool. 
  llvm's type obsession is looking more reasonable now.
- part of why sink() on memargs helps so much is that isel can't even pull an offset into the memory access
  if the address is computed in a different block and is not an RSlot because of :ReferencingOldBlock, 
  - so like ok, just iterate blocks in post order for isel right? 
  - ha, yeah, now sink() just makes the compiler slightly larger 
    and removing it makes coremark iter/sec go 17420 -> 19540.
  - strangely, while sink isn't helpful, getting rid of it makes ALSO_OLD_COPY=true 
    no longer help either. sink+copy still saves 30ms self compile wall time. 
    - the part of old copy that matters now is the folding extension into load. 
      moved that to new copy and now the compilier is fast with sink() and coremark is fast without sink(). 
      which is backwards from the intuition? duplicating the code out more 
      makes the compiler do more work in the hopes of making other programs faster. 
    - maybe sink() prevents inlining something and thus makes the compiler do less work?
      no, both save_for_inlining the same number of things. 
  - no sink: `898800 bytes, frontend: 1003ms, codegen: 447ms,  1.028 s ±  0.006 s, coremark: 19444`
  - sink: `902436 bytes, frontend: 950ms, codegen: 446ms, 991.3 ms ±   2.8 ms, coremark: 17470`

> i sure wish zed wouldn't leak file descriptors (??)  
> it eventually dies and says "too many open files os error 24"
> and pegs a core in "system" and you have to restart it and lose your undos. 
> i feel like it started happening once i started having two windows open. 
> maybe letting it update will help, who knows. 
> hey i wasn't crazy about the scrolling thing!
> "•macOS 26: Fixed an issue where scrolling could sometimes feel choppy. #38179"
> note also that their markdown viewer thingy for the release notes is not 
> text that you can copy from so to get that sentance i took a screenshot 
> and used preview's ocr to copy it out of the image. 

## (Sep 25)

- qbe doesn't say ceql/cnel has_id but i guess they're kinda not a copy because they change width?
  i feel like its fine, i just have to check that i.cls=arg0.cls. 
  similarly, why not do defwidthle for Kl?
- i don't trust narrowpars, if hoisting ext-s was important, why only do it for pars? 
  and it's slower than it looks cause it's an extra fill_use
- `908880 bytes, frontend: 1000ms, codegen: 491ms, 1.036 s ±  0.006 s`
- allow quoted symbol names in text ir parse and dump them that way when they're not valid identifiers,
  so it can parse back the output of import_c's mangled statics. 
- ascii_table works if output it to text and then parse that back and compile it. 
  so its some piece of state that's wrong. 
  works if i do `>=` instead of `>` on sN.loop in rporec. 
  problem was in import_c conditional being folded and not clearing cond_block.s2,
  added that to fails_typecheck. 
- riscv Kw emit
- fix amd64/isel/fixarg calling is_int(Kx)

## (Sep 24)

- just by inspection, it does work, like functions that call i.op() multiple 
  times now only does the `op30_cls2 & 1073741823` once, which makes me feel better 
  about not storing things in variables when im writing code which is kinda the point 
  of doing optimisations. but its sad that the net effect doesn't make the compiler faster. 
- i can't get rid of blkmerge because it helps inlining (and emit_static_memmove has a hard dependency on that),
  - use edgedel in dedupjmp so phis don't reference dead blocks, 
    fixes assertions in replacepreds and write_phi
  - TODO: still broken "rpo not in order" on examples/ascii_table.fr
- interestingly with old fold+copy instead of gvn, mark_seen(force=true) 
  makes it go 896188 -> 943716, 
  so maybe a lot of the code size bloat im seeing with gvn 
  is just that it's emitting functions that have all uses inlined and never calling them. 
  yeah iterating all the instructions after gvn to mark_referenced correctly  
  goes (983380, 1073ms -> 935696, 1055ms) which is still worse than the old way but not as bad. 
- don't sink or gvndup when its just offseting from an alloc (check by looking at tmp.alias.slot)
  because isel converts those to RSlot right before use site anyway. 
  saves 5k and 15ms codegen time. 
- a weird aspect of these numbers is that its compiling the new compiler with itself, 
  so if the new gvn stuff is more code then it's doing more work. i want the optimisation to pay for itself. 
  - old copy+fold: `896188 bytes, frontend: 981ms, codegen: 450ms, 1.012 s ±  0.007 s`
  - gvn: `930196 bytes, frontend: 1010ms, codegen: 509ms, 1.048 s ±  0.007 s`
  - old copy then gvn: `914828 bytes, frontend: 965ms, codegen: 518ms, 1.012 s ±  0.008 s`
  - running old fold before gvn as well doesn't help, so the problem isn't that gvn can't fold sel instructions. 
    similarly the situation in test/fold2.ssa being worse isn't the main problem because that's also in fold. 
- disabling simplify_phi in old copy puts it back to 930k, 
  so im making redundant phis that the new thing can't get rid of. 
  using simplify_phi as part of phicopyref gets the code size improvement but is slower. 
  `915868 bytes, frontend: 1004ms, codegen: 502ms, 1.046 s ±  0.006 s`
- did sel folding: 913116
  - added a test that it only looks at the low 32 bits of the comparison

## (Sep 23)

- stealing qbe's global value numbering
  - confusing time because now it needs defining_block zeroed and i was only doing that in debug mode before
  - self compile works. not impressed, it increases time and code size, 
    but it also doesn't pass on all the tests, so very possible i've still made a mistake. 

## (Sep 22)

- oops, i wasn't setting RegaCtx.loop, 
  but fixing that still doesn't make the wait_list or prio2 stuff seem helpful.
  tho something i wasnt thinking about is that for loop stuff it isn't just about 
  fewer copies is better, having more copies in the program but fewer in deeply nested loops
  is faster. by flipping the loop sort order in carve() i can choose between 5k of code size or 10ms of time. 
- for now im just going to go with less code is better and get rid of stuff. 
  getting rid of everything i want to costs about 25ms, which makes it seem like the wrong choice. 
  but also, just changing the block iteration order i can trade 30k for 30ms, 
  and that's a one identifier diff instead of a 100 line one, 
  so seems like the code im getting rid of was a lot of hassle to get the same effect. 
- tried another round of copy elimination in opt/slots instead of relying on rega to fix it. 
  size is 897984 -> 886356 (-1.3%). nifty but not worth the trouble: 
  - the tests pass except for wuffs_c/(webp.c, jpeg.c) on amd. 
    narrowed it down to one tmp in wuffs_webp__decoder__decode_pixels_slow. 
    the problem is the interaction with previous removal of extuw of Kw results. 
    produces a copy assigning a Kw to a Kl. you end up with a value that gets 
    produced as a Kw and used in a Kl phi. it's spilled to a 4 byte slot 
    and the phi uses an 8 byte load from it. 
    can hackily fix by always using 8 byte spill slot and 8 byte store to it, 
    that way it's fine for the phi to load more and get extra zeros. 
  - but that also breaks (xz,lzip,lzma) on arm. 
    not even reliant on the new copy stuff, 
    maybe its just making a stack frame large enough to reveal an encoding problem. 
    yeah problem is a store where value and address both need to go in the scratch register. 
    happens when there's a copy where both sides are a spill slot out of immediate range.

## (Sep 20/21)

- there must be some pattern that breaks my arena allocator. 
- figuring out which tests newly work on linux now that backend provides start wrapper
- got annoyed by all the places i have to `emit(i[])` instead of emit(i), added an overload. 
  - arm code size 925k -> 916k, thats nice. that's like half what i lost by getting rid of deduplication. 
- arm/amd implicitly zero extend 32 bit operations,
  - before isel, replace extuw with a copy if the argument's def is Kw. 
    - don't want it in copy_elimination because i want to reuse cache between architectures
  - doesn't work if the def is itself a copy instruction which is a bit concerning. 
    oh it's when its a copy from register for a parameter, 
    and that's the case every time its a copy because of copy-elim, so tightening the condition doesn't help.  
  - happens 1119 times (filtering type=Exe so only counting what goes in the binary). 
    arm bytes of code: 915856 -> 912904 (-0.3%). 
    (amd only saved half that but still an improvement). 
  - old compiler both times, the code for that only costs 915856 -> 916104
  - it only saved (916104-912904)/4 = 800 instructions, 
    so sometimes the register allocator is too dumb to put both sides of the copy in the same register
- experimenting with rega/do_parallel_moves because the code generated for swap on arm looks really silly.  
  - found tilting at windmills paper and got it to work with carrying the temporary 
    register across the moves so it doesn't waste time putting a value back in the wrong place,
    and 2 dependent swaps become 4 instructions instead of 6. 
  - looks nicer but code size is only 913596 -> 911968 (-0.17%). 
    so clearly this is the wrong tact and i should be trying to assign registers better, 
    not micro-optimise how fast i can get them to the poorly chosen places. 
  - also some problems:
    - can't reserve an int for scratch on amd so need to do swap there anyway and it's sad to have two code paths
    - since holds the scratch for longer than usual, its a problem if emit needs to use it to 
      create the copy, like if it's a spill slot that's far away because of a large stack frame. 
    - have to give up and reserve a float on riscv, but that's easy.
    - should output them backwards instead of reversing at the end
- sethint for tmp->tmp copy in rega/do_block_instructions: 912848 -> 905052

## (Sep 19)

- provide a start function for linux exe so .ssa and .c work without a linker
  - kinda regretting getting rid of ExprLevelAsm, adding another layer of wrapper to get 
    sp and then call an ir wrapper because i don't want to write the calling 
    __libc_start_main 3 times (each arcitecture) is annoying, 
    actually it's kinda fine to just do an add_code_bytes without a return 
    and fallthrough since i know functions are output linearly. 
- function.c
  - `-2131779649 0 glibc.cpu.aarch64_gcs` from 
    `{ char buf[100]; fmt(buf, "%d %d %s", 1, 2, "foo"); printf("%s\n", buf); }`
    (number changes every time, uninit memory clearly)
  - maybe i was wrong about not needing to align sp on arm? 
    no that's not it, also it works with calling printf directly. 
  - not just arm, riscv is same (with different junk),
    also broken with `-c -nostart` and linker. it works on amd tho.
  - i bet im passing va_list by value wrong. 
    yeah, rv works if i make it a single pointer, 
    and arm it's 32 bytes not 24.
- riscv: `./target/r.out abi5.ssa -bin ./target/q.out -cc`
  ```
  < ta: -10 10.100000
  ---
  > ta: -10 10.099999
  ```
  - works with clang compiling the driver instead of import_c
  - also works with cross compiling (`franca backend/meta/qbe_frontend.fr backend/test/abi5.ssa -cc -t rv64 -o a.out`)
  - strtod in tokenize is returning the same bits on both: 4621875412584313651
  - difference in binaries is just when adjusting that constant after lui: 
  ```
  14768:	99a28293          	addi	t0,t0,-1638
  ---
  14768:	99928293          	addi	t0,t0,-1639
  ```
  - can't recreate in .ssa with an integer literal of that number or the f32 version 0x4121999a. 
    oh so problem with downcasting the f64 to a f32? 
    yeah, with that i can make a that that behaves the same when cross compiled. 
  - truncd wants rm=0 instead of rm=1

## (Sep 18)

- tests/x64_encoding.fr is more code than it's worth and i don't even run it
- dynamic libraries on linux
  - dlopen doesn't accept it if there's no symbol table, fair. 
    make sure to add exports after collect_aot_fixups. 
  - now dlsym can't find the function. `readelf -s` sees it. 
    probably need `.hash` or `.gnu.hash` (clang puts them in Elf.Dyn as well). 
    this is what google tells me https://docs.oracle.com/cd/E23824_01/html/819-0690/chapter6-48031.html
    can i just not bother with the hashing and have one bucket so the chain is just incrementing indices. 
    have to mark them global in the symbol table as well.
    ok that works, but exports every symbol. so only put the exports in the linked list. 
 
## (Sep 15)

- trying to get rid of some of the tests/todo
  - c.fr: taking address of direct field access on a constant pointer. 
    - just compile the var as normal in compile_place_expr. 
      allowing Expr::Value with ty=ptr makes it work for field load as well. 
    - not sure if i want to allow arbitrary values to cast to thier address as well
      (would allow using field of `::` but also mutating it which feels wrong)
  - l.fr: coercing Fn->FnPtr->rawptr as argument to a call works at runtime but not with an `@run` on the call. 
    - it's fixed by adding an extra check against requested in compile_expr(FnPtr), 
      but i don't understand why it makes a difference if it got there through immediate_eval_expr or not. 
    - oh ok, the difference is that the const path calls compile_expr more times 
      so the runtime way it gets bored of compiling soon enough that it gets to emit_ir 
      which doesn't care about fnptr vs rawptr? 
      makes a difference because compile_expr->GetVar->compile_get_var->coerce_constant->make_fn_ptr_expr
      doesn't set expr.done=true, so that coerce sets the type to rawptr, and then compile_fn_ptr sets it back again. 
    - so the alternative solution is to only update the type make_fn_ptr_expr if it's UnknownType.
    - also raises the question of why im ok with implicit casts to rawptr of function pointers but not other pointers. 
  - b.fr: just a parser mistake 
    - wasn't requiring a comma so `(a = b c d)` parsed as `(a = b c, d)`. 
    - can't chain multiple quick exprs like that because it's the same as `if foo() {`, 
      where you want the block to be passed to `if` not `foo`.
  - switch_type_error and index_type_error work now, move them to the errors test
  - declare that the deduplication tests aren't going to be fixed. 
    even if i bring back deduplication, trying to do it without shims is too restrictive. 
    i have no intention of guaranteeing the pointer identity it expects.
  - e.fr: #use inside a block prevents lookups that escape to the outer scope. 
    read gets treated as a new declaration? 
    the problem is only when it's inside nested capturing_call. 
    so it's pausing the do the #use and then getting called and renumbered. 
    but the outer is inlined before the inner, so the inner one hasn't been 
    resolved yet, still has GetNamed, which isn't renumbered, 
    and still points to the parent scope not the cloned one? maybe? 
    i can fix it by just ensure_resolved() before renumbering a block, 
    but i feel like that shouldn't be necessary. 
    
## (Sep 14)

- wasm situation seems fine without temporary_alias, 
  for aot you can always do fixups and for jit it's been replaced by using the table. 
- bump_dirty_new must be left over from when i was doing comptime_jit on another thread.
  it's only called in compile_prefix_macro and report_called_uncompiled_or_just_fix_the_problem
  which doesn't make any sense if it was doing anything. 
  similarly icache_mutex
- emit_instructions
  - auto-convert i64 argument with f.getcon(). 
  - tried to not have the runtime array of Cls but it gets painful 
    because you don't want to evaluate them muliple times 
    and if a ref is an argument, not a new one, the cls will be wrong 
    so can't just notice it's a template cls when emitting 
    and replace the cls in the instruction with the one on the tmp. 
    - tho the reason it annoys me is just that the useless stores to create the array don't get eliminated. 
      problem there is that slots opt pessimistically treats all loads as 8 bytes when deciding if a slot is live.  
- unfortunate that getting rid of the coalesce() opt 
  makes it slower but smaller code 
  because slots opt does better when slots don't alias.
- the winds have shifted; SAVED_PAIRS is smaller and faster now, 
  with the slight disadvantage that it doesn't work (mostly shows up with the wuffs c examples). 
  but i think the pairs were already broken when it needs fixarg,
  i just got lucky that i don't have many big stack frames. 
  ```
  //       if both need fixing with x17 we're fucked. 
  //       because the first one gets fixed first and x17 goes in the instruction,
  //       then decode_mem for the second fucks up x17. 
  //       `off` will be 0 for both because they both try to put the exact address in x17. 
  //        so it will bail out and leave the second address in x17 instead of the first. 
  ```
- did i somehow make run_tests_main_threaded more crash-y this commit? i don't understand. 
  (started happening before i turned on SAVED_PAIRS so it's not that)

## (Sep 12)

- stop making json strings manually for the web demo. 
  think i spent less time doing it properly than i spent messing around with trailing commas before. 
  strange when its hard to tell which things will be hard. 

## (Sep 9)

- converting more of the c in web/gfx.fr
  - whats going on in copy_buffer_data? extra_data isn't used. 
    lol, twinning: https://github.com/floooh/sokol/commit/92d1649e
- web/shaders: 
  - insert casts for unpacking functions. they only take u32 not i32. 
    - actually no, im just giving UShort2/UByte2 the wrong types
  - sadly wgsl doesn't let you put textures in structs (unlike msl),
    if i just hack it out of debugtext and put them in parameters 
    instead so they get lifted to globals, app_events.fr works. 
    did some stupid hacks to automatically pull struct fields out when they're resource types like that. 
    kinda garbage. 
  - no point_size
- pleasingly, depth_test.fr works on webgpu native but not metal, so i've narrowed down the problem.
- terminal.fr -wgpu is flickering garbage. ive had this problem before. 
  so glad i wrote that down last time: don't call surface.present after trying to skip a frame. 

## (Sep 8)

- examples/repl.fr
  - query_current_os is wrong and making it choose `__error` instead of `__errno_location`
  - franca_runtime_init isn't called when running a program directly in the compiler. 
    which is kinda dumb, it has the case for DRIVER_VTABLE_MAGIC and every time this comes up 
    i look at that and assume thats the entry point because that would make sense but actually 
    it's only used if you explicitly compile to .frc and then run that.  
  - so when you do `franca examples/repl.fr` the compiler sets 
    when the repl inits a new compiler it gets 
    `ComptimeEnvironment.comptime_os = query_current_os(),` 
    which is correct because it's inherited from the aot compiler, 
    its global OS is still wrong (zeroed), 
    (that comptime_os is only used for the child program in the repl so that one's OS is correct).
    so then the next time the jitted compiler does push_zeroed_dynamic_context for 
    report_called_uncompiled_or_just_fix_the_problem, it messes it up for anything down the callstack. 
    doesn't quite make sense because i don't see that in the callstack where it's dying? 
    maybe it's lost between:
    ```
    23: Anon__27374 AOT
    28: call_dynamic_values__1723 AOT
    ```
    yeah! it's the Anon, it's not getting the right inferred name. 
  - anyway, that was a linux problem not a riscv one. 
    and i only tested it aot, not running like a driver. 
    that was an unfortuate collision of many the hacky things i do in this compiler. 
    should probably try to do some pruning so stuffs less confusing. 
- rv: oops i was doing the isel sub thing on floats too
- i want to find an embeddable assembler i can use for inline asm in import_c. research:
  - tcc is tempting but it's so annoying that it wants to have the target be an ifdef so 
    i have to have many copies of it to cross compile. also it doesn't have an arm64 assembler. 
  - https://github.com/tyfkda/xcc 
    - has all the arches i want, tho same ifdef annoyance. 
    - they also have a very non-offensive libc, other than the one function per file thing that everyone does. 
      maybe that's what i want in general, doesn't solve my no ifdefs requirement of course.
      and they don't have pthreads, sad. 
    - no mach-o linker but thier as does do mach-o, 
      tho for my inline asm thing i'd rather just always use elf anyway. 
      hopefully nothing in the assembler other than which object format to use cares about XCC_PLATFORM_APPLE.
      sadly it does: @gotpage vs @page, rules about addends. 
- tried to clean up my hare test runner, mostly just made it longer, idk
- use get_or_insert more: 
  - backend/(meta/parse.fr/parse_ref, pack_wasm_result_type_outlined)
  - import_c/(tokenize.fr/read_punct, compile.fr/get_block)
  - compiler/values.fr/intern_type

## (Sep 7) rv

- compiler/main.fr linking libc doesn't work without FRANCA_NO_CACHE=1. 
  so the problem is just in cache loading.
  - qbe_frontend mandel.ssa -frc and then -r on that works, so fill_from_libc isn't the problem. 
  - i'd fucked up as_asm(Sym, Header) because it uses array syntax that's dependent on 
    the order of the Arch enum and i didn't notice because i didn't try to do caching on wasm anyway.  
- trying to run compiler/test.fr natively: 
  - `error while loading shared libraries: libc.so.6: cannot open shared object file: No such file or directory`
    does qemu really not capture the exec syscall? i have to do the binfmt_misc thing? 
    that kinda sucks. blink and orb are magic and just work. 
  - the incantation that seems to have done it is: 
    `sudo apt install binfmt-support && sudo dpkg --add-architecture riscv64 && sudo apt update && sudo apt install libc6:riscv64` 
  - and now i can just run a riscv binary like it's normal. that's kinda cool. 
  - can't just -target riscv-linux-gnu to clang tho that would be too easy. 
    some how it's easier to make a new fake orb universe and install only the riscv clang 
    than it is to just link the program i want to link... every time i have to interact with 
    this sort of thing i get more respect for zig's personal copy paste of clang 
    that doesn't suck as much and just works via magic. but idk i feel wierd about adding more people 
    to the chain that have to touch the compiler, i want it to work with the normal one. 
    but really it doesn't matter, the answer is i should just stop relying on a linker to give me a crt0 for non-franca programs. 
- fix double addend in auipc relocation... but the old way was sure working before...
  did they change whether the second addend gets ignored or added between clang 19.1.7 and 20.1.2? 
  that's a bit unsettling. 
- skip the tests that aren't expected to pass yet
- do signal_handler_body so stack traces work 
  - had a hard time with fp always being 1 because i forgot an #align
- stub try_impl so import_c works (as long as there's no compile error)
- isel: convert sub(const) to add(-const) because it might fit in an immediate
- examples/import_c/test/test.fr: `bad simple_op`
  - fixed fixarg after salloc being on the wrong instruction
- examples/ascii_table.fr: `rv(non-reg: RCon:1)`
  - never try to use immediate in float instructions
  - which is a bug qbe has as well: 
    `%a =d add %b, d_0` -> `error: invalid operand for instruction: fadd.d fa0, fa0, 0`

## (Sep 6) rv

- abi8.ssa: add back the padding at the end of the parameter register lists so it's fine for argsclass to go past the end
- emit for bit ops: byteswap, ones, clz, ctz, extsb, extsh, extuh
- untested float ops: min, max, sqrt, swap
- for now, just do an extra instruction for offset from an import. 
  - strangely that made encoding.ssa work in libriscv. 
    added $no_fixup_addend now it's back to only working with --single-step OR in qemu. 
- did the AsmFunction stuff needed for the franca start/runtime/whatever. 
  hello world works with -syscalls but not with libc.
- deal with conditional jump farther than 4kb 
  by adding an extra direct jump and conditionally jumping over that instead.  
- try more random shit for loading immediates that don't fit well. 
- setup temporary thing to cross compile and run `test/*.fr` (until compiler itself runs on riscv). 
  only `23 of 399 tests failed` 
  - tho some are just at comptime so aren't testing anything
  - and some fails are just because `-syscalls`
- lldb seems to be able to talk to qemu which is very nice. 
  pass `-g 1024` to qemu-riscv64 and it waits on port 1024 before starting. 
  run lldb and `file a.out` to get symbols and `gdb-remote localhost:1024` to connect 
  and then you're in a breakpoint at main and just `c` to continue. 
- dying in try_lock: `2182c: 2e63a2af           .insn 4, 0x2e63a2af`
  - it's casw, sadly even when i encode it right (like the cas.ssa test that works) objdump doesn't know it. 
    aboutrv.com/tools/disassembler says it's right: `0x2e63a2af → amocas.w t0, t1, (t2)`
    and anyway it's `stop reason = signal SIGSEGV` not illegal instruction. 
  - so is this a bad pointer? `t2 = 0xffffffff891ef880`
    i bet that i sign extended 0x0000ffff891ef880 cause that's closer to other values i see in registers. 
  - yeah, in isel im doing fixarg(k) on the cas which would be Kw here 
    and i hyperactively insert sign extensions 
    when using a Kl tmp as a Kw arg (because comparisons want that i think?)
  - that fixed lox_main and parser_doesnt_crash. 
    and now stuff that uses the backend gets past init_default_module
- `type(*Con)` is giving the wrong answer. it has comparison ceqw between constant -1 and a loaduw -> Kw, 
  so the load needs sign extension to compare equal because it's after being xored with the sign extended constant. 
  thats confusing. 
    ```
    %v.69 =w loaduw %f.68
    %v.70 =l ceqw %v.69, 4294967295
    ---
    R5 =w loaduw R5
    R6 =w copy 4294967295
    R5 =w xor R5, R6
    R5 =l cultl R5, 1
    R10 =l extsw R5 
    ```
  - it also fixed load_store_u32 test. 
- i'd swapped a bit in truncd/exts encoding
- handle DynamicPatched in load_symbol. 
- now backend stuff works other than the assertion in clear_instruction_cache. 
  use riscv_flush_icache syscall. 
- make cli args work: continue tradition of adding random hack value to sp in franca_runtime_init
- trying to run the compiler itself. 
  - hits an ebreak unfilled patch. lots of fffff in the address means jitted code, 
    probably because im not respecting got_indirection_instead_of_patches. 
  - yeah, now i get ebreak in aot code: asm called from sys_clone. 
  - did that, now jit toy/hello2.fr gets to the callee in run_franca_file and then hits another unfilled patch. 
    - works all the way if i set got_indirection_instead_of_patches for the non-comptime module as well. 
    - right, need to make sure Pending adds a patch even if it's jitted 
      and then add riscv to the UnfilledGotAccessStillNeedsPatch cleanup at the end, that's a bit of a recurring nightmare. 
    - yay! jit hello world works
- fix sqrt encoding. now things that use the backend can be jitted (ie. kaleidoscope)
  - (backend needs sqrt at comptime for generating sha256 tables for mach-o code sign)
- self compile works (you just have to run `./a.out.<time>.atomic` because rename still doesn't)
- riscv only has renameat2

## (Sep 5) rv

- fpcnv.ssa: comparing disassembly between mine and qbe: 
  needed to set rm differently for some. now it works in qemu. 
    - which i don't understand because the ones that i had trouble with 
    were fcvt.d.wu and fcvt.d.w which should never round? but seems i need to have rm=0b00
- ugly thing for cas based on what i did on arm. works in qemu. 

## (Sep 4) rv

- sel with czero pair and then `or` them together. 8.
- did float conversion instructions. 
  fpcnv.ssa: `if (dtow(fin[i]) != (unsigned int)fin[i])` doesn't work, 
  but it also doesn't work if i use real qbe with libriscv. 
  so i wonder if it's the emulators problem? yeah real qbe's works in qemu. 
- qemu says "Invalid ELF image for this architecture" for my binaries.
  fixed by setting the ehsize field of the header.0

## (Sep 3) rv

- it's very creepy to me that running the tests in libriscv pins a core in kernel_task 
  but none of the other ways of running them do. whats up with that? 
  lol profiler says the entire time is in `libsystem_platform.dylib.__bzero`, that's unfortunate. 
  ew, https://github.com/libriscv/libriscv/blob/dee1731f65d62974e901de6070193475e952f0e0/lib/libriscv/memory.cpp#L82 
  on liunx they call mmap (sane), but on macos they just `new` an array of bytes (cringe), which it appears touches all the memory. 
  add `__APPLE__` to a few ifdefs in memory.cpp and now hello world is 3 samples instead of 381 samples,
  and the ssa tests run in 4 seconds instead of 28 seconds. now clang is the slowpoke. 
- floats: add,sub,mul,div, cast, fix store class. 17. 
- float copy and neg are one instruction: fsgnj 
- lower gt/ge to other comparisons
- fixarg(float copy) in isel. 12. 

## (Sep 2) rv

- call1.ssa: load, extsw
- libriscv doesn't do relocations unless the elf header.type is ET_DYN. 
  - hmm, that also works on the other architectures, maybe it just doesn't matter?
    oh and that's what clang does for if you pass `-pie`. 
  - tho actually im normally using a linker when running the tests on linux 
    so the fact that it works at all in the emulator is it doing me a favour. 
- so for sanity i should be using a linker for riscv too until im ready to do my own entry point
  - set a flag about float abi
  - `ld.lld: error: R_RISCV_PCREL_LO12 relocation points to a.o:(.data+0x0) without an associated R_RISCV_PCREL_HI20 relocation`
    time for my favourite game of see what clang does on a c program i guess.
    ew, because both immediates are sign extended you can't do the two instructions seperatly so you 
    need to make an extra symbol every single time and then the lo points to the hi which points to the actual symbol you care about????
    can i just have one symbol at the start of my code section and use the addend to point to the other instruction? 
    seems like no... https://github.com/ahjragaas/binutils-gdb/commit/50331d64f1080c2c9957fb608e0af236b96c1a41
    they combine both addends and use it for the real symbol. that sucks so much :(
    :StupidEncodingsWinStupidRelocations 
  - now call1.ssa works with `zig cc -target riscv64-linux-musl`
- puts10.ssa: jnz, storeb, xor, call import via GOT
- added some hacks to make my test runner libriscv's emulator. 
  it doesn't exit with the status code, need to find the string in the output, 
  and you don't get that output if you pass -silent so can't compare directly to the expected stdout. 
  added an option to use zig's copy of clang as the linker. 
  ```
  franca backend/meta/test.fr all -rv ./target/franca/deps/libriscv-352ab7829e69e50867d970e8839c80c7dd58b33b/emulator/rvlinux -linker ~/Downloads/zig-aarch64-macos-0.14.1/zig -force_linker
  55 of 75 tests failed.
  ```
- stop pretending rnez/reqz are real instructions. several mistakes but got there in the end. 
- lui for producing 32 bit constants.
- fix a few typos with jmp and some assertions in abi. 
- 41
- hmmmm, libriscv is broken perhaps? fixup1.ssa doesn't work 
  but it does if you step through in the debugger or pass `--single-step`. 
  - without that it seems calling printf (or write) stomps s1 which is supposed to be callee saved? 
    also same for putchar which doesn't do a syscall immediatly (buffered) so it's not a syscall stomping it. 
  - tried more recent version (dee1731f65d62974e901de6070193475e952f0e0) same problem. 
    `--no-translate` also doesn't help. 
  - looking at the disassembly of the linked libc stuff, i can see it saving and restoring s1 correctly. 
    to add to the confusion, their debugger seems to disassemble things wrong? 
  	`[0x1005D64] 64a2 C.FLWSP FS1, [SP+8]` but binutils's objdump thinks `1005d64: 64a2 ld s1,8(sp)`,
    it still affects the int register correctly tho (in the debugger). 
      - https://www.aboutrv.com/tools/disassembler agrees with objdump
      - https://luplab.gitlab.io/rvcodecjs agrees with libriscv,
        oh ok defaults to RV32I which decodes as the float one but if you select RV64I you get the int one. 
    so i guess libriscv defaults to disassembling the wrong version of the isa? 
    they choose which one to execute as based on elf header.class correctly tho. 
  - also works if you say --no-jit to build.sh but then it takes 10 seconds to run instead of 0.6
  - theres no way they have all those tests and none of them use callee saved registers
    so it must be my fault but i don't understand how if it behaves differently depending how you run it. 
- (sub. 34), (mul,div,rem. 29), (swap. 27)
- abi10.ssa: parameter slot computation was off by 4x. 25. 
- (strcmp.ssa: extub), 
- shl. add.uw for extuw and larger constants. 21
- the shift/rot encoding is a nice bit set of the things you care about. 19. 
  - allow bool in `@bits` so it's less painful

## (Sep 1)

- i definitly have a problem in aarch64_clear_instruction_cache (signal 4 is illegal instruction),
  - if i use the linker's `__clear_cache` i never hit is_wrongly_illegal_instruction.  
  - seems fixed if i do another `dsb ish` after the instruction flushing part, not just the data flushing part.
    if thems the rules and im cured now thats fine with me.
  - i'd love to understand the rules for the barriers. 
    experimentally, it's important that there's a dsb at the end (the one in the middle after the dc does nothing and isb does nothing). 
    but i assume im just discovering something about one specific cpu and there's some higher truth available.
    it's made extra confusing that executing these instructions on macos is illegal 
    but im testing the linux stuff with orbstack on the same cpu, so there's an extra level of someone gets to choose how it behaves?
  - i really thought i was seeing a segfault before but maybe im misremembering cause it seems fine now... scary. 
- oh i bet the thing with failing to fetch the font is just you race when two programs try to cache the dependency at the same time? 
  for now lets just see if hack reorder the tests makes it happen less often. 
- import_c/test/test.fr problem 
  - only when the test runner is multithreaded. using more threads i can make crash happen every time. 
  - fix the thing where new_token gets called on a string that's not in the file. 
    problem was promote_to_wide_string_literal, can get called on a macro expanded on a different file,
    and using more threads made it more likely to eventually get allocations that were more than 2gb apart?
    so that gets rid of the assertion failure. 
  - the other problem isn't consistant even with lots of threads but still seems to be never when single threaded.
  - still broken if i remove the codegenworker and running the executable, it's just in the frontend part. 
  - oh fuck me it's always a primitive, problem is the ty_FOO are static but the declaration parsing 
    is done in an insane way and passes out the name on a field of the type. 
    because i stole it from a c program and didn't bother to un-stupid that part.
  - sadly that's one that the mutability obsessed people would have caught. 
- fix CopyElimTruncSymbolBug (inlining `fn con`).
  opt/slots was reusing a truncated symbol. added a test for that. 
- added a backend debug flag for warnings 
  so i don't have to feel quite as guilty about hiding things that are probably a mistake but are technically legal.
  not that i'll ever turn it on... but hey, it's the thought that counts. 
- box -> box_uninit

## (Aug 31)

- i think i need to get rid of deduplication.
  - arm binary size: (with shrink: 905200, always shrink=false: 928200, dedup code commented out: 925056). 
    speed is about the same. it's ~50 lines saved. 
  - 20k bytes isn't worth it. 
    - the vauge stress of knowing the way im doing the hashing is super fragile. 
      i don't want to actaully do eq on all the ir because that has slow vibes (even if it's actually fine). 
    - feels lame to save a copy of the functions, if im doing that i should just use normal allocator 
      instead of an arena and i like the idea of the arena. 
    - i don't like the cruch of having the frontend generate redundant code for generics and having the backend clean it up. 
      it would be better if i had another layer of ir between like real people do (rust, swift, others im sure) 
      where you do stuff before monomorphization... even if im never going to actually do that because it sounds hard. 
      even with what i have now it might be better to try to deduplicate at the ast phase before generating the ir. 
      my previous idea of trying to re-dynamic-ize generics is creepy and has the same problem of doing extra work and then more work to undo it. 
    - needing to carefully dedup types is easy to break and not something i want to write tests for.  
      and you can't give generic types readable names for the ir (which i don't do anyway but it might be a nice idea). 
    - early follow symbol aliases is still required which creeps me out, feels like it might be an ordering bug. 
    - looking at the things it deduplicates, many of them are trivial and you kinda wish i was good enough at inlining to get rid of them that way instead. 
    - really im just justifying to myself that im allowed to generate shitier code if it makes me happier about the codebase.  
- reorganized most of the tests that compile external programs
- auto run some of the harec tests with qbe_frontend.fr
- im hoping the problems i was having with blink were because of the race in my clone wrapper, so giving it another chance. 
  ... signs point to no. 

## (Aug 29)

- errno 26 is text file busy
  - tried calling fsync after write... if anything that made it worse?
  - maybe i have to fsync the directory after rename? 
    i tried not doing the temporary file + rename because there's only one person trying that file at a time in the situation that's broken. 
  - tried adding the OutputPurpose to the name so it's always unique
  - what if i just give up and loop trying to exec, that seems to work.... clearly i don't understand something...
- somehow now ive produced a situation where the -syscalls version segfaults when trying to check the tag of a CRes?
  ah the sleep after making a thread does something after all. 
  at least i understand why that one's a race. 
  do a asm wrapper just for clone that directly calls the new thread's function 
  so it can't be broken by the compiler trying to use stuff that was saved on the stack. 
  - race was because slots are accessed as an offset from rbp/fp, which isn't changed by the clone syscall, 
    so the parent had to not return until the child had called the thread function or the next thing the parent did would 
    mess up the original stack that the child's rbp was still pointing at. i even knew that was a problem before, now i can get rid of that comment. 
  - now one of the cases in @syscall can go away and the convoluted thing with xmm1 in perform_syscall can go away
- without the hack in exec, after writting the file i can read it back, assert the bytes are correct, and it still fails to exec it. 
- woah!!! maybe it just can't fucking be done: 
  - https://github.com/golang/go/issues/22315
  - https://github.com/golang/go/commit/a4b2c5efbc259c7d23159d304f9cb4266cd64643
  - https://github.com/dotnet/runtime/pull/59765
  to be fair, i did make it worse by not using CLOEXEC but fixing that just makes it reproduce less often, 
  it doesn't mean the program is actually correct without retrying on a failed exec. 
  no CLOEXEC widens the race window from just the time between fork->exec to fork->exit 
  (the whole time the child process runs) so you can see the problem more easily. 
  

## (Aug 28)

- get rid of the unfinished NEW_ADDR_FOLDING
- use the table for amd64_memargs and amd64_flag_table
- removed a `m.index != TMP(Amd64Reg.RBP) && m.index != TMP(Amd64Reg.R13);`, 
  that's not a special encoding right? i think i was confusing it with the special_base below it. 
- fixed using unnecessary SIB for RBP access: (`[rbp + riz - 0x10] vs [rbp - 0x10]`).
  compiler bytes of code: 1208987 -> 1151666 (-4%)
- spring cleaning
  - going through and deleting outdated todos
  - no longer need the faster dce in spill. 
    that case only happens 2983 times now instead of the 150k it did when i wrote it and got a speed up. 
    it's not for code size because rega does it anyway, it was just about speed. 
  - arm/isel: did immediate for sub, 3kb, might as well. 
    also i was avoiding immediate when it was SP (which abi inserts) but that's encodable, its movs that are special for SP. 
  - got rid of FixupType.Call.set_link because it's always true now
  - push/pop is always 64 bit so don't need w=1 and can elide rex more often. saves 5k. 
  - experimented with push+pop instead of mov when thats shorter, saves 20k.
    I feel like this should be a pattern the cpu knows and treats the same as mov,
    but unfortunately rosetta hates it: 1830ms vs 2195ms ?? so sure isn't worth it for me. 
    also clang uses mov even when it would be shorter as push+pop so maybe im wrong about them magically being the same speed. 
    ```
    // in emitins for copy
    if rtype(i.to) == .RTmp && t0 == .RTmp && i&.cls() == .Kl {
        encode_best_mov(e.a&, i.to.int_reg_d(), i.arg&[0].int_reg_d());
        return();
    };
    
    fn encode_best_mov(code: *List(u8), dest: X86Reg, src: X86Reg) void = {
        d, s := (@as(i64) src, @as(i64) dest);
        if d < 8 && s < 8 {
            code.encode_op_reg(PrimaryOp.PushBase, src);
            code.encode_op_reg(PrimaryOp.PopBase, dest);
            return();
        };
        code.encode_bin(PrimaryOp.MovReg, dest, src);
    }
    ```

## (Aug 27)

need to fix stack trace with #inline/#macro, it sucks that it's useless for @assert. 
- feels like it would be easy, just allow multiple source locations for an instruction. 
  but it needs to be a stack where you push an inlined thing and then all the following 
  instructions count for that until you pop it. 
  idk at what point this gets silly and i should just give up and use dwarf. 
- tried having push/pop versions of dbgloc that are used when emitting a block. 
  i feel like blocks can move around so it can't quite work trying to read it back 
  from my linear debug info encoding. 
- made the ir dump show the location info. should have done that a long time ago. 
- but also the push+pop aren't even turning out well nested in the ir. 
    - somehow the pop ends up in a dead block and fold eats it. 
      but it's supposed to be in the join after both branches of the if? 
    - fixed `-d F` dead blocks listing. i was trying to do it after removing them from the list. 
    - ok the problem is `if(bytes == 0, => return(empty()));` in alloc_raw. 
      when you do `list(temp())` the size is zero so the whole thing folds away to nothing, 
      which is great, but results in confused debug info.
      all the blocks in that function are between the push and the pop, but 
      one in the middle early returns and the rest aren't reachable so the one with the pop doesn't get emitted. 
      that's improved by just popping extra as needed when reading reading the debug info back. 
- had some confusion with counting the zero terminator entry 
  but now that i have a tag byte amnyway, use that and it seems to work now. 

## (Aug 26)

- wasm: correct abi for single scalar structs. 
  - heh, emit_ir actually does it in the frontend anyway, and none of the .ssa tests have scalar struct parc. 
    makes this whole operation a bit silly. ah ok import_c doesn't do it tho, so before 
    it wouldn't have worked calling a franca function on wasm with a scalar struct parameter. 
- for saving signeture before you have a callsite. did it's own thing that directly calls abi.sel_call
  instead of compiling a whole fake function for it. still not happy with the added repetitive code. 
- trying to make declare_alias less insane. 
  - gave up on my old idea and made push_fixup check symbol.alias and recurse until it finds the real one. 
  - added a special thing for aliases in incremental.fr to fix the printf call in import_wuffs/test.fr 
    which has to go through multiple layers of modules. that was confusing. 
  - now i want to get rid of verbose_bounce_body
    - things that use choose_inode64_on_applex8664 crash on native
      (but not AOT and not when NO_CACHE=1, just cached->jit). 
      it's an unfilled loadaddr_bits of an import (not of the alias tho). 
      it works on amd, not arm. right, it's the same problem 
      as i fix in emit_suspended_inlinables when `got_indirection_instead_of_patches && !link_libc`,
      the `name$INODE64` versions don't exist on arm so the patch is never filled,
      but it needs to be able to produce a null address and just not call it. 
      the pending GOT access is a hlt. worked before because it was getting the address of a local trampoline. 
    - now repl.fr has an unfilled call_symbol in get_info. 
      it's a #redirect on a `!=` overload (`ne__1020 -> ne__3472` is for Type). 
      declare_alias needs to only move the fixups to the target if it's pending. 
      if it's already done, it won't do fixups again, so it does them itself at the end of declare_alias. 
    - now native works but not wasm. 
      need a whole dance of syncronising thier got_lookup_offset for wasm-jit. 
    - wasm bf2ir: `add_numbers: invalid argument for op push RNull:0`. 
      doesn't like CodegenTask.FromCache
      wtf, where's it even getting a call_indirect from? 
      woah!! that was a mistake in SaveSign, need to call reset_scratch at the end. 
      - also fixed inlcalls leaking scratch, which was fine because everybody else resets at the beginning, 
        but that's why i didn't immediately notice that mistake in SaveSign, so it's more robust now perhaps. 
    - delete verbose_bounce_body

## (Aug 25)


- taking a break; porting a parser generator i did in university 
- fixed unfilled fixup for symbol_memmove when 
  `type=JitOnly && prefer_libc_memmove=false && FRANCA_NO_CACHE=1 && got_indirection_instead_of_patches=false`
  - but actually, if i just put_jit_addr that at the beginning instead of the end, it works for got_indirection_instead_of_patches as well. 
    only happens a few times in comptime code but doesn't hurt (zeroed, match_on_enum, match, default_init, emit_instructions)
  - oh hey! that cured `franca examples/mandelbrot_ui.fr -jit`, problems just go away if you ignore them!
- very beginnings of graphics/web/app.fr browser event handling

## (Aug 24) wasm

- do read_and_push and file_exists for web in a dumb way just to make c demo 
- generify printf in import_wasm/run.fr and use it in for_exports
- alias_via_bounce still happens on wasm for vararg imports because they can't have a shim. 
  - just record the alias and the table indices will work out. 
  - copy over the type index when emitting. 
- passing 82/122
- added a shitty strtod replacement. 97.
- fix a few signeture mismatches in tests.
- share implementations of well known libc stuff between import_wasm/run and web/demo
- do dynalloc properly so vla.c passes
- revert function order in env.ssa so the comments about inlining are true. idk why i changed it last time. 
- make AsmFunction do the length for you

i want threads
- did the plumbing for atomic wake/wait/fence
- i think the idea is you just make a new Worker and instantiate the same module on it. 
  it needs to import the memory and indirect table so they share, 
  but not the globals for stack pointer and env parameter. 
  oh and it has to not run the data segment initializers. 
- im a bit concerned it won't let me share tables between workers
  because the shared-everything-threads proposal (which maybe doesn't exist yet) says 
  "The lack of shared tables makes dynamic loading extremely complicated and slow, even for languages that otherwise can already use threads."
  so maybe im just fucked.
  yeah emscripten has dlsync_self which seems to be you re-dlopen everything you've ever dlopened every time you spawn a thread. 
  which is... unfortunate. 
  maybe it's kinda fine because the fast majority of the jitted code i only call on the main thread anyway.
- "Failed to execute 'decode' on 'TextDecoder': The provided ArrayBufferView value must not be shared",
  oh for fuck sake.
- and then if you want to send a sharedarraybuffer to another worker you need the stupid CORS thing. 
- you know what, no, im not doing things that dull my sparkle. 

## (Aug 23) wasm

- web demo
  - less code duplication. just shim one wasm module with all the languages. 
  - select cross target and download an exe
- only set the env global for arge if it's a different value than the functions pare. 
  1444565 -> 1393720 (-3.5%)
- convert_number_dyn
- mandelbrot.fr not working in import_wasm was a problem with my read_leb128(signed = true). 
  the stored(-1) became a storel(-4616189618054758400) 
  and that number overflowed the shift amount when adding back one bits at the top. 
- use T.eqz instruction. 1400060 -> 1394554. not worth it. 
- making more tests pass.
  - env.ssa: import labs. 46/121
  - max.ssa: to fixup.WasmAddr on the jit data address i had to bump WASM_INDEX_PADDING up to 5
    which is very sad, makes demo.wasm 2.6% larger. 59/121
  - call1.ssa: callsite wrong return type
  - queen.ssa: import calloc
  - import_c/test/ffi.fr: varargs write as long but read as int
  - most of the rest are blocked on strtod/printf
- ctz in sudoku.fr/for helps a tiny bit on the hard1 test: 1020 -> 985, and it looks prettier

## (Aug 22) wasm
 
- web demo
  - tab key in textarea. 
  - dumbest thing that could possibly make it not cache the files seperatly. 
- allow allocating table slots early
- import_wasm: implement table.get
- sprinkle around some assignments between table slots. 
  eventually i want to unify that to the same abstraction as the native GOT functions. 
- now it works in browser but not import_wasm. 
  (i get no '@' for comptime mandelbrot.fr, but comptime kelidescope.fr works which is surprising).
  oh even just aot mandelbrot.fr doesn't work for me. 
  works if you #noinline on `fn init(x: f64, y: f64) Pos` 
  but not if you disable using memory.copy which is the obvious thing i know is broken. 
- in the interim where i hadn't quite transitioned to fully using the table indices, 
  had to have a hack with temporary_alias: List(Ty(sym, sym)) and copy the got_lookup_offset at the end, 
  but i think it can go away now? for jit yes, not aot yet tho. 
  at least i can get rid of symbol.my_shim. 
- ok jit_addr being junk sometimes is a massive source of confusion, that has to go. 
- no need for backend/emit/get_addr() to be different from symbol.jit_addr. 

## (Aug 21) wasm

- rn i don't allow you to call something that doesn't have an assigned index yet. 
  which is going to have to change eventually. but for now, can't just make_exec after each shim
  to get a jit_addr because there might be something in the pipe. need to track the shim symbol
  and use that jit_addr instead when filling the patch, instead of copying the jit_addr like i can on native. 
  this sucks and needs to be unified. i should just let you give out table indices early and use it like GOT. 
- if you have a add_code_bytes that needs to be emitted before the first call to it, you don't know the type.
  same problem as i had before with DataSymbol. just now with the jitting the modules are much smaller 
  so it's reasonable to not have a callsite (tho why are we emitting it then?).
  so same hack with asking for a wrapper with a callsite of the right type, 
  but when jitting emit_ir.pending just makes shims, so have shims translate thier type_index to the callee. 
  fuck but the callee is the fake wrapper, so have create_jit_shim check if it's Redirect and save the type.
  hack hack hack, need to clean that up, but it works! now i can call println!

> mandelbrot only works in chrome. 
> `RangeError: Maximum call stack size exceeded.` in safari. also firefox. 

size of the compiler (examples/web/b.fr) wasm build with -unsafe -keep-names:
- before: 2750476 (time to comptime hello world in import_wasm: 1086)
- prepass to fold slot math: 2315530 (-15%)
- make SP global Kw instead of Kl: 2202240 (-5%)
- use fold SP+offset into load/store: 2121009 (-3.5%)
- remove trivial extuw+truncl pairs: 1973902 (-7%)
- remove truncl+extuw after cmp: 1960386 (time to comptime hello world in import_wasm: 880)

experimenting with removing the recursive collapse_op_arm64:
- just collapse_op_arm64: 909196
- just slot scan: 930040
- both: 907196
- niether: 1281632
- slot scan + add chain but no collapse: 910212
now it's something sane that doesn't have arm specific size limits so i can use it for all targets. 

back to wasm size
- use new collapse_addr on wasm: 1945008 (-0.7%)
- only do the pairs thing in simplify blit on arm: 1856773 (-4.5%). 
> at this point mandelbrot works in the other browsers. 
> so reducing number of locals helps it which is surprising. 
- fix_ceqw_not saves 4kb on arm but makes the others worse. so maybe it's better to just have fewer lines of code. 
- since im not doing opt/slots yet i should just always use memory.copy instruction for blit.
  - lol i was a bit disappointed that it only saved like 20kb but it turned out i had turned off -unsafe.  
  - doing it myself for 8 bytes is still better. 
  - 1398795 (-24%)

make more comptime work.
- sudoku: `wasm tried to call unfinished: $assign__637`. 
  don't call ensure_got_slot when making a shim. 
- kalidescope
    - `panic! missing index for $Anon__30699`
        - hack fixed by putting an extra '::' on the function literal when calling put_jit_addr
        - TODO: real fix
    - now it gets to the point of make_exec on the second layer of jit (the actual kalidescope program).
    - `wasm tried to call unfinished: $putchard`
      - also it looks suspisiously like my previous hack is giving me a junk jit_addr. 
      - put_jit_addr doesn't work on wasm becuase of the offset confusiong.
        with manually setting jit_addr i can get to 
        `[while handling a jit shim callback] Tried to call uncompiled function: jit_instantiate_module`.  
        that's just a missing do_export in b.fr, and then it works!!
        mega garbage hacks but i get my mandelbrot set! soon it will be time to pay my debts... 
- comptime qbe_frontend.fr -jit mandel works. 
  only takes like as long 3x as native, more like 2x with -unsafe. that's pretty good,
  that means the plan of recompile the compiler itself in the browser is feasable. 
  (it's super caching stuff tho so it sucks the first time). 

## (Aug 20) wasm

import_wasm
- support importing globals/tables
- make it easy to fill the imports of one module with the exports of another
- finish jit_instantiate_module

making the compiler work.
- temporary hack: forcing #inline on make_func makes it get farther
- wait_for_symbol needs to make_exec every time like i used to when i didn't rely on WX memory. 
- call_indirect if it's a numeric constant
- now the first call_dynamic_values works
- the thing jitshims are doing where they check if the pointer is ready yet and skip the compiler
  doesn't work because the indirect table don't produce observable values the way the GOT does. 
  but it doesn't matter because i haven't hooked up #libc yet anyway so for now just use a #comptime_addr, 
  and then my tiny freestanding putchar program works. 
- now on to making imports work. 
  gave it a shitty `fetch_file(str) -> cstr`. 
- macros with 1 vs 2 parameters matter now. compile_prefix_macro can't just always pass two. 
- now `get_aggregate>insert>hashmap_expand_erased` has null allocator.
  - can make tiny repro with just init_codegen. 
  - reducing this really feels the same as when i broke seal_dynamic/mem5.ssa 
    but it's like the reverse cause that was broken by opt/slot but i don't run that on wasm. 
  - leaf function was using sp directly instead of subtracting it's slot. 
    it doesn't need to save it in the global but it can't reach backward into the last guy's stack. 
  - indeed that was also the problem with make_func
- now it gets through the comptime prelude and can run tiny putchar. 
- no real programs work without jit-shims tho because of scary_exit/scary_log
- get a bit farther if i turn off the fast path in create_jit_shim

## (Aug 19) wasm

- make it clear that O.cas0 class doesn't matter
- the name$module thing isn't used anyway so it's easy to get rid of but i need a replacement eventually. 
- experiment with the compiler itself. got to the point of running call_dynamic_values which needs more invasive porting. 
- getting import_c to work
  - trying to use mutex crashes in my wasm runtime but not browser.
    i don't like that the addresses are unaligned. that's suspicious. 
    yep, making sure Exports.env.mmap returns page aligned addresses as it should makes it match browser behaviour. 
  - now memory corruption stuff that's fixed by not resetting arenas, lets just see how far we get with that hack. 
  - last thing was making sure data address is the comptime one instead of local offset in segment. 
    now c hello world works jitted in wasm

## (Aug 18) wasm 

- fixed "emit_insertions for loadopt doesn't compile on wasm. `bad nesting. expected to end @39 but found @6`". 
  problem was a loop that had to wait before being ended. 
  needed a `<=` instead of `==` when comparing block id to top of loop_stack. 
- now kalidescope gets to the part where we need to actually jit. 
  and like maybe this will just work, make a new module that imports memory,table,etc 
  have an offset to where it starts putting its own functions in the table, 
  the host instantiates it with the old module as the imports and then you can access the functions in the table. 
- works! but sure looks like safari loses a few dots. 
- implemented more table stuff in import_wasm. 
- made import_wasm/convert.fr/comptime_exports() not use inline_for so it should compile a bit faster. 
- no shim for varargs imported for .frc (fixes import_wuffs demo)
- put the demo in a web worker

## (Aug 17) wasm

trying to get dump_wasm to work in wasm
- mega confusion caused by fixup_wasm32()/WasmAddr doing leb128_unsigned() instead of leb128_signed(). 
  (manifested as function pointer table indices having negative values).
- it's so ass that Uint8Array.set(ArrayBuffer) silently does nothing and Uint8Array.set(new Uint8Array(ArrayBuffer)) works
- works now with some load bearing debug prints 
- it's sad that if you throw an exception you don't get a stack trace that shows the wasm functions
  but if you hit the unreachable instruction you do. 
  
make it work with the normal runtime
- finish_alias: add to exports list
- franca_runtime_init references libc_allocator and `__libc_start_main` are a pain because there's no weak imports
- finish the plumbing for AsmFunction

ditto kalidescope. i want to get it to the point of crashing because you can't jit. 
- need .frc to work a bit for the generated module in opt/fold.fr
  - convert int neg to sub
  - store ret_cls in FncFlags
- insert a extuw for i64.extend(16/8)_s (same reason as for cmp)
- just disable loadopt for now 
- TODO: still getting junk vptr indices

why am i not doing this with import_wasm as well...?
- implemented fallthrough return
- extremely reassuring that the fragile order dependence with the indices can be reproduced in import_wasm. 
- silly mistake. i was having SP point to the old stackbase instead of the new one. 
  manifested as stack variables getting stomped when you made a call.

how hard can it be to put 1.4MB of text on the screen?
- `<pre>.innerText+=str` is super slow. forced reflow is the whole time in the profiler. running it once takes ~20ms and twice takes 600ms.
- `overflow:hidden` didn't help and isn't what i want anyway. `white-space: nowrap;` gets it under 500ms. 
- interestingly innerhtml is instant but then you don't get new lines.
- textarea.value is 100ms for twice and 10ms for once. which is still not at all linear but at least im not going to wither away while i wait. 
- now all the forced reflow is at the end so really the timing is cheating and it's still an extra 250ms. 
  but even of the fake 100ms, there are 7 calls to js to show the text, about 10ms each. that's still pretty high overhead.
- for referece, running `./target/w.out a.wasm` in terminal.fr is 130ms (that's for twice) but it's about the same for once, 
  so we're learning v8 has a much faster jit than i do which makes sense. 
  i wonder if they cheat by caching the compiled wasm. 
- `white-space: pre` works on safari. much faster than chrome surprisingly. the lie number is 30ms for twice. 

## (Aug 16)

- easy uses of local.tee 
- make frontends explicitly say the return Cls on functions instead of just picking one from the first ret encountered.
  - because you need to be able to have a function that always ends with a hlt but still has a signeture like it returns something. 
    like you're allowed to export `foo :: fn() i64 = unreachable();` and wasm type checker cares about the return type 
    but previously there was no way to tell the backend what you wanted. added a .ssa test for that. 
  - add debug checking for that in fails_typecheck()
  - that makes it easier to do all the par stuff together in wasm_abi 
    instead of spreading it out (no longer need to see all jmps before knowing signeture)
  - updated example programs to set f.ret_cls
  - made import_c not have inconsistant ret types when a function has an unreachable fall through return
- make native_abi() pass slices instead of begin/end pointers. 
- getting import_c closer to compiling harec
  - check.c:1489:25: support multiple nested dot fields in struct initializer
  - identifier.c:70:29: static_assert
  - more stuff in include.fr 
  - i really don't want to paste a sys/stat.h for include.fr,
    because that's one that really sucks to make work on multiple targets.
    - i'll make an attempt at using system headers. want to know how far i get. 
      - implement `asm` symbol alias
      - unistd.h i get "unterminated conditional directive", 
        definitly my bug but it goes away if i ask for an older version of posix so maybe lets just see what happens. 
      - `__darwin_arm_neon_state64` uses `__uint128_t`, which i can just `-D` away if i don't care about that for now. 
      - if i change my va_list to *void (which breaks the .ssa test drivers) then:
      - got to this point which does work but like, ugh, it doesn't spark joy. 
        `franca examples/import_c/cc.fr target/harec/src/util.c -Itarget/harec/include -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include -D__uint128_t=long -c -D__asm=asm  -D_POSIX_C_SOURCE=200809L -r -D__arm64__`
      - adding `-no_builtin_headers` doesn't have stdarg.h without ` -I/opt/homebrew/Cellar/llvm/19.1.7_1/lib/clang/19/include`
        and clang's stdarg.h needs `__has_include_next`
    - maybe its time to finally do the cleanup work to export the franca libc bindings as something import_c can use.
- keep location info when replacing idents in preprocess.fr/eval_const_expr()

## (Aug 15)

wasm improvements
- `__franca_base_address` can't work because the code isn't in linear memory
- alias_via_bounce doesn't work. either need to generate a body that does the fake call 
  or do real aliasing where before referencing the symbol you check if it should be replaced by something else. 
- relatedly, if it's an import and all the calls are indirect (like they will be for the libc syscall imports),
  it won't be able to infer the type index. 
- both of those are improved by bringing back the version of bounce_body that emits all the pars and a real call. 
  - but that doesn't help when it only goes through DataSymbol without ever getting a FuncId (which the syscall wrappers do). 
    that's a recurring design flaw. 
  - for now just make yet another fake Func when emitting a Expr::DataSymbol (same as aot_bounce but never actually called). 
  - eventually that needs to be cleaned up. either make the frontend treat symbols more consistantly 
    or convert DataSymbol with function type to FuncId in sema, but that wouldn't work for objc/varargs. 
- add_code_bytes needs to add it to the function table
  (nevermind that it won't actually work yet because AsmFunction always passes it an empty slice for wasm). 
- replace that empty slice with a valid function body that just crashes. 
- sel_call needs to save the signeture in case it's from add_code_bytes which won't have pars to get it from. 
- more type errors
  - shift amount is same as argument type but i always allow .Kw. -5
  - push(k) for ext(ub,uh). -2. and don't do an extra extuw if it's already a Kl. -5
  - wasm_push deal with Kl->Kw as well. -8. and then the small ext doesn't need a special case extuw. 
  - confusion about if indirect return parameter is Kl or Kw based on where we learned the signeture. -3
    - `[T11]: (I32,I64,I64,I64,I64,I64,) -> ()`  -- dealloc_raw__339 indirect calls this
    - `[T23]: (I64,I64,I64,I64,I64,I64,) -> ()`  -- panicking_allocator_fn__2867 is this
- sadly the type erased syscall wrappers aren't going to work. 
  they assume you can just always pass 6x i64 and fill the extras with zeros.
- assert all inferred signetures for a symbol match in wasm/abi.fr and fix a few tests

## (Aug 14)

wasm improvements
- add extra par for vaargs if the function is marked variadic instead of only if it uses vastart instruction. 
- export the memory so the host can access it easily 
- implement cas in emit and import_wasm
- add .ssa tests for cas and unused vararg. 

## (Aug 13)

trying to decide if i should steal qbe's gvn/gcm. 
benchmark different backends on an old version of the compiler that could do llvm-ir and qbe-ir. 

- commits used in tests: 
  - old qbe: 9e36cbe4d8f8c9ff3d739ff9ead2a4a4988c0904
  - new qbe: 120f316162879b6165deba77815cd4193fb2fb59
  - old franca: 6ff69bc8de8226faeb8d2cd97619148cd664d25b
- change QBE_PATH in default_driver, then build a compiler, then use that on first.fr because that prints more useful timing. 
  - boot: `mkdir -p ./target/franca/aarch64-apple-darwin && ./boot/aarch64-apple-darwin/franca2 compiler/first.fr -replace-compiler -unsafe`
  - `./target/franca/aarch64-apple-darwin/franca2 examples/default_driver.fr run compiler/other_main.fr -aot=qbe -unsafe`
  - `./a.out compiler/first.fr -no-build -unsafe`
- for using qbe_frontend.fr, the old build script doesn't deal with it outputting object files instead of text. so the hack is: 
  - first do a compile with normal qbe 
  - extra.s: snip out the #asm frunctions from the bottom of target/franca_qbe_temp.s, put them in a different file, add `.globl <name>` before each
  - mine.o: `/Users/luke/Documents/mods/infered/target/q.out <tmp .ssa file path> -o mine.o -c`
  - mine.out: `clang mine.o extra.s -o mine.out`
- i added -O2 to CFLAGS of Makefile for all the qbes so for the codegen time measurements they get llvm's optimisation whereas mine doesn't. 
- for llvm-O2, the build script doesn't give you an easy way so manually pass the temp ir file to clang. 

| who | codegen time (ms) | binary size (bytes) | frontend time (ms) | 
| ----------------|------|---------|------|
| llvm -O2        | 6048 |  763304 |  694 |
| llvm -Os        | 5000 |  664440 |  737 |
| qbe_frontend.fr |  468 | 1057000 |  945 |
| qbe new         |  932 | 2625736 | 2670 |
| qbe old         |  905 | 2609256 | 2700 |
| llvm -O0        | 1075 | 1561000 | 6853 |

- in the table, codegen time is how long the backend took (to process the text file of ir). 
  frontend time is how long the resulting binary took to generate ir for a new compiler as reported by first.fr. 
- note also that real qbe outputs assembly text not machine code like mine and llvm, 
  so you so you have to run an assembler at the end too. which is an extra 4588ms for the 9426735 byte .s file 
  with `Homebrew clang version 19.1.7 Target: arm64-apple-darwin24.4.0`. 
- i wonder if qbe is unfairly punished by not having inlining. 
  i tried to use shared/2024-09-03-gcm9-ifconv-inlinev2 (c7dd98a51855e7d9c1d746dc2585134dec76ea3b) 
  but it doesn't work (doesn't handle RType in newinlref, fixed that and then assertion failed file reassoc.c, line 89). 
  don't care that much so we'll never know. 
- so like idk that experiment sure makes mine look fucking amazing, im not entirely sure how i could be cheating. 
  but doing all the steps manually like this is not super reproducible. 
  so maybe i should automatically test them to make sure i don't regress / check if they add anything i should steal. 

---

- removed dead sync code in wait_for_symbol since comptime_codegen is single threaded
- split worker.fr into the backend part and the frontend part so other programs using the backend don't have to awkwardly import something from the compiler. 
- quest for the bug
- rename alloc() to alloc_uninit so the more dangerous one doesn't have a nicer name. tedious! 
- make sure the lambas in `a || b` and `while => a {` get flagged as WasLambdaLiteral
- got rid of O.assert. easy to bring it back if i actually want to use it for something. 
- move trace_(start, prev, return) out of the backend to just be asm in the library. 
  similar reasoning to syscalls: the infrastructure required to use them to print useful stack traces is so much 
  that there's no point in providing the instruction when you can just do it yourself with barely any extra work. 
  and the speed hit of not inlining trace_start which you use exactly once when you're already crashing super doesn't matter. 
- strace says the arm-syscalls crash is always shortly after calling clone(). 
  i assume it's a race because it happens much less frequently when strace is spamming stuff. 

## (Aug 12)

- meta/qbe_frontend.fr support replacing qbe for hare
  - meta/parse.fr 
    - ignore section and dbgfile statements
    - harec likes to output opaque types that it never uses and have junk sizes? 
      so allow size of opaque type to be -1 and just hope you don't try to use it i guess.
    - handle ub/sb/uh/sh ret/arg/par
      - abi.fr/extsb_parargret: actually replace the op with normal arg since i changed the abi impls to disallow small ones
    - allow s/d in data definitions. it's still just bits so it means the same thing as w/l. 
    - similar to the opaque thing. it wants to make a type for an array of 500000000000000 bytes and never use it. 
  - annoying. can't just skip the line when you see `section` keyword, 
    harec wants the word `export` on that line and i need that or i don't find your main function 
    and you get `ld: warning: cannot find entry symbol _start; defaulting to 0000000008000000`. 
    and `undefined reference to main` if i try to just link them myself with clang. 
    fair enough qbe docs do say the new line is optional. 
  - progress, now if i link manually it runs and gets to main(). 
    if i change config.mk to ask for clang as linker instead of ld it stops complaining about `_start` and just gives me one,
    but it dies in call_weak_fn (which i think it gave me), that seems like a dead end perhaps. 
  - oh ok my hacky way of just not calling the assembler in their makefile (because i output machine code not text, 
    i just compile` rt.ssa -> .cache/rt.s` but it's actually an object file and then say my assembler command is `cp .cache/rt.s .cache/rt.o`) 
    means im ignoring their things that are actually written in assembly like `rt/+linux/start+aarch64` which has `_init`. 
  - ah fuck now that im looking at the code, some of the sections do matter `init_array, fini_array, test_array` because that's what their linker script knows about. 
    im really not a big fan of the (house of cards / everybody's just a small piece of a bigger tool chain) compilation model. 
    maybe what i want is to write my own version of their base runtime stuff: https://harelang.org/documentation/usage/freestanding.html  
    lucky for me the tests in the harec repo don't use @test and 09-funcs.ha is the only one that uses @init/@fini, so it's mostly fine. 
  - this seems to work: `clang rt/+linux/start+aarch64.s rt/+linux/syscall+aarch64.s  .cache/rt.o .cache/tests_00_literals.o -nostartfiles`. 
    yeah, shoving that im config.mk i get `38 tests:	34 passed	4 failed`. 
    ```
    08-slices            ...Abort: tests/08-slices.ha:312:15: assertion failed
    09-funcs             ...Segmentation fault
    11-globals           ...Abort: tests/11-globals.ha:84:15: assertion failed    
    12-loops             ...Segmentation fault
    ```
    - oof, i was relying on my emit_ir always using RInt so if you use the text format, 
      they'd be RCon and isel was diligently loading the line numbers into registers for you. 
      (unrelated to the fails, just made the asm fucking impossible to read) 
    - i wasn't skipping spaces when parsing constant offest so `data $sl = { l $sldata.969 + 0, l 3, l 3 }` 
      was being parsed as `data $sl = { l $sldata.969+0 0 3 3 }`. that fixes 08 and 11. 
    - the problem with 12-loops is im trying to call memmove for the blit of a large return. it works if i set prefer_libc_memmove=false. 
      probably the -nostartfiles kills it? yeah `file` says it's `statically linked` so the import won't work. 
      maybe this is my sign that should be the default because adding imports is creepy. costs 10ms on self compile to do that tho. 
    - last one: 09-funcs. which isn't going to work because of initfini but it should be an assertion fail instead of a segfault. 

## (Aug 11)

- removing outdated todos in old tests. 
  - found a few that weren't running because of forgotten #test. oops. they all pass tho. 
  - it's very cute to see comments with `assert _eq` with a space 
    because i used to count that the builtin was called the same number of times 
    it appeared lexically in the source string. 
  - one was commented out that used to be a sema restruction but is now an emit_ir bug
- added an auto test that checks repro between native and blink. 
  also one that runs the a.ssa hello world in libriscv. 
- change the values in arm64/emit.fr `brk(0x____)` to be 4 digits instead of 3
  so you can always grep for the disassembly you see in lldb (which doesn't add a leading zero). 
- franca-linux-arm64-sta examples/toy/hello.fr (jit) crashes in the write syscall wrapper. 
  - works if you always use the unerased version in @S (but i want to only do that for clone-like ones). 
  - also works if you always take the got_indirection_instead_of_patches case in loadaddr_bits
  - works if you don't set `vtable.libc.it = f;`, but not if you just delete the case that actually reads from it. 
  - oh im dumb. the problem is since it's static, the weak symbol is never filled so the patch there is ignored. 
    but even tho there's no value for the symbol, the brk instructions need to be replaced with loading zero in the register. 
    it works on amd because there i just output an lea and patch the offset instead of patching the whole instruction. 
    also force link_libc=false when jitting and we're built with -syscalls. 
- wasted some time trying to play with callgrind. the strategy of run it on orb and view the result in qchachegrind on macos
  can't even show the assembly (--dump-instr=yes) because it tries to exec apple's objdump on the elf file. 
  with --collect-jumps=yes it just crashes before even telling you objdump doesn't work. sad day. 
- after running actions on all platforms, join, download the artifacts and diff them to make sure repro wasn't broken. 
  run linux-arm tests in actions as well. 
- make all .ssa tests pass wasm type checker
  - just moving the need_trunc into wasm_push makes -verify fails on .ssa tests go from 11 to 3
  - call2.ssa was just wrong cls at callsite. `=w` -> `=l`
  - for good luck, don't try_kill_inst sel0
  - abi9.ssa is specifically about lying about signatures so don't run it on wasm
  - lol just did an oops when i changed wasm_push rn and didn't return so pushed twice and mucked up the stack
  - for test.fr -verify, exec wasm-validate instead of wasm2wat. both are part of wabt and have the same effect here. might as well use the smaller one. 

## (Aug 10)

mystery garbage waste of time. 
- new claim, main_thread_pump is miscompiled and small changes to it's stack frame size make it work. 
  - like -unsafe or #inline on BitSet.set or adding `big := @uninitialized Array(i64, 10);spill :: fn(s: []i64) void #noinline = ();spill(big&.items());`
  - `franca examples/default_driver.fr build compiler/main.fr -o q.out && ./q.out examples/default_driver.fr build compiler/main.fr -os linux -arch x86_64 -o qq.out -keep-names -syscalls && orb FRANCA_BACKTRACE=1 ./qq.out examples/default_driver.fr build compiler/main.fr`
  - BitSet.Big.maybe_uninit.ptr is getting garbage at some point there. 
  - disabling elide slots or promote or coalesce makes it work but does that mean they're wrong 
    or that they just change the stack size enough to confuse the universe somehow? 
  - it happens in the call to mainthreadpump done by the static_memmove constant at comptime so turning that off fixes it but that's just hiding the problem presumably. 
  - emit_ir.alloca always using .alloc8 instead of choosing based on info.align_bytes fixes it
    but so does always using .alloc4 because alignment isn't real and it just fucks with the stack a bit i guess. ugh. 
  - not using -syscalls fixes it which is creepy because that can't change the stack size of that function, it's not a constant. 
    it just changes what happens in the syscall wrappers. 
    i would accept that if it's just oh too much stack use makes me stack overflow 
    the amount of space the kernel gives the main thread 
    but things that fix it include causing it to use even more stack so that can't be it? 
    and indeed, wrapping the compiler's main in a start_thread(...).join() where i get to allocate my own giant stack doesn't fix it. 
  - but using slowuserspacethreads does fix it. 
  - having the worker not use a seperate thread in emit_qbe_included when `fns[0]` name.startswith("copy_bytes") makes it work, 
    but the inverse does not make it work. 
  - maybe im doing sysv abi wrong? that would make sense with pervious odd behaviour of to_bytes(Incremental.Meta). 
  - you know what, fuck it, im just going to get rid of BitSet and use DynamicBitSet instead 
    and if the universe wants the real problem to be fixed it will resurface on its own. 

## (Aug 9)

- it's gotta be a problem with sharing temp() between threads by accident. 
  if i just compile itself macos-amd64 3 times it crashes but works if -keep-names 
  and works if i @tfmt the long name in fmt_fn_name and discard it. 
  (that doesn't fix the actual tests of course but it demonstraits that it's a safety problem). 
- new lore: back trace locations are broken for amd64. 
  because i was doing an align_to(4) in add_code_bytes but on amd64 instructions aren't always 4 bytes
  and i wasn't including the added padding in the offset for finish_debug. 
- new debug mode that leaves a linked list of canaries between all the allocations in an arena 
  and then walks back when you reset it. so detects if you try to reset twice 
  or if you wrote directly past the end of an allocation before resetting. 
- The driver program inherits a env.temporary_allocator that was initialized by the compiler 
  but uses it's one copy of the functions for the vtable. so you need to skip the checks until 
  you've compiled a version of the compiler that can set up the canaries. 
  just make env.temporary_allocator an Alloc instead of an ArenaAlloc so it has the vtable 
  and then this problem goes away. and i think then you could use the @print functions in arena_alloc() 
  because it avoids the comptime bootstrapping problem (tho actually no, it will still just recurse forever when you try to run it). 
  did that and for compiler/worker/codegenentry for good luck
- concerning that i seem to have made my problems go away...? neither of the amd64 ones segfault in the tests any more. 
  im a bit afraid that ./boot/temporary/linux-amd64.sh might hang forever at the end when building the releases if you don't rm franca/cache before. 
  maybe that's the same just displaying differently... im really not very good at this apparently. 
- repro between macos and linux 
  - one problem was the line `r[i] = (id = m.intern(names[i]), off = f[].byte_offset.trunc());`,
    the `off` computation wasn't happening at comptime and there was some other junk in the `Field` (struct padding perhaps). 
  - now the only difference is the bytes of the static_memmove module. 
    the last 4 bytes of Meta.all_deps are not 0 on linux, it's a default struct field: `all_deps := zeroed Sha256.Digest;`,
    if i force zero it in to_bytes, seems to fix it. 
    TODO: whats going on with that. that's suspisously in the same part of the compiler that i know to be broken (since it hangs if not rm cache). 
  - struct padding in blit_op_table
- get rid of the extra extension instruction for `f :: fn(a: *u8) i64 = { b := a[]; b.int() };`
- made is_linking_libc not #fold (stores the comptime value in a static)
  so now the only difference in -frc_inlinable with -syscalls is one byte. 
  had to mark more things as #weak. 
- got rid of query_context_is_implicit()

## (Aug 8)

- allow zero parameter syscall wrappers
- made the list retain functions work on slices so it's easier for rawlist to call 
- parallel backend/meta/test. after: 43 seconds. 
- leak police

## (Aug 7)
  
- continuing how'd i break linux
  - seems it's yet another problem with this:
    ````
    if o == .copy && !p&.is(.Scalar) {
        return(Ok = self.scalar_result(p, arg[0], expr_ty));
        // if it's .Scalar that might be used as an assignable var so can't skip making a new ref for it. (happened with write_leb() test)
    };
    ```
    skipping that instruction saves 4k ir ops but not even convincingly faster so not worth it if it's this error prone. 
  - nope, without that, same rawfromptr problem with `COMPILE ONLY: examples/turing_art.fr`,
    but only if i run compiler/test directly after recompiling, (`franca examples/default_driver.fr build compiler/main.fr -o q.out -unsafe -keep-names -os linux -arch x86_64 && orb ./q.out compiler/test.fr`), 
    it works if i run through `./boot/temporary/linux-amd64.sh`,
    so maybe it's a multi-generational fuckup because the base compiler im using was compiled the old way. 
  - mmmm caching problem. direct run works if i `rm -rf target/franca/cache/` and then fails the next run.  
    works with FRANCA_NO_CACHE=1.
    works if i `rm target/franca/cache/compiler_test_fr.frc` and then fails the second run. 
    works if i directly run that cache file but not if i point it at the source file and it chooses to use cache. 
  - also the symbol it's crashing in is called copy_bytes but it's not `__franca_builtin_static_memmove`
    because i am actually linking libc. either i was wrong before or ive confirmed this is a different problem with identical symptoms. 
  - so like no idea whats going on at this point but clearly ive just uncovered a problem that already existed. 
    i think the strat is just commit to getting rit of unsafenoopcast cause it's weird and just say hey now i have some bugs and slowness to fix. 
- give up for now, just make the tests run faster. 
  - multithreading the test runner seems to have induced that problem on macos-amd64 but without the relationship to caching. 
    which i guess is progress to reproducing it sanely. 
  - sadly i think the lua tests can't run in parallel. 
  - sad that it makes the code uglier (since i don't have real closures 
    so you have to pass around a state struct to run it in another thread)
  - run import_c tests in import_wasm/run.fr as well
  - before: 68 seconds, after: 50 seconds. 

## (Aug 6)

- compiler doesn't use float min/max so i don't need to support those before i had the intrinsic. 
- some fn int() can just be #ir instead of having an awkward body
- would be nice to get rid of #unsafe_noop_cast. going through an replacing it with #ir(.copy, k). 
  - turns out it wasn't even type checking it's arguments all the way so all the more reason it has to go. 
    always surprising when there's something like that and i only made a mistake with it 4 times and they were all fine anyway. 
  - creepily it generates less code but takes longer
```
  before: 885156 bytes of code, ir_ops = 422723, 930.2 ms ±   3.6 ms
  after: 878076 bytes of code, ir_ops = 426789, 946.0 ms ±   2.8 ms
```
    before building after is 941.4 ms ±   2.5 ms so most of the regression is that the new code takes longer 
    to compiler not that it runs slower. maybe it's just cause it's doing more typechecking now? 
    eh, if i fix typechecking of #unsafe_noop_cast in the old version it's still fast so that's not it. 
  - the new one is more ir_ops (and even more, 430k, if i don't add a thing in emit_ir/emit_intrinsic_new to skip easy copies). 
    i guess i can understand why it's more ops because #unsafe_noop_cast just becomes a Cast node 
    in sema and emit_ir just recurses on it and generates no code, 
    so then how can the new thing possibly generate less bytes of code?
    - the difference is NOT just a missing `func.set_flag(.Intrinsic);` changing compliation order. 
      can add that for #unsafe_noop_cast in the old one and it's still big. 
    - RawHashMap.insert is one that changes. 
      difference is whether assigning the second element of `(result._0, Entry.ptr_from_raw(result._1), result._2)`
      becomes load+store (new) or blit (old). 
    - really the backend shouldn't care if it's a blit or a small load+store. 
      maybe i should just expand blits earlier. 
    - special case for scalars in emit_ir/Set/Deref to make it produce the value instead of doing it via .NewMem. 
      gets it to `879060 bytes of code, ir_ops = 420028, 943.0 ms ±   2.8 ms`
      - (tried the same thing for emit_ir/Set/GetVar but not impressive, 
        that only happens 305 times compared to 4166 for the Deref case). 
      - i saw it being `936.8 ms ±   2.5 ms` with just ptr_from_raw changed back to unsafenoopcast so that's interesting. 
    - enum.raw() is the last one to change. doing it with bit_cast_unchecked makes it much much worse
      and doing it with @as(RAW) is a bit better but not much 
      ```
      fn raw(e: T) RAW #unsafe_noop_cast; // A 949 ms
      fn raw(e: T) RAW = bit_cast_unchecked(T, RAW, e);  // B 966 ms
      fn raw(e: T) RAW #inline = @as(RAW) e; // C 957 ms
      fn raw(e: T) RAW = @as(RAW) e; // 953 ms
      ```
      so i feel like that must be a compliation order thing preventing an important inline. 
      that would explain B being worse but i feel like C should be the same as A. 
      but a simple test that uses both shows both are inlined on the first call even without the #inline. 
      and B/C generate the same shape of code. so maybe the improvement there is just that @as is faster to 
      compile than a call to something with const args. that's compelling. 
    - it's really hard to pin down because the difference from any one thing is to little to measure reliably 
      but together i've lost a lot of speed.  
- that seems to have broke compiler/test.fr/run_and_check() on linux somehow. 
  but if i try it on the last commit that worked in actions it also seems broken so who even knows anymore. 
  - ok so as a seperate problem unpacking a dep for examples/prospero.fr is fucked on orb so i bamboozled myself when i tried to check an old commit. 
  - builtin_static_memmove only works if raw_from_ptr is #unsafe_noop_cast,
    but the static_memmove constant has the same bytes either way so the actual problem is elsewhere? 
      - tho as a seperate thing, i don't like that those bytes change depending on the rest of your compilation context. 
        (it's just the mangled name changes because it has a funcid in it so it's not a repro problem, it's just dumb). 

## (Aug 5)

- last thing i needed to get the hello-wuffs-c example to work was just update assignment (like a += 1). 
  so now that program works which is progress. just need to un-stupid emit so it makes sense again. 
  the main problem is confusion about when things are a pointer vs a value. 
- let import_c #include a .frc file so i don't have to type out a vtable to import the wuffs functions. 
  does feel a bit like waste of time factory but it didn't suck as mush as i thought it would. 
  started with just function types and some builtins which works because import_c doesn't type check pointers. 
  so the current state is pretty error prone but at least it checks arity which is better than my previous hack. 
- generate FTy of the right size for wuffs structs so you can have them as a local easily 
  and let import_c import that. 
- emit sizeof_foo and foo_initializer functions
- YIKES. in view_image.fr:
  `doing it by value doesn't work. you get ptr=0, len=6090728512; im pretty sure i checked it's using the version of to_values that makes a copy. -- Jun 9, 2025`
  turns out the to_values that takes an allocator and carefully copies into new memory does not in fact return that memory. 
  - i guess that's the only place i tried to call vtable.add_to_scope with something bigger than a pointer. 
    and in the compiler itself i mostly use to_values (with an s) which did the right thing already. 
    one wonders why i have both. both are from the early days of porting the compiler from rust 
    so maybe one of them is what it was called in the old codebase and i re-ported it without noticing that i had one already? 

## (Aug 4)

just hacking enough stuff together that i can run an example program. 
- a dumb definition for read_u8 and an intrinsic for slice.length()
- simple frc signetures so it can be imported
- even more hacking to get it to run together with some c code
  - pain with casting between the function pointer types. 
    to cheat need to go through a rawptr to make sure not to get the funcid type by mistake. 
    (won't be a problem once i make dependencies between import_frc work)
  - make sure padding in the struct for yield state 
    (won't be a problem once it's genrated by import_wuffs)

## (Aug 3)

generating ir from wuffs. 
- most is the same as any other language, if/while, operators. 
- parameters to a function are accessed with `args.name`
- starting to think about how to lower yielding 
- yikes found a compiler bug where it doesn't type check something. 
- while reducing that, found a big win for debug builds (ie without -unsafe). 
  was recompiling a @print for each match to report the value when you had a corrupted tag. 
  (both numbers are release compiler building the debug compiler, after warmup run): 
  - before: `1272764 bytes of code, 85434 bytes of data. [CPU Time] frontend: 1102ms, codegen: 721ms. ir_ops = 614960`
  - after:  `1139680 bytes of code, 85434 bytes of data. [CPU Time] frontend: 1062ms, codegen: 631ms. ir_ops = 564633`
  not super exciting because it doesn't affect release builds but all comptime code is debug so that's nice. 
- track types, calculate field offsets, loop targets

## (Aug 2)

more wuffs

- std/png/decode_filter_arm_neon.wuffs. 36.
   iterate is kinda complicated. it's mostly used for the simd stuff but not only so i can't just skip it. 
   can have multiple loop variables and an else clause.  
- yay! now it gets through everything 
- start printing out the ast. makes sure this way of storing it doesn't suck too much to traverse 
- show error location

---

> did i really not turn off auto update after i bitched about it so much last time ??   
> no did, i see it in the fucking settings... i feel that does nothing....  
> now i need to ask for `"git": { "git_gutter": "tracked_files"`  
> i also feel like they changed when the horizontal scroll bar shows but im not entirely sure.  
> i really think it used to be only when something went off the screen. 
> you can tell it's a bug because import_wuffs/parse.fr has the bar but if i paste it into a new file it doesn't. 
> https://youtu.be/fE2bqOM8DsU

## (Aug 1)

bunch of work on wuffs parser. slowly working through decode_lzw.wuffs. 
top level declarations, calls, ops, flow stmts, etc. until i got bored and 
had to start keeping track to feel a sense of progress. 

- 259. sad to have to special case io_forget_history for one usage. 
- 385. ending label after while loop. 
- 456. invarients in while expression. 
- 503. assert
- 582. choose. i don't super understand what this is for but im sure i'll find out. 
- 641. %
- 805. roarray
- xz/decode_filter. 12. choosy. 
- 82. more indexing.
- 237. double braced blocks. what do those mean? just for the formatter to ignore it? 
- gif/decode_lzw. 89. nptr
- decode_gif. 987. io_bind
- decode_nie. 358. assert doesn't always need a `via`
- decode_png. 204. 'literals don't always need be/le
- io_limit

## (Jul 31)

- profiling
  - i tried someones macos version of zig's profiler thingy https://github.com/verte-zerg/poop/tree/kperf-macos
    i don't know if i believe it. the branch_misses is way too low. but if i tweak examples/toy/predict.fr 
    to only run one case at a time, the slow one does have twice the number of branch_misses, 
    so i guess it's just sampled. that's fair. 
  - xcode's hardware counters thing is actually much more interesting than i gave it credit for 
    because it gives you the profile graph by symbol when you ask it for counters the same as 
    when you ask for time but the percentages are wildly different. 
      - there's this that tells you what the names mean: https://github.com/jiegec/apple-pmu/blob/master/a8.md
      - resolve_in_overload_set_new is 25% of the time but 60% of the 
        BRANCH_COND_MISPRED_NONSPEC, L1D_CACHE_MISS_ST_NONSPEC, L1D_CACHE_MISS_LD_NONSPEC 
- don't want to deal with fixing bugs. taking a break and trying to write a wuffs compiler 
  because i feel like thier thing is a good idea. experimenting with doing an ast more 
  like the zig people do because they're so excited about it, maybe they're right. 

## (Jul 30)

- deduplicate in new_type so you can deduplicate generics with same repr RType. 
  893488->890616, meh. just to make sure, it did reduce the number of types: before: T619, after: T183. 
  - PageMap.find_entry still doesn't get deduplicated because of PageMap.create_entry and ptr_diff. 
    - ptr_diff is wrong because it's processed too late
    - create_entry is wrong because it is recursive so it's constants don't match because both 
      are calling a different themself but if they deduplicated so self was the same, it would match. 
  - wasted a lot of time on examples/view_image.fr failing because backend/incremental/read_types() 
    would deduplicate against the pre-existing types in the destination module. 
- tried to take advantage of that in call_dynamic_values to use a Typ index as a key but it got 
  more complicated than seemed reasonable (have to include FnType.unary somehow) and didn't work so gave up. 
- lock more to make `-d t` work better but still not actually fixed? 
- added a bake() for Mutex to make sure you don't get stuck with a locked on in your exe if you had threads at comptime. 
- can reproduce mutex problem pretty quickly running `franca examples/import_c/test/test.fr` in a loop.
  - symptoms:
    - "tried to unlock someone else's mutex" in acquire()
    - "tried to lock an already locked mutex" in release()
  - with FRANCA_NO_CACHE you crash in init_codegen_worker all the time. 
    so i think that's a different problem. 
  - does NOT happen if you compile to an exe or if you compile to .frc explicitly and compile that to an exe. 
    and it's not just the first time that fails when running a franca file so it DOES happen without the frontend running at all. 
  - so problem happens when the compiler calls main() without franca_runtime_init. 
    that makes sense, env.thread_index will be 1 but the callee's NEXT_THREAD_ID will still be 1 
    so when it spawns a thread, they're not unique and everything dies. 
  - seems fixed. used to crash in under 10 seconds, now i can run the loop for 6 minutes and it's fine. 
    so that one worked in SLOW_USERSPACE_THREADS because that was correctly inherited. 
- problems remain! import_c/tests/wuffs.fr fails run sometimes in actions (maybe only on amd?). 
  - didn't reproduce on arm in 84 runs, 8 minutes or on rosetta 84 runs, 17 minutes. 
    so that's not super actionable... 
- trying to debloat syscall stuff. 
  - less `if err != nill` junk
  - don't generate a fix_flags for each. just iterate an array of the flags to remap. 
  - outline more of the syscall wrapper logic instead of generating it for each. 
    instead of 91 instructions per syscall wrapper it's 24 (which is still too many). 
    (and not doing clone() yet cause it sucks). 
  - old: `891880 bytes of code, 69282 bytes of data.`  
    new: `883920 bytes of code, 70162 bytes of data.`
  - when doing a DataSymbol it doesn't have a FuncId to put in the pagemap 
    and you get a "can't have a void pointer as a constant" 
      - you can hack around that with adistinctionwithoutadifference 
        but then specifically examples/bf/bf2ir.fr doesn't work?? 
        what? there's no syscall it makes that others don't. 
      - it doesn't like that it calls munmap at comptime. 
        ok i can reduce it to something simple that uses DataSymbol 
        and works if you call it at runtime but not comptime. 
        so that's reassuring, not a scary thing with the syscalls specifically. 
        problem is just that if it runs at comptime, the static has the pointer saved and it tries to bake it. 
- (this was broken before sys debloating as well): 
  static linux dies in push_uninit but only when `-unsafe`. 
  git bisect says ec1cd8573db965d71aeaf5f998efa59029374e0d is the first bad commit, 
  there it works on arm but not amd but currently neither work. that's odd. 
  passing in memory does seem to fix it on amd, maybe i got the calling convention for the last arg wrong? but not arm. 
  im a bit concerned that there's a race and i just changed the timing. 
- prettify
  - import_c/tokenize/read_punct: use `@expand` instead of a macro that's only called once
  - backend/meta/parse: less verbose handling of `argK`

## (Jul 29)

- jump.fr
  - fixed dumb mistake saving rbp twice instead of rbp+rbx. 
  - make layout consistant between different arches and let you pass through an argument 
    for my hack with starting a new thread. 
  - save floats
- simplify green.fr starting the main thread so it's not a whole back and forth yield dance
- more data points
  - you can turn on low power mode (in Setting > Battery) which makes everything take 50% longer 
    and still get the problem. tried on an M4 where things take 60% of the time here. still 
    got the problem. tho i had an attempt go for 27 minutes before the problem. 
  - i can eventually produce the problem by just running ` ./target/r.out all -bin ./target/q.out -jit` 
    in a loop (without recompiling). 2012 runs, 19.5 minutes. 
  - and with `./target/r.out all -bin ./target/q.out -cc`. ~77 runs, 1-1.5 minutes, twice. 
  - and with `./target/r.out mandel.ssa -bin ./target/q.out`. 1062 runs, 38 seconds. twice. 
  - so that's interesting. mandel.ssa doesn't have a c driver so the problem isn't in CodegenWorker. 
    so apparently when i turned off threads i just got bored before producing the problem. 
    the ./target/out/q.exe after failing there does produce the mandelbrot set correctly. 
    so that makes me feel like the test program that diffs the outputs is what's breaking there. 
    ha ok, yeah changed the test to not recompile, still fails. 3298 runs 96 seconds. 
  - maybe i broke it when i rewrote subprocess.fr. 
    that was in commit:037cd25b6cb473d7b5af11928aa3e0b7cda077ef, and the only actions failure 
    between that and commit:22ddd3dc54429e4d5e62d53ebb0234ed16768676 was in gif.c which 
    i know suspect has other problems because ive reproed that one with the slow debug checks at times.
  - made it save the output and give you the diff command. when it fails it just things the output is  
  - consider;;;; you have to waitpid the status before polling the file descriptors. im an idiot apparently. 
    cause maybe the other guy fully runs between when i poll the output and when i check if it's done. 
  - seems to work, can run mandel.ssa loop 32508 runs, 18 minutes without dying. 
  - so i think there's still an import_c mutex shaped bug but this polling thing 
    was the creepy one because it moved around a lot. 

## (Jul 28)

first version of userspace thread scheduling. 
each thread just runs until it cooperatively yields and longjmps back to a dispatch loop where we choose the next thread. 
replace the thread related calls (start_thread, futex wait/wake, sleep, join) 
with something that yields back to that top level loop. 

## (Jul 27)

- taking a break from my problems. want to port my old chess program to my language 
  and compare speed against zig's self hosted backend. so i want to update the chess to new zig 
  version that has the aarch64 backend. their error messages make me feel a lot better about mine... 
  whenever it says "root source file struct 'std' has no member named 'foo'", it says 
  "note: struct declared here" pointing at std.ArrayHashMap. 
  updated everything to 0.14.1, fixed perft.zig enough to make it work with 0.15.0-dev.1222+5fb36d260. 
  meh, the aarch64 still crashes on my program, kinda reduces my enthusiasm. don't really care enough 
  to fix the rest to work with the new writers. could still compare x86_64 tho, not not inspired anymore.
- semi-unrelated; this is fascinating to me. https://github.com/ziglang/zig/blob/771523c67534fc47800608eb720886e9f53da7b4/src/codegen/aarch64/Select.zig#L7172-L7369 
  like that can't be a better way to express that information than mine https://github.com/LukeGrahamLandry/franca/blob/main/backend/arm64/abi.fr#L555-L593 
  i'm very curious to see how much faster theirs is than mine once it works. 
  because i recall llvm release was only 2x-3x of qbe so like... there's not a lot of room. 
  my backend/arm64 is 2274 lines and their src/codegen/aarch64 is 24340 lines (not counting the assembler/disassembler) .
  i feel like i must be missing something. maybe the `@Vector` stuff is really hard? 

--- 

new plan, my problem isn't that i have a bug. bugs happen and then you fix them, thats normal. 
my problem is that it's nondeterministic so i can't rapidly narrow down the mistake. 
the only sources of nondeterminism should be aslr and thread scheduling. 
- something with forcefully killing aslr even on macos my MAP_FIXED-ing a giant thing 
  and using only that is interesting to think about at some point but i doubt that's the problem here. 
- CodegenWorker can run single threaded and it seems that makes the problem go away (but it's hard to be sure which is why this sucks)
```
no threads, no slow debug: 15 minutes: 13 runs passed.
yes threads, no slow debug: 7 minuntes: 7th run failed.
```
- so then is it a concurrency flavoured mistake or a parallelism flavoured mistake? 
  if it's just a concurrency problem, i can do a userspace green threads thing where i fully control
  who gets to run and randomize the order until i find a seed that reproduces it every time. 
- first an easier thing to check, Qbe.Module.intern_mutex has been sketchy for a long time. 
  make an allocator that wraps on in a mutex without needing to be super verbose about remembering 
  to lock it every time you call a function that uses the allocator directly.  
  (still broken). 
- did @must_return just to make sure im not making a silly mistake somehow 

## (Jul 26)

- trying to get CodegenWorker to let you build multiple tasks at once
  - because import_c needs that cause it tries to not keep the whole data structure of a program 
    so it hits constants inside a function and i want to be able to reset temp(). 
  - ended up being an infinite nightmare of mistakes. 
  - since you can now have multiple entries being used by bouba, the old way of just decrementing the futex
    value on release() is wrong i think because when waiting it's compared to the queue_size. 
    that feels true but i can't quite make it make sense so im a bit afraid i just had that cause hangs 
    when i was accidently leaking entries in import_c/function() and it was actually fine. hard to say. 
  - can't have threaded=true for comptime import_c ffi (i assume because it fights with jit shims) 
    so can't have codegen_queue_size=1 when threaded=false but somehow that made self compile ~20ms slower. 
    mostly fixed by not using multiple temp() allocators when not threaded so i think the problem was just 
    heating them all up took time. (was still cycling the entries in the queue even if only one is used). 
    which should be fine but resetting that temp() doesn't quite work. 
  - the end result is kinda simpler? and i hope i'll be able to make emit_ir suck less this way too. 
    but it's a bit slower and i think it's introduced hangs so that's pretty fucking worthless then isn't it. 
    i hope the hang problem is just the sketchy removal of the loop when waiting for no_more_codegen=0. 
  - interestingly i seem to have produced random failures similar to the ones i was having in CI that 
    i thought were helped by EXPERIMENT_LESS_DIRECT_CALLS. i wonder if they're the same problem or just both a crazy 
    memory fuckup so same symptoms. if they're the same problem by some miracle that would be great. 
    ugh i should just give up and make a garbage (collected) language. 
- emit_ir.(pending, new_constants, pending_set_lib) really can't be in temp() anymore. 
    - tho actually pending_set_lib was technically a race before because it just gets passed through to the codegenentry 
      but outer_temp would be reset eventually in poll_until_finished. 

## (Jul 25)

- start getting scc to build with import_c
- import_c: mangle names of static variables
- import_c: new magic word: `#discard_static_scope` because i want to so everything in one compilation unit but 
  some programs rely on being able to declare static functions with the same name in different files. 
- cleaning up the debug printing in backend
  - always use the enum via when_debug() instead of `debug["A".char()]`
  - make debug a bitset instead of an array. 
  - caught a mistake. emit() subtracts from curi first, so it always points to the last thing you emitted. 
    so when amd64/isel.fr/sel() is debug_assert(rslot == 1) on everything emitted, it needs to not do it on 
    the starting value of curi because that will be off the end of the array. it only worked before because 
    the thing after the array was `debug = forever.box(Array(u8, 91))` and it always started with a bunch of 
    zeroes so it was fine to read that as an instruction (allocations linear because it's an arena). 
    (indeed qbe decrements at the beginning of that loop instead of at the end as i was doing)
  - make meta/parse.fr and incremental.fr not mutate the Typ after calling Module.new_type so i can add a dump option for when you make a type
  - flag to dump data and add_code_bytes as well
  - put more `#` in print output so meta/parse.fr can handle the output more often
- start trying to make import_c use temp() in a sane way where you can reset it per function

## (Jul 24)

- fun with archaeology. 
  - cprime.ssa is "generated by Andrew Chambers' compiler", helpfully they have like 17 c compilers, so which one? 
    the commit adding that test is on 2015-09-15. first commit to andrewchambers/qc is 2016-02-16. andrewchambers/c/qbe is 2017-08-02. 
    andrewchambers/cc doesn't use qbe. so im stumped. 
- convert abi8.ssa generation script
  - use `...` in printf calls (qbe updated the generated file but not the script).
  - test.fr needs to allow split sections because only part of the driver is generated. 
- easy wrapper for collect_directory_ordered when you want it sorted so results are consistant accross os (which is always what i want)
- the lastest scc doesn't build but also i don't need that i need the one from 2017-02-23. 
  - 4a695da3a6e31f002608accb745d4369fe7f0eaa reproduces strcmp.ssa but not strspn.ssa
  - 7599d1a808bd0b604b7e5cde5a84342cf868509e has something real fucked up where `cd $pwd` is becoming `cd wd`,
    if i randomly change it until it builds, same result as other commit. 
  - but the structure is the same, just the id numbers change which i don't really care about. 
- EXPERIMENT_LESS_DIRECT_CALLS im suspicious that most of my random failures started when i got rid of that. 
  i would believe that im still messing up the clearcache somewhere. doesn't explain everything because i have problems on amd64 too. 
- give correct error instead of crash when you have an overload set somewhere with the same name as a constant. 

## (Jul 23)

- tried a test with two types of PageMap and get_page() doesn't get deduped which seems wrong.
  also set lnk.shrink when DisallowInOverloadSet and then it gets anything with const args (including box() which fixes get_page). 
  - new hits: alloc_zeroed, assume_cast, to_values, reserve_type, load_command
  - `>>> 889504 bytes of code, 70570 bytes of data. >>> [CPU Time] frontend: 915ms, codegen: 444ms`
  - ... but that makes it hang compiling farm_game.fr
    i think making `fn SEL($name: CStr) ObjCSelector` be quadratic in number of objc selectors is bad for business. 
    just setting an arbitrary cap of 32 entries in a group before you give up trying to deduplicate fixes the hang. 
- my attempt to fix join_codegen_worker was very wrong. 
  - CLONE_THREAD says `nor can the status of such a thread be obtained using wait(2).
    (The thread is said to be detached.)` which i was hoping meant you could still wait for it to finish 
    it just wouldn't give you the status but i think you can't wait for it at all, hence my getting ECHILD. 
    - i guess the extra syscall just slowed it down enough that it seemed to work more often... idk, 
      demonstrates that i should really be auto-testing -syscalls as well. 
      shame i can't get a github actions that doesn't have a libc at all to make sure i can't cheat. 
  - new theory is you have to use CLONE_CHILD_CLEARTID and futex wait on the tid address. 
  - that seems to work.. but that's what i thought about the old thing too so who knows
- continuing curation of .ssa tests 
  - it's tempting to try to figure out what rng python2 uses so i can port the generator script for vararg2.ssa 
    exactly and have a smaller diff. i did some poking around and it turns out cpython has lots of code and 
    i don't care that much. just use my existing rng to produce the arbitrary data.  
      - first one that needs nested sections so had to update backend/meta/test.fr/extract_sections()

## (Jul 22)

- i want to do real symbol aliases instead of CodegenTask.Bounce's weird tail call thing. 
  - use reenterant mutex for symbol bucket lock so it's less painful to do things with multiple symbols. 
    it's fine to read another symbol in the same bucket, you just can't let it resize. 
    - i tested the "compiler bug!! tracetrap if you make this #inline" above the old lock_bucket()
      before removing it. was fixed at some point. 
  - im afraid this is going to end up not working because of objc_msgSend_stret and objc_msgSend 
    mapping to the same thing but needing to tell apart for cross compiling from arm to amd. 
    but that might have been resolved differently, we'll see what happens. 
  - self compile works if i track which symbols are aliases and redirect push_fixup to the target
    but thats order dependent and fails on a bunch of stuff. and really i don't want to add more stuff 
    where you have to check if it's an alias every time you want to use a symbol like i used to do for 
    follow_redirects. 
  - but also maybe i only care about doing it for local things which have already been compiled, because 
    thats the situation that shows up with the deduplication thing im planning to do, which is the easy case anyway. 
- `>>> 903208 bytes of code, 70554 bytes of data. >>> [CPU Time] frontend: 917ms, codegen: 442ms`
- first attempt at bringing back deduplication of generic functions. 
  - same idea as when i did it in the legacy-comptime-jit. 
    rolling hash of instructions and then if everything matches, redirect to the old one. 
  - currently relying on no hash collisions which is a bad idea. 
  - much much less impressive result than when i did it the first time. 
  - some hits: push_uninit, checked_hash, strncpy, init, eq, find_slots, is_sane_pointer
- QbeModule.debug_out shouldn't use QbeModule.forever since you're supposed to take a mutex before using that.  
- `>>> 896376 bytes of code, 70554 bytes of data. >>> [CPU Time] frontend: 918ms, codegen: 442ms`
  (901216 by the bloat.fr measurement i was using yesterday)
- replaces aliases in f.con so you can have chains and notice they're the same. like if a1 and a2 are the 
  same except for calling different functions b1 and b2 but b1 and b2 are the same, then a1 and a2 are the same. 
    - new hits: expand_to_find_slot_erased, insert, get_ptr, deep_clone
- `>>> 893432 bytes of code, 70554 bytes of data. >>> [CPU Time] frontend: 910ms, codegen: 441ms`

## (Jul 19/20/21)

- plug more harmless leaks because it's fun now: kaleidoscope, qbe_frontend, prospero. 
- put more stuff in the Alloc interface: arena mark/reset, free all
- new debug mode: protect memory on arena reset. 
  - caught ascii_table baking temp memory
- on the plus side i can get rid of an ugly thing in copy_bytes that i fixed a while ago
- linux-syscalls crash is just about my thread spawning stuff. 
  join_codegen_thread frees the stack without joining the thread when not linking libpthread. 
  easily fixed by wait() syscall on the pid from clone. 
  update: THAT'S WRONG
- idk what i was smoking with BlockAlloc. i was trying to lazily split a page into blocks but 
  wasn't keeping track if it was the first time or a re-free-ed one. 
- little bit of shrinking
  - 917280
  - poly-outline sema.fr/eval()
  - poly-outline display() on sequential enum
  - 910080
  - still have problems with first call can't be inlined so tiny generic things like 
    get_variant_ptr_for, get_field_ptr suffer if not marked explicitly #inline
  - 907520

## (Jul 18)

- collection of arm-macos random failures in github. 
  so i have problems on all platforms. is there any pattern here? maybe it's speed dependent? 
  - failed to exec `target/cc.out <...>/minic/yacc.c`
  - `use_symbol but already locked by current thread` for import_c/test/test.fr a.c
  - fail output hello.ssa -w
  - import_c/test/lua.fr sort.lua
  - fail output abi6.ssa -w
- lets see if SLOW_MEMORY_DEBUGGING reveals any of my problems.
  - import_wasm dies on output from bf/via_wasm. 
    `IR failed typecheck: temporary %getL0.512 is assigned with multiple types (Kw vs Kd)`
    wasm-validate thinks the wasm file is fine.
    in local_inst() was holding a pointer into f.tmp across a newtmp() and then using t.cls 
  - add some set_bytes(junk): alloc() and box(). the latter breaks import_c. 
    - test.fr line.c: `unexpected (prefix) token ,` and test.fr arith.c: `panic! thats not in the segment`
      `int main() { return __LINE__; }` crashes because there hasn't been a `#line` directive, 
      line_delta is uninitialized by new_file and new_num_token doesn't work with negative numbers apparently. 
  - set_bytes(junk) the extra capacity in List.reserve_expand_erased
  - now `backend/meta/test.fr -w` often fails something in github but it's always a different one: abi8, abi6, puts10, abi5,
    but i can't seem to reproduce it locally. 
  - failing at the end on compiling minic was just because i wasn't chdir-ing out to the right place 
    after cloning the git repo so it didn't work the first time. 
- another dependency on universe state is when i use libc_allocator. need to make sure everything passes with BlockAlloc. 
  - bf/c_string.fr fails but only when reusing CompCtx in compiler/test.fr. 
    safety check in move_from_module for fold.fr/Kw() when compiling import_c for comptime. 
    its not about reusing CompCtx, it's about dropping a QbeModule? maybe that just double frees into the general_allocator. 
    i think it must be a BlockAlloc bug because it works with DebugAlloc which is much more aggressive about catching misuse. 
    (still not fixed; and more things don't work with DebugAlloc tho: import_c/wuffs/gif.c, compiler/test.fr/kaleidoscope) 
- finding some leaks with DebugAlloc
  - default_build_options. make a copy in init_self_hosted so it doesn't have to stay live. 
  - QbeModule.(libraries, macho_function_starts)
  - collect_directory_ordered wasn't calling closedir out of fear you'd early return?
  - relatedly the LocationResolverNode in compile_cached should get popped but still needs to leak
  - STDLIB_PATH
- implemented DebugAlloc 
  - i should have done this a long time ago. only one of those leaks (closedir) was interesting 
    (the rest were finite and round to zero) but having it just dump a stack trace of exactly where 
    the problem is makes it a fun little game to make sure all the memory gets cleaned up instead of an infinite slog. 
    - still needs a lot of work to be generally useful but it's a very good first step. 
    - it's a nice validation of the idea that debugging tools can just be a library that gets bundled into your 
      program instead of an external tool. 
    - and like i didn't quite realize how good for morale any tiny thing i can do to get rid of confusion. 
      in hindsight it's obvious and ive had similar moments before, but i should really be putting more effort 
      into tooling that lets you tell what the fuck the program is actually doing. 
- that last commit went 910ms -> 920ms ?? :(, and i was so recently below 900ms. 

## (Jul 17)

- started a better version of bloat.fr that doesn't depend on objdump
- qbe's minicc with import_c
  - `Assertion Failed: (do_jit_fixups) redeclared symbol 1677: yyerror ZeroInitData[20592]`
    for forward declarations like `int yylex(void), yyerror(char *);` im not treating the second one as a function? 
- trying to reproduce qbe's generated .ssa tests
  - queen.ssa
    - tweak qbe's committed minic/test/queen.c to return status instead of printing
    - need to use an old minic from before it put varargs at the end of everything 
    - but then it works, nice! so the path is import_c compiles qbe's yacc which converts minic to c source,
      then import_c compiles that which then compiles queen.c into ir. 
  - vararg2.ssa
    - when i run their python program i get different random numbers. 
      they do set the seed tho (which is nice) but i guess the algorithm python uses changed since 2022? or changed based on os? 
- lib/sys/subprocess.fr 
  - rewrote to be based on the same logic as examples/terminal.fr so there's less duplication.
  - allow sending input bytes to the child. spend a long tine discovering the child needs to close its copy of the write end 
    of the input pipe to ever read EOF from it. 
- continuing with generated .ssa tests
  - backend/test/generate.fr: autotest that queen.ssa can be regenerated
  - some of the tests aren't compiler output but are ~50 lines of paste-y stuff where the pattern can be generated in ~10 ugly lines. 
    i think that's still worth giving a generator program so you can easily see there isn't a trivial copy-paste error. 
    spill1.ssa, isel3.ssa
  - some of the tests want to think about bits and are much more readable if you can say constants as hex. 
    (to the point of having the hex version in a comment), so just have the ir parser allow hex number literals. 
    load2.ssa, ops.ssa, mem1.ssa
  - i wonder why i had the number "0x1e6a2c48" in a comment in ops.ssa... no idea. 
  - euclc.ssa is minic as well
  - vararg2.ssa
      - ha! the numbers problem is python2 vs python3. everyone's favourite. 2.7 produces the right random numbers. that's reassuring. 
  - mem2.ssa and mem3.ssa are inconvenient. they seem to be from a hare patch that didn't end up getting merged 
    and then further reduced (which is great for debugging but less great for my ability to regenerate them). 
    https://lists.sr.ht/~sircmpwn/hare-dev/patches/38947
- this_is_dumb_but_im_actually_going_to_lose_my_mind: make it easier to include `%` characters when making strings with `@fmt`

## (Jul 16)

- tcc
  - it compiles but doesn't work
  - they include tcc.h with different values for USING_GLOBALS and my detect_include_guard is going off incorrectly. 
    can't assume that a file starting with an `#ifdef` and ending with an `#endif` can be treated like `#pragma once`.
    need to bail from detect_include_guard when the first #if ends, not just go to the end and assume if there's an endif there it matches the start. 
  - i don't super understand what this expression is supposed to do `*& TCC_VERSION + 4`,
    does the `*&` just turn off a warning for adding a number to a string literal? 
  - last problem: it needs to tcc_add_macos_sdkpath to fine libc which needs -DTCC_IS_NATIVE which needs some more imports. 
    (if i hack around that it can compile hello world)
  - need to compile libtcc1.a and runmain.o before it can compile any executables 
    but it can compile those itself without a bootstrapping step so that's not a big deal. 
  - i want to compile it starting with my compiler and with clang and have it compile itself and make sure 
    they converge on identical binaries but they're two bytes different, very sad. 
    objdump and dump_macho are the same, so it's in data or padding somewhere. 
    ```
    diff a.txt b.txt
    33708c33708
    < 008ecc0 0000 0100 6374 5f63 2d61 3762 3363 3738
    ---
    > 008ecc0 0000 0100 6374 5f63 2d62 3762 3363 3738
    33898c33898
    < 008f8a0 5806 0088 0000 0000 0000 7401 6363 615f
    ---
    > 008f8a0 5806 0088 0000 0000 0000 7401 6363 625f
    ```
    ah, fuckers. they put the file path in there. im compiling to tcc_a.out and tcc_b.out 
    and the bytes that change are 0x61 vs 0x62
    
## (Jul 15)

- repl
    - allow #use
    - spent much too long on copying the input for add_file but passing the old one 
      to push_parse which messed up the intern pool. 
        - added a slow_memory_debugging check that catches if you misuse StringPool.insert_owned
    - autotest
- started playing with making a debugger but lost momentum when everythings broken forever and ever and ever
- trying to get tcc to compile with import_c
  - more attributes to ignore: format, noreturn
  - `ssa temporary %c.129 is used undefined in @14 of @is_null_pointer (non-dom ins)`
     don't emit a jnz on a constant because it doesn't like that a value isn't defined on the unreachable path. 
     caught a bug!
  - bleh they exec xcode's codesign
  - more stuff for include.fr, reaching the point that i want to share with lib/sys/posix.fr

## (Jul 14)

- arm linux has some of the open() flag values shifted
- cleanup: get rid of replace_body_source, pre-vbytes bake builtins, comptime_arch, target_(arch/os)_bitset
- made PageMap generic. planning on making a debugger and having a pagemap of symbols for breakpoints, etc. 

## (Jul 13)

- finish directory import 
  - `dir.@"file.fr"` sucks so need some sugar for that. 
    for now just alias whenever there's a `.fr`, but what if you have a directory called that too, sad. 
  - i still want to let you get_constants() and iterate over it so need to readdir so need to find the syscall for that. 

## (Jul 11)

- start allowing import() of a directory
  - i want to make it easier to move files around without making all the importers change paths. 
    so this way you can get rid of a directory and replace it with a file that points to the new stuff to give people time to update. 

## (Jul 10)

- fixed arm-elf adr reloc
- when jitting, check if constant ints are close enough to load with adr instead of several movk
- lox: 
  - some mistakes: nan eq, prec of token after parse, scan pop dot before fraction
  - while, for
- import_c: stop calling count_array_init_elements
  - grow init.children as needed
  - compute array_len as max index as you go
  - set size to base.size*array_len at the end 
  - very confusing when array_len gets set to 0 or -1, one is `static struct { T a[] } = { ... }` and one is `static T a[] = { ... }` ?
- import_c.Obj.size_of: old=192, new=112. just got rid of some fields unused since i made it rely on backend more a while ago. 

## (Jul 9)

- have to make sure #ir gets a body before any calls get compiled so we check if it's foldable
- lox: and/or, run passing tests

---

- thinking about giving cranelift another chance. 
  - maybe now that i do my own inlining it would be better. 
  - could use it for boot and then only need to commit the frontend. 
  - it doesn't do c abi for you so it's a massive pain. 
  - for env parameter, could use their pinned register. 
    - they don't support it on riscv for some reason
    - did some experimenting with changing my env abi to that. it's easy to pin a register 
      but it's annoying to try to use that for passing env because also i need to support both 
      for the one compile where you transition between them because the comptime code needs to agree with the compiler. 
      - tho actually now that i typed that maybe i was making it more complicated than it needs to be because 
        comptime and runtime are in different modules now so they can easily ask for different abis, 
        i don't actually need to allow both at once.  
      - it might be better than using a register you always have to fight over (on arm anyway cause there's lots of registers), 
        so even if i decide against cranelift that's an interesting experiment. 
  - maybe i just like the idea of a distraction. but like depending on several hundred thousand lines of rust 
    is just a worse situation to be in, even if its an optional backend, it's just more time to waste. i kinda 
    don't want the escape hatch, i want to be forced to make my backend good enough to be usable for everything. 

## (Jul 8)

- changed some assert to debug_assert. 
- added a case in immediate_eval_expr for scope access so don't need a SyntheticImmEval for every Qbe.Ref
  lit_fn: 4780 -> 3973
- made `fn not` not explicitly #inline. lit_fn: 3973 -> 3888 and the backend still inlines it anyway
- yesterdays changes to fill_export_ffi broke repro 
  - even if i force CodegenWorker.threaded=false. 
  - because the order returned by get_constants is not consistant.
    sorting by Span in get_constants fixes it, so seems that's the only problem.
  - var.id is stable but Symbol value is not. so since i have scope.vars which is in insertion order, i can just 
    use that instead of scope.lookup and it's faster to iterate that than a hash table anyway so it's just better. 
      - .lookup used to be better because it only had constants, tho that doesn't matter much anyway because 
      you're normally doing get_constants on a namespace-like scope that has no runtime vars anyway, but vars are hosted 
      so they're actually first anyway and it doesn't matter)
  - but also after that change, the symbol values are stable. so intern order only changes if get_constants isn't sorted 
    but also the only thing that causes it to be unsorted is if intern order changes.
    so that doesn't make sense to me. the first place the order of calling `pool.insert_*` changes 
    is before the first call to get_constants. 
    OHHHH, ok, it's because the order you get from get_constants determines not just the order that you try to compile the functions
    but the order they appear in the generated string in fill_export_ffi which is embedded in the new compiler so when it goes to 
    compile something the first thing it parses is that generated string so it sees symbols in a different order so the interned values change.   
- random cleanups: remove some redundant #fold on #ir, use `@Fn()` instead of `Fn()`, i don't use function_in_progress, log(DeclVarPattern)

## (Jul 7)

- terminal.fr: basic argument quoting
- attacking some functions that show up in bloat:
  - start; 953860
  - parser; pass around by ref instead of copying all the time: 932772
  - @try; access .Ok/.Err directly instead of calling unwrap(),unwrap_err(),
    because just checked is_err() so it's fine. 907828. yikes!
    - with these changes, the ldp/stp blit changes only save 25k but it's not slower so still worth it. 
  - fill_export_ffi; generate array of fn pointers at comptime instead of having 
    a @fmt call for each function. 903664.
    - not impressive but making it more reflection shaped is the first step to 
      not typing the function signetures twice which will be less error prone. 

## (Jul 6)

- rearanging expand_blit() so you have ldr,ldr,str,str instead of ldr,str,ldr,str and then 
  having emit notice pairs and use ldp/stp saves space: 1018716 -> 953880 (64k, 6%)
- write syscall wrapper with AsmFunction instead of using the backend's special O.syscall. 
  (required for my cross-incremental plans). 
  - dealing with clone syscall is a bit more painful. before i relied on the wrapper being 
    inlined but AsmFunction can't be so for amd64 where return address is on the stack,
    it has to pop it early and tail call back instead of ret. ended up using a float register 
    because spilling anything to the stack has the same problem that it won't be there in the 
    child thread. which seems like i must be over complicating it. maybe the sane thing 
    is to have the parent layout the new stack such that the new callee is the first thing you'd pop off? 
    you do have enough callee saved registers to make it work without that, 
    but if i want to keep doing that i kinda have to write that whole codepath 
    in assembly so it's not relying on the backend making lucky spilling decisions. 
- give riscv O.asm and do tests/exe/sys.fr without syscall instruction
- fix riscv auipc+add encoding. both immediates are signed which makes it confusing
- it's less code to just pass args to syscall wrapper in registers. 
- now that i think about it, xor is a dumb way to merge hashes. it's very possible to have the same file content twice. 

## (Jul 5)

- experimenting with perf_event_open syscall. got the point of getting ip samples out of it, 
  but it being linux specific magic is kinda lame and the fancy hardware counters don't 
  work on orb anyway. so i want to see if just having a timer send a SIG_PROF is fast enough to be reasonable. 
- theres definitly some memory bug happening where the program text gets corrupted and
  it decides theres an UnterminatedStr error (and prints out the line that it's helpfully deleted part of),
  so that's something to deal with systemically. it happend 4 times today which 
  is enough that it's real and unacceptable but not enough that it's easy to fix. 
- fixed: putting stubs in normal code section broken backtraces because AotDebugInfo is sizes 
  not absolute locations so forgetting one messes up everything down the line as well.
- tried to use stp for storing callee saved registers but that made it 15ms slower which is strange. 
  maybe it's branchier cause it has to deal with the odd case? only saves 8k bytes.
- found embarasing mistake with address instead of distance for `adr`, so that's why it never thought it was in range 

---

> zed is so fucking garbage somehow im so mad.
> oh i restarted the program of course i wanted you to delete the settings file
> so i have to painfully reconstruct it from everything ive complained about before. 
> how can it be this bad and somehow everything else feels worse to type into. 
> whats going on man. as a fun bonus it forgot how to auto indent after a opening `{`
> in my language. also im pretty sure my comments are a different colour now? 
> that seems unlikely. i must have just picked the wrong theme but you can never be sure. 

## (Jul 4)

- arm: re-clearcache after doing pending_immediate_fixup_got

---

more playing with blink.  

(passing -Z to blink to get stats. note: that makes it much slower so have to time seperately).

instead of writing directly into wx memory, write into a buffer and then copy it back. 
```
before: 0m20.058s, smc_checks = 599828
 after: 0m18.273s, smc_checks = 88181
```
only reduced by a factor of 6.8, looks suspisiously like im still faulting every 
iteration of copy_bytes and that's just the difference from writing 8 bytes at a time instead of 1. 
yeah if i copy one byte at a time it's back up to `smc_checks = 584418`. 
so need a copy_bytes that doesn't use a jmp instruction in the loop. 
i feel that ive aranged it so it doesn't use jmp for the backwards branch 
of the loop but it barely helped. 85272. and it still goes back to 600k
if i stop going by 8 bytes, so clearly im wrong about the jmp thing? 
im looking at the disassembly and the only jmps in that function are before 
the writes or to the return.
there should only be 6981 functions and 74 fixups so that's way more than 1 fault per time. 
hmmm, you know, i think their readme is lying, if i run thier `smc2_test.c` example, 
i get `smc_checks = 320` which sure sounds like "300+ independent write operations to RWX memory."
and not the `smc_checks = 22` they report (same in a fresh copy without my changes so it wasn't me that broke it). 
if i go back to the commit that added that to the readme (0ecf9fa73a549982f0ee3a20caa3109f8d412dec)
i get `smc_checks = 66` on thier test (... if the test is compiled with -O2, or the reported 21 if -O0)
and `smc_checks = 13226` for my program. now the downside is that old version takes 0m32.446s 
so that's worse than the new one that gave up on fancy smc.
the commit that makes smc_checks be high is cd66fe49ea2c946cc30f820866d45e42bd18791a "Remove some old code". 

ok so if they're lying about the jmp instruction thing, 
what if i just use rep movsb so they do the copy themselves? 
ha, yeah, smc_checks = 7044. still 18s tho so not as big a win as i was hoping. 

but it now it gets that speed even in normal blink so i don't need my patches that speed up signal handling:
for posterity they were:
```
- 1:27.62: upstream blink (https://github.com/jart/blink/commit/98f95e8383d1032eb4d2dc6aae937b23539e915e)
- 1:03.60: pass pte/vaddr out from FixXnuSignal to IsSelfModifyingCodeSegfault 
           instead of doing page lookup twice
- 0:19.14: assume every segfault IsSelfModifyingCodeSegfault 
           so don't need to FindGuestAddr at all
```

## (Jul 3)

- why am i doing special stuff for `__stubs`, they can just be the next thing in the code segment. 

---

playing with blink. it would be cool to un-sandbox and let my thing jit directly for the real architecture. 

- kStaticAslr: 
  make kSkew=0 even on apple so guest and host share an address space. 
  (ChooseAslr non-zero even for static binaries to avoid base address in low 4gb). 
    - changed context.fr to notice the aslr skew when applying relocations
- LoadArgv: `PUSH_AUXV(0x41434E415246, (i64) franca_escape_sandbox);`
- GetJitHook: `if (virt == (i64) franca_escape_sandbox) return (i64) franca_escape_sandbox;`
```
void franca_escape_sandbox(P) {
  // get the arguments
  i64 a = *((i64*)&m->di);
  i64 b = *((i64*)&m->si);

  // compute something
  i64 result = a + b;

  // return the result
  *((i64*)&m->ax) = result;

  // pop return address and jump there
  i64 ret = *(*((i64**)&m->sp));
  *((i64*)&m->sp) += 8;
  m->ip = ret;
}
```

that does work but it's probably not worth the trouble since the only platform 
(of the 4 i care about rn) that can't run amd binaries easily is linux-arm. 

## (Jul 2)

- new internal pointer test doesn't work on linux but only when actually making an exe,
  not if just running through cache file (which also does bake) so maybe it's just 
  elf addends problem? yeah it works if i go through relocatable or if i use -syscalls
  so use my own do_relocations_static_linux. it's ignoring the addend. i guess the GLOB_DAT
  relocations are just never what i want. huh, i guess i never used elf addends because 
  previously they were only in the import_c tests which i haven't got running on linux yet. 
  welp, progress!
- finishing signal handling on linux. 
  - painful slog of translating the structs for each arch to common represention 
  - amd64 they really scramble the register order. 
  - now works on amd but not arm. 
    dumbass, i typoed sa_flags as sa_mask so i was always saying sa_flags=0 instead of SIGINFO, 
    apparently only arm doesn't give it to you when you don't ask for it
  - now i catch sucessfully but die before printing the stack trace. 
    problem was i just did padding in the inner struct to make 
    the floats 16 aligned but that doesn't give the inner struct itself 16 byte 
    alignment in the outer struct. just use `#align` on the field since i have that now. 
  - now crash2.fr works everywhere
- self compile on linux arm now works if i don't check for bti when handling 
  the icache fault. so i think my clear_instruction_cache doesn't work. but it definitly 
  helps because i couldn't compile anything at all without it. 
  - tried aligning beg back and end forward to 128 bytes, didn't help. 
  - i feel like i have the comparison direction wrong but crashes immediatly the other way. 
    for one thing i was doing p=step instead of p=p+step. 
    ah and then now when i make the comparison look right it works and never hits the bad 
    case, same as macos, success!
- is_wrongly_illegal_instruction being branch_target_marker won't be true for stubs, 
  and that happens occasionally now on arm-linux. but also that seems a bit sketchy 
  because you're relying on the thing you put in a vtable being the real symbol not 
  the local stub, which should always be true but will be a nasty surprise some day. 
  so just saying whatever stick a bti in the stub might be better than allowing 
  adrp as the faulting instruction. maybe im defeating the point of the security thing
  cause now you can indirect branch to the stub and get a libc function or something 
  but you could indirect branch to the libc function anyway? 
- noticed i messed up for producing small negative i32 in one instruction. 
  it's fine that the high bits are zero. 
- .ssa tests pass except for varargs. 
  - fixed the non-apple-varargs assert in arm/emit but it still doesn't work. 
  - compiler/test.fr
    - "tried to get tmp of non-tmp RCon:0 in va_arg()". 
      typoed a place holder @emit_instructions. should really let them have names. 
    - this seems bad for business: trying to write to a function pointer ??
    ```
    stop reason = signal SIGSEGV: invalid permissions for mapped object (fault address=0x1016c44)
    ->  0xffffdc63b964: str    x4, [x5]
            x4 = 0x0000ffffffffa720
            x5 = 0x0000000001016c44  q.out`arena_allocator_fn__18694
    ```
    - oops, broke crash2.fr backtrace when linking libc. 
      hmmm, i was typoing lkr vs lcr in a few places, much confusion now. 
      ok they actually are laid out in a sane way between libc and the kernel,
      maybe i just bamboozled myself by reading something about x86_32 or im just dumb who knows. 
    - ok so now it dies in comptime call to add_ints_and_floats (which is variadic). 
    - bleh, using an undeclared name in @emit_instructions just gives you a garbage register. 
      so trivial typos waste a lot of time. 
- now all the tests that run on linux-amd pass on linux-arm too
- TODO: something going on with unalign cas ?

## (Jul 1)

- 958ms
- consolidate scopes.types and scopes.constants
- the lazytype is only needed for constants so store it indirectly
```
a, b := (0, 0);
each c.scopes.constants& { it |
    ::ptr_utils(LazyType);
    a += int(it.nullable_type.is_null());
    b += 1;
};
@println("%/% empty. saved % bytes", a, b, a*size_of(LazyType) - b*8);
// -unsafe self compile:
// 149352/243854 empty. saved 11192144 bytes
// after not allocating for Finished/Infer
// 243734/243958 empty. saved 19496928 bytes
```
- thats pretty funny. all that messing around with LazyType and i only actually use it 224 times
- 945ms
- the siuation with RenumberVars is confusing. it feels like it's redundantly 
  copying the whole scope but then has to look at each declaration as well.
  - theres a few declarations that seem to not really live in a real scope. 
    - parameters of functions without a `{}` around the body. that makes sense, 
      you push_scope a few times in resolve_func_body without putting the ScopeId somewhere 
      it can be found. 
    - emit_capturing_call moves the parameters into a DeclVarPattern in a new block 
      and then renumbers that, never going through the Func. 
    - field const/default value
  - thats hard to think about so putting a pin in it for now
- always use hashtable when looking up a name instead of only for constants. 
- 938ms
- 39 samples of ask_can_assign_inner, thats... a bit much... the whole parsing is only 68. 
  represent the builtin id types (FuncId etc) with as an empty Enum instead of a Named
  so it doesn't need a special case to make them not transparent. now it's 20 samples. 
- outlining some of the hashmap stuff. RawHashMap is instantiated for 36 different (key, value) 
  pairs but only 18 key types and almost everything only cares about the key type, 
  so value can just be treated as an opaque blob of bytes. 
- 926ms

---

currently baked values deduplicate when you reach the same address from multiple 
places so you can have cycles, but only when you only point to the start of an object. 
i need to allow internal pointers so you could have a slice of things that have 
pointers to other elements of the slice and have them retain thier relationship
in memory through the bake so you can still access the slice as a whole. so i 
need to be able to look up any address inside an object and get the outer object 
so that reference can become a relocation with an addend. so the current hashmap 
of pointers doesn't work. instead track pages of memory and range of allocations 
in each page so the keys are `[]u8` instead of `*u8` and you can nagivage spatially efficently. 
- bit of extra hassle when an allocation spans multiple pages but that won't happen too often. 
- still not handling the case where you see an internal pointer before the larger allocation 
  and have to like go back and unbake it. not sure what to do about that. maybe pre-register 
  all the memory i create in comptime code so you know the types? but that sounds slow. 
  anyway the old system couldn't do that either so what i have now is already much better.
- 935ms, but i kept the old version with the hashmaps too so i can toggle between 
  PageMap and OldPageMap and that doesn't make it fast again. so maybe my old measurement 
  just got super lucky somehow. idk, but im satisfied the page tracking isn't meaningfully 
  slower. 

## (Jun 30)

- something fishy going on with staticmemmove needing an extra step in runtests bootstrapping. 
  need to make sure repro works otherwise before i can debug that sanely tho. 
- so for repro, it doesn't repro between the -unsafe compiler and the normal compiler 
  (both building -unsafe), so the one in `target/release` after runtests is different 
  than if it compiles itself, but after that it repros. 
- the frc_inlinable has only one difference: whatever g423 is. 
  so this is probably a good time to try to give data good names. 
- the names is a bit better tho needs more work to apply to more stuff. 
  current version is pretty much just `name :: @static` which is kinda rare. 
  but anyway, this was all pointless for my actual problem because g423 is 
  the frc module for staticmemmove. last 87 bytes are different (all in idx part),
  but printed as text it's the same. 
  ohhh im stupid, it's because fails_typecheck sets tmp.cls but i only run it in debug mode.
  but actually it only needs to be stored for phis because fill_use resets it. 
  as a bonus frc_inlinable files get ~2% smaller by not storing them. 
//       this cls doesn't matter because it's set again in fill_use,   
//       so don't actually have to store Qbe.Fn.tmp when saving for inlining,  
//       can just have a sepereate array for the mapping that's currently in .visit.   
//       emit_suspended_inlinables immediately does fill_use anyway which would recreate it  
- before inline changes it was 1080, and then the first time after runtests it was 
  1070 and then i did it again and now it's 1002. what the fuck did i change between those?
  i could swear it was just setting read_only and i would understand if fixing that made it 
  way faster but changing it back doesn't make it slow again. ah ok, i just got confused 
  because it takes two compiles after toggling it to see the change since you want 
  to measure the new compiler built by the new compiler. so yeah copying read_only when
  saving the function so you don't add extra blits for the arguments is very good. 
  very sad that i forgot that when i stole the inlining stuff. 
- VarId for a bit more type safety 
- fix 10 samples of memmove for push(zeroed(FatExpr, LazyType)) in scope.fr
- "placement new" for deep_clone. samples: 90->64. time down to 965ms. 
  - kinda sad that my passing things by value is that shitty. 
    hard to decide between keeping it because it's easy to think about and you 
    can just fix the few hot places that show up in the profile, OR changing 
    the whole calling convention situation because my job is to make the code 
    you want to write also perform well. 

## (Jun 29)

- i don't like seeing `mov x, 4; and a, b, x` in the disassembly, 
  so i want to do bit mask immediates but it's such an infinite nightmare somehow. 
  the spec gives you psudocode for decoding but not for encoding. 
  once you can visualize the unintelligable paragraph it almost feels like it should 
  be easy, like count the number of bits of the end type and then shift those off 
  and count the number of the opposite type, but you don't know how big the pattern 
  is so they might bleed into each other. surely you don't have to just try all the options. 
  i think the insight is to think of it as counting islands of ones like 
  the stupid cOdER puzzles. so just need to count how many times you change 
  from doing one bits to doing zero bits, and can do that without a loop which is nice. 
  and then figuring how much to rotate is a bit hard to think about but much easier. 
- actually use it in isel+emit.
  before: 1199181b, after: 1182765b, same time 1075ms. 
  kinda not worth the effort. 
- (when linking libc) convert large blit to memmove call on all arches 
  instead of hardcoding arm abi in a dumb place.
  franca-macos-amd64: 1540752 -> 1409696 (8.5%). 
- so that's enough that i really should do it when -syscalls too. 
  - the sane option would just have the symbol to use as memmove passed in when you
    create the module, but the more interesting thing would be to just 
    compile the `copy_bytes()` alone into a .frc at comptime and load 
    that as needed. which is pretty painful but i think i have to do it 
    that way because it's the kind of thing that's supposed to be easy 
    in this language so using that power gives a chance to improve the experience. 
    then you could do that for other compiler-rt-ish things like wasm needs byteswap
    or whatever and it would be easier to write that once in franca instead of with @emit_instructions. 
  - problems with not inlining all the intrinsics reliably or if it happens in the backend 
    somehow instead of emit_ir you still get them emitted in the module even if not called? 
    and if there's other functions in there, it's hard to find the function you care about 
    because the name gets mangled. need to be able to ask for the name. tho fmt_fn_name 
    is in ast_external so i could call it but it's probably better to put that in the vtable 
    so it's easier to change. 

--- 

- i need signal handlers on linux to make the compiler work reliably on linux arm. 
seems like i might have to do extra stuff for that? 
```
SA_RESTORER
Not intended for application use.  This flag is used by C
libraries to indicate that the sa_restorer field contains
the address of a "signal trampoline".  See sigreturn(2) for
more details.
```
so im probably a libc not an application so i guess i need to do sigreturn syscall at the end, 
that makes sense to me. 

## (Jun 28)

- removed unreachable code that handled b/h arg/par in arm abi.
  those are always converted to normal arg/par in abi0. 
  qbe does the same thing: apple makes them be sign extended, non-apple 
  makes high bits arbitrary, but nobody actually treats small things 
  differently later in abi. even if you run out of registers and have 
  to pass on the stack, the size is rounded up to 8 bytes. 
- go back to doing opt/slots.fr in 4 byte granularity. 
  the abi slots i care about are never 1/2 bytes anyway.  
  and it means less defensive removing things from `slot_contains`
- failing some .ssa tests with linker on amd64_sysv. it's always ones using 
  a c driver (and they work with my cc), a few are abi ones, but they work on 
  apple so i don't believe that it's actually an abi problem. 
  oh i think it's always ones that import a variable from the c. 
  - it's making a MOV instead of an LEA, 
    because fixup_amd64/RipDisp32 converts to got load because that's what it would 
    be for an exe, and machO's X86_64_RELOC_GOT_LOAD will change it back to normal 
    LEA if it ends up being local after linking. 
    so it works for both if i just don't convert to load myself if it's .Relocatable. 
  - but im afraid that means it won't work if it does end up being a dynamic import for elf. 
    is there a different reloc type for just do the right thing once you know where the symbol is? 
    maybe i need R_AMD64_REX_GOTPCRELX? 
    it seems like clang does the same thing as me if i feed it qbe's assembly. 
    oh, but qbe is folding the store so 
    ```
    201537: 89 05 2b 22 00 00            	mov	dword ptr [rip + 0x222b], eax # 0x203768 <a>
    ```
    instead of my:
    ```
    5992: 48 8d 0d 83 a6 00 00         	lea	rcx, [rip + 0xa683]     # 0x1001c <a>
    5999: 89 01                        	mov	dword ptr [rcx], eax
    ```
    which is tighter code but won't work if it ends up being a dynamic import. 
    so maybe ive just been doing extra work this whole time and im supposed 
    to just only bother with static data? 
  - reassured that clang doesn't fold the store if you give it this:
    ```
    extern int a;
    void foo() {
        a = 42;
    }
    ```
    so i think im more right than qbe here. and it does use R_AMD64_REX_GOTPCRELX, so that's compelling. 
  - so i can use that reloc and go back to converting to load which works as well, 
    and linker converts back to lea when it discovers it's local. 
    i feel like that's better
- also i was always using R_AMD64_PLT32 for any import which works for all my tests but must be wrong? 
  plt = procedure linkage table = the stubs thing, which isn't ok if it's data rather than a call. 
- arm64 elf relocations for Call and InReg.
- changes to support linux on arm
  - make entry point just pass sp directly
  - linux stat struct
  - clear_instruction_cache
    - glibc won't give me a `__clear_cache`, maybe there's a secret syscall for it (the table i use has one for riscv)?
      but that seems bad for business and it's not what gcc does for a c program that calls `__builtin___clear_cache`, 
      so i feel like its not a crime to just do whatever the magic instructions are youself. 
    - ah consider, if you make `__clear_cache` not #weak and use a linker, i do get one (and it works, i can jit helloworld),
      so like it only exists in magic builtin land so if you try to dynamically link to it you don't get one? 
      but im not using the c compiler part so it's not builtin to the frontend.. it's like a linker builtin? 
      that's very strange. even so, it seems weird that weak symbol means you can't get one statically. 
    - but on the plus side, since it will only give it to me by pasting it into my binary, 
      it's very easy to see what it's doing. now get to play my favourite game and pick out 
      the instructions that have interesting looking mnemonics and look them up in the pdf 
      - `mrs CTR_EL0`: Move System Register; Cache Type Register. D19.2.34
        - like i did for pthreadjitwriteprotectnp but a different one
      - `dc cvau`: Data Cache operation; Clean data cache by address to Point of Unification. C5.3.23 
      - `dsb ish`: Data Synchronization Barrier
      - `ic ivau`: Instruction Cache operation; Invalidate instruction cache by address to Point of Unification. C5.3.35 
      - `isb`: Instruction Synchronization Barrier
    - seems to work! very pleasing. 
  - now i just need to catch illegal instruction signals for when i mess it up. 

## (Jun 27)

- Experiments with the stupid skiping one the first symbol in macho symbol table
  - no skip: the first function when you `objdump -d` (on the .o file) is always called `__text` instead of it's real name,
    but it's correct after it goes through the linker. 
  - skip: `objdump -d` (on the .o file) shows the correct name of the first symbol, but 
    after it goes through the linker it's called `_junk` in objdump 
    and the empty string in clion's profiler (dtrace?) 
  - either way, lldb can set a breakpoint. 
  - clang always skips one called called ltmp0
  - when you skip, if you look at the symbol table in the exe after the linker, you can 
    see that it helpfully swapped the order of them so the nice named one gets skipped. 
  - somehow it can tell if you just do `symbols[0] = symbols[1]`, it wants a fresh name 
    as an offering? what is going on man? oh maybe it's like elf and it just wants a null
    value for missing name? yeahhh, if you just put a 0 byte before you start doing strings, 
    and then everything works. so why isn't that what clang does?? clang just wants to eat your first name. 
    - there's also the confusion of my objdump is `Apple LLVM version 16.0.0` and my 
      clang is `Homebrew clang version 19.1.7` so maybe they changed thier mind between there? 
      but that seems unlikely to me. zig's clang version 19.1.7 has the same eating a symbol behaviour. 
  - profiler still can't see the symbol names if i make the exe myself instead of through linker,
    but it couldn't the old way either so not any worse off. 
- opt/slots.fr handled overlaps wrong

## (Jun 26)

- problem with the compiler itself is that im not handling negative addends correctly, 
  but i feel like it shouldn't be negative, you're not going to offset backwards from 
  a symbol unless something fishy is going on. ah, it's when the instruction has another 
  immediate after the displacement, distance is always from the end of the instruction. 
  oh and the displacement is where clang's floating -4s were coming from, i guess 
  mach-o gives you that implicitly because you always want it. 
- now the linker accepts it but the resultant exe crashes. 
  even sudoku.fr crashes so it's something pretty fundamental. 
  - not getting `__errno_location`? but with -syscalls crashing in new_arena_chunk, 
    allocator's giving back null? but it should be the page allocator which shouldn't be returning null ever. 
  - mmm, i think that's a different problem, i think -syscalls doesn't work at all with -c, 
    maybe something about the universe shifts and my shitty os detection doesn't work anymore? 
    yeah if i assume linux and don't check issanepointer more programs work. 
    so the errno thing was it using the apple name instead of the linux name. 
  - ok so it's wrapping me in `_start` which does some random shit first and confuses franca_runtime_init,
    everything works if i say `-Wl,--entry=main`, so that's nice. 
  - can't just call my main `_start`, then it says duplicate symbol. 
  - ok that's fine, if it looks like the args to my main are from a hosted libc,
    instead of just assuming macos, check past the end of the environment variables, 
    and if it looks like a pointer, it's apples extra info and if it's a low number, 
    it's a key in auxval so we must be on linux. that seems to work everywhere. 
- TODO: elf_loader.fr doesn't work on linker output: `panic! not divisible by page size`
  is it ok to just align backwards the vaddr to make them page aligned? 
  that seems sketchy but idk what else you'd do since they have different memory protection
- don't bother with mach-o code signeture on amd, it only cares for arm
- zig 0.14.1 fixed `/zig_install/lib/libc/glibc/sysdeps/riscv/start-2.33.S:48:2: error: unknown directive`,
  so it can target riscv now which is nice 
- i want exe_debug_symbol_table to work for elf.
  - seems you can just stick another symbol table in there. 
    gnu's objdump puts names on call targets but not at the start of each function and llvm's objdump does neither.
    oh it just didn't like that i was giving it two .text sections that overlap i guess? 
    previously i was doing mark_code_sections just to make the disassembler work. 
  - painful offsetting to get import stubs to disassemble correctly.
  - now it works in objdump but not lldb (you can't set break points, etc).
    a zig binary that works has `.debug_info, .debug_abbrev, .debug_aranges, .debug_line, .debug_ranges, .debug_loc, .debug_str`,
    that allegedly have dwarf stuff, maybe lldb only looks at those and not at symbol table, that would be sad.
    ah! gdb works with what im doing now... that's annoying. maybe i just give up on lldb for now. 
- seems like a good time to do arm linux as well. 
  compared to mach-o, there seem to be more relocation types so it doesn't have to look in the 
  instruction slot, like `b` and `bl` are seperate instead of putting that bit in the code. 
  well exe only needs glob_dat
  - without -syscalls it segfaults in prefer_syscalls, with -syscalls i can helloworld. 
- added `-force_linker` option to backend/meta/test.fr so you can make an exe via relocatable 
  for all tests, not just the ones that have a c driver. which i think is the only thing 
  that tests the output of my c compiler with relocatable but more importantly, lets 
  me run the .ssa tests on linux before i have a wrapper to fix the stack. 
- deciding i have to evaluate the left of an assignment before the right,
  (because otherwise you have a horrible aliasing problem like zig does with RLS). 
  the unfortunate side effect of this is for tagged unions, the old way it would just 
  poke in the new varient and leave the old padding bytes and the new way 
  initializes the whole new thing on its own and then copies the whole size of the 
  largest varient into place, which is wasteful but also there's places 
  where i was relying on holding a pointer into the active varient
  while stomping over it with a smaller varient, and then reading from a 
  later field of the old varient, which really only worked by luck and is too fragile 
  to let slide now that i noticed it even if i change my mind about evaluation order, 
  but it's tedious to untangle. 
  costs 20ms on self compile which is almost 2% which seems really high, very sad, 
  cause like such a large amount of code doesn't care, but the old way was such a footgun,
  it being a rare footgun just means it's that much more confusing when it goes off. 
- i feel that revealed a bug in late slot promotion perhaps or im just losing my mind,
  elf/emit/seal_dynamic() only works if you force it to spill everying and i don't see how 
  the difference could be caused by evaluation order change. 

## (Jun 25)

- machO: when direct exe, make symbol names for import stubs so when you look at the disassembly it 
  shows as a call to a sane name instead of treating all the stubs as one thing so you can't tell whats going on
  - pleasingly, i do this better than clang which seems to always say any call to a stub 
    is a call to the first import + the stub's offset. maybe there's some magic flag you 
    have to pass it? idk. tho i guess that's an argument that i shouldn't make people say -keep-names,
    because if someone tried my thing they'd probably think i did it wrong too because they wouldn't know to ask for it. 
- trying to make relocatable elf file. did some refactoring to make elf/emit.fr make more sense. 
  - lots of `ld.lld: error: undefined symbol: main`, i can mark it GLOBAL instead of LOCAL, 
    and then i need to reorder it to be at the end or it complains like `non-local symbol (2) found at index < .symtab's sh_info (3)`
    and then also have to decrement local_count to account for that and then it links successfully. 
    so i guess elf doesn't have a specfific thing for exports vs imports, you just put the global 
    symbols at the end and if its section index is non-zero that means it's defined and thus an export instead of an import. 
  - program still doesn't actually run but hey you can't win 'em all. 
  - looking in lldb, it is getting to what the exe says the address of main is, but that's unmapped and way lower than `_start`,
    oh maybe i need MYSTERY_SPICE somewhere? but clang's .o files have low addesses so i feel like it's fine
    ```
    * thread #1, name = 'a.out', stop reason = signal SIGSEGV: invalid address (fault address: 0x6c7a)
      * frame #0: 0x0000000000006c7a
        frame #1: 0x00007ffff7df0083 libc.so.6`__libc_start_main + 243
        frame #2: 0x00000000002014aa a.out`_start at start-2.33.S:120
    ```
  - tried adding Write,Alloc, flags to the section, still crashes but now it gets to franca_runtime_init. 
  ```
  * thread #1, name = 'a.out', stop reason = signal SIGSEGV: invalid address (fault address: 0x0)
    * frame #0: 0x0000000000209aab a.out`franca_runtime_init__2175 + 355
      frame #1: 0x00007fffffffe400
      frame #2: 0x0000000000209c95 a.out`main + 27
      frame #3: 0x00007ffff7df0083 libc.so.6`__libc_start_main + 243
      frame #4: 0x00000000002016da a.out`_start at start-2.33.S:120
    ```
    the instruciton is 4 of ` add    byte ptr [rax], al` which is definitly what 0 is,
    because i was zeroing fix.patch_at for RipDisp32, which should just be the offset which the linker pokes in. 
    i bet the relocation isn't happening and trying to do ` mov    qword ptr [rip + 0x00000000], rcx` decodes to something 
    else because saying displacement of zero is useless. 
    readelf seems to see my relocations, maybe i just guessed the type wrong. 
    on apple which works it says that instruction is supposed to be `100006acc: 48 89 0d 1d 97 ff 03        	movq	%rcx, 0x3ff971d(%rip)   ## 0x1040001f0 <_g67+0x1f0>`
  - clang says `addend = -4` a lot which is creepy to me but doing that doesn't help
  - another fun bonus is that old-real-gcc still thinks `undefined reference to main` even tho clang
    accepts it and successfully gets to main, so that's interesting. 
  - and zig's clang 20.1.2 seems to have regressed compared to zig's 16.0.6,
    now you can't give it a file with the extension `.out` instead of `.o`,
    now it says 
    ```
    ld: warning: -m is obsolete
    ld: unknown option: --hash-style=gnu
    zig: error: linker command failed with exit code 1 (use -v to see invocation)
    ```
    which is not how i would personally spell we care about the file extension now but hey who am i to judge. 
    but if i call my thing `.o` instead, it accepts it and just drops the relocation like before, 
    so no insight about my gcc woes. 
  - maybe i have to do something to ask for a got slot (my macho/emit has a big comment about track_got_reloc),
    but it seems to be dropping local function calls too, if i switch the order of functions in hello.ssa
    so it doesn't get inlined, but no it's getting to franca_runtime_init and calling is_sane_pointer__6380 just fine,
    so what the hell? 
  - call1.ssa is `unknown relocation (6) against symbol add_one`,
    it doesn't like R_AMD64_GLOB_DAT? but that works for exes. 
    ```
    0000000000d9  000300000001 R_X86_64_64       0000000000000000 .text + 20
    2: 0000000000000020    12 FUNC    LOCAL  DEFAULT    1 add_one
    ```
    it seems it's doing 1 instead of 6 ok sure i can do that, now clang aborts. fantastic. 
    gcc says `reloc against 'v': error 4`, fucking useless, somehow still more helpful than clang. 
  - YAYYYYYYYYYYY i was still doing virtual offset from start of code instead of file offset from start of segment,
    hello.ssa works now! and so does franca hello world. i typed a comment that i had to change that
    offset math early on but i didn't actually do it, very sad day. 
    - and now after fixing that clang no longer crashes if i try to not align_to the segments
    - and of course you can't say exports are global when making exe, that freaks it out too, 
      or maybe i just fucked up and didn't change everything on that codepath, but anyway broke the tests
      fixed the tests all is fine. 
  - linker doesn't work on the compiler itself tho, that would be too good to be true.
    ```
    LLD Link... ld.lld: error: a.out:(.text+0x1cffd4): relocation R_X86_64_PC32 out of range: 4295075980 is not in [-2147483648, 2147483647]; references g4766
    >>> defined in a.out
    ```
  - and .ssa auto tests still don't work (i would expect the non-driver ones to fail 
    because they don't have franca_runtime_init to fix the stack before calling into libc)

## (Jun 24)

- give up on my failed experiment insane way of doing llvm abi 
  (it kinda worked at one point but it didn't spark enough joy to finish and has rotted since then). 
  i'll probably change my mind again, who knows. but 20k lines of backend is getting to be too much 
  and riscv is more interesting to me than llvm. 
- something is fucked with specifically orb-dyn hello world, static works and dyn mandelbrot works, 
  and in fact all the tests work, just that one program is cursed somehow. if i convert bit by bit 
  from mandelbrot to hello world, it seems you are required to call `temp()` or the program doesn't work. 
  and temp(), context(), borrow() inline and disappear so it's actually the same code in main(), even creepier. 
  weird that a bunch of stuff changed in the binary then. 
  my theory is that the first call to each intrinsic doesn't get inlined 
  (if the caller is emitted before the callee is sema-ed) 
  and if that happens to be the set_dynamic_context in impl2(), you're totally fucked 
  because to get the current os from that to choose syscall numbers. 
  indeed an extra push_zeroed_dynamic_context{} fixes it, as does just calling set_dynamic_context at comptime
  which has exactly no effect other than to force it to go through sema sooner. 
    - that must be the problem i was having with the graphics programs as well. 
      being very compilation order dependent fits the symptoms. 
- avoid an extra clone of all function bodies (at top level + no const args + no #where): 1294 functions, <10ms
- elf_loader: implement dynamic imports
- added a test that runs dump_elf and makes sure my static binaries are actually static

--- 

- fuck; all kinds of stuff going wrong at once: 
  - load_elf doesn't work on real linux but works on orb
  - static linux "Tried to allocate 67071058 bytes with panicking_allocator" in amd64/emit, but it works on orb. 
    - on my ancient linux and blink
    - self compile on blink takes 3 minutes before getting to that point,
      precompiled kaleidoscope.fr repros the problem fast. 
  - dynamic doesn't work on my ancient linux new one segfaults, 
    the old one that boot/temporary downloads says `symbol lookup error: ./target/boot-linux-amd64-dyn.out: undefined symbol: fstatat`
    which would make sense that the new one would try to call null because i don't handle a missing weak import gracefully,
    but `ldd --version` says `Ubuntu GLIBC 2.31-0ubuntu9.18) 2.31` and the internet says fstatat "was added to glibc in version 2.4",
    so mines from 2020 which is old but sure after 2006 and indeed a c program can have fstatat, just not me.
    tho.... `3: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND __fxstatat@GLIBC_2.4 (3)`, 
    that's sure not what i would have tried to call. another `$INODE64` moment i guess? 
    but it does also say `59: 0000000000001220    25 FUNC    GLOBAL HIDDEN    16 fstatat`,
    `zig cc f.c -target x86_64-linux-gnu -Oz` just wants `2: 00000000002015cd    37 FUNC    LOCAL  DEFAULT   14 __fstatat64`. 
    confusing because there's a syscall called "fstatat64" which doesn't exist on amd64, but i guess 
    glibc exposes the syscall called "newfstatat" as a function called "fstatat64" and not as a function called "fstatat"? 
    wait no that's LOCAL, disassemble it and it's doing the syscall itself even tho it's supposed to be dynamically linked. 
    so maybe fstatat is just a magical thing you can't trust libc for somehow? 
    ok whatever, just always do the syscall, now dyn and sta both have the panicking_allocator problem
  - panicking_allocator problem is because it gets suck the for_bits_rev loop when handling ret0,
    simpler repro is backend/test/folding.fr/unit(). 
    clz doesn't work on some versions of linux?? that seems untrue and impossible. 
    like theres the thing where my LZCNT will be BSR on old cpus, but that's only wrong for 0 which i carefully 
    don't call it with.  
    see ok this sentance is not the way i would phrase this information at all: 
    "LZCNT differs from BSR. For example, LZCNT will produce the operand size when the input operand is zero. 
    It should be noted that on processors that do not support LZCNT, the instruction byte encoding is executed as BSR."
    because when i read i decided that meant "for inputs other than zero, LZCNT is the same as BSR" but what it really means 
    is "for all inputs BSR and LZCNT are totally different and for inputs other than zero 64-BSR = LZCNT", 
    which like ok that is my fault for not thinking about the sentance hard enough to notice that
    index of highest set bit is the opposite thing as number of leading zeroes because 
    index counts from the right. but like it's phrased so similarly to the one 
    for tzcnt which is in fact the same other than for zero. 
      - it's confusing to me that blink has a function called AluLzcnt when it seems they don't support that instruction, 
        but also clearly i don't understand what's going on because it uses a macro called bsf() which is sounds wrong 
        for supporting it or not supporting it but the numbers it gives back are right for BSR as though LZCNT were not supported. 
        so maybe im still totally wrong about what the instructions are supposed to do? who knows man, i give up. 
    - blink does the right thing for tzcnt(0) it seems but ancient computer gives me junk so did similar thing with checking the flags
  - so all that and now im at the point that i can discover that my elf loader works on fake linux 
    and my real linux but not on github's real linux

--- 

- tiny test just to make sure my silly double emitting a symbol as export+local in machO works.
  - fix import_c to let you cast a function name as though it were a pointer
- when doing mach-o exe directly, include symbol table of local symbols and functionstarts even tho they're 
  not required because they let you see them in debugger and disassemble the right parts. 
  so now i don't have to go through relocatable object and linker just to use lldb, which is good 
  because it's kinda a last resort so if im at the point of needing lldb im not in the mood to running thier linker too. 

---

> i love when i let zed update itself and they find new and exciting ways to turn on shitty auto complete
> it's not show_inline_completions=false anymore and show_edit_predictions=false also doesn't work. 
> ah of course show_completions_on_input=false, totally unrelated to edit_prediction_provider=none;
> losing my mind. oh boy and now they have a debugger so i have to turn off the giant circle every time 
> you mouse over the side of the screen. i really really hope they fixed the burning a core when 
> another program writes to a file. signs point to yes!
  
## (Jun 23)

- plumbing for FuncImpl.Asm.rv64
- split elf/emit into smaller functions to refamiliarize with it before i try to support relocatable. 
  - had to do it eventually anyway and i think using zig to statically link and run the .ssa tests is an easier starting point 
    than either a) dealing with an emulator with a dynamic libc or b) trying to make everything work at once and use the 
    franca lib/sys/ impls. 
- wasted so much time on something that could have been caught by the slice functions bounds checking that 
  the indices are positive. so finally fixed that. my original bootstrap bytecode only had signed comparisons,
  but that's been gone for many months now. should have done this so long ago. 
- started examples/elf_loader so i have at least some reference for what's going on like i do for mach-o. 
- little thing to dispatch to the right program to dump an executable / compile a program because it's just a little 
  bit too much brain tax to type a different word every time

## (Jun 22)

- the risc64 syscall numbers are the same as the arm64 ones. that's nice. 
- finally fixed `rosetta error: bss_size overflow`, it just doesn't like an empty bss segment
- random cleanup: use a bit mask instead of `Target.(n(g,f)pr, (g,f)pr0)`, 
  makes it more clear that being consecutive doesn't matter, since rglob can mask things out anyway. 
- started riscv emit, instruction encoding, etc
- trying to do hello world with write+exit syscall and run it in https://github.com/libriscv/libriscv
  - libriscv says "Exception: Invalid ELF section header string table index" if you don't give it at Elf.Header.section_header_names_index,
    because it wants to look for a `.interp` entry. real linux kernel doesn't care for static binaries but sure whatever. 
  - yay! hello world works. 
    - interesting that write syscall to stdIN shows it to you on orb and blink but not on libriscv

## (Jun 21)

- more playing around with the byte-prefixed-paths in deps, so `@/` means a source file and `!/` means a cache file. 
  should really have types tho. it's error prone to pass around strings. 
- figured out why `NOCACHE franca ../cc.fr a.c -r` was crashing: codegen_thread_main was calling write_protect for any jit, 
  not just if expecting_wx=true, and run_franca_file was setting expecting_wx when it didn't need to. 
- fixed tracy zones in report_aot_progress to include the compiler identity so it works when you have nested modules
- taking a break from module stuff. stealing to steal riscV abi/isel from qbe. 
  (i wonder if this will be the arch that makes me get strict about unaligned loads)
- bit of cleanups found for other arches
  - abi: use List(temp()) instead of linked list for stkblb insertions
  - abi: top level loop is the same for arm,amd,rv
  - isel: ^same
  - i don't like that my fuse_addr for arm looks a defining block and only does it for local things but the amd one doesn't. 
    notably qbe's amd one (with the hella generated code matching language) does `anumber` on one block at a time which i 
    think has a similar effect of only looking at local ones. both of mine look at Tmp.def which is probably not ok if it's 
    an already processed block that got reallocated, and the arm one is careful not to do that but the amd one isn't. 
    and the arm one breaks if you take out that check, so how does amd work?
    i guess it's actually fine if it reallocates because it's in an arena and you just see the old one, 
    the problem is if it doesn't reallocate but new instructions caused it to shift and now def points 
    to the wrong thing and the amd one does detect that, it checks `get_temporary(r).def.to == r`. 
    so clearly i knew this before i just didn't write it down. doing the check against def.to seems to generate the same code so that's good i guess. 
    wrote it down in the code this time. in arm, the thing it's doing is just checking if the tmp is an offset from another,
    so could make sure that's up to date in Tmp.alias and then have a better chance of actually triggering it across blocks.

## (Jun 20)

- converting backend/lib.fr to something you can #use instead of #import_std. 
  - not needing to import it at top level scope for every graphics program is a big win
  - i don't love that now every file in the library has to import the library into itself, 
    but it's not any worse than when most languages do where you put what module a file is a part of at the top, 
    i just don't have a unique syntax for it. 
  - once again reminded that i really really need to replace the order dependent explicit generic instantiations thing. 
    it's so tedious any time you change anything. sucks so much. 
- trying to use some backend functions through import_module and some normally, super doesn't work. 
  it thinks there are conflicting overloads or multiple versions of the same type. 
  i think @rec is a problem, the new CompCtx is doing QbeModule twice with different values, in different scopes.
  one in the import scope and one in TOP_LEVEL_SCOPE. ohhhh fuck, it's because i #include_std in cached_compile_module, 
  but other files in the backend #use backend/lib.fr and i don't deduplicate between those. worked for import_c/ffi.fr 
  because the rest of import_c never imports that file. but that's fine, the only reason i was doing #include_std instead of 
  import() was because import_c depends on the backend and the backend expected itself to be at top level but now it will work in a scope. 
  not going to bother with a sane error message because the whole point of this is to get rid of #include_std 
- maybe i want a thing that means #use+#reexport+(make the target #use myself as well), that would make it less 
  painful to replace all the #include_std, cause it would mean a similar thing: go load this file such that we can 
  see each other's names. but doing that through a ScopeId indirection would mean you could still access it with import(), 
  which include_std doesn't let you do (which is why i can only deduplicate within #include_std OR #use but not between them). 
- brief interlude; The thing where i ctrl-c my make file and it leaves a garbage process spending 100% of a core in "system" 
  is so annoying. the only thing i can think of is maybe i fucked up my signal handler when i tried to make 
  stack tracing use debug info instead of for_symbols, but it happens even without FRANCA_BACKTRACE,
  oh it's definitly my shitty message reminding you the envvar exists but now doing any syscalls needs a dynenv. yep.
- (back to import_module of backend). it's trying to emit the quicksort wrapper that returns a funcid, that's odd. 
  it gets past that if marked #fold but it really shouldn't have been on the work_stack because it could only have been called at comptime. 
- ok works for calling try_fold through the import_module, comically takes 60% longer the first time and 10ms longer even when cached. 
  but that makes sense, doing it for just that tiny function is a waste. 
- import_frc stick the hash of all deps of the imported module in the target module as a new dep so you rebuild when transitive dependencies change. 

## (Jun 19)

- get a bit more serious about not leaking memory. it doesn't matter when you just run the compiler once 
per process but compiler/tests.fr uses a bunch of compiler instances and was growing up to 4gb which offended me. 
  - only interesting thing was ArenaAlloc.deinit, i wasn't going forward all the way in the linked list so you'd lose 
    things if you reset_retaining_capacity before deinit. 
  - i didn't know you could `/usr/bin/time -l` instead of the boring builtin time that just gives you... the time. 
    last commit: 3,279,257,600 -> 1,544,503,296 maximum resident set size for compiler/test.fr.
- going to get rid of my hacky ProgramIndex that i was using for my lsp attempt a long time ago. 
  new idea is to add a section of frc with whatever info is needed so you can record a compile and then have the lsp load that. 
- i want all early passes to be target independent so abi0 is a problem (in theory; i don't actually use it so it doesn't even matter). 
  i think it's fine to just do apple_extsb always. the other one just drops 8/16 args. 
- cleaning up some of the old broken tests
- trying to make parsing_large_things test less embarrassing.
  it makes a 200k array of sequential floats. so 200k unique constants and 200k store instructions in that function.
  - inlined the one ir instruction for index_unchecked
  - hashmap in backend/util.fr/newcon instead of scanning them all to add one (can't be doing n^2 when n=200k). 
    (also this fails some of the franca tests? but not the ssa tests... should look into that).
  - reuse bitsets in backend/opt/spill.fr, especially good because you immediatly copy to them so the zeroing in bsinit is waste.
    now it finishes in 40 seconds but "we don't allow a function larger than one arm64 instruction can jump". (whatever, i still want to make it faster). 
    has 800144 tmps by the time it gets to spill(). really what we're learning here is my bit sets are terrible at sparse data.
  - i was hoping i could just track max and min but that doesn't really help, it's not chunked like that. 
    also tried always skipping a few from the bottom and not tracking those in min because like at least the bottom u64 is 
    always used because it's for physical registers. but also not helpful. can't decide if i care enough to try a real sparse bit set. 
    is there anything better than just hashset of numbers? 
- the problem of how to unify identity of types for import_module was shockingly easy to dodge. 
  only care about the signetures of the exported functions so just evaluate them on both sides and stomp over the imported types. 
  the frontend typeinfo in the .frc file is only useful when it's like import_c generated from another language so 
  there's no existing source of truth for it. 
    
## (Jun 18)

- i want to stop doing follow_redirects in emit ir because i think i barely use #redirect and it's kinda weird, 
  but that makes it ~20ms slower which seems unreasonable for how tiny the things i use it on are. 
  probably just that i can't inline through it. fixed by copying SymbolInfo.inline in worker/CodegenEntry.Bounce, 
  now they're the same speed. 
- i still feel like there's a bunch of small things that should be inlined but aren't. 
  - there's only 8 functions where that inline forwarding made a difference, so if that's 20ms, 
    maybe whatever the ordering problem is will be even more. 
  - i was looking at the ir for lex_int yesterday and le in is_ascii_digit isn't getting inlined, and that's not even a #redirect. 
  - calling through function pointers won't go in callees and gets picked up later by EmitIr.pending, 
    but that's not what's happening for the tiny things that offend me. 
  - i guess the problem is if they go in the list early and then other callees that get emitted first 
    also call them, they don't get pulled forward in the list. so instead of just pushing new callees
    to work_stack, callees that were already there should get pulled forward as well, so tiny things that many people 
    call are emitted first. just have to be careful you don't get stuck in cycles, so like only let a function pull 
    its callees forward the first time you see it in that loop, but you let a callee get pulled forward as many times as possible. 
  - ha! (time: 1106 -> 1014, bloat.fr: 982152 -> 971472)
  - not bothing to check if things from `pending` are already on `work_stack` also helps a bit. 999
  - (still din't fix the lex_int thing i was complaining about)
  - unfortunate that the reordering messes up tracy 
- fix error_reporting.fr/find_function() to not call for_symbols (reuse my nicly fixed find_ip_in_module)
- start working on smaller compilation units. main usecase is i don't want to recompile the c compiler from source every time you import a c program.
  - yikes noticed `<<12` vs `<<16` typo for offset from a symbol. should add a test for that. 
  - when outputting function types into root_scope, imports need an extra entry, like if funcimpl=DynamicImport, 
    theres a mangled name and an unmangled name that both need to have typeinfo so we know its a function when loading and it's allowed to have a shim. 
    but now the thing im doing in @syscall is really annoying because it directly makes a callable Expr::DataSymbol, 
    so there's no Func that shows up. so i could just have it note that it's callable and give you the function type, 
    but really it should be legal to import an untyped symbol and cast it to a function pointer. 
    which kinda should just work, i just have to put something in the root_scope so the importer knows it exists. 
    // holy shit this is annoying. 
    // the problem is when it's not a function, i can't just make a shim for it,  
    // and since it's an import it won't have a bakedvarid to track.  
    // maybe the sane thing is to do this in move_from_module, but you can't make an new entry there  
    // because they need to be ordered or you'll have repro be a race between threads.  
    // the specific problem im having now would be easier to fix by just making @syscall make a Func body  
    // for it but you'll have the same problem if you actually try to import data.  
    // i should just do a massive refactor of how the frontend represents symbols but that's such a pain  
  - so that works as a proof of concept. not impressive the first time, it's not parallel so it's even slower because you need to start a new 
    CompCtx and do redundant work on stuff like @match and @syscall, but it opens the door to doing something better. 
    i do kinda hate it because im just reinventing linkers, but it was getting a bit dumb that the tests take a whole minute 
    to run because they recompile the backend 23 times, etc. etc.
- something's very broken with examples/bf/c_string.fr, but the more complicated tests seem to work (other than amd64), so thats a bit crazy town. 
  problem was that i was starting a new compiler instance sharing the BuiltOptions which includes ComptimeEnvironment which has some 
  local indices to blessed types set in bootstrap_compiler_environment, so since compiler/tests.fr also calls main_thread_pump multiple times, 
  the later ones get the wrong franca_runtime_init FuncId. 
- insane early return bug: when you fall through you don't get a new block so if you have the right level of nesting 
  theres a brief window where new code gets inserted before the terminator. but @if gives you a new block, 
  so it was only a problem if you unconditionally early return from inside an inlined closure to the outer function
  so the frontend doesn't notice the code after that is unreachable. 
- take out the junk blocks for unused return labels earlier. doesn't matter, just makes the debug output easier to read. 
- not sure if i should bother caring about got_indirection_instead_of_patches on amd64

```
//       i think a better system might be to just do the syscall instruction with AsmFunction instead of doing it in the backend. 
//       my original thought was you want to expose that so you could write a frontend for another language without depending 
//       on the franca library at runtime and still be able to do syscalls without caring about the arch calling convention. 
//       but since you need different numbers/struct layoyt anyway, it's such a pain in the ass to do it right that the extra 
//       help the backend gives you is kinda just a drop in the bucket, why bother. 
```

## (Jun 17)

- fix bf2ir and kalidescope to pass environement pointer now that i need it to print characters.
  - extra confusion because bf2ir uses import_frc which wasn't setting FnFlag.YesContext, so emit_ir 
    was trying to helpfully guess that you didn't need the evironment pointer because it used to 
    be more expensive to pass before i made regalloc do fewer redundant copies. 
    tho this means varargs on amd64 will be even more a problem because both want rax. 
    maybe could use a high float register? is it slow to move between int and float? 
- now that i can get rid of #syscall, i can also get rid of FuncBody.Merged. 
- now that find_or_create_impl doesn't do anything interesting. inline it everywhere and give better error messages. 
- make trying to AOT a comptimeonly function a hard error. will have to go back when i do more caching but for now this is the sane option. 

## (Jun 16)

- find_ip_in_module: use debug_headers instead of for_symbols, so it's not completely screwed if you crash in the body of a use_symbol. 
  - as a test, reintroduce yesterdays problem with load(inlinable), 
    and now i get a full stack trace instead of getting stuck. fantastic. 
  - still sketchy if there's another thread trying to output debug info while you're reading it but baby steps. 
- report the first ip as well. originally i skipped one because it would always be the panic() which is useless 
  but now i do it for segfaults, etc. as well which have a meaningful starting ip. 
- did the last step of storing both arches MachineCode so frc_inlinable can be used on either target. 
- kinda dumb that you have to use qbe_frontend instead of the compiler to run a non-driver cache file. 
  can have runtime_init detect that situation and do the right thing. 
- experimenting with @syscall as a macro instead of compiler magic. 
  - the only problematic dependency is that scary_log calls write and panic calls exit.   
  - now that weak symbols are referenced even when -syscalls, need to handle that properly in elf/emit.fr, 
    so you have to tell the backend if you're expecting a static binary instead of telling the frontend. 
  - hmmm, i think trying to complicate the runtime dispatch of syscalls causes you to have callee saved registers 
    in the function that calls clone() which manifests as pushing them to the old stack and then trying to pop them off 
    the new stack (which wouldn't have them anyway) and underflowing and then returning to whatever the next junk happens to be. 
    same experience as when i did it the first time and wasn't setting a new stack and the threads were racing to return. 
    need to inline everything up to calling the new thread's entry point. some weirdness about needing a body to have a return label, 
    but it works.
  - trying to deal with cycles for exit(), write(). 
    - for panic() calling exit(), hack to experiment is just put a loop{}, but actually just forcing a jit shim should achieve also solve the problem?  
    - `panic! Unfinished type 208` when trying to call current_compiler_context(), but i don't understand whose calling exit. 
      ahhhhh, get_offset_to_dynamic_context_field() isn't a builtin. 
    - "Type Error: found tuple but requested non-tuple i64", only one ive tried that has a single argument to Expr::Tuple complains. 
    - current_arch() does @bits which does a @ct_assert so it needs to compile format_into, i was annoyed by that before in the profiler.
    - somehow i've cursed ./target/boot-macos-amd64.out to get stuck forever, the linux one is fine tho. 
      it doesn't like exit causing a jit-shim maybe (directly or with #libc). 
    - egghh and just doing exit syscall breaks the tests that try to call libc putchar because it tries to buffer it. 
      as good a time as any to get rid of one more libc thing i guess. 
      but the whole point of sticking to the libc interface is that i want to be able to work and play well with others. 
      if i dont know why it doesn't work in the old compiler, seem like theres a risk of the problem resurfacing in new compilers. 

## (Jun 15)

- stop using libc rand(). found a short one from the internet that seems to produce numbers. what more can i ask for. 
- fetch_or_crash: don't exec `shasum`, just use my own Sha256 implementation
- it's nontrivial to find a c program that can open a zip file and is not distributed in the form of a zip file. 
- did userspace_target_os() as a tiny test just to make sure it's not a big expressiveness loss
- removed #link_rename, same rational as #target_os, but easier, last usage was just for objc_msgSend. 
- trying to sequester all the syscall definitions as a chance to make it more clear which places use them, 
  now they have to explicitly reference that scope. 
  - call exit() less often. seems rude to not call the panic hook. 
  - something of those changes made `examples/import_c/cc.fr -frc_inlinable` fail a bounds check. 
    changing compilation order made one of the CodegenEntry.Bounce end up in there and not doing 
    default_init() when !did_regalloc is fine when the loader is expecting it but this was unequipped to deal somehow i guess. 
    tho also, ow that i notice it, using QbeNull as sentinal for Bounce is bad if it can be mixed in with inlinable stuff. 
- a userspace version of #asm that makes the FuncImpl directly (so replacing the sema part, not the emit_ir part), is super easy, that's pleasing.
  - im not even sure why i did it the other way before. i think because i was trying to not have an intrinsics so even stuff like 
    `add(i64, i64)` was done with #asm so you couldn't compile any comptime stuff at all before needing to call an #asm function. 
    but the more formal ir has been very successful so i don't care about retaining the ability to do asm before bootstrap_compiler_environment.
  - you lose the ease of having them be independent function declarations but i think that's fine, 
    i really want to get rid of `sema.fr/do_merges()` because there's a bunch of situations i don't trust that it works. 
    i have a lot of trouble with the columns vs rows organisation choice. this might be a step in the wrong direction there. 
    but the old way wasn't setup to let you add an architecture without the frontend explicitly supporting it either so 
    the sane thing has got to be to provide the current features as simply as possible, not let more complexity sit 
    in sema just in case it maybe becomes useful later. 
  - tho also now that i think of it i can just let you pass two funcid's and read the body in the old format. 
  - a bit dumb that i have to manually write wrappers to put a FuncId in an overload set tho. 
  - this whole thing  freezes the layout of Func even more which maybe isn't the best plan. 
    maybe before next updateboot i should provide compiler apis for setting useful combinations of FnFlags. 
  - for small things, the new way shoves everything together such that it's impossible to read somehow. 
    maybe im just not used to it. but the old one was kinda ugly too. so making it more in userspace instead 
    of a blessed compiler thing makes it more clear that it would be reasonable to replace it with a real 
    assembler in comptime code.
- i kinda want something like #inline but instead of doing anything, it just asserts that it was automatically inlined.  

## (Jun 14)

- continuing: t.finish_module not getting set on amd if the module is on the stack.
  when i changed con repr, i changed `c0.bits.i += c1.bits.i * m;` to `c0.set_bits(c1.bits() * m);`, 
  which is super not the same, just didn't matter because const folding happens earlier so you 
  adding a symbol and a number doesn't show up. 
- made Target.caller_saved a bit mask instead of an array
- un-#target_os: find_os_libc_dylib, query_cli_args, get_executable_path, apple_thread_jit_write_protect, do_signals
  raw_create_thread, wake, wait
- so without #target_os, i need a new way of implementing query_current_os. 
  - i don't want to have the backend just have an op for it. eventually i might want it to not only target one at once 
    (tho that's hard because they have diferetnt ABIs as well)
  - first idea was try to find a syscall number that does different things everywhere so you could do it 
    and look at the error number you get back or something but didn't find a good one before i got bored. 
  - macos passes `char **apple` extra env vars as fourth parameter to main. maybe could do something with that. 
  - for now doing something super hacky with which arguments are passed to the entry point, 
    cause on macos the blessed interface is a c-like main(argc, argv, envp) but on linux-amd64, 
    the kernel passes those on the stack so the regisers values don't have the same pattern of pointer-ness. 
    seems to work? probably going to suck so bad to debug later when something breaks that assumption. 
  - the other problem is where to store it after startup. putting in the dynamic environment seems like what you want 
    but it sucks if you can't produce it out of the void in callbacks. putting it in a static doesn't really work because 
    you need to set it to the host in comptime but if that ends up in your binary it breaks repro. well maybe it's 
    fine, just have a bake_relocatable_value that zeroes it and then the first thing the entry point does is set it back again. 
    but then you need to be super careful not to do anything in comptime code before its initializer can be compiled. 
    and linking different modules together won't work because only one guy will set their own static in the entry point. 
    could do both, get it from dynenv but keep a static around to referesh it in case of emergencies? 
    it feels kinda gross for push_zeroed_dynamic_context() to set it for you but otherwise it's super annoying to remember. 
  - was afraid that thing with the arguments would mess up running a .frc of a main() program but it's fine because entry_sym 
    is the user main() not the wrapped main that goes in a real exe (even tho it's still in there). 
  - how can #target_os have only been 89 lines of code in the compiler? it felt so vaugly threatening to think about. bit anticlimactic. 
- looking through examples/bloat.fr on the compiler to see if any stupid stuff stands out
  - get_or_create_type: missing brackets so the fast path wasn't folded
  - init_driver_vtable: just fill in the pointers at comptime
  - there's really a lot of dumb monomorphisations, should do something about that. i used to have deduplication. maybe should bring that back. 
    something's going on with the ordering. like how are we not inlining all uses of ptr_from_int. 
    maybe the first usage where it gets instantiated, the caller gets emitted before the callee. 
  - 1021288 -> 1014176
- the thing where you don't get a panic message seems to be only the run where you created the cache file. 
  - it doesn't like make_dirs_or_crash()? but it's succeeding, it just curses you if you happen to panic later.  
  - eprintln() specifically doesn't work somehow?? it's fine if you make it println in print_and_exit1(), 
    why can i print to stdout but not stderr?? it's fine on linux. `ENOSTR: Not a STREAM`, im losing my mine apparently. 
  - oh it's fucking line buffered? what the fuck? it works if i put a \n in there. ok i can reproduce it in a stand alone aot program, 
    if you call read_entire_file on a file that doesn't exist, it fucks your stderr somehow?? 
  - so problem is definitely my SysResult thinks open() is succeeding when it's not and then somehow 
    trying to call lseek on a bad file descriptor cascades into making your stderr be line buffered,
    which sounds like im making shit up but that's what i see happening? 
  - ok so when i was redoing posix.fr, i switched to always doing openat as a syscall because varargs for `mode` is annoying, 
    but on macos it just returns the errno as a positive number and sets the carry flag which i don't bother to check for because i 
    mostly use the libc ones anyway, and ENOENT is 2 so if you're stupid like me and treat that as a file descriptor,
    you start calling lseek on stderr which fucks all your shit up apparently. fair enough i suppose. 
    agh which also means fix_errno just checking if is_linking_libc isn't good enough, it needs to know if that specific call was a syscall. 

## (Jun 13)

- need to make fold_constants optional if it's going to be using the module binary format, 
cause otherwise there's some parts i can never change since the source needs to match the old compiler. 
  - it's awkward because of amd64/isel trying to pull constant arguments into the instruction, so for unary 
    things like extub, i don't want to bother encoding them for a const arg because that's stupid, it would just 
    be folded. so if i want to allow disabling folding, i would need to add something that doesn't allow const 
    arg for unary, and then maybe you only turn that on when folding is disabled but that produces a codepath that's 
    only tested when you rebuild the compiler after changing module api which is not great.
  - before even getting that far, can't forget to mark_referenced which now happens at the end of folding. 
  - selsel_amd64, seljmp, fixargs: insert a copy when everything's constant, makes all .ssa tests pass. 
  - tests/macros.fr/if_chains() fails amd: just messed up b.jmp.arg being mutated
  - test/return_small_pair_const arm: `panic! failed encode cmp Expected (0 == 8589934593)`,
    problem is 0x200000001, which when sign_extend_low32() is 1, but imm() doesn't save that change. 
    happens because a store of `(1, 2)` gets promoted to copying as one constant and shifting it around. 
    that's a real bug tho, would happen if you compared a tmp to that constant too. 
    so suddenly this whole operation isn't a waste of time if it uncovers actual problems. 
  - test/intrins arm: ctz needs to insert an extra instruction and was relying on there being no fixarg for ordering.
    actually i think that was a real bug you could hit with constant folding: if you try to ctz of a stack slot pointer. 
    which is probably not something you'd ever do? oh! unless you wanted to check alignment. 
    should really have a debug check for regalloc use-before-def 
  - isel5.ssa -cc arm: loadaddr_bits can't do a negative offset from a symbol. 
    - which generally is fair cause it's considered disrespectful to go underflow your static allocation. 
      but you could imagine someone exposing a symbol that points to an offset into a data structure. and anyway, 
      you should be allowed to compute the invalid pointer, it's only a problem if you try to use it, like what 
      happens in isel5.ssa, it knows the callee is going to offset it forwards before dereferencing it. 
    - i think that assertion is actually junk. like if it ends up being a local it's fine, i just shouldn't be storing 
      `increment` as a u32. and then i have to deal with how to encode the macho relocation if it's an import or relocatable, 
      but they're already a bit sketchy, my previous way can't do the u32 bits either. 
  - (mem3.ssa, isel5.ssa) -w: i think that's just because my handling of 32 bit target is unfinished and was getting lucky 
    because of folding. so that should be fixed eventually but maybe it will happen for free when i get more serious about wasm. 
  - qbe_frontend.fr amd
    - but also #weak doesn't work with .Relocatable on amd? `-Wl,-undefined,dynamic_lookup` isn't doing the thing. 
      needs REF_TO_WEAK flag that doesn't do anything on arm i guess, wild. 
    - t.finish_module is 0x100000001 when you try to call it. set to the right thing at the start so so it's getting lost somehow? 
      doesn't like that it's in a static somehow? that seems insane. but yeah it works if it's on the stack... for tomorrow

## (Jun 12)

probably a good idea to consistantly let you pass os specific flags if you want to do something non-portable, 
instead of special casing MAP_JIT which i need. seems like they tend to be 32 bits so i can just make mine 
64 bits and `or` the top part back in at the end? i guess MAP_JIT maybe kinda is supported on linux, it 
just happens to have a value of 0 and be the default state (and no toggling). so thats probably nicer the old way. 
tho the high bits thing is error prone because you have to remember to transcribe the platform numbers and shift them, 
so if i write bindings to i write them preshifted? so maybe i should put mine in the high bits but then i can't compile 
c programs that want to use int for those. choices, choices :(

--- 

- dynamic dispatch: readdir, clear_cache
- random spewing of executables is getting on my nerves. limit it to the `target` directory. 
- yikes did cloudflare die? makes it more obvious that my `./boot/temporary/*.sh` is not an acceptable solution! 
  tho that's part of why i want to get rid of `#target_os`, then i can just have one program instead of one per os, 
  just need a system loader specific loader.
- type out the argcls table like qbe does. compiles in 4ms instead of 12ms and is less unintelligible, 
  250 lines of table instead of my 150 lines of boolean expressions. that's probably better, 

- trying to generate do_fold() as a FrcModule instead of with inline_for. see if that compiles faster. 
  - certainly more verbose, 50 lines -> 130 lines. but qbe is ~136 (and more error prone but less magic). 
  - it's too easy to mess up SwitchPayload.case_count. TODO: make it RawList
  - TODO: something's broken with using temp(), iff you try to printfn after runqbepasses in gen_do_fold. 
    maybe it's the thing where it gets reset inside a shim? 
  - in tracy: (do_fold_old = 53ms, do_fold_new = 1ms), so that's... quite a bit better. 
      - ah, but i was cheating by not counting the time to compile gen_do_fold because i was calling it 
      an extra time as a comptime thing that was reached first so it was already compiled at the point 
      i started looking in the profiler. so extra 20ms, but a lot of that is stuff that would have to 
      be compiled anyway (like that's the first place ops table, default_init, etc is reached), so it's 
      cheaper than it looks. i don't love seeing a 20ms pipeline stall but it's better than a 50ms one. 
    - it would be great if i a had way to mark a function as a well behaved edge in the callgraph and compile 
      it in a threaded module instead of comptime_codegen, so you wouldn't have the stall. (emit_instructions 
      has the same problem and the worse case situation is import_c). 

## (Jun 11)

I think I've changed my mind about using cosmo. They have grander goals than I do and trying to reduce it 
down to the parts I want might be enough work that it would be more interesting to just build it up from 
the other direction. I might revisit it if I decide to care about windows. The big thing I've learned from 
it is maybe just that syscalls are so slow it's fine to do a bit of work remapping structs/numbers at runtime 
instead of trying to use conditional compilation to project the differences out to user programs. Which seems 
in hindsight to be something obvious that everybody knows. Even in other languages that have lots of compile 
time decisions, the api they expose tends to not be linked to the system one, which sounds obvious when you 
write it down. 
  - https://github.com/ziglang/zig/blob/6810ffa424fccae0ecbf2f9e3db80069d8e7543b/lib/std/fs/File.zig#L397
  - https://git.sr.ht/~sircmpwn/hare/tree/master/item/rt/+linux/stat.ha#L24
  - https://github.com/golang/go/blob/d4c6effaa7b95a2ea149ece4a400c0ace2773839/src/os/stat_linux.go#L13-L14
    but fileStat is different on unix vs windows, so kinda in between
  - rust's fs::Metadata changes with the target, but you always call the same getters, 
    so they're doing kinda what i was trying to. 
- for the numbers, cosmo has them be variables instead of constants and fills them in with the right 
  thing for the current system at startup. but if the premise is that being slow is fine, maybe i'll 
  start with just declaring arbitrary numbers as constants and remapping them when you call into a function. 
  see if that's feasible or if i have to get more serious.  

---

- If I want do dispatch at runtime instead of comptime, and not fully commit to syscalls, I still need to support weak symbols. 
  - for Exe, it's easy, there's a bit in ChainedImport that tells the loader not to error if it's missing
  - for Relocatable, there's N_WEAK_REF to put in SymbolEntry.n_desc but it doesn't do anything, 
    you still have to pass `-Wl,-undefined,dynamic_lookup` to the linker or it errors. 
    same behaviour using clang to compile a program with `__attribute__((weak))` so it's not just me. 
  - made sure not to fold an RCon as non-zero
  - the hard part is how it should interact with jit-shims.
    you can't just check if the symbol is null. but it doesn't really matter for now if im just using it for platform 
    support stuff where i actually know what symbols will be available at runtime without checking. 
  - did it for elf too 

lets try with my favourite syscall to pick on: stat. 
- tried a new shape of type for a result which had lots of exciting bugs. 
  - can't call a function returning a struct like `@sturct(v: void, i: i64)` unless you take the result variables 
  address later because it tried to not give it a stack slot and then freaks out that there's no stack slot. 
  so have repr_transparent look at other fields. then constructing a struct like that freaks out, have it 
  just do the one scalar field too. 
  - old boot compiler doesn't like if an `#inline` function has a const FuncId parameter and then you try to take a pointer to it, 
  but ive encountered that before cause the line that reports that error is commented out in the new compiler. 
  - `zeroed void` doesn't work. the return's jmp.arg was QbeConZero instead of QbeNull. 

- convert open+mkdir+mmap
  - takes more fucking around when there's bit flags to remap between systems and it looks much clunkier 
    but it's so much nice to not have a viral `#target_os` escaping into user code. 
  - very error prone doing the conversion if you mix stuff using the old system flags and the new fixed flags

---

in unrelated news i think warp's calculation of where to put the highlight drifts more and more the longer 
your scroll back buffer is cause it sure is not remotely correct anymore. 

## (Jun 10)

- for now, add enqueue_task to vtable so ffi/include() doesn't need to recompile the backend: 674->479 (safe)
- bf2ir
  - looks messier because it wants to replace functions in an existing scope. 
    but it gets rid of an "order matters" which was a bit of a time bomb so that's nice at least. 
  - it's kinda annoying if you have to translate the types to FTy yourself when they're already on the function 
    signetures, so my current thing is have a helper to call that does it based on the abi type info on the Qbe.Fn already. 
    which maybe is dumb, could just have a thing to do it based on franca TypeInfo in ast_external since i'll need 
    that eventually anyway, but that would only help this specific case, not like import_wasm for example. 
    or make it so you don't need to give it type info, could just say the symbol is a rawptr and cast it at the usage site, 
    but that doesn't help for jit-shims (which is why the frontend really cares about the types of things). but it's 
    dumb that the current system forces you to give it signatures of any internal functions twice. 
  - anyway now it works but not with FRANCA_NO_CACHE=true or at comptime. i think it's just the import that's fucking up. 
    yeah it doesn't like if your import doesn't have a Func type, cause it doesn't know to make a shim for it. 
- via_wasm, import_wasm/ffi
  - this one gets A LOT LESS fucking around with signetures+callees.  
  - not ready to think about how imports should work so hack in temporary_funcid for now. 
  // Since we don't run fold.fr, symbols won't be marked so they won't be emitted  
  // if they're an inlinable export from CompCtx to the FrcModule and only used there.  
  // like bf/via_wasm.fr/env'putchar(). forgetting this manifiests as the function appearing  
  // in the bytes passed to import_frc() but as .Invalid in the frontend's cache file.  
  TODO: need better error reporting for that sort of mistake
- so now nothing uses FuncImpl.Ir so I can get rid of it
- little improvements: 
  - exit(0) considered harmful
  - don't use arg_types() when you just need to know if the function is unary
  - might was well let you import() frc bytes directly, just check the magic
- im seeing types with padding that doesn't make any sense `type :T146 = { b, p 7, l, p 7, l, p 7, }`,
  - because add_padding() wasn't changing current_size[], and qbe_frontend.fr doesn't use that so 
    the .ssa tests that compare abi to clang didn't catch it. 
  - tho even changing that (cause it makes the code less bit-shifty to call the function),
    i don't even have a test that cares if you do padding at all. added a case in abi5.ssa. 
  - wasted some time on wasm being the only platform where %d vs %ld matters immediately. 
- dumb mistake; getting confused by parse_args trying to collect from `rest` after `--` but anything that matches an argument doesn't go in `rest`,
  so running nested qbe_frontend.fr to test -frc compiles was behaving strangely: the second run wouldn't see a `-r`. 
- move more tests to running as exe+frc+inlinable and check output

## (Jun 9)

- the module always has types as FnPtr because exposing Fn is kinda weird. 
it's probably more useful to have expr.ty be FuncId if you want to iterate the scopes, 
and that way it doesn't force a jit shim all the time. 
- now that i think of it, data constants really should be pointer to thier type, because i don't have an operator 
for address of a global, like `a :: @static(i64)` corresponds to `static int a;` but my `a` is already a `*i64`
- don't quite know if i can make this work. i want data to be able to exist in the comptime 
jitted module and then if you change it in comptime, the version that gets baked into your program 
is the updated one (like how it works for franca code) but i think that would require exposing 
bake_relocatable_value in the frc format. or since you already know where the relocations are, 
maybe remember that shape when you need to bake and hope there are no unions? 
that's hard to think about. so maybe start with the basic version for now. 
tho it's going to get super confusing if a pointer into that escapes into the comptime franca code. 
- classic mistake: not increment index while looping over relocations. should really have a nicer way of zipping iterators, 
but i feel like you kinda need the external iterators to do that and they're so much more annoying boiler plate to write for each container. 
- now that's the whole import_c/test/ffi.fr working except the comptime variadic call. 
- added debug check for !threaded CodegenShared that you don't try to acquire the entry before releasing it (since the queue is only one long)
- now add_many works if i call it either at comptime or runtime but not both. 
- problem was "need load() to not alias since we might want to use in multiple modules", which i did have a comment for so at least i knew at some point. 
- for view_image, needed to call emit_suspended_inlinables. 
- ascii_table.fr was broken by trying to use speculate=true in count_array_init_elements()
- ughhh, another bug with a `+=1` in a loop over something else :(, when translating Fld to make a Params, 
so tuple field names were all `_0` and they didn't intern to the right thing so function didn't match a real declaration. 
- i think i've now achived feature parity with what i had before
- hmm, it sure got a lot slower (1032 -> 1113) at self compile since whenever my current release compiler was built (Jun  7 22:01), 
(both compiling current code so it's just cause i added stuff).  

---

- very confusing times trying to get direct data symbols access to work:  
ok so calling mul_two directly at comptime makes it work when nocache. (not for the original 
reason i was trying to test about the inlining) because it means the function has a jit_addr before 
the backend tries to emit the Dat for comptime_codegen's copy of mul_vtable, so the relocation 
happens. since mul_vtable is a data symbol, it gets sema-ed to Expr::FrcImport and trying to read 
it from the scope does an immediate_eval_expr to get a Values, can't be quick_eval-ed so it makes 
a synthetic function and goes through emit_ir for the comptime module even if we're only going to 
use it at runtime. for functions that's solved with shims (which can create a pointer to something 
that doesn't exist), but this is the first time it's possible to make a chunk of constant data 
without just mallocing some memory and filling it in. the thing i want to do is very similar to 
:ADistinctionWithoutADifference, i just don't have a version of that for non-functions. the other 
perspective is that the backend does know about the relocations even in the jit module, it's just not
doing them in time before the thing gets rebaked by the frontend trying to move it from jit module 
to aot module. so really the problem is that it's trying to rebake because it doesn't have a jit_addr. 

--- 

- import_c: now that i've committed to relying on the franca frontend more, can get rid of special Relocation type and ref tracking
- now that import_c just outputs a blob of bytes, free its arena at the end
- something along the line of making FrcImport work, fixed -frc_inlinable for kaleidoscope.fr (still not the compiler tho)
- in compiler/test.fr, run three versions: Exe, Cached, CachedEarly
- grow after free for bf2ir.fr, but only when run in the multi-module CompCtx, any alone works. 
reassuringly same problem if (exe, exe) so it's not about the frc format, it's just the first time ive reused a compctx like that. 
meh, that was just a mistake in old move_from_module(FuncImpl.Ir) not replacing the old f.mem, even tho there was nothing in it, 
growing it records that it was freed and affects the template function because an QList still has an allocation. 
but that's what im replacing with with FrcImport anyway, which puts a bit more thought into reusing with multiple modules. 

- something crazy going on with default_driver crashing when trying to reset temp() 

## (Jun 8)

- obj.init_data_needs_null_terminator was adding a redundant null terminator because tokenize makes 
a copy out of the source anyway (to handle escapes) so it's easy to do then. doesn't matter but 
confusion is bad for business. and i wasn't even zeroing the last byte, it just happened to be fresh memory all the time. 
- string literals in array initializers were being emitted twice because of count_array_init_elements. 
- strings for function names were emitted twice: `__func__` and `__FUNCTION__`
- sema: show declaration line for InvalidField error reporting

---

- need to store an extra layer of mapping for each QbeModule that uses the FrcModule, because they'll have different symbol ids.  
- i was trying to be clever about lazily loading stuff by treating them as normal expressions in a scope but that doesn't quite 
  work because you need to pre-intern all the mangled names before you can load() a function.
- the cache files kinda are a better api to expose because they treat all symbols uniform, 
  so it isolates the hacks to deal with my historical (func vs dat vs import) separation in the frontend. 
  i think that's most of what's making this seem painful. having an explicit translation step really makes 
  the weirdness about how i handle symbol names stand out. maybe this will encourage me to redo that part 
  of the frontend sooner. 
- it's a bit hacky that i keep swaping between the expr storing funcid or ptr. 
the module says the type is ptr because func only makes sense to the compiler, 
and then we change it to funcid because that's more useful, but if coerce_const_expr 
sees it in between it will switch back (and convert the value)
...why am i even doing any of this shit. `module.sym[i]` has this info. lmao. 
oh but you don't know the type unless you're going through the scope. 
ok that's fine, just save the types in another array at the beginning. 
that's much simplier than manually poking around in the scope to recreate info we already had. 
- new way of doing type remapping is less painful than old emit_ir.fr/remap(). rely on doing 
the source module being finished and then you just need to apply an offset to type indices to remap them. 

## (Jun 7)

- seperate out the fields of QbeModule you're supposed to set to init it. 
make the section size limits runtime configurable. 
- remove a few #include_std-s
- wasn't remapping Addr.offset.sym when outputting cache file
  - so idk how anything at all was working on amd64. oh because i was mostly loading one whole cache file 
  into a new module so all the symbol hashes worked out to be the same (if intern order was the same per bucket)
  so it didn't matter if you remapped something as long as it was symetrical in read/write. 
  it just started being a problem when i started zeroing out unused symbols after `fold` and that changed the order. 
  - that was fun to write a repro for. need symbols that collide where one is a non-exported function with all uses inlined,
  and that one is interred first. 
  - this fixed: 
  ```
  examples/repl.fr: compiling it infinite loops on macos-amd64 and crashes on linux-amd64 
  (when passed directly to the compiler cli or compiler/test.fr/compile(), but works through default_driver)
  ```

--- 

for import_c of cosmo, i need to deal with data imports. it's such a hassle to keep doing that sort of thing.  

thinking about going all in on the binary format instead of having import_c/ffi.fr build 
compiler data structures, have it output a .frc blob of bytes. which still kinda feels wasteful to 
me, like why add an extra serialize+deserialize step, there's no rule that your api has to be flat blob of bytes. 
but it means there's a very narrow tube for sending information and you can always pause and look at what's 
going through the tube. even if i had a debugger, it's really hard to beat command-f in a text file dump of the ir. 
the downside is needing to compile the whole c program instead of doing it lazily like franca does, 
but i already compile the whole thing because the idea of making a FuncImpl that's a callback into 
comptime code feels like it would be a nightmare for being able to understand what's going on. 
i guess it's fine if im just learning that the way other people tend to do things makes sense, 
the learning is the whole point of this operation, i don't actually care as much about my thing being different. 
i am a bit concerned about losing my sparkle. 
i still can't reconcile how every interaction with linkers is negative but somehow moving my thing more 
in that direction seems mostly positive so far, like im just inventing what we already have, why does it feel different? 

- rework tentative definitions so they don't require scanning the whole linked list of globals each time. 
  now just be careful to use the previous one so you don't get redeclarations and keep track of the set to be emitted at the end. 
- eagerly send functions to the backend while parsing instead of waiting until the end. 
  that means it emits functions that aren't called from main() too, which makes it worse as a standalone compiler,
  but the franca frontend does that part anyway when it's used through ffi.fr which is the usecase i care about. 

## (Jun 6)

- strange overload bug if you try to use this form of list constructor:
    b := 0;
    x: List(T) = list(b+b, temp());
  before this one:
    T.list(temp());
  in a compilation context. types don't even have to match. 
  you just need to reference the #generic one first. that's what `::assert(1, 1)` in core.fr was doing. 
  ok so that's because it wasn't evaluating the type annotation on `$T: Type` because the `#generic` 
  scared it off. 
- track type declaration site a bit and report that when you try to access a missing const field. 
- default_driver.fr support -frc_inlinable
- fixed edgedel not removing all phis because it checked 
`identical(source.s1, source.s2);` after `dest_p[] = Qbe.Blk.ptr_from_int(0);`. 
Leaving some dead options around is normally fine because they're compared 
by pointer identity so it safely won't match even if the block is later removed 
by blkmerge. But when writing out .frc files, references are done by `id` 
so it can't be allowed to encounter invalid blocks. 
  - fixes bounds check fail in read_phi. 
- made dump_bin.fr show data bytes in the format qbe_frontend can read
- set con.bits=no_symbol on anything thats dead after fold_constants so those symbols won't 
show up the the .frc file as Invalid. moved mark_referenced from inlcalls and do it at the 
same time after fold. 
- compiler/test.fr 
  - a bit more robust by checking the output more so it can't just call exit(0) by accident
  - allow compiling tests through the cache format

## (Jun 5)

- just hacking out some stuff that won't work to see if i can get cosmo to compile far enough to get to the syscall shim
- i was infinite looping on `#define FOO FOO`, i guess introduced by my attempt to simplify hideset tracking
- found :BrokenCGeneric
- fixed this problem with compiling from linux to macos:
```
dyld[54996]: Library not loaded: /System/Library/Frameworks/libc***.framework/libc***
  Referenced from: <no uuid> /Users/luke/Documents/mods/infered/target/release/franca-macos-arm64
  Reason: tried: '/System/Library/Frameworks/libc***.framework/libc***' (no such file), '/System/Volumes/Preboot/Cryptexes/OS/System/Library/Frameworks/libc***.framework/libc***' (no such file), '/System/Library/Frameworks/libc***.framework/libc***' (no such file, not in dyld cache), '/Library/Frameworks/libc***.framework/libc***' (no such file)
```
now the program works but it doesn't repro (not the same bytes as if compiled on macos). 
- cross compiling graphics programs from linux to macos was crashing. 
difference in binary is (rebase = (resereved = 0 on macos or 7 on linux))
wasn't zeroing fix.patch_at before starting in emplace_fixup. 

## (Jun 4)

- the rework of aot_import_body was very successful. fixed AOT amd64 graphics programs. 
- stop being lazy with u32 for backend symbol index
- BakedValue.Reloc.addend 
- not having any type checking in the c compiler is kinda dumb. should probably try to do better, 
but there's so much stuff you're supposed to allow anyway that it seems to quickly become
`-Wno-whatever` hell. 
- looking through cosmo's tls and `__constructor__`s

---

- `__stdio (mutex), [__stdout, __stderr, __stdin] (*FILE)`
- thread/tls.h: 
  - see ape/specification.md. 
  - wants to reserve x28 on arm (which is callee saved) so it won't work 
    in a callback from external code (like when i want to do macos gui stuff)
  - tls is a *CosmoTib
      - tib_ftracer (runtime/ftracer.c)
          - only for `--ftrace` 
          - shorted depth of callstack (so how many frames before main()?)
          - last fn addr so you don't spam on recursion?
      - tib_garbages (mem/gc.c)
          - some gcc extension to let you defer() a function to call when you return 
          (by rewriting stored return address). also called in pthread exit. 
      - tib_atexit (runtime/cxa_thread_atexit.c)
          - destructors for globals in `__cxa_finalize`
      - tib_ptid 
        - stashed from sys_gettid
        - used for MUTEX_SET_OWNER in pthread_mutex_lock.c
      - tib_pthread is a *PosixThread which is handed out to user code as a pthread_t
      - tib_syshand the real pthread_t from dynamically linked pthread_create
    - you give that tls allocation to apple in CloneSilicon too tho, so do they actually look at it 
    or is it just a fixed size header we have to stay in before `.tdata, .tbss` (see thread/mktls.c)a
- `__constructor__` can poke stuff into the startup function
  - calls/getrandom.c, calls/rdrand_init.c
  - calls/getloadavg-nt.c
  - calls/finddebugbinary.c
  - calls/program_invocation_short_name.c
  - calls/unveil.c
  - intrin/armlse.c
  - intrin/isrunningundermake.c
  - intrin/mmap.c
  - intrin/nocolor.c
  - intrin/sig.c
  - intrin/unsan.c
  - intrin/x86.c
  - log/countbranch_report.c, log/countexpr_report.c
  - log/logfile.c
  - proc/exec-sysv.c
  - proc/getppid-nt.c
  - stdio/ecvt.c
  - stdio/fflush.c
  - stdio/stderr.c, stdio/stdin.c, stdio/stdout.c
- stdio is almost not a special cosmo thing. 
  so could almost use someone elses but the cosmo one sure looks simpler than musl, 
  cosmo just trusts that their own mutex is good enough and musl like inlines it again as a fun bonus?
  - only dirstream, getentropy have IsWhatever() 
  - ctermid has v_ntsubsystem
  - have to agree on where errno lives

i'd love to know what nexgen32e means. vaguely assembly related? 
it doesn't mean amd64 cause there's arm64 code in there too. 
but maybe it does anyway:?
```
static int GetElfMachine(void) {
#ifdef __x86_64__
  return EM_NEXGEN32E;
```

## (Jun 3)

Some frontend cleanup stuff:
- replaced FnType.arity with (unary: bool, variadic: bool)
- CallConv on Func and TypeInfo.Fn was only used for inline or not inline so replaced it with FnFlag.Inline
- There are only 6 functions that use #link_rename and im spending 8 bytes in every Func to store it. 
so made that a flag too and you just go look in the annotations as needed. 
- sadly i can't actually shrink it unless i :UpdateBoot very carefully
- instead of variadic-ness being a seperate magic thing, 
just have a special type for your last parameter to represent that idea. 
if im ok with this: instead of `printf("%s%d", "a", 0)'` now you have to do `printf("%s%d", @va("a", 0));`, 
then i can simplify sema which is a big win. then it works nicely with overload resolution 
without extra work because the arity matches, you just emit_ir the last expression differently. 
- got rid of some forward declarations of overload sets that i needed in the original scope system
- on feb24 i switched from task.bounce to bounce_body in a few places to better support wasm/llvm, 
but that makes imported objc/variadic functions much sketchier on native targets as well. 
but given i don't fully support those yet anyway, so perhaps reverting wouldn't be the worst idea. 
  - on wasm objc doesn't matter and va abi is the sane just pass an extra pointer so bounce_body is fine
  - on llvm, worst case i could do mixed stuff where some shims are still done with my asm, 
  tho that would kill inlining so probably a bad idea. STOP THINKING. DO STUFF. PLEASE. 

## (Jun 2)

The other thing is that my pre-main init stuff is not very formal. 
Like .c/.ssa doesn't work statically on linux because the startup code 
for doing relocations is in franca. So I have to redo that and add more 
coupling between the languages anyway, regardless of what i do about libc. 

I think my plan is:
- get import_c to the point where it can actually compile cosmopolitan
- rewrite their startup stuff and my startup stuff into something that works for both
- replace the .S files for constants/syscalls
- replace the build system with not a billion object files
- and only after all that do i have to decide how much to fuse import_c with the franca compiler.

one more counting operation
- 206716 lines of libc/(.h/.c) =
  -  34582 lines of license headers / ascii art / `/*-*- vi:`
  -  14868 lines in the form `#define NAME TOKEN\n`
  -  10823 lines in the form `#include TOKEN\n`
  -  14778 lines blank
  -  28184 lines comment
  - 103481 lines the rest

---

- `libc/integral/normalize.inc` is what they stick on the front of every program. 
define `__LP64__` and then it gives me some extra type defs.
- need to accept more attributes
- error message showing the macro's definition site instead of where 
you called it from is going to drive me crazy. made it walk back the tok.origin chain 
and show everything. 
- added an option for import_c/tokenize.fr to be lossless so i can use it for counting (see above). 
- playing with a script that uses my c compiler to take a file with `#define name number` 
and generate a program like `printf("%s,%d\n", "name", name);` to output a csv
  - something's super broken with sending a #c_variadic function from franca to a jitted QbeModule. 
  i thought i was being clever giving it just one instead of fill_from_libc, but it behaves erratically.
  works better on amd64 than arm64 but then decides to crash if i rearrange the program slightly. 
  ```
  printf :: fn(fmt: CStr) void #c_variadic #libc;
  use_symbol(m, m.intern("printf")) { symbol |
      it: rawptr = printf;
      m.do_fixups(it, symbol);
  };
  ```
  - i could see jit-shim not working (speaking of which, added an error message for that), 
  but the driver gets fully compiled before being called so that's not the problem 
  - prediction: my printf addr is different from the dlopen one. yep. 
  ```
  printf: 5805095980
  printf: 6493874984
  ```
  emit_ir.fr/emit_call() has a special case for DynamicImport and if i take that out, 
  printf calls from franca stop working as well. so that's the problem, import_c doesn't do 
  that so when the franca program gives it the address, it gets the import-shim that doesn't know 
  to pass on the right registers. and it works with ExecStyle.JIT even tho it doesn't have 
  that special case because then it uses DynamicImport.comptime without going through the 
  import-shim. 

## (Jun 1)

- use sib byte less often, save a bit of space: 1478189 -> 1445357 (30kb, 2%)
- import_c: when compiling lua, 12/95 frontend samples were scan_globals calling slow_len.
switch Obj.name from CStr to Str
- extending .frc for storing frontend declarations/types
- import_c: better error message when you try to typedef something that isn't a type

---

Research.  

i can't keep copying random struct definitions for libc stuff. 
it's not the thing im interested in and it's super unreliable.  
maybe cosmopolitan libc is what i want but it's kinda large. 

```
14.323 total
------
 1.825 str/unicodedata.txt
 1.705 2020 license header
 1.171 testlib/moby.txt
 0.707 2022 license header
 0.518 2021 license header
 0.377 2023 license header 
 0.323 the rest of testlib
 0.290 isystem (remapping thier files to normal includes)
 0.287 2014 musl license header
 0.171 libc/sysv/consts/*.S (gen consts.sh)
 0.116 2024 license header 
 0.087 arm license header
 0.056 2020 musl license header
 0.040 sysv/calls/*.S (gen syscalls.sh)
 0.050 sysv/(errfuns.h, errfuns/*.S) (gen errfuns.sh)
 0.050 vga/vga-font-default.c (1235 bytes of font data)
------
 6.873 the rest
```

for reference, the franca repository is `5.611` currently.  

so that's not bad actually, i double my amount of code and never have to 
deal with copying out libc structs again and also get something that 
actually works instead of my current situation that just kinda maybe works. 

since they do one exe that supports multiple oses, they can't use `#ifdef os`, 
they have to do a dynamic check. which is great for me, that restriction overlaps 
with how my `#target_os` allows comptime execution with cross compilation. they 
have one set of struct definitions that user code imports and when you call a 
function they're translated to the native ones as needed. very good. ten stars. 

- they still use `#ifdef arch`, including in structs which is unfortunate. 
even public ones: sigcontext. so it doesn't completly free me of needing to 
sema twice to get different field offsets but it's still an improvement
- there's a bunch of special cases for their own stuff
  - zipos (tho that's kinda cool, i probably want to keep that)
  - an error message that suggests you try a different version of redbean
  - sys_execve execs `/usr/bin/ape` if the bytes in the file start with thier initials
- heavier runtime than i expected (tho the trace ones are optional)
  - `--strace` is just they call ~fprintf when it's turned on 
  - `__morph_begin` is pretty invasive
    - `--ftrace` is the compiler leaves nops at the begining of functions 
    and then the runtime reads its own elf headers to get find all the functions 
    and rewrite them to call ~fprintf so it has to call pthread_jit_write_protect_np
    - tls on amd64 starts at __executable_start and looks for the code sequence 
    it knows the compiler generates for tls accesses and rewrites them. 
  - they have to un-leb the syscall numbers before calling your main()
- they still depend on a blessed xcode to compile ape-m1.c to fill a Syslib struct 
with apple's blessed function pointers. 
  - which i fine, im not as excited about the single exe thing as they are. 
  i'd rather just keep doing mach-o like i do now. but that also means 
  i have to replace any of thier runtime stuff that tries to read the elf headers. 
- dlopen() isn't supported on x86-64 MacOS
- they use inline assembly which my import_c doesn't support. 
  - tho they stole chibicc just like i did and they have some `!defined(__chibicc__)` 
  so maybe i could steal their changes to make that work. 
  - most of it is just for doing syscall/svc
- `tinymath` is musl and arm's stuff
- they've got a bunch of extra stuff that isn't libc in the folder called libc. 
i guess it's probably good stuff, so maybe that's what i want, but i'd rather it be 
sorted differently. 
- they've kinda already done the whole thing i want to do with this project. 
like take a little self contained slice of interesting things in the universe
that you can cross compile to all the sane targets. maybe that's a bit disheartening. 
  - their "build from source" instructions starts with downloading a 1.3GB personal copy 
  of clang so that makes me feel a bit better. 
- they've aggressively bought into the tiny files compilation model.
like one per libc function so it can build one .o file per libc function.]
  - heh, i don't need cached compliation, i need cached concatenate the files so you don't have to reopen them all
  - you have to scroll past reincluding things so many times. 
  the lines in lib/(.h+.c) files are (1217+9380)/(46828+123927) = 6% `#include` 
  (counted after collapsing most of the licence headers). 
- they do `_weaken` and `__static_yoink` a lot which looks 
kinda messy but is a good way to solve the bloat problem (like as 
you add new runtime stuff, you need to add support code for it 
elsewhere and then you're stuck doing that whether you use the runtime
or not) which i've thought about didn't have a solution for. 
- the math stuff has hella tables of magic numbers

So if i wanted to do this and get rid of the libc bindings in franca, 
that either means adding import_c to the core compiler or using it to 
generate franca code from the c headers at comptime. 
- calling (init_ctx, compile_file, emit_the_code) in the compiler 
costs 1117037 -> 1281277 space and 1056 -> 1243 time, 
which is well within the range that i could plausibly claw back. 
tho that's not accounting for actually running it on the libc every compile. 
- stand alone binary that calls Ffi'include costs 362ms to compile and 
26ms to load from cache. so i could keep it an external program, have the compiler 
export any parts of libc it needs (which is probably just an allocator) and have 
that jitted as needed when you `import(libc)` or whatever. 
- generating bindings at the compiler's comptime would probably be a similar amount of 
code to embed in the binary as cosmo's libc/isystem which is 0.290MB which is more 
space than just adding import_c to the compiler. plus i want you to be able to 
edit the c headers trivially. 

## (May 31)

- `graphics` programs segfault on macos-amd64. 
with -keep-names: `ld: invalid use of rip-relative addressing in '_junk' to '__NSConcreteGlobalBlock'`
```
driver :: fn(vtable: *ImportVTable) void = 
    Easy'build_for_graphics(vtable, "a.fr");
    
main :: fn() void = {
    foo := @import_symbol("_NSConcreteGlobalBlock", "libc");    
    @println("%", foo);
}
```
need to do my trick converting LEA -> MOV even when .Relocatable and use a different magic number 
to tell it the relocation references a GOT slot instead of the symbol directly. 
now it crashes in the same way whether using the linker or not. 
- it doesn't like `NSScreen::frame`, i bet it's because it returns a big thing and i need to use the sret version of objcmsgsend. 
yeah now it gets farther and the numbers from frame() look reasonable. 
  - but arm64 doesn't have the `_stret` versions and i don't really let you ask the target arch, 
  so i have to do it with #link_rename which reveals some problems apparently 
  - it not working when the annotations are in the wrong order is dumb. 
  added an error for that which is better than silently being different if you try to do a direct comptime call. 
  - i have check_link_rename and get_link_name which do the same thing. dumb. 
  - it's not doing emit_ir again for the `_stret` ones at runtime? 
  oh fuck it's because im cross compiling from arm and they have #avoid_shim 
  so when both get mapped to the same address at comptime, it can't tell when 
  the rawptr needs to be baked into the other funcid. 
  thats... kind of impossible to get right actually... fuck. 
  i can cheat with Expr.FnPtr, glad i left that in. 
- now it crashes in realizeClassWithoutSwift (while calling arrayWithObject),
if you comment out that part, same in setMagnificationFilter.
  - then drawableSize, but that one's because threshold for `_stret` should be 16 bytes, 
  which then matches what i thought the abi was so that's reassuring. 
  - then nextEventMatchingMask
  - then `[CAMetalLayer nextDrawable] returning nil because allocation failed`
  - so what do (arrayWithObject, setMagnificationFilter, nextEventMatchingMask, nextDrawable) have in common? 
  actually the first three all use an `@import_symbol`, that's a bit suspicious given that i was just fucking with that recently. 
  i think im giving it the address of the GOT slot instead of loading from it, 
  doing an extra `[]` makes kCFAllocatorDefault=0 like on arm which i have more faith in, so that fixes the first 3. 
  ```
  10000a3eb: 48 8b 05 26 2d 02 00        	mov	rax, qword ptr [rip + 0x22d26] ## 0x10002d118 <_write+0x10002d118>
  10000a3f2: 48 8b 00                    	mov	rax, qword ptr [rax]
  =
  10000b590: b0000080    	adrp	x0, 0x10001c000 <_write+0x10001c000>
  10000b594: f9408800    	ldr	x0, [x0, #0x110]
  10000b598: f9400000    	ldr	x0, [x0]
  ```
  i don't understand why `_NSConcreteGlobalBlock` seems the same either way. 
- in update_dimensions, macos.view.bounds() is always (0,0) which seems bad. 
im passing something sane to initWithFrame but it's calling setBounds(0, 0), 
maybe my (f64,f64,f64,f64) abi is wrong? 
like no, i have abi6.ssa cause i noticed when qbe did that wrong on arm, and they pass with clang as the c side. 
or they did before i fucked with lea for imports this morning...
- 13 .ssa tests failing on `only LEA can be converted to GOT load` (when .Relocatable so not using my c compiler)
  - now that i try to allow for dynamic imports in the object files i output, 
  a nonlocal addr can't be folded into a load/store.
  - fixing that in isel also fixed the need for an extra load in `@import_symbol` which is interesting. 
  - now all work but abi8.ssa. tho while we're at it, added a test for folding an Increment into an import (which also fails). 
  - abi8.ssa: only difference is leas -> movs and relocation type 1 -> 3.
  but this whole thing doesn't make any sense. isn't the whole point of having a linker that 
  you just tell it what symbols you need to import and it works it out? there's no way 
  i need to tell it in my object file if a symbol is to be provided statically or dynamically. 
> woah! mandelbrot_ui works jitted on amd64 at this point. not aot tho, so that points to this imports thing being the last problem. 

```
// a.c
int printf(const char*, ...);
extern int a;
int main() {
    printf("%d", a);
    return 0;
}
// b.c
int a = 5;
```
in a.o, clang is doing mov and type=X86_64_RELOC_GOT_LOAD 
and then if you do `clang a.o b.o -target x86_64-apple-darwin`, 
the linker converts it from mov to lea so it's a static offset (without an extra load) as you'd expect. 
if you do `clang a.o -target x86_64-apple-darwin -Wl,-U,_a`, it stays mov and it makes a got thingy for it. 
- it really looks like im doing the same thing and it's working, it just doesn't actually work. 
```
464: 48 8b 05 00 00 00 00         	mov	rax, qword ptr [rip]    ## 0x46b <_main+0xe1>
46b: f2 48 0f 10 38               	movsd	xmm7, qword ptr [rax]   ## xmm7 = mem[0],zero
becomes
100001d34: 48 8d 05 dd 12 00 00        	lea	rax, [rip + 0x12dd]     ## 0x100003018 <_ss>
100001d3b: f2 48 0f 10 38              	movsd	xmm7, qword ptr [rax]   ## xmm7 = mem[0],zero
```
have i just been losing my mind this whole time? 
if i put `call $pbig(l $big)` at the beginning or end of main() that prints the right thing 
so clearly it's getting the symbol just fine. i've just cursed the abi somehow even tho it's identical asm. 
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa. ok so problem is the trying to fold an offset into the load (which i knew 
was a problem, i was just going to do that next, dumbass). 

## (May 30)

- changed blk.ins to be a RawList instead of a QList so can be loaded from binary without an extra copy. 
- embarrassing mistake to get confused by. rare L for memory unsafety. 
```
if n > b.ins.cap {
    b.ins.len = 0;
    b.ins&.reserve(n - b.ins.cap, temp());
};
```
- rediscovered :CreepyNop
- un-pointered m.(curi, target)
- changed Qbe.Typ layout so it's closer to what i want to serialize
- for RType in .frc, just start with the simple thing for now. always do every type in the module so no need to renumber. 
- hmm, `abi1.ssa -frc` doesn't work if qbe_frontend.fr is running jitted but it does work when it's aot (like run_tests does). 
that's very odd. the `,` in `int i6,` is changing to a random byte. two memory safety fails in one day? 
that's starting to look bad. 
problem was `import_c/tokenize.fr` somewhere isn't doing :BoundsPadding, 
really i should just clean all that up and deal with doing bounds checks, 
it's just a bit irritating to pass index+length or the end pointer around everywhere.
- `36 of 71 tests failed.`
  - finish_passes when loading instead of trying to redo the beginning. `4 of 71 tests failed.`
  - calling default_init inside a mark_temporary
  - wasn't counting the FEnd in nfield in backend/meta/parse.fr (also in compiler/emit_ir.fr/get_aggregate()/Array)
  i'd noticed the symptom before when i tried to assert it in for_unions(), so now i can get rid of "this is bad! emit_ir trusts it". 
  it worked because import_c didn't make that mistake and that's was the only place that emit_ir imports from which was previously 
  the only time nfield was needed without iterating
- amd64
  - linux: `didn't find encoding for extub R imm32` for `R1 =w loadub 109942031`,
  same as a `<4gb` problem i had before, need to smuggle it in an RMem. 
  idk what code produced that tho, something in prospero.fr. 
    - also took out my old fix for fault-na: `(.imm32, .imm32, (0xC7), 0),`, 
    get the same code gen by just putting the constant in an RMem in isel 
    and then don't have to deal with it in emit. previously i just got lucky 
    that i never tried to store register `->` imm32, etc. and i don't want to 
    add that many more table entries. added a test for that. 
  - `expected -1 <= 1` problem with prospero was i wasn't encoding f32 xcmp properly. needs no prefix. 
  - now x64 prospero fails a safety check but produces identical output to the arm one with -unsafe so that's progress. 
  problem was x64 makes RMem and i was reusing the same Qbe.Fn without clearing that or default_init-ing so it was held across a reset_temp. 
  - macos: dir_exists doesn't work. oh it's the fucking `$INODE64` like with read_dir. 
  trivial fix tho which is nice. 
- actually run prospero in run_tests, just on a lower width and assert that the right number of pixels were set.

## (May 29)

- new bootstrap idea. minimal jit loader for .frc. 
- crashes on the compiler and qbe_frontend(mandel) but can `-r` it.  
problem was not passing envp which the compiler checks and so does debug_log_byte_size. 
- and now i have statements dreamed up by the utterly deranged. 
```
# build
franca examples/default_driver.fr build boot/loader.fr -o target/loader.out -unsafe
franca examples/default_driver.fr build compiler/main.fr -o target/franca1.frc -frc -unsafe
# boot
./target/loader.out target/franca1.frc -- examples/default_driver.fr build compiler/main.fr -o target/franca2.out -unsafe
./target/franca2.out examples/default_driver.fr build compiler/main.fr -o franca -unsafe
# check
./franca examples/default_driver.fr build compiler/main.fr -o target/check.out -unsafe
diff ./franca target/check.out
```
- compressed it's not even that much bigger than the current boot exe. 
tho ive improved binary size since then so it's probably more fair to compare to what it would be now if i updated it. 
i feel like there's a path to saving that much space. 
```
old:     1330621 | 533058
new:     1117037 | 418778
franca1: 4965535 | 816851
loader:   148205 |  34852
```
- so my idea was to compile for each target and then extract out the functions that changed between 
them (which would only be #target_os and #asm) and then merge everything into one file and have 
the loader pull out the arch it wants. 
but that doesn't quite work because it's after abi/isel/regalloc which is already super arch specific. 
- new plan, support saving the ir earlier (which i want anyway so i can compile modules seperatly and still inline across them),
and then see how big that is and if it still makes sense for the bootstrapping thing. 
  - did phi+tmp (but untested)
- shrink Qbe.Typ.fields. kinda silly that im compelled to be more memory efficient now that im thinking 
about writing out files but that should make the compiler (imperceptibly) faster just in general as well. 
- in the spirit of making it easier to move across modules, 
i think it's probably work an extra indirection for RType to have instructions 
index into a function local array instead of the global one, i'd really 
like to keep the property that you don't have to iterate every instruction and do stuff to load a module. 
(tho rn i have to copy to a qlist which is dumb). that helps import_c too, can get rid of the 
stupid remap thing. kinda dumb cause it doesn't even show up in the profiler once for stbtruetype 
but it has slow vibes and i don't like it. but maybe that's dumb because it makes everything 
take more screwing around just to improve loading inlinable functions (which will be small anyway), 
so i guess that's not really the trade off you want

## (May 28)

- also found another maybe race. emit_ir.new_constants was in temp()
and iterated over after enqueue_task(), so i think i was just getting lucky 
that when single threaded, the following bouba_acquire() resetting it didn't 
stomp the memory in time? it just didn't matter because i only do single threaded 
when jitting which doesn't need to bake constants. 
yeah, SLOW_MEMORY_DEBUGGING=true, use_threads=false, can't `driver.dylib build examples/toy/hello.fr`
- fix yesterdays discovery. call set_library on the codegen thread
- that seems to have fixed the random failures and more cache files repro but still not all. 
also fixed linux, so i guess that just exacerbated the race somehow? 
- my theory of self_hash being the same accross architectures was wrong :(,
tho in hindsight it's not unreasonable that you'd load different files, 
like `lib/sys/linux.fr` doesn't need to be loaded when compiling on mac targetting mac...
but it is anyway so that doesn't narrow down the problem. 
oh it's because -syscalls doesn't load lib/sys/subprocess.fr because that's just in llvm_mc_dis(),
the point remains maybe that's not the best idea tho. 
- hmmmm i had to add .syscall to amd_flag_op to compile on linux which is fair but why was i able to cross compile to it, that seems bad. 
nvm, it must be something that runs at comptime but not runtime, which is fine. 
- experiment with more dynamic `@fmt`
  - don't use the get_jitted that tries to compile the function first because display(i64) is recursive
  - call created_jit_fn_ptr_value in vtable.get_jitted_ptr
  - not any smaller binary. bleh. i guess what you save in codegen you pay in vtable. 
  but it did find a compiler bug so keeping it as a test program. 
- got rid of some random junk
  - why pretend to support static context if i never test it
  - why have a bunch of commented out prints
  - why have ExportVTable that i never use
  - why use a power of 2 in vnew, vgrow does the doubling anyway
  - test for trying to destructure `void`

## (May 27)

- remove a couple clowns: mkdir, cp
- stop using maybe_borrow_cstr
- make write_chunks_or_crash atomic (by writing to a different file and renaming)
- namespace the structs that used to be in bindings/sokol.fr
- unfortunate that -syscalls doesn't actually enforce you don't link libc, it just does its best. 
  - opendir isn't just a linux syscall i can do so use stat in dir_exists instead
  - spend so fucking long on typing an octal number from a header file in as hex
- seal_debug_info sets `__franca_base_address` which is read by do_relocations_static_linux 
on linux so that's gonna be a problem if you use `-frc` and then try to run it jitted. 
started with a dumb hack where you include `__franca_base_address` in the cache file 
but as just a different magic bytes that you check for and don't do relocations. 
but fix_stack_linux is also fucked. 
- bounds check failing in amd64/emitins()
hmm that sure isn't a program
```
function $main__720() {  # nblk=2
@0
	R8 =l addr 8589934608
	R7 =l copy $println__549+137438953471
	call 16, C33  # what no syscall for giacomo?
	jmp @1
@1
	ret0
}
```
only with -syscalls tho. linking glibc it's fine. and `-frc -syscalls` and then 
run by the `syscalls` compiler is also fine. 
mystery
- complexify backend/incremental.fr/check()   
// Doing it this way instead of `X :: fn=>; X(self.dep&); ...etc;` saves 4kb (says bloat.fr).   
// Would be nice if i could let you express that gain without it sucking so much.   
// I still like it cause it would suck so much more in rust.   
// But really the problem is just that my @assert is so much bloat.   
- manually outline the printing for the panic codepath in `(debug_)assert_XX`.
  - (arm) hyperfine --warmup=1 "./qq.out examples/default_driver.fr build compiler/main.fr"
  ```
  1642349 -> 1527437 (-7%)
  1.309 s ±  0.008 s -> 1.272 s ±  0.003 s (-2.5%)
  ```
  - only saves 32kb with -unsafe but that's as expected (and that's still 2.5% which ain't bad). 
  even just making debug builds cheaper would be very good for business, 
  especially since run_franca_file() always has safety checks enabled so 
  this de-bloats the cache files as well. 

- c_string.fr: this super isn't a program
```
diff aa.txt bb.txt
1c1
< - (size = 43323, arch_os = 0)
---
> - (size = 43267, arch_os = 0)
78c78
< === 115 sym ===
---
> === 114 sym ===
499a500
> - 'getchar' is Import (lib = libc)
583c584
< - 'offset__2968' is Invalid !!
---
> - 'offset__2968' is Import (lib = libc)
588,589d588
< - 'getchar' is Import (lib = libc)
< - 'putchar' is Import (lib = libc)
1033c1032
< 	call $putchar, C33
---
> 	call $offset__2968, C33
```
in fact, even when the tests pass the cache files don't repro?
there's way more changes to the binary than to the text dump. 
order of symbols is changing (which doesn't change the dump 
as much since con references by name). 
great success; set_library called directly in emit_ir so race 
with the codegen thread for when map_sym gets called. 
indeed, diffing `strings` of the binaries, the changes always 
start at an import. previously it was fine because use_symbol has 
lock_bucket so no corruption and the interned number never made it into the binary. 

## (May 26)

- don't store (QbeNull, QbeUndef) in con
- i was right about emitting asm being basically free. 
```
franca examples/default_driver.fr build compiler/main.fr -o q.out -keep-names -c -unsafe && gcc q.out -o q.out
./q.out backend/meta/qbe_frontend.fr backend/test/mandel.ssa -o a.out
```
  - uncached samples: 875 main thread + 445 codegen thread
  - cached samples: all=37
    - hash files: 9
    - hash self: 8
    - read files: 2
    - dlsym: 8
    - emit_func_arm64: 5
    - load(Qbe.Fn): 3
  - `hash self` i knew was slow and should be done at the compiler's comptime somehow 
  - the dlsym time seems way to long for 30 imports. 
  oh, im doing it for every Invalid (from a function that was always inlined).
  i started doing it by tracking imports but it's way easier to just put a check for 
  `symbol.fixups.len == 0` in the dumb fill_from_libc for now. that makes dlsym be 0 samples as
  i hoped.
  - `hash files` is unfortunate. should i just trust the os last modified date instead? 
  i really don't trust it, plus it would make the cache files reproducible which just feels creepy. 
    - ah that's an interesting thought, the `hash self` needs to be of the compiler's source code 
    not the exe so it's the same for all arches of the compiler, so you can cross compile cache 
    files and get the same bytes (not that it's useful, just that repro is good for sanity). 
  but i can save that in a codemap and reuse it if you miss the cache so you don't have to 
  do it twice. tho that only speeds up the slow path anyway so it's less exciting. 
- it's unfortunate how much slower the caching makes the first run (where it has to create the cache). 
  - 868ms vs 812ms with FRANCA_NO_CACHE, for `backend/meta/qbe_frontend.fr backend/test/mandel.ssa`,
  - tho that measurement is fishy because the order matters for the file system's caching maybe? 
  like that was yes then no but if you do no then yes they're the same speed. 
  which if i don't think about it too hard could mean it's slower by exactly the amount of time it takes to cold read all the files?
- did `hash self` as hash of everything in the codemap when compiling the compiler. 
  - needed some compilation order hacks because the newly increased shim enthusiasm 
  to make it run after all files are loaded (backend/ARCH/emit.fr is only called through 
  a vtable so the stuff it `import`s is loaded late). 
  - brings the fast case above to all=~20 samples.
- afraid i've introduced spurious failures to examples/bf
- in compile_all_symbols:  
  - pre-intern all the names and just index into an array instead of needing to rehash every time
  - don't call default_init since it needs to recreate the `tmp` array which emit() doesn't even use
  - saves like 2 samples on backend/meta/qbe_frontend.fr (1.07 ± 0.02 times faster)

## (May 25)

- my previous thing with shims for mutual_callees broke jitting the graphics programs
  - `Terminating app due to uncaught exception 'NSInvalidArgumentException', reason: '+[NSApplication ]: unrecognized selector sent to class`
  - it's at `NSApplication'sharedApplication()`
  - it works if i add back the `for mutual_callees: dispatch::Action.Jit`,
  which is odd because that should only affect comptime code, even though the "runtime" code is 
  jitted, it still goes through the normal main_thread_pump. 
  - can filter to the names objc_msgSend/objc_msgSendSuper and it still works,
  so it isn't even a compilation order thing because those don't have a body to compile, 
  it's just an import, the only thing it can be doing is calling create_jit_shim but
  it doesn't work if i just do that. 
  - it has to be another thing like TodoDontZeroX0WhenVoid because the error changes 
  if i add another argument to those functions (silent crash instead of apple's backtrace), 
  but it still works aot. 
  - difference is not the special case emit_call to DynamicImport when aot. still works without that.
  - i can fix bounce_body to not emit for void and then aot keeps working without the hack in objc_msgSend's signeture, 
  but that doesn't help jit (tho does change it to silent crash)
  - OHHHHH, because create_jit_shim trusts the par types so it's not passing on all the extras when you put other shit in 
  the call site. when it was `objc_msgSend :: fn(self: ObjcId) void` and you call it with `(cls::NSApplication, sel::sharedApplication`, 
  the shim passed one argument so only the cls got through and the sel became garbage and you got the nice stack trace. 
  and then saying there are two params means you get through the dispatch part and the actual implementation crashes 
  (on the first message that actually needed multiple arguments), and similarly, when i fix stomping x0 and change the sig to void, 
  the reciever doesn't get through at all and it crashes trying to dereference whatever junk is in x0 after the shim handler. 
  - now this is particularly offensive because this is the only function where you need to lie about the signeture at the declaration site 
  and it's an import anyway so there's no need for a shim. 
  but you don't want to say never shim for imports, because you want to allow things that are only available 
  at runtime to exist as a function pointer at comptime as long as you don't try to call it. 
  for now just hack in a `#avoid_shim` that opts in to the old behaviour. 
  it's fine if you make a shim for the msgsend functions, they just need to be compiled properly 
  before you try to call through it.
- always caching the thing in run_franca_file seems to mostly work. 
  - `examples/bf/code_string.fr` says ` Cannot call init_self_hosted inside another compiler`.
  because it's looping because it has calls `import()` on a string of source code which 
  gets added to the CodeMap as a file named `(import literal)` which of course doesn't exist. 
- something i didn't think about: you need to treat the compiler binary itself as a Dep and put that hash in there too. 
but it's kinda awkward to get that at comptime, it can't be the same hash as for mach-o codesign
because you can't add it to the data segment that late. for now just get_executable_path at runtime 
i guess, bleh. but it's cripplingly broken if i have to delete the cache folder or remember to change the 
version number every time i recompile the compiler. 

## (May 23/24)

- im very suspicious that my terminal starts lying about your 
programs crashing if you let it run too long. or im just going crazy. 
either is entirely possible at this point. something to look out for...
- got to the point of having a binary form of the ir i can write and read back. 
running it works for "hello world" but nothing else. 
- one dumb mistake: forgot other info it needs from the Qbe.Fn: reg/slot/leaf/vararg,
which fixed it for mandelbrot but still not everything. 
- idk why im trying to start with the main compiler for this. 
better strategy is just run the .ssa tests through it first. 
yeah `6 of 71 tests failed.` but they're helpfully fairly minimal examples. 
- call1.ssa: it's not emitting add_one (called through the vtable)
was skipping the part of emit_data where i mark_referenced for any relocations. 
- fault-na.ssa: wasn't outputting size for Dat2.template.Zeroes
- all .ssa work now. 

## (May 22)

- silly source of tiny slowness: when you have a function that returns a vtable, 
so it has to make function pointers, im not using shims so they get jitted even 
if you never call them at comptime. which is especially sad because the comptime 
module doesn't run the backend in a seperate thread (because doing that without 
being able to reorder things means im just waiting on it for tiny functions all 
the time) so it's even slower than it sounds. 
  - happens in import_c:comptime_init_rules_table, graphics:register_objective_c_classes
  - took a lot of looking around to find the right place but the fix for the import_c case 
  is trivial: just don't loop over mutual_callees at the end of dispatch.fr::exec_task()::Jit.v 
  it was just going through ask_coerce_const_expr. 
  - `./trace.out driver.dylib build examples/import_c/cc.fr -keep-names`: 
  before: running Backend for do_fold takes 4ms and happened twice, one of which was on the main thread. 
  after the fix, it's just once and on the codegen thread. 
  - the graphics is going through CompCtx.git_jitted, 
  fix: just make a shim instead of polling on Action.Jit
  - same result. ie. now macos_mods only goes through the backend once. 
  - very marginal speed improvement (~10ms on hello_triangle) but it made the code simpler
  so thats a win
- made Qbe.Con smaller

--- 

My theory is that really good fine grained incremental compilation takes an infinite amount 
of work and is something that I don't have the resources to do well. However, the current 
state is that it's a 300ms tax to recompile import_c every time you compile a program that 
uses a font. So maybe a middle ground where we don't try to cache individual functions in 
the program you're editing but the libraries you never change and have a very thin api 
can just be reused. 

I can't quite articulate why this feels different than linkers to me. 



## (May 21)

- made dump_wasm.fr show types after Call/GetLocal/SetLocal, 
which is redundant but it's silly to make it harder for myself every time 
by needing to scroll up to the top and remember indices. 
- trying to make the .ssa tests pass wasm type check
  - vararg1.ssa: `function d $f(l %x, ...) {` imported as `extern double f(int, ...);`
  - isel5.ssa: import_c was always emitting `!` as a Kl which i guess is silly (because it 
  doesn't match the ty_int the compiler thinks the expression outputs) 
  but the backend is supposed to allow truncating that so added it to subtyping.ssa test. 
- fixing ENABLE_TRACY bitrot. 
  - it prints `Unfinished Type 20` and then segfaults. 
  - works if you don't try to log a Values for the zone name in curry_const_args
  - the crash was because it was recursivly trying to log_type in a debug_assert 
  that the field offsets make sense and they don't make sense because get_info 
  for an unfinished placeholder says the align is 0 because it's supposed to be unreachable. 
  - so the root problem there is that im calling get_info to check if there's an inferred type name, 
  but get_info does a memoized sizing thing so you can't call it at random times, 
  and i used to correctly check if it was already done before calling it in log_type but i 
  took that out to make names for opaque `@struct(_: rawptr)` work because they don't seem 
  to get sized in time when i try to generate nicer wgpu bindings. 
- TODO: register_objc_class jits the methods to fill the vtable even if they're only called at runtime. 
should use shims :SLOW

## (May 20)

- shader translation for wgsl (unfinished: texture/sampler bindings)
- webgpu: if you call surface.present after trying to skip a frame makes it flash like crazy
- start epicycles example

## (May 19)

- a few days of slow progress. redoing shader translation
- it's very easy to just silently get garbage
  - if you forget `[[stage_in]`
  - if you put `[[buffer(0)]]` on a struct of texture/sampler. 
  (so it's fine to put your bindings in a struct but it can't be the same struct as your other stuff?)
- if i were to generate the relection info for the vertex types, 
it could be more succinct because you wouldn't need to say .unpack() 
anywhere but i find it super confusing when trying to figure out which 
cpu side data goes into which part of the shader. it's easier for me 
if they're the same type on both sides so you can watch it flow through. 
- similarly, the vertex output and fragment input ("varying" variables?) 
being different structs makes it less clear that they're the same. 
TODO: is it slower if you say you need `#position` as an input to the fragment 
shader but don't use it? cause it seems like that's the only disadvantage. 
surely the other guy's copy of llvm notices that if it matters. 
i guess it's confusing if its a magic fragment input like `front_facing`, 
but i could always just let you nest structs or have that as a seperate argument. 

## (May 15)

- so the metal docs have an example where they have same program written as (a storage 
buffer (which is just an array) indexed by vertex_index) or (using `[[stage_in]]` and 
telling it attribute locations and it picks them out and passes you the struct). 
the first way makes way more sense because there aren't seperate uniforms and vertex inputs 
now, it's just one example of a global variable is an array that you index into. 
- can i get hello_wgpu to work with that? it runs but no triangle. huh, alignment 
rules are different somehow? with the vertex buffer i was giving it 7 floats per,
saying they were vec3f,vec4f in attributes but 2 vec4fs in the shader and that was fine. 
but when using a storage buffer you need to give it 8 floats per (even if you say vec3f 
in the shader there's padding). but now that works. i don't understand why vertex buffer 
doesn't need padding. surely it's not using the extra info about attributes to like recopy everything 
into a new buffer to realign it right? maybe it's doing an SOA thing? or maybe the alignment 
rules on storage buffers are just totally arbitrary. mystery. 
- so does that mean i can get rid of all the code that talks about vertex buffers?
  - if it means wasting a bunch of space on alignment padding that's quite unfortunate 
  - sokol exposes vertex_buffer_offsets but you can do that for storage buffers too. 
  metal's setBuffer makes it trivial and webgpu has dynamic offsets (tho maybe more limited?)
  - instancing doesn't care: https://webgpufundamentals.org/webgpu/lessons/webgpu-storage-buffers.html#a-instancing
  - indexing doesn't care: https://github.com/floooh/sokol-samples/blob/master/sapp/vertexpull-sapp.c
  - different limits might be a problem. metal just says you get 31 buffers, good luck, 
  but webgpu splits up the limits with defaults being maxStorageBuffersPerShaderStage=8 and maxVertexBuffers=8,
  so if you can use both you get 16 and 8 is kinda a lot less than 16. 
  - maybe old versions of opengl doesn't have storage buffers? don't care. 
- hmm, the thing where you tell it the types of the fields, they're not the types of 
the fields in the shader, it like reinterprets them into floats between when the cpu writes 
them and your shader code gets to have them. but i can't ask for that on normal buffers so 
do i just lose if i don't want my 8 bytes of uv+colour to become 24 bytes? cause like 
for colour i could write the code myself with bitshifts but wgsl doesn't give you u16 so...?
ahhh, everyone has unpackXXXX functions that do the int->float stuff. ok it's fine. 

## (May 14)

- another round of working on webgpu support
- `Unsupported texture format TextureFormat::Depth32FloatStencil8, reason: requires feature FeatureName::Depth32FloatStencil8`
but if i getFeatures() i can see that it does support that feature? 
i guess i have to ask for it specifically somewhere? 
yeah, when you create the device, need to say up front what features you need. 
i guess that's fair, nicer to crash and burn at the beginning than the first time you try to do something interesting. 
- now it crashes in uniform_buffer_on_commit calling writeBuffer. 
which is especially offensive because i don't have any uniforms? 
is writeBuffer of zero bytes an illegal build technique? 
thats what sokol does tho? 
- other than that, their error messages are like very reasonable. 10/10 would uncapturedErrorCallbackInfo again.  
- ah fnucker, i shouldn't have complemented them. submit() crashes silently. 
but if i comment that out too i do get exciting purple rectangle (which isn't what i asked 
for which makes sense as i comment out the asking) but at least im successfully giving dawn access to my window. 
- is there any chance that if i make DAWN_ENABLE_ASAN be ON it will give me a stack trace? 
nope. glad i waited 4.5 minutes for that to compile...
ah! but real lldb finds the debug symbols! amazing. either clion's copy of lldb is garbage 
or asking for address sanitizer sprinkled some fairy dust on it (nope, clion still doesn't know whats going on). 
- 0x800000004 doesn't look like the pointer i want for my WGPU.Queue... 
and indeed it sure is not the pointer that getqueue returned at the beginning, 
so i super fucked it up somehow. 
oh dear their Limits struct is bigger than mine so when i asked them to fill it,
it clobbered a bunch of other shit. thats rather unfortunate. 
so what we've learned is you're super not allowed to use use a webgpu.h, 
you have to regenerate it all the time. 
- much closer now. i get the right background colour. still no triangle tho. 
- now i have to go scrounge around for anything else i turned off.
that was easy. colorwritemask. triangle works!
- i like that opening webgpu.h in zed consumes 400% cpu forever 
(written in rust btw, rendered on the gpu btw), 
but maybe that's `clangd`'s fault
- i don't think it's the right triangle but it's hard to tell because i can't 
get the sokol-samples that use webgpu to compile. surely it's supposed to look the same 
as the sapp ones. oh yeah, i was giving it a `[]f64` instead of `[]f32`. 

## (May 13)

- change font size based on dpi scale at startup
- move around with arrows. click to move cursor. 
- input text at the cursor. 

## (May 12)

- convoluted example that makes an ascii table png
- experimenting with why my font feels blurry. it's because i had high_dpi=false, 
so i was taking the perfect font, rendering it small and then letting the os
scale it up instead of just rendering it to the right size in the first place? 
- cleaned up the test program a bit
- tab to complete from history
- drag to select any direction not just down+right

## (May 11)

- cmd click on a file:line:col to open it
- skip ansi escape codes
- discovered :ConstInFuncOrLoseTemp
- select text and copy to clipboard
- double/triple click to select word/line
- fixed early truncation in import_c. i don't super understand why i had that problem and chibicc didn't but oh well. 
so now stb_truetype works in my c compiler without patching it. 

---

it's sad that this makes me feel better https://github.com/profullstack/smashlang/blob/cdf3f2576c8be0cdf8259223e8fed8f6af4dcd88/src/compiler.rs#L32
i don't quite understand the scam but it's definitly entirely ai produced. 
there's hella stuff in `runtime` but how are you running it when `// In a real implementation, this would be a complete interpreter`. 
but it really looks real. like if i were scanning resumes, there's kinda looks better than mine, 
but then you start looking at it and discover:
```
/// CompiledFunction represents a compiled function that can be executed
pub struct CompiledFunction {
    // In a real implementation, this would contain JIT-compiled code
    result: i64,
}
```
i'd feel so guilty if this is a real person and im just trash talking their thing but like...
https://github.com/profullstack/smashlang/commit/0afe8c53eb0bb687997fd976f5d592799a40ac69
"feat(compiler): Add support for Linux target platforms"
no it fucking doesn't, that is not a program at all, but it really really looks 
like a program if you haven't quite seen a program before. 
i like "fix compiler" https://github.com/profullstack/smashlang/commit/ca574c8e54b3380cf071ca679b12964650204592#diff-b654b512cb7fbcfcbd4eb750cad8b8c2128385631e195fed3f2671304de76cf5
where they had kinda an llvm thing going on but instead now it's kinda a c thing and then disappeared too later. 
"Key Skills & Competencies: AI-Augmented Development: Proficient in using conversational AI assistants (e.g., Cursor, ChatGPT, GitHub Copilot) to generate, review, and refactor code efficiently."
i would NOT have guessed...

## (May 10)

- sending SIGINT to `make` doesn't make it stop. am i supposed to be sending it to 
all children somehow? SIGKILL works but maybe that's rude. 
- discovered my problem with stb_truetype, i do the casting wrong in `b: *u8; b[]<<16 + b[]<<8 + b[]`, 
i truncate to u8 instead of promoting to i32. they only do it in one place so i can just hack it out for now. 
- added `-r` to import_c/cc.fr which mostly works but panics if you try to use it on 
something that imports stb_truetype, but also it segfaults trying to call the panic handler which is really creepy. 
TODO: tests/todo/b.c, add a simpler test case to run_tests.fr
- fixed the spread syntax not going through name resolution
- use a nice font. had some confusion with how to get the right offsets, 
like i was rendering `_` at the top of it's cell. since i want a monospace font anyway, 
can easily poke it into debugtext.fr, just have to make glyph_size not a square. 
- right click and drag to scroll because otherwise i can't go sideways without a trackpad
- drag and drop a file to paste it's path
- small string optimisation
- made non-ascii chars show as filled in squares. 
  - found that `_NSGetExecutablePath` has a null byte on the end
  - rediscovered that im padding all my strings to 4 byte alignment which is kinda dumb but :MiscompileCStr

## (May 9) 

- I was kinda hoping that the graphics stuff would be a fun vacation from compiler 
but it turns out it's not like super easy either (who knew!), so time for a break. 
- made the farm_game example playable. game loop where it gives you a list of items
to get each round and you lose if you don't finish in time. 
- new project: terminal emulator
- starting from my old `edit.fr` example so can already draw some text and scroll around
- exec a process, poll for output every frame, dump the text on the screen
- type on the bottom line and press enter to run it as a command
- scroll down as text is added but only if you were already at the bottom, 
so you can scroll up to look at something while running a program that spews logs 
without it ripping you away down to the new stuff. 
- history. up/down arrow to cycle commands
- control c to send SIGINT to the child process
- why is mine way slower than warp at running my prospero example? 
:BigOutputBuffer 
I only call poll() once per frame, but I want to read as much data 
as possible (my buffer can be way larger than the OS pipe buffer) so the child 
process doesn't block with a full pipe while i sleep on the frame timer for another 16 ms. 
```
1480ms: in Warp
3020ms: mine, no loop so 64KB per frame
1230ms: mine, loop with 4MB buffer
```
- fuse it with the repl.
- why is mine slower at running all my tests than warp? 33s vs 28s. 
ahhh right XProtectService eating a core. i have to add my terminal to "run programs that do not meet the system security policy" 
in system settings like i did for warp and terminal.app. hah, now 26s.
- im very afraid that security thing in per hash or something so everytime i recompile i have to take it off the 
list and put it back. i wonder if every time warp updates itself it takes itself off the list 
so it's actually supposed to be way faster than mine still? seems like no. 
maybe they do the signing properly and that lets you stay. but it also doesn't forget for me every time, 
i feel like there's something im missing cause it can't just be random? 
- mine being open makes WindowServer eat 30% of a core consistantly even when it's not doing anything. 
nothing else has this problem. actually when i type into zed or warp, WindowServer time shoots up. 
so maybe i have to ask for my frame rate to be lowered when i know im not drawing anything. 
do i like toggle on and off my CVDisplayLink? or switch to CADisplayLink so i can use preferredFrameRateRange? 
but then do i have to deal with polling the other process in another thread? 
i don't want it to get stuck with a full buffer just because my window is inactive. 
i can just drop frames im not in the mood for. the cpu usage goes away if you don't call presentDrawable. 
so i can ask for 60fps and just not produce the frame if there were no events/output since last time. 
much better, now doing nothing is free as it should be. 

## (May 8)

- track field declaration Span for nicer error message when you forget a field in a struct literal.
- hmmm, "invalid vertex step mode for vertex buffer layout" but i can see that im giving it the 
right number. i can even recompile wgpu and print the number in that error message to make sure 
it didn't get lost in struct padding or something crazy. ahhhhhhh this is going to suck. 
it turns out wgpu's copy of webgpu.h is totally differnt from dawn's and i had generated a 
thing off dawn's and then tried to use wgpu. i kinda assumed that when `webgpu-native/webgpu-headers` 
said `**NOT STABLE YET!**` they meant oh you know they might be different next year not like right
now every implementation disagrees completly about what the numbers should be and we've just picked
one at random to give you. so that's unfortunate. 

```
// count.fr is skipping graphics/web/ sometimes ??!

// [examples/count.fr] main()
// SOMEHOW I DONT TRUST THIS
// THE NEW str() FUCKS IT UP????????? BECAUSE IT HAD A BRANCH AND DIDN'T GET INLINED
// :Compiler :FUCKED
// if you change the args0 we check for here the same thing happens on the amd one !!
// so it quacks like an abi bug but on both backends !!
// this is very bad perhaps 
// doesnt happen AOT (even with ajusting this string and running `./a.out examples/count.fr graphics` so it takes the same codepath)
if args.len >= 2 && args[0].str() == "franca" && args[1].str().ends_with("count.fr") {
    args = args.rest(2);
};

// [examples/count.fr] handle_path()
// adding this fixes it
_ := @tfmt("%", arg);

// [lib/fmt.fr]
// fixed by inlining this (which you want it to be anyway but it's 
// not ok that it doesn't fucking work at all otherwise)
fn str(self: CStr) Str #inline = 
    @if(self.ptr.is_null(), empty(), (ptr = self.ptr, len = self.slow_len()));

// [compiler/lex.fr] lex_ident()
// alternatively fixed by getting rid of this 
// (which is NOT that it lexed the program itself wrong; count.fr uses the lexer)
// and like this is stupid anyway but it's a massive problem that i still can't explain what the hell is happening. 
// i only thought of this because i tried printing in eq(Str, Str) to make sure it was getting the 
// right thing for the args check in count.fr (which it was, at least with the print in there) and 
// was annoyed that it was comparing to "fn" a lot in the lexer. 
@switch(name) {
  @case("fn") => self.put_token(.Fn);
  @default fn(s: Str) => {
      ident := self.pool.insert_owned(name);
      self.put_token((Symbol = ident))
  };
};
```

## (May 6/7)

- putting this in fill_objc_reflect is such a flex. 
```
@assert(func.body&.is(.Normal));
func.body = (Normal = @{  // :InstrumentObjc
    scary_log("#### ");
    scary_log(@[@literal def.typename().str()]);
    scary_log(" ");
    scary_log(@[@literal sel.str()]);
    scary_log("\n");
    @[func.body.Normal]
});
```
- wtf 7 billion years of weird race condition situation and all i have to do 
is not say i implement CALayerDelegate? apple's example does. why's mine different? 
every method on that protocol is optional and im not implementing any of them 
(but neither are they!) and you do have to set the delegate as something (even 
if it doesn't implement that protocol or it just crashes). hmmmmmmmmmm,
adding a null check when i call objc_getProtocol and it seems that the string it 
has as the protocol name is the whole source code of my program. ok yeah that's my fault. 
so somewhere along the confusion line (either not coercing in get_constant which is a known problem 
or aliasing a baked Str and CStr which is a known problem) and it lost the null terminator, 
so that returned null and i gave that to the (reasonably) `_Nonnull` parameter of objc_getProtocol, 
which then explains why what lldb revealed was `_class_conformsToProtocol_unrealized_nolock` 
trying to `ldr    w8, [x21, #0x44]` with x21=0. i was just assumed that the null thing 
was the object not the protocol. in adding an assertion for that it seems NSApplicationDelegate 
doesn't exist? maybe im not opening AppKit in time, but also it seems fine without? 
- so now i no MTKView and it seems to mostly work except for depth buffer stuff (so geo example looks weird). 
- :WindingDefault
- dumb mistake with slot indexes. `16-1` vs `(1<<16)-1`
- maybe geo just always looks weird, made a better test
- did an attempt at my own depth buffer and it doesn't work but it also doesn't work 
in an old commit from when i was using MTKView (but it does work if i do it in sokol-samples). 
- ok i feel like i gave the not having types thing a solid try but it's just kinda stressful. 
- can do a similar trick with instrumenting my objc_msgSend wrapper to spit out 
function signetures that i try to call (with the right parameter names, etc). 
- just something super hacky that im only going to run once. 
idk why some of them get `Attempt to use unknown class`, 
but hey, who cares, im only going to do it once. 
just interesting to see what it spits out. 
NSStringFromClass is in Foundation not CoreFoudation. 
today i learned those are different. 
```
X :: @static(bool) false; 
@if(@[@literal (selector.str() != "UTF8String" && selector.str() != "class"  // these make sense
&& selector.str() != "updateTrackingAreas" && selector.str() != "setFrame:")])  // but these don't
if !X[] { // :bindings
    X[] = true;
    reciever := @[args[0]];  // sketchy!!!! side effects
    scary_log(@[@literal log.items().sym().str()]);
    scary_log(" ");
    if bit_cast_unchecked(@type reciever, i64, reciever) != 0 {
        c := @objc @as(ObjCClass) reciever.class();
        c := NSStringFromClass(c);
        c := @objc @as(CStr) c.UTF8String();
        scary_log(c.str());
        scary_log("\n");
    }
};
```
- they have `_sapp_macos_show_mouse` which is never called?
- they have a list of cursor images where DEFAULT is carefully stored as nil 
but the only place you use it you have to check if it's nil before sending it a message. 
- // TODO: sokol docs say "the UTF-32 character code" but that's not what NSString.characterAtIndex gives you? 

---

I think if your program contains the sentence "Although this function looks imperative, 
note that its job is to declaratively construct a build graph that will be executed 
by an external runner." you are unserious. Like there is not high value in leaving in 
the random junk from the init template. Have your program be your own program. 
Tho it's kinda cool that I can search that string on github and find a higher 
than average percentage of things that interest me. 

## (May 5)

- fixed stack traces in new modules created in driver program so you can see past `build_for_graphics()` when running jitted. 
the compiler context now has a list of all QbeModules it knows about for when it wants to reverse lookup a function pointer.  
- style update in graphics/macos/app.fr to use new/cls/id/objc_set for so it matches `[...]`/gfx.fr
- make registering objective c classes less painful. 
take all the functions in a struct and make those be methods on the class. 
revealed a lot of bugginess with how `get_constant` interacts with 
literal coerceon (Str -> CStr, FuncId -> rawptr), so that should be improved 
before i can really argue that other people can use my compiler apis. 
- doing a cached SEL($CStr) instead of calling sel_getUid for every message send at runtime 
is a 15ms (5%) compile time cost which seems like a lot. it's only 202 specializations of it. 
i guess 202 is really a lot more than the 27 im doing for cls() but like... 
74 microseconds per? is that reasonable? 74000 clock cycles for one $function? 

---

so i want to use my windowing stuff with a native webgpu implementation doing the 
rendering. hopefully i can make a webgpu context with the same MTLDevice pointer 
i used for my own metal gfx implementation? notably the sokol-samples for wgpu 
don't use sokol_app, they use glfw. it seems like webgpu wants me to use a chained 
struct with SurfaceSourceMetalLayer to give it a CAMetalLayer, but i only have a 
MTKView which has a CAMetalLayer but maybe doesn't want to share? so my impression 
is the window will put views on the screen and metal will put pixels into layers 
and MTKView is apples helpful glue thingy to make it not suck. also public opinion 
on MTKView seems poor and CAMetalLayer is just better except you have to do more work 
yourself? which sounds like what i want because this whole project is an exercise in 
understanding how stuff works by controling as much of it as possible. 

- https://github.com/floooh/sokol/issues/1116
- https://github.com/floooh/sokol/issues/727
- https://developer.apple.com/documentation/metal/creating-a-custom-metal-view?language=objc
- https://github.com/floooh/sokol/pull/1136/files
  - no mtkview, yes displaylink
- https://github.com/floooh/sokol/compare/master...sapp-multiwindow
  - still uses mtkview but also displaylink
- https://dawn.googlesource.com/dawn/+/refs/heads/main/src/dawn/glfw/utils_metal.mm
- https://github.com/floooh/sokol-samples/blob/master/wgpu/wgpu_entry_dawn.cc
- CADisplayLink vs CVDisplayLink?

the extra you have to do is 
- depth stencil buffer
- msaa surface
- render loop (= display link?)
- poll events
- something when it resizes? 
let's just see what happens and if it breaks `examples/geo` then i know the magic words to google. 

## (May 3/4)

- i want to work on making my shader translation better but i think 
it's a bad idea to do that while i only have Metal because i don't know 
what things are different elsewhere so maybe first i'll add back the wgpu backend. 
- I don't understand why my webgpu.h is different than theirs. 
SupportedLimits vs Limits. 
theres emscripted vs dawn but they support both (and have a section at the top 
for dealing with a few differences) so seems it wouldn't be that. 
- comptime thing to make nice overloads for calling webgpu methods. 
just strip off the wgpuFirstArgumentType prefix, would be a good test 
encourage making it easy to add new functions from comptime. kinda bad to 
make the names different so you can't just google them but the js api has 
methods so these prefixed names are not useful anyway.  
had to add to the compiler: get_or_create_overloads and add_to_overload_set. 
and it relies on typename being its name in that scope: 
// TODO: `self.get_info_is_ready(ty) &&` but that's never set for .Ptr and i need that for wgpu bindings, 
//       maybe i should take out ptr again and write the wgpu as @struct(_: rawptr) instead of *@struct(). 
i did that change and it's also not set for those simple structs, so that still needs to be fixed. 
- fixed error locations for StructLiteralP type errors 
- fixed error location for `Poison expression EmptyBindingValue` when you do `name: Type;` in a block
- fixed aot stack traces on crash. 
i just fully forgot to do the relocations when i changed to Dat2. 
really need tests for that. 

// TODO: it would be nice if i could do optional arguments. 


## (May 2)

- make the backend's library tracking just default to libc like it did before 
so i don't need to deal with the old .ssa tests yet. 
- make `Expr::DataSymbol` let you say which library to import from so 
can get rid of the dlopens in macos/app.fr
- handle DataSymbol for jitting. just look it up in comptime_libraries like i do for functions. 
- graphics/easy.fr for less boiler plate in little examples where most of the code ends up being just starting the default window
- naming consistancy (sapp/sgl/sdtx).Common -> Self
- started a new mandelbrot example. add `if` to shader translation. 
- very sad that metal doesn't give you goto. 
not even nested break/continue. it's even worse than wasm :(

- TODO: allow linking to something without importing any symbols so you get the objc classes from it. 
- TODO: replace the func$module thing i used to do for wasm with the new libs thing
- TODO: unify add_comptime_library with the paths output in the exe but need to be careful about target platform. 
- TODO: allow importing symbols from different libraries but the same name so current system 
where you have a flat namespace and each symbol knows its library as an extra piece of 
metadata doesn't work at all. 

## (May 1)

- there's a bunch of `sg_query_*` functions that don't spark joy. 
the sokol-samples pretty much don't use them. 
i think if i add them back it would be as part of something that provides 
the same c abi as sokol so you could drop in try their tests. 
- cleanup: unify DynamicImport with ComptimeAddr. makes stuff look less confusing, 
you kinda had to treat them the same before anyway. 
- added new syntax, `..` to spread a value across a tuple, which when 
combined with coercing to arrays makes using default fields for the 
desc structs much more tenable. 
- not sure if that was the right choice. it's not really an expressivity improvement.
you could just have a macro do the trivial desugaring. it would maybe feel better if it was 
became a macro call but then you need to give macros access to the requested type which im not 
ready to deal with. it really makes the code more pleasent than it would be if you 
had to do `@spread_array(Array(SgLongNameDesc, 4)) ((a = 1), (a = 1),);`
- found crippling mistake where emit_ir/construct_aggregate/Array would 
evaluate to one past the end of the array instead of the start, just didn't 
matter because an `Expr::Tuple` wouldn't become an array before. 
- bad sign for my tiny graphics tests that the Desc stuff seemed to work with that broken
- want to stop using a linker for graphics programs so need to pass which 
library each symbol comes from all the way through to the backend. helpfully 
i already have that information since i need to know which dylib to look in when jitting. 
- as a starting point, super hacky thing that assumes the `/System/Library/Frameworks/`
path structure, eventually that should be done by the driver program i guess but 
this is already much better than "eh check libsystem.dylib, good luck" which was the old strategy. 
- hmmm, now trying to get the NSCursor class returns nil. 
right ok, some frameworks you link to just load objective c classes into the runtime, 
even without you directly importing a symbol from it. 
just dlopen-ing it is good enough but you probably want it to just work. 
- TODO: think about why i'm unwilling to go through the dynamic loader to 
get objc class references (just calling runtime functions instead) but i don't apply 
that logic to real symbol imports (could just call dlopen in franca runtime init and only 
use the loader to get libc, need libc on macos because the dylibs aren't real files you can find)

## (Apr 30) porting sokol_gfx

Mistakes (after it compiled)
- pool.queue_top off by one
- replaceObjectAtIndex:atIndex:
- setVorderColor
- init_pool calling alloc_raw without zeroing
- objectAtIndex vs objectAtIndexedSubscript
- setBuffer vs setVertexBuffer
- `popDebugGroup:` because i said `mtl.encoder().popDebugGroup(name);` (instead of no args) so it sent the overload set id called `name`
- !!!! it works !!!!

- still lots of stuff that's unreachable from app_events.fr tho
- funny that it takes ~100ms longer to compile which is a net win once i get rid of the linker
- oof, get_slot() not even using slot_index
- updated farm_game, edit
- build_for_graphics `-jit` use a seperate backend thread: 324ms -> 254ms. 
- but that driver itself is an extra 119ms which is way to slow
- `geo` has lots its points. just gray :(
my depth stuff is fucked somehow? 
it doesn't like `SgPipelineDesc.depth.compare = .LESS_EQUAL;`
it works if i make it GREATER/GREATER_EQUAL, did i like flip a coordinate somewhere somehow?
but no, if i do `>` i can like see through it in a creepy way, 
or maybe i just lost some defaults, since anything that has arrays in it is kinda unfinished 
until i have better array literals. 
maybe doing the validation stuff will magically reveal the problem?
seems like no luck.
found a line that makes it work if i comment it out `@objc d.setClearDepth(action.depth.clear_value);`
ha, classic blunder: that field is an f32. objc seems to mostly want f64. 
but not always (setBlendColorRed:green:blue:alpha:) so i can't just make it an error. 
- that was pretty successful all things considered, 
collected a bunch of things to improve in the compiler which was kinda the hope. 

##

- im ready to give up. 
```
if (!is_apple_gpu) {
    _sg.mtl.use_shared_storage_mode = false;
} else {
    _sg.mtl.use_shared_storage_mode = true;
}
```
- something i don't understand
```
bnd.pip = _sg_lookup_pipeline(&_sg.pools, _sg.cur_pipeline.id);
if (0 == bnd.pip) {
    _sg.next_draw_valid = false;
}
SOKOL_ASSERT(bnd.pip->shader && (bnd.pip->cmn.shader_id.id == bnd.pip->shader->slot.id));
const _sg_shader_t* shd = bnd.pip->shader;
```
- this is something im jealous of https://makepad.dev/

## (Apr 25/26)

- port example programs to use the new app.fr
- debugtext: don't hold on to the pixels after copying into a texture. 
make it easy to change the font data after setup if you want.
don't limit you to 8 fonts. like none of that really matters because anything real 
would use a real font but also the limits are arbitrary and i find that makes
the code more confusing cause im always looking for the place where it matters. 
- app_events: extend the example to use features where you get to change stuff about your window. 
makes it less minimal which is sad but i figure its more useful if it tries to use 
every feature possible so i can at least vaguely test them. 
- TODO: should see if sokol_app also resets the cursor image when you change the window title. 
- started poking at sokol_gfx.h, there sure is a lot of pool stuff that would be a lot less painful in a language with generics. 
`_SG_TRACE_ARGS` is gonna be a great thing to play with comptime for. 
- ive discovered wsl really doesn't like my compiler. 12 seconds is like a long time. whats going on. 
- finish copy/paste, file drop event, toggle full screen
- somehow objc blocks are forbidden knowledge. from my brief googling it seems 
theres no runtime function or method that will let me create one out of a c function pointer. 
and yet i apparently need to pass one to addLocalMonitorForEventsMatchingMask if 
i want it to not randomly ignore all key up events while holding command. 
why are we doing garbage waste of time suddenly? 
is pasting struct definitions out of clang docs really what you have to do? 
https://clang.llvm.org/docs/Block-ABI-Apple.html
- if i get to the point of making it work on linux i'd really like to be able to 
do the x11 thing with orb. 
this worked: https://www.nickgregorich.com/posts/gui-in-orbstack-machines/
i gave up on the ssh key thing and had to make myself a password, 
which is hard when you don't have a password. `sudo passwd luke`
- weird that when i messed up my set_icon CGImageCreate flags it was different jitted and aot.
- doing set_icon at startup doesn't work. 
you get weird blank page image, but so does sokol-samples/sapp/icon-sapp.c 
(and it's not even trying to set it at first) so it's not my fault i guess. 
maybe blank page vs little terminal is something about how long it takes the process to open a window, 
and it's just not allowing you to set the icon before calling run(). 

## (Apr 23/24) porting sokol_app

- stop copy pasting around the driver that links the graphics libraries. 
i still want to think of a nicer way to deal with it 
but just pasting it around in protest is silly. 
- resurrect my previous attempt at porting sokol_app from objective c
- app example program that just shows all the events 
- doing an insane waste of time dance to smuggle the pointer 
through an objc instance variable because i've been brain washed that static variables are bad, 
even though you'd probably never have multiple app contexts anyway (i assume there's a more sane 
way to have multiple windows if that's all you need). 
- 
```
2025-04-24 02:12:34.808 franca[17698:5068346] *** NSForwarding: warning: object 0x6000011d84e0 of class 'FrancaView' does not implement methodSignatureForSelector: -- trouble ahead
2025-04-24 02:12:34.808 franca[17698:5068346] *** NSForwarding: warning: object 0x6000011d84e0 of class 'FrancaView' does not implement doesNotRecognizeSelector: -- abort
```
that means you didn't extend NSObject (or anything because everything extends NSObject). 
happened because i tried to extend something before getting that class so passed nil as my super. 
- the thing were you can send any message to nil and just get nil back with no error 
message is kinda ass actually. like please just crash and burn and tell me whats wrong. 
like oh you typed `@objc macos.window.alloc();` instead of `macos.window = @objc c.MyWindow.alloc()`
that's not an error, thats actually fine, no window tho, good luck with that. 
at least in normal c every little mistake makes you segfault and a debugger can 
tell you where it happened.
- objc_msgSendSuper isn't magic. you need to tell it what point in the chain you are. 
- it's a bit scarry that i never use the _stret versions of objc_msgSend. 
it's very possible i've just gotten lucky so far cause most things are just `id` anyway. 
the meta lesson here is it would be super cool if DriverMessage::EmitDataEnd
were easy enough to use that it would be feasable to check
(but i guess that would need it to send you a message in the middle of doing abi stuff 
which is starting to sound slow for such a niche usecase). 
- ooo! compiler bug. `@debug_assert(macos.common().valid, "not valid");` compiles 
and gives you junk (that's not 0 or 1) when `common()` uses `#unsafe_noop_cast`. 
rn you're supposed to need a redundant `[]` in a call like that. TODO!
- ayy window on the screen!
- very long time on ok so most of the events work but nothings drawing on the screen. 
hmmm, thats weird, updateTrackingAreas has a panic in it but it isn't getting 
called even tho i call it directly. oh fucker. i register the method `updateTrackingAreas:`
which is not the same as `updateTrackingAreas` (has no argument so no `:`). 
so now that gets called and i get mouse enter/leave events, still no drawing tho. 
- ten thousand years of trying to make it actually draw. like i have no recourse 
when it just does nothing. i guess i can just keep making other stuff work and see if it 
comes to me. 
- if i turn on high_dpi i get `MTLTextureDescriptor has width of zero. MTLTextureDescriptor has height of zero`,
hmm, im reading backingScaleFactor as a f32 instead of f64. 
now no crash but still no drawing... 
aaaaaaaaaaaaaaa same for setContentsScale, needs to be f64.
criiiiiii, now it works!
- ... but only jitted. when aot, `NSApplication.sharedApplication()` returns null. 
wtf, i feel like that's not supposed to happen. im linking the same frameworks 
as the sokol one that works and i still have app_events_old.fr which works. 
i guess sokol uses the `NSApp` symbol instead of that return value. 
running jitted, NSApp is null until you call sharedApplication, and then it's the same 
as that return value. running aot, NSApp is always something and sharedApplication returns null.  
but the aot one is -6215112630617299873 which doesn't look like a pointer (jitted it's 5198880496 
which is in the normal range) and doesn't work (run() returns). 
can dlopen AppKit at runtime and it's the same as my import so that's not the problem. 
- oh also what the fuck! i was thinking oh i wish i could turn off alsr to compare the pointers to classes, 
but NSApplication is 8812562848 both aot and jitted which is really strange. 
just to make sure im not insane, if i print out one of my function pointers it changes every 
run which makes sense to me. ok if i dlsym to get objc_msgSend it also is the same every time 
so i guess there's only one copy of the dynamic libraries? 
but i thought the way that worked was sure there's only one real copy but you still map 
it into a randomized place for every process? clearly not tho? anyway, doesn't matter for my problem. 
just interesting that my intution for aslr is somehow totally wrong. 
- i would believe that my `#import` of a fn hack won't work aot because 
i'll always make a shim for them but that doesn't explain why sharedApplication() returns null. 
yeah that's where the number that doesn't look like a pointer comes from. 
lol, -6215112630617299873 is 0xA9BF7BFDD503245F, look familiar? 
```
0000000100023a78 <_NSApp__11722>:
100023a78: d503245f    	bti	c
100023a7c: a9bf7bfd    	stp	x29, x30, [sp, #-0x10]!
```
so now sharedApplication and NSApp[] are the same aot but it's null so that doesn't help me. 
- ok im going crazy. if setting a breakpoint in lldb for objc_msgSend (which is hard
because you have to break in main first before setting the breakpoint or you get 
stuck stepping through the loader) x0 is 0, you don't even get to the implementation, 
it just bails out because the reciever is null. but if i print out c.NSApplication 
before calling sharedApplication i get something reasonable. 
why is my code doing this: 
```
a.out`objc_msgSend__10538:
->  0x100024004 <+0>:  bti    c
    0x100024008 <+4>:  stp    x29, x30, [sp, #-0x10]!
    0x10002400c <+8>:  mov    x29, sp
    0x100024010 <+12>: mov    w0, #0x0 ; =0 
    0x100024014 <+16>: bl     0x10002c8a8    ; symbol stub for: objc_msgSend
    0x100024018 <+20>: ldp    x29, x30, [sp], #0x10
    0x10002401c <+24>: ret   
```
oh well great success, if i change `objc_msgSend :: fn() void #import("objc");`
to `objc_msgSend :: fn(asdas: i64) void #import("objc");` (and same for objc_msgSendSuper), 
it works now. so problem was in emit_ir, aot_import_body/bounce_body/emit_par_instructions 
it tries to put par/arg to forward, and `if info.stride_bytes == 0` uses `QbeConZero`
because clearly you don't need a par for a void arg... right... right...?
so here just casting it to a function pointer didn't let the callee see the real args, 
since it helpfully set your x0 to the blessed value of void (0). wild. 
i guess that's a compiler bug, i probably shouldn't be injecting code 
inside your function call that zeroes registers at random, but also it's 
kinda reasonable if it's considered disrespectful to lie about a function's signeture. 
anyway, rare W for lldb. 
:TodoDontZeroX0WhenVoid

## (Apr 22) wasm backend

- mem1.ssa
  - use the right index for indirect return type
- call1.ssa
  - DataAbsolute fixup for function index
- load1.ssa
  - wan't calling wasm_push for phi args so if they had an slot assigned from an alloc, 
    the var was never written to.  
- fold3.ssa
  - implement Select in import_wasm
  - TODO: doesn't pass verifier but works on my runtime
- dynalloc.ssa
  - very half-assed implementation of alloca because i never use it
- strspn.ssa
  - `bad loop nesting Expected (10 == 9)`
  - i think im getting screwed by "with the constraint that once a loop starts 
  all the subsequent blocks must be dominated by the loop header, until all the
  loop blocks have appeared"
  - but like the stuff i stole from qbe is already doing a thing about counting 
  how deeply nested a loop is and using that for the rpo thing if it's been computed 
  already. is there any chance that's the same as what i want and i just need to call that 
  and then re-rpo? it seems like yes... test works.
  hopefully that's real and not just it happened to mix the order around in a way that fixed my one test. 
- echo.ssa
  - need to pass it the cli args. untangling u32 vs u64 will be sad if i ever want to run someone else's wasm module. 
- 57 -> 69. that's all the `.ssa` tests working, with my wasm runtime tho. 
next step is to make sure they all verify correctly i guess. 
pretty close: 11 of 69 tests failed.
- so the question is would i rather do soul crushing truncation adventure or make the c compiler work for wasm? 
import_c/test/test.fr: 23 of 35 tests failed.
- confusion about when debug assertions are enabled because when you jit run it uses the compiler's backend to avoid needing to compile it. 
wasted some time finding where it was asserting because my report_crash wasn't giving me something 
useful. TODO: maybe it gets confused by having multiple threads? 
- vararg call always tries to pass something based on SP even if no args actually passed 
so since f.slot==0, it didn't load SP and then wasm_push_addr asserted. 
ooo that helped a lot: 8 of 35 tests failed.
- it's looking like this might just work immediately for free? 
just adding more libc stuff to import_wasm/run: strcmp/memcmp/memcpy/strncmp/sprintf/vsprintf. 
1 of 35 tests failed.
- function.c seems to be the only hard one. there are many (non-overlapping) chunks of calls 
i can remove that make it work. something doesn't like when a function is too big? 
but it can't be the backend. most of my programs have some monster functions. 
maybe im doing leb wrong when a wasm function is too big? but i feel like 
lengths of data and stuff are always big so that seems unlikely. 

## (Apr 21) chipping away at wasm tests

- ops.ssa
  - hack my fake printf to make it display right
- vararg2.ssa 
  - gets `unable to read i32 leb128: i32.const value` from wasm2wat, 
maybe if you want 0xFFFFFFFF you need to encode that as -1 with sleb? so i need to sign 
extend c.bits.i before trying to encode it. 
  - doing that makes it get hella type errors so i guess that's progress. 
  thats that vaabi is putting RSlot and im emitting those as Kl without truncating. 
  - fixing that leaves it with just call errors. 
  oh ok, i think the rules are you need to put the instruction for `...` 
  even if there's no varargs being passed for that call, which my c compiler doesnt do. 
  - but doing that breaks a bunch of old tests because the c drivers were forward declaring 
  like `void f()` which was treated the same as `void f(...)` so it thinks its doing a va-call 
  but since the def isn't va it might get inlined even though my inlining wants to not deal with va. 
  - fixing that and now it's `allocating so many bytes bro`... in push_float, 
  unless you `@eprintln("%", self.f.ncon);` in there, that changes it to `con oob` somewhere the fuck else. 
  memory thing i guess? changes if you fmt into temp() or general_allocator(). sad. 
  was resetting temp but telling it to assume_owned(self.stack, temp()) and just clear it when starting a function. 
  silly!
- abi5.ssa
  - convert pending_indirect_return to a stack slot
  - don't add the indirect return to the signeture twice
- abi1.ssa
  - changed the test to not assign temps for the result of void call. 
  - works if i align to 16 before a va call instead of 8. very afraid that's just luck because i don't understand. 
- abi3.ssa
  - indirect calls super didn't work. especially if you're trying to call an import. 
  i guess i started implementing that and then got struck by lightning perhaps? 
  pretty easy fix tho. 
- split conaddr.ssa because writeto0 isn't special on wasm
- 48 -> 57
- started some docs about generics

## (Apr 20)

- uninspired so perhaps more playing with example programs is in my future. 
- new example: kaleidoscope (little language from llvm tutorial). 
  - that was quite successful. wrote a nontrivial program, didn't find any compiler bugs, 
it pretty much just worked like a normal language, thats crazy man. 
  - it's less code than the c++ version which is pleasing
  - had a confusion with the backend api when you try to get_addr for 
  something inlineable that you forgot to mark exported. so added an 
  assertion message for that. 
- making driver use different module than comptime makes running bigger 
  programs with `franca foo.fr` much faster (950ms -> 650ms for kaleidoscope),
  because then i do them on different threads. 
  also means you won't have jit shims. is that the semantics i want? 
  feels kinda sad to not take advantage of host and target being the same 
  to only compile things once if used at both comptime and runtime. 
  also leads to a sad situation where putting `::` in front of your `main()` body
  (which should be the same when running jitted) makes it much slower. 
  so maybe what i really want is for the comptime situation to adapt to whether 
  the program seems large and notice the situation where you want to do a call 
  immediately to make progress so should do it on the same thread. 
  because the last attempt at using another thread for all comptime stuff 
  was slower because i naively block to wait for everything in case i call it. 
  the other thing was floating the idea of using more jit shims so you could do things 
  like `qbe_frontend.fr` only compiles the c compiler if you actually use it. 
  so the other perspective is compiling in parallel is less important if you can
  just skip compiling most of the program. doing both would be better tho. 
- new example: turing_art
  - embarasing amount of time wasted once again on negative mod. 
  maybe i should give up and have mod/rem/(whatever the fuck else) like zig. 
  kinda annoying that the one you're supposed to call "mod" is not the normal programming one. 

THINGS I ALWAYS LOSE 
- https://torstencurdt.com/tech/posts/modulo-of-negative-numbers/
- https://www.chiark.greenend.org.uk/~sgtatham/coroutines.html
- the name of the rust benchmarking thing is "hyperfine"

## (Apr 19)

- finish converting old versions of some asm intrinsics to a test
- the problem i was having with lox was only slightly a memory safety thing. 
im using mod to map hashes to indices so it doesn't like negative numbers so 
checked_hash does an `abs` but in lox i have a copy-paste of some of it that 
caches the obj hashes and that wasn't calling abs so everything got super confused i guess. 
once again it is revealed that using i64 for slice indexing and not bounds checking for negative 
numbers is a supid idea. maybe the memory safety argument isn't that the problems are super 
common or hard to fix, it's that you're giving small bugs the power to break everything 
instead of being localized. 
- fixed prospero safety checking on decreasing ntmp too soon 
- experimenting with the rules for writing to a file while it is executing on linux (and various emulators). 

## (Apr 18)

- get import_wasm working well enough to do the bf example hello world
- just adding `.CVariadic` isn't enough to let me call an import_c `...` function. 
for some reason it has a result type so is calling tuple_types to check the args expression, 
but it doesn't do that if i'm just calling a function written in franca. what's the difference? 
hmmm, this is unfortunate. it matters if we have it as a `FuncId` (call_direct immediately), 
or a `Fn` (first compile the arg then call_direct). 
just making import_c say the constants it create are of type FuncId makes it work. 
but that feels fragile, it should matter if it's FuncId or a Fn. 
Oh the difference i suppose is that when you have a parameter of type `@Fn` it will 
be a Fn and when you call a normal function it gets it's type from an `Expr::Closure` 
which always says `FuncId` because you don't want to force the arguments to infer too early. 
ok so then if you take out the difference and make it just do call_direct on the `@Fn` as well, 
it tries to infer_arguments on the fid to get the arg_ty instead of just looking at the 
type of the callee ast node. but when you pass a closure you don't put type annotations, 
you expect to get them from the parameter's type annotation instead. so just need to pull 
down that check to a place where we know it's not variadic but before asking the 
function to infer args. very bad sign that im relieved to have changed something 
in the sema file without causing the world to collapse around me. 
- fixed a `unhandled node type CVariadicMarker` when you try to `::` a varargs call 
- import_c keep enum names and export them to be useable from franca
- removed "VoidPtr: hack to match old behaviour and still call intern_type at the beginning", 
so now c `*void` will export as franca `rawptr` (without `Type Error: expected rawptr but found rawptr`).
- still don't know why there were two entries for .VoidPtr in the fixed types but we used the latter 
one so it's fine to just skip an index. it seems i just randomly added an extra one when i
started hardcoding type id numbers (10 months ago). maybe i just didn't notice it was already there? 

## (Apr 16/17)

- documentation is hard
- making sure every example program as a comment saying what it does
- examples/toy/spill.fr doesn't demonstrate the thing i was trying to 
- view_image: use colour escape codes instead of outputting PPM
- new examples/bf: create a wasm module 
- rediscovered that import_wasm doesn't support implicit return at the end of a function 

```
- code {  // [85..<104]
    Block "" { 
        Loop "" { // 1: 
            LocalGet(0);
            I32_loadub(a = 0, o = 0);
            I32_Const(0);
            I32_ceqw();
            BrIf(1); // :0
            Br(0); // :1
        }; 
    }; // 0:
}; 

export function $main$module() {  # nblk=8
@0
	%env.65 =l pare
	%wasm_globals.64 =l par
	%L0.66 =w par
	jmp @1
@1 # preds: @0, 
	jmp @3
@2 # preds: @4, 
	Jxxx 0, 0
@3 # preds: @1, 
	jmp @5
@4 # preds: @5, @6, 
	jmp @2
@5 # preds: @3, @7, 
	%getL0.67 =w copy %L0.66
	%a.68 =l extuw %getL0.67
	%a.69 =l add %wasm_globals.64, %a.68
	%load.70 =w loadub %a.69
	%wasm.71 =w ceqw %load.70, 0
	jnz %wasm.71, @4, @7
@6
	jmp @4
@7 # preds: @5, 
	jmp @5
}
IR failed typecheck: block @2 is used undefined
```

## (Apr 15)

- started splitting up import_wasm to make it useable from comptime
- removed some old unused function tags
- attempting to write more documentation

## (Apr 14) 

> fixing import_c/ffi jit

- jitted wuffs_drop_in__stb__load1 is calling a null function pointer. 
ok wuffs_base__image_decoder__set_quirk does a weird vtable lookup thing. 
- oh, maybe that makes sense. it's a function pointer in memory, and normally when 
jitting franca code, you'd only be able to produce that situation by writing an address from
comptime code so the compiler would already have made a shim for you or you wouldn't have the address, 
but with the c compiler, you can get to emit_data with a relocation to something that hasn't 
been compiled yet, but we do always push_fixup. 
- ok i think the path is, it's only reachable through the data relocation, it gets added to EmitIr.pending,
then those go to create_jit_shim, but that doesn't do the fixups, because franca code won't need it. 
so then you never call through the shim and it never notices it needs to be compiled. 
- so if im right, then adding to mutual_callees in the loop over pending in shallow_jit_func should fix it? 
yeah, now it gets to do_jit_fixups, and i just have to take out the part where i clear them if got_indirection_instead_of_patches,
because now you do in fact need the data patches, and that makes it work on jit. 
but, taking out that clear of the patches breaks aot, things don't end up in baked.functions anymore. 
- so the problem there is that for normal jitting franca, the address you'd put in memory would be a shim, 
and create_jit_shim records that address if TookPointerValue, but now that we want to patch over that 
as soon as the real address is ready, the one we try to lookup when baking will be different, 
so that's fixed by just recording the real address after emit_ir. 
- which actually that's much better anyway maybe, because now jitted code calling through a vtable 
doesn't need to take a super slow detour through the compiler every single call. 
so i probably do want to keep it this way even if import_c didn't need it. 
- yikes. that was pretty convoluted. it would be nice if i could design this to be less of an error prone infinite nightmare. 

---

> making linux presentable 

- trying to use a -syscalls compiler to aot compile a program that links libc:
`we hit a dynamicimport with no comptimeaddr for jit. the frontend should make sure this doesn't happen.` 
because use_raw_syscalls is shared for comptime+runtime so it decides it wants the #libc version 
and then when it can't call dlopen to get it, it gives up. 
i guess the easy way out there is for emit_ir to choose the #syscall version for jit if it's not linking libc, 
regardless of what you asked for. 
- that doesn't help for situations like 
memmove/memset where i want to prefer the optimised libc versions if you're linking that 
anyway but fallback to mine if not. i could fix that in the same way, if i make my versions 
use the same name and just have #libc and also a body and change emit_ir to prioritize 
imports over normal body. 
- but it feels kinda bad that i've been avoiding thinking about how to redesign TargetSplit for a long time. 
maybe i'll just turn off fast memcpy until im ready to deal with that. 
- more important is that the compiler doesn't work with glibc. 
- for one thing i seem to have lost the `/lib/x86_64-linux-gnu/` when switching to multiple 
paths in find_os_libc_dylib, but actually that seems to not matter. 
#include <gnu/lib-names.h> LIBC_SO is just `libc.so.6` so i guess it figures it out. 
which should have been obvious since i can cross compile hello world and that's fine. 
it's just running the compiler itself that dies. 
oh im stupid ok `#use(MacosLibc);  // TODO: linux` no shit 
- turn off one basic_libc signals test for now
- at some point since i wrote toy/cpuid.fr i fixed abi for Array(u32, 3) to not return in memory so it broke
- lox_main `double free or corruption (out)`. i guess it works with -syscalls because my allocator doesn't check and i get lucky. 
unfortunately turning on debug_trace_execution in the vm fixes it since that debug printing was using the same allocator. 
welp i cant do it, time to pack up and become a garbage (collected) language i guess. 

## (Apr 13)

- new system where you always compile into a different module and copy over was a lot easier. 
- translating types really sucks tho. 
- this fails a safety check:
```
b := baz(b);
@assert(b.a == 5.0 && b.b&[0] == 1 && b.b&[1] == 1);
```
but not if you split into two @assert and not if you print something before, and not if you specify a return type for the enclosing function. 
confused. TODO!
- also broke bf/c_source. now an `idom` is randomly null? that's gotta be a memory thing now that people are sharing temp. 
thats unfortunate. but no i can disable reset_retaining_capacity and it still happens. 
oh it's because for_blocks goes by `link` and `sdom` goes by `rpo` so if i take some blocks out 
of the link chain, `sdom` still sees them so i need to copy them. 
- `Assertion Failed: cant inline call with wrong argument type. (callee = strlen__27729, caller = bar__27728)`
when you try to aot. didn't happen when jitting because aot tries to reorder to help inlining. 
and bounce_body isn't making the right par instructions because im not setting the func.args bindings.
easy fix.
- forgot to move f.retty
- move switch
- oh damn, good progress. view_image.fr works aot but not jit. 

## (Apr 12)

:AssemblyLanguageForShapes

instead of always evaluating the original expression, recompile the simplified version
a few steps down the quad tree and then use that for all the children. 
`976.1 ms ±  14.9 ms`

clealy im missing something cause theirs takes 44ms. 
it does make me feel a bit better that a clean build of their thing takes 53 seconds. 
so they run 22x faster but at least my language that compiles 100x faster. 
oh and dont forget im cheating by preprocessing the input a bit but it's 10ms and im so bad that doesn't matter yet. 
and like their jit is way more specialized for this than mine so i could accept that mine is 
too slow to recompile enough to take advantage of the interval simplifications, 
but it doesn't matter, it's still fast with --eval=vm, so the trick is entirely that you 
skip work with the interval stuff. maybe my interpreter is just so bad it dwarfs all the work you save? 

i sure am doing a lot of libsystem_platform_memmove, like a lot, a lot, like half the time. 
i guess i'm always copying the whole big function's tmps even once they've shrunk a lot. 
yep, if when doing the dead code elimination, i also renumber, it goes away. 
now i can scale from 6 to 8 levels and speed up, and jit an extra level deeper.
`718.8 ms ±   1.3 ms` now only 16x to go, sounds like a lot when you put it that way. 

don't need to copy con/tmp since i only append: `661.6 ms ±   0.8 ms`

---

the problem i was having with import_c/ffi was that the symbol id in the c compiler doesnt 
match the mangled name the franca frontend wants to use. 
```
// wrong because different name id
// > self.entry.task = (Func = f);

// this doesn't work because ffi didn't put stuff in args
// > self.bounce_body(self.f.symcon(f.lnk.id), .call);

self.entry.task = (Bounce = (lnk = self.f.lnk&, target = f.lnk.id));
```

and then same sort of trick for imports (from forward declarations).
so that works for functions now. but now i have to do the same for 
data. i was hoping i could make it work before making it sane but 
im really going down a garbage direction here, so maybe i need to rethink.

- oof, `rel := c.arena.box(Relocation);` instead of zeroed so .next was garbage,
not caught before because import_c doesn't reset temp. a rare one that i can't make 
an excuse for it not being memory safety related. but still why's that a linked list anyway, 
doesn't spark joy. 

## (Apr 11)

:AssemblyLanguageForShapes
playing with interval arithmetic: 1.034 s ±  0.002 s
- recurse down a quad tree and optimise based on input ranges 
so if the output is known +/- you don't need to evaluate each pixel.

## (Apr 9)

found something that is maybe really useful for me: 
https://docs.kernel.org/userspace-api/landlock.html
because im kinda freaked out that like the first time i run some tests after changing something in the backend, 
maybe i broke something and oops the register allocator put the wrong number there and 
now your syscall changed to be deleting an important file or whatever. 
because like when random garbage happens by mistake because the compiler broke while experimenting, 
you might catch the bug immediately but you still had to run it once and then you're 
just getting lucky that most permutations of a given program are crash and not do something scary. 
a simple example might be oops swapped the read syscall to write so instead of reading all your 
source files into this uninitialized memory, i wrote over all your source files with garabge. 
it would be nice to have guarentees against that. 

eventually i want to do some sort of capabilities thing (as a language? or just in the libraries?) 
to make it easy to say hey my png library can't make http request or whatever. 
but anything i do like that needs to depend on the compiler working but one of the things 
i want to defend against is a buggy compiler stomping on stuff. 

---

:SlowAlignmentAnnotation about fn alignment() in compiler/values.fr:
no it's not compile speed because the first run after you comment it out is still slow. 
this costs 40ms somehow 
in fact, the !null case is NEVER taken in the compiler, 
you're telling me a correctly predicted branch costs 40ms/16427 = 2.4us? really? 
its still slow if that's `if false`, which gets sccp-ed out (i can check asm). 
so ok back to my theory of the problem is compile time. 
#inline doesn't matter
0: frontend: 1007ms, codegen: 475ms
then comment out condition
1: frontend: 1013ms, codegen: 476ms
2: frontend: 976ms, codegen: 434ms
so since 1 was slow. the problem can't be the time to sema the slow code. 
the slow thing is the generated code. 
ohhh, hmmmm, this is unfortunate. maybe it's about spilling locals. 
the first diff is fast saves x19-x22 and slow saves x19-x25. 
dont really understand how it can change the backend time so much as well. 
but no, didn't i just say `if false` didn't fix it? 
clearly im confused about something. 

---

:AssemblyLanguageForShapes

- was reminded of https://www.mattkeeter.com/projects/prospero/ and did one with my jit.
hella slow but i love it anyway. so neat to see the math spit out words. 
- fixed bug in make_exec() where it wouldn't work if the code spanned multiple pages. 
no idea what i was thinking when i typed that. yikes! 
but previously it was only used for `qbe_frontend -r` 
(which i don't test because i didn't used to have -cc) 
so this was the first real stress of it. 
- added a float sqrt instruction to the backend. 
reassuringly, its the same speed as libc on examples/toy/pi.fr (1.5s hard vs 4.1s soft)
but saves a lot of spilling on prospero i guess. 
- added instructions for float min/max

```
franca driver.dylib build examples/prospero.fr -o a.out -unsafe && hyperfine "./a.out > /dev/null"
libc: 3.782 s ±  0.024 s 
sqrt: 3.445 s ±  0.008 s
fmin: 2.576 s ±  0.022 s
fmax: 1.557 s ±  0.002 s
```
(for reference some other guy's cranelift one is 0.673s on my computer so mine is still super slow)

I could support sqrt/min/max for ints 
but they don't give me cpu instructions for that so meh, 
it would just be for symmetry. 

- added sqrt/min/max on amd64
- this did reveal a bug on x64 (for other instructions as well) 
memargs thinks float ops can access memory but the encoding tables don't deal with that. 
and some (ie. add) need to be 1 instead of 2 because they won't let you encode both directions (dest can't be memory). 
for now i don't care enough to deal with it so i'll just hack it out. 
the memargs is only used to save a few instructions when things are spilled, 
it won't merge an explicit load/store into an arithmetic instruction anyway, 
so i assume it can't help that much. 
- run the .ssa tests jitted as part of run_tests

TODO: another bug! doesn't work if you run it like this `franca backend/meta/test.fr isel4.ssa -jit` same with abi1.ssa
TODO: use the new instructions from lib/math.fr
TODO: ops.ssa test for the new instructions

## (Apr 7)

what the actual fuck?? am i going insane??? 
my program is suddenly slow but not if i run it in a profiler?
im losing my mind apparently.  
i run it in warp: `./a.out self.fr  1.45s user 0.09s system 105% cpu 1.464 tota`
i run it in terminal.app: `./a.out self.fr  1.41s user 0.09s system 142% cpu 1.053 total`
oh you know just... restart warp... to turn threads back on in programs it execs????!!
how can that even be possible?? 
how can it be that i am forced to choose between being able to click with the mouse
where i want the cursor to be and being able to run a process that spawns a thread. 
written in rust by the way. ai built in by the way. garbage. 
why does everything have to be so hard? 

---

getting more wasm tests passing. 
- they don't have a cmp ordered/unordered. so replace with nan check. (isel2)
- more work on varargs abi
- sel_wasm was try_kill_inst on unused par
- truncl on retw
- don't try to export data symbols as functions. fixed many that returned via `int a;`. passing 31 -> 43
- need my own printf
- push_slot_addr was going backwards
- untangling when pointers are i64 vs i32 is gonna be a nightmare

## (Apr 4-6)

- update ssa parser to use new Dat2
- what's the difference between `0xAC ⇒ i64.extend_i32_s` and `0xC4 ⇒ i64.extend32_s`,
one is i32->i64 and one is i64->i64?
- some of the ssa tests have a harness written in c that i use clang to compile. 
so instead of that, have it use examples/import_c and just add the c functions to the same 
module without going though an object file. 
old way was annoying because 
  - relying on a system installed thing is uncool
  - it means i need to support outputting a relocatable object file before i can run most of my tests on a new platform 
  - it's kinda slow? 
  if i have a 70ms tax on each of 45 tests that i want to run on 3 architectures, 
  thats suddenly an extra 10 seconds every time you change something which seems unreasonable. 
  ```
  before: backend/test/sum.ssa                    [ok] 4ms, 85ms
  after:  backend/test/sum.ssa                    [ok] 7ms, 12ms
  ```

the new way doesn't quite work tho. 16 fail. 
- was discarding c functions it thought were unreachable because it thought that was the whole program. 7
- a few additions to standard headers in include.fr. 4
- c parser didn't allow `type function(), variable;`. 1
- fix crash when accessing anonymous union fields. 0

time to run backend/meta/test.fr:
- old: 4.31s
- new: 0.95s

still have to run the slow version because it tests relocatable objects and compares c abi to a known good compiler. 

so now i can run more wasm tests. passing 16 -> 29.

## (Apr 3) Thinking About Incremental Compilation

maybe that mini project makes me change my opinion on incremental compilation.  
because like this progam compiles in `<`200ms but it still felt annoyingly slow in a way
that working on the compiler never felt. there were times when i was just playing around 
with which direction you had to multiply matrices or how to map intensity values to colours 
in a pretty way, and there was a phase after i had laz decompression working but before i had 
chunk streaming where you had to stare at the blank window for a couple seconds while it loaded 
on the first frame before you could see if the changes worked. so it wasn't that the compiler was slow,
it was that you lost the state that was built up before you got to the interesting part. 
and like ok i could add to the interface and let you change transformations or whatever in the program 
but thats not what i want, i just want to change the number in the code man. 
there's no reason that has to suck so much. 

also this is fucking amazing to me https://www.youtube.com/watch?v=yY1FSsUV-8c  
sure as hell can't do that in my language right now, not that i want to, but it's kinda sad that i couldn't. 

the simple case might not even be hard. like there's an infinite amount of work in trying to make it 
track dependencies if you want to allow inlining while doing incremental compilation or what if you want
to change struct layout? does that have to rewrite all the objects in memory somehow? do you need to 
provide a remapping function every time you add a field? that can't be a good idea. or what if function 
arguments change? or maybe another thread is running in a function while you're trying to replace it? 
or if it's for aot you have to make sure you have space in the sections or you have to change all the offsets
and keep enough information to map the binary's symbol table back into FuncIds in the compiler frontend. 
for things that have $constant parameters, you need to be able to trace back all the specializations 
and recompile them too, even worse for #macros or any comptime code that has side effects. 
like what about the functions that get reflected on to generate shader code? 

so like ok, i accept that i don't have the resources to solve the hard problem well 
while making the kind of language i want to make (ie. not java). 

but also, the usecase i was describing above doesn't actually care about any of those problems? 
like there might be a very high quality of life improvement just in being able to tag one pure 
function that does some math as `#incremental` and then while running jitted you can just edit 
the text and have it hot load when you save the file and use the new version on the next frame. 
and i know people do a hot loading thing in c or whatever where you call some functions through a dylib 
but first, the base time to `echo "int add(int a, int b) {return a + b;}" >> a.c && time gcc a.c -dynamiclib` 
is 80ms, can't be doing that for every function presumably. then how to you have it call other functions 
in your main program? are we suddenly in crazy town again where you're making a plugin system for every 
program? i just want to change a number man. or maybe you have the whole thing that runs every frame in 
the dylib and the hotloading part is just to keep the data in memory. that would probably work, if its like
200ms to compile the thing that's fast enough to be fine but it feels like the one advantage i have is 
that i built the whole foundation of my compiler so i don't have to be limited to serializing everything 
through an executable. 

i already have a lot of machinery for dealing with the combination of dependency cycles and arbitrary comptime execution. 
there are situations where you need to know the literal address of a function before it can be compiled, 
and i do that by making shims that save thier arguments and then call into the compiler to finish compiling 
the real function. so all i'd need to do to hotswap a simple function is replace it's body expression 
with one parsed from the changed file and then conveniently forget that it had already been compiled so 
calling through the shim would just redo the work and get the new version. and that doesn't address any of the 
hard problems but it might just work? 

the painful thing is that i try to make direct calls since that's way way way faster but when running in incremental 
mode you probably always want to go through the got so you can get the new shim if needed. 
a hacky soloution good enough for a tiny demo is to compile a new callsite every time but that isn't good enough 
because it makes the calling code look insane. so then what are the options? 
- always call through the GOT. that makes everything slower to enable the rare swapping case. feels bad. 
- track the patches like for aot and stomp in the new address in the machine code. 
that's more memory usage (but maybe not in a way i care about), makes the decision about which patches 
need to be applied more confusing. right now i know that for JitOnly, it's always no patches or one GOT patch. 
need to deal with flushing the instruction cache. 
- could patch just the beginning of the old callee's machine code to call the new shim the first time it gets patched. 
that has the same caching problem and it's extra scary if another thread's inside that function (tho maybe i don't care 
about that case for now). 
- require manually marking `#incremental` and then only do the slow GOT thing for those. 

have to think about what problem im trying to solve. 
honestly i want a flashy demo that i can point to as something cool that was made easier by 
controlling every stage of the compiler. but i also want something that doesn't feel like it's trying too hard. 
i want it to fall out of the work i already did so it feels natural. not like i spent months making a whole 
new system to enable this but sort of like the way comptime feels where like oh of course you can call a c 
function, why would i go out of my way to stop you from doing that. 
my whole goal is kind of to have something where it's not like im trying to be better than other compilers/runtimes,
i don't want it to be interesting that mine's good, it should be interesting that other ones are super limiting after a lot more work. 

just have a build option that makes it always call through GOT is the easiest answer. 
but then suddenly my single compilation unit thing kinda sucks because "oh your 
high level ui thing needs hot loading? ok i helpfully disabled inlining in your matrix math library
so your whole program's slow now. you're welcome." 
that can't be the policy. 

---

:ByteOffsetUninitRootCause

fixed the compiler bug with unset field.byte_offset. 
this is a bit over kill for a fix with a two character diff but in the spirit of not letting confusion fester: 

i had this: 
```
enumerate pattern.bindings& { i, b | 
  field := or find_struct_field(f, ..., i - 1) {
```
which passed -1 as the guess for index 0. 
```
if index_guess >= f.fields.len || f.fields[index_guess].name != name { 
```
so since i use signed numbers for everything, -1 is `<` the length, and my bounds checking doesn't check 
that it's possitive because im silly so it would just read whatever memory is behind the fields array. 
and i guess most of the time that word happens to not match the name you want so it does the scan to find the right one, 
but for this specific layout i bet the last thing allocated was a smaller version of that fields array that got resized. 
so thats why the name was there and that's why the number we got for byte_offset was FIELD_LAYOUT_NOT_DONE 
which made me assume it was a compilation order problem. 

and that's why the bug was so sensitive to field order / count. 
it happens when the last field's index is a length that will cause a List resize 
and the construct_aggregate uses that field in index 0, so offsetting backwards 1 will 
get you a matching name because you get the old version of that same field, just without the 
byte_offset set because that resize was while building up the struct before calculating layout. 

and if the compiler had debug checks off, it would generate code with the field offset being 99999999999 
in the initializer so that's why it would either crash (if that wasn't a valid address) or when you tried to 
read the field it have some bits set instead of being zeroed (because the initialization was way the fuck somewhere else). 
but accessing the field later used the right offset because sema/field_access_get_type didn't have that bug. 

but also why would i think typing the -1 was a good idea? 
because 5 months ago i pasted that emit_ir from the old emit_bc which had this
```
i := 0;
each f.fields { field | 
    continue :: local_return;
    i += 1;
    if(field.kind == .Const, => continue());
    // ... 
    field := or find_struct_field(f, name, i - 1) {
```
because it was from before i reworked scoping and seperated constant and runtime fields. 

it's fascinating how broken it can be and still mostly work. 

--- 

- clock_ms nanos off by 1000
- clean up farm_game screen messages a bit
- examples/geo scale movement by dt so you don't move slower while loading

## (Apr 2)

- binary search instead of linear scan, makes decode_symbol go from 74% time (15k samples) to 58% (6k samples). 
(the original thing where i always scanned the whole list was extra dumb 
but going backwards and stopping at the first match was still 14k samples, so binary much better). 
- fix delta_median to just do swaps after inserting instead of quicksorting the whole thing.
2270 samples of inner_sort (22% of all) gone at the cost of next() self samples 340 -> 490, so great success. 
- also i saw `inner_sort__13494` and `inner_sort__13476` which should just be calling the 
same specialization... am i really making a new copy for every callsite?? 
I really hope so because that would be great compile speed improvement to fix. 
ohh, not quite, it's because i do `sort :: import("@/lib/sort.fr")'quicksort(i32, fn(a, b) => a[] < b[]);`
inside the closure that gets inlined twice so it doesn't know that the comparison function is the same. 
it re-handle_compile_func_body for each and they get a new FuncId. 
so that would be fixed if emit_capturing_call treated AllowRtCapture and #inline the same 
so didn't repeat work that way. not as exciting tho. 

## Mar 31 / Apr 1

one hundred and one point cloud mistakes

- not setting total_count before calling update_distribution the first time 
- chunk size delta is always 1... decode_symbol is always 0...
i imagined distributions as ordered lower bounds so 
`while => self.distribution[symbol].zext() * ltmp > stream.value { symbol += 1;`
but no, thats not the same. now more sane: first is high because it's delta from 0 and then rest are smaller. 
- hmm 2148447271 sure looks negative. but -2147097928 is also not sane
- k_coder.init(n_bit + 1) 
- ugh failed copy paste. `self.distribution[symbol]` -> `self.distribution[s]`
- cheating by looking at the rust one's output on my file. 
im right for entries 0-18 in the chunk table. 
then im wrong: `k = 13 c = 214 lower_bits = 25 c = 6873 k = 13 c = 6874`
final c is supposed to be -34919. huh, k is supposed to be 16. 
dumbass i was was cleverly storing the count of symbols seen to avoid re-summing them 
to check when to halve in update_distribution but wasn't incrementing it in decode_symbol. 
so now i can get through the chunks table. 
- off by 8 because i didn't skip the offset in chunk_table before reading the first chunk
- i had the order of bit_fields backwards. tho in my defense, the spec reverses the order of the table randomly. 
- now i can read the first compressed xyz successfully. 
- rust one thinks the colours are all the same but mine are different. 
do i have the bit order reversed again? no. :LazSpecLies
RGB in changed bit set, the one for r being identical to g/b is backwards from the spec????
or im going crazy and can't read. 
spec says "If bit 6 is set, Green and Blue are equal to the uncompressed value of Red", 
but the c++ code says `if (sym & (1 << 6)) { NOT equal } else { equal }` https://github.com/LASzip/LASzip/blob/master/src/lasreaditemcompressed_v2.cpp#L477
- now im right for points 0-3. 
4 should be `4294605888 4294935618 4294967247` but i get `4294605952 4294935618 4294967247` then colours are garbage. 
i was clamping to 255 for the high byte instead of 255<<8. 
fixes colours but not positions. 
i32 vs u32 in streaming_median fixes it. 
- on the plus side, i know why i had a bonus half rocket ship on the other side of the universe, the coordinates are supposed to be signed. 
- now 8.x is wrong. wasn't sorting streaming_median correctly.
- 16.z -215 vs -271. typo used self.y& again
- 77.z -3008 vs -350. (big jump tho. progress). 
the corrector for k=0 needs to be shared like the others. i was doing 1 per instance. 

## (Mar 31)

`franca self.fr && ./a.out self.fr && cp a.out b.out && hyperfine "./b.out self.fr"`
required (bug???): inline, copy_elimination  
amazing (300): collapse_op_arm64+fix_memarg  
good (100-150): elide_abi_slots, loadopt, promote  
ok (20-40): fold, coalesce, fillloop+fillcost  
pointless: simplify_jump_chains  

interesting factoid: since im using the compiler itself as the benchmark, 
disabling something means compiling less code and running less code. 
so it really needs to help to be worth turning on an optimisation. 
so disabling fold makes it a bit faster, but also compiling do_fold() 
takes a really long time so i think that means fold actually is a win. 
indeed if i disable it with `if true { return() }` instead of just `return()`
the frontend thinks it still needs to compile it and then it looks worth it. 

## (Mar 24-27)

- make it easy to run more tests (with run_tests.fr -- deps)
- fixed float const coerce + overload 
- fixed c sizeof on static struct with flex array
- starting a more interesting demo of import_c/ffi that uses wuffs. 
needed to be careful about cycles in types so everything's a placeholder that gets patched in a sketchy way. 
making export_data work is painful, need to think about this more.  
- speaking of which have i really never allowed cycles in constants in my frontend? oof, fixed that. 
- having a bit of a moment about weird unicode 
- convert worker/seal_debug_info/import_c/import_wasm to use a nicer data structure than Qbe.Dat. 
- re-enable memmove for large blits. 992260 -> 939304 is 5%, that's kinda big. TODO: x64 as well

- isel for wasm byteswap (which sucks)
- start wasm varargs, but did it backwards so it doesn't work yet
- backend/meta/test run with import_wasm

- starting a first-person-3d example program. 
10000 years of scrounging around in my old projects to try to remember how to make the matrices work. 
i think it's still not clipping so i can see behind me? or i just typoed a negative sign somewhere random, who knows!
- speed regression because of #align field tag which is super unfortunate :SlowAlignmentAnnotation
//       supporting #align makes it much slower (30ms ??) which is really unfortunate FIXME but i need it for now  
//       i dont understand how this can take so long when the branch is so predictable.   
//       ohhhh, it's still slow even if you `if false {` which the backend will remove.   
//       so it's probably just that #where is really really slow. thats bad! very very bad!   


## (Mar 19-23)

- experimenting with representing wasm blocks with marker instructions 
so it's easier to split out restructuring control flow from emitting the code. 
- for wasm, need to be careful about try_kill_inst, because you still need a pop even if you don't need the value so the stack works out. 
- import_wasm: implement conditional branch
- deduplicate wasm block types
- kinda waste of time tracking down why get_fn_callable doesn't get called as often as i expected but at least i understand the program better now. 
- in import_c/import_wasm i do this thing with a giant struct that has both the state 
that is kept for the whole compilation unit and the stuff that gets reset every function. 
because it's easy to just pass that as the first argument to everything and then always have all your globals. 
would be better to keep the two seperate so you don't forget to reset something, 
but then `long.foo` becomes `long.short.foo` which is annoying. 
could do the c thing where you have an anonymous struct field and inherit the names. 
that would be nice because import_c/ffi/export_type could use that and then using c libraries that did that would feel natural. 
- it's kinda unfortunate how much code it takes. 
- to make it feel more integrated, make it work with struct initialization as well 
- needing to match on a field of a struct when you want a tagged type with common fields
is something that always annoyed me about rust so i think it will be great success once i can actually use it in the compiler. 
- allow `#where` without a condition to not name the parameter type,
so you can have it almost look like a dynamic language and just specialize for every call.  
- FuckedJunkPointerExpr seems to have fixed itself sometime in the last 6 months, that's very good for business. 
i wonder if i was cured as soon as i stopped using llvm. 

## (Mar 16/17/18)

- make Tmp/Blk name field a slice instead of an array so it takes less memory when empty
- have track_ir_names on the Fn instead of the module so the threads don't stomp on each other
- remove a few compiler builtins
- don't need the hashtable for methods cache in overloadset since im scanning the array and looking at the Func's par type anyway. 
just need to not do that filtering when it's a type that might coerce (like a FuncId). 
- clean up the math intrinsic fucntion defs a bit. still super verbose since i don't want to enable_franca_ir_types() in every program.
- often a low number of parallel moves in rega, don't bother zeroing 64 every time. 
- can take out the copy of struct pars in abi because my emit_ir always generates a copy itself. 
- suspicious of register hinting in ralloctry. it seems that taking it out saves 10% code size. 
```
>>> 1088940 bytes of code, 66600 bytes of data.
>>>  972244 bytes of code, 66600 bytes of data
```
- occasionally getting to 0.945s now
- i can cheat and make 60fps.fr look a lot better by carefully not using `__string_escapes` (\n) because that's jitted every time

## (Mar 14/15)

- trying to reduce the number of redundant copies made when passing lambdas as `$@Fn`. 
there are some situations where you know the value is unique so the specialization doesn't need to be saved. 
(A = just use aliased_body, B = needed clone). 
```
before = (A = 3016,  B = 33065)
{|     = (A = 13588, B = 22493)
()=>   = (A = 16164, B = 19925)
```
i think it does't help as much as it looks like it should because the body is often GetParsed which is trivial to copy. 

require marking parameters that will be called multiple times as `#duplicated`. 
which is kinda dumb cause the compiler could figure that out for you, 
but i care about trying to reduce binary bloat so it's kinda nice to have a marker for it. 

- dont allocate a new string of `_i` for every tuple field (210000 times). 
same speed, just makes me feel better. 
- less raw_type() when getting TypeMeta (since they have same repr)
- don't track ir names unless logging
- sprinkle more #inline around.
- down to 1.03s for self compile. not bad, 4 days ago it was 1.33s (at commit "remove legacy example").  
lie: extrapolating this rate of improvement, i expect it to compile instantly in 2 weeks :)
- sad day i didn't catch this. 10 stars for Quentin:
 		if (s > sz * 4095u) {
-			i = &(Ins){Oaddr, Kl, TMP(IP0), {r}};
+			i = &(Ins){Oaddr, Kl, TMP(IP1), {r}};
That fixes import_c: wuffs/std/gzip

## (Mar 13)

- fix which end of the stack is passed to clone() on linux. which makes self compile work on blink. 

```
blink (upstream)   - 112.0
blink (my patch)   - 068.0
blink (my hack)    - 021.0
rosetta (orbstack) - 002.3
rosetta (macos)    - 002.2
native arm macos   - 001.2
```

ok the thing where im waiting for the jit codegen thread is stupid. 
there's waste on both sides as you wait to get back on a core or whatever. 
so what if i just keep toggling pthread_jit_write_protect_np like they seem to want you to. 
holy shit that's way faster, 1.2s -> 1.1s. how can mprotect be super slow but this be super fast? 
so like is it not doing a syscall? i can't see it in Instruments. 
like surely im calling it a billion times. but how can it be doing magic memory protection things if it's just 
in userspace? like could i write my own code that does whatever it does? 

i can find the pthread code on github `https://github.com/apple/darwin-libpthread/blob/main/src/pthread.c#L1292` 
but idk where `os_thread_self_restrict_rwx_to_rx` is declared. 
where's `<os/thread_self_restrict.h>`? looked in `https://github.com/apple/darwin-xnu`... empty file... `¯\_(ツ)_/¯`

i can just step into it in lldb, it never disappears like it does when you do the syscall instruction, that's fascinating.

- `dmb    ishst`, `isb` are just barriers
- `pacibsp`, `retab` are just pointer authentication 
- `mrs`, `msr` on `S3_6_C15_C1_5`, `S3_4_C15_C2_7` must be the magic things. 
that's read/write to a "system register" apparently? so they just have a bit for can you execute i guess? 
and those numbers go in `<op0> <op1> <Cn> <Cm> <op2>`?
- they're also doing stuff with fixed memory addresses which is really weird. 
  `0xfffffc10c`, `0xfffffc118`, `0xfffffc110`
  what happened to ASLR bro? how do you know where to look? 
- also this can't be code generated by llvm? how can it be so clunky? 

trying to set `S3_4_C15_C2_7` is an illegal instruction. 
maybe it is in fact magic and it checks if you're doing it from thier libc somehow? 
or i just failed at reading the assembly. 
im not super confident, like what is `0xc10c` even for? 
oh well S3_6_C15_C1_5 instead just works. 

<details><code>
libsystem_pthread.dylib`pthread_jit_write_protect_np:
    0x18cf390ac <+0>:   pacibsp 
    0x18cf390b0 <+4>:   stp    x29, x30, [sp, #-0x10]!
    0x18cf390b4 <+8>:   mov    x29, sp
    0x18cf390b8 <+12>:  mov    x8, #0xfffff0000 ; =68719411200 
    0x18cf390bc <+16>:  movk   x8, #0xc10c
    0x18cf390c0 <+20>:  ldrb   w9, [x8]
    0x18cf390c4 <+24>:  cbz    w9, 0x18cf39210 ; <+356>
    0x18cf390c8 <+28>:  ldrb   w9, [x8]
    0x18cf390cc <+32>:  cbz    w0, 0x18cf39120 ; <+116>
    0x18cf390d0 <+36>:  cbz    w9, 0x18cf392b0 ; <+516>
    0x18cf390d4 <+40>:  ldrb   w8, [x8]
    0x18cf390d8 <+44>:  dmb    ishst
    0x18cf390dc <+48>:  sub    w9, w8, #0x2
    0x18cf390e0 <+52>:  cmp    w9, #0x2
    0x18cf390e4 <+56>:  b.hs   0x18cf3916c    ; <+192>
    0x18cf390e8 <+60>:  movk   x0, #0xc118
    0x18cf390ec <+64>:  movk   x0, #0xffff, lsl #16
    0x18cf390f0 <+68>:  movk   x0, #0xf, lsl #32
    0x18cf390f4 <+72>:  movk   x0, #0x0, lsl #48
    0x18cf390f8 <+76>:  ldr    x0, [x0]
    0x18cf390fc <+80>:  msr    S3_6_C15_C1_5, x0
    0x18cf39100 <+84>:  isb    
    0x18cf39104 <+88>:  movk   x1, #0xc118
    0x18cf39108 <+92>:  movk   x1, #0xffff, lsl #16
    0x18cf3910c <+96>:  movk   x1, #0xf, lsl #32
    0x18cf39110 <+100>: movk   x1, #0x0, lsl #48
    0x18cf39114 <+104>: ldr    x8, [x1]
    0x18cf39118 <+108>: mrs    x9, S3_6_C15_C1_5
    0x18cf3911c <+112>: b      0x18cf391a8    ; <+252>
    0x18cf39120 <+116>: cbz    w9, 0x18cf392b0 ; <+516>
    0x18cf39124 <+120>: ldrb   w9, [x8]
    0x18cf39128 <+124>: sub    w10, w9, #0x2
    0x18cf3912c <+128>: cmp    w10, #0x2
    0x18cf39130 <+132>: b.hs   0x18cf391c0    ; <+276>
    0x18cf39134 <+136>: movk   x0, #0xc110
    0x18cf39138 <+140>: movk   x0, #0xffff, lsl #16
    0x18cf3913c <+144>: movk   x0, #0xf, lsl #32
    0x18cf39140 <+148>: movk   x0, #0x0, lsl #48
    0x18cf39144 <+152>: ldr    x0, [x0]
    0x18cf39148 <+156>: msr    S3_6_C15_C1_5, x0
    0x18cf3914c <+160>: isb    
    0x18cf39150 <+164>: movk   x1, #0xc110
    0x18cf39154 <+168>: movk   x1, #0xffff, lsl #16
    0x18cf39158 <+172>: movk   x1, #0xf, lsl #32
    0x18cf3915c <+176>: movk   x1, #0x0, lsl #48
    0x18cf39160 <+180>: ldr    x9, [x1]
    0x18cf39164 <+184>: mrs    x10, S3_6_C15_C1_5
    0x18cf39168 <+188>: b      0x18cf391fc    ; <+336>
    0x18cf3916c <+192>: cmp    w8, #0x1
    0x18cf39170 <+196>: b.ne   0x18cf392b0    ; <+516>
    0x18cf39174 <+200>: movk   x0, #0xc118
    0x18cf39178 <+204>: movk   x0, #0xffff, lsl #16
    0x18cf3917c <+208>: movk   x0, #0xf, lsl #32
    0x18cf39180 <+212>: movk   x0, #0x0, lsl #48
    0x18cf39184 <+216>: ldr    x0, [x0]
    0x18cf39188 <+220>: msr    S3_4_C15_C2_7, x0
    0x18cf3918c <+224>: isb    
    0x18cf39190 <+228>: movk   x1, #0xc118
    0x18cf39194 <+232>: movk   x1, #0xffff, lsl #16
    0x18cf39198 <+236>: movk   x1, #0xf, lsl #32
    0x18cf3919c <+240>: movk   x1, #0x0, lsl #48
    0x18cf391a0 <+244>: ldr    x8, [x1]
    0x18cf391a4 <+248>: mrs    x9, S3_4_C15_C2_7
    0x18cf391a8 <+252>: cmp    x8, x9
    0x18cf391ac <+256>: b.ne   0x18cf392b0    ; <+516>
    0x18cf391b0 <+260>: adrp   x8, 423103
    0x18cf391b4 <+264>: ldrb   w8, [x8]
    0x18cf391b8 <+268>: cbz    w8, 0x18cf39210 ; <+356>
    0x18cf391bc <+272>: bl     0x18cf3c434    ; pthread_jit_write_protect_np.cold.1
    0x18cf391c0 <+276>: cmp    w9, #0x1
    0x18cf391c4 <+280>: b.ne   0x18cf392b0    ; <+516>
    0x18cf391c8 <+284>: movk   x0, #0xc110
    0x18cf391cc <+288>: movk   x0, #0xffff, lsl #16
    0x18cf391d0 <+292>: movk   x0, #0xf, lsl #32
    0x18cf391d4 <+296>: movk   x0, #0x0, lsl #48
    0x18cf391d8 <+300>: ldr    x0, [x0]
    0x18cf391dc <+304>: msr    S3_4_C15_C2_7, x0
    0x18cf391e0 <+308>: isb    
    0x18cf391e4 <+312>: movk   x1, #0xc110
    0x18cf391e8 <+316>: movk   x1, #0xffff, lsl #16
    0x18cf391ec <+320>: movk   x1, #0xf, lsl #32
    0x18cf391f0 <+324>: movk   x1, #0x0, lsl #48
    0x18cf391f4 <+328>: ldr    x9, [x1]
    0x18cf391f8 <+332>: mrs    x10, S3_4_C15_C2_7
    0x18cf391fc <+336>: cmp    x9, x10
    0x18cf39200 <+340>: b.ne   0x18cf392b0    ; <+516>
    0x18cf39204 <+344>: adrp   x9, 423103
    0x18cf39208 <+348>: ldrb   w9, [x9]
    0x18cf3920c <+352>: cbnz   w9, 0x18cf39218 ; <+364>
    0x18cf39210 <+356>: ldp    x29, x30, [sp], #0x10
    0x18cf39214 <+360>: retab  
    0x18cf39218 <+364>: ldrb   w9, [x8]
    0x18cf3921c <+368>: cbz    w9, 0x18cf392b0 ; <+516>
    0x18cf39220 <+372>: ldrb   w8, [x8]
    0x18cf39224 <+376>: dmb    ishst
    0x18cf39228 <+380>: sub    w9, w8, #0x2
    0x18cf3922c <+384>: cmp    w9, #0x2
    0x18cf39230 <+388>: b.hs   0x18cf3926c    ; <+448>
    0x18cf39234 <+392>: movk   x0, #0xc118
    0x18cf39238 <+396>: movk   x0, #0xffff, lsl #16
    0x18cf3923c <+400>: movk   x0, #0xf, lsl #32
    0x18cf39240 <+404>: movk   x0, #0x0, lsl #48
    0x18cf39244 <+408>: ldr    x0, [x0]
    0x18cf39248 <+412>: msr    S3_6_C15_C1_5, x0
    0x18cf3924c <+416>: isb    
    0x18cf39250 <+420>: movk   x1, #0xc118
    0x18cf39254 <+424>: movk   x1, #0xffff, lsl #16
    0x18cf39258 <+428>: movk   x1, #0xf, lsl #32
    0x18cf3925c <+432>: movk   x1, #0x0, lsl #48
    0x18cf39260 <+436>: ldr    x8, [x1]
    0x18cf39264 <+440>: mrs    x9, S3_6_C15_C1_5
    0x18cf39268 <+444>: b      0x18cf392a8    ; <+508>
    0x18cf3926c <+448>: cmp    w8, #0x1
    0x18cf39270 <+452>: b.ne   0x18cf392b0    ; <+516>
    0x18cf39274 <+456>: movk   x0, #0xc118
    0x18cf39278 <+460>: movk   x0, #0xffff, lsl #16
    0x18cf3927c <+464>: movk   x0, #0xf, lsl #32
    0x18cf39280 <+468>: movk   x0, #0x0, lsl #48
    0x18cf39284 <+472>: ldr    x0, [x0]
    0x18cf39288 <+476>: msr    S3_4_C15_C2_7, x0
    0x18cf3928c <+480>: isb    
    0x18cf39290 <+484>: movk   x1, #0xc118
    0x18cf39294 <+488>: movk   x1, #0xffff, lsl #16
    0x18cf39298 <+492>: movk   x1, #0xf, lsl #32
    0x18cf3929c <+496>: movk   x1, #0x0, lsl #48
    0x18cf392a0 <+500>: ldr    x8, [x1]
    0x18cf392a4 <+504>: mrs    x9, S3_4_C15_C2_7
    0x18cf392a8 <+508>: cmp    x8, x9
    0x18cf392ac <+512>: b.eq   0x18cf391bc    ; <+272>
    0x18cf392b0 <+516>: brk    #0x1
libsystem_pthread.dylib`pthread_jit_write_protect_np.cold.1:
    0x18cf3c434 <+0>:  stp    x20, x21, [sp, #-0x10]!
    0x18cf3c438 <+4>:  adrp   x20, 0
    0x18cf3c43c <+8>:  add    x20, x20, #0xf9d ; "BUG IN CLIENT OF LIBPTHREAD: pthread_jit_write_protect_np() disallowed by allowlist"
    0x18cf3c440 <+12>: adrp   x21, 423092
    0x18cf3c444 <+16>: add    x21, x21, #0x8 ; gCRAnnotations
    0x18cf3c448 <+20>: str    x20, [x21, #0x8]
    0x18cf3c44c <+24>: ldp    x20, x21, [sp], #0x10
    0x18cf3c450 <+28>: brk    #0x1
}
</code></details>

<details><code>
pthread_jit_write_protect_np(x0: bool) {
  x8 = magic(0xc10c)
  x9: u8 = x8[]
  if x9 == 0 => :356
  x9 = x8[]
  if x0 == 0 => :116
  if x9 == 0 => :516
  x8 = x8[]
  dmb(ishst)
  x9 = x8 - 2
  if x9 >= 2 => :192
  x0 = magic(0xc118)
  x0 = x0[]
  S3_6_C15_C1_5 = x0
  isb()
  x1 = magic(0xc118)
  x8 = x1[]
  x9 = S3_6_C15_C1_5
  if true => :252
116: 
  if x9 == 0 => :516
  x9: u8 = x8[]
  x10 = x9 - 2
  if x10 >= 2 => :276
  x0 := magic(0xc110)
  x0 = x0[]
  S3_6_C15_C1_5 = x0
  isb()
  x1 = magic(0xc110)
  x9 = x1[]
  x10 = S3_6_C15_C1_5
  if true => :336
192:
  if w8 != 1 => :516
  x0 = magic(0xc118)
  x0 = x0[]
  S3_4_C15_C2_7 = x0
  isb()
  x1 = magic(0xc118)
  x8 = x1[]
  x9 = S3_4_C15_C2_7
252:
  if x8 != x9 => :516
  x8 = adrp(423103)
  x8: u8 = x8[]
  if w8 == 0 => :356
272:
  call pthread_jit_write_protect_np.cold.1()
276:
  if x9 != 0x1 => :516
  x0 = magic(0xc110)
  x0 = x0[]
  S3_4_C15_C2_7 = x0
  isb()
  x1 = magic(0xc110)
  x9 = x1[]
  x10 = S3_4_C15_C2_7
336:
  if x9 != x10 => :516
  x9 = adrp(423103)
  x9: u8 = x9[]
  if x9 != 0 => :364
356:
  return
364:
  x9: u8 = x8[]
  if x9 == 0 => :516
  x8: u8 = x8[]
  dmb(ishst)
  x9 = x8 - 2
  if x9 >= 2 => :448
  x0 = magic(0xc118)
  x0 = x0[]
  S3_6_C15_C1_5 = x0
  isb()
  x1 = magic(0xc118)
  x8 = x1[]
  x9 = S3_6_C15_C1_5
  if true => :508
448:
  if w8 != 1 => :516
  x0 = magic(0xc118)
  x0 = x0[]
  S3_4_C15_C2_7 = x0
  isb()
  x1 = magic(0xc118)
  x8 = x1[]
  x9 = S3_4_C15_C2_7
508:
  if x8 == x9 => :272
516:  
  fault(1)
}
pthread_jit_write_protect_np.cold.1() {
    x20 = "BUG IN CLIENT OF LIBPTHREAD: pthread_jit_write_protect_np() disallowed by allowlist"
    x21 = gCRAnnotations
    fault(1)
}
</code></details>

<details><code>
pthread_jit_write_protect_np(want_exec: bool) {
  A = magic(0xc10c)[];
  B = magic(0xc118)[];
  C = magic(0xc110)[];
  if(A & 0xFF == 0) return
  if want_exec {
    dmb(ishst)
    if A - 2 >= 2 {
      S3_4_C15_C2_7 = B
      isb()
    } else {
      S3_6_C15_C1_5 = B
    }
  } else {
    if A - 2 >= 2 {
      S3_4_C15_C2_7 = C
    } else {
      S3_6_C15_C1_5 = C
    }
    isb()
  }
  return
}
</code></details>

- manually polymorphise hashmap:resize
- all tests down to 16.3
- finally use the version of open() that creates the file instead of exec-ing touch and chmod all over the place

## (Mar 12)

- fix import_c losing the variable when header and implementation use different parameter names. 
- get rid of old `Intrinsic :: @enum(`
- speed up inline_for a bit by not resolving index() every iteration
- idk why i thought invoke_specialized() needed to be a 30 line thing in the compiler instead of just 
`const_eval(Ret)(@{ @[@literal f](@[@literal arg]) })`, you need to be in comptime to call builtins anyway 
so needing to call unquote_placeholders isn't anymore restricted. 
- i feel like 100k of data is way too much. 
  - track source location with BakedValue so i can see what's going on. 
  >>> 1151760 bytes of code, 105256 bytes of data.
  - in emit_ir i used to be always doing a blit from memory if there were no relocations in the value.
  which was dumb for `?T = .None`. 
  also don't reference_constant if we're emitting it in pieces. 
  and bump up the size where you fallback to blit: 40->64 bytes. 
  >>> 1133764 bytes of code, 86536 bytes of data.
  - pack enum names into one big string and compute them with an array of u16 (off, len) pairs. 
  so you don't need to create the array of strings. instead of (16 bytes + 1 relocation) 
  per name, this is (4 bytes + 0 relocations) per name. 
  >>> 1136212 bytes of code, 77696 bytes of data.
  - .sym().str() to put it in the string pool in `__string_escapes` and `format_into`. 
  >>> 1136212 bytes of code, 72704 bytes of data.
  - some actual code changes: shrink table in argcls, wasm/bits. remove some direct calls to get_enum_names. 
  >>> 1136284 bytes of code, 67728 bytes of data
  - franca-linux-amd64 is down to 392 data relocations, last commit it was 4936. great success. 
  running the tests is down to 18.0s from 18.5s last commit. 

- there's some bug that breaks a test if i try to intern the string in pack_strings()

- speed up resolve_in_overload_set_new: program/samples/own 1258/506/269 -> 1099/382/189. 
which sounds like a lot but only gets it to like 1.2s

## (Mar 11)

- improving guess type_of to work on fields and it broke 
`xxx: i32 = f.nblk.bitcast() - 1;` which actually shouldn't compile because `nblk: i32,`
so that's nice. 
- start import_c actually talking to the franca compiler
- de-linked-list-ify parameter tracking in import_c
- get rid of old_backend. i don't test it, it doesn't support new language features, i'm not excited to fix it, 
its a lot of almost duplicated code, does not spark joy. 
but i should probably add a new alternate backend because that's a cool comptime demo. 

## (Mar 9/10) 

- be better about detecting unterminated blocks and comments
- don't require semicolons after blocks (treat `}\n` the same as `};\n`)
- farm_game aot doesn't work with how im doing non-8-aligned baked constants. 

but instead of adding more hack on top, i'd rather fix: the way im representing baked values is kinda dumb. 
I split them into 8 bytes at a time tagged with whether it's a pointer or not and what it points to. 
so it's painful to deal with fields that are less than 8 bytes, 
and it uses 3x as much memory to represent things that have a pointer but are mostly raw bytes. 
and then i translate that to a different data structure (that has the same problems) to send to the backend 
to handle on another thread so it doesn't fight with codegen when it needs to do fixups. 
I think it would be more sane to store the bytes and pointer relocations separately. 

- made an attempt. fixed farm_game but broke self compile which is funny. 
hmm, i do in fact try to use the comptime value after it's emitted so i can't just set pointers to zero. 
- fixed panicking as lass expression in a function that returns a scalar (it used to confuse ssacheck). 

## (Mar 7)

ok i think the take away is once again that it can be multiple problems. 
when i tried inserting an insane amount of fences that was before i moved the illegal instruction 
catching into the compiler to run even when not FRANCA_BACKTRACE=true. 
and maybe the thing where the terminal prints "illegal instruction" gets lost in the many levels of execing things idk. 
(dito for `DEBUG_MY_MUTEX_BY_USING_PTHREAD`). 
but with the signal handler, the problem is consistantly impossible debug assertions in inline.fr that look like memory 
corruption. and using way more cas (on count/thread) in Mutex makes that pretty much go away. 
and putting fence() in Mutex seems to fix both that and the call to a null jit_addr in `__got`. 
(which makes sense, broken Mutex on ArenaAlloc and then your memory gets lots of garbage, and 
inline.fr just happened to be a place that used QbeModule.forever and did some debug checks). 
now i can do 100 runs on arm without crashing, so i think it's officially fixed? 
i was just making the classic blunder of only changing one thing at a time 
and if that didn't work, declaring that couldn't be the problem. 

## (Mar 6)

- stop leaking file descriptors of pipes in subprocess.fr,
apparently super old macos defaults to a lower limit. 
- ok i have an old real macos-amd64, takes twice as long (but faster than rosetta). 
runs all the tests 100 times without crashing. 
so either my problem is arm specific or it's a race condition that goes away when you're super slow. 
- ok what if i DMB at the start of every function? no not enough. 
every block? sure makes it slower. stills dies tho. 
still dies if i make codegen_queue_size=1 which should mean the threads are basically serial. 
those two together make it the same speed as the old computer. 
and i still crash. DMB after every instruction?? (takes 4x as long).
still crash! clear_instruction_cache the whole Code segment after every finish_function? still crash!
the hack sleep in `report_called_uncompiled_or_just_fix_the_problem` that i only do on x64? still crash! 
so that's just a normal bug then right? 
if we decide it's not a threading problem, what else changes between runs? 
try clearing flags and stomp regs after every call? still random. 


- is there any chance it's not a problem with the jit? 
try improving ir dump/parse so i can save the text version of one compile and 
rerun the backend alone a bunch of times. 
  - parse: con+off, argc/e cls, FPad in type. dump: retty, float con as bits because @fmt doesn't do fractions yet.
  - need to send more to driver: wrap_with_runtime_init, asm functions too, __franca_debug_info
  - if i hack those into the output, i get something that looks like a macho file.
  it crashes if i try to run it so something about my dump isn't right, 
  but i can run the qbe_frontend like that 100 times without crashing and producing the same file,
  so looks like the problem im looking for isn't in aot backend? 


## (Mar 5)

- clone3 syscall so don't need pthread_create on linux. so many flags. 
just getting -22 which is sad cause the man page has a very very long list of things that give EINVAL,
but was just a dumb mistake of not setting stack_size in the args struct. 
- don't have Elf Interp header when no imports so blink can run my programs. 
- blink doesn't give high garbage value for argc so have to actually check os in franca_runtime_init. 
- `__libc_start_main` does actually do something for me. the compiler crashes without it (both blink and real, with and without Interp). 
I assume it's doing relocations? makes sense: simple programs work already, i'd believe that sudoku doesn't have any pointers in static memory. 
that's such a pain, i need to get a pointer to the elf headers off the stack and then parse them to find all the 
places to add my base address to. i guess this is what i wanted, i'd rather it be in my program than 
somewhere random that i can't see and every computer has a different version of. 
- emit_ir/bounce_body wasn't passing the dynamic context pointer.
i guess it mostly just worked out with nothing stomping the register most of the time??
ok cool that works. compiler can run without libc on real linux. 
- still doesn't work on blink because they don't do clone3(). 
but using clone() instead is easy. 
now hello world works but takes 1.6 seconds. yikes. 
i bet the -m thing makes it much worse. 
so i need to figure out what this means:
```
p_vaddr p_offset skew unequal w.r.t. page size; 
try either (1) rebuilding your program using the linker flags: 
-static -Wl,-z,common-page-size=65536,-z,max-page-size=65536 
or (2) using `blink -m` to disable the linear memory optimization
```

just need to align my segment sizes to 64k. 
now without -m i can jit hello world in 1.2 seconds. 
not quite the 4x i was promised but still nice. 
maybe it really really hates self modifying code? 

yeah looking in the profiler: you want all the time with g_code at the top of the stack, 
that's the buffer they jit code into. on the backend thread that's just a normal program it's 70% g_code. 
on the frontend thread that does comptime execution, it's 46% OnFatalSystemSignal. 
ConvertToHostAddress is super slow and they do it twice on the same address: FixXnuSignal and then IsSelfModifyingCodeSegfault.

>  5 files changed, 20 insertions(+), 14 deletions(-)
and now it's 18% only in OnFatalSystemSignal, and my program takes 1.0s.
now the self compile gets to the crash in 24s instead of 41s, oh boy. 

- what if i just catch the illegal instruction signal and ignore it when jitting for arm and then i don't have to care about the caches so much? 
- memory safety police: added bounds checking on QList.cap. founds stupid mistake in add_padding. 
- seems like a good idea to let the allocator interface return how much memory it actually gave you, 
so for block based ones, expandable collections can use the extra capacity. 
doesn't help me because i almost always use arenas. 

## (Mar 4)

- don't depend on pthread_cond_t. 
just count how many slots are free and then you know you only need 
to sleep/signal when the count gets to one end of the range. 
clean up the stuff with scanning through the queue, just strictly only take one entry at a time. 
- start using #where for a few functions that are only called with simple argument expressions. 
- fix get_constant safety check when the type had no constant fields at all 

## (Mar 3)

- exit_group syscall instead of exit on linux so you take your threads with you. 
- lots of splitting things out into imports, seems less invasive to your namespace. 
- make most of the c compiler tests work on amd64_apple
  - implement jump.fr (try/throw)
  - fix the cls used with O.load in sysv return (great! this was a real bug!)
- don't ask the os in query_current_arch. 
- implement some other asm test for x64: multistack, truncate_to_byte, 

## (Mar 2)

- need to replace pthread_mutex. the macos version of futex seems to be ulock. 
- update the bootstrap binary so i can fix a bunch of annoying stuff. 
  - always supports_self_sign, don't need to exec codesign anymore.
  - offset_of works so i can static assert that fields are arranged correctly for unsafe code.  
  - can start using import() in the compiler. 
  - don't need the fake cas implementation 
  - can have types larger than 64k
  - don't need to feature detect the abitilty to detect features
  - string literal coerces to CStr

//       i don't know what the socially acceptable way to make things thread safe is. 
//       are you supposed to just slap a mutex around it instead of trying to do a convoluted 
//       thing with atomics every time? 
//       It feels like haha i treat the list for each size class seperatly so 
//       you get less contention if the threads happen to want different sizes. 
//       but the whole array is gonna be on the same cache line so you get false
//       sharing and it's the same as if you had a mutex around it anyway?  
//       i wonder if i could make a test that got faster by adding 128 bytes of padding 
//       between them, that would be pretty cool. 
//       but also if there's lots of contention then i've just created a spin lock again which isn't what you want. 

## (Mar 1)

- reducing dependency on libc because i want to get the compiler running in blink again 
(but without statically linking someone else's libc this time). 
  - memmove/memset are easy but the obvious thing (one byte at a time) is super slow (1.23s -> 1.5s).
  doing 8 bytes at a time and then the rest makes it 1.25s which is fine. 
  - using getcwd and get_executable_path (the latter won't work on blink)
  to find the lib folder. but can just use "./" for cwd and that's enough to find it, 
  it's just a bit more fragile to force you to do everything in this folder. 
  - sigaction for back traces is easy to disable
  - should stop using libc_allocator all over the place. 
  trivial to just replace it with page_allocator when building without libc 
  but that's way too slow to even type the code for (can't be getting a new page for every allocation). 
  (only like 1.3s for the compiler cause it mostly uses arenas but the point remains). 
  
## (Feb 27)

- cross compile matching hash has been broken for a while (ie. compile on arm and amd both targetting arm should give identical bits but doesn't).
but objdump gives identical disassembly for the two binaries and examples/dump_macho.fr is also the same. 
diff of hexdump:
```
diff a.s b.s
100270c100270
< 01968a0 0000 0000 0000 0000 0001 0000 0000 0000
---
> 01968a0 da00 0000 0000 0000 0001 0000 0000 0000
101333c101333
< 019b0a0 0000 0000 0000 0000 0001 0000 0000 0000
---
> 019b0a0 0000 0000 c32d 0001 0001 0000 0000 0000
101336,101337c101336,101337
< 019b0d0 0000 0000 0000 0000 0001 0000 0000 0000
< 019b0e0 ff50 ffff ffff ffff 0000 0000 0000 0000
---
<... snip 18 lines>
```
that sure is not a lot of different bytes out of 1.7MB.
at least it's consistant and recompiling on amd and then targetting arm again gives the same results. 

ok so first one: 01968a0 = 1665184.  
ok good, so that's in `__DATA` (start file offset is 1638400) which makes sense. 
so which is it? 
```
off := 1665184 - 1638400;
xx_start := ptr_diff(m.segments&[.MutableData].mmapped.ptr, m.segments&[.MutableData].next);
emit(self, dat);
xx_end := ptr_diff(m.segments&[.MutableData].mmapped.ptr, m.segments&[.MutableData].next);
if xx_start <= off && xx_end >= off && m.goal.type != .JitOnly {
    @println("% % %", m.str(dat.lnk.id), xx_start, xx_end);
};
// g1057 26768 26792
```
so problem is at byte 16 of a 24 byte thing?
should really track names/source locations for data but i can just print the function name when i see a loadcon of it. 
it's referenced by apple_selvaarg__1850 which is just a @emit_instruction. 
it's the second arg to a newcon. 
ok so there's only one constant in that function. what's at byte 16 of the Con(8)?
explicit default value of 0 for the padding fixes maybe that one diff but there's still a bunch. 
but manually expanding `c := @[@literal c[]]` to `c: Qbe.Con = (type = @[@literal c.type], sym = @[@literal c.sym], bits = @[@literal c.bits]);`
fixes all of them and makes repro work. 
i don't understand why zero_padding() doesn't fix that. 
and i don't understand why explicit default value doesn't fix it, 
because getcon() has `f.con[c] = (type = .CBits, bits = (i = val));`
which should be setting all those defaults. 

##

- bit fields

## (Feb 24)

- do assert() lowering to blocks earlier
- maybe i want to change my mind about llvm. seems sad that everything could be twice as fast when not working on the compiler. 
  - restore an old commit
  - insert more casts
  - rotate/bswap/`c*z`/blit are intrinsics you have to call
  - fix string constants to do escapes
  - try just putting a little piece of inline asm before every call that moves the env parameter to the right register. 
  there's no way you're allowed to do this but it kinda seems to maybe work so far? 
  i think it only works on -O0 tho which makes this whole operation a complete waste of time. 
- CodegenEntry.Task.Bounce is super confusing and doesn't work on non-native targets that want type info (wasm + llvm),
so let's try replacing it with explicit (par...) -> (arg...). 
waste of time when all i really want is a symbol alias but maybe it won't be noticable. 

## (Feb 23)

- hacky fix for `#libc` to work with multiple lib files.  
- ssh-keygen && ssh-copy-id luke@cache
- internet says
```
brew install fswatch
alias run_rsync='rsync -azP --exclude ".*/" --exclude ".*" --exclude "*.out" --exclude "target/" --exclude "editors/" "./" luke@cache:./franca'
run_rsync; fswatch -o . | while read f; do run_rsync; done
```
eventually should write my own with CoreServices/fsevents but also i don't care that much yet. 
- internet says arm SVC wants syscall number in r8 but breakpoint on write() reveals it's r16 on macos. 

## (Feb 21/22)

- catch signals (segfault etc) and show backtrace. 
very painful transcribing of the u_context struct of all the registers. 
- include source and instruction location info in AOT so you can have traces that go through the compiler. 
also extract crash_report into a library so you can print backtraces on crash when not running in comptime. 
- glibc wants the stack to be 16 byte aligned but when the loader calls into your elf's entry point, 
it doesn't care you you have to fix the stack yourself. 
i remember the thing about keeping it 16 byte aligned but i guess i only did jit before 
so i always had someone else's linker to do whatever arbitrary start up stuff you need. 
- no longer looks like im crashing on an unaligned mov so that's progress. 
try calling `__libc_start_main` maybe it's a crime to jsut call into mutexes, etc without  that setup? 
didn't seem to help. 
- in amd64/emit/call. symbol was null because id was out of bounds because RCon was CBits because it was
a jitted address that fits in 32 bits because linux doesn't reserve that address range like macos does. 
- in amd64/emit/addr, if it's already DynamicPatched, use GOT to avoid an assertion in push_instruction(). 
TODO: I don't understand why that only happens on linux tho. different things found in dylib somehow? 
- TODO: whatever im dlopen-ing doesn't have sqrt (for generating sha256 tables)
but other than that it self compiles on linux now!

## (Feb 19/20)

- floats are different on x64

	R1 =l copy R17
	R18 =d cast R1
	became
	movsd	xmm0, xmm0                      ## encoding: [0xf2,0x0f,0x11,0xc0]
	movq	xmm1, rax                       ## encoding: [0x66,0x48,0x0f,0x6e,0xc8]
	
	`R1 =l copy R17` is illegal but happened because elide_abi_stack_slots thought they were the same class. 
	because sysv.fr generated 
	```
	%abi.164 =d copy R18
	%abi.165 =d copy R17
	storel %abi.165, [S0]
	storel %abi.164, [S8]
	```
	which uses storel even tho it's a float. 
	which is fine for qbe because the assembler deals with doing the right thing for "mov",
	and i was like sure whatever, i'll just do the right thing for storel even tho that seems imprecise. 
	but then adding the slots optimisation later was bad new bears because suddenly we care about knowing
	the classes of everything and i didn't think to add in that hack to make .copy work as cast. 

- on x64 i randomly get stuck infinitely recursing on garbage instructions. 
which sounds a lot like you just need to call `__clear_cache` but apparently you shouldn't have to on x64 and 
it compiles to nothing: `https://github.com/llvm/llvm-project/blob/main/compiler-rt/lib/builtins/clear_cache.c#L63`
the problem goes away when running with `use_threads :: false` 
- sprinkle some mutex around 
- add `assert` instruction so you can easily crash without manually making a bunch of blocks in the frontend. 
- simplify isel/emit of cas. make it work when you need to fixarg(). 
- new flavour of debug mode where you make every 4/8 byte store into a cas that crashes if it fails. 
currently works on x64 but not arm which is funny. 
but doesn't help my problem which is very strange. 
- fix dynalloc.ssa test to not get constant folded to nothing. 
remove some asserts in emit(). 
- start_thread wasn't using the stack i allocated because i passed something that wasn't page aligned 
to pthread_attr_setstack
- ok wait only the top stack frame is garbage and the repeated one is different. 

```
frame #0: 0x000000013ac66fff
frame #1: 0x0000000138c85399
[... same a billion times]
frame #208: 0x0000000138c85399 ; via create_jit_shim
frame #209: 0x0000000138c853bb 
frame #210: 0x0000000138c853d5 ; via create_dyncall_shim
frame #211: 0x000000010016dc24 x.out`call_dynamic_values__1507 + 897
```
so it's looping calling itself because it thinks its not compiled or soemthing idk man. 
- fixed exporting symbols so you can make dylib without -legacy
- stack walking instructions and use them to show a backtrace when you panic at comptime. 

## (Feb 17)

- most of the problems were that i didn't set b.jmp for void `return;`
- gif `TODO: args need reg. op=and` was because of a dynamic size alloca (which i never do in franca),
test_wuffs_gif_decode_frame_out_of_bounds uses a VLA and my little vla tests actually can get constant folded to a static slot size. 
i was only calling fixarg on 1 instruction. i think qbe didn't have that problem because you actually 
can encode AND with a constant, i just don't do it cause it's annoying. 
- to make gifplayer work i had to bump up the max ArenaAlloc size, that's a pretty bad sign. 
do they some giant array of constants? oh no, it's just that to handle this:
`uint8_t g_src_buffer_array[SRC_BUFFER_ARRAY_SIZE] = {0};`
i allocate a new Initializer for every byte that's fucking stupid man. 
the whole time is in write_gvar_data and new_initializer. 
before:         1.8 s
all_zeros hack: 1.2 s
skip =0 early:  0.13s
similarly for test/gif, `wuffs_gif__decoder dec = ((wuffs_gif__decoder){});` takes a billon years, 
but this time in add_type_inner. setting all_zeros for `{}` in struct_initializer1 saves 2 seconds. 
and that's still with generating the most unserious 60000 instruction function for that zero init in test_basic_initialize_not_called.

trying to compile the lua interpreter. 

- TODO: my line numbers are off!
- hide_stack can miss a token if it only expands to 1
- gen_addr of ND_CAST for prefix --
- cast enum to long was confused because i was saying TY_ENUM was U64 but Kw, 
- iter_auxlax symbol not found but it is getting added to refs and emitted. 
ah! it's a real backend bug! suspend for inlining checks instructions but not phis so it missed `lax ? iter_auxlax : iter_auxstrict`

## (Feb 16)

- instead of tracking hidesets on each token, just keep a stack of active macro expansions. 
- shrink Token from 104 bytes to 56. 
- doing add_line_numbers as a seperate pass is 4% of the time. just track line number incrementally instead. 
- same for remove_backslash_newline
- started toady at 810 samples, now at 666
- fantastic, caught another real backend problem. 
"can't encode offset" was because i was calling fixarg on i.arg&.index(0) instead of a0& (dito for store). 
that fixes lzw but not bzip2. but must also be backend problem because it works on amd. 
- BSS for mach-o relocatable so i can get symbol names
- data pointer of a buffer is mysteriously 1 and adding printfs in some places changes it... thats pretty bad. 
wasted a lot of time in lldb before just looking at the code. 
```
self.inst(movz(Bits.X64, reg, s.trunc(), .Left0));
self.inst(movk(Bits.X64, reg, s.shift_right_logical(16).trunc(), .Left0));
```
for stack addr that didn't fit in 16 bits, i wasn't shifting for movk. 
now src.data.ptr behaves consistantly. wuffs_bzip2__decoder__struct is way bigger than i ever make things. 
need to remember i need to fix TypeMeta size field before i can translate that to franca. 
still doesn't work, now it dies in read_file. 
it's crashing trying to setup a normal stack frame. and sp is something that looks too big, but nothing in the callstack has a big frame. 
it's when wuffs_bzip2_decode returns it's not reseting the stack properly. 
hmmm... lets play can you guess what variable names i swapped. 
```
top: u16 = (o - 16).bit_and(0xFFFF).trunc();
bottom: u16 = (o - 16).shift_right_logical(16).trunc();
```
bad sign that it took me that long steping through even tho i just typed the sentence about looking at the code. 
- getalias of RNull was a frontend problem, saying void functions returned a value, which got confused when it inlined and found out you lied. 
i guess that should probably work because it would work if i didn't do inlining, 
but it feels weird to know there's a problem and not report it. so i really want to get into warnings for frontends? 
i fixed the reverse before (void call to int function). 

## (Feb 15)

- did bss, now adler32.c compiles in 100ms instead of 400ms. 
now i can take out my hack of caching sha256 of a page of zeros.
- bit of a scare bit it was just QbeModule::drop using the SegmentType length
- the importing GOT data when static linking thing i did yesterday works for me but crashes the linker on github actions. 
`ld: Assertion failed: (pattern[0].addrMode == addr_other), function addFixupFromRelocations, file Relocations.cpp, line 700.`
ughghgh how do i even debug that. the line it references sure doesn't exist if i go look at llvm-project on github. 
so like it's apple's personal clang or some ancient version? 
even if i knew what version it wasm, would i have to compile my own with debug assertions turned on, 
that seems impractical. am i really going to just amend the commit a hundred times as i try things? 
joyous day. that would have taken be a billion years. 
  - https://github.com/bytecodealliance/wasmtime/issues/8730
  - https://github.com/gimli-rs/object/pull/702
you just have to swap the order of the relocations for adrp and ldr. 
why didn't i think of that... fucking garbage program man. 
praise cranelift. 
- elf bss and hackily convert LEA to LDR from GOT for x64 data imports. 
some ifdefs in include.fr. now works on x64-macos but segsevs on linux 
`libc.so.6_IO_printf: ->  0x7ffff7e38cbb <+43>: movaps xmmword ptr [rsp + 0x50], xmm0`
feels familiar. because the stack isn't 16 byte aligned? `rsp = 0x00007fffffffe408`
yeah, fixing that makes it get to "hello world" but now it's crashing in my own code. 
needed to offset by 8 in the same way for vararg save area. 
still think it's wrong tho. hard to make sure everything cancels out. 
- there's also the problem of getting argc/argv which i fixed in init_franca_runtime
- for now just always init small slots
- clean up parsing Prec.Stmt hack
- start factoring out the test runner part

## (Feb 14)

trying to get wuffs to compile with my c compiler. 

- need to type out a bunch of libc signetures 
- 6590 wuffs_base__utf_8__next__output__is_valid
  phi for && was using the start of else instead of the end of else
- 18191 wuffs_private_impl__io_writer__limited_copy_u32_from_history
  doesn't like `for (; n; n--) { *p++ = *q++; } *ptr_iop_w = p;`
  tries to treat the for loop as the left of a multiply. 
  parsing's actually the hardest part did you know that...
- 20777 wuffs_private_impl__high_prec_dec__assign
  :ParamTypeList
- need to do BSS for giant static variables. but can hack around it for now by jsut making segments way bigger. TODO!
- now the frontend gets through everything and we just have to make it actually compile 
- little break from making it work. just put it on -unsafe and make it less slow first. 
  - hmmm did the start at main and only compile what you need thing (with WUFFS_CONFIG__STATIC_FUNCTIONS) and it's barely faster. 
  - all the time is in wuffs_testlib__initialize_global_xxx_slices? 
  ahahaha no all the time is after the end when signing the exe.
  cause i didn't bother doing BSS so im calculating sha256 of a page of zeros a billion times. 
  lamo, caching the hash of a page of zeros and checking if each page is all zeros first 
  makes it 1.2s instead of 4.1s. 
  and now 35% of the time is my stupid is_all_zeros instead of 88% of the time being sha256_update. 
  making is_all_zeros do 8 bytes at a time makes it 9%. 
  so yeah really need to do the bss thing. 
  - can't have read_punct taking 4% of the time. inline_for helps a bit but not enough. 
  write out the giant switch. now it's 0.4%, much better. TODO: comptime magic to generate it. 
  - can't have read_ident taking 3% of the time. #inline is_ident1 and is_ident2.
  now it's 0.6%, much better. 
  - can't have update_kind taking 3.5% of the time. make read_punct return the kind instead of doing a hash lookup later.
  now it's 1.5%, still slow. 
  - emit_data_to_segment taking 43% to write zeros one byte at a time. 
  boring because BSS will fix it but calling memset instead makes it 12%.
- // has a habit of reading from uninitialized memory 
  // but not using the result which confuses ssa_check later. 
  // it feels weird to disallow reading uninitialized memory because if we'd done no 
  // optimisations, the program would run, you'd just get some arbitrary number. 
  // there is also the MADV_DONTNEED problem (you can read twice and get different values). 
- i think it's really funny that the first time the result exe tries to run it takes like 2 seconds 
before main() and then the next time it's instant. almost like that's how long it takes the loader 
to hash 600MB of zeros to check my signeture and then it remembers. 
- to debug why the generated exe is crashing, want to support .Relocatable so i can get symbol names in lldb. 
so need to allow importing non-function variables when not targetting .Exe. 
you can't just use adrp/add apparently, need to load from GOT. 
- now a little hack to wuffs/testlib/parse_args and i can run adler32.c
TODO: fix for real

## (Feb 13)

- get argc,argv from the stack
- elf local relocations
- almost all the lang tests pass on linux. there's just a few libc-ish ones that don't work. 
  - jit (still tries to call __clear_cache somehow)
    - + some i only did on arm and it gets confused by cross compiling. 
  - signal (never did the magic numbers?), 
  - open libc dylib
    stopped hardcoding the macos path but that's not enough. 
  - fork
  - threads
  - walk_dir (like macos and need a different insane name for the function?)
- easy instructions: ctz, clz, popcnt
- allow main() to be a normal constant instead of an overload set so you can import() and call non-main functions 
  without it conflicting with your own main(). 
  also gets rid of a stupid linear scan over every function in find_unique_func which is nice. 
  - it seems immediate_eval_constexpr of `overloadset -> Fn(A)R` doesn't actually call coerce_const_expr because
    of my hack that needs to be cleaned up. 

## (Feb 12)

- new instructions: rotr, rotl, byteswap
- compare and swap
  - arm: there's like memory ordering stuff and im hoping i can just ignore it and always do the same one,
   and it will just be a bit slower than it could be but prevent threads from stomping on eachother which is the important thing. 
  - arm: a bit painful cause it's like the only arm instruction that has so many arguments that one is both read and written.
   so a bit of gymnastics with a fake copy to make sure it doesn't allocate the out parameter to the same register as the wrong input. 
   which is sad because i always need an extra real copy because i don't have a good way to tell rega that the output needs
   to be the same register as a specific input without choosing a specific register like you do for amd shifts. 
  - my llvm-mc dis doesn't think cas is a valid instruction. it sure runs tho so idx what's going on. 
  - need to treat cas as a store so loadopt knows it can change in the loop
  - there's still some problem where it doesn't work if you try to use the return value instead of reloading it. 
   i think i'll do it for amd so i can see if it's a logic bug or i'm just using the wrong instructions somehow. 
  - amd: it's another one with a fixed register so old_in and res are always RAX. 
  again end up with extra copies because i don't know how to express the actual constraints. 
  - amd: need to encode as RMem or it's an illegal instruction
  - ok on amd it works with reusing the result, so the mistake is arm specific. 
  ah, i was emitting at the position of cas0 instead of cas1 so when rega inserted a copy between them 
  to preserve the register that gets stomped, it would happen after the stomp instead of before it, 
  and it would always think the cas succeeded. 
  - confusion with trying to generate a shim function for each instruction (like just cas0 doesn't work because it needs the two parts).
  so it was segfaulting going off then end looking for the other part. 
  but it only tried to do that when running in the test program. not as a stand alone program. 
  strange. but same fix as vastart, just dont do that. 
  should really have one big table of all the info i need for adding an instruction (like qbe does). 
- and of course that new test can't work with the legacy backend. maybe i should just give up on that, idk. 

More soul crushing Elf work.

- even before blink notices it doesn't have `/lib64/ld-linux-x86-64.so.2`, it says 
`blink/loader.c:139:20266 program headers aren't ordered, expected 15000 >= 6011000` 
so maybe i have to fix that for real linux too. 
- still `elf_machine_rela_relative: Assertion `ELFW(R_TYPE) (reloc->r_info) == R_X86_64_RELATIVE' failed!`
- if i make RelA.type 8: `signal SIGSEGV: address access protected (fault address: 0x2010000)`
that is indeed the right address `(offset = 33619968, type = 8, sym = 1, addend = 0) memset`. 
is it not doing relocations before setting my memory to read only? i can ask for 
ConstantData to be writable and then i fault on zero instead. 
i feel like this is where i was before where it just doesn't do the relocation at all. 
whatever i put in the addend is the thing i get. 
i think my theory before was it's telling me the symbol doesn't exist because im not asking for a specific version of it. 
the other idea is maybe i have to call some libc init function before doing anything. 

`readelf -W --dyn-syms a.out`

Zig: 
```
Symbol table '.dynsym' contains 4 entries:
   Num:    Value          Size Type    Bind   Vis      Ndx Name
     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND 
     1: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND __libc_start_main@GLIBC_2.2.5 (2)
     2: 0000000000000000     0 NOTYPE  WEAK   DEFAULT  UND __gmon_start__
     3: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND puts@GLIBC_2.2.5 (2)
```
Mine:
```
Symbol table '.dynsym' contains 7 entries:
   Num:    Value          Size Type    Bind   Vis      Ndx Name
     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND 
readelf: Warning: local symbol 0 found at index >= .dynsym's sh_info value of 0
     1: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND memset
     2: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND munmap
     3: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND mmap
     4: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND abort
     5: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND exit
     6: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND write
```

what am i supposed to set info to? length of the table?
zig has it 1. first valid entry? readelf goes away if i do that. 
doesn't help me tho. 

there's stuff about default versions being the one with `@@` so 
surely i just get that one if i don't ask for something specific. 

the internet seems to agree with the need to init libc theory. 
- https://stackoverflow.com/questions/62586808/how-are-relocations-supposed-to-work-in-static-pie-binaries
it really sounds like they're talking about when you're not dynamic linking tho. 
if i am supposed to call it before i can import anything, how the fuck do i do that. 
i can see `__libc_start_main` calls `_dl_relocate_static_pie` but that's in `#ifndef SHARED`
so yeah when you statically link it. maybe i need to include my own? 

zig is allowed to use R_X86_64_GLOB_DAT, why aren't i??

are you fucking kidding me??
it was `dynamics&.push(tag = .RelACount, val = m.imports.len);  // TODO: remove. surely this does nothing. clang doesn't do it`
taking that out makes it work. 
it was also the header ordering thing and the ConstantData writable thing. 
so if you have any of those wrong it just segfaults so when i tried them individually i figured they did nothing. 
so nice that every possible mistake you could make as identical symptoms. 
to be fair, the RelACount was different, just totally useless. 

now it segfaults after printing hello world but that's much more encouraging. 
that's just franca_runtime_init needs to call exit. i figured that out before. 
ugh, now i have to go through and do that in all my .ssa tests.

but anyway, i can link to libc on linux now. only took 700 years. 

- clean up some libc functions to use #target_os because they don't exist for x64 linux. 
`__clear_cache`, sys_icache_invalidate, sys_dcache_flush, pthread_jit_write_protect_np. 
- made expr::switch discard branches if inspect is a constant. 
- need to ask for libdl (for dlopen) and pthread seperately from libc
- franca_runtime_init isn't getting the cli args passed to it (it segfaults trying to read them and the length looks like a pointer. 
query_cli_args works tho (hacking that in makes sudoku work but still not the compiler). 

## (Feb 10)

- holy shit do functions with >8 args just not work? there's no way. 
yeah, add10 test as ssa works on amd but not arm. think i broke abi slots when i stoped scaling them? 
no it was `!needs_frame` didn't work when some args were passed on the stack (because i was thinking no frame means no RSlot). 
off by 16 bytes (because it was expecting to step over saved fp/lr) and need to use sp instead of fp. 
so that's reassuring for why i didn't encounter the problem before, it only happens when
you have a function with a lot of arguments that doesn't actually do any work. 
- wasn't setting ret/par cls for floats
- gen_call wasn't recursing up through function pointers to get to the right type 
- my hacky way of dealing with new_string_token didn't work when concatenating string literals
- convert CType to backend Typ

## (Feb 8/9)

- fixed compile_for_arg so hashmap(_, void) works
- add_hideset:
// there tend to be long runs of tokens with the same hideset 
// (ie. the output of a whole expansion), so don't reallocate 
// each one in that case. for my current tests, this takes it from 
// 9546 to 141 calls to hideset_union in this loop. 
// changing it to just be pointer identity makes it 143 so thats fine
- make skip_cond_incl just count instead of recursing to skip_cond_incl2
- '' sign extension. (literal.c)
- chibicc token struct has a filename field that is never read? maybe this isn't the project i should be stealing from...
- clang thinks this is UB:
  ASSERT(-2147483648, (double)(unsigned long)(long)-1);
with -O2 it works but with -O0 it's -1 on arm and 35 on x64
- i don't agree with the test `_Bool false_fn(); int false_fn() { return 512; }` 
i feel like you can require masking for every function that returns `_Bool`. 
"When any scalar value is converted to _Bool, the result is 0 if the value compares equal to 0; otherwise, the result is 1."
it's probably UB to declare it with two different signetures anyway. 

## (Feb 5)

- no, needs to be Token.ty.size, because of strings with wide characters.
- (string.c)
- return ? instead of crashing from some number parsing lib functions
- new syntax for lookup in a scope without doing a dot call. `Lib'func(arg)`. 
kinda silly but seems convenient. 

## (Feb 4)

- c vla just using .alloc and size as a Ref instead of a var
- hack for top level #use: just allow a string as though you had called import()
- two things i wasn't thinking about with icache:
  - you're supposed to check the predicate for a condvar inside the mutex
  - i wasn't clearing cache after creating stubs
- x64 is failing on no extsh. oh i bet i never use that but now i have to emit it to implement folding for it. 

```
// variable.c
// clang 18.1.8 arm64-apple-darwin24.2.0 gives
// (-1, -7) or (16, 16) with -fsanitize=address or (-1 -6) with -O2 
// so clearly you're not allowed to do this. 
ASSERT(7, ({ int x; int y; char z; char *a=&y; char *b=&z; b-a; }));
ASSERT(1, ({ int x; char y; int z; char *a=&y; char *b=&z; b-a; }));

// sizeof.c
// i dont do that. spec says sizeof(long double) >= sizeof(double)
ASSERT(16, sizeof(long double));

// pointer.c
// UB! asan agrees with me
ASSERT(5, ({ int x=3; int y=5; *(&x+1); }));
ASSERT(3, ({ int x=3; int y=5; *(&y-1); }));
ASSERT(5, ({ int x=3; int y=5; *(&x-(-1)); }));
ASSERT(7, ({ int x=3; int y=5; *(&x+1)=7; y; }));
ASSERT(7, ({ int x=3; int y=5; *(&y-2+1)=7; x; }));
```

- backend: Fixup.DataAbsolute allow increment (including negative!)
- another staticarrayofunknownlength problem, maybe i never actually got that to work, i just started and then gave up on initializer.c. 
i thought i had already done that so when i saw `typedef char T[]; T x="foo"; T y="x";`
i was confused for a long time thinking it had somehow made a VLA.
but no, VLA never has an initializer, you're just declaring an array of inferred type and then inferring it to two different things which is fine. 
type defs are just macros i guess. 
or i thought it was that the string needed to have a Ref maybe. but also no, we go one char at a time (which is silly). 
anyway the trick was just so create the ref for the array up front so that while you're reading the initializer, 
you can emit offsets from that ref to store the parts,
and then at the end, you put an alloc of the right size in the start block so you don't have to worry about the definition not dominating uses. 
- i was treating Token.str as a CStr, but instead need to get the length from Token.ty.array_len
- (initializer.c)

## (Feb 3)

- ah but now to use try_fold as my constexpr, i need to add back symbol offseting which i moved to isel before
- new flavour of emit() that always checks if it can fold right now
- gen_cast needs to deal with array sizes correctly. 
array types know thier size for sizeof but when we look at size for a cast we want to have decayed to a pointer.
- agh, painful, sometimes don't know the array length until parsing the initilizer so need to dealay making a stack slot for the variable. 
- handle global initilizers with pointers inside
- add folding for extsb/extsh, fix fn div(u64, u64) u64; to use the right #ir
- (constexpr.c)
- it's been so many mistakes in translating ops back to franca functions for folding. 
give up and do the fancy comptime thing where i just iterate over all of them and ask the backend to poke the right code in. 
means folding can't be compiled by legacy backend but that's fine since it's just an optimisation. 
- experimenting with invoke_specialized. i remember being annoyed that i couldn't make an Option or Array type in a macro easily. 
- functions for accessing members of a ScopeId, so c parser functions can just be in a struct that we iterate over.
- use the table for parsing stmts as well

TODO: import_c cant compile with debug compiler now

## (Feb 2)

- need to be careful with expressions that don't evaluate thier argument, just look at thier type, without generating ir for them
- need constant folding of binary operators incrementally as they get parsed so they can be used in array types. 
reusing my fold functions instead of re-pasting the giant switch statement. 
- i was treating init_data as a CStr instead of taking the whole size past zeroes
- making bit fields work again: i have a char and then two bit fields in an int, 
the struct is 4 bytes but it thinks the offset to write the field to in write_gvar_data is 4 which doesn't make any sense:
- my version of this didn't work for negative numbers which thier align_down needed.
made for confusing times for bit fields. 
```
fn align_to(offset: i64, align: i64) i64 = {
    // bad:
    //extra := offset.mod(align);
    //if extra == 0 {
    //    offset
    //} else {
    //    offset + align - extra
    //}
    // good:
    ((offset + align - 1) / align) * align
}
```
- `_Generic`

## (Feb 1)

- reworking all the control flow in the parser to output to *Blk with the right terminators directly instead of string labels. 
incremental step towards removing the ast. start by just having each node remember which block it should output instructions to. 
- sad amount of time on default_init(Qbe.Fn) expecting the caller to zero the memory
- the assignment to initial value in declaration() wasn't happening because it wasn't going through parse_expr, the node was just getting created and returned. 
- b.arg for loop conditions
- confusion about `label:` immediately parsing another statement, so you have to jump to the label at the begining not the end. 
- conditional(): where you need to insert casts to unify the branches it's painful to make sure they go in the right blocks. 

## (Jan 31)

- ND_SWITCH, ND_CASE, ND_DO
- ND_NEG use the right cls to make `switch(-1) case 0xffffffff:` work
  then use the right cls for cmp to keep `-1<1` working 
- (control.c)
- shinking casts do need to truncate sometimes? 
- (decl.c)

## (Jan 30)

- its not getting to funcall(). it thinks `puts` is a type. 
in find_typedef, converted to returning optional but didn't check if var.typedef is null so returned (Some = NULL). 
- in add_type, flipped to `if(!node.is_null(), => return())`
- (hello.c)
- in declaration, i converted to use comma_sep() but that eats the ; so i need one less tok.next
- prefix operators
- assignment operators have the other associativity 
- some of their tests are testing UB (clang doesn't pass them, and has differetn behaviour with -O2)
- (variable.c)
- my `// why not >= ?` in declspec i wasn't crazy, the internet agrees with me: https://github.com/rui314/chibicc/issues/142
- i think my test that forks has the problem of your jit thread being gone
- in declspec `ty := @match(` instead of `ty = @match(`, oops
- pass alloca to the backend
- trying to do casts without just pasting the table
- ! arg might not be `_Bool` so it's not just xor with one bit
- ND_SHR is shr or sar based on the type
- i get `(void *)0xffffffffffffffff > (void *)0` wrong. 
folding was wrong for u64 because my comparison operator functions in franca were converting to i64 first for the old backend. 
amazing, ok so thats great success. this was the whole point. found a bug in my backend,
that didn't exibit with the specific pattern of code franca generates (i almost always use signed numbers). 
- (arith.c)
- signed div/mod. typoed loaduh vs loadub
- (cast.c)
- make it easy to toggle between recompiling the backend every time and borrowing the host's version from the vtable when running as a driver. 
so the tests can run in 300ms instead of 800ms. 
- codegen ND_FOR
- convert ND_LOGOR, ND_LOGAND to ND_COND in the frontend
- goto, labels, break/continue

## (Jan 22/23)

- get rid of jump table switch instruction in the backend. 
keep the switch block terminator because it's convient for the frontend, 
just lower it immediately instead of letting it complicate everything by allowing arbitrarily many jump targets. 
- very long time on dumb memory safety mistake in emit_ir new emit_switch. 
- change constant folding back to how it was before so it can deal with the fold2.ssa test again. 
- very long time on constant folding of sel. 
my confusion was because it starts by assuming all edges are dead so does some folding based on incorrect phi values and then fixes it later. 
so if you try to make changes in visitins directly instead of waiting until the end you get garbage. 
- frontend using sel doesn't work on amd64. 
tried using a jmp instead of cmov in emit() to see if i just had the opcode wrong. didn't help. 
narrowed the problem down to is_hex_digit getting miscompiled.
ah! my nice xor to create a zero with smaller encoding clobbers the flags!
- don't handle symbol add/sub during constant folding. 
i think it makes more sense to just do it in isel,
because for arm encoding you might be better off doing the offeset seperately in a memory op. 
i don't do it all the way correctly yet because you still want to fold them if you're just creating the pointer to pass to someone else. 
but it generates a couple kb less code already so its not totally garbage which is reassuring. 
nice to get rid of 30 lines of boring code. 

## (Jan 21)

- changed Amd64Reg enum to order by encoding so i don't need a lookup table in emit
- yikes. i transcribed ulog2 as << instead of >>. lucky i barely use unsigned numbers. 
- take out nfree/namel in opt/ssa, because i can just use temp() (and then you maybe get nicer locality than reusing a global linked list like qbe does). 
- i want to do something nice for signed division by constant. 
for unsigned you just do a shift, but that rounds the wrong way for signed. 
here's what clang does:
```
void foo(long* a, long* c, long b) {
    *c = b % 8;
    *a = b / 8;
}

foo:
  add     x8, x2, #7
  cmp     x2, #0
  csel    x8, x8, x2, lt
  and     x9, x8, #0xfffffffffffffff8
  asr     x8, x8, #3
  sub     x9, x2, x9
  str     x9, [x1]
  str     x8, [x0]
  ret
```
you check if it's negative and cmov to do the right thing. 

I stole sel from a qbe patch: <https://lists.sr.ht/~mpu/qbe/patches/55968>. 
they only did x64 tho.

Hard to decide what's worth it. 
Like i can't measure the difference but I think it's neat. 
Is that worth ~100 lines of code?

This one's nice because sel is useful for other things too. 
Like if the frontend sees `@if(a, b, c)`, it's kinda wasteful to generate a bunch of new blocks for that. 

## (Jan 20)

- set block id in create_jit_shim and use right arg tmp cls in create_dyncall_shim so i can always run fails_typecheck in debug mode. 
- ok my new theory is that elf sections do nothing and it's all about the segments 
(except for static linker input (?), but the real loader doesn't care).
- Elf.Dyn val field should be virtual not file_offset (so i guess thats the difference between the data for static linker and for loader)
- oh boy, different crash in lldb when i put Interp header before the loads. 
now im seeing myself in my code and then calling 0. 
it's jumping to the stub and loading the callee from the right address 
(i can crash at any fixed address instead of 0 by writing something there), 
changing the addend doesn't do anything so it's not seeing the reloc.
- probably need `Needed = libc.so.6` but that doesn't help
- `readelf -r a.out --use-dynamic` sees it so it's clearly there and the offset looks reasonable. 
- aaa wasn't even using the interp_header i setup, making a new one instead. revealed by trying to add back sections for debugging. 
- now it's crashing on 0x6003040 which is very close to my Dynamic:val's offset by MYSTERY_SPICE
- ooo! you can `/lib64/ld-linux-x86-64.so.2 ./a.out` and then you get fucking error messages!!
- `./a.out: error while loading shared libraries: ./a.out: ELF load command alignment not page-aligned` i can fix that
- Inconsistency detected by ld.so: ../sysdeps/x86_64/dl-machine.h: 541: elf_machine_rela_relative: Assertion `ELFW(R_TYPE) (reloc->r_info) == R_X86_64_RELATIVE' failed!

## (Jan 19)

- better code gen for boolean not. 
notice when ceqw on the result of another cmp and just flip the comparison instead.
- small codesize improvements. arm64: don't make a frame if leaf + no stack. 
don't fold constant offsets because you'd rather only have one relocation and reuse the value (i go back and forth here). 
- make emit_instructions dsl allow passing a different function to call instead of emit().
use that for amd64/isel.fr:sel_inner:ultof.

## (Jan 18)

- im so stupid. i was like how can RsVec()::empty have ptr=8 so my bake_relocatable_constant chokes on it when i try to AOT? 
the whole point of that originally was that it followed rust's rules so it wasn't all zeros because the want to have a niche for `Option<Vec<T>>`,
and i've just been using that as a RawList that doesn't carry around an allocator. 

```
fn empty() Self = {
    // "unsafe precondition(s) violated: slice::from_raw_parts requires the pointer to be aligned and non-null, and the total size of the slice not to exceed `isize::MAX`"
    nonnull := T.ptr_from_int(T.size_of());
    (cap = 0, ptr = nonnull, len = 0)
}
```

- made slot not *4 so i can track byte granularity in opt/slots
- when there as a addr that didn't really escape but couldn't be folded because of alignment problems, 
don't do store elimination in that function, so now i can turn that on and get rid of a lot of junk i introduced in abi passes. 
arm64: (1201048 b, 1.174 s) -> (1084872 b, 1.074 s). 10% ain't bad. 
especially when it aplies to approx. any program that... calls functions.
legacy is 1.09M so this is smaller! but legacy is still faster :(
- TODO: there's something to think about with blit. 
when you copy forwards you iterate forward over increasing sizes but you start at the end of the area
because emit() is reversed. so the alignment doesn't work out. 
if you blit an aligned (i64, u32), you copy 4 bytes from the front and then 8 bytes unaligned. 
which like is fine clearly but means you occasionally can't encode the offsets you want and need to waste extra instructions. 
- noticed the panic macro function was generating a lot of (a := big constant; b := a + small constant; c := load b;) instead of folding the offsets into the load. 
experimenting with (arm64 isel:collapse_op)
```
if f.get_int(i.arg&[0]).is_some() && !f.get_int(i.arg&[1]).is_some() {
    i.arg&.items().swap(0, 1);
};
```
the interesting thing is when jitting so both sides are constant and the address computations were generated by a blit so they didn't already get folded,
but if you fold them there then they don't fit in the immediate for load (ditto if you swap so the small one is on the left).
you're better off creating the big constant address once and then doing many parts of the blit at different offsets. 
for aot the swap only happened 62 out of 174835 times so it's really not worth it. 
```
// jitted ____ bytes of code (./a.out backend/meta/qbe_frontend.fr)
// 650744 int(0)
// 643960 int(0) && !int(1)
// 650324 ___ but fold con+con
// 650464 int(0) but fold con+con
// 650700 int(0) && !int(1) but fold con+con
// 643568 ___ 
```
- changed slot repr on amd64. store elim: (1641784 b, 3.022 s) -> (1305461 b, 2.666 s). 
(super slow because rosetta i assume). that's 20% size, 10% speed. why did size shrink so much more than arm?
i guess abi differences would make sense since thats the main source of stack junk that doesn't get fixed by earlier passes. 
oh plus variable width encoding so load+store is 10 bytes, reg mov is 3 bytes. 
so one replacement is 70% savings vs 50% on arm (2 insts -> 1).
- TODO: holy shit this is not ok man
```
ldrb	w2, [x2]                      ; encoding: [0x42,0x00,0x40,0x39]
cmp	w2, #0                          ; encoding: [0x5f,0x00,0x00,0x71]
cset	w2, eq                        ; encoding: [0xe2,0x17,0x9f,0x1a]
uxtb	w2, w2                        ; encoding: [0x42,0x1c,0x00,0x53]
cmp	w2, #0                          ; encoding: [0x5f,0x00,0x00,0x71]
b.ne	#28     
```

## (Jan 17)

- fixed llvm backend a bit. promote prim for when @tagged literal passed to function by value. use right type for switch. 
so now it passes `run_tests.fr -- aot-llvm` and can compile the compiler (but the latter doesn't work!). 

## (Jan 16)

- my exit(42) elf works in blink but not real linux. it just segfaults on 0x7ffff095017b. that's distressing. 
oh its you have to add 65k to every offset for reasons. so it's like mach-o fake zero page i guess. 
but you don't need to map a segment there. and i was already skipping some to make it work in blink (you can't say your code starts at 0x0),
but it seems they have looser rules than real. 

## (Jan 15)

- fixed encoding for x64 float conversions 
- made opt/slots work for amd64. went from 2318611 to 2092056 bytes of code. 10% smaller is quite a lot. 

## 

- started some new examples: c and smalltalk frontends 

## (Jan 8)

- i really need to have a version of `@struct` that lets you use the normal top level declaration syntax
  so you don't have to go around switching semicolons to commas. this is super dumb.
- added `#reexport` which is like `pub use` so you can have internal dependencies without letting people depend on importing you to get other random stuff.
- simplified how const-fields in structs are represented. now they just point at a scope so you don't need everyone who wants to iterate
  over runtime fields to think about that which was getting really annoying for the rest of the compiler.
- attempting to resolve blocks lazily but have to renumber scopes more aggressivly, so kinda backed out of that.
  i got as far as it mostly working if i didn't stop at function declarations which meant it was slower,
  but that version fixed `// TODO: compiler bug? do we really want you to be able to bind to chunks which is declared lower in the scope?`,
  so that's interesting. i'll come back to that but for now a small vacation.
- examples/terminal.fr
- renamed some io functions to make it clear that they `_or_crash`

## (Jan 7)

- #use for grabbing stuff out of another namespace. again same as rust's `use` keyword but with an arbitrary constant expression.
  as a start, it can be a function annotation and then just apply inside the body of that function.
- cleaned up scope resolution stuff a bit, removed the separation between blocks and scopes because that was confusing.
- `fn import(Str)` as a more structured replacement for `#include_std`. instead of just pasting the code like a c `#include`,
  you get a ScopeId value that you can access const fields on like a struct or pass to `#use` to get the old behaviour.
  needs more refinement on when you have to wait on evaluating a top level `::` expression before i can actually replace the old thing with this.
- cleaned up Elf/Wasm bits to use import() for namespacing

## (Jan 6)

- #where for auto intantiating generics and adding to overload set.
  same idea as a `where` clause in rust to constrain argument types,
  but instead of a seperate language of traits, i just put a comptime expression in there so you can run arbitrary code.
  still lots of work left integrating with the rest of overload/call system but the basics work.
  i hope eventually it will be:
  - faster? less iterating through overload sets so its ok to do more work per entry.
    and don't have do any work at all when you make the type but don't call a function using it vs
    the current system where you tend to put the methods inside the function that returns the type.
  - less order dependent. right now when you manually `::Whatever(T)` in one function,
    you don't have to do it in later functions (because it exists already),
    so if you just put those in randomly when there's a compile error (like i do),
    your program is compile order dependent and breaks when i change the main_thread_pump logic.
    and that prevents me from doing cool jit-sema only the first time you call a function
    so you can load drivers faster and only compile what you actually use.
- added a coercer_const_expr at the very end of immediate_eval_expr. that happened 39945 times.

## (Jan 5)

- started transcribing linux syscall numbers. eventually i want to not use someone else's libc.
- only compile #target_os cases for comptime and runtime target instead of all possible ones.
  so now you're able to have functions that are not implemented on every os.
- start elf bits. examples/dump_elf for sanity purposes.
- basic emit elf with no imports and return 42. blink crashes on address 1.
  but if i make my entry point anything other than the correct offset, it crashes at that new number instead.
  so clearly it's running something? and then just jumping into garbage.
  in blinkenlights i can see RAX has 0x2a which is a very good sign.
  oh you can't just return from `_start`, you're supposed to do an exit syscall.
  if i do a staticlly linked c program you get a whole bunch of wrapper libc runtime stuff that does that for you.

  > also warp is garbage? the colourful disassembly in blinkenlights doesn't render anymore.
  > it used to work, i'm using the same blinkenlights binary, it still works in Terminal.app,
  > they just feel the need to update shit and make it not work anymore?
  > am i just going crazy and somehow i sleep walked turned off the option for "render the text likme at all"?

- syscall instruction. so now i can call exit() and get my 42 out of blink.
  adjusted the abi to use different registers for the later arguments.
  its actually just swapping out RCX because the cpu stomps that one.

## wasm (Jan 4)

- finish f.const for import_wasm
- now mandel.ssa works on my wasm runtime but not real ones.
  had to flip stack argument order for binary ops.
- indirect calls. need to make a `table` and use an `element` to initilize it full of function references,
  and then our "function pointers" become indexes into that table.
  for now i just put all functions in the table but (TODO:) an easy size optimisation
  would be to only put the ones referenced indirectly with `O.addr`,
  but then i need to keep track of another set of index mappings.
- cmp sometimes needs an explicit extension to Kl
- fix the convoluted thing i was doing to patch the function size before locals.
  it's a leb128 so you don't know how big it will be up front, but in code when its at the end of an instruction you can pad out will nops.
  but for locals there's no nop so i was like shifting up the locals to the end of the length and then padding the beginning of the code with nops.
  but that's stupid, you're allowed trailing padding inside the leb number by just putting `(sign_bit | LEB_MORE)*, sign_bit`,
  which is much less confusing than my way.
- use another global for hidden `env` parameter.
- #c_variadic in the frontend

## wasm (Jan 2)

- globals for stack pointer so now (static) alloca works.
  (for now i don't let frontends create thier own globals, they can just use a normal symbol in the data segment until i want exports).
- store instructions.
  TODO: fold constant offsets into the instruction.
- typecheck_failed: added check for store instruction with result.
  very distressing if you make that mistake because rega expects instructions (other than alloc) with result to not have a side effect so it gets removed.
- i was confused before about what block result types meant.
  every block has an implicit fall through at the end back to its parent,
  and any types passed there is its result type. if it ends with an explicit jump instead,
  its result type is Empty, not the argument type of the target.
- import_wasm: convert block arguments to phis.
  and rework the block stack so jump depths match the wasm structure.
  one wasm block does not corrispond to one basic block.
  track its start block (jump target for Loop), its fallthrough block (jump target for Block, If),
  and its current block where instructions will be emitted next.

## wasm (Jan 1)

- data segment. kinda nice that relocations are just you put the number in because you know your base address is zero.
  skip a page so we don't assign something to the null pointer because that sounds confusing.
  (TODO: should i have an option insert null checks since you won't auto-crash like you would on native?).

# ^^ 2025 ^^ 
# vv 2024 vv

##

- added J.switch block terminator
- experiment with the jump table being a data symbol of offsets or being a bunch of direct jump instructions.
  everything (including no jump tables, just if-chains) is roughly the same speed
  (ie. i can't tell the difference when just eyeballing the time to self-compile).
  so that's kinda disappointing. should probably take it out since it's extra code for no benifit.
  really we've just learned that branch predictors are really good and it's not helpful for me to go fucking around with things.
- reworked opt/fold to work with J.switch. my version is a regression,
  it doesn't handle phi nodes that become constant properly anymore (added a test case: fold2.ssa).
  but at least i understand how it works now and can come back later.
- fixed @is on non-sequential enums

## (Dec 29)

- fixed problem when doing `use_map_jit_on_apple_silicon` i was calling mmap twice
  and kind-of assuming they would be next to eachother and trying to munmap them as one.
  and it looks like it should have just worked out because i said too big a size for the first one so it wouldn't try to free junk when they didn't get put adjacent,
  and i guess i don't rely on layout that much when jitting, i just always pictured it a certain way in my head.
  now i mmap the whole thing at once and then MAP_FIXED over it to change part of it to not be MAP_JIT.
  It seems that MAP_JIT and then MAP_FIXED over it is fine, but the reverse is not.
  now i can drop comptime_codegen and run with `-repeat` for longer (also fixed 60fps.fr).
  `./boot` is still flaky tho.
- fix `display` for non-sequential enums.

## (Dec 27)

- failing `@debug_assert_gt(c.canvas_width, 0.0);`, which i think is actually a miscompilation of the compiler itself when doing stuff with the constant `: f32 = 640,`
  fixed incorrectly flipping prefix for truncd but that wasn't enough.
  but now it works if you do it in a normal variable instead of in a struct default value.
- construct_struct_literal wasn't calling coerce_const_expr, it was just assuming the type of the value matched.
  TODO: really find_const should call it for you
  that gets farm_game working on amd64!
  > bad that i can't explain why that worked on arm since the little repro test i wrote fails on the old version of the compiler.

in fact, i think amd64 now works better than arm64 because of my cache coherency woes, it can run 64fps.fr.

- fixed `need to do_merges when resolving by type.`
- clean up arm emit() for some instructions with similar bit patterns

## amd64 bug fix extravaganza (Dec 26)

- jit hello world works if i got_indirection_instead_of_patches=true on both modules. so clearly i was right when i asserted `!=.JitOnly` but i still don't know why.
  problem was a call to a DynamicPatched was outputting a fixup that would never be applied.
  so it was just calling offset 0 which is just the next instruction, and it was near the end of the function, so it returned to itself and then returned again.
- `rosetta error: unexpectedly need to EmulateForward on a synchronous exception` is fun because lldb chokes on it forever so you can't see what's happening.
- ok so it must be something shim related. sudoku works when aot compiled to x64 but not when jitted on x64,
  but it works if you don't call `solve` at comptime. oh and it works if #inline `assign`.
  works also if you uncomment `is_ready := f.getcon(0);` in create_jit_shim (which is also enough to run `examples/default_driver.fr`).

hmmm,

```
R2 =l addr $F924__shim
R6 =l addr $assign__924
xcmpl R2, R6
R2 =l addr $assign__924
jfine @, @_
<snip>
---
endbr64                                 ## encoding: [0xf3,0x0f,0x1e,0xfa]
push	rbp                               ## encoding: [0x55]
mov	rbp, rsp                            ## encoding: [0x48,0x89,0xe5]
push	rbx                               ## encoding: [0x53]
push	r12                               ## encoding: [0x41,0x54]
push	r13                               ## encoding: [0x41,0x55]
push	r14                               ## encoding: [0x41,0x56]
lea	rcx, [rip - 24]                     ## encoding: [0x48,0x8d,0x0d,0xe8,0xff,0xff,0xff]
mov	rax, qword ptr [rip + 100556228]    ## encoding: [0x48,0x8b,0x05,0xc4,0x5d,0xfe,0x05]
cmp	r8, rcx                             ## encoding: [0x4c,0x3b,0xc1]
mov	rcx, qword ptr [rip + 100556218]    ## encoding: [0x48,0x8b,0x0d,0xba,0x5d,0xfe,0x05]
jne	56                                  ## encoding: [0x0f,0x85,A,A,A,A]
<snip>
```

so that sure isn't a program...
it loads from the got into rax and then uses r8 instead.
sad day, in the .addr hack i just did, i typed 0b01001000 instead of calling pack_rex_si so high registers didn't get encoded.

- using the x64 compiler to aot, i get `we guessed wrong so the array resized and all the pointers are junk`
  sometimes, with a random multiple of 4096, but when it works it gets the same hash as the arm one cross compiling which is a good sign.
  ah, trying to zero_pad_to_align to page size, and doing that starting at the pointer to the base of the array, not at zero and just using the length,
  and `commands` is at the start of the text segment so it's allocated by page_allocator,
  so when running on macos-arm, where the page size is 16k, its always 16k aligned, but macos-amd's page size is 4k.
  need to do it based on the length instead of the address.
  same thing in codesign `deal with page spread across chunks` because of the SegmentCursor.align_to at the top of macho.emit.
  TODO: i should take advantage of the smaller page size to emit less padding on that platform but for now it's not a big deal.

- avoid folding memory accesses that create `base+index+(>4GB constant)`,
  which happens when jitted code has constant addresses and then you try to access a nested array i guess?
  TODO: i think now im being too conservative with the folding. it's fine as long as you don't have both base and index.
  that's enough to self compile on amd. hurrah!

and reproducibility works with cross compiling which is amazing.
still not everything works on amd64: lox, mandelbrot, and the gui programs.
incidentally, those are all programs that use floats heavily.

- float bitcast encoding mistake. in the table we use (.F, .M) vs (.M, .F) to tell the directions apart,
  but the movd:movq always takes the float arg on the same side so one of the directions needs a swap before actually being encoded.
  that fixed lox+mandelbrot.
- did register shuffling for 2-adddress fdiv which gets farm_game to compile.

## amd64 bug fix extravaganza (Dec 25)

- yikes, unpleasent memory safety thing.
  Alias.slot is a pointer to another tmp's alias field.
  However, it can't be an actual pointer because we read it in getalias which is called from loadopt:def
  which is intersperced with newtmp calls in that pass, which cause f.tmp to be resized, invalidating any internal pointers.
  I think it works for qbe because f.tmp is PFn so free doesn't do anything and after the alias pass once you start making new tmps,
  you don't need to update the alias info so it's fine that you're pointing into an old copy of the tmps array if it resizes.
  which would apply to my temp() alloc too except that when i suspend for inlining, it copies into a new QList in libc_allocator,
  and then keeps using that at the end if i need to emit_suspended_inlinables, at which point free does do something and
  now we're at the mercy of whatever your libc's malloc decides to do.
  i think i want to just use the module's arena instead of libc_allocator anyway but still nicer to follow the rules i think.
  seems that was what was making ./boot fail most of the time so that's very good to have fixed, but -repeat still calls into garbage so more mistakes exist :(

trying to get the compiler to run on amd64 with my own backend:

- fix typecheck_failed
- in sysv-abi, zero AClass array so we don't have junk in the align field
- turn off some div folding temporarily cause i don't want to deal with it yet
- truncd
- handle call to forward reference when got_indirection_instead_of_patches for jitting
- `Assertion Failed: cannot encode offset 4641883552`, hmm, we sure are loading a constant... `R2 =l load [4641883552]`.
  oh because we're jitting so we poke in real addresses and those are larger than 32 bits so can't be done encoded in one memory displacement.  
  have to catch that in isel and copy to a register.
- for got_indirection_instead_of_patches i need to convert RMem into a load from the got instead of a constant displacement in the instruction,
  but by the time you realize you want to do that in push_instruction it's kinda to late to do it sanely.
  like now i need to pull emitting prefixes into that function as well so i can do extra instructions first?
  but then i kinda need to reserve a register to use there.
  tho so far it mostly seems to be O.Addr which is easy because it's just producing the address.
  so start with just hacking it in there and see how it goes i guess.
  that's enough for it to jit `hello.fr` to the point of `// Hello Compiler` but then it gets bored and doesn't do the rest.
  you win some you lose some i suppose.
- fixed walk_dir test. cross compiling with #link_rename means fill_from_libc didn't find things.

## (Dec 24)

- fixed compiler_gui/dearimgui_demo examples
- don't need the asm versions of call_dynamic_values anymore
- there's something very strange where it sometimes decides you can't coerce an overload set to a function pointer.
  problem was suspending in coerce_const_expr with done=true so you don't come back.
  so indeed a subtle compilation order thing that started exibiting when i started marking more things done yesterday.

## (Dec 23)

- fixed occasionally getting spliced names like `_Anon__5989Ano`.
  thread safety in intern: each bucket has a lock but they were using a shared ArenaAlloc.
  the rust people start to make some good points.
- i was calling join_codegen_thread on comptime_codegen instead of the one used for the driver,
  so if you called a shim that tried to trigger a compilation, it would get stuck.
  but maybe changing that doesn't make any sense either, because if my premise is that (we use different modules
  for the driver part and the comptime execution because we're pretending the driver is like an aot program and
  saving time outputting a binary is just an implementation detail),
  then you shouldn't be calling a function in the comptime module from the driver module.
  but if you made a function pointer value, that will be in comptime_jit and it doesn't do any remapping since it's not Aot.
  so then calling directly and calling through the pointer will be calling different versions of the same function.
  so maybe it's wrong to try to use two jitted modules on the same compiler instance.
  but also you kinda don't want got_indirection_instead_of_patches set on the driver one because that's slower.
  somehow ive revealed a massive oversight here.
- uncommenting (in compile_expr:Tuple)
  // This would make sense to me but it makes lox test call uncompiled.  
  //expr.done = done;
  saves ~50ms (1350 -> 1300 non-legacy, when i get lucky with timing).
  gets me to 777 when i cheat (old jit + use legacy but build new + precompiled driver).
  the comment was from before i had shims so now it's safe to be more careless.

## (Dec 22)

Made from_bc single threaded because i think it makes a better example if it's not sharing the CodegenWorker stuff with the new version.

Extracting -legacy. Getting rid of `opts.comptime_jit_vtable` because i think supporting multiple is more painful than it's worth,
you can still have driver programs for other runtime backends, but for the core thing that has to deal with out of order compilation stuff,
it's really painful to add any more confusion than necessary. go through and make sure all the ifs on NEW_COMPTIME_JIT are static so we don't have to include the code.

before:

```
>>> compiler configuration: context=implicit, threads=true, debug_assertions=false, nctjit=true
>>> compiled driver__1783 in [CPU Time] frontend: 160ms, codegen: R:90ms + C:25ms
>>> [CPU Time] frontend: 1268ms, codegen: 515ms
>>> 1345576 bytes of code, 87264 bytes of data.
```

after:

```
>>> compiler configuration: context=implicit, threads=true, debug_assertions=false, nctjit=true
>>> compiled driver__1950 in [CPU Time] frontend: 296ms, codegen: R:169ms + C:42ms
>>> [CPU Time] frontend: 1201ms, codegen: 434ms
>>> 1105560 bytes of code, 80176 bytes of data.
```

Now you have to pay the cost of compiling that in default_driver but you can precompile it and then we're back to how it was before but with a different connotation.
I'm considering having drivers use shims too so you don't compile big functions until the first time they get called.

the thing where run_tests hangs is because fork doesn't duplicate threads.
so when it tries to compile with NEW_COMPTIME_JIT it gets stuck in wait() forever.

## (Dec 21)

this code is pretty funny

```
if (_sapp.quit_ordered) {
    return YES;
}
else {
    return NO;
}
```

and like ok sure, maybe you're not supposed to assume you know what `__objc_yes` is,
but it would seem that's just so the compiler can tell if you meant to make a bool or an NSNumber
(https://releases.llvm.org/3.1/tools/clang/docs/ObjectiveCLiterals.html).
we can't be having 6 lines of code per line of code.

```
#if __has_feature(objc_bool)
#define YES __objc_yes
#define NO  __objc_no
#else
#define YES ((BOOL)1)
#define NO  ((BOOL)0)
#endif
```

Little vacation from compiler was nice but I'd feel a lot better
if i could extract the old backend from the core compiler
so stuff gets less insane before i risk forgetting more things.

Trying to get it to compile the tests that do `@if(@run query_current_arch().unwrap() == .aarch64) {`
is somehow super painful because of how shims interact with redirects (in this case inserted for #target_os
but probably always because there's something in vec that didn't work as a #redirect).
It should be so easy but somehow i can't do it.
Clearly I need to make that system simpler somehow.

Jit shims make fewer redundant calls to report_called_uncompiled_or_just_fix_the_problem:

```
// If the entry in __got for the target symbol is not this shim, call it directly.
// Otherwise, call into the compiler to ask for it to be compiled.
// Most calls will just go through __got directly in which case this check is wasteful,
// but it helps in the case where you created a function pointer to the shim and stuck it in memory somewhere,
// now not every single call to that has to go through the compiler and take a mutex to look at the symbol.
// It seems this doesn't make each shim only call into the compiler once (TODO: why?) but it does lower the number.
```

not measurably faster but spiritually better.

need to fix repro. it's only when NEW_COMPTIME_JIT and not -legacy which is iteresting.
so it's something about trying to use two modules in new emit_ir at the same time?
but -legacy still uses the new backend so it's specifically a frontend problem?
many programs work tho. parser specifically has this repro problem.
ah, there was a path in emit_constant that didn't go through `c.zero_padding(ty, to_zero&);  // Needed for reproducible builds!`
so thats reassuring. works again.
still can't quite articulate why it only happened when both sides used the new thing.

## (Dec 19)

- allow multiple things in index expression `a[i, j]` -> `index(a, (i, j))[]`

## (Dec 18)

- start porting sokol_gl
- added require_layout_ready(Type) so i can add offset_of
- start working on translating my language to MSL for shaders.
- added get_function_ast so you can do more powerful reflection things at comptime without wrapping everything in a macro.
- allow #annotations on struct fields.
  they don't do anything, Binding and Field just pass them through, but comptime code can read them.
  so i can use them for `layout(binding=0)` / `[[position]]` / etc.
- shader languages have the vector swizzling thing like `v.xxxx` which would be kinda nice to have so it could be lowered directly for shader translation,
  and still allow you to run those functions on the cpu if you just want to print out some numbers for debugging.
  so im tempted to let you register magic types where field accesses become macro calls,
  but maybe that gets super confusing quickly.
  did some plumbing for TypeInfo.Swizzler but the compiler doesn't call into it yet.
- (debugtext) parse font data from readable strings at comptime.

## (Dec 17)

- auto-cast float literals to f32 so you don't always need a `.cast()`
- allow underscores in literals. `1_000_000_000` is more readable than `1000000000`
- auto-cast string literals to CStr so you don't always need a `.sym().c_str()`
- infer type of const fields on structs lazily so you don't need to use `@rec` as often to use them as namespaces.
- implement link_rename in from_bc/emit_ir for walk_dir on macos x64. works with -c + clang but segfaults with my own exe. sad day.
- fix x64 emit neg acting on the wrong register. that took much too long to find. ugh.

> using a different computer.
> recall: `spctl developer-mode enable-terminal` -> Privacy Security Settings -> Developer Tools.
> makes ./boot go from 19s to 10s.

## (Dec 16)

- examples/softdraw can successfully make a window and draw stuff now that i have a jit backend that follows the abi correctly.
- new example: simple gui program that lets you scroll around a text file.
- ported sokol_debugtext. it looked intimidating but most of it was tables and c macro boilerplate.
  eventually i want to be able to compile a subset of my language into the shader languages but for now,
  i think having a seperate binary file for the precompiled shaders is less ugly than putting it in a giant byte array in the code.
  clicking into a file that's 450 lines long is much less unpleasent than one that's 4900 lines long.

## (Dec 9-15)

- my asm for call_dynamic_values is super primitive so i can't make `fn zeroed($T)` less dumb,
  so instead of that just jit a function for each FnType, that deals with moving arguments between byte arrays,
  and then i only have to implement the abi in one place. but i can't use that until i get rid of old comptime jit because it didn't follow the abi either.
- want to use new backend for comptime jit as well but that's harder
  because you need to be able to call functions in the middle of adding more to the module.
  instead of doing patches, always have forward references go through `__got` even if they're local,
  and then i don't have to worry about flushing modified code after doing a patch.
- version of `create_jit_shim` that uses new backend. again, can't use it until everything does because of abi struggles.
- new backend does more work per function so more incentive to fix how many tiny useless ones i create in `immediate_eval_expr`.
  instead of suspending on Jit after deciding it needs to be wrapped in a function, try CompileBody first and see if it decarys to a value.
  add PtrOffset/Deref to `check_quick_eval`
- trivial mistake with just using `idx := 0` every time i wanted to get a new entry instead of keeping it around
  so things didn't always get compiled in the right order. so extra confusion about which callees would be in range for a sys_invalidate_icache
- we've learned condvar wait/signal is not the same as just sleeping in a loop until a variable changes.
  i guess you're not guarenteed on what order you'll see changes on different cache lines even if you do both changes and check in a mutex?
  which is fair but then why is condvar any different? do they put some magic barrier thing in there that applies to all memory?
- forever confusion about when you have to call sys_invalidate_icache.
  i guess it has to go on the thread that runs the code, but also surely you need to make sure the thread that writes it flushes its data cache.
  so do you need to call the other one there too?
  can't just have shims get filled in later because you need to make sure the new thing gets flushed even if you don't compile anything else in between?
  memory barrier instruction seems to make it a bit better.
- another round of sprinkling #inline around. got resolve_in_overloadset_new from 740 to 500 samples
- take out add/sub 0 (in arm isel which is silly but convient)
- fix ir templates to use tmp.visit for arg index so you can disable TRACK_IR_NAMES
- use copies instead of load/store in the frontend for scalar vars that you don't take the address of.
  doesn't make it much faster but it's easier to read the generated code. could do better with more guess_can_assign.
- silly fix for `-c` with new emit_ir, just had to leave imports Pending.
- #target_os has a problem with looping calling its shim when jitting because the target doesn't get added as a callee.
  but its going to be such a pain for cross compiling if calling it when jitting puts junk in the callees list,
  maybe it needs to be a more aggressive special case.
  the worse thing is just how painful and confusing diagnosing that sort of problem is but I'm not sure how to make it less complicated.

##

- in arm isel, instead of doing fixarg and then going back later to fold address computation into the immediate of load/store,
  do a prepass. this way you don't create a bunch of extra temps that are just going to disappear later.

trying to do elide_abi_slots.

stats are -unsafe, compiling itself.

```
before:
>>> [CPU Time] frontend: 1692ms, codegen: 595ms
>>> 1420472 bytes of code, 86384 bytes of data.
removing loads:
>>> [CPU Time] frontend: 1569ms, codegen: 598ms
>>> 1311064 bytes of code, 86496 bytes of data.
```

##

- make slot promotion treat 1/2/4/8 byte blit as a load+store.
  kinda silly cause the frontend just shouldn't generate those but while prototyping its nice to not care and still get nice output.
- mistake making arena_alloc always return 0 was Block using the result ref from compile_expr instead of pulling it out of the placement.
- i bet the problem with default_driver is that im trying to use the real calling convention now.
- when a constant is Value.Small it might still be an aggragate like (u32, u32) so scalar_result should allow storing that from an int literal.
- trying to run other_main its trying to jump to 0s when calling into jitted code by the old backend.
  which looks a lot like the not calling clear_cache problem but the address is wrong.
- for macho_loader, it seems to think MAX_i64 is -1? oh cry, i said cgew instead of cgel so it was only looking at the low 32 bits and comparing as i32.
- trying to make it work for aot. its trying to import vars. oh im not calling reference_constant recursively.
  and same for mangling imported symbol names. on the plus side, this is the most helpful error message i've ever seen from the machO stuff `dyld[3164]: Symbol not found: _memmove__231`
- got run_tests to allow using the new stuff. fail 201/309.
- forgot to call check_for_new_aot_bake_overload. sympom was strings only being the first character because it thought it was just a \*u8,
  and then you'd print whatever other garbage bytes which were often invisible because unprintable.
  fail 151... fluctuates tho which is creepy.
- replace emit_constant with creating a data structure so codegen thread doesn't need to access compctx and frontend thread doesn't need to access the segments.
- randomly fails sometimes, but at least now i can look at the exe. mandelbrot is pretty small and still has the problem which is nice.
  lowering abi types wrong sometimes, less if i spam print so you know its a threads thing.
  mutex for module.types, frontend needs it the first time you reference a type, backend needs it during abi and inlining.
  got lucky for old backend because i used opaque types cached by size instead of trying to follow the calling convention.
  also wrap_with_runtime_init can't use module.scratch.
  fail 5.
- back to the old one for a moment, time's bloated cause i have an extra ~2k lines.
  finally doing #inline not makes codegen thread 650 -> 610.
  it's annoying because #inline happens before #fold so you have to manually `::` for conditional compilation.
  kinda fixed it but :InlineFoldHack.
  really should just do the special case and make sure `if !` just swaps the branches/cmp.k
- add BakedEntry.FnPtr to pending. fail 3.
- aha i remember this when i was ffi-ing with the bootstrap rust compiler.
  Span:(u32, u32) my old jit wants to pass in 2 registers but that's not what the abi says to do.
- changed a bunch of stuff in the frontend to let things be intrinsic on one backend or another with normal body otherwise.
  thought it might get slower but seems the same-ish.
- i think my current thesus is that the emit_ir is about the same speed but generates much shittier code so slower when self compiled.
  the new one compiles faster if it was itself compiled by the old one, 1450->1400.
  but then compiling itself is 1700. backend time is more similar for all 590 vs 615.
  i bet its because im actually using the abi passes now and they generate really dumb loads+stores because they happen after mem opt.

> i really need to write my own terminal thingy.
> i like that warp lets me click the mouse to move my cursor.
> i don't like that they give you a shitty banner ad until you update
> and then as free bonus with your update change the colour of the suggestions background and the boldness of the text.
> like bro please just let me run the program

## (Dec 1)

- finish emit for shared libraries
- support loading driver from a dylib instead of recompiling it everytime.
  - confusion about a `.sym().c_str()` on a runtime value that was relying on being jitted.
  - need to improve error detection for calling a #comptime_addr at runtime
    now that i don't detect it when trying to compile Bc.GetCompCtx (since that doesn't exist anymore).
    right now its just an int3/brk which is not super helpful.
  - saves 170 for default_driver.fr and now i don't need to feel bad about taking backends out of the core compiler,
    because they don't always run in the slow jit anymore.
    for example, `-aot=llvm -S` takes 1800 to jit and run or 1240 when precompiled.
- for bscopy, just calling memcpy instead of `BSOP(a, b, fn(a, b) => b);` makes codegen thread go 600 -> 550.
  im sure llvm would have noticed but i certainly don't do idiom recognition.
  backend isn't the bottleneck currently tho so it doesn't actually make it compile faster.
- asm for count_ones and trailing_zeros cause i don't like seeing them near the top of the profile.

it's sad that my asm functions have to be done through a call with the c abi where i spill all the locals first.
i can't decide if i should try to commit to making intrinsics for everything you could ever want,
or try to make real inline asm that can exist as an expression inside a function so good that it's not painful to do everything with inline asm.

letting the backend have asm inside functions might actually be easy, because rega already doesn't know very much about calling conventions,
the abi/isel stuff expresses register constraints as copies, so if i let you just have an opaque blob of asm bytes
that uses specific inputs/output registers as though it was a call. just need to have the frontend passes that expect only tmps
to leave references to registers alone. tiny change to alias, copy, fold.
that's not quite as good as c's inline asm where you can have template registers that get picked by the compiler,
but it's pretty pleasing for so little work.

## (Nov 29/30)

- frontend: slow progress on new emit_ir from ast instead of going through old Bc.
- arm: fixealways run in the slow jit anymore. d HFA abi. it was always passing >16 byte structs in memory which i learned was wrong a from soft_draw example a while ago.
  > <https://lists.sr.ht/~mpu/qbe/%3CCAHT_M7Pp-6_vSjOd-WkRt4ACJWLrKq=YpgUrnzW0Vy=T-7AFYg@mail.gmail.com%3E>
- arm: fixed incorrectly transcribed udiv bits. only manifested on fold test with fold turned off which is strange.
  always run in the slow jit anymore.always run in the slow jit anymore. didn't mess up franca because i only use signed math currently.
- amd: temporary rort/byte_swap #asm impl. revealed bug with const arg + FuncImpl.Merged that im going to ignore for now.
  that gets sha256_examples working on x64 (but only with linker, not my exe... odd).
- incantation to get sane disassembler: in `~/.lldbinit`: `settings set target.x86-disassembly-flavor intel`
- amd: embarasing amount of time on storeh being an incorrect copy paste of storeb `e.a&.push(@as(u8) c.trunc());`
  instead of `e.a&.reserve_type(u16)[] = c.trunc();` so putting out one byte of immediate instead of two,
  so all the instructions later get decoded wrong, because variable width instructions are garbage.
- amd: found some only work with linker. `unexpectedly need to EmulateForward on a synchronous exception`, ive seen that before,
  odd way to spell stack overflow but go off i guess.
  do_fixups_amd64:RipDisp32 wasn't including increment when an offset got folded into the constant reference.
- amd: simple_addr wasn't always converting fast-allocs to RSlot. 7.
- amd: make old jit return result pointer in rax because new backend relies on it.
  added new test for that which llvm-aot fails but its because it optimises away the pointer comparison when the caller checks if it worked,
  it does however follow the abi correctly for the callee.
- amd: painful float negate
- arm: neg even though franca doesn't use it

## codesign (Nov 28)

ahahahaha i've defeated apple.
ive produced a binary signed so incorrectly that when you try to run it the computer freezes for ~20 seconds
and then the screen goes purple, it shuts down and restarts. garbage. i love it.

```
codesign -d -vvvvvvv a.out
a.out: code object is not signed at all
```

hmmmm, why your os crashing then friendo... me thinks thou dost protest too much.

```
objdump  --all-headers  a.out

a.out:	file format mach-o arm64
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/objdump: error: 'a.out': Invalid/Unsupported object file format
```

this thing can parse it...

```
/Users/luke/Downloads/llios-main/macho_parser/macho_parser --cd a.out
LC_CODE_SIGNATURE    cmdsize: 16     dataoff: 0x128210 (1212944)   datasize: 2497
SuperBlob: magic: CSMAGIC_EMBEDDED_SIGNATURE, length: 2496, count: 2
Blob 0: type: 0000000, offset: 28, magic: CSMAGIC_CODEDIRECTORY, length: 2460
  version      : 0x20400
  flags        : 0x20002
  hashOffset   : 92
  identOffset  : 88
  nSpecialSlots: 0
  nCodeSlots   : 74
  codeLimit    : 1215969
  hashSize     : 32
  hashType     : SHA256
  platform     : 0
  pageSize     : 16384
  identity     : foo
  CDHash       : Unavailable. Use 'build.sh --openssl' and run again.

  Slot[  0] : 29a5508ca386f6b14cca312d782f5504d78dd3acd26ae7a6ae4e9ab569ed3556
  Slot[  1] : 15ca388192bb891e2a8b6bcdd3c1249dca923824737513e53920f530cd666bfd
  Slot[  2] : 0409137b727cedb66f23b0fe974bf0caf2d8d343f70d93f2f8ae8f64942f2fed
  Slot[  3] : 6d6540fef7e20424468235786621f9b3287aa51b3b67ae51601eb8fb35c8fbd9
  Slot[  4] : a9f2009ecda80a18ec61a65356e65fab08139396338c3ca18d4bd33353d00e97
  Slot[  5] : 7b2ffc6c5bee282f59bf6961b25a4eec9b09dfcf76b400c46d7caaaabaa0b9aa
  Slot[  6] : d1ad3a340641ca99a94e26d80323625e17025254c7079da30d474af642d2f0df
  Slot[  7] : 681da381a6ef15659b872354acfae545814fd7a18166c31f6ec9cb0248ab81fd
  Slot[  8] : 0eb5903bf534fc4777184400dcae9f5dea819e657412b0261711c11c7409ca64
  Slot[  9] : a8ce78769ede445625c03e8a16d3ecd0a82588feb2337149a782c797e14cd171
      ... 64 more ...

Blob 1: type: 0x10000, offset: 2488, magic: CSMAGIC_BLOBWRAPPER, length: 8
```

maybe pageSize needs to be 4096?
no it doesn't matter, i just did the math wrong and was missing the hash for the last page or got the file size wrong and had extra hashes depending which mistake canceled out.
that seems like a slight over reaction.

pleasingly mine is faster than using `codesign` because i don't have to write out the file, exec something, have it read it, finally do the work, and then write it back again.
20ish ms that comes out of the 160 mysterious system part reported by time.

---

i think ive started triggering a thread safety problem more reliably.
hopefully its not something insane about code signing wrong and just one of the many shortcuts i took.
in the objdump diff between a good one and one that hangs, it starts with a bunch of just offsets being different,
and then the first real change is missing

```
< 100050814: 13805800    	ror	w0, w0, #22
< 100050818: d65f03c0    	ret
```

and then the infinite garbage of all the addresses on the left being different
(i should really remember to use --no-addresses, it just annoyed be cause it doesn't take out jump targets).

but anyway thats reassuring because i think its just because when you declare an asm function
it calls add_code_bytes on the frontend thread directly, which then fights with emit_func_arm64 over `m.segments[.Code]`.
yep, seems to be fixed by adding those to the queue.
I guess adding codesign stuff just make it more likely to happen because i added rotr/bswap that i haven't bothered to make intrinsics for yet.
Seems to have also fixed the rare reproducibility problem i noticed before, so thats's good.

just to be safe also delaying emitting constants until after rejoining thread instead of just near the end randomly.
but better would be doing them in the queue because then you wouldn't need as many fixups.
acutally it would have been fine until i started trying to use DataConst instead of only DataMutable becuase that's what GOT entries use.

---

TODO:  
llvm backend repro has regressed tho which is odd cause you'd think thats the easier one.
but i don't think "llvm" and "easy" have every been used together in a sentance not containing the word "not".

```
523217c523217
< @g821 = private unnamed_addr global [3 x i8] c"\C8D\B0"
---
> @g821 = private unnamed_addr global [3 x i8] c"\C8\04\DE"
523223c523223
< @g827 = private unnamed_addr global [3 x i8] c"\00\00\DA"
---
> @g827 = private unnamed_addr global [3 x i8] c"\00\00\92"
```

## (Nov 27)

- sha256 for codesign
- started stealing exports-trie from zig for dynamic libraries

## (Nov 26)

yikes, this was gonna suck when i got around to trying ctx=implicit.

```
// [snip]
mov rax, rdx  // env
// [snip]
mov rax, rdi  // callee
// [snip]
call rax
```

using arm compiler to run qbe-exe-x64

- fail 78
- lots of Safety Check Failed, comparisons messed up? fixed w=true on the setcc of xtest becuase you can't encode some registers that way. 73.
- w=false extub, 72

ahh i have reading comprehension problems. i saw:

```
1. In 64-bit mode, r/m8 can not be encoded to access the following byte registers if the REX prefix is used: AH, BH, CH, DH.
```

and interpreted that as "don't use the REX prefix when accessing a/b/c/d as a byte",
but it means the opposite: "if you don't use the REX prefix, you get bits 8-16 which is never what you want."
AH means bits 8-16 of RAX, AL means bits 0-8 of RAX.

better phrasing elsewhere:

```
1: When any REX prefix is used, SPL, BPL, SIL and DIL are used. Otherwise, without any REX prefix AH, CH, DH and BH are used.
```

- fail 17

---

Running the backend on its own thread goes from (1850-implicit or 1780-static) to 1390-implicit (all numbers unsafe).
Profiler says ~25% of the time was in the backend so that's pretty close to purely parallel. We only stall waiting on the backend 11 times when self compiling.
dynamic context pays for itself now (unless i do thread locals but maybe they're slow too, who knows, probably not cause i wouldn't have to generate code for saving the register).
Notably, because of how the queuing works, threaded builds are (mostly?) reproducible.
would get harder if i wanted more backend threads.
which is funny because single threaded builds didn't reproduce recently and i just now revealed problem was not calling push_dynamic_context enough.
sadly they're not reproducible between threaded and not threaded (both building the same program).
can print names in codegen and they're doing things in the same order tho, so not sure what the difference is.
seems threaded occasionally doesn't repro itself tho, perhaps i spoke too soon, farther investigation required.

now the flat 90ms spent loading the driver before getting to the threaded part looks more attackable,
so i could bring back loading from dylib but that means i have to make my own dylibs.
damn hard to pick which is more soul sucking between mach-o and x64 encoding.

---

- rex prefix for long sign extend beforev division. 12

## (Nov 23)

- static dynamic_context on llvm (oxymoronic)
- `#log_ir("RC")` so you can see ir without it being super spammy.
- apple_extsb only copy blocks if something changed (like simplify.fr does)
- panic handler use context so different threads can have thier own
- treat extuw after loaduw as a copy. can't always do it because you're allowed to use extuw to clear the high bits.
  maybe loaduw should always output a long?
- in spill: `dead := pass.f.tmp[t].slot == -1 && !b.out&.bshas(t);`
  // my fuse_addressing in arm isel doesn't update usage counts correctly so i end up with a lot of dead pointer arithmetic.
  // rega would remove the dead code anyway but we can skip doing some work and reduce false register pressure.
  // A less hacky solution would be to fix it in fuse_addressing instead. that would prevent us from incorrectly thinking the args are live until getting here.
  // saves ~70ms. This happens 150k times with fuse_addressing on and 6k times with it off which mostly reassures me that i understand how we get here. -- Nov 23
- lock buckets while you access symbol info to prep for threads.
- less CStr when working with symbols
- list of functions with seperate arenas so threads can pass them around.
- clear `tmp.use` before going in inline cache so you don't try to reuse memory if it gets suspended.

##

before i did some manual outlining in list and context stuff, the worst the time got was around 2250.

## (Nov 18)

isel mistakes:

- initial port, 20
- emit() call ordering for float immediates, 17
- safety check getting index past the end to do bounds checks in runmatch, 9
- hacked around miscomplication because of max_homogeneous_tuple, 3

## (Nov 14)

- relative addressing is from the start of the next instruction but i was getting the current location before encoding the displacement.
- xtest needs extra swap like cmp. need to figure out whats going on there.
- conaddr.ssa test wants to encode an absolute address in sib. TODO: does this ever happen in a non-contrived situation?
- for stubs to got, jmp not call because you don't re-push addr (its like b without l),
  and i keep forgeting that offsets are from the end of the instruction not the start of the patch like they are on arm.

could do such a cool steganography thing with the redundant instruction encodings. surely someone has that already.

## (Nov 12)

- made sequential enum use the right value size based on its declared type so const_eval_any doesn't fail debug check.
- start of ir parser so i can run tests without LINK_QBE_C
- (minimal, 58), (loadw alas + predeclare blocks + jumps, 54), (phi, 47), (header par, 35), (fix off by one on lines, 33),
  (call arg, 29), (blit, 27), (float literals, 25), (data, 21), (vararg, 18), (env, thread, fix export\n, fix cstr, 12)

## (Nov 10)

- spent so long on typoing `Indirect01 = 0b10`

## peaceful cleanup (Nov 6)

a little strange that these generate different code.

```
fn is_load(j: Qbe.O) bool =
    @is(j, .loadsb, .loadub, .loadsh, .loaduh, .loadsw, .loaduw, .load);

fn is_load2(j: Qbe.O) bool =
    j.raw() >= Qbe.O.loadsb.raw() && j.raw() <= Qbe.O.load.raw();
```

the latter has extra sxtw's (and some are on constants becuase I'm not running the fold pass),
because i don't use 32-bit cmp because I didn't want to deal with it on the old backend.
In the @is_op i extend the constants to i64 at comptime.

- move inlining before memory opts just incase it can delete anything extra.
  would help a lot more if i inlined things with aggragate arg/ret because thier abi inserts a bunch of junk copies.
- move some @debug out of lambdas in emit
- combine is_store+size_of_store (or load) by returning an option
- alignment padding between 4 and 8 byte local stack slots to help fuse_addressing.
- sprinkle #inline fairy dust on functions that look dumb in the disassembly.
  I feel its always a win if it actually ends up making to code smaller.
- use @is in the frontend
- generate lookup table for argcls at comptime
- go through some old code and use operator syntax sugar
- Trunc64To32 becomes copy:Kw instead of extuw:Kl

nets code size ~1260KB -> 1188KB, but the main thing is it's nicer to look at now.

## the line must be drawn here (Nov 4/5)

- versions of the compiler compiled by llvm.fr, from_bc.fr with my qbe, and qbe.fr with real qbe, compiling with my qbe all produce the same binary when they work
- versions compiled by either qbe have the bug but the llvm one (-Os and -O0) does not.
- often it works. if not, mostly segfault but sometimes its LexError.Unexpected.
  which suggests to me that im stomping random memory and sometimes it happens to be fine or unmapped or in the source code itself.
- it can't be use after arena_reset because that would never be unmapped.
  unless its using junk as a pointer. easy to check, still happens if I disable
  reset_retaining_capacity/reset_retaining_original/deinit.
  still happens if i disable dealloc so its not a normal use after free either.
- i can try address sanitizer on the llvm one, maybe there's some logic bug in the frontend part that just doesnt really manifest somehow, idk, cant hurt.
  need -fsanitize=address to clang (both compile and link) and also sanitize_address addtribute in llvm ir.
- asan thinks im calling memcpy with overlapping args, im not calling it from my code, its inst_call for CopyBytesToFrom uses llvm's memcpy intrinsic.
  I can use thier memmove instead and then asan stops complaining. that's promising.
  but making my qbe call memmove for every blit still has the problem. sad.
- i suppose asan might not be doing much because i mostly use my own areana allocators instead of malloc.
  making get_alloc use todo_allocator doesn't change anything.
  (tho it does make a compile error unless i change @static to zero the memory which is offputting but easy to fix and clearly not related to the main problem).
  so thats kinda a dead end.
- ok new tact. write a simplier version of rega and skip as many opt passes as possible and see if that still has the problem.
  just give every tmp its own stack slot and only use 3 registers ever.
  - special case to ignore copies to null that the other rega needs as markers after a call.
  - mistakes around phi nodes.
  - why does store argcls need to be Kw?
  - swap needs to be a special case but is only inserted by other rega so don't care
  - fold1 fails without constant folding (but it does that with normal rega too)
- ok so this is good. minimal_rega+isel+abi+convert_to_ssa passes tests and can compile the compiler, but doesn't seem to have the bug.
  or im just getting impatient because its slow as fuck.
  but it got through 1000 compiles of the empty program without crashing (before i got bored because that took almost 8 minutes).
- ok so what if i add back other passes? same as normal but this minimal_rega instead of spill+rega.
  that also seems to not have the bug. so i guess its some subtle bug in qbe's spill or rega that I faithfully ported.
  thats kinda painful.

oh hey i even noticed this before.
`// TODO : qbe uses x18 for this but i feel like you're not allowed to do that. :SketchPlatformRegister`
whelp todays the day we todo that todo. progress.

by far the most common thing that used the scratch register was swaps inserted by rega.
and i can see how randomly when you try to swap registers and one gets zeroed it could manifest as seeming to take the wrong branch or just segfaulting.
also suggests i wasnt going crazy when it happened way more often when trying to run in profiler, because maybe thats a context switch to take a sample?
so the solution to all that was just to use x17 as my scratch register instead of x18.

minimal reproduction without involving qbe at all:

```c
#include <stdio.h>
#define C 10000

int main() {
  int stomps = 0;
  for (int i = 0; i < C; i++) {
    long x = 1;
    for (int k = 0; k < C; k++) {
      asm volatile("b .bbb\n"
                   ".aaa:\n"
                   "    ret\n"
                   ".bbb:\n"
                   "    mov x18, %0\n"
                   "    bl .aaa\n"
                   "    mov %0, x18\n"
                   : "=r"(x)
                   : "r"(x));
      if (x != 1) {
        stomps++;
      }
    }
  }
  printf("stomped %d/%d\n", stomps, C * C);
  return 0;
}
```

for posterity this is on an m1 with macos 14.6.1.

## (Nov 3)

hoping the compiler constants thing also shows up in one of the tests.
setup script to diff them so its less painful.
yes! basic_libc:catch_signal does it alone, thats much more managable.
same symptoms: identical asm except for adrp+add offsets. one extra data symbol.
it's like the `handled_a_signal :: @static(bool) false;` is happening twice in the qbe version.
I guess one is IS_PANICKING and thats why it doesn't happen when i don't print something too.
add_constant is only called in walk_bc (not c/b/llvm or b/from_bc) and it happens for both llvm and qbe so its miscompiling the frontend.
oh its the deduplicating by jit_addr in emit_relocatable_constant so when its less than 8 bytes and stored inline, its deduplicating by stack slot,
and the one compiled by llvm happened to use stack space such that they were at the same place for both constants and it deduplicated them.
a bit alarming that it wasn't a problem for months but anyway we're back to reproducible builds of the compiler so thats good for morale.

oh hey i even noticed this problem before (macro_in_const_cloned test)
`// TODO: this doesn't work on llvm, presumably because bake value does the wrong thing? : FUCKED -- Jul 10`

the new backend still doesn't fully work, the occasional crashes were unrelated apparently, but at least now we know its not a
confusing miscompliation that only surfaces after multiple iterations, just a normal friendly miscompilation.

- typing f.rpo.offset(i)[] is gonna drive me insane, same for f.pred.
- with using init_default_module_dyn you finally get a speed up from fuse_addressing, around 2.45s -> 2.25s.  
  looking good for the idea of only adding optimisations that make it compile itself faster including the extra code than the old one did without.
- another peephole isel improvement: using madd saves ~10KB of instructions.
  not sure its worth it speed wise.

## (Nov 2)

farm_game is also different exes depending if using lq or lqq.
hoping thats the same problem as the compiler but don't thing so because this changes code and i think compiler is just different in constants.

```
3f30: d2800001     	mov	x1, #0
3f34: f2d33321     	movk	x1, #39321, lsl #32
3f38: f2e7f921     	movk	x1, #16329, lsl #48
3f3c: 1e270021     	fmov	s1, w1
```

vs

```
3f30: d2800001     	mov	x1, #0
3f34: f2c00021     	movk	x1, #1, lsl #32
3f38: 1e270021     	fmov	s1, w1
```

so its miscompiling emit:loadcon? no n is just different going in,
but the same in from_bc:inst_literal. so lost somewhere in the middle.
no, its different at the very beginning (`-d P`):

```

args 4596373777117347840
args 4596373778182701056
call $sdtx_pos__1050
```

vs

```
call $sdtx_color3f__1058
args 4294967296
args 5360320512
```

was because of setting f32 union field in push_literal and then reading back an i64 because when emitting i just care about the bytes.
that was interesting because it did make different binaries but both were correct compilations of the program.
because the difference was just wastefully loading the high bits of a register that you were about to truncate anyway,
so it could never be observed.

should decide what that should do.
maybe x = (i = 123) should zero the other fields but x.i = 123 should leave them unchanged?

## (Oct 31)

made the arm isel for the add instruction use an immediate when the right hand side is constant that fits in 12 bits
(instead of an extra register).
this alone makes the compiler go from 2,485k -> 2,089k bytes of code. thats insane.
runtime (safe) goes 4.52 -> 4.13 (but again some of that is just because the new version does less work, unfortunate that my only big program is the compiler itself.
it looks branchy but it generates so much less code that its faster even before accounting for it generating better code for itself.

in testing that, found confusing behaviour where it updates to my changes
without recompiling even when default_driver is using emit_qbe_included_dyn
instead of emit_qbe_included_sta so it should be using the version in the compiler.
oh! its because I'm setting function pointers in the target vtable in fill_target_arm
which gets called from the module initilizer in the driver program instead of within the precompiled part.
which is fantastic news because it also explains why there was a chunk of the backend code in the profiler that didn't have symbol names
(because they were getting jitted every time) and that chunk is ~11% of the total time which perhaps is about to get 5x faster by precompiling correctly.

using immediates for ldr goes 2,089k -> 1,737 and using for str goes to 1,407k.

those numbers are all before including vtable.init_default_qbe_module.

## (Oct 30)

Found someone on the internet to tell me how Mach-O does thread locals: https://stackoverflow.com/a/75297331

So debugging why it works when I let clang handle the offsets instead of doing everything myself.

```

_xaddr:
.word 3573752927
.word 2847898621
.word 2432697341 ; 910003FD
adrp x0, _x@tlvppage
ldr x0, [x0, _x@tlvppageoff]
.word 4181721089 ; F9400001
.word 3594453024
.word 2831252477
.word 3596551104

```

assembles to this:

```

0000000100003ebc <_xaddr>:
100003ebc: d503245f bti c
100003ec0: a9bf7bfd stp x29, x30, [sp, #-16]!
100003ec4: 910003fd mov x29, sp
100003ec8: b0000020 adrp x0, 0x100008000 <_xaddr+0x20>
100003ecc: 9100c000 add x0, x0, #48
100003ed0: f9400001 ldr x1, [x0]
100003ed4: d63f0020 blr x1
100003ed8: a8c17bfd ldp x29, x30, [sp], #16
100003edc: d65f03c0 ret

```

the important thing is that (3 words, adrp, ldr, 4 words) assembled to (3 words, adrp, add, 4 words).
becuase we just know that the `@tlv` means do something totally different from what the instructions say.
So my blindly doing the instruction encoding for ldr when its a thread local was wrong.
So when qbe outputs assembler text, it has a special case to use ldr instead of add if its a thread local,
and then the assembler has a special case to treat ldr as add if a thread local.
this all seems very sane and normal to me.

That's enough to pass the one qbe thread local test when outputting exe. still have to deal with relocatable obj.

## (Oct 28)

- `farm_game` doesn't work with new backend. asserts on `_start_canary` being nonzero after calling sglue_environment.
  i can't recreate the problem with just my language so its either we disagree with clang about size of the struct or its a calling convention thing somehow.
  oh heck, thats crippling. in from_bc:emit_bounce_fn i was treating the indirect ret addr as first arg (link it is inside a bc block),
  but it actually has to be a blit to a qbe return instruction. so it was just using whatever junk happened to be in x0 instead of x8,
  which i was the address of the whole SgDesc.
  but on the plus side the compiler now works on new backend now!
  at least for hello world.
- ok this is getting confusing. we need some notation.
  f_l = the normal one compiled by llvm.
  f_lq = compiled by f_l with the new backend
  f_lqq = compiled by f_lq with the new backend
  f_lql = compiled by f_lq with llvm
- the take away i think is that the new backend cannot compile itself but it can compile the old backend.
  but luckily for me the broken f_lq also fails tests. ie. `push_it`.
  so now i have two binaries for that test that should be the same and one segfaults, and i can diff them?
  thats creepy.
  the new one does

  - `cmp XX, #0, lsl #12` instead of `cmp XX, #0` for cbz
  - `mov XX, #YY; add	XX, x29, XX, uxtx` instead of `add XX, x29, #YY` for addr of slot

  which is interesting because f_lq is clearly broken since its different,
  but those instruction sequences do the same thing.
  ok thats still too big tho lets try something smaller.
  only one of the qbe tests fails with the broken compiler.
  oh shit which one changes tho. was collatz then rega1.
  so we've produced UB. thats fun.
  cry.
  `c.bits.i < 1.shift_left(12) - 1` is not at all the same thing as
  `c.bits.i.bit_and(1.shift_left(12) - 1) == c.bits.i`. (off by one. negatives are handled on different path anyway).
  and that was the only problem.

## emit mach-o reloc (Oct 27)

- string escapes, 1
  and that last failing qbe test is for thread locals which i haven't got to yet.
- pretty embarassing that in run_tests for qbe-exe i forgot to actually run the resulting binary...
  but they did still all pass so thats a good sign for the qbe tests being comprehensive at least.
- the my language tests are all exes that don't link something else,
  but i can still have them output a relocatable object and link it against nothing to test that i get the relocations right.
  failing 250/287.
- DataAbsolute relocs and track when data contains a local pointer, 52.
- add to local_needs_reloc in do_jit_fixups so you handle when code references data, 34.
- for data pointer reloc, the value in the data is added to the symbol address so it needs to be 0. all run_tests pass!
  that took so long to figure out.

## emit mach-o reloc (Oct 26)

- for relocatable object you can't have headers included in `__TEXT`?
- `ld: Assertion failed: (addr >= lastAddr && "function addresses not sorted"), function FunctionStarts`
  means your `__text` offset was high like it has to be when you include the load commands.
- single func relocatable. fail 28/58
- use correct offset, 23
- adrp+add import reloc, 13
- call reloc, 10
- skip `__got` data relocs because you just emit for each use instead, 8
- data segment and reloc when code references data, 5

## (Oct 25)

- print the most negative number
  - oh hey i even noticed this before `// assert_eq(MAX_i64 + 1, MIN_i64); // TODO : llvm backend chokes on this.`
- emit constant of a null CStr
- switch on u32
- aaa global_module is a different variable between the precompiled and the normal.

## emit mach-o exe (Oct 24)

- i was hoping i could use `https://lief.re/doc/latest/formats/macho/python.html`
  to start with a working exe and remove parts until it broke.
  but just doing nothing:

```

import lief
import sys
path = sys.argv[1]
app = lief.parse(path)
app.write(path)

```

breaks the exe. so either im stupid or they're stupid but either way it doesn't help me.
it seems they unset the N_EXT bit in LC_SYMTAB entries?
ahaaaa im dumb! its because they're signed so i have to run codesign -s on it after changing any bytes.
which means by previous experiments with editing the flags of a section to be zero and that breaking the exe,
didn't mean i need to have sections, it just means changing bytes invalidates the signeture.
codesign still isn't enough to make my program work tho.
`codesign -s "Luke Graham Landry" hello.o -f`
i hate this so much.
i know more than i ever have before. not a meaningful statement but makes me feel better. its fun because its always true.

// TODO: real ones have the file offset of `__TEXT` being 0 so it contains all the load commands too?
// and then section `__text` is just the code. do we need to do that? (we dont now). why would that be the case?
// ooooo maybe its becuase you want to declare a symbol `__mh_execute_header` so your program can
// reference that and read its own exe file like for debug info, etc.
ok so doing that ^ (without `__mh_execute_header`, just making `__TEXT` have file_offset 0 and include commands),
made it go from `killed` to `main executable failed strict validation` which is slightly better i guess.
and that was just becuase i was still doing the add_data_file_offset thing and redundantly adding the size of commands again
to all the offsets but now since its in TEXT its accounted for automatically. so thats an improvement i guess.
now its `bus error`. which suggests that its actually running.
yeah, i can run it in lldb now and it crashes trying to load from `__got` in a stub.

aaaaaa problem was you're not allowed to have a segment's address_size different from size???
that can't be true but it sure breaks when i change that.
maybe you're supposed to explicitly have a section that says the rest is zero filled?
no that doesnt work.
clearly im wrong because linkedit doesn't have page multiple size and doesn't have size==vmsize.

ok one mystery solved.
chained_starts_in_offset.segment_offset is actually the offset in virtual memory from the first intersting segment (end of `__PAGEZERO`).
before i thought it was the offset in the file.
its just a fun coincidence that normal binaries made by clang have thier segments packed together in virtual memory so those numbers are the same.
and this dump to text thingy (which i was using because i couldn't get objdump to give me the information i want)
`https://github.com/qyang-nj/llios/blob/main/macho_parser/sources/chained_fixups.cpp#L126` gets it wrong too,
and segfaults if you have your sections spaced out (unless you use segment_offset incorrectly).
however, `llvm-objdump --chained-fixups --macho` doesn't bother tryingto give you that information and doesn't crash
(it just tells you the imports table and shows you the page_starts without showing the rest of the chain).
oh and look at that they even have a comment with the information i needed: `https://github.com/llvm/llvm-project/blob/main/llvm/include/llvm/BinaryFormat/MachO.h#L1086`
if only i saw that 8 hours ago.

so recap of things we learned:

- `__TEXT` is required to have file offset zero (even if you don't define/reference `__mh_execute_header`)
- you need to run codesign.
- you cant have segment virtual size greater than file size unless file size is zero. you can space things out with gaps in between tho.
- segment size needs to be a multiple of a page, except for linkedit?
- you don't need sections except to make objdump disassemble the right places.
- you don't need LC_SYMTAB, LC_DYSYMTAB, UUID, min os version

```
import lief
import sys
path = sys.argv[1]
app = lief.parse(path)

import os

app.remove_symbol("__mh_execute_header")
app.remove_symbol("_main")
app.remove_command(5) # exports trie
app.remove_command(8) # uuid
app.remove_section(segname = "__TEXT", secname = "__unwind_info")
app.remove_section(segname = "__TEXT", secname = "__cstring")
app.remove_section(segname = "__TEXT", secname = "__stubs")
app.remove_section(segname = "__TEXT", secname = "__text")
app.remove_section(segname = "__DATA_CONST", secname = "__got")
app.remove_command(12) # function starts
app.remove_command(8) # build version
app.remove_command(8) # source version
app.remove_command(10) # data in code
app.remove_command(5) # LC_DYSYMTAB
app.remove_command(5) # LC_SYMTAB


app.write(path)
print("replaced exe.")
os.system('codesign -s - "' + path + '" -f')
```

## (Oct 23)

- damn. solved that in like 30 seconds this morning.
  each bl to an import was a different place. each getting thier own stub.
  that wasn't actually the problem yet but it would have been confusing.
  mistake was in init, i was make_segment in the wrong order (constant and mutable data flipped).
  so that makes mandelbrot work.
- for f_vtable, needed to fix rebase. target was using file offset to patch instead of virtual offset to target.
- cant run normally tho. killed.
  lldb helpfully tells me `error: Malformed Mach-o file`.
  i tried to open it in xcode's debuger incase thats different and can't even select the file which is funny. it knows its broken.

## mach-o loader (Oct 22)

is `__got` guaranteed have sequential chained fixups for all the imports?
maybe you're supposed to use that instead of doing the hacky some_extra_memory thing.
but if that were the case, you'd think they'd pre-setup `__stubs` to load from there instead of
needing to patch them which i think you do unless im misinterpreting.
oh yeah, i just got confused by the output of objdump.
example:

```

000000010000be2c <**stubs>:
10000be2c: b0000010 adrp x16, 0x10000c000 <**stubs+0x4>
10000be30: f9400210 ldr x16, [x16]
10000be34: d61f0200 br x16

```

it says its to `__stubs` but 0x10000c000 is the address of `__got` in that binary.

so now the loader it good enough to run itself and the compiler.

## mach-o loader (Oct 21)

- since im doing import patch in place before moving, the offset i calculated for adrp is wrong.
  then also had to make sure the fns array was close enough to the code that it could reference that far so mmap them together.
- whatever version of objdump is on my computer, -r doesn't show the entries in ChainedStartsInSegment.
  and i cant find an argument that does. `objdump --chained-fixups --macho` sure doesn't.
  i can only assume their goal is to make life as confusing as possible to reduce the competition.
  ah different program for some reason: `otool -fixup_chains`,
  or `dyld_info -fixups` also works.

## (Oct 20)

- caching the `primitives` slices doesn't make it faster so don't bother.

## (Oct 16)

- emit: for extuw, need to `mov	w0, w0` (with Bits.W32) to clear the top bits. even tho the class of the instruction is Kl because thats the output.

## (Oct 15)

- stop being lazy and make a macro to make else-if chains less painful.

## (Oct 14)

- improve tracy zones so you can actually see the dependency hierarchy.
  previously it was just flat because of how i unroll the recursion.
- add comptime resolve_overload and use it in format_into. 435->410.
- don't care enough to figure out what's making cranelift flaky.
  My own x64 supports the whole language now so i don't rely on it anymore.
  And somehow it's 730ms to run c_bindgen vs 530 for my old unoptimising and 120 for new qbe.
  so there's no point if its worse than doing nothing. (might just be heavily punished by not inlining anything but my old one doesn't either so that seems fair).
  tho calling franca_comptime_cranelift_flush less often gets it to 650ms so maybe im just forcing a lot of indirect jumps and being unfair.
  but thats still slower. i want to like it but it seems more trouble than its worth for me.

## (Oct 13)

- so long on wrong sp encoding for epiloge with large stack frame.
  should have been obvious because it was bus-error-ing between the last line of a function and the next line in the caller.
  (still cant run the frontend on new backend but it gets farther)
- get backend to compile itself. had to fix #redirects of i64->i32 because it treats those as different types.
- made walk_bc do callees first so it can inline intrinsics more reliably (and qbe gets things in a better order).
  that gets c_bindgen to 120ms. so thats like 30% slower than llvm, thats pretty damn good. with -unsafe tho llvm goes 90->40 but mine only gets to 110.
  so still a long way to go. maybe inlining things with branches would make a big difference. if just single block functions was a 115% improvement.
  presumably im also generating pathologically stupid but slightly bigger functions.

- renumber doesn't work if you have out of order constants because it just assumes they're from an upper scope.

## (Oct 12)

- can trivially recreate the c_bindgen problem with enum_names_constant.
  but it works if i use old emit_qbe to make the text and then parse that so its not my emit asm.
  it's the way i produce the constants in from_bc.
  unlike the other baked constant tests it still fails when run one at a time.
  ugh, in AddrOf i was putting its own id instead of the target id.
- the parser_doesnt_crash safety check fail is during compilation.
  in emit_func_arm64 on parse_block_until_squiggle.
  oh interesting, it's just more code for that test than fits in my 256k segment.
- using adrp for stuff and bigger segments also fixed the broken constants tests.
  so i was probably fucking up the offseting math with adr and in my too many hours fixing it got it right.
- did patching instead of the indirect table. its a lot faster (only noticable without inlining).
  trampolines if its too far because its imported from a dylib, which nicely matches what you have to do with \_\_stubs for aot.
  its just a bit of a pain for jitting becuase now that has to be executable so you can't just poke an address in like with the data table.

## (Oct 11)

- i think my adding indirection for direct calls really hurt. `c_bindgen sokol` on mine is 330ms instead of 260 with real qbe.
  but adding inlining of tiny functions gets it to 200ms on both. which supports the theory that i added call overhead.
- had to add RType as a pass through getinlref for when you inline something that contains another aggragate call.
- inlining fixes walk_dir and breaks bus_error_baking_constant/baked_constant_tagged_padding.
- TODO: mine messes up string constants. the word struct in c_bindgen is random gargbage.
  but that happened without inlining and doesn't for real qbe so its a problem with my asm emit.
  but perhaps thats also causing the new breaks in tests related to constants and the inlining just revealed a problem there.

## (Oct 9)

- ported test runner script
- working on inserting casts for llvm backend
-

##

- side tangent of i'm stupid. lets play can you spot the mistake...

```

fn println(i: i64) void = {
mem: List(u8) = list(i.div(10).abs().add(2), temp());
i.display(mem&);
mem&.push_all("\n");
print(mem&.items());
}

```

ok we're doing some printf(A) driven development.
that's where i decide the resolve_overload loops are too slow so i put a println("a") in there and count the lines, and then change something.
its like a sampling profiler but instead of being good it's inconvient and slow as fuck.
note also that I have to redirect the output to a file because my terminal clamps the output to 100k lines.

- when i infer_type on each arg of the pattern i wasn't savingthe finished tpye even if all were successful.
  so if an overload was never taken i'd try to reinfer it every single time you look in that overload set.
  so now save it in finished_arg if all were known.
  before 5000k, after: 23k. amazing. 470ms -> 440ms
- comptime zeroed() gets to ~430ms but can't do that until i can do float comptime calls more reliably.
  ah but i can cheat and do it by pointer.
  saved half the samples in coerce_const_expr (in real profiler)

##

- so long on a `:=` vs `=` in a loop. perhaps i was wrong.

## (Oct 6)

- pretty ironic to not be able to incrementally port abi.c because functions pass structs by value and my old backend doesn't follow the abi correctly.

##

- setup default_driver test and its always failing the third one it tries.
  heck it was just getting confused by compiling twice becuase i pasted wrong in run_tests, 15.
  and then i can just recreate the module every test in default_driver too for now and that works.
  and after that i can add back the redeclared symbol check and it doesn't fire.
- from_bc gives me the inline asm, 8. and that's just as many as passed with aot.
  but in 4 seconds instead of 11 seconds. there's gotta be something wrong with clang's assembler.
  (and thats still with the new backend running jitted on the old unoptimising backend so it should get faster with release mode).
  ~200ms of that was emitting the unneeded asm text.
- fixed minimal_inline_bc not setting arg_prims on its block
- the loop cost stuff is creepy because the program still works if you get it wrong. its purely an optimisation.
  so finally did shitty padding for formatting so i can make exactly the same output as qbe when it does `%s-10` and check it that way.
- spill mistakes:
  - typoed extra & of a pointer passed to a function and apparently i don't typecheck that sometimes, thats a massive problem.
  - wrongly redeclared a variable
  - translated i++ to i+1
    // bf_interp fails with mine! tho not different answers. it segfaults in the compiler.
    // and thats the only one. so thats strongly in favour of going and finding more tests.
    // and not getting rid of the old backend, just turning it into an example so i have it as an extra test because its such a large proportion of the code i have.
  - problem was le vs lt in sort.

## (Oct 2)

- you can't ask the linker to adrp you to a dynamic import? that makes sense, the loader puts it somewhere random and you have to look it up in a table.
- had adr encoding high low flipped
- ok that worked much better than expected. franca tests are failing 57/289. and i can still run in text mode that mostly works.
- put jit addr for constants in the table, 45
- started doing emit constant properly and poking in references, 33

## (Oct 1)

- (big imm, cset, 37), (float ldr/str, 35), (slot ldr/str, 34)
- off by one in big imm loop, 31
- much pain with sp encoding, 29
- ugh stupid mistake where i patches the asm bytes in memory but for im still emitting the text unbuffered so all my oforward jumps were brk, 24
- (rem/urem, 21), (swap, 19), (int ext, 16)
- aaahhh im so dumb. jumps were randomly off by 8 or 16 bytes sometimes because i was just emitting text for calls so the offsets in my "jit" code were off, 10.
- sometimes objdump is ass and you have to say -D instead of -d? what? and they're the same, why doesn't mine work.
  ohhh its ones that happened to have a format string data before the code and i wasn't putting out a `.text` directive after so it switched me to data,
  and then objdump throught my code wasn't something it should try to disassemble, and the test failed because it wasn'tmarked executable.
  ok thats fair, my bad ig, 7.
  oooo! and thats also what was making most of my franca tests fail (cause main was at the end after data) so now thats 255 -> 27 too. amazing.
- painful float cc, 5 + 27.
- cast=fmov, 2 + 23
- a billon d/s<->l/w s/u, 1 + 12
- fixed emit ordering for Thr fixaddr isel, 0 + 12
- sltof, 0 + 10
- 0 + 8
- filluse first try.

## (Sep 30)

- intermediate step so i can still use thier test scripts: do the asm myself but emit it as `.word xxxx` for an external assembler.
  also lets me delay dealing with patching calls which is nice.
- flipped a few of my encodings when the arg registers are interchangeable because its easier if the disassembly of mine matches what real qbe does.
- (math/load int/copy, 50), (get global addr, 47), (load args backwards, 46), (call, 45), (cond jmp, 41)

## (Sep 29)

- TODO: fix opsies when trying to match on a non-sequentual enum. fails safety check
- pass: instruction selection
- wasted so much time on a flipped condition in `imm`
- implemented sign extension properly on my old arm backend becuase i need it for interacting with qbe's 32 bit integers.
- added ability to run qbe's tests and caught blit mistake related to ^

##

- off putting that i accidently flipped the conditions in simpl div

## (Sep 26)

- store has no result type so the instruction must have .Kw
- forgot to set nunion to 1 and it caused a 0 opcode somewhere. took so long to figure out but can't really complain, like yeah thats my bad, don't do that.
- i was still using qbe parsing for TestRunnerMain so at the end, it would free my `typ` and thats why only the first test case would work every time.
- a bunch of `error: assembler local symbol 'Lfp0' not defined` because i wasn't calling `target.emitfin` for it to output float constants it made in the backend.
- insane behaviour where it looped forever (at runtime) if you tell it store returns a value instead of using QbeUndef.
  which again fair enough i guess. my builder should warn you about that. im surprised typecheck doesn't.
- made TestRunnerMain return 0 which made more pass. i guess many tests just happened to leave 0 in x0.

## (Sep 25)

- my c_bindgen emits both fields as the whole backing type for bit fields. that was confusing. TODO: fix // :BitField

## (Sep 24)

- so like its gotta be https://github.com/llvm/llvm-project/blob/3fbf6f8bb183ad8b9157e50c442479f4ca7a9b8d/llvm/lib/MC/MCDwarf.cpp#L630
  and clion warn try to manually select a file that the old name was "" so that matches.
  but where the fuck is it getting that from. the empty string does not appear in my ir.
  clang sets `source_filename` at the top level which i don't but that doesn't fix it.
  At least i can use `objdump -d -l a.out` to check if its working instead of running it in lldb every time.
  ah problem was my DISubProgram didn't have `file` field... so whats the scope field for then??? because thats also a file.
  TODO: write a test with that objdump thing that you still get debug info.
- the compiler works on qbe which is pleasing. still useless because clang assembling qbe's output takes so long,
  but its really nice to not rely on llvm as our only way of doing an AOT build.
  (runtime of output code) qbe is 5x slower than llvm release mode.
  oh shit i take back everything bad i ever said about qbe.
  targetting x64 clang can assemble its output in under 380 ms.
  compared to arm where it takes 3900 ms which i was complaining about.
  assembly size: x64=5.3MB, arm=8.2. so like its bigger but its not 10x bigger!
  what could clang possibly be doing.
  so for x64, qbe+assemble takes 30% longer than llvm debug mode but runtime is twice as fast.
  so thats like a totally reasonable option.
  I should really make a table of times.
- considering hooking into qbe and replacing the parser with just generating thier in memory data structures,
  and the assembly emit with my own machine code emit. i find thier code incomprehensible so want to understand it,
  working on making c_bindgen not choke on thier anon enum struct fields.

## (Sep 22)

- made most of the wasm tests work again, at some point it started generating calls to memcpy. hack: just had rt provide it.
  it must have been the change from calling memcpy to defining one the takes advantage of alignment but you'd think that would have the opposite
  i guess i don't know what `@llvm.memcpy.p0.p0.i32` does.
- (bf2bc) fixed the arm encoding problem when IncPtrBytes is negative so now i can disassemble it but it still infinite loops
- ah printing something in the loop changes its behaviour so it must be a register allocation bug?

## (Sep 21)

- always use shims for jitted function pointers.
- added a new type of inline asm comptime function where you just fill in my bytecode format.

bf2bc

- needing the loads/stores for phis is annoying
- to work on llvm i needed the entry block to have the right arg types
- llvm cares about i64 vs i8
- llvm doesnt jsut compare to 0 it wants 0 or 1 exactly.
- works on x86/llvm/qbe but not arm

## (Sep 20)

out of ~500ms, resolve overloads of `if`, `eq`, and `display` are each 50ms.
so the root problem is my linear scan overloads thing, but can probably make it better by not instantiating those
inside generics as much.
I got rid of `::if_opt(T, bool); ::if_opt(T, T);` in fn Option and now if takes `25ms`.
So like, its dumb to create a situation where im deciding how to write code to make it compile faster,
but also its so easy to make the standard library stuff not pessimize speed so maybe thats worth it.
The `display` is probably becuase of ::enum which i use a lot but don't print by super often.
I bet `eq` would get a lot better if DeriveEq could create the sub-functions it needs for fields without
adding them to the list you scan every time you type `==`, because there's a bunch where i only manually call the top of the hiarchy.
Really i just want to be able to have generics that don't add to overload sets until you need it.

- eek! noticed i had added get_build_options to the vtable twice. add error for conflicting field names.
  same for function args because it feels weird to just have the later one shadow.
  TODO: but for function args i should allow multiple `*` if you just want to discard.
- shims for better errors when missing libc

## (Sep 19)

- update qbe backend, implement switch, make walk_bc usable from driver, enable -aot=qbe in default_driver.
- fixed stupid typo in flags for open_append/open_trunc
- actually show lex error reason
- implemented stack arguments on x64 when you run out of registers (>6 ints).
  i still don't actually implement the c abi correctly because you're supposed to pass structs in xmm registers sometimes.
  also since then some of the functions have progressed to having >8 args which the aarch64 backend can't handle.
  maybe i should just make you use a struct. or maybe slices should be passed by pointer if there's too many.
- fixed silent exit(1) if you hit an error at top level of the driver.
  TODO: figure out how to print more info since you don't have the compiler instance at the point where you decide you want to panic on it.
- make resolving by type without an expression less insane.

## (Sep 18)

- added syntax for short-circuiting `or` because its dumb to only have `and`.
  kinda nice that you can use it like zig's `orelse` operator.
- fused a few operations in sema so its less clunky to interact with values of known type.
- started trying to call get_or_create_type everywhere so i can get rid of tagged indices
  because it spiritually annoys me that i have to encode larger immediates all the time,
  and im pretty sure its never caught a bug since before i got rid of the interpreter.
  can't quite do it yet. maybe im missing some but also im a bit afraid of my hashing Values by bytes.
  did get rid of tags for scope/overloadset/funcid tho.
  there's also some `==` that don't take self parameter and use the constant void/Never/etc.
  ah i was just missing an iteration that removes the tag bit in get_or_create_type.
  so that works now and my indices are small.
- made the primitives stuff a bit less painful by hackily having secret (P64, P64) before the array so you can just offset backwards for `#ct` and indirect return.
  its slightly less ugly than the old thing, and less code, and saves 10ms self-compiling. :ConfusingPrims
- made the parser more strict about semicolons after declarations.
- finally fixed field_ordering

```
// : FuckedJunkPointerExpr
// it doesn't work on linux if you don't do this 
// but it was fine on macos arm and x64. 
// what the actual fuck
// and like... this is the only thing
// changing this made it self compile... and reproducible build. 
// so like this line is the only thing in the whole compiler that is different on linux.    
// inline_match: the need for this (in addition to normal match) when self-compiling was added in "or syntax, remove tag bits, etc"
case_access = case_access.expr.Block.result[];
```

## more more linux (Sep 17)

added ability to ask an allocator if it owns a pointer so now expr log doesn't crash on trying to print garbage
if its not in the compiler's areana or on stack. so i can see the place where the garbage number is.

// : FuckedJunkPointerExpr
tried run_tests with cranelift on linux but `parser_doesnt_crash` still hits the `bad expr ptr` (before the cranelift type error on that test that happens on mac).
its definitly using cranelift because it passes the tests that `[TODO X64]` normally.
that seems pretty conclusive that the problem isn't my x86 asm backend.
so i guess i don't need to bother setting up compiler/first to make sure it uses cranelift for jit.

// :ClearCacheHack
hmmm inline_asm_jit:manual_mmap tries to call uncompiled on real linux but works on blink if i smuggle uname.
thats concerning. its not just -static because that still fails, tho with different args to the uncompiled and after trying to link `malloc` which it doesn't on blink.
blink definitly is running the x64 code, it breaks predictably if i change the numbers there.
the non-static linked problem where the args are pointers at least is x86 glibc not giving you an empty `__clear_cache`.
and the static linked one (idk if its from musl or zig) has a noop impl of that.
and i wasnt falling back to smuggled libc if found a dynamic one but it didn't have the function.
but my binary dynamiclly linking glibc has a static copy of a noop \_\_clear_cache,
so i guess its fine if you lie and say it exists at comptime because the linker magically fixes that....
idk, the non-hack solution would be have `#target_arch` like `#target_os` (rn you can only do that with asm),
but until there's cases i need that other than this one function + unimplemented asm tests, i don't really want to bother.

> self compile can't cope if the smuggled uname uses the other os type from the compiler target because it tries to redeclare the symbol in the llvm ir.
> for now i just hack around it with manual #redirect.

I broke `-unsafe -keep-names` but only on arm so its probably a `__clear_cache` thing.
I really need to start running more combinations of tests in github actions.
but.. only if i call `0.rawptr_from_int()` instead of `rawptr.uninitialized()` as the ignored argument in alloc.
TODO: should fix the root problem

also last commit needed boot update becuase i moved ast_alloc to the compiler.

TODO: can't dlsym `dlopen` from the glib .so file, i assume they have it sepereatly because you also have to pass `-ldl` to clang to link (but not zig cc because its magic and knows).

## more linux (Sep 16)

Fixed the looping on `_NS*` functions on linux.

// : FuckedJunkPointerExpr
now when compiling itself its segfaulting on 590072672032938 in compile_expr_inner.
i feel thats not even a pointer becuase it doesnt change if i turn aslr back on but all the other numbers passed into
compile_expr_inner do.
bindings.len == 1 case of decl_var_pattern
this is the one that crashes on linux and its the first time it runs
this is valid but its subexpr Block.result is a garbage pointer.
and the block loc is `case_access = @{ @[case_access]& };` so thats compelling that it would be a strange one.
ok good it cant do tests/front.fr or requested_type_through_match either.
so its not something crazy about self-compiling, its just @match is broken.

i went through and fixed readdir stuff so now run_tests works on linux.
hmmmmm now requested_type_through_match passes. but still not front or the compiler itself.
but the number is different, and its different between compiler and front but then consistant within one.
589312462821369, 826016700493633. and the compiler one is still very close.
10000101111111101000000000000000100001011111111001
10111011110100001000000000000000101110111101000001
it doesn't have the fancy bit set for one of my indexes so its not just reading 64 bits instead of 32 from one of those being treated as a pointer.
lox has 698683805039472. the consistant per program + compiler binary is creepy.
i can recompile and add a check if its exactly that number and it fires.
I can `@println("comptime: %", FatExpr.int_from_ptr(case_access.expr.Block.result));` in macros.fr and its not the same number.
hmmmm, run_tests.fr has the same problem and just happens to not try to dereference that subexpr??
it only crashes if doing my debug printing the tag in the compiler.
hmhmmmrmrm lox works now... well later crashes for a different reason.
so im just like doing UB then, its not a sane mistake.

the compiler still generates exactly the same llvm-ir when run back to back so confusion about its behaviour isnt as easy as uninit comptime memory.
tho running on arm and x86 give different (same target).
oh thats just it calling get_comptime_arch in go_build_yo_self.
fixed that and now arm-macos and x64-macos generate same code for linux which is reassuring.
all 3 targets can build identical bf.fr which is extreamly pleasing.
interestingly, lox build is not reproducible (even on the same system)...
which is the program that tends to be super fragile about freeing a garbage pointer...
what a coincidence that its the one that has a comptime pointer in the ir...
and its build is reproducible on my linux where i turned off the aslr.
it was the null_rule_addr constant becuase i called int_from_rawptr so it didn't know it had to be relocated. thats a footgun.
now its reprodicible on all 3. alas too good to be true, its still unreliable to actually run on linux.

forgot to do X86AsmBytes (the compiler itself uses X86AsmText because it was done before inline asm was implemented on x64)
so llvm-aot tests didn't work on llvm. that fixed math_ops/branching.
the ones that do a comptime arch check becuase they're not implemented yet don't work because im cross compiling.

did: i want to hack in using compiler impls instead of libc so it can run in blink and see if same problem.
yes same problem.

## linux (Sep 15)

so a thing that was sketchy was calling allocators with 0 lenght,
adding a zero check and just returning null in the general alloc broke the compiler,
made it unwrap in poll_until_finished.
oh probably the resetting temp before any allocations and i just happened to get lucky with a zero length one before.
yeah, just had to fix coconut.jpg.
that wasn't the actual segfaulting problem, it just surfaced when trying to debug by changing allocators.

oooo lox test also fails on linux in a vaugely similar way, thats nice.
but i think its differnt :( cause it changes if i don't free in libc_allocator_fn but compiler doesn't.
tho... only if you remove a few tests, just not calling drop still crashes.
the only libc functions it calls are (abort, write, memcpy, munmap, mmap, malloc, free).
I already did different mmap flags number 34 (linux) vs 4098 (macos).
i feel like the others can't be different.

its super suspisious that the place in the compiler that's crashing is tuple_of
where it was being super confusing a long time ago
(i think it was when i tried x64 with rust).

aaaa fucking stack alignment, same place as my old problem god damn it.
and i saw it using the 16 byte load on the instruction it was faulting on `movups	xmm0, xmmword ptr [rbp - 480]`.
and in blink it even gave the right faulting address but real linux said 0 and i assumed i was just confused somehow.
aaa.

now looping trying to get `malloc` but that makes since becuase i hardcoded the macos libc path.

i think musl dlopen can't cope with glibc .so? fair enough i suppose.
gah, no they just dont do that if you statically link.

- https://www.openwall.com/lists/musl/2012/12/08/4
- https://musl.openwall.narkive.com/lW4KCyXd/static-linking-and-dlopen
- TODO: i bet zig doesn't hate me personally, try their thing https://github.com/ziglang/zig/blob/master/lib/std/dynamic_library.zig#L14

glibc mmap gives invalid file descriptor for -1??
strace says
`mmap(NULL, 1048576, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_EXECUTABLE, -1, 0) = -1 EBADF (Bad file descriptor)`
ooooohhh they have different numbers!!! MAP_EXECUTABLE instead of MAP_ANONYMOUS...
how can that be i thought mmap just did a syscall?
with musl in blink i have the number right
`I2024-09-15T17:47:06.685846:blink/strace.c:778:52190 (sys) mmap(0, 0x1000000, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) -> 0x2168409d0000`
oh but sometimes i have it right `mmap(NULL, 16777216, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7fffe63de000`
so is that a calling convention thing or maybe that one was done by the libc not my code.
jit.fr always mmaps writable and then later mprotects to exec, so im never doing that on purpose.
and `MAP_EXECUTABLE` isn't the same as `PROT_EXEC`(the former says `This flag is ignored.`).
hmmm `MAP_EXECUTABLE = 4096` so like its gotta just be calling the macos version instead of the linux version in #target_os.
fuck. yeah i forgor that i `comptime_os: Os = .macos,` dumbass.

todo: it might not find dlopen because its actually `dlopen@@<some insane garbage>` like apples's opendir.

## (Sep 14)

- ok the thing where mmap was failing in qemu must be because MapFlag.Anonymous needs to be 32 instead of 4096.
  and also (not the problem yet but will be) uname struct is a different size, so its really not just the args stuff,
  i need a general system for different impls on different targets.

there's gotta be a better way than typing

```

mkdir [mount point]
mount -t 9p -o trans=virtio share [mount point] -oversion=9p2000.L

```

every time, i tried the `/etc/fstab` thing but it didn't seem to work.
how the fuck do i make it not reinstall every time.

## linux (Sep 13)

ok i know `_NSGetArgv/_NSGetArgc/_NSGetExecutablePath` wont exist but i should be able to get the rest to work.
compiling with `-target x86_64-linux-musl` to make a .o file and then using `zig cc f26bUYh.o	-static -target x86_64-linux-musl`
(becuase idk how to put musl somewhere clang can find it),
it also complains (`ld.lld: error: undefined symbol:`) about the assmebly functions `arg8ret1__1000/arg8ret2__1001/arg8ret_struct__1002/arg8ret1_all_floats__1003`,
i tried this guys gcc https://github.com/FiloSottile/homebrew-musl-cross, same thing.
ah thank you internet https://stackoverflow.com/questions/26051946/problems-compiling-assembly-file-error-undefined-reference-to-function-name
you just don't put the `_` prefix on linux.
manually changing that in the llvm ir makes it only complain about the `_NS*` as expected.

## more more more x64 (Sep 12)

- float add/sub/mul/div is very sane, no comment, they must have added it later.
  tho im not sure if im supposed to be using the VEX versions that im avoiding because i don't want to deal with another encoding.
  im not remotely at the level of optimisation where a false data dependency on the top of the simd register would be measurable,
  so its probably fine for now...
- x64 CVTSD2SI rounds to closest instead of towards zero like the arm one does.
  the internet says "When the conversion is inexact, the value returned is rounded according to the rounding control bits in the MXCSR register."
  i'd really not have magic global state that decides rounding behaviour.
  looking at cranelift's disassembly (which does the rounding i want), it says `cvt_float64_to_sint64_sat_seq %xmm7, %rsi, %rax, %xmm6`,
  which... drum roll please... doesn't exist 🎉🎉.
  googling `cvt_float64_to_sint64_sat_seq` has one result and its a cranelift test. https://cocalc.com/github/bytecodealliance/wasmtime/blob/main/cranelift/filetests/filetests/isa/x64/fcvt.clif
  ah of course, https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/codegen/src/isa/x64/lower.isle#L3680
  silly me, i want cvttps2dq not CVTSD2SI, but no thats a vector, the second t is trucate so cvttsd2si? hehe ye https://www.felixcloutier.com/x86/cvttsd2si
  why do we even have an instruction for "eh fuck it whatever rounding the last guy wanted is probably fine".

## more more x64 (Sep 11)

- x64 jump if. setcc and load byte don't zero the register (TODO: there's a move with zero for the latter at least).
- 2227 but unreliable. lox test fails most of the time, but i've seen it work.
  and it almost always works if you run it alone (and never fails the way it does when run through examples).
  but i think its cranelift getting stressed out by a lot of functions existing?
  becuase if i comment out the part where i flush_cranelift after every time i use my backend, a bunch of tests fail
  (with uncompiled as expected), but then the lox one works every time.
  and that matches my previous impression of it getting more fragile the more you call finalize_definitions,
  (and i was calling it more now that more used my backend).
  and now that most use mine, flushing after using cranelift is more reliable.
  so i choose to belive this will fix itself when i can stop using cranelift all together.
- switch, 2255
- div. they make this a massive pain. it always outputs to rax, and puts the remainder in rdx (which would be nice if i exposed div_mod).
  and the divisor has to go in rax but its 128 bit with the rest in rdx so you have to sign extend if you just want 64 bit division.
  2263
- in_reg unspill, 2291
- load/store I32, just set w=0 and then its the same as I64.
  2891
- bit shifts. can only use rcx as the shift amount, because they hate me personally i guess.
  2956
- load/store 16bits. 2967
- call without link. 3800
- trunc. 3903

## more x64 (Sep 10)

- 683
- fix dumb load bug, was using addr twice.
- 885
- enabling stack_to_ccall_reg for more args breaks in new_arena_chunk but mostly works if you add a debug_log_str call.
  so at least thats a classic spilling bug.
  yeah missing a ensure_specific_free.
- 1284
- allow indirect ret
- 1329
- CopyBytesToFrom (simple version where its always a call)
- 1376
- intrinsics: int cmp (with setcc), nop zext/int<->ptr.
  i'd messed up the condition codes because i didn't have a check for declaring conflicting enum values.
  and then in the encoding test it picked the first name (because that matched the index) so it thought it was fine,
  and just ran redundant tests.
- 1657
- intrinsics: int add/sub/mul
  mul is insane and can only use rax but imul is all i actaully use for now.
- 1736
- CallFnPtr 1764
- bitwise and/or/xor only 1771

## (Sep 9)

- x64: setup stack frames. indexing down from rbp this time. had to go back and get rid of u16 everywhere.
  attempt load+store.

## (Sep 8)

- working on calling objective-c to make a window with the raw macos apis.
  very very confusing time with strange warnings about passing the wrong flag values but with numbers i wasnt using.
  problem was that i was passing a struct of 4 f64 as a pointer to memory instead of in 4 registers. (arm).
  because i can't read apparently and assumed the calling convention was the same as ints (only reg if <2).
  so that pointer was being seen as the first flag int and it was using whatever garbage happened to be in the float registers for the rect.
  note: its not an objective-c thing, i have the normal c-abi wrong.
  :float_struct_cc still need to actually fix that.
- x64: handle calls to known address with no args/rets.
  jump distance is from the end of the instruction! so you need to know how big the instruction is when writting the offset.
- working on :trivial becuase it annoys me.
  for functions like List/Slice, it doesn't realize they're just a value because thier body becomes noops.
  changed the criteria for a block result replacing the whole block so it does it for noops nbot just empty.
  and do it even if that block has a return label if the result is just a value since it can't possibly early return.
  so now i think at least it doesn't actually call them,
  but it still jits them because to compile the body it suspends on Jit.
  i was hoping i could just catch those trivial ones in emit_bc_and_jit and not bother since they'd be snatched anyway but it didn't make much difference.
  it was like 100 not 600 and it didn't make the 600 go down.
  i guess you still end up with calls to Option which isn't .SyntheticImmEval or .ComptimeOnly.

## (Sep 7)

- store PrimSig in a seperate array from the bc so the instructions are less chunky.
- fixed not checking switch_payloads when deduplicating and made a test for it

- such a painful time on -spam now tries to get_info too soon so i didn't notice that the error was different when i tried to debug it.
  real problem was just an extra == .Cranelift check but uuuuughghhgh im stupid.

- did the plumbing to get fuctions jitted with my own x64 backend.
  currently any functions that just push constants and return 0/1/2 (no args, no vars, no calls, etc),
  are done by me and the rest are given to cranelift.
  kinda embarrassing that my:cl is 617:3391 already (when compiling the compiler). so 15% of the functions i jit are trivial.
  :trivial

## (Sep 6)

- discovered the modrm/sib tables aren't actually that much information

## tiny cleanups (Sep 5)

- store a vec for debug loc of each instruction on BasicBlock
- move a few fields only needed during building from FnBody to EmitBc
- don't store ?TypeMeta, have is_sized field instead (padding was already there), save tag space. TODO: would be nicer to have niches in the field padding.
- track name when assigning type to constant so can print ie. `EmitBc vs FnBody` insatead of `[50 lines of garbage ommited]`.
  still pretty big message because it tells you all the arguments to a call instead of just the problem one.

> i might be going crazy but... mandelbrot test runs faster in warp if i spam scoll down !!!!!!! i love it???
> perhaps relates to my discovery before with bloat running objdump faster when i compiled in release mode because i could empty the pipe faster

## (Sep 4)

- infer arg type for #x86_bytes function
- x86 branch encoding jit examples

improving some error messages.

- added parse error for lone identifier instead of treating it as beginning of quick expr and then getting confusing sema errors.
- fixed losing error in resolve (missing @try) so you always hit a GetParsed in sema and reported the wrong location.
  should make driver programs powerful enough to add warnings for that sort of thing.
- overload not found error show the types
- fixed missing type check error for get_variant_ptr `if you pass a pointer as $t it still tpyechecks !!! : FUCKED -- Jul 16`
  I was passing arg_ty instead of arg_expr.ty for arg_ty_found of bind_const_arg.
  // TODO: really immediate_eval_expr should do more strict type checking.
- returning more sturctured errors so i can write tests for them, and so i can delay the slow formatting the error message until we know its actually going to be printed.
  this is gonna take a billon years, sema still does `@err` 133 times.
  started with what seems like the most common ones (ExpectedCompileError, TypeMismatch, CoerceConst, InvalidField).

wrote my own json lexer for c_bindgen instead of using wuffs.
processing time for c_bindgen sokol is ~70ms for wuffs and ~95ms for mine (both aot).
the json.fr for both versions is the same amount of code tho (not counting bindings/wuffs).
mine doesn't do much error handling or support string escapes (tho my old couldn't cope with escapes either).

// TODO: examples/llvm_mc_repl.fr so i don't have to use the python thing
// TODO: example image viewer to use an actually interesting part of wuffs because i still think it's cool
// TODO: nicer imports system. json.fr shouldn't clutter the namespace, most programs won't use it.
// #file_scope, better namespaces in general.

## (Sep 3)

- allow `:=` for default with inferred type on first field of sturct
- init all default fields with `()` literal
- allow empty struct when you just need to implement a trait.
  which is kinda dumb but its even dumber if you can't do it.
- trying to clean up building the compiler.
  ok the problem statement is that when repl wants to depend on the compiler it has to copy paste a 70 line driver program.
- `Tried to call un-compiled function` when coerce const overload set to runtime fn ptr (but not fid -> fnptr).
  just needed to call compile_fn_ptr.
  that got running repl on jit a bit farther but its still calling uncompiled.

started x86_64 encoding
//! The more fun game is `from pwn import *; context.arch = "amd64"; print(asm("mov rbx, rcx"));`
//! and then slowly figure out how to map that back to the tables.
//! important to note that it prints normal ascii things as characters instead of in hex because we like confusion!

```

from pwn import \*
context.arch = "amd64"
def show(s):
print(" ".join(("{: 8x}".format(x) for x in asm(s))))
print(" ".join(("{:08b}".format(x) for x in asm(s))))

```

- expand macros in imm_eval without function context (@FnPtr is common).

also (with new using -unsafe) its now faster than the old compiler (~520ms vs ~580ms).
safe its still 780 tho which is sad.

- replacing if with @if in a few places that get instantiated a lot makes SemaOverloads go from 10k to 6k.
- the step where you do type_of seems to never make progress if the type is funcid so can skip it. that might be a big fragile.

## (Sep 2)

it feels like i should be able to get cranelift passing everything on aarch64 before dealing with x86,
but it seems super unreliable. a different subset of my tests fail every time i run.
ocassionally all except multistack work so i suspect thats the only one that's actually broken.
i must be doing something wrong.
A common crash is this
https://github.com/bytecodealliance/wasmtime/issues/8852
"I just noticed that the assertion failure is unrelated to StructArgument. [...] I'm not sure when this assertion fires though."
but thats not the only one.
i don't remember it being this flaky before tho.
maybe calling finalize_definitions on every function stresses it out.

debug mode revealed `FunctionBuilder finalized, but block block_ is not sealed`, which i can fix with builder.seal_all_blocks at the end.
still super flaky.

in other news, added back zero_padding which fixed the easy case of reproducible builds (run the compiler twice back to back and the checksum of the binary should be the same both times).
// TODO: should add a test for that

## cleaning up

- update go-build-yo-self to create v2
- remove padding in ExportVTable
- make enums in driver_api zero indexed
- move Flag (fixed ident) out of driver_api and removed unused values
- stop tagging things #compiler, remove some now unneeded functions.

## faster! (Aug 30)

the old one was ~580ms safe. at the very least i should be able to match that unsafe.

(times are -unsafe).

another like 10% is the old resolve_in_overload_set because every single macro call goes through resolve_by_type.
inline_cache takes us from 900ms to 800ms.
can do a hash table in resolve_in_overload_set_new and get to somewhere around 770-730.
but its kinda clunky because you need to be constantly checking if you know all the types for the parts and then creating a tuple of them.
and its hacky because i can only get it to work if i don't add to table when any part.is_const.
becuase of the coercion stuff i assume but it feels too fragile.
so im just doing the simpiler inline_cache that makes macros not crippling and takes like 10 lines of code.

misc, got to around 750.

theres a whole seperate tree where its doing check_for_new_aot_bake_overloads thats 10% of the time.
thats just a bunch of extra work thats flat with number of types in the universe when doing AOT, not with number actually used.
so instead just collect from the overload set up front but only compile+jit once you actually need to emit a constant of that type.
doing custom constant bake functions lazily got to ~680.
and the whole emit_bc is now ~5%.

that one was a bit less satisfying because this fix would speed up the rust version too.
but it was a pain to do back when everything was rust and i didn't want to just turn off the borrow checker.
so maybe it counts as an advantage of this langauge.

but anyway thats like 17% slower than the old compiler. getting there.
the correct thing to do would be fix whatever the crazy exponential overloading problem is thats taking half the time,
but i kinda want to see if theres any other low hanging fruit first.

## it compiles itself! (Aug 29)

its like 3x slower.
the first number i remember was ~2200ms safe.

with -spam you can see `decl_var as_int%100600` 71 times. so i think a done flag on FatStmt will help a lot.

oh damn, turning off debug assertions goes from 1800 ms to 1580 ms, thats the combination of saved time and not having to compile them.
using that to compile a version with debug assertions + indexing takes ~1650 ms.

done flag on FatStmt gets down to (unsafe 1400, safe 1780) and brings spam output from 4396386 lines to 2033850 lines.

fixing .TookPointerValue (unsafe 950, safe 1300). my hashtable of integers is really fucking slow.

##

seems you no longer need this.

```

// If you don't do this at all, you loop (TODO: why? gets stuck on EvalConstant:i64).
// But if you @check it, you'll error out on things that need a type hint.
// We do a real typecheck after dealing with the const args so its probably fine.
// TODO: compiling it at all here (check or not) has to be wrong tho,
// because you don't want things in a const arg expr to be added to runtime callees.
\_ := self.compile_expr(arg_expr, arg_expr.ty.want());

```

a couple mistakes in the compiler code that were incorrectly allowed by the old sema. thats kinda cool.

- `self.put_token((BinaryNum = (bit_count = bits.trunc(), value = total)));`
  with value wanting u64 but total being i64 used to work without .bitcast() but shouldn't
- `out.push((Num = (i, .I64)));` where `Num: @struct(value: i64, ty: Prim),` used to work but you should need field names.

---

- forgot to call get_or_create_type for u8 in eval_str

##

remove some unneeded sema_regression changes.

- (macros, bits) didn't used to need the annotation.
- (mandelbrot, mul) this should be able to be #inline and still get its type hint.
- (panic, panic) i used to just call unreachable() here but now i can't cope with mutual recursion
  even when fixed, this should still actually abort() because if the hook returns we're in a fucked state. -- Aug 6
- (arena, ::) old sema didn't need the type annotation! its picking the wrong overlaod now
- (hash, StupidAddHashable) used to work without `: Type`
- (run_tests, run_llvm) shouldn't need to be constant :runner_segfault

## debugging on x86 (Aug 26)

v2;
so tuple2 is getting the right type args but it segfaults if i try to print types.len so comp_ctx ptr is garbage?
yeah the hack number im passing to emit_bc_and_aarch64 is not the one i get out, but it is on arm.
ooooooooo kkkkkkk now it seems to be working....
maybe i didn't recompile comptime_cranelift for x86 since i changed how comptime_ctx_ptr was passed...
i feel like i did.... but i must just be super dumb.
but anyway now i can run mandelbrot on franca2 x86.
v1 doesn't work still but that makes more sense because it has to deal with matching rust's c abi.

---

v2 can't `run_tests -- cranelift`. maybe `DirEntType.File` has a different value on x86 macos libc?
no `clang libc_constants.c -target x86_64-apple-darwin` gives the same results and file says `a.out: Mach-O 64-bit executable x86_64` so it seems to have worked.
but my basic_libc walk_dir test works on arm with v2 but not x86.
different DirEnt layout? seems like no.
but cross compiling `franca examples/default_driver.fr test tests/basic_libc.fr -aot=llvm -x86` also doesn't work so thats definitly the problem.
`println(entry.name().len());` gets garbage lengths.

what the fuck?? ok so i can write a c program that uses readdir_r and works, but if you look at the llvm ir clang produces,
its calling `readdir_r$INODE64` on x86. on arm it just calls `readdir_r` like you'd expect.
and sure enough if i manually edit the ir i produce and call `readdir_r$INODE64` it works on x86.
how the fuck am i supposed to know that??
oh damn if i google that string its just a thing people know,

- https://www.reddit.com/r/Compilers/comments/fuosy2/psa_link_name_for_readdir_on_64bit_osx_is/
- https://github.com/rust-lang/libc/issues/414

and if i look in my dirent.h

```

int readdir_r(DIR _, struct dirent _, struct dirent \*\*) \_\_DARWIN_INODE64(readdir_r);

```

and then in cdefs.h

```

#define **DARWIN_INODE64(sym) **asm("\_" **STRING(sym) **DARWIN_SUF_64_BIT_INO_T)

```

and

```

# if \_\_DARWIN_64_BIT_INO_T

# if \_\_DARWIN_ONLY_64_BIT_INO_T

# define \_\_DARWIN_SUF_64_BIT_INO_T /_ nothing _/

# else /_ !\_\_DARWIN_ONLY_64_BIT_INO_T _/

# define \_\_DARWIN_SUF_64_BIT_INO_T "$INODE64"

# endif /_ \_\_DARWIN_ONLY_64_BIT_INO_T _/

# else /_ !\_\_DARWIN_64_BIT_INO_T _/

# define \_\_DARWIN_SUF_64_BIT_INO_T /_ nothing _/

# endif /_ \_\_DARWIN_64_BIT_INO_T _/

```

we need to appritiate reddit bro for a moment

```

// https://github.com/peterdelevoryas/mylang/blob/master/src/llvm.rs#L52
for func_decl in &module.func_decls {
let lltype = type_bld.func_type(&func_decl.ty);
let mut name = func_decl.name.deref().to_string();
name.push('\0');
let mut link_name = name.as_ptr() as *const i8;
if cfg!(target_os = "macos") && name == "readdir\0" {
link_name = "readdir$INODE64\0".as_ptr() as *const i8;
}
let llfunc = LLVMAddFunction(llmodule, link_name, lltype);
llfuncs.push(llfunc);
}

```

thats funny.
ok so this is just how life is i guess.
now i need to make a better #import that lets you set a link name.

---

now back to trying to get v1 to work;
changed the name of the self hosted lib file so it doesn't collide with the rust one and that got rid of `ld: warning: ignoring file 'target/x86_64-apple-darwin/release/libfranca.a[19](libfranca.o)': found architecture 'arm64', required architecture 'x86_64'`
but that didn't help.
if i compile the rust with `TRACE_CALLS = true`, it panics on `Expected function to have scope` instead.
so thats interesting.
tho arm fails a safety check with that so maybe its just all fucked.
maybe x86 doens't have the same field alignment rules? but sizeof(Func) is 448 on both.
actually maybe that happens to be sorted to not have obvious padding.

```

pub fn main(){ #[cfg(target_arch = "x86_64")]
println!("sizeof(A) = {}", core::mem::size_of::<A>())
} #[repr(C)]
struct A {
a: i64,
b: bool,
c: i64,
}

```

on rust playground says 24 tho so that seems to be normal.

- make_and_resolve_and_compile_top_level print stmts, they seem reasonable
- printing names of functions as they're added also panics on `Expected function to have scope`.
- compiling rust debug instead of release makes it `assertion failed: func.get_flag(NotEvilUninit)`

so like everything's just confused. and it changes if you try to observe it.
feels a lot like the instruction cache flushing problem i had a long time ago but the whole thing we learned there x86 doesn't do that.

---

cleaning up some todos

- (run_tests) @assert
- (mandelbrot) fix += with new place expressions.
- a bunch of places with hardcoded exe paths from when i used execv instead of execvp

## supporting x86_64 (Aug 25)

x86 inline asm because the compiler uses it for comptime c calls. just passing a string to llvm for now.

- magic incantation `.intel_syntax noprefix` from https://stackoverflow.com/questions/66532417/how-to-inline-assembly-with-clang-11-intel-syntax-and-substitution-variables

bringing back cranelift because i want the compiler to work on x86_64 but i dont want to deal with it yet.

- int intrinsics. failing 50
- switch. failing 35
- fcmp. failing 23
- float casts. failing 11
- clone stack before each case of switch. failing 1, but its the runner_segfault that always fails.

fixed runner_segfault on old.

now to make it actually run on x86 i get the thrilling job of hunting down everywhere i was sketchy with abi stuff.

v2 dying in tuple_of after a call_dynamic.
v1 dying in platform_mmemmove after hoist_constants

- put_constant taking by value instead of ref. that didnt help but probably will eventually.
- call_dynamic_values took a slice as an argument. converted to ptr, len.
  that moved the problem, only for v1 not v2, which makes sense.
  now v1 isn't crashing, it just thinks theres no overloads for bootstrap_compiler_environment which is kinda worse cause you don't know where to look.
- make_and_resolve_and_compile_top_level, unquote_macro_apply_placeholders, insert_owned, and tuple_of took slice arg. didnt help

## hack overloading some more (Aug 24)

symptom: `arr: Array(i64, 3) = init(@slice (7, 8, 9));` choosing the overload for `Array(i64, 5)`
It works with the arg being i64 but not []i64 or \*i64,
so i think its about suspending in the argument type.
so the answer that fixes this is to not continue after infer_arguments suspends in resolve_in_overload_set
but that makes it segfault on run_tests.fr

can make it work by taking out hte block where we infer args or continue which should be fine because you do it one arg at a time later,
but that makes compiling run_tests llvm path segfault.
also broke overloading_cycle which i guess im less attatched to cause the old one couldn't do it.
and apparently the new one only could by randomly guessing.

---

aot emitting forwarding functions for redirects brings llvm to parity with aarch64.
still kinda feels like a hack becuase i didn't need that before.
but its certainly nicer if compilation order matters less

## repl (Aug 23)

have to do some fixing up of the tree to make runtime vars work nicely.
you cant just compile into a function because you don't have all the statements that might want to reference your variables.
you can't just make a new function, append the statement, and rerun every time you get a new one because what if the print or access the file system or something.

## gui (Aug 22)

initial gui for introspecting the compiler.
lets you click through functions, overload sets, and constants.
not super helpful for me yet because you can't use it if there was a compile error.

## fixing aot on new sema (Aug 21)

- resolve named args. mandelbrot works now. can't do a single named arg yet.
- (old) problem after making neg #fold because i write fmt_hex for llvm before i had a shift_right_logical so i was using div and it only worked when the high bit was 0
- implement check_for_new_aot_bake_overloads
- forgot to call created_jit_fn_ptr_value based on TookPointerValue in dispatch.
- TODO: deal with execv vs execvp

> failing 11 + 175

now problem is it doesn't fold and thinks we need GetCompCtx. like calling size_of in arena.
Was only folding when `!self.dispatch.enclosing_function.is_some()` to prevent recursing but thats not what you want.
in old sema you could just mark expr.done because you know you only get there once but now i have to be able to suspend.
skip fold when we're in a function made by imm_eval helps a bit.

> failing 9 + 174

but now llvm not failing on GetCompCtx, tho its still `!!!! failed to emit speciai function: size_of` which is a bad sign.
but the actual error is i64 vs ptr of last argument to new_arena_chunk, passing a const null pointer, so thats a `.None`.
so the problem is my new contextual_field of tagged becoming a value instead of a structliteralp, and then emit_bc gets the wrong prims for it.
adding :tagged_prims_hack to emit_relocatable_constant_body fixed a bunch. mega ugly but progress.

> failing 9 + 32

always fold functions created by imm_eval so you never add them as a callee to a runtime function.
hard to think about and broke the `floats` test because of a now missing coerce_const?
but fixed a bunch and type errors are less scary than an incorrect callgraph.

> failing 10 + 24

now the problem is with redirects.
i can make it better by if the target is special, handling its body right there and pulling forward the body if its an intrinsic.
breaks the first case in `deduplicate_functions` test tho.
I think things aren't always getting added as callees correctly?

> failing 11 + 14

## fixing overloading (Aug 20)

- rust program with wasmtime and some libc shims to run most tests as wasm.

> failing 32

HACK.
just say no type hint for the first argument when resolving overloads.
thats good enough to not incorrectly coerce a constant and then fail to match on a runtime second arg for the binary arithmetic which is what it all ends up being.
eventually i'll need something more robust but its not like the old version worked super well either.

> failing 26

regression add a @as for `mask: u32 = 1.bit_not()`. since thats a single arg, im now skipping giving the literal a type hint,
and then since i don't have i128 anymore i think its a negative number and don't let it coerce?
also wierd ness about if my new thing is doing #fold enough.

> failing 23

had a TODO when you suspended after removing some of const args and then circled back.
for something like `get_field_ptr :: fn($S: Type, self: *S, $f: *Field) *f[].ty #generic`,
you'd replace the first argument and suspend on the second.
arguments are removed from the cloned function as you go because later ones might refer to the bound values,
but we leave the arg_expr alone until the very end.
so now when you try again, you know how many args have been removed from the function, and just skip over that many constant args before doing more processing.

> failing 17
> failing 15

exec_task.Jit calls copy_inline_asm fixes all the #asm tests.

> failing 10

## procrastinating fixing overloading (Aug 19)

- track stack depth so more nested early returns work
- struct field default values, contextual fields
- allow DeclVarPattern with no name because @match can make them
- fnptr -> rawptr coercion
- llvm target wasm. hack around 32 bit pointers in constants (i still just pretend everything is 64 bit and pad const arrays with zeros).
  js shim of a couple libc functions to run mandelbrot in the browser.

## debugging new sema (Aug 13)

variables declared in quote expressions (like in @match) don't get renumbered (also didn't in old sema),
so new ones are seen as the old type if you lookup in the global thing what the type of the variable is.
i don't understand why it worked before since it was using the same `get_var_type`.
wait, nvm, unquote_placeholders renumbers at the end, so i was just logging too early (inside the @[] expr),
and seeing the template var names. So the new one is indeed getting new vars each time.
ok but the new one isn't renumbering all the way into the switch expr,
the `callable` is always referencing the old `arg_ptr` somehow.
which again i don't understand because the old one also calls into the self hosted renumber_expr.
The problem was not cloning the arguments of unquote_placeholders so the first renumber would
mutate the template of a quoted expression was used in an unquote, and then subsequent renumbers wouldn't see it. :double_use_quote

##

format_into.
commenting out the expr.done shortcircuiting works so we know its something wrongly being marked done.
the problem is that im leaking a loss of dispatch.enclosing_function somewhere.
hackily setting it back every compile_expr call lets it get further. still need to track down the actual mistake.

## (Aug 6)

- crippling bug with `expr[] = (Tag = self.box(expr[]));` where you have to make a var for the self.box(...) or it segfaults

##

- thinking about how to do #generic without forcing you to mark it and letting the expressions compose better.
  need to move argument typing to the bind_const loop. added a ResultType that holds a tuple so you can have partially known args and pass that down.
- but first, tuples with fields should be easy.
- back to consts. so just delaying infer_types and doing it one arg at a time as enough to make it work.
  so the remaining problem is that you still have to precompile the argument
  because you want to know its type in const_args_key to check if its a tuple, but that can just be optional.

## debug info (Jul 31)

- lldb doesn't see it. oh, need to connect the !SubProgram with the functions?
  `inlinable function call in a function with debug info must have a !dbg location`,
  complaining again is good i guess. adding that just segfaults tho...
  maybe its https://github.com/llvm/llvm-project/issues/92724
  thats the function its crashing in tho it doesn't show up multiple times in the stack trace,
  idk if that means its not recursing or they just don't want to spam you.
  oo i bet this one https://github.com/llvm/llvm-project/issues/59471
  yup, you just also need it for DILocalVariable, same segfault.
  so now its back to the state of `warning: no debug symbols in executable (-arch arm64)`
  ok that goes away if you put a real filename in the DIFile.
  it doesn't have to exist, you just can't have the directory be an empty string or it swollows it silently.
  so now in clion, i can see the functions in the stacktrace have my nice names instead of the mangled names.
  i can't see the variables tho. but `frame variable` in lldb lists them (which it didn't before), so its just clion that hates me now.
  in lldb if i break in main and then step i can see the value of the variables.
  i told it pointer so i just see 8 bytes always but i can see `max_steps = 0x000000000000002d`
  which is 45 which is correct. so the value there is whats behind the pointer, not the pointer that i passed to dbg.declare.
- more new sema work
  // Since were evaluating in const context, any functions that are called in the expression weren't added to anyone's callees.
  // So we want to say we need to recompile the expression, adding to callees of the lit_fn we're about to make.
  // I think this is not enough, and we need to deeply clear done?
  // so really we just shouldn't have tried to compile outside a function context! -- Jul 31
  // it works without clearing done here if you remove :the_compile_above, but you kinda want to do that for GetVar on a const
  // i guess that should just be handled in quick_eval and just deal with the occasional redundant work.
  // changing that should make us more robust to "ICE: Tried to call un-compiled function."
  removing the compile attempt brings lit_fn up to 24 but adding a quick_check for already compiled constants brings
  it back down to 5 so it's fine.

## tiny tests on new sema (Jul 29/30)

- got it to the point where it can load a test file and use the existing parse/scope/bc/asm systems.
  so thats very pleasing because now there can be a quick feedback loop as new features are implemented.
- addr/deref
- call zero arg functions, needed to track callees in the new dispatch system
- early return

## Switch (Jul 27/28)

Added a Switch ast node and bytecode instruction for @match to use instead of a chain of ifs.
The thought is that llvm has a switch instruction and maybe it would be faster if i just told it what im doing,
instead of it having to do work to figure out my chain of ifs.

It seems llvm takes about the same amount of time, but it does make my part go from 455ms to 425ms, which is interesting.
I guess we've learned that my thing where blocks are functions that then get inlined so my old match was kinda copying out this
whole linked list kinda thing and then collapsing it all down again, was dumb and if i just have list of expressions its faster.
(note: it doesnt seem to mater which version you're compiling with, its which version you're newly compiling, its just a library change in @match).
I have mixed feelings about adding redundant things, like it would be conceptually simpler if you just have ifs.
but also 5% for like 80 lines extra (once i remove the old @match impl) is pretty damn good.
and it kinda is easier to think about if you ever have to look at the ast for debugging the compiler if it matches more closely with whats actually happening.

- @match on enums (not only for @tagged-s), down to 415ms.
- maybe I'll try making @if at least not expect closures so insert the call in the code instead of in the compiler.
  that takes it down to 405ms. @loop remove extra lambda makes it 395ms. and those changes were nice because they deleted code.
- TODO: qbe inst_switch, use Switch node for @switch

## (Jul 25)

- ask the compiler which safety checks are enabled. i have some bug with order of compilatio nwhen disabling debug_assertions on the compiler.
- finished porting bloat.fr from size.py
- fixed fork_and_catch to poll the pipes so it doens't block on big outputs.
- discovered how fast you clear the pipe is the main thing that determains how long it takes to disassemble.
  bloat.fr running objdump on the compiler takes 430ms jitted and 230ms llvm-Os.
- wrote a generic example driver program so its easier to just run random programs with different backends.

## Admiting intrinsics are special (Jul 24)

The currently basic math like add is defined as just functions with inline assembly.
Which i think is cool because it means you can click into operators and see what they do.
It also forces making the inline asm language feature good because its tested for absolutly everything which is good.

But its perhaps noteworthy that almost every real language doesn't do this, perhaps they know something I don't.

- It makes a weird dependency so some of the inline asm stuff needs to make sure not to do anything fancy
  because most stuff can't be compiled yet (which is why I need two versions of @bits).
- It's got to be slower that i have to compile that stuff for every single program. Like its not that much work,
  but if it were just already done it would be free.
- If you break anything along that path its a nightmare to debug because you can't really make a minimal test case because there is no action you can perform.
- I end up typing out a bunch of dumb strings (llvm ir for example, instead of just templating in the one operation).
- It generates dumber code. My asm backend doesn't know you can add any registers other than x0 and x1.
  It gives the llvm backend more work to inline it all probably, like its a super easy choice to inline the one add,
  but surely it would be faster to have it already be done. I wonder if im wasting some optimisation juice on that.
  I wonder if assembling the output of qbe is so slow because it wasn't expecting to resolve that many symbols,
  and is tuned to most instructions actually being instructions, because otherwise i don't see how it could possibly be slower than llvm doing optimisations.
- I want to get rid of the @BITS vs @bits distinction, having a file that looks like user code but you actually can't edit without
  careful thought about internal compiler implementation details. is a bit silly.

So far converted int operations. Didn't make assembling much faster. Still worth it tho.
Maybe im having this guy's problem and clang just sucks at being an assembler `https://github.com/llvm/llvm-project/issues/68445`.
Older clang takes 1.5 seconds instead of 2 seconds which is interesting...
i was hoping for actually gcc

```

$ gcc --version
Apple clang version 14.0.3 (clang-1403.0.22.14.1)
Target: arm64-apple-darwin22.2.0
Thread model: posix
InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
$ clang --version
Homebrew clang version 18.1.6
Target: arm64-apple-darwin22.2.0
Thread model: posix
InstalledDir: /opt/homebrew/opt/llvm/bin

```

I also tried real gcc and thats 1.4 seconds

```

$ /opt/homebrew/Cellar/gcc/14.1.0_2/bin/gcc-14 --version
gcc-14 (Homebrew GCC 14.1.0_2) 14.1.0
Copyright (C) 2024 Free Software Foundation, Inc.
This is free software; see the source for copying conditions. There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

```

So thats enough faster that it makes qbe just slightly faster than llvm.

## Playing with Qbe (Jul 23)

- discovered one bug with llvm tests failing grouped but passing individual.
  when entry points get deduplicated, i don't make shims or call the deduplicated version.
  TODO. but also im pretty sure that was happening before too.
- XProtextService sucks infinite cpu. thank you internet the magic incantation is `spctl developer-mode enable-terminal`
  and then in settings you can see the developer tools tab in privacy/security and then you can add other things (like other terminals),
  and then they're allowed to run new programs without being crippled. that takes running the tests on the llvm backend from 5.5 seconds to 1.7 seconds.
  so thats nice.
- i bet qbe never passes an opque thing in regisers even if its 16 bytes.
  yeah, saying pairs are a `{ l l }` made the temp function stop touching x8 (indirect return).
  that fixed it jumping to null when trying to call through alloc vtable.

```

qbe bug? docs say im supposed to use fake types for calls but this loses the argument.
making it w works. but without -NDEBUG it hits an assertion so maybe the docs are lying?
Assertion failed: (v->t[0] == T.rglob), function spill, file spill.c, line 507.
yeah apparently just use w for everything.

function l $trivial(){
@start
%v1 =l call $something*important(ub 123)
ret %v1
}
.text
.balign 4
\_trivial:
hint #34
stp x29, x30, [sp, -16]!
mov x29, sp
bl \_something_important
ldp x29, x30, [sp], 16
ret
/* end function trivial \*/

```

- hangs on test `buckets` but its something wrong with my fork_and_catch

## (Jul 21)

- made typed ast nodes to replace all the SuffixMacro ones, which i like more cause there's less junk code about decoding the arguments,
  cause you just already know the answer.
  but sadly its slower to compile itself. (and also broke the lox test, todo).
  i wonder if the slower is just because of removing #compiler_builtin_transform_callsite so there's more indirection.
  old 390ms -> new 430ms, 10%.
  yeah cause making single arg if function call macro instead of the two arg function brings it to 400ms.
  so extra const arg function expanding really hurts.
  doing @loop directly in fn while gets me back to 390ms.
  so i guess thats good enough and then change is fine now, but its a bit of a bad sign for my avoidence of builtin things,
  that implies it could be similarly 10% faster if I just had syntax for `if` and `while` like normal people instead of most code going through an extra function expansion.
  lox was fixed by adding a semicolon at the end of a loop body so it didn't try to infer the type of the last expression.
  because now it wants to typecheck before noticing its a loop.
  which is a bit yucky that i changed behaviour but probably fine, dont really care.
- made it a lex error to have a string literal split across multiple lines
  so typos don't have completely incomprehensible errors
  where you can't find any later functions cause they're all in the string
- fully got rid of 'quoted' syntax in favour of @{ quoted }

## deduplication (Jul 20)

- it seems pretty effective. first attempt was ~17% less ir and made llvm take ~8% less time
- trying to do it for jit too made aot try to call non-existant functions. i thought i had to complicated seperate redirect tracking,
  but just adding the redirect to pending in llvm/emit_special seems to work.
  i don't trust it tho, i think it just happens to work because i don't have a test that ends ip calling something in jit and
  then needing to include it as aot but needing to emit differently but the body has already been replaced with the redirect so you can't notice the problem.
- TODO: i reintroduced sometimes broken const temp areana base_size i think?

Moving type storage to self hosted.

- a bunch of pain about when you have to extend_options on the TypeExtra
- typo-ed @tagged starting at size 8 and fold(max) on fields instead of 8 plus fold(max) on fields
- speed is the same it seems? generates ~8% more llvm ir (cause theres more code in my language now) and takes ~8 longer to compile itself.

## Jul 19

- temp alloc had the wrong base_size because baked constants weren't including tagged padding.
  didn't matter cause its just virtual memory but thats one less lurking bug.
  while debugging that found another missing \_\_clear_cache when calling into the compiler for dyn_bake.
  at some point i should just give up and write an interpreter.
  tho i could also try to do something fancy where every time i return or call a function that might be jitted,
  check the target address against the mmapped code segment and only clear cache if its in there and new or something.
  i guess returning should never be a problem? how can you be returning to something that wasn't compiled yet.

// TODO: (maybe) instead of storing TypeId of vars, store size+align.
// then don't throw away fnbody after use, keep them in a hashmap to deduplicate generics.
// like List(\*T) will often generate the same code even with different types.
// can keep a running hash as you construct the bc so maybe pretty fast to discard non-matches.
// have that as a build option so you don't have to take the hit for debug builds if you don't want to.
// but it actually might make it faster in general because it means giving less stuff to llvm which is 90% of the time.
// I kinda want to wait for emit_bc and bc_to_asm to be self hosted tho cause then its less awkward to change things.
// Note: you wont get all the matches until you really deduplicate because they might call different functions that actaully generate the same code,
// so have to have a thing like redirects where if checks if a call has been deduplicated. -- Jul 5

Did the first part of that, now vars are just size + align.
That's also kinda nice cause it decouples the bytecode from the rest of the compiler more,
so it might be easier to try to import wasm or something to bytecode as a way to test that backend eventually?

## debugging ported aarch64 (Jul 17/18)

- its a little too easy to typo a `:=` inside a scope. maybe should think about that.
  or at least warn if a variable is shadowed and also unused.
- had an offset by -4 not changed to -1 now that i use `*u32` for instructions.
- ok we segfault in bootstrap_compiler_environment. trying to call debug_print_int as the first thing does run
  but prints twice and the second time is the wrong number.
  and it sanely prints once when not using the new backend so its just totally messed up somehow...
  and im like doing a lot of calls with the dispatch table but the old version had direct?
  oh im doing every call twice because i had `return;` and didn't call it in one of the cases in branch_func.
- aaaaaa i bet im just miscompiling the code that does the inline assembly and thats why im getting garbage.
  because i changed the brk number at the end of the function and turned off BASIC_ASM_INLINE and the op function im crashing on
  was emitted by the rust code which means it was a copy_inline_asm. ok thats reassuring.
  can narrow it down by finding one function that doesn't work and using the old asm for all the rest.
  slice eq. hangs forever. if i debug_log_int(i) in the loop its always 0.
  heck. in the old version my ldr_uo/str_uo took a u2 size so the could be used for 8/16/32/64
  but in the new version i just called the ones that took Bits (u1, for 32/64) but still passed the u2.
  so what we've learned is i should fix type checking for my little types, not just allow casting any i64 silently,
  and then start using the little types more throughout the jit code.
  and have range checks. i long time ago i took them out from shift_or_slice because you got junk reading a u16 as a i64 or soemthing,
  and i just masked it off. perhaps i should be more strict about just letting seemingly harmless bugs exist in the universe.
- ok now all but 3 tests pass using new asm for all functions.
- huh found a place in the old one that used register_type instead of Bits.X64 for add_im on a pointer which is wrong but happened to work apparently
  thats scary
- typoed a put_jitted_function in flush_callees, saving it to `f` instead of `from`. that fixed 2/3
- funny that the only failing one is write_empty_module which i added yesterday. lucky i did that i guess. again... scary.
  if i hadn't got bored of this being broken and done something else it would have been silently broken for longer.
  ha, the compiler works with new asm, so its just this one test.
  tho now i cant make the test work with the old asm and it seems like jsut a missing overload instantiation... maybe im going crazy....

// this is slower than the old version. i suspect because of \_fold_shift_or,
// i can see in objdump llvm doesn't inline it away and its calling brk,
// which is doing loops and shit when it could just be a single or.
// need to do more of the work in the macro like i did originally so it just expands out into math that's easy to optimise.
did that ^. now the function for the emitting brk is only 4 instructions... which is never called because llvm always inlines it.
fantastic.

## Jul 13

- converted my comptime ffi call asm to my language.
  can compile itself but comptime_float_call fails.... but not in the RustRover debugger, unless no_fork... so thats fun.
  thats really creepy, i just had the registers totally wrong.

##

```

[
{
"label": "Franca Compiler",
"command": "cd /Users/luke/Documents/mods/infered/compiler && franca first.fr && clang++ /Users/luke/Documents/mods/infered/target/aarch64-apple-darwin/release/deps/libfranca.a target/libfranca.o -o ../target/release/franca_new && mv ../target/release/franca_new ../target/release/franca",
"description": "compiler",
"use_new_terminal": false,
"allow_concurrent_runs": false,
"reveal": "always"
}
]

```

## Jul 9

- self hosted emit_bc is about 3% slower (170ms -> 175ms) at building the compiler.
  note that to be fair you have to build the non-self hosted version with the self hosted version
  because there's also just ~50% more code now.
  and pleasingly it seems the front end times are approx. linear so far.

##

// - did: @match/@switch on enum names so you don't have to say @case in switch.

## Jul 6

- flow result type through function literals + constants so you don't have to put type annotations on @match to use dot field of enum/tagged.
- fix bad index when multiple constant args in #generic

## Jul 5

- stopped hardcoding lib path
- figured out that llvm will keep function names if i don't mark them private.
- made const lookups use a hashmap. made fn find (in scope.fr) go from 19/105 to 0/82 samples.
  need one in every block, not just every scope or you get the wrong answers.
  really smarter would be only start using it when theres a few constants because i suspect all the win is from the top level where all the code in the universe exists and its all constants.
  so i just make it not be n^2.

## Jul 4

- something corrupting the tag when trying to return a `Result(Str, Str)`.
  Making it `Result(Ty(Str, i64), Str)`, kinda seems to fix it but no now the payload string is disappearing.
  Hmmm, my destructuring was leaving the pointer on the stack an extra time, kinda distressing that so much worked like that,
  but easy to fix. I suppose since its a statement, you mostly don't care because you're not flowing into an expression, you just end up with extra junk at the end.
  TODO: test that catches that mistake.
  That also fixed `// something super sketchy happens if you try to use destructuring here. its way slower all the grouped ones are failing but all the singles still pass. -- Jul 1`.
  so thats reassuring.

## Jul 3

- caching slices by addr saves 47/369 KB on libfranca.a (with -O2) (including emit_debug_info not using cache yet, tho with trace off so might not be included by llvm).
- interestingly -O2,-Os,-O1 are make frontend time all approx the same and they're all ~6.5x faster than -O0.
  but -Os makes a binary ~17% smaller and saves about half a second of llvm running on my thing.

## Jul 1

- made maybe_direct_fn handle it being a Label gracefully.
  remove resolve_function because its a copy paste of maybe_direct_fn, it just only handles named constants which isn't what you want.

> I think its bad style to call something that dispatches when we know which branch it will take,
> it feels harder to think about because the other implementation might drift.
> // TODO: go through an see if applying this elsewhere makes it more clear?

##

- forgot to clone after expanding PendingParse cause not going through the rust side anymore.
  symptom was confusing getting same value every time for Option(T)'s arg.
- now deep enough stack that it tried to free the stack trace's constant memory into libc free. had to not do that!
- the self hosted scope now works but makes it 5-10% slower maybe. need to give llvm function names so i can use a profiler.
  i wonder if its just that the scope stuff uses a lot of hashmaps and my hashmap is 150 lines and [rust's is 4000](https://github.com/rust-lang/hashbrown/blob/master/src/map.rs) (not including comments!).
  which feels like they could be doing a lot better than me.
  tho if i make sure ids are unique i could make it a vec and just use way more memory because of constants that aren't vars.

## self hosting feature flag (Jun 23/24)

problem with running parser on llvm is pool returns ?u32.
currently i say thats {i64, i64} but it tries to create one from {i64, i32} which doesn't typecheck.
cause i return small enums by tuple not bytes.
need more complicated resolution of which prims to use based on the varients.
easy cases are when the same or all but one are unit or all ints or all floats but then need to upcast sizes.
only matters when theres one or zero cause otherwise you pass by reference.
mixed ints/floats make sense that you can't do cause it probably wants to pass in registers and then you cant know which are real without checking the tag which is hard to think about.
rustc just uses `[i8 x _]` in that case which i have to match anyway for now since i need to call between the languages.
mainly just a hack solution that's good enough for ?u32 (which is an easy case), would be fine for now.
when i get more serious have abi tests that compare to rust's repr(c), but thats a little dumb until i stop forcing i64 for the tag.

- i think calling convention for span is messed up, for now hack returning as tuple of i64 on rust side
- have to use non-null pointer for empty vec.
- hard to move include_std handling to scope.rs because parser isn't used anymore. eventually want to move it to driver.
- forgot to add flags to pool in init was just doing on parser test
- forgot to lex float.
- args needs to not just read expr for type after : to handle :=
- capitialisation of flags enum so the values match the rust one
- off by one for binary literals
- if_empty_add_unit so overload arity didnt match (first thing it hit was fn ret).
- horrible special case @return -> `__return`

that seems to make it work on first??!!! fucking great success.
not tests tho? `Undeclared Ident assert_eq`.
its printing `=== Aarch64 Jit ===` so its getting into the driver.
wanst adding back the stmt if it had annotations but ended with semicolon when parsing block
which i now use for top level stmts so wasn't getting it for injecting include core.

- fixed lex float
- oh one problem with release mode is the debug typeid bit but i hardcode them in the self hosted parser based on whatever it was compiled with.

removed magic `@return` and now return rebinds every `=` function and `local_return` rebinds on capturing functions.
which is still a bit confusing but probably better.
but i achived that without using the old parser, so thats a good sign that its at least somewhat safe to remove.
it was definitly more fiddly than it would have been before self hosting.
some confusion about needing to rebuild an extra time because driver_api (which you need to change to add shared Flag value),
is built into the rust exe. so should make that just a normal part of the compiler? hard to think about.
I guess you want included like now but the self hosted compiler build against the new driver_api, not its own compile time copy.

Added coerce_constant for fn->fn_ptr. A bit of a hack because you want to insert !fn_ptr instead of just mutating the value,
because you want to delay needing to create the assembly of the function. using an expression defers it and lets the backend do the fix-up.

I'm doing this to eventually get rid of !macros so its easier to parse 'if !cond' but also
its pretty dumb to not implicitly cast a constant function to a function pointer.
It's not ambigous and it does the type check.
Similarly, most other uses of ! are left over from when my type system wasn't expressive enough and i needed lots of hooks into the compiler.
The few that are actually magic (!if, !slice) I can make into @macros now that i have better bootstrapping system for them.
Those are really thier own ast nodes, so perhaps I should treat them as such.
`if` you always call through the function so its chill.

## the parser grind (Jun 21/22)

- stmt var decl
- tuples, struct literals
- calls, dot calls
- assign, update-assign
- macro, quote, unquote
- trailing lambda
- prefix type ops, bang macros, bit literals
- fn exprs and stmts

## debug x86 (Jun 20)

fucking wikipedia lied to be.
system v x86 cc does NOT replace large arguments with a pointer (which sounded reasonable cause thats what arm does),
it just copies them onto the stack. oh an also does it right to left apparently.
this one knows the truth https://www.uclibc.org/docs/psABI-x86_64.pdf
and godbolt agrees.

i think when farm_game x86 was working before its because i was still using flat calls for macros specifically, and sokol always passes large args by pointer.
cause i can still compile on arm, produce llvm, and then cross compile that for x86 and it works
(other than warning about overriding the module target triple because i hardcode it in the ir but activity monitor says the exe is Intel so its fine).

thats much more of a pain in the ass to deal with cause i have to know the size of the things behind the pointer.
i was so confidently like hey i dont need libffi it doesn't do much for me but maybe arm is just simple.
aaaaa i dont want to do itititititititi. fuck.

i also have to start giving llvm byval attributes on args so i can do x86 ffi.

## (Jun 19)

- have an overload set for changing how constants get baked so don't need a special case `__save_cstr_t`
  to tell the compiler that it needs to walk until the null terminator when deciding what to put in the aot exe.
  TODO: should have slices and lists use that too. lists don't need to keep thier unused capacity
  and should change thier allocator maybe? (its a problem if something tries to call free on readonly memory).

## parsing (Jun 18)

- self hosted math ops parsing and ast logging.
- allow `for x {| .. }` without the extra brackets
- sometimes convert struct types to arrays when large and all same types.
  should do more stress tests for weird generated code, like my swift parser generator problem.

## give up and use llvm (Jun 15/16/17)

- empty block. oh but also same label twice. switched to the block before checking if already did that one.
- they have pair struct literals but only for constants so you have to painfully insertelement to return a pair.
- all the ones where it was just silently crashing was because i forgot to do tailcalls and was just trapping because emitter was supposed to have returned

// :PushConstFnCtx
// make sure things that get called at compile time don't get added to the call-graph.
// this allows aot backends to just blindly walk the call-graph and not emit loads of garbage empty functions,
// for things that returned types or added to overload sets. extra important because they might use Bc::GetCompCtx, which can't be compiled.
// this is special because of check_quick_eval, so we're not always in the context of a new function that will be thrown away after the expression is finished.

- sret needs to be added to the callsite too, not just the declaration, which is fair enough.
  I think it only matters for calling through function pointers.
  clang does it for both tho so playing it safe.
  can see the problem in the debugger becuase it was doing `blr x8`
- dont emit const slice as load of ptr to slice. ~2700 -> ~1200 emitted constant definitions for compiler/first.fr
  that broke please_be_the_same which has a comment "sketch allignment, its fine if you break this test when emitting exes",
  so... thats a win i guess.

## distrust qbe (Jun 15)

I'm not sure i trust qbe, like ok i want to return two ints.

```

type :pair = { l, l }
export function :pair $get_two() {
@start
%out =l alloc8 16
%snd =l add %out, 8
storel 123, %out
storel 456, %snd
ret %out
}

```

generates this:

```

.text
.balign 4
.globl \_get_two
\_get_two:
stp x29, x30, [sp, -32]!
mov x29, sp
mov x1, #8
add x0, x29, #16
add x1, x0, x1
add x2, x29, #16
mov x0, #123
str x0, [x2]
mov x0, #456
str x0, [x1]
mov x1, #8
add x0, x29, #16
add x0, x0, x1
ldr x1, [x0]
mov x2, #0
add x0, x29, #16
add x0, x0, x2
ldr x0, [x0]
ldp x29, x30, [sp], 32
ret
/_ end function get_two _/

```

and in my language,

```

fn get_two() Ty(i64, i64) #log_asm = (123, 456);

```

with my current garbage asm where i do absolutly no optimisations, you get this

```

=== Asm for Fn2352: get_two ===
stp x29, x30, [sp]
mov x29, sp
sub sp, sp, #0
mov x0, #123 ; =0x7b
mov x1, #456 ; =0x1c8
mov sp, x29
ldp x29, x30, [sp]
add sp, sp, #16
ret
===

```

and the obvious correct that clang gives for this

```

struct pair { long a; long b;};
struct pair get_pair() {
return (struct pair) {123, 456};
}

```

is

```

get_pair: // @get_pair
mov w0, #123 // =0x7b
mov w1, #456 // =0x1c8
ret

```

## (Jun 11/12)

> we're at 15404 loc .rs

- hashmaps said the wrong size, i was treating them as a Vec.
- i was saying align of a struct was the align of the first field, but it should be the max field align.
  which is weird cause you have to pad it anyway if you can't reorder.
- a problem i'd eventually have is rust probably doesn't define padding but i try to use Values as map keys,
  but i don't see how that could be the current problem.
  fixing that might help debug this tho.
- trying to zero_padding on Box<Func> dies somewhere, and that was the sketchy one before so maybe thats interesting.
  but you can do it after serializing so thats the same as the real problem.
  its recursing on structs but not on pointers, so the FatExpr in FuncImpl i guess?
  no, VarType. oh because i have to zero padding on the tag first maybe? no its repr(i64).

oh shit im just not at all typechecking for generics????

```

// We might have compiled the arg when resolving the call so we'd save the type but it just changed because some were baked.
// Symptom of forgetting this was emit_bc passing extra uninit args.
arg_expr.ty = new_arg_type;

```

what the fuck am i talking about....
that lets me call slice_from_tuple that expects a Ty(*T, i64) on something that's a Ty(*T, i64, i64) cause i changed it to real vec.
I don't understand why it doesn't have the constant argument type in there tho.
Oh its a tuple (T, TTT) and then i remvoe one so its (TTT) and that slots to TTT so it just happens to not need const args remove from the type in this case.
Right so for fn alloc it doesn't work.
so the comment is true, but i still need to do the typecheck somewhere!!
so i was swapping length and capacity of vectors if i try to bit cast them instead of going through my serilizer that just uses length for both for now cause i just leak everything.
that wasn't enough to fix the problem tho.

- also ptr_cast_unchecked(From = Type, To = u8, ptr = mem.ptr) instead of ptr_cast_unchecked(From = Element, To = u8, ptr = mem.ptr)

I was saying the type for Option<T> was (i64, T),
so zero_padding always tried to walk the None section if it wasn't present,
fixing that fixed doing zero_padding in box serialize but still not the real problem.

I'm just stupid and assumed vec would be in order of the fields (ptr, cap, len) but ptr is in the middle.

> for reference, `franca run_tests.fr`, just unit tests: 62 ms. before prim_sig change.

Actually, now i don't even need all the flat call shit, I'm just using it for macros because that's what one does apparently.

- stop using bounce_flat_call for COMPILER_FLAT
- update_cc wasn't setting for #ct unless you specified #flat_call or #c_call. which i wasn't doing cause #c_call is the default.
- change a bunch of `(arg, reg): (E, E)` to `arg: E, ret E`

- TODO: that broke exceptional.fr somehow (bus error. maybe just a make_exec thing?) and I haven't actually taken the win yet and removed the flat_call code.
  leaving that for tommorrow. also need to fix the single vs double arg copy-paste, now that it can easily be dynamic with the vec.

flat call served me well but it will not be missed.

## (Jun 10)

debug log says width/height can't be zero.
problem is probably that there are f32s before these fields in the struct,
and im treating those like 8 bytes so the offsets are wrong!
yup, making it pretend to be i32 instead of f64 makes that error go away and it draws text!

TODO: use for bindgen: `cargo +nightly rustdoc -Z unstable-options --output-format json`

## (Jun 8)

- let type_of see through fn ptrs so can do inline .unwrap on vtable calls.
  I think nothing relied on Program::fn_ty denying FnPtrs.

## Build script goes brrr (Jun 7)

- when returning something larger than two registers,
  excplitly make the first arg the out pointer because i don't correctly use x8 yet.
  and need to use ManuallyDrop because you can't have the rust side trying to drop uninitialized memory when it sets the result in there.
- forgot to make it exec!
- compiling driver to c and then dylib works if i hardcode the source but not if i try to read a file.
  - though it might be because i was treating Fd as i64 instead of i32 but that wasn't the problem.
  - confusion with read returning giant number that must be -1 but im not seeing it as `lt` 0,
    because my comparison functions had thier arguments emitted as unsigned ints, for now hack jsut cast them back
    because that's just why the error message was confusing, not the actual problem.
  - oh im stupid. my emitting constants doesn't know that a cstr is a special type of thing,
    so it thinks its just a pointer to one byte so I was trying to open a file called "m" instead of "main.fr",

## (Jun 6)

- just had to remove is_unit check on arg in c::declare because now there's the indirect return address sometimes.

my own setjmp/longjmp was easy... for my asm backend anyway.

for c,
i can't just write it how i did other functions where it becomes
a `_FnXXX` that calls the libc setjmp because you can't setjmp and then return,
because now you're in a different place on the stack. maybe it would get inlined by luck on higher opt but that seems sketchy.

but i can't make clang on -O2 make it behave in a sane way (works on -O0).
i tried attribute returns_twice and i tried making all the local variables the compiler generates volatile.
also tried modifying the generated code to use libc setjump/longjmp and do it in an if() like `https://en.cppreference.com/w/cpp/utility/program/setjmp` says you have to.
but it still doesn't pass my test.
allocating the counter makes it work tho, with or without returns_twice.
so i guess that does nothing and its just more conservative about aliasing the random pointer
it got from an indirect call into the allocator, than a stack address, which fair enough i guess.
but it kinda makes using that less appealing.

I wanted to use it for the parser where there's only one way at the top to handle an error.
A question mark operator like rust's seems like more effort, since I have a weird concept of returns
so you have to like walk up the stack of blocks to find something of the right type to shortcircuit from and that seems a bit unpredictable, idk.
Could also try the `x <- try(whatever)` thing again, but I feel like that's meh.
cause you don't just want it to eat the rest of the block, it needs to work in nested expressions, and that gets hard to think about in the same way.

## (Jun 3)

my painful bug was about dropping a stack slot after dup-ing it so it got reused for another variable.

> pass by ref, arity, sig

- (parser) let you do loop captures without the extra brackets so its a bit less painful to look at.
- removed TypeInfo::(Scope, OverloadSet, Type), because they're really just unique u32.

:return_u32_pair
need to fix the crippling corruption when passing (u32, u32).
need to not assume \*8 but that's a lot of places (3) to write the annoying loop.
oh but actually, i'm only ever loading/storing 1/2 sized structs because anything else you pass by reference/copybytesttofrom,
so I don't need a whole compilicated logic for nested structs, I just need those cases to work.
using correct-ish offset instead of always 8 did quickly fix return_u32_pair but now i have to debug the other 1/4 test it broke :(
nvm, just an ordering problem, that was easy, and that fixed codemap too.
and it works on c too, so that's a good start towards using that as small step to selfhosting.

## (Jun 2)

indirect return address for large structs instead of using flat_call.

- bc_to_asm setup and dyn_c_call have a hacky +1 to arg_count to include that ret ptr.

## (Jun 1)

- I think now that i store correct sizes, i can't have my loose int casting,
  because just putting a u8 in an i64 variable stores 1 then reads 8 and my asm backend doesn't zero the stack.
  it used to work because i stored the whole register, and it works on cranelift because
  store get type info from the value not my bc instruction and i zero extend all ints from before i was tracking primitive types.
  calling zext on the digit byte fixes parsing_hex, parsing_float, and escape_characters.
  bf_interp was same problem.
  the aarch64_jit problem is also clearly the same but hard to see where cause its in an ffi macro.

## (May 31)

```

RUSTFLAGS="--emit=llvm-bc" cargo build --release -Z build-std=panic_abort,std --target aarch64-apple-darwin --no-default-features

```

If you don't have `panic_abort` you get `duplicate lang item in crate core: sized.`...
idk bro https://github.com/rust-lang/wg-cargo-std-aware/issues/56

Confusing dereference garbage pointer in c when calling alloc. just forgot to pop the fn ptr after a call.

Ok this whole backend not knowing about small primitive types thing is getting a bit dumb.

- spent ten thousand years on slice ptr-len wrong direction and then incrementing before re-pushing the pointer
- interestingly cranelift was easier to make work than my asm now.
  other than it can't arm... but x86 was fine.
- redirect function caused storing the result with the wrong type so unaligned (found in codemap with ubsan),
  (ex. add u32 redirects to i64 so it things it needs to store 8 bytes).
  but that didn't fix my problem.
- the problem with `Expected (18446744073709551615 == 255)` in basic.fr,
  was loading a byte as a (char) and then casting that to a (void\*),
  char is signed so it reads -1 and then tries to preserve that so it sign extends and you get the highest number.
  ok im kinda sold on zig's billion different names for casts now, should probably do that in my language too now that i'm getting close to non-painful generics.

## (May 30)

I think i want explicit allocators.
Can have a comptime one to use whatever arena the compiler wants ast nodes in.
Also nice for the lox example program, can have a gc allocator and still use the standard containers.
need to be able to do flat call through function pointer if i want it to return a slice,
but other than that it works for all the lib collections.

just tracking cc on fnptr was tedious but now i need to actually use it to make flat call in the backends.

fixed confusing crash that only happened on lox demo (my asm backend. cranelift's still fucked).
In bounce_flat_call!::bounce, needed to set the jitted page to executable.
Since we just called into a compiler function, it might have emitted new code and left it in write mode...
but our caller might be jitted code (like a macro), so before returning to them, we need to make sure they're executable.
this avoids a `exc_bad_access (code=2, <some code address>)`.
The problem only happened in the lox demo, not my small tests, so its fairly rare.
Need to remember for future that the address it shows is the address of memory you tried to access (not the ip when you tried to access it),
so if there's disassemblable code there, it muse be a jitting permissions/caching problem.
The frames might be wrong in debugger because it hasn't gotten to the part where it sets the frame pointer? idk.

- for emitting c, im trusting the callees list.
  problem with VERBOSE_ASSERT where list/displace weren't being seen as callees of anyone.
  needed to add callees in emit_capturing_call because for things with const args, the body will already have been compiled.
- needed to use flat_tuple_types instead of tuple_types for c fn ptr type because want to see struct fields as seperate args.
- stack buffer overflow because i was making vars that were right byte size (like 1 for a bool) and then writing to them as a u64 i think.
- something emitting a comptime pointer as a pushconstant is getting through.
  i thought it was some subtle thing about threading the right exec time through but actually
  i was emitting bool constant values as a memcpy because the backend isn't supposed to know how to load a single byte.
  cry.

## improving inference (May 29)

made it so overloading checks one argument at a time,
so it can decide which loop iter function you're calling based on just the collection,
and then infer the types for the closure, so you dont have to write the element types a billion times.
and then in promote_closure, slot in the types if they're missing, like i did in @match a while a go.
also had to carry through the requested type to each tuple element so it gets passed from the resolved function arg
all the way to compiling the closure.

gonna replace !while with !loop and implement fn while with early returns.
eventually want it to just be expressed as tail recursion but this is a good start.
it really doesn't like me writing while with a loop tho, `Missing value result.inlined_return_addr.get(&return_from)`.
Oh its because of my reducing Block with empty stmts to just the result expr and Block is the only place I hang a return label from.
So just need to check if that's been defined (which is only done by emit_captuing_call).

trying to emit shitty c.
its a problem that there's #ct functions that try to get called at "runtime".
like assert_eq-ing on a tag_value doesn't get folded.
same for some basic.fr assertions on Ty(...), i started fixing those manually before i noticed it was so common.
its because expr.as_const doesn't check tuples.

## remove #comptime (May 28)

another attempt at removing #comptime because const args should do the same thing.
const_bound_memo ends up being the same as generics_memo, if all args are const
so it maps to a function with no args that just returns a value.
tho be be exactly the same you probably need to :: at the call site.
but its mainly being used for functions that add to overload sets and return types so they're called in a const context anyway.
I'm hoping this lets me get rid of the hundred line emit_comptime_call function.
tho it might not because #generic needs to delay knowing its return type.

made const_args_key imm_eval the expression if there's only one and its not already values.
that was enough to make fn List work without #comptime. and the rest with only one argument.
same thing for macro eval. same for multi-arg case in const_args_key.
so now its only needed with #generic.
damn that was actually easy. all it needed was an extra infer_types in curry_const_args.
well that was good for morale.

i wanted to make a quick c backend, so i made a bucket array because it seemed convient to refer to types as a Str,
but then its a pain to write to because everything is hardcoded List(u8).
so maybe its time for traits (tho i guess it wouldn't even help this situation cause i need to pointer back but one step at a time).
I think I'll try to rip off Jai's $T pattern matching polymorphism stuff.
The current thing where the easy to use functions are the ones inside the generic function that creates the type is really annoying.

start with having #generic work like zig where tou can have later argument types depend on const args.
ok so the problem there is you can't renumber in the clone, because you haven't resolved the body yet,
so it still references the un-renumbred scope.
so i need to like insert a new scope in between where the arguments can go?

dumb mistake with checking flags on a mem::take-en func.

## (May 25/26/27)

now that im doing alignment shit, i dont really want to duplicate it for InterpSend::SIZE_BYTES so it just reads it from the program,
which is dumb and slower but the eventual goal is to use same layout as the rust structs so the whole thing goes away.

- was off by 8 redundantly skipping tag for de/ser Option.
- operator_index got garbage value for expr tag. needed to do correct alignment offsetting for NameFlatCallArg because (FatExpr, FatExpr) has padding.
- ok cranelift now works for 80/128 tests (with verbose_assert=false).
  but my asm says 0 for `debug_log_int(if(0.lt(4), => 1, => 0))`.
  problem was cmp_with_cond being read as (i64, i64, i64) when asm was trying to return (u32, u32, u32).
  struct coerceion now checks that byte offsets match. really the whole thing needs to go away eventually. TODO: at least make sure to finish_layout
  now my asm is same as cl.
- in deconstruct_values i wrote bit sizes instead of byte sizes :(
- `name: Symbol = part.expr.GetNamed;` doesn't work because it needs the special u32 load fn.
  but I can't easily refer to the type GetNamed.
- `missing value program.get_enum(enum_ty).\nType is not enum. (tried tag_value of GetVar)`.
  because i typo-ed when i redid builtin_const
- had to fix load_u64/store_u64 for my asm backend with non multiple of 8 offset.
- when emit_bc for ::Values, values.len != ty.stride. I think because of @as for ints doesn't actually do anything.
  for now just copy the right number of bytes but should really handle it in ::Cast somehow.
  now i fail 11, cl fails 3.
- @enum(a, b, c) has indexes (0, 4294967296, 8589934592). off by 4 bytes.
  im stupid and did align_to(offset, info.align_bytes as usize); without asigning it back to offset.
- all the rest of my asm failures are happening in @fmt for floats.
  problem with adding fn int as callee when you call it inside the macro?
  cause it works if you call it first and then also in the macro. but idk why it would work on cranelift then.
  for now just do the easy hack and TODO: actually fix the problem.
- i just want to get everything kinda working so i can make it less painfully slow!
  the new InterpFfi has to call get_info like a billion times and its ~47x slower than before i started using bytes.
  tho get_info is only 64% of samples, im hoping it just gets inlined sometimes.
- last problem is cranelift jumping into a bunch of zeros for bf_interp.
  - it happens during compilation, before running main.
    i can #log_ir of run tho, so it gets that far, it can't compile main.
  - it seems it can't compile the assert_eq on bools (if i ::println on either side of it, the second doesn't go off).
    but it can do it in other tests... so what gives??
  - and deleting random cases from the @switch also makes it compile....? this is kinda scary.
    like it works if i remove Bf.Open or Bf.Close...
    but it always compiles fn run anyway, thats somehow affecting its ability to compile the assert_eq in fn main!!!
  - it works on x86 so i guess its either an alignment thing or a instruction caching thing.
    i tried always aligning to 8 in my GlobalAlloc since i do flat_call arg/ret with Vec<u8>
    which might not be correctly aligned but that didn't help.
  - maybe its like about stack alignment? cause deleting an unused local variable makes it compile.
    tho asking for opt_level=speed doesn't fix it and i'd hope that would remove the unused things.
  - its getting to call main but it fails before doing the print.
    !! but not if i call debug_log_int. now it actively can't call println?
    can't call print either so its not the stack address thing for the new line.
    but it does work if i inline print. so its a problem with passing a string as an argument??
    it can call debug_log_str.
    but it can also call `fn pls(s: Str) Unit = debug_log_str(s);`
    can also call a copy pasted version of print... so this is all just useless and its some crazy layout thing??
  - something to do with string escapes maybe?
    but those work everywhere else.
  - like i dont have high faith that its not a cranelift bug.
    maybe we're back to just doing everything myself because its harder to debug other people's stuff.
- most of the slowdown in calling get_info a lot was because i was checking cache on ty but saving on raw_ty
- adding real u16. trying to deconstruct_values a @struct(\_0: u1, \_1: u5, \_2: u16, \_3: u2) from 26 bytes instead of 32.
  something's not emitting padding for the u16.
  ah, its `for v in values? { pls.extend(v.0); // TODO: alignment }` in quick_eval ::Tuple.

## (May 24)

- ok so now i never match against Values::One, so i can take it away, just do everything with Vec<i64>, then do everything
  with Vec<u8>, and then add back the by value varient when its small as an optimisation. thats the plan.
  got to everything being Vec<i64>, its like 15% slower than last i checked, thats fine for now.
- problem where it didn't compile f in create_slice_type if i set the type to FuncId instead of Fn(Type, Type).
  fixed by calling compile instead of ensure_compiled in emit_runtime_call and switching some checks from matches TypeInfo::Fn to also allow FuncId::get_type.
  not sure if thats the best choice. or if i should just have seperate as_literal for functions that checks the type...
  but you want to allow for ones that haven't typechecked yet.

## (May 23)

- compiling the function before folding a call to it because those didn't go in anyones callee list.
  and to avoid that recursing, it marked the expression as done so it wouldn't get added as a callee of the lit_fn,
  so it would go in pending_indirect list and get compiled at the last minute.
  dont have to worry about the arg trying to call something that's not ready because you only fold if its already a const value.

## (May 22)

- got rid of Value::(Type, Label, Unit, OverloadSet)
- got rid of some implicit casts
  becuase they get in the way of just representing all values as bytes.
  probably want to bring them back eventually but in a more explicit way where you just declare overloads for implicit_cast maybe.
  - tuple of types to type so you have to call Ty all the time.
  - overload set to Fn, now you use @resolve (only needed to change 3 tests + `__save_slice_t`).

## (May 21)

I want to be able to make terminal ui things but that kinda needs correctly sized array of bytes as a struct fields to call the terminos functions.
Its 20 bytes so i can't even hack it with bitshifting cause i don't have u32 fields either.

- removed ability to .(index) on structs, how that i have seperate PtrOffset ast node.
- made value arrays. should have them more interchangeable with tuples in the language.
  but the compiler wants to see them differently so `[u8; 4096]` isn't storing 4k TypeIds in a vec.
  also arrays let you use runtime known indexes cause they're all the same type.

## (May 20)

- have @match fill in argument types since it knows from the varient name
- want to add escape chars in strings by just having the parser emit calls to a comptime function.
  but to pass that to .char(), which you often want to do, need to save a way of saying that function should be constant folded.
- started tracking which constants are pointers so the backend can handle relocations when not just jitting.
- trying to output an executable with cranelift-object.
  the thing it gives you is a .o file and then i run clang on it to link, but i get bus error trying to call a Linkage::Import libc function.
  i tried having a function called main and i tried having a c program call one of my functions and giving that porgram + my obj to clang.
  no luck.

## (May 19)

- use bit set for func flags. make some other structs smaller
- got rid of ::F64, ::Heap. ::Bool, and ::Symbol Value varients, because the compiler doesn't care about them other than type.

## (May 18)

- want to clean up type checking / coercion stuff.
  so I want @as to be the only one that does the current loose `type_check_arg` where you can assign structs to tuples.
- @as can't just change the type on the node because `c: i64 = 0; d := @as(u8) c`,
  makes emit_bc debug check see that as a u8 load without calling fn load.
  it does work if you take that out but i don't really want to remove that check.
  but doing it on the pointer type instead of the value type makes sense to me.
- made it give the compiler access to fn Slice(T) so strings and !slice can have the right type less painfully.
- made bool take one byte. interestingly that made @bits break in emit_bc debug check
  because it was using !if instead of fn if, so the cond didn't go through the normal deref thingy so didn't get converted to a load call.
  thats pretty janky.
- i feel like its important that eq/ne/load/store know that they can call the same function for most unique types of i64 or whatever.
  so i can use types for enum tags and just forward those functions to the int versions but not say you can call fn add on an enum tag.

## zero size unit (May 17)

- in the backend you mostly need size and float_mask at the same time so put them together so its less verbose.
- want zero sized Unit so you don't spend instructions moving around garbage.
  removed places that pushed/popped a zero to Vec<i64>/v-stack.
  had to check for it in c::call because you can't just read garbage if the vec doesn't have an allocation.
  special case in cranelift signetures / intrinsics.
  last that fail are `match_macro` and `tagged_unions` trying to load a zero sized thing.
  strangly adding a check for zero size in emit_bc Deref fixes those but breaks `test_string_pool`.
  but why am i even getting there? i gave Unit a special load/store that do nothing.
  so enums are adding loads without checking overloads.
  ah, it happens if i have a Unit struct field too.
  so place exprs like that don't go through the compile deref that checks the overload.
  nice i caught that now cause it would be a problem when i tried to make small types take the right amount of space in structs.
  so thats mostly easy to fix by just not setting `.done` in compile_place_expr.
  but @tagged are still wierd because if you load the whole thing that includes padding so you have to get that off the stack before treating it as a smaller varient.
  but no, thats not true, you always access specific fields on it from memory and only load the whole enum type if you're going to store the whole enum type.

TODO: it should know Unit load/store does nothing so my backend doesn't save registers before doing the garbage call!

## compiling the compiler for blink

https://betterprogramming.pub/cross-compiling-rust-from-mac-to-linux-7fad5a454ab1

in `.cargo/config.toml`

```

[target.x86_64-unknown-linux-musl]
linker = "x86_64-linux-musl-gcc"

```

```

rustup target add x86_64-unknown-linux-musl
brew install FiloSottile/musl-cross/musl-cross
cargo build --target x86_64-unknown-linux-musl

```

ok that worked on my mandelbrot demo.

so now i have to write the c_call shim for x86.
i don't understand why it can't find my function!
is it because of changing whos doing the linking?
it works if i make it a `#[naked]` function with an asm block, but not if i make it a global_asm function...
thats so strange. its in the same module which was the problem last time.

- https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI
- https://stackoverflow.com/questions/18024672/what-registers-are-preserved-through-a-linux-x86-64-function-call
- https://en.wikipedia.org/wiki/X86_assembly_language
- https://en.wikipedia.org/wiki/X86_instruction_listings

- i have to restore the stack pointer after pushed args, callee just reads them.
- cranelift: can't resolve libcall memcpy. fair, there's no dynamic libc for it.
  so how do i tell it the fn ptr to use.
  JitBuilder::symbol_lookup_fn lets me just switch on the name and give it a pointer to my staticlly linked memcpy, cool.
- `debug_assert_eq!(addr as usize % 4, 0);` x86 has variable length encoding so does't force an alignment i guess.
- hitting a SEGSEV somewhere. i wonder if its something aarch64 inline asm that i forgot.
  yep. skipped any #aarch64 functions and now it segsevs with rip=0x1337.
  ah ptr_to_int, int_to_ptr and fn int(f64) i64.
  that wasnt the problem but still good to have fixed.
- turning off VERBOSE_ASSERT helps for now.
- `not compiled Fn286 gt`. oh for value of a constant the compiler needs to call it so need to emit the cl intrinsics as thier own functions.
  but just try a different example for now.
- its definitly trying to jump to a missing function, the address changes if I change Jitted:EMPTY.
  but wheres it getting it from??.
  even without fixing that, passing 79/112.
  tried writing the func id in the empty slot.
  for generics it seems that its trying to call `$eval_0$Fn%73Cs1b3([Unit%3Cs1b3, Unit%3Cs1b3])$` through the dispatch table.
  fuck no im an idiot, thats putting the f that extend_blanks is going up to, its actually trying to call offset,
  which makes sense that doesn't exist cause its one of the intrinsics i just skipped. sigh.
  AAAAA, i was so confused about how it was getting the ptr because i thought i took out the thing
  where it uses the dispatch table when you try to GetNativeFnPtr and it wasnt even doing that bc inst....
  but i was still embeding the dispatch ptr when trying to CallDirect to something that wasnt ready yet,
  hoping it would get filled in later, so thats why it wasnt trigging the check in get_fn.
  so problem was my mutual recursion hack meant i couldn't catch the error of trying to.
  but even with emit_cl_intrinsic, it doesn't get there.
  maybe its like its added to callees of something that doesn't end up having compile called on it before getting popped off the stack,
  and it works on aarch because the merge causes special_body to be evaluated directly?
  DAMN IT. i typo-ed the return type of offset as i64.
  ok so i should really consider reworking my overloading system to catch that sort of thing.
  at the very least typecheck my hard coded string signetures somehow. fuck.
- well anyway now all that fails is floats.fr (only comptime stuff cause mandelbrot works)
  and libc.fr/open_dynamic_library (which wont work cause im running it in blink which doesn't have dynamic libraries).

## finishing cranelift backend (May 16)

- floats on cranelift. im being dumb and not tracking types so doing a bunch of bit casts.
- have to be able to call comp_ctx functions on cranelift for macros.
  might as well just make that a normal part of the bytecode instead of special thing in the backends.
  for now just made an inst to push the ctx, but it still needs to know on the call inst to add an extra argument,
  so it didn't really make it simpler. would be good if i just actaully put that info in the function type.
- dont put a return val for sig when its Never.
- forgetting to set asm_done breaks comptime_float_call which is weird.
  but you want it in a more central place so you never forget anyway so not a huge deal?
  creepy tho. why does trying to compile the same thing multiple times make you try to call something with a different calling convention.
- when returning an if expr, it does a jump with block args, so needed to tell cranelift about them.
  thought it would be annoying because types but since i cast floats for ::ret already, i can just use ints everywhere.
  that seems to have fixed pack_i64 too.
- can't try to do a tail call from cranelift to a rust c abi function.
  and can't use cranelift tail abi if its something the compiler wants to call.
  for now i guess just giveup on that and try again later when the normal stuff works.
- need to forward declare func ids for recursion.
- thats enough for 100/116 to pass on cranelift (comptime+runtime)
- next problem is entry block args being floats instead of ints because i stopped using append_block_params_for_function_params.
  cause i want them to be ints so i dont have to deal with it but then it doesn't match signeture which it rightly complains about.
  doing that and then casting from float fixed another 6.
- mandelbrot floats mask was wrong. lol `// TODO: HACK: wrong!!!!!!! size_of. -- May 3`.
  when making the mask, i was always shifting by one for each tuple element even if it took up more slots.
  and it mostly didn't matter cause the only one where i passed a struct of floats was mandelbrot and that didn't do
  try to pass it to the compiler so it didn't matter that i wasn't using the float registers, cause both sides made the same mistake.
- the 2 mutual recursion tests need forward declare. on asm i just look it up from the dispatch table but cranelift has fancy relocation stuff so why not use it.
  but im lazy and just want to see it work first so using table for now.
- need to say right number of block params for @tagged, even tho they don't have Some(tuple_types).
- emitting an unreachable and abandoning that block after you call something that returns Never.
- fixed bc emitting StorePre of size 0 when assigning to a var from an if with a Never branch.
- thats everything working except backtrace.
- unwind on cranelift gets messed up by it doing pointer integrety things? pacibsp and retabsp.
  so when it saves the return address its infact some different thing you can trade for the return address?
  https://devblogs.microsoft.com/oldnewthing/20220819-00/?p=107020

## small types on cranelift (May 15)

- got rid of FnWip. cause i wasnt using it right with constexprs anyway.
  now just tracking var types in the global thing.
  the other thing they did was tracking callees so you could try to emit them first.
- trying to make small_types test work on cranelift.
  revealed problem of using normal loads for implicit access to vars that should be u8,
  so then really you should zero the stack slot at the beginning which i wasn't doing so i guess it worked by luck.
  new system is emit_bc never sees a GetVar, the frontend inserts `&[]` every time so it goes through the normal logic
  that might replace that with the intrinsic load/store.
  but now there's the u8/i64 types problem, so i guess i have to say zero extend on every load.
  oh they have a special uload8 that does that.
- next problem is the reverse, in my test, the store has to store 1 byte
  but then it has to read back the whole word without assuming its the same as it just stored.
  so it needs to see the value being stored as a u8 even if it started as an i64 (like a constant).
  so truncate the value first. that works.
- perhaps this shows that my bytecode should keep better track of things.
  cause it did know which was float/i64/u8/ptr/unit and im just painfully reconstructing what i threw away.
  presumably you want to help it be fast by giving it fewer redundant bit size casts to optimise out.
- not following the calling convention with x21 is creepy.
  and i was already reloading it every time cause i didn't trust, so just stop using it at all.
  had to adjust unwind.fr to use a new builtin to the compiler to get the dispatch table.
- was a pain dealing with R64 vs I64 in cranelift, but it seems i can just make everything I64 and dereference it just fine,
  so i don't understand why R64 exists.
- for using results of comparison as values, had to uextend to i64. eventually should just be a byte (or bit??)

## stop committing generated stuff & do u8 strings (May 14)

If I can't have a self-hosted compiler yet, I should at least get the benifit of being able to build from source directly.
Rn I generate instruction encoding stuff so there's a sad amount of code there that i barely use.

- cleaning up the rust side asm encoding stuff is easy.
- for the rest, start by defining a version of @bits in rust.
- fuck im trying to pass it a slice on the stack but that call is the last in the function so it tries to do a tail call.
  and the tailcall resets the stack so the rust function puts its local variables overlapping the slice i tried to pass as an argument.
  so i guess i have to say no tail calls if any of the args are pointers?
  damn lifetimes are looking like a pretty good idea all of a sudden.
  suppose its not that big of a deal cause the main thing i want them for is closure loops where the lambdas have no arguments anyway.
  should make the calling convention thing a more useful struct if im gonna be having this many combinations.
- then need to make !asm not force a slice. instead eval each int of the tuple seperately and build the vec on rust side.
  if the expr isn't a tuple, you can still return a slice like before so the language hasn't lost cool jit power there.
  had to get rid of u32 as inst return type cause it just makes everything harder cause ffi u32 things its just an i64.
  bring that back when i actually do smaller types as a first class thing.
- then can remove the stupid hack with allocating a list for the asm so it looks much neater.
- so that gets the arith but cmp still needs to call a fucntion so has to return a list.
  cant just compile first so inline fns work because then have to thread a FnWip through which is annoying.
  and even then, inlining doesn't project the args so i couldn't just poke through the ast like im doing now, would have to actually call it.

FIXED:

```

//! If you change anything here, you have to `./bootstrap.sh` again.
// TODO: some of the argument names matter because they're hardcoded in emit_rs

```

I wonder if --64fps demo would be less flashy if i did u8 properly instead of printing x8 as many nulls.
Maybe start with still spending 8 bytes of stack/struct on everything and just intercepting load/store
with calls to the inline asm functions that use the short instructions.

- so now i hackily desugar `u8_ptr[]` to `load(u8_ptr)` and `u8_ptr[] = val` to `store(u8_ptr, val)`.
  which seems to work. its doing a sad slow overload set find every time it sees one of those expression tho.
- painful debugging of requested not being unpointer-ed. really need to make my error message better.
- ok trying to make Str be Slice(u8).
  after adding some casts, only ~8 tests fail. eq is sometimes reading them like its a i64 still.
  made ints interpsend know thier bit count so string literals can be the right type.
  that made it worse.
- it gets weird with string being InterpSend cause that expects everything to be serializable,
  as a vec of ints which is odd when reading from a ptr to u8 with the wrong len.
  so hacky answer for now is i dont actually need strings to be interp send, ill just treat them as (\*mut u8, i64).
- oh ok that just worked. i thought it was broken cause 2 eq tests were crashing on oob
  but it was just my debug print that i did before the length check in the loop.
- so now the state is that slices/lists/ptrs/sizeof correctly treat u8 as bytes.
  however, they still take 8 bytes as struct fields/stack slots.
  does that mean size_of is wrong for structs containing 8 bytes? yikes.
  oh, no it doesnt... becuase size_of specially lies for u8 and uniques of it, otherwise it still does slot count times 8, so thats ok for now.

## more place exprs (May 13)

- using 32 bit indices everywhere saves 9MB (15%)
- made backends only get program and asm instead of whole compile struct so it feels a bit less messy.
- i dont actually use most of libffi, i dont need structs abi since my backend can't do it and i only need aarch64 for now.
  so removed it and replaced with tiny asm shim that just loads ints from an array into registers and calls a function.
  libffi is ~45k lines of asm/c, mine is 13 lines... i just have very simple requirements compared to what its trying to give me.
- interestingly that also fixed comptime print. so i guess i was lying to them about struct abi flattening somehow?
- discovered that when you do global_asm! and extern c to your own function, it must be in the same rust module as calls it?
  if you try to import it the linker spews a billion lines of errors.
- made `.(i)` and `[i]` work like other place expressions.
  had to add a new PtrOffset ast node that they desugar to so you can tell the tuple accesses
  in user code apart from pointer ops that everything desugars to.
- auto deref in chained place expressions (like rust).
  you still need to manually dereference for function calls (which is kinda inconsistant, might change that),
  and if the type of the first is a pointer and you want the value, like it doesnt just auto deref on assignment,
  its just a magic part of the dot operator. which is how rust does it, and feels kinda nice,
  cause you never meant to access a field on the pointer so it always guesses right.
- im suspisious that it wasn't inlining InterpSend::size cause i saw it in the profiler so i made it an associated constant.
  that seems to have made less serialize_to_ints show up but that seems crazy? like its always through a generic (its not even object safe), why wouldn't it get inlined?

## place exprs (May 12)

- made inserting #include_std stmts a bit less dumb.
- change @namespace to take a closure instead of a block so every block doesn't need to remember its scope.
  lookup field access in constants of that scope, dont care about which block for now, but eventually need to so you can do private/shadowing.
  taking off the scope tracking field from Expr::Block saves 24 bytes on every ast node.
  saves ~7MB on tests so I have 284K nodes, yikes. Stats::ast_expr_nodes_all says 34k... oh but it doesn't count clones.
- made TypeId::scope just be one int, removed local_constants and some old fields of Func
- made Enum cases a real type instead of weird contextual_fields thing in the sky.
  prep for actually checking that casts are valid values.
- replaced `tuple[index]` with `tuple.(index)` which frees up the brackets for something an overloadable operator for containers.
  the tuples one looks dumber but you'll probably never want that once i can pattern match on them properly in declarations.
  using just .index means you can't use an arbirary expr and you have to deal with it when lexing floats if you want to chain them.

// :PlaceExpr
I want less painful place exprs.

- currently `n&[] = 456` and `n = 456` generate the same code (as they should),
  but raw var access has wierd special cases as tho it needs to specially optimise out taking the pointer to it (which it then does it in the end anyway).
  so give set_deref the special power of evaling place exprs. the easy case is wrap <var> in <var&[]>.
- then assinging to a field access expr (not a []), doesn't conflict, so handle that recursively inserting &,
  so that avoids writing `a&.b[] = c` all the time instead of `a.b = c`. but doesn't help with more complex nesting or even jsut reads of the value yet.
- now !addr means eval that as a place expr but remove the deref. that still needs a special case for var!addr because there's no desugaring you can do for that.
  so `var&.field` and `var.field&` produce the same value. which might be the last unambigous change i can make on the path to being normal.
- changed to more c like syntax where `var.field` is a load and `var.field&` gets the address.
  also i think `ptr[].field&` is really dumb because you have to pretend to load the whole struct but you don't actually.

  now its much slower cause i do a bunch of intermediate addr/deref chasing.
  holy shit im stupid, no, nothing matters, i just left on my log and leak everything on every recrusive call to compile.

## cranelift (May 11)

I want x86 but I don't want to go back to the nightmares factory.

- cranelift maybe doesn't inline things cause they expect you to already have optimised wasm?
  so my llvm strat of i write the basic functions in text ir and parse that into my module seems meh.
  instead just have a few magic callbacks in the compiler that get access to an instruction builder.
- my old thing of SplitFunc for different targets is kinda confusing and annoying to deal with.
  what if instead i merge them so theres only one func for iadd or whatever, and you just pick the impl depending on the target you're emitting for.
- now with merge it means it tries to load the llvm ir funcs wven tho not using and i havent loaded them since i changed to inline asm needs to say #c_call,
  and that was unacceptibly painful to figure out cause my error messages are useless cause they get swollowed by type_of,
  so by the time it bubbles up, it doesn't have the right loc on it anymore. fuck.
  also slower! but the old thing was so dumb. TODO: this one's doing redundant work somewhere, it merges the same thing twice.

- it seems you can declare a symbol on the jit builder but not add imported functions later? which i want to for comptime_addr.
  I guess i could to them up front but like... might as well just do it through a function pointer.
- iconst never an R64 even if you're going to use it as a pointer
- it gets less far if i try to return_call_indirect tail call comptime_addr??
- the way flat_call works rn is dumb. in the prelude, it loads the args to v-stack and stores them in its own stackframe in var slots,
  instead of just treating that space as its variables. so you have to do that whole copy even if you only use one part,
  and my backend does the spilling in a deranged way where it puts them in the wrong place when it runs out of registers, cause it can't look ahead, and recopies later.
  Using that space as vars is sad because then you have to save the pointer, which is why the normal c abi is better, they have the caller just copy it to below the stack pointer.
  but I i dont want to change too much at once. so just gonna add memory reference variables to bc,
  and then it becomes a bit less painful to support on the new backend.
- FIXED: `// TODO: why can't i do this ? feels like this has gotta be a lurking bug -- May 8`
  the "x1 is not free" thing from a while ago, that happens when you remove a reg from the stack after assigned for arg:
  was because i wasnt dropping the old reg after the move.

cranelift has much better error messages than llvm (in that it doesn't just segfault if you make a mistake),
and is like 2MB extra instead of 60MB extra compiled binary.

as a side quest i tried building my mandelbrot with `--target x86_64-unknown-linux-gnu` (yes I `rustup target add x86_64-unknown-linux-gnu`),
to try blinknlights. it spewed errors. internet thinks maybe i need to install gcc.
zig `zig build-exe tail_call.zig -target x86_64-linux-gnu` just worked.
My cross compiling goals give me extra insentive to get rid of libffi so i dont need to figure out how to get thier massive pile of c to compile.

- removed some #bs that werent actually called by the rust code
- removed most of my old spammy logging. i never looked at it anyway so it was just kinda distracting to have in the middle of the code everywhere.
  really what i want instead of that is a repl-ish thing where you can have an error and then be able to look around at the compiler's internals.
  the just dumping out a bunch of files is not super useful.
  anyway I've made constants less confusing since then and now what I have truble with is the asm stuf which has a different logging system.
- finally removed last traces of my Any type from the interp.

## simple tail recursion (May 10)

- fixed next_label starting at from_raw(0) instead of from_index(0) so serialization works
- cache specilization of const args.
- did simple tail calls. needed to add hack to backtrace test so all the frames would show up.
- saved the start of the function at the beginning instead of the end so direct tail recursion works better

# simplify FnBody/cc/returns/constants (May 9)

- using Vec::with_capacity in a few places
- fixed do_flat_call_values where I was doing Vec(i64) -> Vec(Value) -> Vec(i64)
- emit asm right after bc always so don't have to do callee loop in asm.
  then it turns out you never look at any function's FnBody other than the one you're currently doing, so there's no point in saving them all.
  very nice to replace every `self.compile.ready[self.f].as_ref().unwrap()` with `self.body`.
  only .7MB but feels less complicated now.
- track exactly one calling convention on a function instead of having to reinterpret the flags every time

(start plan)
Ok my FnWip result thing doesn't make any sense. It currently just holds var types (tho eventually early return targets too??)
and its tied to functions but really there should be multiple contexts like that in a function.
Like all the comptime expressions kinda take place in a different universe.
Which is how my current make_lit_fn works but I try to skip that as much as possible because it seems dumb to make a whole function just to lookup a single constant or whatever.
And my current way of threading it through macros is a hacky unsafe thing.
You need to be able to have them inherit like scopes for my @type thing to work intuitivly.
Maybe the same thing as const values makes sense where you just do it by var in a hashmap in the sky and if you are able to refer to something you must be allowed to have it.
I think its really important that it always work if it looks like it should work lexically.
The thing where most languages closures don't capture the control flow context of thier declaration is the extream version of this problem i feel.
So for vars i think the easy choice is that global thing but how to handle the stack of lambdas you might be trying to return from.
Like you want it labeled by name but you might have nested of the same funcid and be trying to refer to the outer one,
and it would look reasonable because maybe the closer one is inside another lambda so you can't see it.
The current thing where you just hope the inner one is right will be super confusing.
So you need like a parallel system to the var scoping... but with exactly the same rules kinda.
Cause again, having it thread through a result thing means you have to recreate the path you should have had from scope.rs anyway.
Easy way i guess would be force you to declare labels on the lambda you're trying to return from.
Kotlin also lets you use the name of the function you're passing the lambda to to qualify the return.
Have to just start with the easy thing to prove it works even tho the syntax is ass.
I guess even easier is the original idea of only unqualified !return and you just assign to a variable,
but then you need to track what block you're in all the time. When really you want '!return' to just act like a variable.
could declare a fake `__return` in scope.rs? maybe that's the dumbest option (affectionate).
(end plan)

So problem with passing it into a closure (like trying to implement break for fn while),
renumber doesn't do constants cause they're like in the sky hashmap already.
but you can't just make it runtime because emit_bc needs to be const or it doesn't know where to jump to.
and i forgot closure args can't be const yet because of that same renumbering thing.

That's a very related problem to constants in macros and I fixed that my delaying when constants get hoisted out of blocks into the global map.
Quite pleasing because the old system had weird logic to pass information between scope and compiler but now you just do things in one place kinda.
Feels like it would mean compiling more copies of the same constants but that's what allows them to work intuitively.
You really want each expansion of the macro to have its own version of the constants becuase its reasonable to have different types for them like how it works for generics.

- removed no const arg check on closures and added renumber in bind_const only if its capturing. redundant but its fine fore now.
- had to fix emit_bc to allow nonlocal return to discard-ed function and then use 0 arg jump. cause while is always discard i guess?
  thats all it took for fancy_while to work.

TODO: really you dont care about closures being inlined, you just care about them not having thier own stack frame and not escaping thier declaration frame.
so represent them as labels too and then you don't have to be paranoid about calling the same one multiple times bloating your code.
Then you can allow tail recursive closures and the you don't need !while as a language builtin anymore.

## syntax tweaks (May 8)

- Made '=>' instead of '=' in function declaration mean capturing.
  So then I want to use '=> body' instead of 'fn = body' for passing a lambda as an argument.
  Also it makes @switch look more like rust.
  The problem is it means you can't have function names be part of the value which I wanted to use for targeting nonlocal returns.
  But currently its weird that functions aren't just values bound to variables,
  and that if you use that syntax it decides they're a closure so it will parse through them unlike the statement ones.
- Made name(arg = value) mean named arguments (instad of arg:value) so it looks like default argument values.
  Which also means you can tell a decl and a call apart without the fn keyword. is that a good idea?
- Allow #markers before the equals sign of the function instead of only above the decl, cause you want to allow them on values too.
  So you can say the calling convention of closures, etc.
- changed struct literal syntax to be equals signs so its like named args / vars.
  now everywhere in the language, : means type and = means value.
  except functions i suppose. maybe I should use -> instead of =.

Fix remaning tests:

- fixed early_returns. was using the result_location of the ret call instead of the thing you're returning to.
  TODO: have it work when its not pushstack
- now asserting has c_call marker if you do that so have to add those earlier in compiler.
  before i just figured it out in asm based on sizes. but now i need to change stuff in emit_bc based on it too, feels cleaner to decide in one place.
  inline asm now needs you to say #c_call.
- now only mmap is failing.

Did default values for struct fields.
Involved some hacky threading result around but really it shouldn't need the result since that just holds var types and default values are always constexpr.
Vars know thier scope anyway, could just track types there too.
But it feels a bit cringe to have every tiny thing know whats going on?

## early returns / rls bc again (May 7)

Doing early returns by having labels as a comptime value.

- keep track of number of incoming jumps so if its only one, you can just fall through,
  without spilling stack, cause you know you dont need to match value positions with anyone else.
- if a return_block only has no incoming jumps after the last expression, dont bother emitting it, just fall through in bc.
  i do that check in asm too casue i did that first but seems nicer to not even get there.
  only fixed call_if_in_unquote.

ok i have to go back to passing a result ptr because its compeletely impossible to read the asm,
cause it does these insane redundant loads where it spills to the wrong place while trying to load the whole thing to do a copy.

- made Never say it takes 0 slots which removed some weird -1s and let asm assert no flow on stack between blocks without failing more tests
- putting 16 in a register and then dereferencing it. swap_remove for storepre instead of shifting remove. cri.
- then an off by one in storepre
- moved most of the flat call abi work to emit_bc so it can emit to its arg loc and use its ret loc without copying.
  now passing 48/90.
- typo construct_struct was duping on PushStack instead of ResAddr. pass 52/90.
- impl dup non sp reg. pass 56/90
- fix 'assertion failed: target_c_call && !target_flat_call': move adding flat_call if big arg/ret from bc_to_asm to emit_bc
- flat_call PushStack tries to load but it doesnt push the ret addr first. and was loading |arg| instead of |ret|.
- for flat_call header, it was only deciding to use result addr based on ret size, not considering explicitly marked cc like in my test.
- offset from spill wasnt doing the mul 8. would affect anything not inlined with multiple returns. pass 65/90.
- result aliasing miscompilation is back. whatever. ignore for now.
- another card on the house of not tracking dominators. now i leak slots because when you do the second if branch,
  it doesn't know which are safe to reuse anymore. can do that now because result locations mean less wasted slots.
  but thats 95/101. that was the problem with @bits.

TODO: use => instead of = for capturing functions. remove need for fn keywork on values, just use that for
add to overload set stmt. fix stack slot reuse. !return targeting an overload set and it just resolves to the current function.
!return without fn arg for outermost fn. version of while loop that passes in labels for break and continue.
same by var ptr for large values for !return as !if. fix whatever debug check is going off.

## basic blocks (May 6)

The way my bc handles loops and ifs is kinda hacky and convoluted. gonna try adding the concept of basic blocks.
The general goal of this whole adventure is making it easier to think about how to add tail recursion and nonlocal returns.

The problem is that any time control flow diverges (ifs and loops),
you need the places where it rejoins to represent each v-stack slot with the same reg/slot.
Its what llvm phi instructions are for but I like my way of just both branches leave something on the stack cause its so simple to think about.

BEFORE:
ifs made a variable that each branch wrote to and then read the return value when rejoined.
while always spilled everything to stack at the beginning so both times you get to the cond block its the same,
dont have to worry about stuff getting spilled in the body so its in a different place the second time around.
I don't keep track of basic blocks so you can't really tell whats going on.

AFTER:
Split bc into basic blocks where you can't jump in/out of the middle (other than calling functions that return to the same place).
Blocks can take arguments (only used for rejoin after if), where jumping in puts in ccall reg and then the block puts back on stack.
But mostly stuff still just implicitly flows on the stack (i use that for temporaries).
I still just spill everything (other than args) at the end of each block.
TODO: be more consistant about representing block arguments? rn stuff can just flow on the stack behind you kinda.
real function args should be args to the entry block but currently are hacked in.

- failing math_still_works. JumpIf: flipped true and false branches but still tried to cbz. true needs to be right below.
  that got 68/87 working
- three problems now: `unhandled prefix in @switch`, `attempt to subtract with overflow`, `(slots as u32 - float_mask.count_ones()) < 8`
  the two are ifs returning big values that don't fit in register, need to use variable like before and floats aren't done properly.
- made ifs use var if they return something big. 81/87 but had to turn off reusing stack slots because bbs are done depth first not in emitted order now.
- 86/87. fixed nevers now that sometimes they need to push junk on stack. TODO: thats a hack. calling a Never should be a terminator like unreachable.
- broke never more so minor code change in derive makes all pass
  BUT bug call_if_in_unquote is fixed and that was really creepy so i think thats a net win and i'll call it a day.
  i'll fix Never properly eventually.

its not slower which is nice. stack slot reuse is fucked which is bad. code is more complicated but looking at the bc dump is more legible.

## (May 5)

- removed some places where it pre-interned a fnptr type since all exprs have thier type now, emit_bc shouldn't need to mutate program to make a new one.

trying to remove uses of type_of and mut_replace because they're clunky ways of doing things.
A) its a massive problem if you error in a mut_replace and try to recover cause you lose something.
b) type_of is kinda all or nothing on getting the type so its not really what you want for overload resolution.
you just need more compile errors to be recoverable so it can iteratively narrow down the types of each arg seperately.
now only type_of is for overloading so I can spare some code to fix that cause it means getting rid of the type_of junk.

- convert to @At macros: !symbol, !assert_compile_error, !type.
  tried to convert @as to !as but its still a massive hack because i use the @as syntax but wrapper macro isn't enough,
  because type_of can't just apply one layer of macro and get the type, it always tries to compile the whole thing, so theres still a speciall case.
- changed serial tests to reuse the same compiler instance and now that's faster than the fork one.

## improving enums (May 4)

all these are not big expressiveness wins, really just trying to raise morale.

- renamed rust @enum to @tagged cause it seems kinda dumb to overload the name enum.
- did @enum(name1, name2) as a lib macro so don't have to write the sequential values cause that's annoying.
- remove #enum (for generating init constructor). it was used in 1 place and didn't integrate well with treating types as values.
  still have mixed feelings, maybe i want it to be easy to add constructor without changing callsites but meh.
- remove #struct. that was used in a few more places but still felt like low value add.
  if I want that, I should just implement it in the library, it doesn't need to live in the compiler.
  so need to implement hash macros that transform functions soon.
- remove !enum, just define it as @tagged in the compiler.
- remove !struct, like above. that was used more often but i like the prefix syntax more
  and started using as soon as i had the forwarding macro anyway. it means you don't have to look all the way to the end to see whats going on.
  (maybe should rethink my & syntax then too...). only sad thing is now if you define one inline as a function return,
  you have to wrap in brackets because the lhs of an assignment could be a macro call so the parser complains.
  i think its not actaully ambigous but threading the context through seems a bit yucky. idk.
- made !size_of just be a compiler ffi function, i was already using it through a function wrapper anyway but now that doesn't have to be pointlessly specialized.
  it does mean you have to explicitly '::' it if you want it to be const known because I don't have auto folding yet.
- hack fix to ""!type not being Slice$i64, since constants are lazy now, just have the parser emit casts to a fixed identifier.
  sad that its 4 ast nodes for a string constant now (5k extra nodes in full test, ~2%, yikes) but its much less cringe than the old system where you had to manually do the cast or write a bunch of overloads.
  there was something weird about @as not resetting the type when I set the values to the expr which made it the unique type from rust String ffi.
- removed `program.intern_type(TypeInfo::Int(IntTypeInfo { bit_count: 32, signed: true })); // HACK: for ffi find_interned after moving IntType to export_ffi -- Apr 17`
  seems to work fine without now.

(PLAN)
eventually want to unify the different ways of doing @enum and maybe make the sytax be a block isntead of a pattern.
so you could mix implicit values and specific ones and specifiying the type.
Also switching to zig sytax where struct def looks like a block and then initilizing fields uses = instead of : would
be pleasing I think. have named arguments use = as well so there's symmetry.
(END PLAN)

## Finished new bc_to_asm (May 2/3)

- calling assert_eq wasn't leaving the v-stack empty at the end. lol Bc::Pop wasn't doing the right count, always 1.
- thats enough to get through 5/29, but I can't turn on COUNT_ASSERT so I don't trust that its actually working.
  yeah assert_eq(1, 2) passes :( i was replacing any eval that gave structured::const with Noop, but might still have runtime side effects.
  fixing folded ifs made that work, now 4/29 but i have more faith.
- why are so many things flat_call?
- fixing corruped types was enough to make hello world actually print the whole string!
- problem with slize size vs len. sadly i even rememeber deleting the code that got it right cause it looked weird.

Fixed aliasing result location shit so some tests changed:
// (structs) You'd expect this to be 1. It would be great if you break this test somehow.
// (structs) BUG. You'd expect this to be the same as above (b=3)

TODO: fn index doesn't work if not inline thats like something cripplingly wrong in the calling convention stuff!!!!!

- force spill before every loop fixed all except ones with floats (which i dont have yet) and parse_asm

I think my prize for getting this working will be `+=, -=, \*=, /=`. (did that, its just a macro).

Then maybe llvm ir text will get me normal operatiors.

## stack based ir (May 1)

I think my current bc format is dumb.
It tries to treat tempararies the same as variables in a weird way and uses MarkContiguous/slot_is_var to reconstruct that info.
It tracks slot types but I don't really trust that its always the raw ones and you only really care about unit/int/float, and i guess other int sizes eventually.
I think a stack based on would be easier, don't bother specifying ins and outs of each op, its just implicit flow.
Then you know temps are on the working stack and have var slots for vars.
Originally i was thinking llvm does infinite registers so i should too but there's no point if im not doing data flow optimisations-y stuff.
As long as im friendly about how I use the stack, like an if branch can't pop backwards and replace, it should be trivial to turn into llvm ir,
probably easier than the current one.

I think I'll ditch my current llvm backend regardless because thier c api is annoying.
It feels like it would be less code to just generate the text.
Maybe I want to commit to only having one backend in rust and write the rest in my language.
It doesn't work anyway and I'm not inspired to fix it cause the error message is always just SEGSEV so you can't even see the broken ir you generated.
Maybe thier cli stuff has friendlier validation, it can't be worse, and I could still link to thier c api and just call LLVMParseIRInContext if I want to jit it in my process.

So far looks like stack based bc removes a bunch of reserve slots stuff which is nice.

- confusion about the stack having addr of a stack slot vs deref of a stack slot. currently I don't have spilling.
- there's somethign weird where functions with a constant value body still get emitted asm so have to fix that later but it should work anyway
  its comptimes ones where the function decls get left in the body as declfinished so it doesn't know its safe to ignore the block body.
  oh and im keeping anything with annotations just in case but that inlcude #pub.
- lamo in my load after switching to trying to keep reg on stack instead of storing immediatly i forgot to move get_free_reg into the loop so a big store was putting all parts in the same reg (and stomping)
  that was enough to fix cases/corrupted_types.fr.
- GetVar emit_bc was just getting the address without doing a Load
- wasn't dropping x1 after flat call prelude but that wasn't the problem. I bet its that i dont restore args as free after a c_call, yeah that helped.
  oh and i was wrong about dropping x1, load() drops the ptr reg
- added spill support for when you make a call so your v-stack doesn't get stomped
  thats enough to get hello_world to run but it just prints one letter "H".
  thats very unfortunate. if its gonna be wrong it really shouldn't compile.

// TODO: dont have to do full spill for inline basic ops that only use fewer registers than args. have the annotation mean that too.

## working towards replacing 'enum Value' with bytes (Apr 30)

- Made Value::Heap just be the pointer. I have to track type anyway so i already know the length.
- can get rid of the hack about reconstructing slices
- c call go though ints instead of values
- reduce the big dispatch thing for turing values into asm consts
- can get rid of the values version of serialize
- need to get rid of type_of_raw because you don't be able to do that when its just bytes. but thats fine casue it was only used redundantly it seems.
  same for type_of(Value)
- switch overload set to tagged_index because i live in fear
- use a generic over InterpSend to create Expr::Value when the rust type is statically known.
  rn its worse cause it has to allocate the vec even for ints but makes a thinner interface to switch to byte repr and after that's done it will be better than the old way.
- have to be careful about which rust ffi types get wrapped in a unique and which don't. like i64 is the same on both sides.

Almost successfully changed Values::Many to be a vec of ints instead of tagged Value.
Only problem is tests/flat_call where its trying to pass a function pointer to the compiler as a callback.
What used to be a GetNativeFnPtr now gets serialized to an int and its a problem if you try to emit asm for one thats already been turned into a pointer?
Because I store both the funcid requesting to be a pointer and after the addr that is the pointer as the same FnPtr type because to the user program they're the same.

## ConstantData arena (Apr 28)

- The idea here is that unsafe language with no const pointer type yet,
  but it gets pretty scary if you mutate something the compiler expects to be constant (like an interned string).
  So put all the constants together and any time you build up a whole page, mark it as read only.
  So there's a chance that a program trying to do crimes will fault but I don't have to pay the price of adding checking to every dereference.
  Doing it with 16K granularity isn't really enough to be reassuring but mprotect is already ~10% of compile time because of the jit stuff, so i dont really want to do extra on every ffi call.
  I really need to work on a more clever page rotating scheme.
- I've dug myself such a hole with the lifetimes that I mostly just leak everything, so might as well put it in an arena and not pay for the book-keeping.
- I think I'll need to be strict about memory segments when trying to emit an exe anyway so might as well start now and have everything go through a central point.
- Allowing constants with internal pointers is weird for exe so being able to reason about whether its in the range and needs to be remapped seems useful.
  Then maybe this should store type info too because what if you just happen to have a large int constant in the range by chance.

ok fuck it, the parser isnt actually in parallel (i looked in Instuments) because it needs to wait on files at the beginning because it needs the whole top level before it can to anything.
and i cant add more threads because then the contention for the intern pool lock is too high and it gets way slower.
so no point in extra complexity of thread stuff.

## lox ch 1/2 (Apr 27/28)

Feel like i need to do some project in my language to see what the biggest pain points are to decide what to work on.
Gonna go through crafting interpreters again and keep a list of problems I have.

Did:

- more pointer math helper functions.
- print negative numbers.
- round float to int asm and print that. TODO: llvm
- inline the basic functions (like add) asm so dont have to do the jump.
- load/store for u8 for working with CStrs.

## out of order constants (Apr 27)

Currently constants can bind names out of order but they still get evaluated in lexical order.
So functions can reference each other becuase evaling that statement just means putting it in the overload set and bodies are done later, but normal constants can't.
Want to not do 'eval_and_close_local_constants' at the beginning, just do it when you try to read a constant for the first time.
I've already got the constants stored globally, so don't have to worry about needing to clone them with the functions.

- now treating 'Unit' as type unit even tho has type annotation? but it didn't even try to eval that const? im printing. its getting in consts map before somehow.
  oh parse puts empty tuple out as 'unit' so since i dont eval the const early, it doesn't get cast by the type annotation, and since there's a value there not an expression, it doesn't think it needs to bother declaring it.
  using '()!type' for that value fixes half the tests.
- checking for saved type being finished instead of just that the expr is Values fixes all but 1.
  I should make my handing of casting values more consistant maybe. Anything with unique types and overloadsets vs Fns is a bit fishy.
- the last failure is parse_asm `self.program.overload_sets[i].pending.is_empty()` but it works if you turn off that debug check.
  but still, why'd it change? i guess that makes sence, evaling the types of a function might cause a data dependency on some constant that causes a new instantiation when evaluated, that's probably fine.'
  just extend and keep looping until they're all done. it happens only for 'fn init' currently which is like pretty popular so that's fair.

damn that was like not at all painful. i guess i did most of the work before when i moved local_constants to hold names instead of expressions.
that's so pleasing, can finally just do shit. can put all my imports at the bottom :)
Would be nice if I fixed the template instantiation order stuff because now that feels really out of place.

That's also another 50% less make_lit_fn because those are for evaling expressions and now a bunch of constants get skipped.

The tangible benifit of all this would be if panic could print a stack trace but i think that will need fixing the calleees tracking for larger mutually recursive graphs.

made sure inline asm expansion on resolve_body not when added to the overload set. if you don't call it, it shoudn't bother.
it was. so not every program needs to jit the float functions every time. it happens in emit_body which makes sense.

## lazy parsing (Apr 26)

- was using .source to get a str to slice so it was taking the whole file not just my little span. which was fine before when i always did the whole file at once.

I was thinking i'd be reparsing for every resolve when they're nested because the same source text will get multiple numbers.
because when you clone the ast for resolve, it would stop at the nested function bodies and they'd get different index numbers.
actually i think it cant happen because even if a clone just clones the number, when it looks up the index, it wont recursively do work.
cause only the parser adds things to the list of tasks, the resolver doesn't trigger work, it just waits for existing work to be done.
So no matter how much you clone the numbers, you can't trigger two paths parsing the same thing.

## fewer fn clones (Apr 25)

- doing the comptome memo lookup before the clone makes Consts.log go 2200 lines -> 1800 lines. saves 200 States::fn_body_resolve
- previously quick eval always failed if it was a comptime_addr in the compiler because the FnBody wouldn't be in ready. make_lit_fn 2423 -> 2346. meh.
- UInt/SInt seems to be the main thing not in 'ready'? oh im dumb, its once per run: the first time they get called for a const, then after that they're ready and can just be a jit call. that's not so bad.
- operator_star_prefix is often an overload set not a function.

quick eval overload set with no pending and one ready: make_lit_fn goes 2346 -> 1465.
can't do it when arg is macro !addr or !slice becuase then you're trying to const eval pointer onto the stack frame.
the only place that shows up is my inline asm where i always pass the slice of insturctions to something that allocates it as a list to return.
discovered by skipping (name == "lst" || name == "heap" || name == "fcmp_with_cond" || name == "hackheap") by trial and error.
its kinda handy that any non-bootstrapped inline asm bug breaks tests/floats cause thats such a random one to not work.

so thats ~40% less lit_fn and made debug mode --no-fork ~5% faster.

// DID: just treat @builtin as a normal expression instead of a magic thing that const looks for so you can do 'const Type: @builtin("Type") = @builtin("Type");'
// then you dont have to eat this check for every constant, you just get there when you get there.

// DID: allow macros do add to a HashMap<TypeId, HashMap<Ident, Values>>,
// to give generic support for 'let x: E.T[] = T.Value[] === let x: T.T[] = .Value' like Zig/Swift.
// then have @test(.aarch64, .llvm) instead of current special handling.
// also add a new annotation for declaring macros with some other type as the argument where it const evals the expr before calling the macro.
// that should be doable in the language once I allow user macros to modify functions like the builtin ones do.
// -- Apr 19

## dont resolve vars up front (Apr 24/25)

did a bunch of refactoring the scope system yesterday.

You get to a problem where you can't call a comptime function,
because you can't resolve the overload, because you need to know the types,
but ive paired resolving the argument names with the argument types because what about when you're allowed to refer to previous arguments.
Can't go back to @comptime args not being const because you want to use them in types of declarations in the body, if anything i want to get rid of @comptime.
Maybe start with the easy @impl case where you know the types without binding the args.

So answer is have resolve_sign not bind argument names, just resolve the type expressions, which works for anything that's not @generic
because you can't read argument vars in thier types. Then had a very long problem of it trying to do an overload on the template
version of functions in an @impl @comptime, so they would never be bound. but it also wasnt finding the specialized versions.
needed to add an extra block around args because all the args and constants were going into the function's outer scope,
so specializations would shadow which is an error so the resolve would fail and I think a mut_replace! somewhere in the
overloading so it lost the function.

then little problem of now generic args dont get renumbered.
i'm declaring args during resolve_sign becuase I wanted to have later args depend on values of previous,
but my tests don't use that and the whole type checking system isn't super setup for partial argument types where you need to
know previous arg values for type of later ones. tests just use generic return types so probably fine to limit to that for now.

## stricter closure capture handling (Apr 22)

:ChainedCaptures

trying to have stricter rules about captures. started by marking const args as const vars in resolve binding,
which caused two tests failing (before this check) on Missing resolved variable. with this check six fail. but this really shouldn't be happening.
seems the problem is when you call a captured const Fn. but i dont see how it worked before becuase it still must have looked up variables in the rusult for set stmts.
maybe in emit_body binding the args? the pattern used to say it was a var so it would get added as just a tpye and the constant would get found somehwere else latter?
Actually I bet the problem is in type_of because it doesn't happen in test/closures, so feels like it could be
just when complicated generics means it has to backtrack to infer types so hasn't properly got to the point of dealing with the closure but tries to type check it too soon?
but that doesn't make sense because im hitting the error in capturing_call. surely I don't get there before doing the rest of the body before it?
I wasn't adding closed_constants in bind_const_arg but that didn't help.
Oh the other thing is expanded macros don't recompute captures, which might be why changing Option::if to not use @match helped.
I would make sense if it was broken for fn stmts because of constant hoisting trying to compile the body before, but that doesn't happen because they become an overload set.
its always Expr::closure values that are broken.

I think there's some situation where it only figures out that it needed to have captures too late so its already created a new result.
Like you're doing a capturing call and then you pass a closure into another function as a const arg,
but it doesn't realize that function needs to be a capturing call until it gets to the body where it calls the argument?
OH! I think when there's a chain of passing const Fn through multiple capturing_call, the first time the argument gets bound,
it assumes the capture was handled when it might be from an upper caller so should have stayed a requirement of the
newly specialized function. am i doing it from the inside going out instead of top down?
bind_const_arg just extends capture list.

Spent such a long time on something that should have been easy.
Feel like I need to be more rigorous in making the compiler easy to reason about.

## scan ahead to resolve constants (Apr 22)

(plan)
I want to change how identifier resolution works so its breadth first instead of depth first so you can have out of order constants.
So I want the resolve pass do be done gradually as part of the compile pass.
So you do a whole scope to get names before going into implementations and doing thier names, so you can skip resolve if the code never runs.
The more interesting goal being having a normal macro that says "oh you're trying to compile me? i'll parse this header file and spit out all those names into your scope".
I think my existing broken (they can't even nest) module system is more work to keep than remove and rewrite a better one later.
Ideally a module could be just a struct of functions (like zig) but then an @using macro can add those names to your scope.
The other weird thing currently is constant hoisting.
All constants in a function are pulled out during resolve so macros can't expand to blocks containing constants.
But I do the resolve pass doesn't backtrack so normal code can only refer to constants lexically before it because otherwise the names wouldn't bind.
So its the worst of both worlds.
Also expressions that disappear because of constant folding and shouldn't even be compiled,
still have thier constants hoisted to the function so you can't use that for platform specific stuff which is sad.
I want `#[cfg(whatever)]` to just be a normal comptime `if`.
(end plan)

- removed modules. easy. only a toy test relied on them.

doing scan ahead in scope: (end result will be can bind to later declarations but stil generally can't use them because compiled in order).

- needed to move `const FatExpr: Type = @builtin("FatExpr");` up above `#include_std("compiler_macros")`.
  I guess it worked before because the builtin identifiers get checked if a name is unresolved but now it gets resolved dispite being declared later,
  but they're stil evaluated in order so it doesn't exist to get closed over?
- Then my `const T = T;` hack in comptime functions that fixed some old renumbering bug (i think), breaks now because the value T binds to the new declaration T.
  But i seem to have fixed things enough that sometimes making the arg constant works without the extra binding (even if still seems redundant if the func is comptime but since inner constants need to refer to it, makes sense).
  Thats enough for half the tests to work.
- switching memcpy to a trailing lambda instead of const binding make another quarter work. need to go back and fix that.
  it can't find value for the T in fn set's signeture?
- now a bunch of no stomp assertions are triggering in comptime functions cause I took out the rebinding hack. forgetting to renumber somewhere?
- adding renumber to emit_comptime_call broke everything. had to add handling for functions. was forgetting to remap function names and closed vars.
  so it couldn't find like outer comptime args (or anything at all at one point).

ahhhh such painful mistake (err below).
When compiling an if, I require the args to be functions but I always inline the call, so its a problem if I try to compile twice, cause the second time it wont be a call.
(interestingly that means if branches always have thier own constants lists so what i said before about not being able to do platform specific stuff was wrong).
But I originally solved the problem by `arg.ty = Unit` and then if it was unit instead of unknown at the beginning, I'd know we'd done it already and just return.
But because I'm dumb, i'd always return unit as the type of the if expr. Because I did that for while loops and it made sense because they don't return a value.
and apparently only now with my ordering changes, you hit an error in a type infer somewhere and end up trying to compile and if that returns a value twice.
so you get a sanity ICE because it says its a unit but last time you were hear it was some useful type.
now my fix is check if its a function type and if not, just typecheck the branches are the same value.
which fixes my current tests so yay, scan ahead works.
HOWEVER ITS WRONG! now you can't have an if expr that returns a function. TODO: write a test for that and actually fix it!
maybe just add an extra marker to the end of the macro args that its a value if not a function if. or change the macro to ifx.
theres no real reason it has to be functions as arguments, I just think thats more consistant because one of them isnt going to be evaluated.
could have the macro !if just be expressions and generally call the function if that takes lambdas once type inference is more reliable.
now that macros are a more normal part of the language, its not that weird for an argument to not be evaluated.

```

sanity ICE. Type check expected Ty2 = Unit but found Ty465 = { Some: i64, None: Unit}!enum
--> main_file:84:71
│
84 │ (self.is_some(), fn() O = then(self&.Some[]), fn() O = else())!if

```

So the point I'm at now is that you can resolve to constant names in the same scope but lower down.
However, its not useful because I still evaluate them in order, without any clever dependency chain analysis.
So you still mostly have to have your code strictly ordered.
Even functions which would almost work because body isn't compiled until needed can't call out of order because the declaration needs to close over the overload set which might not exist yet.

## improving macros (Apr 21)

Syntax sugar for !quote/!unquote goes a long way.
Now its not too bad to put the whole logic in an unquote so I can use that to make lexical scope work instead of @with_var which was really annoying.
TODO: Still can't do constants because of hoisting tho which is sad.

from a long time ago:
// TODO: dont do the builtin ones this way. put them in thier own function and expose them as @ct @comptime_addr() @call_conv(RustPrefixMacro) === fn name(&mut self, arg: FatExpr, target: FatExpr) -> FatExpr;
// then you don't have to eat the these checks every time, you just get there when you get there.
// plus it makes the transition to writing them in the language smoother. until then, the only cost is that it becomes a virtual call.

started doing that. did it for @enum.
can't do it for @as yet because I use that in quoted things without @literal so it might not be closed over but I can't invoke an expression as a macro yet.
I can't decide if the destinction between var resolution and captured const values makes any sense.
Ideally the vars would always be unique so maybe the extra level of dynamic scope is just because I wasn't rigorous enough with renumbering originally and now its hard to reason about.
The current thing means a raw identifier is never useful sincei t can't be from the caller's scope (wouldn't resolve) and can't be from the macro's scope (doesn't capture).
But its applied inconsistantly if caller they happens to have already captured it, you won't get an error.
I guess its the same problem as rust letting you use relative import paths in macros so I end up needing to import stuff when I add InterpSend to a new file becuase I'm too lazy to actually fix it.
added @(e) for comptime overload set, but its still a bit broken. works for as so at least its slightly more consistant now.
feels like it made the compiler like 10% slower which is sad but also not worth bothering about until i fix the thousands of redundant ast serilizations.

Really need to stop doing the stupid export_ffi wrappers. got confusted by pasing the wrong macro impl and of course args matched cause they're all exprs so error message wasnt obvious.

## getting rid of interpreter (Apr 20)

So end goal is for comptime repr of values to be the same as rust repr so I don't have to do any serialization when you call into the compiler.
But that's such a massive change that would be so painful so doing it in tiny steps and keeping the compiler functional throughout the transition.

First step was making all ffi calls in macro handlers go through an array of integers instead of an array of interp values.
You shouldn't need the runtime type (rust enum) tags since you know the function signetures statically.
That's what the flat_call calling convention does. Just pass in argptr/len and retptr/len and have both sides serialize the values out.
But helpfully the int repr is what my asm uses already so that side is a no-op. (most of this was yesterday).

- Seperated out the macro_msg impls from the serialization code.
- Required some ugly hacks to dereference pointers before going into the flat call because interp stack pointers aren't real pointers.
- Mistake with enum padding being added twice because of the double serialization.

So after that, we're at the point where all the interesting ffi stuff is happening in native code.
The interpreter is only doing moves and calls, which asm can do on its own.
The interface to the interpreter is really thin, its just 'fn run'.
So now allow TargetArch::Aarch64 at comptime and replace interp run with emit_asm and then jumping to the jitted page like you would for runtime calling main.
Now that ast functions are in asm, its a problem that I can't do large structs so had to use flat call for internal things too sometimes (not just compiler ffi).
Really need to implement a less stupid cc than that at some point.

So that kinda worked but caused my favourite bug.

## (plan) passing by pointer (Apr 14)

Llvm doesn't do all the c ABI stuff for you. Need to use pointers to pass structs by value.
Other backends are likely need the same thing.
Currently my asm one just hopes all the args will fit in 8 registers but that doesn't follow the abi for calling other c code
so needs to change too.
So it would be nice to do it on the bytecode representation instead of in the backend,
but that gets a bit sketchy because what if different abis have different rules about when to use registers vs pointers?
Like the aarch64 one sometimes passes on the stack without passing that pointer in a register (because the caller knows where to look).
(https://learn.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions?view=msvc-170).
Tho following the calling convention only matters for ffi but if I have to support it anyway, why not do it all the time.

## burn it with fire (Apr 10)

Code is evil. I want less of it.
Go back to commit "remove unused experiments" when ready to try again.

## Planning a Linker

- calls
- jump tables for switch stmts. (a-b)>>2
- function pointers
- const vtables
- imports
- global vars @PAGEOFF
- extern globals @PAGEOFFGOT
- `___stack_chk_guard`

> Note: qbe doesn't give thread-safe vibes. There's a lot of static vars. Probably part of why they don't distribute as a lib.

## Planning Casts (Apr 9)

Currently @as sets the result location, and stops inference from going deeper, so you can use it for resolving an overload.

- But it doesn't actually change the type returned by compile_expr so its not as powerful as specifying a variable/return type.
- And it doesn't do a type check (because that's the point), which means `let a: i64 = @as(Str) 1;` is legal (and a no-op) which is kinda dumb.

Doing casts as annotations works for helping overload system but its weird to not be a function cause they really are.

- `as(Str)(e)` feels like too many brackets but `as(Str)$e` is the same as the `@` one.
- Then `as` could be a comptime function returning the cast function.
  - `fn as(const t: T) Fn(__any__, T);` but there's no type I can put for the input.
- Really `e.as(Str)` would be better but means a different thing.
  - That sounds like evaluate `e` as whatever type it is, and then convert to `Str` instead of giving that as the result type.
  - Maybe you want `fn as(e: T, const T: Type) T = e;` which would match the current `@as`.
    - But I can't do that with my current generics yet.

(want different types of cast, just using `as` for example).

## new test runner (Apr 6)

at first i wrote a dumb serial one which was fine but 3x slower than cargo test.

- what the heck is happening.
- its doing something for a really long time before calling main but after it claims to have run the exe when
  calling through cargo run but if i just do it directly its fine. 0.5 vs 1.3 seconds
- but cargo build and then run it doesn't help. its just slow the first time you run regardless.
- it would make sense if it were like getting the files from disk thats later cached but its not even running main
  (i printed something at the beginning and flushed stdout)
- same behaviour in release mode
- oh damn turning off the llvm feature flag makes it fast again.
- so the reason i didnt notice before is most of my tests were in the compiler crate, not the llvm-backend crate so that didnt include it.
  and the i did see that my llvm tests took longer but i thought that was just running llvm is slow not... the existance of llvm is slow.
- so like maybe its because llvm is doing initilization shit but then why only the first time? so not that
- then must be because the binary is giant (9MB vs 69MB)? so it has to load the whole thing into memory? or macos is dumb as fuck and like sending a hash somewhere to check for viruses??
- speaking of which how the fuck is it 9MB even without llvm?? ok release is 2.2 which is better.
- looking in Instruments, its spending the time in dyld4::prepare so i guess it is the linker thing.
  so like am i dynamiclly linking? theres no way, my exe is so much bigger. loader doing offsets?
  and like maybe it can cache that for latter if i run the thing again immediatly?
- oh it probably doesn't even get a new address space. ok science time.
  nah. 'println!("main addr: {}", main as usize);' is different every time so not that simple to understand sadly.
  i also tried on a function from llvm c api but its also different every time.
- but like also, even without llvm its 0.195 vs 0.077 the second time you run it (release mode).

/// The normal cargo test harness combines the tests of a package into one exe and catches panics to continue after one fails.
/// It doesn't expect you do be causing segfault/illegal instruction signals, (which is generally a reasonable assumption but less so for my compiler sadly).
/// So when a test does that it kills the whole thing instead of being recorded as a failing test and continuing.
/// Instead, I start each test in its own process so no matter what happens, it can't interfear with the other tests
/// (unless they're like trying to use the write files or something, which like... don't do that then I guess).
/// My way has higher overhead per test but it feels worth it.

## VoidPtr & overloads (Apr 5)

It's cringe to make the language more verbose just to make the compiler, but void pointers aren't what I want anyway.
The only reason they exist in the first place is that my type system is too shitty to express things I want.

- Recursive types
  - Current use is linked list of stack frames for unwind.
  - Didn't feel important because I'm used to rust where lifetimes make it painful anyway but obviously need it for ast nodes soon so idk what i was thinking.
  - Require pointer indirection so you know the size. Need to be able to reference the name while making the type.
  - Mutually recursive should be allowed too (like stmt/expr).
- Use const args in return type even when the whole thing isn't comptime
  - current example is alloc which should return a Slice(T) instead of a VoidPtr
  - means I need to support const args in split FuncRef too. so just do both specializations I guess. Should notice which never get called at runtime anyway (macros).
- raw_slice used to impl real Slice is probably fine just being a cast there
  - There's a hacky test in generics.fr with tuple layout that I want to make unspecifed anyway
  - MySlice generics.fr is more reasonable but its fine to make that more clunky.
- literal_ast needs const arg used in later arg type except that it's a builtin function where you don't actually want to generate multiple bodies.
  - but currently used through a macro as a work around anyway

My overload change also made ""!type vs Str more painful. I really need some auto cast subtyping stuff.

Looks like it was a bad change but really its just revealing latent problems that jsut happened to pass because I have small enough programs that it just worked out.
It feels extreamly important to actually resolve overloads robustly and not just pick the first.

## How small can it get?

```

RUSTFLAGS="-Zlocation-detail=none --remap-path-prefix $HOME=~" cargo +nightly bloat -Z build-std=std,panic_abort -Z build-std-features=panic_immediate_abort --target aarch64-apple-darwin --release --no-default-features -n 50

```

Surely I can do libffi in less than 30KB given I have to do it anyway for my own asm backend.

## const args (Mar 28)

You want Fn and Type to be comptime only so when you pass them as an argument it generates a new version of the function specialized for each callsite.
But some comptime functions want to be able to work on those values without generating a bunch of redundant versions of themselves,
because they just do stuff to the compiler data structures, they don't actually use the type/func in thier body.
So maybe it makes more sense to say you have to be explicit about the difference
and declare the argument 'const' if you want to call the function or have a variable of the type.
Which is nice because its more clear when calling a function will slowly bloat your binary.
Plus it means I don't have to deal with some weirdness around which capturing_call args need to be added to 'constants' vs 'vars'.
So makes the compiler simplier and kinda has a justification in the language.
I think that was a win, I had to add it in like 6 places and got to delete some compiler code.

## llvm agragate returns (Mar 22)

LLVM can't return structs by value.
Seems like clang will return a vector for two words but use pointer param for 3 or more (https://godbolt.org/z/qGWvbf4K4).
I probably want that for other backends anyway so perhaps I'll make that a pass on the AST.

## trying to make an lsp

- "expected tuple of two elements found map": request is (id, params) but notification is just params.
- symbol provider vs highlighting vs colours vs semantic tokens.
- Kinda works in Kate, it will give colours for "Data Type" for ENUM, comment works, but you get "Normal" for everything else regardless of what you ask for (those work in theme editor).
- Dot completion works in Kate tho.
- Maybe kate is just broken, so want to try in vscode but you cant just connect to an lsp, you have to make an extension that just delegates to the lsp.
- That just spews errors and doesnt show my guy in the drop down of logs.
- Of course the magic command that's the only way they tell you to setup a project targets a version newer than it is possible to install, silly me!
- So now I can see my stderr when selected in the drop down so I know its answering the requests, but it still doesnt show colours or dot completion.
  It works less than Kate so the problem must be in my extension not my lsp.
- problem was not telling it which TextDocumentSyncCapability i wanted. kate just picked one i guess.
  But now it works and I do get colours, so that part at least was just a kate problem.

## struct init as function (Mar 16)

// The above is a macro that expands to:
// const Person = (age: i64, gender: i64)!struct;
// fn (age: i64, gender: i64) Person = (age: age, gender: gender)!construct;
// Note, this is not a ~constructor~, its just a function that happens to construct a value of the new type.
// There's nothing actually special about the function, its just a convention that seems nice.
// I think constructors in java/c++ are a bit weird because you have access to a 'this' pointer before you've actually produced a value in a valid state.
// Instead, here you are forced the figure out all the fields you want and put them in place all at once.
// TODO: currently (a: b) === (a: b)!construct and there's nothing forcing someone to actually call your nice factory function (private fields, etc.).

## Dot call syntax (Mar 15)

Syntax sugar for 'o.a(b)' === 'a(o, b)'

number literals need the brackets because they lex as floats. thats easy, not ambigous, just backtrack if its not a number after the dot.

Having a callable in a field will be ambigous.
I think prioritize dot calls, force brackets for fields, and use macros to generate wrappers for things with vtables.
You want them to be special anyway to get intuative single dynamic dispatch like c++ virtual, java, rust trait objects, etc.
They need to look up the function pointer to call in the vtable, but also pass themselves as the first argument.
Lua handles that problem with 'o:a(b)' === 'o.a(o, b)'. So you can only have the infix syntax if you're doing dynamic dispatch.

```

fn log(self: Ptr(i64), important: bool) Str = str(self[]);
fn log(self: Ptr(Str), important: bool) Str = self[];

const Loggable = (
vtable: FnPtr(Ty(VoidPtr, bool), Str),
dataptr: VoidPtr
)!struct;

fn log(self: Loggable, important: bool) Str =
(self.vtable)(self.dataptr, important);

fn upcast(self: Ptr(i64)) Loggable = {
const log: Fn((Ty(VoidPtr, bool), Str)) = log; // Somewhere to hang the type annotation to resolve the overload.
(dataptr: self, vtable: log)
}

var hello = "Hi";
assert_eq(true, str_eq("Hi", hello!addr.log(true)));
var n = 24;
assert_eq(true, str_eq("24", n!addr.log(true)));
var unknown: Loggable = hello!addr.upcast();
assert_eq(true, str_eq("Hi", unknown.log(true)));
unknown = n!addr.upcast();
assert_eq(true, str_eq("24", unknown.log(true)));

```

## Thinking about continuations (Mar 11)

It feels like you could get a bunch of features for free if you had a good way of
referring to what happens after a function call.
Like return is really the same as throw/panic, just you know its only going up one stack frame.
Generators, async, and coroutines are like that, but you can go back and forth multiple times.
Return is a lot like a tail-call to your caller and while loops can be trivially expressed as tail-calls.
I was thinking you then have to optimise out the tracking of where to return next, but normal languages already do that.
Really lr is a function pointer that you tail call when you return.
I guess you have to use fp if you want to unwind multiple frames, but you want that anyway for debuggers.

```

const Cont = .{
returnAddress: VoidPtr, // lr
resultAddress: VoidPtr, // x8/&x0
stackPointer: VoidPtr, // fp
};
var return: Cont;

```

If the return value is big, the caller wants to give you an address as an argument,
and you don't pass args to return. The normal case where you return a value in x0,
even matches the c calling convention where you pass the first argument in x0.
It fits so well, maybe this just something obvious that everyone already knows.

So you need a way to talk about stack-frames at the call-site.

- call: push a frame for the callee
- return: pop your own frame
- self tail call: leave frames alone
- throw: pop frames until you hit a catch block
- yield: pop one frame but store it somewhere for when you get called again

For generators, if it's not recursive and doesn't escape, you know how much stack space it needs for itself and all internal callees,
so you could put that space in the caller's frame so the generator's locals can live across calls.
Even if it does escape, there might be a statically known result address that gets passed in, so you could construct it in place.
Probably don't want to allow moving closures anyway because then you can't take pointers to locals.

When you call a function, you pass as an argument the function pointer to call when it's done.
You need to include how to adjust the stack frames, but in the common case you know statically.
Separate where to reset the stack pointer to when you're done from where you're allowed to grow your stack into.

## Cleaning up fn addrs (Mar 5)

For the libc things where you need to get the addresses at comptime,
make a list of signatures/pointers and then treat calling those pointers uniformly with inline assembly.
That also gives a place to put annotations saying you want to dynamically link for when I want to output real executables.
So you can have one FuncId with `@dyn_link` to call at runtime and `@comptime_addr(_)` to call at comptime.

## learning about outputting executables (Mar 4)

- https://en.wikipedia.org/wiki/Mach-O
- https://github.com/horsicq/XMachOViewer
- https://github.com/aidansteele/osx-abi-macho-file-format-reference

So there's a command for telling it which address to start at,
and that's like an offset into the virtual address space you've loaded the binary into?
You have load commands that map the bytes in the file to where you want them to be in that virtual address space.
Everything is relative to some base address the os chooses for you when the thing actually runs.

There's an indirect symbol table where you list the names of the functions you want the loader to give you.

Every time you want to call one of those dynamic functions, you jump to a stub like

```

adrp x16, #0x100004000
ldr x16, [x16, #I]
br x16

```

where I is the offset into the indirect table.
and you carefully put those in a named **stubs section, so it can patch them in after the first call?
That adrp thing is the address (encoded as an offset) of the page that the loader filled in all the pointers you asked for,
But how does it know where I expect them to be? Oh, you have a **got section, and it fills that in.

## improving inference (Mar 3)

Instead of LazyType::Infer just becoming Any and leaking around everywhere,
it needs to be legal for a function to have partially known types for longer during compilation.
You want to remember that we don't know the return type yet, then try to compile the body without that information
and stick whatever you get in as the return type.
So there's a few of places (like if/while) where I used to just unwrap_ty, but now I need to
infer -> get the arg -> check it -> emit the body -> then either check ret or set it to ret.
It gets wierd with closures, you can't always just compile a function any time you see it,
have to wait as long as possible?
I need to fix resolving overloads so if it sees something with an inferred return type
it just tries to compile the function (should be since it must not be a closure),
right now it just always skips those options unless someone else somehow caused it to get compiled already.
Or for now just... don't have inferred returns in generic impls.
But it does work, infer_types_progress no longer lies and says "we do definitely know the type, its Any!"

The other popular place that uses Any is assert_eq.
But changing it to have a bunch of overloads breaks some places that can't infer which to call.

- one was my weird generics test where I was treating a tuple of types as a tuple of ints: just fix the test.
- !tag: easy, Ptr(i64) for now
- `assert_eq(no.None[], unit);`, I guess that should be legal, it's an easy answer.
- @At macros, i guess you really need to expand them to get the type. but now we're going deeper into the territory of type_of
  being the same as compile_expr. which really it already was, so maybe I should just stop pretending they're different things.
  That leads to a wierd thing of do you decide which function you're calling before or after you compile the arguments.
  And I guess its just try one then the other and if you can't figure it out it's an error. Maybe that's fine.
- bit_literal. painfully special cased because I already have the function for that since its type is right there and
  I don't want to commit to doing the comptime call right away.
- `assert_eq(0b101, 5);` right so I guess I should lie and say i64 is the type because I don't want to deal with
  real subtyping or assert_eq for all eq and eq for all ints yet.
  and it doesnt break the bit length checking for the assembler functions so seems fine for now.
- `assert_eq(25, (fn(u: Unit) i64 = { 25 })(unit));`. Have to actually look through the function expr and try to see its type.
  Again that's unfortunate because that might mean really compiling it so would be nicer if type-checking were defined that way.

## quest for an assembler friendly language (Feb 27)

Bit syntax.

- dumb mistake of treating 0x0 as 16 bits not 16 values.

Enum constants (what java calls an enum).

- the macro emits a pointer to the struct instead of the struct itself,
  becasue the interpreter guesses that it will only need one stack slot when there's no type annotation.
- hacky special case for addr of constant pointer where it just gives you back itself instead of its address.

// I've heard it said that quickly selfhosting leads to a language thats just good for writing compilers... that's exactly what I want.

## spliting sema (Feb 24)

I want to do a jit asm backend and use that for comptime instead of my dumb interpreter,
so i guess should start by seperating the stuff that emits bytecode from the comptime rewriting the ast.
They're clearly two seperate things happening together in compiler.rs currently.
Then it should be less painful to add more backends.

## serializing asts

- fix logging of recursive types
- my InterpSend for option always said None cause im dumb
- need to make sure everything has a staticly known serialized size. so padding for enums and vecs as allocations.

## the road to partial evaluation (Feb 19)

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
Fuck, it's on a function labeled `// TO-DO: this needs to be an error`.
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

## notes.txt (Feb 12)

Value Repr

A tuple is a value type. Some range of memory on the stack/heap.
Passing a tuple to a function is always conceptually a memcpy from the old location into the callees's stackframe.
Some types are Move but not Copy so passing by value invalidates the original data (Drop will not be called on the original).
Move is a semantic construction, it doesn't actually change the calling convention.
Structs have the same repr as tuples, just with syntax for accessing named fields.
All scalars (numbers, ptrs) in the interpreter take one Stack Slot, a tuple is a range of stack slots.

Variables

runtime cvar capture is by just inlining the function.
comptime var capture is by copying it into the function object (always immutable).
const means constant to the function's compilation.
comptime functions can still have let/var.
types and closures fns can only be in consts or in comptime variables.

Other Notes

// opt: pass by &const if not mutated. Have some way to assert that in the function def so you don't accidently cause copies.
// But I don't like Zig's way of forcing you to writing a whole line assigning to a new variable.
// LAMO https://github.com/ziglang/zig/issues/16343
// Maybe like rust where you just write 'mut' on the arg but don't add a keyword.
// Distinguish between wanting to mutate the caller's value (explicit pass ptr),
// and wanting to mutate your copy. Make sure you can't make the mistake of taking address to mutate and getting caller's.
// Distinguish logical mutation (like through a pointer) and reassignment (where you change what's in the stack slot)?

// Goal: I want to not need the linker.
// If you don't want to interact with other languages you should be able to run my compiler and then have a program.
// Go's compiler stuff seems pretty good, that might be a good one to steal.
// https://www.youtube.com/watch?v=KINIAgRpkDA&list=PLtLJO5JKE5YCcdJGpmMqB8L4C4VNFN7gY
// If I use their stuff, be careful about too many calls between rust and go...
// But I assume c abi is only slow if you're trying to use goroutines which I don't care about.
// - https://shane.ai/posts/cgo-performance-in-go1.21/
// - https://www.reddit.com/r/golang/comments/12nt2le/when_dealing_with_c_when_is_go_slow/
// I need to be very careful about sharing common work between similar backends if I want to pull off having so many.

// I want to keep the interpreter using fully linear types in debug mode,
// but have them happen to work out as a normal stack so you can have a fast mode that just skips the redundant stuff.
// I wonder if I would then be easy to do wasm since that's also stack based.
// I'd be fine with tweaking my bytecode a bit to make that work better.

// unify function statement and closure expressions?
// but there's a difference between one you release to the outer scope for overloading and one that's just a value you can pass around.
// might be a good idea to allow a name on closures just to let it show up in stack traces for debugging purposes.
// but then its less clear that it's different from a possibly overloading function declaration.

// Overloading and Generics: maybe distinquish between a function template and a bound function.

## parser rewrite (Feb 10)

Normal assign left can be a tuple for pattern match-y assign multiple at once (like rust/python).
But that means you can't put an annotation there because its ambigous if the tuple is names or a call on the annotation?
@a (x, y) = z; but no, because @a(x, y) = z; is illegal so what's the problem.

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

```

```
