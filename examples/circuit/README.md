- <https://franca.lukegrahamlandry.ca/?file=examples%2Fcircuit%2Fgui.fr>

this is a logic gate simulator. it understands two forms of circuit programs (`defs` and `tc`). 
circuit graphs are compiled into franca code and then jitted. 
there's a very ugly visualization that's mostly just enough to tell what's going on while debugging it. 

## `defs`

a textual language for defining circuits (kinda my own shitty version of what verilog is). 
[defs.fr](./defs.fr) has an example of two 8 bit cpus (my solutions for `[overture, leg]` from TuringComplete.Game). 
there are assembly programs for those cpus that solve a simple maze from a level in that game 
(input `[wall, empty, exit]`, output `[move forward, turn right, turn left]`). 

there are only a few primitive operations:
- nand: output is false if both inputs are true. 
- delay: normal wire variables propagate their value instantly. 
  this outputs its input value from the last tick. its the only one allowed use-before-def cycles. can be used to make memory. 
- byte_make/byte_split: represent 8 bits as one wire variable.
  this doesn't add any computational power, it just simplifies the circuit definitions so you can tell what's actually happening.
- switch: outputs its first input when its second input is true. basically an and-gate
  except that it allows multiple definitions of the same wire value as long as only one is enabled on each tick. 

it's all built up from those and simulated faithfully so you can click into sub-circuits and see the values in the wires as it runs. 

## `tc`

- <https://turingcomplete.game>
- <https://github.com/Stuffe/save_monger/tree/22fa398d95c3d3a0b5e88a80e9daee613cab1823>
- <https://github.com/LukeGrahamLandry/TuringCompleteSchematicArchive>

circuit.data save files from TuringComplete.Game (v6 / 0.1059 beta). 
those files have the positions of wires/gates which i convert into the same graph representation as the `defs` language. 
in this mode there are many more builtin gates (ex. a 64 bit xor compiles to one cpu instruction, etc.) so will run much faster than the `defs` one. 

it works well enough to run my overture/leg in the maze and a few things from the schematic hub (mandelbrot and tetris). 

## TODO

- devices
  - missing: network, hdd, sound
  - console: wrong font
  - keyboard: character mode param. click to focus instead of a keybind. 
  - display: rotation
  - probes (they render internal values on the outside of custom components)
  - latency ram, Stack
  - sprite display: positioning is wrong
  - dot display: wrong default colour
- Mul64 high on wasm
- bidirectional pins are sketchy 
  - makes the visualization confusing because i always inline the whole custom component
- assembler
  - expressions with spaces in them are wrong
  - what happens when a label is definied multiple times
- need a better interface for choosing circuit/programs, showing memory contents
- pre_read mistakes would be a lot easier to debug if they were made explicit seperate operations in the trace display. 
- dropping file doesn't work in franca-web-playground (even when fixed will be annoying because need to feed it all the custom components individually)
- needs the franca library folder available at runtime
- "str scratch frame"
- from schematic hub
  - screenvurture: "H" as char literal in expression
  - ASM computer 64 bit: Hdd, RamLatency
  - matrix display playground: fails safety check
  - ploter: SR-Latch: use-before-def ("Allow circular recipies")
- the game has a setting ("Allow circular recipies") to idiom-regognise a few shapes of circular dependencies and convert to (AndOrLatch,NandNandLatch,NorNorLatch)
- i don't think im doing the keyboard right. ex. mandelbrot is behind a key sometimes?
- use f64 for pos/zoom in ui because it gets jerky scrolled down far in the trace for big circuits
- finish leg: memory, stack, call, shifts
- make the defs language less painful
  - number literal instead of const()
  - nested expressions
  - variable names
  - speed mode that cheats and implements components with native instructions (like tc) instead of nand gates
- tc: at top level sort by type of IO maybe so don't have to add random offsets in the evaluator depending on the arch. ie in my overture and leg input and output are swapped because the order in save file is arbitrary. 
- compile a version with save_wire_values so can get rid of the interpreter? 
  (when running the defs.fr one. tc import will probably always do that)
- fast mode that doesn't even save_wire_values once per frame
- ui: allow copying the code to clipboard
- `Main :: import().examples.circuit;` doesn't work in the web demo
- pressing l to fetch archive doesn't work in web demo
  `panic! Assertion Failed: jit_addr too early: perform_syscall__12311`
- nice ui for web gallery from my archive of the old schematic hub
- i don't really care about ui for visually editing the wires
  but i have a bunch of code for converting positions into graph 
  and rn the only way to test it involves opening Steam which is a pain, 
  so maybe i have to give up and have my own shitty version too. 
- option to do scenario test on circuit.data
- less trash code. builtin.fr and gpu.fr are particularly bad.. 
- deduplicate error checking between parse and load
- handle all load errors gracefully. show in ui instead of log. also still try to show the circuit diagram just don't allow running it. 
- don't spam "multiple defs must be switch" for bidirectional pins
- show wire comments/colours in the ui
- auto throttle tick rate to keep a sane frame rate even if you ask for something too high. (also report number in Hz instead of per frame so it's easy to compare to the game)
- :NotReorderingTheseBreaksOvertureLegNandverture
- don't be checking names of components all the time (at least also check the ids are in the builtin range)
- it's annoying that you can't see the wire values if it halts on a tick that was evaled in fast mode. 
  delay all writes to the end somehow so can rerun the tick in slow mode after knowing it will halt?
- use 32 bit instructions instead of bit_and(0xFFFFFFFF)
- arm has nice merged (and,or)+shift in one instruction
- something that dumps raw circuit.data as readable-ish text
- option to see generated source or -d logging in the ui
  - ^ would be nicer if i factor out examples/gpu/terminal.fr into a reusable textbox widgit and use that (also for showing ram contents). 
    the other option would just be rendering the whole sapp thingy into a texture which might be less annoying than infecting everything in the terminal with positioning code. 
- tc2
  - outputs don't coerce z so need to check if that wire is a switch and compile to two outputs like the old OutputNz did
    ;actually it's more convoluted than that, output pins are bidirectional pins now (ie they can be used as inputs or both depending on switches). 
    so maybe should figure out a nicer way to compile bidirectional pins first. 
    but the common case will be that the wire connecting to an output pin doesn't connect to any input pins of any components inside the CC so then it can be compiled the easy way. 
  - memory is extended with load/store ports that are applied in order so need to represent a new type of dependency
  - new pin positions, more sizes of wires
  - components have configurable (sometimes auto?) wire size
    - some of the logic ones can go up to 2048 bit but the concatenators don't seem to work so i don't really understand
  - isa: assembly parser generator thingy https://github.com/Stuffe/isa_spec
  - si: language the level tests are written in. 
    - not needed for just running circuits but makes the whole thing much more inspiring because 
      i could auto test it by just pointing it at my own solutions to the game. 
    - also then could run other people's custom levels. 
      i like the idea of my thing being a way to have headless ci tests for user campaigns. 
  - have to decide if i care about keeping support for the old version
```
// TODO: don't always recompile everything when loading a new circuit. it might reuse some custom components. 
//       and don't reset the state.fr every time because bootstrapping the comptime stuff takes ~50ms. just reset when arena is getting too full. 
//       similarly don't bother recompiling the natives every time. import them from the host exe (if not inlinable). 
// TODO: if i want to let you edit the circuit in the ui, 
//       have two copies that swap when you change something so it doesn't hang the ui while compiling the new one. 
```
- for a long time i've had the vague idea of a game where you're exploring a 3d maze and have to collect
  circuit components and spend them to solve puzzles (like build more complex gates) to unlock doors
  but the wire layout matters because they become paths in the 3d world. 
