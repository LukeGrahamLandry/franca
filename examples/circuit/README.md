- <https://franca.lukegrahamlandry.ca/?lang=aot&file=examples%2Fcircuit%2Fgui.fr> 
  - (blazingly 15% native speed)

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
- <https://github.com/Stuffe/save_monger> (v1: 22fa398d95c3d3a0b5e88a80e9daee613cab1823, v2: 2ef0e5b3b76915e1b0a23a88fcbd885371cdb68e)
- <https://github.com/LukeGrahamLandry/TuringCompleteSchematicArchive>

circuit.data save files from TuringComplete.Game (v1: 0.1059, v2: 2.1.334). 
those files have the positions of wires/gates which i convert into the same graph representation as the `defs` language. 
in this mode there are many more builtin gates (ex. a 64 bit xor compiles to one cpu instruction, etc.) so will run much faster than the `defs` one. 

it works well enough to run my overture/leg in the maze and a few things from the schematic hub (mandelbrot and tetris). 

> i've started to support circuits the 2.x rewrite of the game 

## TODO (v1)

- devices
  - missing: network, hdd, sound
  - console: wrong font
  - keyboard: click to focus instead of a keybind. 
  - display: rotation
  - latency ram, Stack
  - sprite display: positioning is wrong
  - dot display: wrong default colour
- Mul64 high on wasm
- assembler
  - expressions with spaces in them are wrong
  - what happens when a label is definied multiple times
- from schematic hub
  - 3D Engine: "str scratch frame"
  - screenvurture: "H" as char literal in expression
  - ASM computer 64 bit: Hdd, RamLatency
  - ploter: SR-Latch: use-before-def ("Allow circular recipies")
- the game has a setting ("Allow circular recipies") to idiom-regognise a few shapes of circular dependencies and convert to (AndOrLatch,NandNandLatch,NorNorLatch)
- i don't think im doing the keyboard right. ex. mandelbrot is behind a key sometimes?
- use f64 for pos/zoom in ui because it gets jerky scrolled down far in the trace for big circuits
- make the defs language less painful
  - number literal instead of const()
  - nested expressions
  - variable names
  - speed mode that cheats and implements components with native instructions (like tc) instead of nand gates
- ui: allow copying the code to clipboard
- `Main :: import().examples.circuit;` doesn't work in the web demo
- nice ui for web gallery from my archive of the old schematic hub
- it's annoying that you can't see the wire values if it halts on a tick that was evaled in fast mode. 
  delay all writes to the end somehow so can rerun the tick in slow mode after knowing it will halt?
- use 32 bit instructions instead of bit_and(0xFFFFFFFF)
- arm has nice merged (and,or)+shift in one instruction
- option to see -d jit logging in the ui

```
// TODO: don't always recompile everything when loading a new circuit. it might reuse some custom components. 
//       and don't reset the state.fr every time because bootstrapping the comptime stuff takes ~50ms. just reset when arena is getting too full. 
//       similarly don't bother recompiling the natives every time. import them from the host exe (if not inlinable). 
// TODO: if i want to let you edit the circuit in the ui, 
//       have two copies that swap when you change something so it doesn't hang the ui while compiling the new one. 
```
