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

circuit.data save files from TuringComplete.Game (v6 / 0.1059 beta). 
those files have the positions of wires/gates which i convert into the same graph representation as the `defs` language. 
in this mode there are many more builtin gates (ex. a 64 bit xor compiles to one cpu instruction, etc.) so will run much faster than the `defs` one. 

it works well enough to run my overture/leg in the maze and a few things from the schematic hub (mandelbrot and tetris). 

## tc is INCOMPLETE

- not all components are implemented / have pin positions transcribed
  - also missing: 64 bit mul high, add overflow. 8/16 arithmetic shift wrong. 
- devices
  - missing: network, hdd, sprite display, sound
  - console: wrong font. data size 16/32? colour with data size 8?
  - keyboard: character mode param. click to focus instead of a keybind. 
  - ram: 128/256 bit, duel load
  - display: rotation
  - probes
  - halt: message (set on the gate)
- need to position the displays correctly because they can be tiled
- the way i represent unpowered (hi-z) wires is sketchy
  - need to do it for disable pins of decode3/register/ram?
  - ram needs to not write if an input is unpowered? (rn only does that for fully disconnected)
- bidirectional pins are sketchy 
  - using as both input/output depending on a switch probably won't work
  - makes the visualization confusing because i always inline the whole custom component
- insert casts when wire width doesn't match required. 
- assembler: mnemonics, constants, arithmetic
- need a better interface for choosing circuit/programs, showing memory contents
- pre_read mistakes would be a lot easier to debug if they were made explicit seperate operations in the trace display. 
- dropping file doesn't work in franca-web-playground (even when fixed will be annoying because need to feed it all the custom components individually)
- not all work without FEAT_RT_COMPILE?
- etc.

## other todos

- put world.instance in a local/parameter and don't increment it in the builtin impls
- finish leg: memory, stack, call, shifts
- make the defs language less painful
  - number literal instead of const()
  - nested expressions
  - variable names
  - speed mode that cheats and implements components with native instructions (like tc) instead of nand gates
- tc: do they have an api for the schematic hub?
- tc: at top level sort by type of IO maybe so don't have to add random offsets in the evaluator depending on the arch. ie in my overture and leg input and output are swapped because the order in save file is arbitrary. 
- compile a version with save_wire_values so can get rid of the interpreter? 
  (when running the defs.fr one. tc import will probably always do that)
- fast mode that doesn't even save_wire_values once per frame
- ui: allow copying the code to clipboard
- `Main :: import().examples.circuit;` doesn't work in the web demo
- i don't really care about ui for visually editing the wires
  but i have a bunch of code for converting positions into graph 
  and rn the only way to test it involves opening Steam which is a pain, 
  so maybe i have to give up and have my own shitty version too. 
- option to do scenario test on circuit.data
- less trash code. builtin.fr and gpu.fr are particularly bad.. 
- deduplicate error checking between parse and load
- show wire comments in the ui

```
// TODO: don't always recompile everything when loading a new circuit. it might reuse some custom components. 
//       and don't reset the state.fr every time because bootstrapping the comptime stuff takes ~50ms. just reset when arena is getting too full. 
//       similarly don't bother recompiling the natives every time. import them from the host exe (if not inlinable). 
// TODO: if i want to let you edit the circuit in the ui, 
//       have two copies that swap when you change something so it doesn't hang the ui while compiling the new one. 
```
