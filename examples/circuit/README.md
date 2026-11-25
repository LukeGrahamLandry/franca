- <https://franca.lukegrahamlandry.ca/?lang=aot&file=examples%2Fcircuit%2Fgui.fr> 
  - (blazingly 15% native speed)

this is a logic gate simulator. it understands two forms of circuit programs (`defs` and `tc`). 
circuit graphs are compiled into franca code and then jitted. 
there's a very ugly visualization that's mostly just enough to tell what's going on while debugging it. 
i also have an editor and a few of my own levels. 

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

- not implemented v1 components: network, hdd, sound
