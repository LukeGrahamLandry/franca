THIS IS UNFINISHED  

current state is i can compile tests/mandelbrot.fr into a .sb3 file 
that scratch can load. then i can load that file and interpret 
it and draw a ppm of mandelbrot set. most other blocks are not implemented yet. 

## resources

- my first attempt at aot scratch was 
  https://github.com/LukeGrahamLandry/hctarcs
- if i decide to look at the real scratch code, use 
  https://github.com/scratchfoundation/scratch-editor/commit/7c3c5948459a66a9d146c0063712152a4f40fb66
  which is conveniently before they added an agents.md and before they switched to agpl
- if i want to fetch projects can use trampoline.turbowarp.org

## todo

- text langauge for making scratch projects
- interpreter
- nan tagging
- compile to franca
- infer types
- dump Node ast back to builder franca code
- textbox edit as franca
- hell, they're gonna have SVGs in there
- when i have compiler so it isn't slow try things from 
  turbowarp featured projects https://scratch.mit.edu/studios/27205657
  (graphing calculator: 973800483; life: 1143869507; mandelbrot: 396320314; scratch in scratch: 290745095; riscvi32 linux: 1201938491; minecraft: 869264071;)
