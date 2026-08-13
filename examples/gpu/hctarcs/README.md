THIS IS UNFINISHED  

current state is i can compile the programs in tests.fr into a .sb3 files 
that scratch can load. then i can load that file and transpile it to franca, 
run onflagclicked and draw a ppm of one frame at the end. 
main.fr has a list of scratch project ids that also work in my runtime. 
most other blocks are not implemented yet. 

## resources

- scratch.mit.edu is the original scratch interpreter
- turbowarp.org is convenient for testing projects that are too slow in real scratch 
- my first attempt at aot scratch was in 2023 
  https://github.com/LukeGrahamLandry/hctarcs
- if i decide to look at the real scratch code, use 
  https://github.com/scratchfoundation/scratch-editor/commit/7c3c5948459a66a9d146c0063712152a4f40fb66
  which is conveniently before they added an agents.md and before they switched to agpl
- if i want to fetch projects can use trampoline.turbowarp.org
- if i want to play with other people's projects as tests, use ones from before January 22, 2026 
  (thats when they changed license of user projects from cc-by-sa-2 to only dear scratch ai slop license tm)

## todo

- gpu rendering
- infer types
- dump Node ast back to builder franca code
- textbox edit as franca
- hell, they're gonna have SVGs in there
- when i have compiler so it isn't slow try things from 
  turbowarp featured projects https://scratch.mit.edu/studios/27205657
  (graphing calculator: 973800483; life: 1143869507; mandelbrot: 396320314; scratch in scratch: 290745095; riscvi32 linux: 1201938491; minecraft: 869264071;)
- implement more blocks
  - async
    - broadcast and wait
    - glide _ secs to _ (waits until the glide is done)
    - wait _ seconds
    - wait until _
    - ask _ and wait
    - yield after each loop iteration if not "run without screen refresh".  
      turbo mode is like that but just run until out of time in the frame. 
  - events
    - broadcast
    - scroll sends up/down arrow event but the is_pressed blocks say false
  - clones
  - costume, stamp
  - strings, lists
