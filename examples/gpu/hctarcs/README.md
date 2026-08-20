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
- if i want to play with other people's projects as tests, use ones from before January 22, 2026 
  (thats when they changed license of user projects from cc-by-sa-2 to only dear scratch ai slop license tm)

## todo

- what does shadow:true mean?
- have the generated main() call gui instead of viewer
- support+test outputting broadcasts/keys from the ast builder
- images should be drawn at half size
- use the right value tags when outputting .sb3
- coerce_number for all the math ops
- gpu rendering
- infer types
- hell, they're gonna have SVGs in there
- they do nan in an insane way? 0 / 0 == NaN but (0 / 0) + 1 == 1 but (0 / 0) > 50 == true
- implement more blocks
  - async
    - broadcast and wait
    - glide _ secs to _ (waits until the glide is done)
    - ask _ and wait
    - turbo mode: instead of yielding each loop iteration, run until out of time in the frame. 
  - events
    - scroll sends up/down arrow event but the is_pressed blocks say false
  - is touching, if on edge bounce
  - clones
  - drag
- strings should be utf16
- ui
  - cli/CLIPBOARD_PASTED/FILES_DROPPED to load .sb3 (needs to embed the compiler)
  - show warnings if projects use unsupported features (sound, cloud variables, extensions)
  - load project by id
  - for external ones, put info from meta.json in the output and have button in ui to show it. 
  - see generated code
  - flag/pause/stop
  - see the code as blocks
  - keyboard, drag, click bounds, scroll, ask
  - dump Node ast back to builder franca code and textbox edit as franca
  - collect warnings when loading the project and show them at the end
- add simple tests: looks_sayforsecs,motion_glidesecstoxy,broadcast,broadcastandwait,clone,timer,setbackdropandwait, 0x_hex and #_colour strings
- what happens if you broadcast the same message again while someone else is waiting on a broacast_and_wait
- should look at the extensions array first to give nicer error message before dying on an unknown upcode

## notes

- there are two types of menus blocks can have. 
  ovals (inputs) let you place an expression there instead so it can be a computed string (ex. broadcast,costumes). 
  rectangles (fields) are forced to be chosen from a valid constant (ex. lists,variables,effect,mathop). 
  when an oval is chosen from the menu, there's an inserted fake block that has a field as though it were a rectangle. 
- there are some blocks that don't exist in the website's ui but still work if projects have them. 
  (while, for_each).
