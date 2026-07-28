
a 3d world containing screens running some of my other examples programs.

[![demo](https://img.youtube.com/vi/E0jMBol_KSI/0.jpg)](https://www.youtube.com/watch?v=E0jMBol_KSI)

press f3 to enable cheats (shift/space to fly, x to inspect wall). 

## TODO: Puzzle Ideas

chess:  
- the pieces are 3d and take up their whole square and the board is the whole room 
  so you're blocked from walking to the other end in the initial position 
  and you have to play and take pieces to open up a path for yourself. 
- another one where you just have to win 
  (or same room, two doors, one only unlocks if you win). 
- start with just one rook on the board blocking the door and you can take the king in one move
- have a coridor where you're above the chess game in another room and can walk on top of the pieces so need to arange a path

life:  
- the grid is the floor and you can only walk on the solid cells. 
  need to create a pattern that repeats so you have time to walk across while it runs.
- maybe the controller panel is in another room and somewhere else there's ones with hints for patterns that would work. 
- it could also be vertical so its like a locked door

circuit:  
- have to make a truth table to open door but the wires you place are also paths in the 3d world. 
- you collect gates that you can place on the door and you might have to go back and take them off to use somewhere else 
  (which would lock a past door you've already explored). 
- could also have wires physically between doors so which outputs are powered on is which doors are unlocked 
  (maybe with fixed wire layout so you just have to place the right gates so its not just a trivial one wire). 

farm game
- earn enough to buy a key? 
- survive a certain number of rounds? 
- have shelves/farm be in the 3d world so you can take items out of the game 
  and move them to a different instance of it in a different room.
- buy a chess piece in the shop and bring it to a board to win

stackie
- write a program to match a pattern on the door? 
- have a big one with a small patch taken out and you have to match the patch (so like downscaled from the big one)

mandelbrot
- maybe just you can only walk on black so have to zoom in to fill the floor?
- have one where you can't zoom so you have to stand on the platform while it moves
- zoom so the whole thing matches the colour of a background wall and then you can walk through it
- the screen is one of the grid squares of chess and controls all of them and the one that needs to match to walk on a path is a different one
  - or the square you need to move to to win is a mandelbrot of the wrong colour and you have to change it before the chess lets you make that move. 

terminal: 
- an editor with a program that affects the world in some way.
  - languages: (franca, c, kaleidoscope, lox), (si, minijinja, wuffs)
  - ideas
    - print certain characters are filled in floor
    - it has a binary and you have to disassemble/dump_wasm to get the password
    - last level could be franca repl with access to the game's State and you have to set the door to unlocked manually

viewer
- the image is of an item you need and you have to move it to your side of an obstacle. 

wasm4
- get enough points in tetris

trophy room that shows you how many puzzles are solved/unsolved of each type. 
- rn i have one that counts screens clicked which is good enough if all puzzle chains lead to a hint room. 

editor
- have one that syncs to the world as the prize at the end

the whole game in a screen so you get to be in a different place in the world and can use it to teleport kinda would be cool. 

## TODO

- visual feedback for toggling cheats mode
- see comments in puzzles/mandelbrot.fr
- always allow entering flat mode even if no puzzle so you can use that to pause the game to get your mouse back
- record a run through that i can replay as a test. replay.fr can make a comeback perhaps. 
  will have to redo anytime i change the world or rules but it's better than nothing. 
- config to lower memo table size for Chess/Life instances for puzzles that don't need big perf. 
  i guess on_init has to look at the state struct because normal path will zero init which can use default 
  and the game can poke in smaller numbers early. 
- the mandelbrot square flickers red on the frame where life gets suspended. which could give away the hidden puzzle early. 
- fix the rooms. life unloads when you can still see it in the blue hallway 
  and the blue corner isn't in any room at all. 
- takes so long to compile (2300ms -safe) 
  - filtering out the apps i dont use yet helped, ~1750, but still
    half of that is FEAT_REPL for terminal but once i add circuit/gui.fr i'll need the compiler anyway so not really worth changing. 
- make it work in wgpu
  - depth texture
  - web: use less memory
  - web: lock mouse
  - web: full screen and precompiled
  - for website give fake file system a way to access local storage for save file
- when designing the world,
  do the knowledge gating thing where you see a complicated puzzle early 
  and have to go somewhere else where it teaches you how to solve that type of thing. 
- make the walls look less flat
- allow copy from child app, don't just always clear app.requests
- allow screens in non-cardinal directions
- save system
  - stutter when changing rooms with Life is unplayable
  - instead of needing to remember to change save_file_magic, 
    include schema from the reflection info? 
    do it per puzzle type so can keep save file when adding new ones? 
  - which allocator slices are loaded into is fucked
    it's fine if its just for going into a list or to be translated for a repr. 
    but it's an easy mistake to make.
    ex. editor doesn't work because of Object.Screen.app and Object.Linked.Extra.(init_fen, init_pattern)
    :SaveSliceAllocator
  - cli arg for save path
  - button to reset save file for testing
  - debug program that dumps the primitives in save file as text with field names
  - use that reflection to generate editor ui
  - do the repr for remaining apps
  - load_world needs to cope with load() returning error. also rn restore() will crash if that happens. 
    also not validating up front that it parse correctly if !active so you might only find out later. 
- ui for resetting a specific puzzle to initial state if you mess it up when you don't understand the controls yet? 
  the annoying thing is that it could reveal hidden/connected puzzles. 
  idk how to do it in a way that isn't tacky. 
- figure out the antialiasing thing
- resize the apps based on their size in the projected world? (ie don't hardcode PUZZLE_LOGICAL_WIDTH)
- all the 3d puzzle effects need to be rotatable
- be generic about the extra 3d puzzle positioning so don't have to change so many places to add one
- editor: allow choosing which to delete when multiple things overlap
- editor: colour picker
  - going to want to make a `Colour :: @struct(rgba: u32)` so can reflect on it to choose the right ui. 
- editor: copy paste an area
- editor: the sorting actually makes it less stable when you edit the world if you change room arragement. maybe it just be insertion order.
- store object positions relative to room so can rearrange the map without a huge diff?
- make embedded terminal usable
  - needs app.requests copy
  - should probably have a mode that disables running real commands if i want to use it as a text editor for puzzles
  - suspending the repl is going to be nontrivial. i do have bake_relocatable_value
    so i could do it. the problem is just when its supposed to alias a world thing like inspect_wall. 
  - on_cleanup: "... leaked ref count"
  - can't press escape to pop buffer stack because it exits the screen
  - editor mode where up/down arrows just move the cursor instead of history
- theres still a place you can walk through the wall in the life room, can just make the exit 2 wide
- option for double sided puzzle
  - make the back wall of the mandelbrot room match the green so the puzzle makes sense in the other direction too
- don't let you open a puzzle through a wall
- instead of the room overlapping thing for adjacent puzzles,
  maybe precompute visibility from all the tile positions in the room and merge them? 
- wasteful when there's multiple of an app so could share immutable img/shader/etc. 
  but i think the simplicity is totally worth it for now. 
- i keep needing to google "fen editor" to make the chess rooms. 
  should just add a mode to examples/chess/gui.fr that lets you place pieces. 
- Object.Screen needs certain disabled inputs so can't cheat the idea even tho the standalone programs need to let you set the whole state 
  (ex. chess fen paste or life draw/pattern/pause once i want to force you to interact with the world while it changes)
- check that init fens don't have impossible castle rights
- for the yellow mandelbrot chess hallway one to make sense from the other direction the wall blocker needs to be thin i guess so its actually just the transition between squares you can't walk on. 
- have the existing room give you another door if you win the game. 
  one of the paths lead to a new full board that you just have to win so you learn that can do stuff too. 
- try to have the screens oriented so you don't have to think about mirroring them / rotating. 
  ex. chess is off by 90 degrees, but it's annoying because you want the chess board to not be blocked by pieces. 
  start having an extra border square so it can be near of the board on the black side?
- have an indicator for when you've already solved the puzzle a hint applies to? 
- mandelbrot:
  - colour pick current colour from world for making new puzzles
  - better feedback when you're very close to getting the right colour
  - be more strict about number of matching pixels
- don't let you clip through the diagonal between chess pieces
- preserve the legal component of movement when diagonal against a wall
- maybe child_fullscreen should send resize event and stop doing force_render_target 
  and stop remapping events and just directly run that child. would probably be less code. 
- discoverable interface for child_fullscreen
- farm
  - positions use Vec2 and pass_action be a local
  - icon for crow and drought
- better error messages (show line number) for load_text
- same in game hot reloading inspect thing with the visual editor.fr
- cli arg for world map. i like the idea of people being able to share levels.
- don't make the app textures early, only on first load
- for the ones that tick it could be cool to have them tick while you're away
  because theres often a faster way to simulate a bunch of ticks at once (life,stackie)
- push_as_tls to hide cli args from the apps
- chess repitition draw won't work when suspend because it's not in fen
- instead of each app type needing to deal with unlockable door, have one that points at a link=xxx with different condition. 
- comptime validate that map.fr init data parses (ex. chess fen, life pattern)
