
a 3d world containing screens running some of my other examples programs.

## TODO: Puzzle Ideas

chess:  
- the pieces are 3d and take up their whole square and the board is the whole room 
  so you're blocked from walking to the other end in the initial position 
  and you have to play and take pieces to open up a path for yourself. 
- another one where you just have to win 
  (or same room, two doors, one only unlocks if you win). 
- start with just one rook on the board blocking the door and you can take the king in one move

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

stackie
- write a program to match a pattern on the door? 
- have a big one with a small patch taken out and you have to match the patch (so like downscaled from the big one)

mandelbrot
- maybe just you can only walk on black so have to zoom in to fill the floor?
- have one where you can't zoom so you have to stand on the platform while it moves
- zoom so the whole thing matches the colour of a background wall and then you can walk through it
- the screen is one of the grid squares of chess and controls all of them and the one that needs to match to walk on a path is a different one

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

## TODO

- takes so long to compile (2300ms -safe) 
- make it work in wgpu
  - depth texture
  - for website give fake file system a way to access local storage for save file
- when designing the world,
  do the knowledge gating thing where you see a complicated puzzle early 
  and have to go somewhere else where it teaches you how to solve that type of thing. 
- make the walls look less flat
- allow copy from child app, don't just always clear app.requests
- allow screens in non-cardinal directions
- save system
  - instead of needing to remember to change save_file_magic, 
    include schema from the reflection info? 
    do it per puzzle type so can keep save file when adding new ones? 
  - which allocator slices are loaded into is fucked
  - auto save when you make progress instead of keybind. 
    or maybe in the middle of suspend_for_room_change so a few extra are suspended already. 
    keep prev save_data bytes for each puzzle and autosave when one changes? 
  - cmd+w needs to send QUIT_REQUESTED so can auto save then too. 
    maybe that's good enough, doing it hyperactively is really just because im afraid of crashes. 
  - cli arg for save path
  - button to reset save file for testing
  - debug program that dumps the primitives in save file as text with field names
  - use that reflection to generate editor ui
  - do the repr for remaining apps
  - wasteful extra init-suspend-restore cycle on first load. 
  - load_world needs to cope with load() returning error. also rn restore() will crash if that happens. 
    also not validating up front that it parse correctly if !active so you might only find out later. 
- ui for resetting a specific puzzle to initial state if you mess it up when you don't understand the controls yet? 
  the annoying thing is that it could reveal hidden/connected puzzles. 
  idk how to do it in a way that isn't tacky. 
- have the inspect_wall thing work on puzzles too and give you easy text to edit from the reflection thing
- figure out the antialiasing thing
- resize the apps based on their size in the projected world? (ie don't hardcode PUZZLE_LOGICAL_WIDTH)
- all the 3d puzzle effects need to be rotatable
- be generic about the extra 3d puzzle positioning so don't have to change so many places to add one
- editor: allow choosing which to delete when multiple things overlap
- editor: colour picker
  - going to want to make a `Colour :: @struct(rgba: u32)` so can reflect on it to choose the right ui. 
- editor: copy paste an area
- store object positions relative to room so can rearrange the map without a huge diff?
- make embedded terminal usable
  - needs app.requests copy
  - should probably have a mode that disables running real commands if i want to use it as a text editor for puzzles
- theres still a place you can walk through the wall in the life room, can just make the exit 2 wide
- option for double sided puzzle
  - make the back wall of the mandelbrot room match the green so the puzzle makes sense in the other direction too
- don't let you open a puzzle through a wall
- instead of the room overlapping thing for adjacent puzzles,
  maybe precompute visibility from all the tile positions in the room and merge them? 
- wasteful when there's multiple of an app so could share immutable img/shader/etc. 
  but i think the simplicity is totally worth it for now. 
