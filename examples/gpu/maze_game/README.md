
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

stackie
- write a program to match a pattern on the door? 

mandelbrot
- maybe just you can only walk on black so have to zoom in to fill the floor?
- have one where you can't zoom so you have to stand on the platform while it moves

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

- fix the weird transparent background debugtext
- make it work in wgpu
- when designing the world,
  do the knowledge gating thing where you see a complicated puzzle early 
  and have to go somewhere else where it teaches you how to solve that type of thing. 
- make the walls look less flat
- hint screens in the world with text. 
  - "wasd + mouse" "try pressing escape while looking at a screen" "there's a door behind this rook. if only it would move out of the way"
- allow copy from child app, don't just always clear app.requests
- allow screens in different orientations
- still render the puzzles in adjacent rooms
- i'll need to save progress eventually anyway 
  so the snapshot thing where you unload the apps when you're in a different room is a good idea, since save can reuse. 
  and that will help with pools running out of space. 
- figure out the antialiasing thing
- resize the apps based on their size in the projected world? (ie don't hardcode PUZZLE_LOGICAL_WIDTH)
- all the 3d puzzle effects need to be rotatable
- be generic about the extra 3d puzzle positioning so don't have to change so many places to add one
