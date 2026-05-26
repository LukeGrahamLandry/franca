> franca examples/life/gui.fr -jit -unsafe

Life: grid of cells that live or die based on its neighbours. 
- https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

Hashlife: quadtree + deduplicate + memoize = fast eval because many patterns are repetitive 
- https://en.wikipedia.org/wiki/Hashlife
- Exploiting Regularities in Large Cellular Spaces, Bill Gosper
  (original paper but i'm dumb and can't read things that aren't code)
- https://www.dev-mind.blog/hashlife (has very nice pictures)

This program can load RunLengthEncoded (.rle) and Macrocell (.mc) pattern files. 
- https://conwaylife.com/patterns/all.zip

TODO: infinite canvas
TODO: control how many generations are stepped when doing hashlife
      instead of just fast=true/false, toggle it at each recursion level to get any power of two step size. 
TODO: put eval on other threads
      then can allow moving the shader params to give a slight sense of progress while waiting to update the frame if its slow. 
TODO: experiment with eval in compute shader
TODO: write .rle and .mc files
TODO: cli to step generations without the gui
TODO: pattern gallery
TODO: add some tests
  - round trip compress/decompress
  - round trip rle/mc pattern
  - run a small pattern in normal and in hashlife and makes sure they match
  - move non-hashlife eval out of eval.fr into test
TODO: bring back smooth movement when zoomed in by putting part of pos in the shader
TODO: speed up compress() (for loading .rle to hashlife)
TODO: a thing to generate big patterns from one life-in-life cell
TODO: fix the dead cell on the edge for fractional move
TODO: non power of two scale in shader

TODO: program that generates this visualisation
![indices in eval_center_split_merge](https://lukegrahamlandry.ca/assets/hashlife.jpg)
