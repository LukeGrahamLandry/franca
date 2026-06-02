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

TODO: put eval on other threads
      then can allow moving the shader params to give a slight sense of progress while waiting to update the frame if its slow. 
TODO: experiment with eval in compute shader
TODO: cli to step generations without the gui
TODO: pattern gallery
TODO: add some tests
  - run a small pattern in normal and in hashlife and makes sure they match
TODO: speed up compress() (for loading .rle to hashlife)
TODO: fix the rare missing pixel on the edge for fractional movement
TODO: the zoom is still kinda shaky?
TODO: store size/step as log2 so can go farther? but then pos math is a problem. 

TODO: program that generates this visualisation
![indices in eval_center_split_merge](https://lukegrahamlandry.ca/assets/hashlife.jpg)
