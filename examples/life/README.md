
Life: grid of cells that live or die based on its neighbours. 
- https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
Hashlife: quadtree + deduplicate + memoize = fast eval because many patterns are repetitive 
- https://en.wikipedia.org/wiki/Hashlife
- Exploiting Regularities in Large Cellular Spaces, Bill Gosper
  (original paper but i'm dumb and can't read things that aren't code)
- https://www.dev-mind.blog/hashlife (has very nice pictures)
Interesting starting layouts
- https://conwaylife.com/patterns/all.zip

TODO: control what happens at the edges (wrap/die/infinite)
TODO: edge behaviour doesn't match between hashlife and normal. 
      normal: edges are treated as dead but you can't write in to them so you can get stuck.
      hashlife: bit boarder is added and then discarded so you can go off the screen and disappear. 
TODO: control how many generations are stepped when doing hashlife
TODO: keep cells square if window is rectangle
TODO: each frame only decompress the area that's actually in view? 
TODO: when zoomed out colour pixels based on population ratio in a group of cells
TODO: put eval on other threads
TODO: experiment with eval in compute shader
TODO: allow draw while in hashlife mode
TODO: write .rle files
TODO: https://golly.sourceforge.io/Help/formats.html#mc
TODO: cli to step generations without the gui
TODO: little program that generates the visualisation for indices in eval_center_split_merge. 
TODO: pattern gallery
TODO: add some tests
  - round trip compress/decompress
  - run a small pattern in normal and in hashlife and makes sure they match
