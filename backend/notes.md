- We're very often iterating backwards so `emit` inserts into the block buffer backwards!
  For now we use the buffer in Qbe.Globals but that will move out eventually.

## Words

- When trying to understand what phi instructions do, remember that they're totally isomorphic to block arguments and then it's suddenly super simple.
  It's just a choice to represent it as the callee knowing which value to use for each argument for each caller.
