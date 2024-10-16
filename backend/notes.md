- We're very often iterating backwards so `emit` inserts into the block buffer backwards!
  For now we use the buffer in Qbe.Globals but that will move out eventually.
- When looking at the debug dumps, register numbers are off by one. ie. first argument on arm (x0) is in R1.

## Words

- When trying to understand what phi instructions do, remember that they're totally isomorphic to block arguments and then it's suddenly super simple.
  It's just a choice to represent it as the callee knowing which value to use for each argument for each caller.

##

If you use llvm you don't get full c-abi compatibility for free.
For example, here's where rustc does it in the frontend https://github.com/rust-lang/rust/blob/master/compiler/rustc_target/src/abi/call/mod.rs
