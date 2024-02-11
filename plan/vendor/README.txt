// TODO: Is 'thirdparty' a better name than 'vendor' if im not putting the source here, just urls? 
// TODO: my theory of putting hashes in your code where you add the dependency so auto lockfile 
//       doesnt work if I provide a list here. dont want updaign std version to just force grab new versions of common stuff. 

The idea is to not have to write bindings, just parse the definitions from the library's code. 
These are just some I've found myself using that I want to use as a test case that my parser doesn't choke. 

## Abstractions I want on every target. 

Expose canvas api on native targets and use browser version when targeting that so easy support both 
and super low size overhead for web by not shipping your own implementation of chapes and images in terms of low level pixels. 
I feel like someone must already have this. 
I don't care about writing it myself I just want it to be trivial to put 2d graphics on the screen anywhere you run. 
Also keyboard/mouse events. 

## C libraries that seem useful

- https://github.com/floooh/sokol
    - cross platform open a window, get events, talk to the gpu. 
- https://github.com/nothings/stb
    - load images, sound, perlin noise.
- https://github.com/llvm-mirror/llvm/blob/master/include/llvm-c/Core.h
    - optimising compiler framework.

## Rust libraries that seem useful

- https://github.com/rust-windowing/winit
     - cross platform open a window, get events.
- https://github.com/gfx-rs/wgpu
    - cross platform talk to the gpu.
- https://docs.rs/image/latest/image
    - load/manipulate images

