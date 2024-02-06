
The idea is to not have to write bindings, just parse the definitions from the library's code. 
These are just some I've found myself using that I want to use as a test case that my parser doesn't choke. 

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
