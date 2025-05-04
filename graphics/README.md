Franca ports of various things useful for creating cross platform graphical applications. 

> I've only implemented shader translation for MSL so its not at all cross platform but hopefully that's a temporary situation. 

- Some example programs can be found in the franca examples folder. 

## https://github.com/floooh/sokol

- gfx: 3D-API abstraction layer (backends: Metal, WebGPU)
- app: open a window, get a 3D-context, handle input events
- gl: OpenGL 1.x style immediate-mode rendering API
- debugtext: a simple ASCII text renderer using vintage home computer fonts

## Other

- shaders: translate a subset of franca to MSL
- vec: 2/3/4-D linear algebra
- easy: reduce boilerplate for small example programs

## Changes from Sokol

### app

- removed builtin default app icon
- removed limits on paste/drop
- fewer getters (just access fields directly)
- (macos) no dependency on an objective-c compiler
- (macos) wait until after setActivationPolicy to set the startup icon/fullscreen (otherwise it doesn't work)
- UNFINISHED
  - don't have timing stuff (frame_duration)
  - only implemented for macos
  - (macos) not caching objc selectors

### gfx

- generics are a surprise tool that can help us later
- fewer getters (just access fields directly). removed the `query_*_(desc/info/____)` functions
- no limit for commit listeners. removing doesn't make holes in the array 
  so the order will be different. 
- removed canaries
- (macos) no dependency on an objective-c compiler
- (macos) removed ios, opengl, and macos <13 support (temporarily?)
- UNFINISHED
  - logging and trace hooks don't work
  - init(Attachments) 
  - compute shaders
  - validate Shader.Desc `_sg_validate_slot_bits`
  - shutdown (dealloc/deinit/fail resources)
  - move structs from bindings/sokol to here
  - clean up defaults (compiler: implement partial array literals that respect default fields)
  - append/update buffer/image
  - only implemented for macos
  - (macos) im not setting labels
  - (macos) not caching objc selectors

### debugtext

- write the shaders in franca 
- remove printf-like functions. i don't like c-style varargs. can use @fmt into a temp buffer instead.
- remove getters/setters for ctx's pos/color.
- remove global `_sdtx` and context pool. instead pass the context explicitly to every call. 
  (caller manages the memory for the contexts but maybe that's annoying, idk)
- remove byte array clutter. load font data from readable string. 
- removed the 8 font limit

## Tradeoffs vs Sokol

The main value of this code is the understanding I personally gained by typing it all out. 
Whether my changes are an improvement over the c version is a matter of personal preference. 
If you choose the c route, you can use `examples/c_bindgen.fr` to avoid manually typing ffi signetures. 

- They support multiple platforms. Currently I only support macos-metal. 
- They have more example program you can learn from. 
- They have better tests. I mostly just run an example program and see if it looks mostly ok. 
- They have global variables so you can pass fewer arguments to functions. I make you explicitly pass an extra argument to everything. 
- They care more about providing a stable API. They have getters and setters for a bunch of fields that I let you access directly. 
- They make sure each library is an standalone header file you can include in your project. 
- They care about making it easy to write bindings in other languages. I'm making a franca library for franca programs. 

A few changes are fairly clear improvements:

- Store data for simple fonts as readable ascii art strings and use comptime execution to convert them to the 64-bit-per-character format used at runtime. 
  (instead of pasting a massive byte array into your c program).
- Write shaders as annotated franca functions and use comptime introspection to translate them into shader languages.  
  (instead of writing it in glsl-ish and using a seperate build step to translate it into... you guessed it... a massive byte array to paste into your c program).
- No dependency on an objective-c compiler
- Less code per code. 

## Sokol License 

zlib/libpng license

Copyright (c) 2018 Andre Weissflog

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from the
use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in a
    product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not
    be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.