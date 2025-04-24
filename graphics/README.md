Franca ports of various things useful for creating cross platform graphical applications. 

> I've only implemented shader translation for MSL so its not at all cross platform but hopefully that's a temporary situation. 

- Some example programs can be found in the franca examples folder. 

## https://github.com/floooh/sokol

- app: app framework wrapper (entry + window + 3D-context + input)
- gl: OpenGL 1.x style immediate-mode rendering API
- debugtext: a simple ASCII text renderer using vintage home computer fonts

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
- No dependency on a c and objective-c compiler (eventually). 
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