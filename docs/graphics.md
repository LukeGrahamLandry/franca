# notes about ../graphics

`franca/graphics` is a collection of libraries for writing programs that don't live 
in a terminal. It will open a window, listen for events, draw pixels on the screen, etc. 

```
app.fr : gfx.fr :: winit : wgpu
```

Take all of this with a grain of salt; I know much less about gpu stuff than I 
know about compiler stuff. A major part of why I'm converting the gfx code to 
my language is to force myself to look at it all in the hopes of understanding 
what it's doing. So this is mostly just rephrasing stuff from the sokol doc 
comments, but perhaps with a target audience that doesn't know what the gpu 
words mean (because I don't know what the gpu words mean). That also means 
this is an awkward mix of general information and stuff specific to the sokol_gfx 
abstraction layer. 

## examples

- app_events: tries to use all features of app.fr and debugtext.fr
- geo: decompresses LAZ files, draws 3d point clouds, and lets you fly around
- farm_game: a 2d game drawn with coloured rectangles
- edit: opens a text file and lets you scroll around

## other places to find words

- https://github.com/floooh/sokol/blob/master/sokol_gfx.h
- https://sotrh.github.io/learn-wgpu

## api organisation

- 6 resource types: Buffer, Image, Sampler, Shader, Pipeline, Attachments. 
- To create resource Foo, pass a FooDesc struct to sg.create(). 
- For ones that correspond directly to a native api object, instead of passing setup 
parameters in the Desc, you can create one with the native api yourself and pass us 
the context pointer. So you can mix in a bit of backend specific 
stuff while mostly using the cross platform api. Ideally the gfx layer is powerful 
enough that you don't need to care what platform you're targetting. 
- Different platforms have different apis for talking to thier gpus. 
When i say "native" i just mean not my gfx abstraction layer. 
It could still be someone else's abstraction layer (like webgpu). 
- The resources are represented as an opaque index into a pool. 
The pool stores some common cpu side state and the native api handles. 
The stuff in the pool is "private", you just interact with it by passing 
those resource ids to the sg.whatever() functions.
- gfx.fr doesn't open a window for you (do it yourself or use app.fr), 
it just needs you to pass in a native api handle
- app.fr wants to own the event loop. 
- I think i started stealing stuff at an unfortunate time where sokol 
was half way through adding support for compute shaders. 
- Doing stuff with the gpu always requires a shader program. Often it will be 
so trivial that it seems silly. You can use gl.fr when you just want to put some 
triangles on the screen. That's such a confusing name for something that does not 
depend on opengl. It mimics old opengl in that you can draw stuff without thinking 
about shaders/buffers/etc. 
- gfx.fr requires you to provide shader programs in the right format/language 
for the native api. shaders.fr is the beginning of translating franca into 
those shader languages, but if you have your own convenient way to author 
shaders, you can keep doing that instead. 
- Supporting a new platform means adding an implementation for app.fr, gfx.fr, 
and shaders.fr. For now i only support Metal. 
