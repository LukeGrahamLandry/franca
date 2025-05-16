# notes about `@/graphics`

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

## sending data to the gpu

It's made convoluted by having a seperate processor and venders
wanting to be different from each other but i think there's a coerhernt 
mental model where everything map to something in normal cpu programs 
because they're fundamentally solving the same problem (where that problem 
is how can we use lightning to trick a rock into thinking).

- Buffer
  - a blob of bytes that can be accessed from the gpu 
- Image (=texture)
  - so also a blob of bytes that can be accessed from the gpu, but more structured. 
  It cares if it's 2d/3d (cube? array? idk... TODO). So your own code doesn't deal 
  with mapping n-d coordinates to a 1d index. 
- Sampler
  - a rule for how to access data in an image
  - so i think this is conceptually just more function arguments when your shader calls `sample`
  but the gpu wants to know what they will be way ahead of time. also maybe old gpus didn't 
  let you dynamically choose to use different sampler / image combinations? 
  - min/mag/mipmap filter: (?)
  - wrap: what happens when you index out of bounds ie. clamp or wrap
  - lod: (?) level of detail
  - border: (?)
  - compare: (?)
  - anisotropy: (?)
- Shader
  - a program that runs on the gpu (and its function signeture kinda)
  - either vertex+fragment or compute
  - data layout info for arguments that will be passed to your shader program
  - how many Image/Sampler the program will access and from which stage
- Pipeline
  - a shader + info the gpu needs to know how to call your shader
  - primitive: you send the gpu a flat buffer of vertex data and it needs to know how to 
  interpret those as shapes. 
  - indexing: instead of using vertex data directly, you give a seperate buffer of indexes 
  into the vertex buffer so if you have a lot of data per vertex but most vertices are used 
  multiple times you can only send the data once per vertex. ie. a rectangle is made of two 
  triangles so 6 vertices but only 4 are unique. 
  - depth (?)
  - stencil (?)
  - culling: when you have closed models, you know one side of each face is facing inwards
  and will never be in view so notice as fast as possible that those can be discarded. 
  - winding: how does which side of a face is the front based on vertex order (for culling)
  - colours (?)
  - alpha (?)
- Attachments
  - only relevant for an offscreen render passes
  - some colours, some resolves, and a depth stencil. each of which is one "attachment",
  which is one image that you will write to 
  - you can't have the same image as both input and output of a single pass

All of those are a "resource" that you `make` on the cpu side (which translates into 
whatever calls with the native api) and then reference later with its handle. 
You often create Buffer/Image with data but you can also mutate it later 
(with update/append functions). You often create your resources when your 
program starts but you can also dynamically create/destroy them whenever you want. 

The way you do stuff at all is start a "pass" and then record a bunch of commands.
You can have multiple passes per frame. After recording for all your passes, you 
commit() and send the commands to the gpu to be executed. 

The support librarieslike gl.fr/debugtext.fr don't want to be rude and force you 
to only call thier functions at a certain time inside your render pass so they have 
another layer of recording commands and then calling draw_layer() inside a pass 
executes those to record realgpu level commands to finally be executed by the gpu 
when you call commit(). 

- Bindings 
  - the arguments to a specific invocation of a Shader
- types of passes: swapchain, off screen, compute
- uniforms
  - data that's the same for many runs of a shader. 
  ie. for every vertex in a scene, the player's camera transform is the same. 

## Alignment

Vec3 is a bad plan!
