sokol_debugtext.h   - simple ASCII debug text rendering on top of sokol_gfx.h

Original Project URL: https://github.com/floooh/sokol

CHANGES FROM SOKOL
==================
- remove printf-like functions. i don't have c-style varargs. can use @fmt into a temp buffer instead.
- remove getters/setters for ctx's pos/color.
- remove global _sdtx and context pool. instead pass the context explicitly to every call. 
- remove byte array clutter. embed font data and precompiled shaders from a seperate binary file. 

FEATURES AND CONCEPTS
=====================
- renders 8-bit ASCII text as fixed-size 8x8 pixel characters
- comes with 6 embedded 8-bit home computer fonts (each taking up 2 KBytes)
- easily plug in your own fonts
- create multiple contexts for rendering text in different layers or render passes

STEP BY STEP
============

--- to initialize sokol-debugtext, call sdtx_setup() *after* initializing
    sokol-gfx:

        sdtx_setup(&(sdtx_desc_t){ ... });

--- configure sokol-debugtext by populating the sdtx_desc_t struct:

    .fonts (default: none)
        An array of sdtx_font_desc_t structs used to configure the
        fonts that can be used for rendering. To use all builtin
        fonts call sdtx_setup() like this (in C99):

        sdtx_setup(&(sdtx_desc_t){
            .fonts = {
                [0] = Sdtx.Data.font_kc853,
                [1] = Sdtx.Data.font_kc854,
                [2] = Sdtx.Data.font_z1013,
                [3] = Sdtx.Data.font_cpc,
                [4] = Sdtx.Data.font_c64,
                [5] = Sdtx.Data.font_oric,
            }
        });

        For documentation on how to use you own font data, search
        below for "USING YOUR OWN FONT DATA".

    .context
        The setup parameters for the default text context. This will
        be active right after sdtx_setup(), or when calling
        sdtx_set_context(SDTX_DEFAULT_CONTEXT):

        .max_commands (default: 4096)
            The max number of render commands that can be recorded
            into the internal command buffer. This directly translates
            to the number of render layer changes in a single frame.

        .char_buf_size (default: 4096)
            The number of characters that can be rendered per frame in this
            context, defines the size of an internal fixed-size vertex
            buffer.  Any additional characters will be silently ignored.

        .canvas_width (default: 640)
        .canvas_height (default: 480)
            The 'virtual canvas size' in pixels. This defines how big
            characters will be rendered relative to the default framebuffer
            dimensions. Each character occupies a grid of 8x8 'virtual canvas
            pixels' (so a virtual canvas size of 640x480 means that 80x60 characters
            fit on the screen). For rendering in a resizeable window, you
            should dynamically update the canvas size in each frame by
            calling sdtx_canvas(w, h).

        .tab_width (default: 4)
            The width of a tab character in number of character cells.

        .color_format (default: 0)
        .depth_format (default: 0)
        .sample_count (default: 0)
            The pixel format description for the default context needed
            for creating the context's sg_pipeline object. When
            rendering to the default framebuffer you can leave those
            zero-initialized, in this case the proper values will be
            filled in by sokol-gfx. You only need to provide non-default
            values here when rendering to render targets with different
            pixel format attributes than the default framebuffer.

--- Before starting to render text, optionally call sdtx_canvas() to
    dynamically resize the virtual canvas. This is recommended when
    rendering to a resizeable window. The virtual canvas size can
    also be used to scale text in relation to the display resolution.

    Examples when using sokol-app:

    - to render characters at 8x8 'physical pixels':

        sdtx_canvas(sapp_width(), sapp_height());

    - to render characters at 16x16 physical pixels:

        sdtx_canvas(sapp_width()/2.0f, sapp_height()/2.0f);

    Do *not* use integer math here, since this will not look nice
    when the render target size isn't divisible by 2.

--- Optionally define the origin for the character grid with:

        sdtx_origin(x, y);

    The provided coordinates are in character grid cells, not in
    virtual canvas pixels. E.g. to set the origin to 2 character tiles
    from the left and top border:

        sdtx_origin(2, 2);

    You can define fractions, e.g. to start rendering half
    a character tile from the top-left corner:

        sdtx_origin(0.5f, 0.5f);

--- Optionally set a different font by calling:

        switch_font(ctx, font_index)

    sokol-debugtext provides 8 font slots which can be populated
    with the builtin fonts or with user-provided font data, so
    'font_index' must be a number from 0 to 7.

--- Position the text cursor by mutating `ctx.pos.x` and `ctx.pos.y`
    directly. All values are in character grid cells as floats and 
    relative to the origin defined with sdtx_origin().

--- Set a new text color with any of the following:
        ctx.color = pack_rgba(u8, u8, u8, 255)    - RGBA 0..255
        ctx.color = pack_rgba(f32, f32, f32, 1.0) - RGBA 0.0f..1.0f
        ctx.color = u32                           - ABGR (0xAABBGGRR)

--- Output 8-bit ASCII text with the following functions:

        put(ctx, u8)             - output a single character

        put(ctx, CStr)           - output a null-terminated C string, note that
                                    this will *not* append a newline (so it behaves
                                    differently than the CRT's puts() function)

        put(ctx, Str)            - output a string (continuing past zero bytes)

    - Note that the text will not yet be rendered, only recorded for rendering
        at a later time, the actual rendering happens when sdtx_draw() is called
        inside a sokol-gfx render pass.
    - This means also you can output text anywhere in the frame, it doesn't
        have to be inside a render pass.
    - Note that character codes <32 are reserved as control characters
        and won't render anything. Currently only the following control
        characters are implemented:

        \r  - carriage return (same as `ctx.pos.x = 0;`)
        \n  - carriage return + line feed (same as `ctx.pos.x = 0; ctx.pos.y += 1;`)
        \t  - a tab character

--- You can 'record' text into render layers, this allows to mix/interleave
    sokol-debugtext rendering with other rendering operations inside
    sokol-gfx render passes. To start recording text into a different render
    layer, call:

        set_layer(ctx, int layer_id)

    ...outside a sokol-gfx render pass.

--- finally, from within a sokol-gfx render pass, call:
    
        draw_layer(ctx, int layer_id)

    ...to draw a specific layer, or for non-layered rendering, render only the 'default layer' 0:
    
        draw_layer(ctx, 0)

--- at the end of a frame (defined by the call to sg_commit()), sokol-debugtext
    will rewind all contexts:

        - the internal vertex index is set to 0
        - the internal command index is set to 0
        - the current layer id is set to 0
        - the current font is set to 0
        - the cursor position is reset


RENDERING WITH MULTIPLE CONTEXTS
================================
Use multiple text contexts if you need to render debug text in different
sokol-gfx render passes, or want to render text to different layers
in the same render pass, each with its own set of parameters.

To create a new text context call:

    sdtx_context ctx = sdtx_make_context(&(sdtx_context_desc_t){ ... });

The creation parameters in the sdtx_context_desc_t struct are the same
as already described above in the sdtx_setup() function:

    .char_buf_size      -- max number of characters rendered in one frame, default: 4096
    .canvas_width       -- the initial virtual canvas width, default: 640
    .canvas_height      -- the initial virtual canvas height, default: 400
    .tab_width          -- tab width in number of characters, default: 4
    .color_format       -- color pixel format of target render pass
    .depth_format       -- depth pixel format of target render pass
    .sample_count       -- MSAA sample count of target render pass

...and after that call the text output functions as described above, and
finally, inside a sokol-gfx render pass, call sdtx_draw() to actually
render the text for this context.

A context keeps track of the following parameters:

    - the active font
    - the virtual canvas size
    - the origin position
    - the current cursor position
    - the current tab width
    - the current color
    - and the current layer-id

The default context configured by the initial sdtx_setup call is stored in Sdtx.Common.default_context&,
and can be used interchangeably with contexts you create later. 

To destroy a context, call:

    destroy_context(ctx)

Do not attemp to use a context after destroying it. 

You can directly draw the recorded text in a specific context without
setting the active context:

    draw_layer(ctx, layer_id)

USING YOUR OWN FONT DATA
========================

Instead of the built-in fonts you can also plug your own font data
into sokol-debugtext in the sdtx_setup call.

For instance to use a built-in font at slot 0, and a user-font at
font slot 1, the sdtx_setup() call might look like this:

    sdtx_setup(&sdtx_desc_t){
        .fonts = {
            [0] = sdtx_font_kc853(),
            [1] = my_font_data,
        }
    });

Where 'my_font_data' is a byte array where every character is described
by 8 bytes arranged like this:

    bits
    7 6 5 4 3 2 1 0
    . . . X X . . .     byte 0: 0x18
    . . X X X X . .     byte 1: 0x3C
    . X X . . X X .     byte 2: 0x66
    . X X . . X X .     byte 3: 0x66
    . X X X X X X .     byte 4: 0x7E
    . X X . . X X .     byte 5: 0x66
    . X X . . X X .     byte 6: 0x66
    . . . . . . . .     byte 7: 0x00

A complete font consists of 256 characters, resulting in 2048 bytes for
the font data array (but note that the character codes 0..31 will never
be rendered).

ERROR REPORTING AND LOGGING
===========================
init_context and set_layer return false if an error occurred.

LICENSE
=======
zlib/libpng license
Copyright (c) 2020 Andre Weissflog
