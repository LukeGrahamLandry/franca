/*
    Embedded source code compiled with:

    sokol-shdc -i sgl.glsl -o sgl.h -l glsl410:glsl300es:hlsl4:metal_macos:metal_ios:metal_sim:wgsl -b

    (not that for Metal and D3D11 byte code, sokol-shdc must be run
    on macOS and Windows)

    @vs vs
    layout(binding=0) uniform vs_params {
        mat4 mvp;
        mat4 tm;
    };
    in vec4 position;
    in vec2 texcoord0;
    in vec4 color0;
    in float psize;
    out vec4 uv;
    out vec4 color;
    void main() {
        gl_Position = mvp * position;
        #ifndef SOKOL_WGSL
        gl_PointSize = psize;
        #endif
        uv = tm * vec4(texcoord0, 0.0, 1.0);
        color = color0;
    }
    @end

    @fs fs
    layout(binding=0) uniform texture2D tex;
    layout(binding=0) uniform sampler smp;
    in vec4 uv;
    in vec4 color;
    out vec4 frag_color;
    void main() {
        frag_color = texture(sampler2D(tex, smp), uv.xy) * color;
    }
    @end

    @program sgl vs fs
*/

#if defined(SOKOL_GLCORE)
/*
    #version 410

    uniform vec4 vs_params[8];
    layout(location = 0) in vec4 position;
    layout(location = 3) in float psize;
    layout(location = 0) out vec4 uv;
    layout(location = 1) in vec2 texcoord0;
    layout(location = 1) out vec4 color;
    layout(location = 2) in vec4 color0;

    void main()
    {
        gl_Position = mat4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]) * position;
        gl_PointSize = psize;
        uv = mat4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]) * vec4(texcoord0, 0.0, 1.0);
        color = color0;
    }
*/
static const uint8_t _sgl_vs_source_glsl410[520];
/*
    #version 410

    uniform sampler2D tex_smp;

    layout(location = 0) out vec4 frag_color;
    layout(location = 0) in vec4 uv;
    layout(location = 1) in vec4 color;

    void main()
    {
        frag_color = texture(tex_smp, uv.xy) * color;
    }
*/
static const uint8_t _sgl_fs_source_glsl410[222];
#elif defined(SOKOL_GLES3)
/*
    #version 300 es

    uniform vec4 vs_params[8];
    layout(location = 0) in vec4 position;
    layout(location = 3) in float psize;
    out vec4 uv;
    layout(location = 1) in vec2 texcoord0;
    out vec4 color;
    layout(location = 2) in vec4 color0;

    void main()
    {
        gl_Position = mat4(vs_params[0], vs_params[1], vs_params[2], vs_params[3]) * position;
        gl_PointSize = psize;
        uv = mat4(vs_params[4], vs_params[5], vs_params[6], vs_params[7]) * vec4(texcoord0, 0.0, 1.0);
        color = color0;
    }
*/
static const uint8_t _sgl_vs_source_glsl300es[481];
/*
    #version 300 es
    precision mediump float;
    precision highp int;

    uniform highp sampler2D tex_smp;

    layout(location = 0) out highp vec4 frag_color;
    in highp vec4 uv;
    in highp vec4 color;

    void main()
    {
        frag_color = texture(tex_smp, uv.xy) * color;
    }
*/
static const uint8_t _sgl_fs_source_glsl300es[253];
#elif defined(SOKOL_METAL)
/*
    #include <metal_stdlib>
    #include <simd/simd.h>

    using namespace metal;

    struct vs_params
    {
        float4x4 mvp;
        float4x4 tm;
    };

    struct main0_out
    {
        float4 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
        float4 gl_Position [[position]];
        float gl_PointSize [[point_size]];
    };

    struct main0_in
    {
        float4 position [[attribute(0)]];
        float2 texcoord0 [[attribute(1)]];
        float4 color0 [[attribute(2)]];
        float psize [[attribute(3)]];
    };

    vertex main0_out main0(main0_in in [[stage_in]], constant vs_params& _19 [[buffer(0)]])
    {
        main0_out out = {};
        out.gl_Position = _19.mvp * in.position;
        out.gl_PointSize = in.psize;
        out.uv = _19.tm * float4(in.texcoord0, 0.0, 1.0);
        out.color = in.color0;
        return out;
    }
*/
static const uint8_t _sgl_vs_bytecode_metal_macos[3381];
/*
    #include <metal_stdlib>
    #include <simd/simd.h>

    using namespace metal;

    struct main0_out
    {
        float4 frag_color [[color(0)]];
    };

    struct main0_in
    {
        float4 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
    };

    fragment main0_out main0(main0_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]])
    {
        main0_out out = {};
        out.frag_color = tex.sample(smp, in.uv.xy) * in.color;
        return out;
    }
*/
static const uint8_t _sgl_fs_bytecode_metal_macos[3033];
/*
    #include <metal_stdlib>
    #include <simd/simd.h>

    using namespace metal;

    struct vs_params
    {
        float4x4 mvp;
        float4x4 tm;
    };

    struct main0_out
    {
        float4 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
        float4 gl_Position [[position]];
        float gl_PointSize [[point_size]];
    };

    struct main0_in
    {
        float4 position [[attribute(0)]];
        float2 texcoord0 [[attribute(1)]];
        float4 color0 [[attribute(2)]];
        float psize [[attribute(3)]];
    };

    vertex main0_out main0(main0_in in [[stage_in]], constant vs_params& _19 [[buffer(0)]])
    {
        main0_out out = {};
        out.gl_Position = _19.mvp * in.position;
        out.gl_PointSize = in.psize;
        out.uv = _19.tm * float4(in.texcoord0, 0.0, 1.0);
        out.color = in.color0;
        return out;
    }
*/
static const uint8_t _sgl_vs_bytecode_metal_ios[3493];
/*
    #include <metal_stdlib>
    #include <simd/simd.h>

    using namespace metal;

    struct main0_out
    {
        float4 frag_color [[color(0)]];
    };

    struct main0_in
    {
        float4 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
    };

    fragment main0_out main0(main0_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]])
    {
        main0_out out = {};
        out.frag_color = tex.sample(smp, in.uv.xy) * in.color;
        return out;
    }
*/
static const uint8_t _sgl_fs_bytecode_metal_ios[3017];
/*
    #include <metal_stdlib>
    #include <simd/simd.h>

    using namespace metal;

    struct vs_params
    {
        float4x4 mvp;
        float4x4 tm;
    };

    struct main0_out
    {
        float4 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
        float4 gl_Position [[position]];
        float gl_PointSize [[point_size]];
    };

    struct main0_in
    {
        float4 position [[attribute(0)]];
        float2 texcoord0 [[attribute(1)]];
        float4 color0 [[attribute(2)]];
        float psize [[attribute(3)]];
    };

    vertex main0_out main0(main0_in in [[stage_in]], constant vs_params& _19 [[buffer(0)]])
    {
        main0_out out = {};
        out.gl_Position = _19.mvp * in.position;
        out.gl_PointSize = in.psize;
        out.uv = _19.tm * float4(in.texcoord0, 0.0, 1.0);
        out.color = in.color0;
        return out;
    }
*/
static const uint8_t _sgl_vs_source_metal_sim[756];
/*
    #include <metal_stdlib>
    #include <simd/simd.h>

    using namespace metal;

    struct main0_out
    {
        float4 frag_color [[color(0)]];
    };

    struct main0_in
    {
        float4 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
    };

    fragment main0_out main0(main0_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]])
    {
        main0_out out = {};
        out.frag_color = tex.sample(smp, in.uv.xy) * in.color;
        return out;
    }
*/
static const uint8_t _sgl_fs_source_metal_sim[439];
#elif defined(SOKOL_D3D11)
/*
    cbuffer vs_params : register(b0)
    {
        row_major float4x4 _19_mvp : packoffset(c0);
        row_major float4x4 _19_tm : packoffset(c4);
    };


    static float4 gl_Position;
    static float gl_PointSize;
    static float4 position;
    static float psize;
    static float4 uv;
    static float2 texcoord0;
    static float4 color;
    static float4 color0;

    struct SPIRV_Cross_Input
    {
        float4 position : TEXCOORD0;
        float2 texcoord0 : TEXCOORD1;
        float4 color0 : TEXCOORD2;
        float psize : TEXCOORD3;
    };

    struct SPIRV_Cross_Output
    {
        float4 uv : TEXCOORD0;
        float4 color : TEXCOORD1;
        float4 gl_Position : SV_Position;
    };

    void vert_main()
    {
        gl_Position = mul(position, _19_mvp);
        gl_PointSize = psize;
        uv = mul(float4(texcoord0, 0.0f, 1.0f), _19_tm);
        color = color0;
    }

    SPIRV_Cross_Output main(SPIRV_Cross_Input stage_input)
    {
        position = stage_input.position;
        psize = stage_input.psize;
        texcoord0 = stage_input.texcoord0;
        color0 = stage_input.color0;
        vert_main();
        SPIRV_Cross_Output stage_output;
        stage_output.gl_Position = gl_Position;
        stage_output.uv = uv;
        stage_output.color = color;
        return stage_output;
    }
*/
static const uint8_t _sgl_vs_bytecode_hlsl4[1032];
/*
    Texture2D<float4> tex : register(t0);
    SamplerState smp : register(s0);

    static float4 frag_color;
    static float4 uv;
    static float4 color;

    struct SPIRV_Cross_Input
    {
        float4 uv : TEXCOORD0;
        float4 color : TEXCOORD1;
    };

    struct SPIRV_Cross_Output
    {
        float4 frag_color : SV_Target0;
    };

    void frag_main()
    {
        frag_color = tex.Sample(smp, uv.xy) * color;
    }

    SPIRV_Cross_Output main(SPIRV_Cross_Input stage_input)
    {
        uv = stage_input.uv;
        color = stage_input.color;
        frag_main();
        SPIRV_Cross_Output stage_output;
        stage_output.frag_color = frag_color;
        return stage_output;
    }
*/
static const uint8_t _sgl_fs_bytecode_hlsl4[608];
#elif defined(SOKOL_WGPU)
/*
    diagnostic(off, derivative_uniformity);

    struct vs_params {
      /_ @offset(0) _/
      mvp : mat4x4f,
      /_ @offset(64) _/
      tm : mat4x4f,
    }

    @group(0) @binding(0) var<uniform> x_19 : vs_params;

    var<private> position_1 : vec4f;

    var<private> uv : vec4f;

    var<private> texcoord0 : vec2f;

    var<private> color : vec4f;

    var<private> color0 : vec4f;

    var<private> psize : f32;

    var<private> gl_Position : vec4f;

    fn main_1() {
      let x_22 : mat4x4f = x_19.mvp;
      let x_25 : vec4f = position_1;
      gl_Position = (x_22 * x_25);
      let x_32 : mat4x4f = x_19.tm;
      let x_36 : vec2f = texcoord0;
      uv = (x_32 * vec4f(x_36.x, x_36.y, 0.0f, 1.0f));
      let x_45 : vec4f = color0;
      color = x_45;
      return;
    }

    struct main_out {
      @builtin(position)
      gl_Position : vec4f,
      @location(0)
      uv_1 : vec4f,
      @location(1)
      color_1 : vec4f,
    }

    @vertex
    fn main(@location(0) position_1_param : vec4f, @location(1) texcoord0_param : vec2f, @location(2) color0_param : vec4f, @location(3) psize_param : f32) -> main_out {
      position_1 = position_1_param;
      texcoord0 = texcoord0_param;
      color0 = color0_param;
      psize = psize_param;
      main_1();
      return main_out(gl_Position, uv, color);
    }
*/
static const uint8_t _sgl_vs_source_wgsl[1162];
/*
    diagnostic(off, derivative_uniformity);

    var<private> frag_color : vec4f;

    @group(1) @binding(64) var tex : texture_2d<f32>;

    @group(1) @binding(80) var smp : sampler;

    var<private> uv : vec4f;

    var<private> color : vec4f;

    fn main_1() {
      let x_23 : vec4f = uv;
      let x_25 : vec4f = textureSample(tex, smp, vec2f(x_23.x, x_23.y));
      let x_27 : vec4f = color;
      frag_color = (x_25 * x_27);
      return;
    }

    struct main_out {
      @location(0)
      frag_color_1 : vec4f,
    }

    @fragment
    fn main(@location(0) uv_param : vec4f, @location(1) color_param : vec4f) -> main_out {
      uv = uv_param;
      color = color_param;
      main_1();
      return main_out(frag_color);
    }
*/
static const uint8_t _sgl_fs_source_wgsl[647];
#endif
