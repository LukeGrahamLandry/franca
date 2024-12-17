// https://github.com/floooh/sokol/blob/bb7126bead8d482bcbb4981cd92d2d5dbb7536a1/util/sokol_debugtext.h
/*
    Embedded source code compiled with:

    sokol-shdc -i debugtext.glsl -o debugtext.h -l glsl410:glsl300es:hlsl4:metal_macos:metal_ios:metal_sim:wgsl -b

    (note that for Metal and D3D11 byte code, sokol-shdc must be run
    on macOS and Windows)

        @vs vs
        in vec2 position;
        in vec2 texcoord0;
        in vec4 color0;
        out vec2 uv;
        out vec4 color;
        void main() {
          gl_Position = vec4(position * vec2(2.0, -2.0) + vec2(-1.0, +1.0), 0.0, 1.0);
          uv = texcoord0;
          color = color0;
        }
        @end

        @fs fs
        layout(binding=0) uniform texture2D tex;
        layout(binding=0) uniform sampler smp;
        in vec2 uv;
        in vec4 color;
        out vec4 frag_color;
        void main() {
          frag_color = texture(sampler2D(tex, smp), uv).xxxx * color;
        }
        @end

        @program debugtext vs fs
*/
#if defined(SOKOL_GLCORE)
/*
    #version 410

    layout(location = 0) in vec2 position;
    layout(location = 0) out vec2 uv;
    layout(location = 1) in vec2 texcoord0;
    layout(location = 1) out vec4 color;
    layout(location = 2) in vec4 color0;

    void main()
    {
        gl_Position = vec4(fma(position, vec2(2.0, -2.0), vec2(-1.0, 1.0)), 0.0, 1.0);
        uv = texcoord0;
        color = color0;
    }
*/
static const uint8_t _sdtx_vs_source_glsl410[343];
/*
    #version 410

    uniform sampler2D tex_smp;

    layout(location = 0) out vec4 frag_color;
    layout(location = 0) in vec2 uv;
    layout(location = 1) in vec4 color;

    void main()
    {
        frag_color = texture(tex_smp, uv).xxxx * color;
    }
*/
static const uint8_t _sdtx_fs_source_glsl410[224];
#elif defined(SOKOL_GLES3)
/*
    #version 300 es

    layout(location = 0) in vec2 position;
    out vec2 uv;
    layout(location = 1) in vec2 texcoord0;
    out vec4 color;
    layout(location = 2) in vec4 color0;

    void main()
    {
        gl_Position = vec4(position * vec2(2.0, -2.0) + vec2(-1.0, 1.0), 0.0, 1.0);
        uv = texcoord0;
        color = color0;
    }
*/
static const uint8_t _sdtx_vs_source_glsl300es[301];
/*
    #version 300 es
    precision mediump float;
    precision highp int;

    uniform highp sampler2D tex_smp;

    layout(location = 0) out highp vec4 frag_color;
    in highp vec2 uv;
    in highp vec4 color;

    void main()
    {
        frag_color = texture(tex_smp, uv).xxxx * color;
    }
*/
static const uint8_t _sdtx_fs_source_glsl300es[255];
#elif defined(SOKOL_METAL)
/*
    #include <metal_stdlib>
    #include <simd/simd.h>

    using namespace metal;

    struct main0_out
    {
        float2 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
        float4 gl_Position [[position]];
    };

    struct main0_in
    {
        float2 position [[attribute(0)]];
        float2 texcoord0 [[attribute(1)]];
        float4 color0 [[attribute(2)]];
    };

    vertex main0_out main0(main0_in in [[stage_in]])
    {
        main0_out out = {};
        out.gl_Position = float4(fma(in.position, float2(2.0, -2.0), float2(-1.0, 1.0)), 0.0, 1.0);
        out.uv = in.texcoord0;
        out.color = in.color0;
        return out;
    }
*/
static const uint8_t _sdtx_vs_bytecode_metal_macos[2892];
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
        float2 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
    };

    fragment main0_out main0(main0_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]])
    {
        main0_out out = {};
        out.frag_color = tex.sample(smp, in.uv).xxxx * in.color;
        return out;
    }
*/
static const uint8_t _sdtx_fs_bytecode_metal_macos[3033];
/*
    #include <metal_stdlib>
    #include <simd/simd.h>

    using namespace metal;

    struct main0_out
    {
        float2 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
        float4 gl_Position [[position]];
    };

    struct main0_in
    {
        float2 position [[attribute(0)]];
        float2 texcoord0 [[attribute(1)]];
        float4 color0 [[attribute(2)]];
    };

    vertex main0_out main0(main0_in in [[stage_in]])
    {
        main0_out out = {};
        out.gl_Position = float4(fma(in.position, float2(2.0, -2.0), float2(-1.0, 1.0)), 0.0, 1.0);
        out.uv = in.texcoord0;
        out.color = in.color0;
        return out;
    }
*/
static const uint8_t _sdtx_vs_bytecode_metal_ios[2876];
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
        float2 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
    };

    fragment main0_out main0(main0_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]])
    {
        main0_out out = {};
        out.frag_color = tex.sample(smp, in.uv).xxxx * in.color;
        return out;
    }

*/
static const uint8_t _sdtx_fs_bytecode_metal_ios[3033];
/*
    #include <metal_stdlib>
    #include <simd/simd.h>

    using namespace metal;

    struct main0_out
    {
        float2 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
        float4 gl_Position [[position]];
    };

    struct main0_in
    {
        float2 position [[attribute(0)]];
        float2 texcoord0 [[attribute(1)]];
        float4 color0 [[attribute(2)]];
    };

    vertex main0_out main0(main0_in in [[stage_in]])
    {
        main0_out out = {};
        out.gl_Position = float4(fma(in.position, float2(2.0, -2.0), float2(-1.0, 1.0)), 0.0, 1.0);
        out.uv = in.texcoord0;
        out.color = in.color0;
        return out;
    }

*/
static const uint8_t _sdtx_vs_source_metal_sim[577];
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
        float2 uv [[user(locn0)]];
        float4 color [[user(locn1)]];
    };

    fragment main0_out main0(main0_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]])
    {
        main0_out out = {};
        out.frag_color = tex.sample(smp, in.uv).xxxx * in.color;
        return out;
    }

*/
static const uint8_t _sdtx_fs_source_metal_sim[441];
#elif defined(SOKOL_D3D11)
/*
    static float4 gl_Position;
    static float2 position;
    static float2 uv;
    static float2 texcoord0;
    static float4 color;
    static float4 color0;

    struct SPIRV_Cross_Input
    {
        float2 position : TEXCOORD0;
        float2 texcoord0 : TEXCOORD1;
        float4 color0 : TEXCOORD2;
    };

    struct SPIRV_Cross_Output
    {
        float2 uv : TEXCOORD0;
        float4 color : TEXCOORD1;
        float4 gl_Position : SV_Position;
    };

    void vert_main()
    {
        gl_Position = float4(mad(position, float2(2.0f, -2.0f), float2(-1.0f, 1.0f)), 0.0f, 1.0f);
        uv = texcoord0;
        color = color0;
    }

    SPIRV_Cross_Output main(SPIRV_Cross_Input stage_input)
    {
        position = stage_input.position;
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
static const uint8_t _sdtx_vs_bytecode_hlsl4[692];
/*
    Texture2D<float4> tex : register(t0);
    SamplerState smp : register(s0);

    static float4 frag_color;
    static float2 uv;
    static float4 color;

    struct SPIRV_Cross_Input
    {
        float2 uv : TEXCOORD0;
        float4 color : TEXCOORD1;
    };

    struct SPIRV_Cross_Output
    {
        float4 frag_color : SV_Target0;
    };

    void frag_main()
    {
        frag_color = tex.Sample(smp, uv).xxxx * color;
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
static const uint8_t _sdtx_fs_bytecode_hlsl4[608];
#elif defined(SOKOL_WGPU)
/*
    diagnostic(off, derivative_uniformity);

    var<private> position_1 : vec2f;

    var<private> uv : vec2f;

    var<private> texcoord0 : vec2f;

    var<private> color : vec4f;

    var<private> color0 : vec4f;

    var<private> gl_Position : vec4f;

    fn main_1() {
      let x_19 : vec2f = position_1;
      let x_27 : vec2f = ((x_19 * vec2f(2.0f, -2.0f)) + vec2f(-1.0f, 1.0f));
      gl_Position = vec4f(x_27.x, x_27.y, 0.0f, 1.0f);
      let x_37 : vec2f = texcoord0;
      uv = x_37;
      let x_41 : vec4f = color0;
      color = x_41;
      return;
    }

    struct main_out {
      @builtin(position)
      gl_Position : vec4f,
      @location(0)
      uv_1 : vec2f,
      @location(1)
      color_1 : vec4f,
    }

    @vertex
    fn main(@location(0) position_1_param : vec2f, @location(1) texcoord0_param : vec2f, @location(2) color0_param : vec4f) -> main_out {
      position_1 = position_1_param;
      texcoord0 = texcoord0_param;
      color0 = color0_param;
      main_1();
      return main_out(gl_Position, uv, color);
    }

*/
static const uint8_t _sdtx_vs_source_wgsl[922];
/*
    diagnostic(off, derivative_uniformity);

    var<private> frag_color : vec4f;

    @group(1) @binding(64) var tex : texture_2d<f32>;

    @group(1) @binding(80) var smp : sampler;

    var<private> uv : vec2f;

    var<private> color : vec4f;

    fn main_1() {
      let x_23 : vec2f = uv;
      let x_24 : vec4f = textureSample(tex, smp, x_23);
      let x_28 : vec4f = color;
      frag_color = (vec4f(x_24.x, x_24.x, x_24.x, x_24.x) * x_28);
      return;
    }

    struct main_out {
      @location(0)
      frag_color_1 : vec4f,
    }

    @fragment
    fn main(@location(0) uv_param : vec2f, @location(1) color_param : vec4f) -> main_out {
      uv = uv_param;
      color = color_param;
      main_1();
      return main_out(frag_color);
    }
*/
static const uint8_t _sdtx_fs_source_wgsl[663];
#endif
