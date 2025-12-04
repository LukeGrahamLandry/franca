import { webgpu, reader as R, writer as W } from "./webgpu.g.js";

let G = create(); 
export function reset_G() {
    cancelAnimationFrame(G.animation_id);
    G = create();
}
export function get_G() {
    return G;
}
export function create() { return {
    wasm: null,
    canvas: null,
    adapter: null,
    device: null,
    surface: null,
    valid: false,
    I: null,
    animation_id: null,
    
    objects: [undefined],
    refs: [0],
    free: [],
    
    push: function(it) {
        if (it === null || it === undefined) return 0;
        if (this.free.length == 0) {
            this.objects.push(it);
            this.refs.push(1);
            return BigInt(this.objects.length - 1);
        } else {
            let i = this.free.pop();
            this.objects[i] = it;
            this.refs[i] = 1;
            return BigInt(i);
        }
    },
    get: function(it) {
        return this.objects[Number(it)];
    },
    ref: function(it) {
        this.refs[Number(it)] += 1;
    },
    release: function(it) {
        it = Number(it);
        this.refs[it] -= 1;
        if (this.refs[it] <= 0) {
            this.objects[it] = undefined;
            this.free.push(it);
        }
    },
    M: function() {
        return this.wasm.instance.exports.memory.buffer;
    }
}};

export const webgpu_wasm_exports = webgpu;
export const init_gpu = async (wasm, canvas) => {
    if (navigator.gpu === undefined) return;  // should be unreachable but clearly not
    G.wasm = wasm;
    G.canvas = canvas;
    const adapter = await navigator.gpu.requestAdapter();
    G.adapter = G.push(adapter);
    const device = await adapter.requestDevice();
    G.device = G.push(device);
    const surface = canvas.getContext('webgpu');
    G.surface = G.push(surface);
    // TODO: configure options should be done from the franca side. 
    //       requestDevice/requestAdapter are more painful because they're async. 
    surface.configure({
      device,
      format: navigator.gpu.getPreferredCanvasFormat(),
    });
    G.valid = true;
};

export const ESCAPE_MAIN = "escape main";

webgpu.francaRequestState = (I, frame_callback_p, init_callback_p) => {
    if (!G.valid) console.error("called francaRequestState before init_gpu");
    G.I = I;
    let [width, height] = [BigInt(G.canvas.width), BigInt(G.canvas.height)];
    let ratio = 2; // TODO: window.devicePixelRatio
    const init_callback = G.wasm.instance.exports.__indirect_table.get(Number(init_callback_p));
    init_callback(I, G.adapter, G.device, G.surface, width, height, ratio);
    G.animation_id = requestAnimationFrame(call_frame);
    const frame_callback = G.wasm.instance.exports.__indirect_table.get(Number(frame_callback_p));
    function call_frame() {
        let noframe = frame_callback(I) != 0;
        // TODO: figure out how to make should_skip_frame work. 
        //       there's no present() call so if you try to just drop the frame 
        //       you get black instead of the previous frame. 
        G.animation_id = requestAnimationFrame(call_frame);
    }
    
    // TODO: make it less painful to setup a dynamicenvironment thats not on your stack frame
    //       but need to be careful because that changes the semantics of app.run
    throw ESCAPE_MAIN;
};

webgpu.wgpuDeviceGetQueue = (device) => {
    return G.push(G.get(device).queue);
};

webgpu.wgpuDeviceCreateBuffer = (device, i) => {
    const o = R.BufferDescriptor(i);
    return G.push(G.get(device).createBuffer(o));
};

webgpu.wgpuDeviceCreateBindGroupLayout = (device, i) => {
    const o = R.BindGroupLayoutDescriptor(i);
    return G.push(G.get(device).createBindGroupLayout(o));
};

webgpu.wgpuDeviceCreateBindGroup = (device, i) => {
    const o = R.BindGroupDescriptor(i);
    return G.push(G.get(device).createBindGroup(o));
};

webgpu.wgpuDeviceCreateCommandEncoder = (device, i) => {
    const o = R.CommandEncoderDescriptor(i);
    return G.push(G.get(device).createCommandEncoder(o));
};

webgpu.wgpuQueueWriteBuffer = (queue, buffer, buffer_offset, data, size) => {
    const src = new Uint8Array(G.M(), Number(data), Number(size));
    G.get(queue).writeBuffer(G.get(buffer), Number(buffer_offset), src, 0, Number(size));
}

webgpu.wgpuDeviceGetLimits = (device, i) => {
    let o = G.get(device).limits;
    W.Limits(i, o);
}

webgpu.wgpuDeviceCreateShaderModule = (device, i) => {
    const o = R.ShaderModuleDescriptor(i);
    return G.push(G.get(device).createShaderModule(o));
};

webgpu.wgpuDeviceCreatePipelineLayout = (device, i) => {
    const o = R.PipelineLayoutDescriptor(i);
    return G.push(G.get(device).createPipelineLayout(o));
};

webgpu.wgpuDeviceCreateRenderPipeline = (device, i) => {
    const o = R.RenderPipelineDescriptor(i);
    return G.push(G.get(device).createRenderPipeline(o));
};

webgpu.wgpuTextureCreateView = (texure, i) => {
    const o = R.TextureViewDescriptor(i);
    return G.push(G.get(texure).createView(o));
};
webgpu.wgpuCommandEncoderBeginRenderPass = (encoder, i) => {
    const o = R.RenderPassDescriptor(i);
    return G.push(G.get(encoder).beginRenderPass(o));
};

webgpu.wgpuRenderPassEncoderSetBindGroup = (self, group_index, group, dynamic_offset_count, dynamic_offsets) => {
    let offsets = R.array_loaded(R.u32, 4n, dynamic_offset_count, dynamic_offsets);
    G.get(self).setBindGroup(group_index, G.get(group), offsets);
};
webgpu.wgpuRenderPassEncoderSetPipeline = (self, pipeline) => {
    G.get(self).setPipeline(G.get(pipeline));
};
webgpu.wgpuRenderPassEncoderSetStencilReference = (self, reference) => {
    G.get(self).setStencilReference(reference);
};
webgpu.wgpuRenderPassEncoderSetBlendConstant = (self, color) => {
    G.get(self).setBlendConstant(R.Color(color));
};

webgpu.wgpuRenderPassEncoderDraw = (self, vertex_count, instance_count, first_vertex, first_instance) => {
    G.get(self).draw(vertex_count, instance_count, first_vertex, first_instance);
};

webgpu.wgpuRenderPassEncoderEnd = (self) => {
    G.get(self).end();
};

webgpu.wgpuQueueSubmit = (self, command_count, commands) => {
    let o = R.array_loaded(R.CommandBuffer, 8n, command_count, commands);
    G.get(self).submit(o);
}

webgpu.wgpuCommandEncoderFinish = (self, i) => {
    let o = R.CommandBufferDescriptor(i);
    return G.push(G.get(self).finish(o));
};

webgpu.wgpuSurfaceGetCurrentTexture = (surface, o) => {
    let texture = G.push(G.get(surface).getCurrentTexture());
    let SuccessOptimal = 1;
    let M = new DataView(G.M());
    M.setBigUint64(Number(o + 8n), texture, true);
    M.setUint32(Number(o + 16n), SuccessOptimal, true);
};

webgpu.wgpuBufferUnmap = (buffer) => {
    G.get(buffer).unmap();
};

webgpu.wgpuSurfacePresent = (surface) => {
    // nop
};

webgpu.wgpuInstanceProcessEvents = (instance) => {
    // nop
};
webgpu.wgpuDeviceCreateTexture = (device, i) => {
    let o = R.TextureDescriptor(i);
    return G.push(G.get(device).createTexture(o));
}

webgpu.wgpuQueueWriteTexture = (self, destination, data, data_size, data_layout, write_size) => {
    destination = R.TexelCopyTextureInfo(destination);
    data_layout = R.TexelCopyBufferLayout(data_layout);
    write_size = R.Extent3D(write_size);
    let src = new Uint8Array(G.M(), Number(data), Number(data_size));
    G.get(self).writeTexture(destination, src, data_layout, write_size);
};

webgpu.wgpuDeviceCreateSampler = (self, descriptor) => {
    let o = R.SamplerDescriptor(descriptor);
    return G.push(G.get(self).createSampler(o));
};

webgpu.wgpuRenderPassEncoderSetViewport = (self, x, y, width, height, min_depth, max_depth) => {
    G.get(self).setViewport(x, y, width, height, min_depth, max_depth);
};

R.u32 = (i) => new DataView(G.M()).getUint32(Number(i), true);
R.u16 = (i) => new DataView(G.M()).getUint16(Number(i), true);
R.p64 = (i) => new DataView(G.M()).getBigUint64(Number(i), true);
R.f64 = (i) => new DataView(G.M()).getFloat64(Number(i), true);
R.f32 = (i) => new DataView(G.M()).getFloat32(Number(i), true);
R.r64 = (i) => G.get(R.p64(i));
R.i64 = function (i) {
    return Number(this.p64(i));
};
R.Str = function (i) {
    let ptr = this.i64(i);
    let len = this.i64(i + 8n);
    if (len == 0) return "";
    let buf = new Uint8Array(G.M(), ptr, len);
    const wasteofmytime = new ArrayBuffer(buf.byteLength);
    new Uint8Array(wasteofmytime).set(new Uint8Array(buf));
    return new TextDecoder().decode(wasteofmytime);
};
R.Bool = function (i) {
    return this.u32(i) != 0;
};

R.array = function (reader, element_size, count, address_of_pointer) {
    if (count == 0xFFFFFFFFn) return undefined;
    if (count == 0n) return [];
    return R.array_loaded(reader, element_size, count, this.p64(address_of_pointer));
};

R.array_loaded = function (reader, element_size, count, it) {
    if (count == 0xFFFFFFFFn) return undefined;
    reader = reader.bind(this);
    let o = new Array(Number(count));
    for (let i = 0; i < o.length; i++) {
        o[i] = reader(it);
        it += element_size;
    }
    return o;
};

R.pointer = function (reader, address_of_pointer) {
    reader = reader.bind(this);
    let it = this.p64(address_of_pointer);
    if (it == 0n) return undefined;
    return reader(it);
};

let ShaderModuleDescriptor = R.ShaderModuleDescriptor.bind(R);
R.ShaderModuleDescriptor = function (i) {
    let o = ShaderModuleDescriptor(i);
    let nextInChain = this.p64(i);
    let ShaderSourceWGSL = 2;
    let sType = this.u32(nextInChain + 8n);
    if (sType != ShaderSourceWGSL) throw "ShaderModuleDescriptor expected ShaderSourceWGSL";
    o.code = this.Str(nextInChain + 16n);
    return o;
}

let RenderPassColorAttachment = R.RenderPassColorAttachment.bind(R);
R.RenderPassColorAttachment = function (i) {
    let o = RenderPassColorAttachment(i);
    if (o.depthSlice == 0xFFFFFFFF) o.depthSlice = undefined;
    return o;
}

// in the c api you provide all three as optional pointers and only set one of them, 
// in the js api you only provide one in the resource field (which doesn't exist in the c api). 
let BindGroupEntry = R.BindGroupEntry.bind(R);
R.BindGroupEntry = function (i) {
    let o = BindGroupEntry(i);
    if (o.buffer !== undefined) o.resource = o;
    if (o.sampler !== undefined) o.resource = o.sampler;
    if (o.textureView !== undefined) o.resource = o.textureView;
    return o;
}

// :JsReprLiftUndefined
for (let name of ["BufferBindingLayout", "TextureBindingLayout", "SamplerBindingLayout", "StorageTextureBindingLayout"]) {
    let prev = R[name].bind(R);
    R[name] = function (i) {
        let o = prev(i);
        let type = this.i64(i + 8n);
        let BindingNotUsed = 0;
        if (type == BindingNotUsed) return undefined;
        return o;
    }
}

for (let name of Object.keys(webgpu)) {
    if (name.endsWith("Release")){
        webgpu[name] = (self) => G.release(self);
    }
    if (name.endsWith("AddRef")){
        webgpu[name] = (self) => G.ref(self);
    }
}

W.u32 = (i, o) => new DataView(G.M()).setUint32(Number(i), o, true);
W.i64 = (i, o) => new DataView(G.M()).setBigUint64(Number(i), BigInt(o), true);
