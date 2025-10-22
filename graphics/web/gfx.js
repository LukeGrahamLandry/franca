import { webgpu, reader as R } from "../../target/franca/webgpu.g.js";
import * as FrancaGraphics from "../../graphics/web/app.js";

let G = {
    wasm: null,
    canvas: null,
    adapter: null,
    device: null,
    surface: null,
    
    // TODO: ref counts and free list of indices
    objects: [undefined],
    
    push: function(it) {
        if (it === null || it === undefined) return 0;
        this.objects.push(it);
        return BigInt(this.objects.length - 1);
    },
    get: function(it) {
        return this.objects[Number(it)];
    },
    M: function() {
        return this.wasm.instance.exports.memory.buffer;
    }
};

export const webgpu_wasm_exports = webgpu;
export const init_gpu = async (wasm, canvas) => {
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
};

webgpu.francaRequestState = (I, frame_callback_p) => {
    console.log(G);
    let [width, height] = [BigInt(G.canvas.width), BigInt(G.canvas.width)];
    G.wasm.instance.exports.francaSaveState(I, G.adapter, G.device, G.surface, width, height);
    FrancaGraphics.js_init(I, G.wasm, G.canvas);
    requestAnimationFrame(call_frame);
    const frame_callback = G.wasm.instance.exports.__indirect_table.get(Number(frame_callback_p));
    function call_frame() {
        frame_callback(I);
        requestAnimationFrame(call_frame);
    }
};

webgpu.wgpuDeviceGetQueue = (device) => {
    // TODO: don't give it a new slot every time
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


R.u32 = (i) => new DataView(G.M()).getUint32(Number(i), true);
R.p64 = (i) => new DataView(G.M()).getBigUint64(Number(i), true);
R.f64 = (i) => new DataView(G.M()).getFloat64(Number(i), true);
R.r64 = (i) => G.get(R.p64(i));
R.i64 = function (i) {
    return Number(this.p64(i));
};
R.Str = function (i) {
    let ptr = this.i64(i);
    let len = this.i64(i + 8n);
    if (len == 0) return "";
    let buf = new Uint8Array(G.M(), ptr, len);
    return new TextDecoder().decode(buf);
};
R.Bool = function (i) {
    return this.u32(i) != 0;
};

R.array = function (reader, element_size, count, address_of_pointer) {
    if (count == 0xFFFFFFFFn) return undefined;
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

let BindGroupEntry = R.BindGroupEntry.bind(R);
R.BindGroupEntry = function (i) {
    let o = BindGroupEntry(i);
    o.resource = o;
    return o;
}

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
