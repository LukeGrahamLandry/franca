
let Franca;
let start = performance.now();
let version;
let canvas;

export async function handleWasmLoaded(wasm, args) {
    let load_end = performance.now();
    show_log(" Loaded in " + Math.round(load_end - start) + "ms.");
    Franca = wasm.instance.exports;
    console.log(Franca);

    if (canvas !== undefined) await init_gpu(wasm, canvas);
    
    let p = write_args(args);
    try {
        Franca.main(BigInt(args.length), p, 0n, 0n);
    } catch (e) {
        if (e !== "called exit 0" && e !== ESCAPE_MAIN) {
            show_error(e);
        }
    }
    let time = Math.round(performance.now() - load_end);
    show_log(" Ran in " + time + "ms.");
    self.postMessage({ tag: "done" });
}

// i this so much
function write_args(args) {
    var e = new TextEncoder();
    let length = (args.length + 1) * 8;
    let off = length;
    for (let i = 0; i < args.length; i++) {
        args[i] = e.encode(args[i]);
        length += args[i].byteLength + 1;
    }
    
    length += (8 - length / 8 * 8);
    let p = BigInt(Franca.__stackbase.value) - BigInt(length);
    Franca.__stackbase.value = Number(p);
    let pointers = new DataView(Franca.memory.buffer);
    for (let i = 0; i < args.length; i++) {
        let dest = new Uint8Array(Franca.memory.buffer, Number(p) + off, args[i].byteLength);
        // note the extra `new`, passing it an arraybuffer silently does nothing ?? 
        dest.set(new Uint8Array(args[i].buffer));
        // big endian is the default because we're smoking crack
        pointers.setBigInt64(Number(p) + i*8, p + BigInt(off), true);
        // null terminator already zero
        off += args[i].byteLength + 1;
    }
    return p;
}

import { webgpu_wasm_exports, init_gpu, G, ESCAPE_MAIN } from "./target/gfx.js";
export const imports = {
    main: {
        memory: new WebAssembly.Memory({
          initial: 300,
          maximum: 1<<(32-16),
          // TODO: if the module wants unshared, catch the exception and change this to a normal memory. this is stupid. 
          shared: true,
        }),
    },
    webgpu: webgpu_wasm_exports,
    env: {
        fmodf: (a, b) => a % b,
        fmod: (a, b) => a % b,
        sinf: (a) => Math.sin(a),
        cosf: (a) => Math.cos(a),
        js_worker_stop: (status) => {
            if (status == 0) throw "called exit 0";
            cancelAnimationFrame(G.animation_id);
            throw new Error("called exit " + status);
        },
        jit_instantiate_module: (ptr, len, first_export, table_index) => {
            if (table_index != 0) throw new Error("table_index");
            const buffer = new Uint8Array(
                Franca.memory.buffer,
                Number(ptr),
                Number(len),
            );
            const module = new WebAssembly.Module(buffer);
            const instance = new WebAssembly.Instance(module, { main: Franca });
            // console.log(instance);

            let i = Number(first_export);
            for (let func in instance.exports) {
                Franca.__indirect_table.set(i, instance.exports[func]);
                i += 1;
            }
        },
        FR_debug_write: (ptr, len) => show(get_wasm_string(ptr, len)),
        js_write: (id, ptr, len) => { 
            switch(id){
                case 0xBBBB0000n: {
                    show(get_wasm_string(ptr, len));
                    return len;
                }
                case 0xBBBB0001n: {
                    const bytes = new Uint8Array(
                        Franca.memory.buffer,
                        Number(ptr),
                        Number(len),
                    );
                    const wasteofmytime = new ArrayBuffer(bytes.byteLength);
                    new Uint8Array(wasteofmytime).set(new Uint8Array(bytes));
                    self.postMessage({
                        tag: "download",
                        content: new Blob([wasteofmytime], { type: "octet/stream" }),
                        name: "a.out",
                    });
                    return len;
                }
                default:
                    console.log(get_wasm_string(ptr, len));
                    return -1n;
            }
        },
        js_performace_now: () => performance.now(),
    },
};

let weak_imports = [
    "malloc",
    "free",
    "__libc_start_main",
    "__error",
    "__errno_location",
    "__clear_cache",
    "pthread_jit_write_protect_np",
    "unlinkat",
    "poll",
    "fork",
    "close",
    "wait4",
    "pipe",
    "nanosleep",
    "execvp",
    "read",
    "mkstemp",
    "dup2",
    "readdir$INODE64",
    "dlsym",
    "fstatat",
    "fstatat$INODE64",
    "renameat",
    "readlinkat",
    "_NSGetExecutablePath",
    "mkdirat",
    "pthread_attr_setstack",
    "opendir$INODE64",
    "closedir",
    "readdir",
    "lseek",
    "pthread_create",
    "pthread_join",
    "opendir",
    "dlopen",
    "sigaction",
    "pthread_attr_init",
    "strtod", "localtime_r", "time",
    "snprintf",
    "ppoll", "execve",
    "tcgetattr",
    "tcsetattr",
    "clock_gettime",
    "yield_file",
    "puts", "putchar", "abort", "exit", "munmap", "mprotect", "fsync", "fetch_file", "mmap", "write", "null",
    "posix_getdents", "FR_openat", 
];

for (const it of weak_imports) {
    if(imports.env[it] === undefined)
    imports.env[it] = () => {
        throw new Error("called weak function: " + it);
    };
}

const handle = (_msg) => {
    const msg = _msg.data;
    switch (msg.tag) {
        case "start": {
            version = msg.version;
            canvas = msg.canvas;
            WebAssembly.instantiateStreaming(fetch(`target/${msg.url}?v=${version}`), imports)
                .then(it => handleWasmLoaded(it, msg.args))
                .catch(show_error);
            break;
        }
        case "event": {
            if (!G.valid || G.I === null || canvas === undefined) break;
            msg.args[0] = G.I;
            if (msg.handler == "resize_event") {
                let [clientWidth, clientHeight, devicePixelRatio] = [msg.args[4], msg.args[5], msg.args[6]];
                canvas.width = clientWidth * devicePixelRatio;
                canvas.height = clientHeight * devicePixelRatio;
            }

            Franca[msg.handler].apply(Franca, msg.args);
            break;
        }
        default:
            throw msg;
    }
};

const show = (s) => self.postMessage({ tag: "show", text: s });
const show_error = (s) => {
    if (s instanceof Error) {
        console.error(s);
        s = s.toString();
    }
    self.postMessage({ tag: "err", text: s });
};
const show_log = (s) => self.postMessage({ tag: "log", text: s });

function get_wasm_string(ptr, len) {
    if (len == 0) return "";

    const buffer = new Uint8Array(
        Franca.memory.buffer,
        Number(ptr),
        Number(len),
    );
    const wasteofmytime = new ArrayBuffer(buffer.byteLength);
    new Uint8Array(wasteofmytime).set(new Uint8Array(buffer));
    return new TextDecoder().decode(wasteofmytime);
}

self.onmessage = handle;
