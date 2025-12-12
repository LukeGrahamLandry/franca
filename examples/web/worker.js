
let Franca;
let module;
let canvas;
let current_exit_futex;

let known_wasm_jit_event = 0n;

function handle_child_thread(child) {
    const [userdata, stack, exit_futex] = child;
    Franca.__stackbase.value = Number(stack);
    current_exit_futex = exit_futex;
    known_wasm_jit_event = Franca.wasm_init_thread(userdata, known_wasm_jit_event);
    set_zero_and_wake(exit_futex);
    self.postMessage({ tag: "recycle" });
}

function set_zero_and_wake(exit_futex) {
    let mem = new DataView(Franca.memory.buffer);
    mem.setInt32(Number(exit_futex), 0);
    let buf = new Int32Array(Franca.memory.buffer, Number(exit_futex), 1);
    Atomics.notify(buf, 0);
}

export async function handleWasmLoaded(wasm_instance, msg) {
    const { args, child, devicePixelRatio } = msg;
    Franca = wasm_instance.exports;
    if (child !== undefined) {
        handle_child_thread(child);
        return;
    }
    
    let load_end = performance.now();

    if (canvas !== undefined) await init_gpu(wasm_instance, canvas, devicePixelRatio);
    
    let ok = true;
    let p = write_args(args);
    try {
        Franca.main(BigInt(args.length), p, 0n, 0n);
    } catch (e) {
        if (e !== "called exit 0" && e !== ESCAPE_MAIN) {
            show_error(e);
            ok = false;
        }
    }
    let time = Math.round(performance.now() - load_end);
    show_log(" Ran in " + time + "ms.");
    self.postMessage({ tag: "done", ok: ok, });
}

// i this so much
function write_args(args) {
    // this is important even without the arguments because the 300 
    // starting pages is arbitrary and might not be enough for the module
    let extra = Franca.__stackbase.value - (Franca.memory.grow(0) * 65536);
    if (extra > 0) {
        let pages = Math.round((extra + 65535) / 65536);
        Franca.memory.grow(pages);
    }
    
    var e = new TextEncoder();
    let length = (args.length + 1) * 8;
    let off = length;
    for (let i = 0; i < args.length; i++) {
        args[i] = e.encode(args[i]);
        length += args[i].byteLength + 1;
    }
    
    length = Math.round((length + 15) / 16) * 16;
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

import { webgpu_wasm_exports, init_gpu, ESCAPE_MAIN, reset_G, get_G } from "./target/gfx.js";
export const imports = {
    main: {
        memory: null,
    },
    webgpu: webgpu_wasm_exports,
    env: {
        fmodf: (a, b) => a % b,
        fmod: (a, b) => a % b,
        sinf: (a) => Math.sin(a),
        cosf: (a) => Math.cos(a),
        sqrt: (a) => Math.sqrt(a),
        pow: (a, b) => Math.pow(a, b),
        acos: (a) => Math.acos(a),
        cos: (a) => Math.cos(a),
        fabs: (a) => Math.fabs(a),
        floor: (a) => Math.floor(a),
        ceil: (a) => Math.ceil(a),

        js_worker_stop: (status) => {
            if (current_exit_futex !== undefined) set_zero_and_wake(current_exit_futex);
            let G = get_G();
            if (status == 0n) throw "called exit 0";
            cancelAnimationFrame(G.animation_id);
            throw new Error("called exit " + Number(status));
        },
        jit_instantiate_module: (ptr, len, first_export, table_index) => {
            if (table_index != 0) throw new Error("table_index");
            const buffer = new Uint8Array(
                Franca.memory.buffer,
                Number(ptr),
                Number(len),
            );
            
            const wasteofmytime = new ArrayBuffer(buffer.byteLength);  // firefox
            new Uint8Array(wasteofmytime).set(new Uint8Array(buffer));
            const module = new WebAssembly.Module(wasteofmytime);
            const instance = new WebAssembly.Instance(module, { main: Franca });

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
                case 0xBBBB0002n: {
                    const sha256_hash = get_wasm_string(ptr, 64);
                    // caller (fetch_or_crash) checks that the hash matches. 
                    // should allow any url not just mine but you need to be strict 
                    // about cors for threads/gpu to work so it wouldn't matter anyway. 
                    const url = "https://lukegrahamlandry.ca/franca/mirror/" + sha256_hash;
                    const result = sync_fetch(url);
                    if (result === null || result.byteLength > Number(len)) return -1n;
                    const dest = new Uint8Array(Franca.memory.buffer, Number(ptr), result.byteLength);
                    dest.set(new Uint8Array(result));
                    return BigInt(result.byteLength);
                }
                default:
                    console.log(get_wasm_string(ptr, len));
                    return -1n;
            }
        },
        js_performace_now: () => performance.now(),
        js_worker_spawn: (userdata, stack, exit_futex) => {
            self.postMessage({ tag: "spawn", child: [userdata, stack, exit_futex], memory: imports.main.memory });
        }
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
    "FR_wasm_jit_event",
];

for (const it of weak_imports) {
    if(imports.env[it] === undefined)
    imports.env[it] = () => {
        throw new Error("called weak function: " + it);
    };
}

function handle(_msg) {
    const msg = _msg.data;
    switch (msg.tag) {
        case "start": {
            if (known_wasm_jit_event != 0n) {
                if (msg.child === undefined) throw "expected to be a child thread";
                handle_child_thread(msg.child);
                return;
            }
            imports.main.memory = msg.memory;
            canvas = msg.canvas;
            current_exit_futex = undefined;
            if (module === undefined) show_error("must setmodule before start"); 
            WebAssembly.instantiate(module, imports)
                .then(it => handleWasmLoaded(it, msg))
                .catch(show_error);
            break;
        }
        case "event": {
            let G = get_G();
            if (!G.valid || G.I === null || canvas === undefined) break;
            msg.args[0] = G.I;
            if (msg.handler == "resize_event") {
                let [clientWidth, clientHeight, devicePixelRatio] = [msg.args[2], msg.args[3], msg.args[4]];
                canvas.width = clientWidth * devicePixelRatio;
                canvas.height = clientHeight * devicePixelRatio;
            }

            Franca[msg.handler].apply(Franca, msg.args);
            break;
        }
        case "ready": break
        case "reset": {
            Franca = undefined;
            canvas = undefined;
            known_wasm_jit_event = 0n;
            imports.main.memory = undefined;
            current_exit_futex = undefined;
            // not resetting `module`, it gets reused
            reset_G();
            break
        }
        case "setmodule": {
            module = msg.module;
            break;
        }
        default:
            throw msg;
    }
};

const show = (s) => self.postMessage({ tag: "show", text: s });
const show_error = (s) => {
    if (s instanceof Error) {
        show(`${s.toString()}\n\n${s.stack}`);
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

function sync_fetch(url) {
    let it = new XMLHttpRequest();
    it.responseType = "arraybuffer";
    it.open("GET", url, false);
    it.send(null);
    if (it.status !== 200) return null;
    return it.response;
}


self.onmessage = handle;
self.postMessage({ tag: "ready" })
