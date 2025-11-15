const page_size = 1n << 16n;

let bump = 0n;
let Franca;
let FrancaXXX = {};
let start = performance.now();
let version;
let fs_index;
let fs_bytes;
let canvas;

export async function handleWasmLoaded(wasm, args) {
    let load_end = performance.now();
    show_log(" Loaded in " + Math.round(load_end - start) + "ms.");
    Franca = wasm.instance.exports;
    console.log(Franca);

    // TODO: something more official than this.
    //       might be better to have page_allocator do something different instead?
    FrancaXXX.__heap_base = BigInt(Franca.memory.buffer.byteLength);
    FrancaXXX.__heap_end = FrancaXXX.__heap_base;
    
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
    let mem = (FrancaXXX.__heap_end - FrancaXXX.__heap_base) / 1024n / 1024n;
    show_log(" Ran in " + time + "ms. " + mem + "MB. ");
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
    
    let p = imports.env.mmap(0, BigInt(length), 0, 0, 0, 0);
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
    webgpu: webgpu_wasm_exports,
    env: {
        fmodf: (a, b) => a % b,
        fmod: (a, b) => a % b,
        sinf: (a) => Math.sin(a),
        cosf: (a) => Math.cos(a),
        puts: (ptr) => {
            let len = 0;
            let buf = new DataView(Franca.memory.buffer);
            while (buf.getInt8(Number(ptr) + len) != 0) {
                len += 1;
            }
            let msg = get_wasm_string(ptr, len);
            show(msg + "\n");
            return 0n;
        },
        write: (fd, ptr, len) => {
            if (fd != 1 && fd != 2) return -1n;
            const msg = get_wasm_string(ptr, len);
            show(msg);
            return len;
        },
        putchar: (c) => show(String.fromCharCode(c)),
        clock_gettime: (clock_id, ptr) => {
            const ms = performance.now();
            // @struct(seconds: i64, nanoseconds: i64);
            const time_spec = new BigInt64Array(
                Franca.memory.buffer,
                Number(ptr),
                2,
            );
            const seconds = Math.floor(ms / 1000);
            time_spec[0] = BigInt(seconds);
            time_spec[1] = BigInt(Math.floor((ms - seconds*1000) * 1000000));
        },
        abort: () => {
            throw new Error("called abort");
        },
        exit: (status) => {
            if (status == 0) throw "called exit 0";
            cancelAnimationFrame(G.animation_id);
            throw new Error("called exit " + status);
        },
        mmap: (addr, len_, prot, flags, fd, offset) => {
            const start = FrancaXXX.__heap_base + bump;

            const len = ((len_ + page_size - 1n) / page_size) * page_size;
            bump += len;
            if (start + len >= FrancaXXX.__heap_end) {
                const pages = len / page_size + 1n;
                Franca.memory.grow(Number(pages));
                FrancaXXX.__heap_end += pages * page_size;
            }
            return start;
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
        munmap: (addr, len) => 0n,
        mprotect: (a, b, c) => {
            return 0n;
        },
        fsync: (fd) => 0n,
        fetch_file: (ptr, len, p_file_length) => {
            let path = get_wasm_string(ptr, len);
            if (path.startsWith("./")) path = path.slice(2);
            const offset_length = fs_index[path];
            if (offset_length == undefined) return 0n;
            // note: not shadowing the paremeter called 'len' or the worker silently doesn't run and there's no error in the console. 
            const [off, lenXXX] = offset_length;
            let src = new Uint8Array(fs_bytes, off, lenXXX);
            const p = imports.env.mmap(0, BigInt(src.byteLength), 0, 0, 0, 0);
            let dest = new Uint8Array(Franca.memory.buffer, Number(p), src.byteLength);
            dest.set(src);
            
            const file_length = new BigInt64Array(
                Franca.memory.buffer,
                Number(p_file_length),
                1,
            );
            file_length[0] = BigInt(src.byteLength);
            
            return p;
        },
        yield_file: (ptr, len) => {
            const bytes = new Uint8Array(
                Franca.memory.buffer,
                Number(ptr),
                Number(len),
            );
            self.postMessage({
                tag: "download",
                content: new Blob([bytes], { type: "octet/stream" }),
                name: "a.out",
            });
        },
        null: () => {
            throw new Error("wasm guest tried to call a null function pointer");
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
    "remove",
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
    "readlink",
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
            fs_index = msg.fs_index;
            fs_bytes = msg.fs_bytes;
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
    return new TextDecoder().decode(buffer);
}

self.onmessage = handle;
