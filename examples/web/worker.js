const page_size = 1n << 16n;

let bump = 0n;
let Franca;
let FrancaXXX = {};
let start = performance.now();

function handleWasmLoaded(wasm, args) {
    let load_end = performance.now();
    show_log(" Loaded in " + Math.round(load_end - start) + "ms.");
    Franca = wasm.instance.exports;
    console.log(Franca);

    // TODO: something more official than this.
    //       might be better to have page_allocator do something different instead?
    FrancaXXX.__heap_base = BigInt(Franca.memory.buffer.byteLength);
    FrancaXXX.__heap_end = FrancaXXX.__heap_base;
    
    let p = write_args(args);
    try {
        Franca.main(BigInt(args.length), p, 0n, 0n);
    } catch (e) {
        if (e !== "called exit 0") {
            show_error(e);
        }
    }
    show("");
    let run_end = performance.now();
    show_log(" Ran in " + Math.round(run_end - load_end) + "ms.");
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

const imports = {
    env: {
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
            time_spec[0] = BigInt(Math.floor(ms / 1000));
            time_spec[1] = BigInt(Math.floor(ms * 1000000));
        },
        abort: () => {
            throw new Error("called abort");
        },
        exit: (status) => {
            if (status == 0) throw "called exit 0";
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
            console.log(instance);

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
        fetch_file: (ptr, len) => {
            const path = get_wasm_string(ptr, len);
            const text = sync_fetch("target/" + path);
            if (text === null) return 0n;
            let src = new TextEncoder().encode(text)
            const p = imports.env.mmap(0, BigInt(src.byteLength + 1), 0, 0, 0, 0);
            let dest = new Uint8Array(Franca.memory.buffer, Number(p), src.byteLength);
            dest.set(new Uint8Array(src.buffer));
            return p;
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
    "strtoul", "strtod", "localtime", "time",
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
            WebAssembly.instantiateStreaming(fetch("target/" + msg.url), imports)
                .then(it => handleWasmLoaded(it, msg.args))
                .catch(show_error);
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

const sync_fetch = (url) => {
    let it = new XMLHttpRequest();
    it.open("GET", url, false);
    it.send(null);
    if (it.status !== 200) return null;
    return it.responseText;
}

self.onmessage = handle;
