let manifest_version = Math.floor(new Date().valueOf());
if (typeof WebAssembly === "undefined")
    alert("Your browser does not support web assembly.");
if (typeof Worker === "undefined")
    alert("Your browser does not support web workers.");

let thread_pool = []
let running_threads = []
const handle = (_msg) => {
    const msg = _msg.data;
    switch (msg.tag) {
        case "show": {
            show(msg.text);
            break;
        }
        case "log": {
            document.getElementById("time").innerText += msg.text;
            break;
        }
        case "err": {
            document.getElementById("err").innerText += msg.text;
            break;
        }
        case "done": {
            // don't terminate the worker here because that kills the nice objects in the console
            // also, if it's a graphics program, its still controlling the canvas. 
            document.getElementById("btn").innerText = start_message;
            running = false;
            let end = performance.now();
            document.getElementById("time").innerText += " Wall: " + Math.round(end - real_start_time) + "ms.";
            flush();

            // calling terminate has a viral slowness where it just makes random later operations slow. 
            // might as well reuse the workers anyway. 
            for (let w of thread_pool) {
                w.postMessage({ tag: "reset" });
            }
            break;
        }
        case "download": {
            let a = document.createElement("a");
            let url = window.URL.createObjectURL(msg.content);
            a.href = url;
            a.download = "a.out";
            a.click();
            window.URL.revokeObjectURL(url);
            break;
        }
        case "ready": break
        // dear google claims you can spawn a worker from a worker but it sure doesn't work so here we are ??
        case "spawn": {
            let w = get_worker(); 
            w.onmessage = (msg) => {
                if (msg.data.tag == "recycle") {
                    running_threads.splice(running_threads.indexOf(w));
                    // don't call reset here. the point is to avoid replaying all the jit events if they're in the same program. 
                    // just reset in toggle_worker when the main thread exits and you run a new thing.  
                    thread_pool.push(w);
                    return;
                }
                handle(msg);
            };
            running_threads.push(w);
            w.postMessage({ tag: "start", args: [], child: msg.child, memory: msg.memory });
            break
        }
        default:
            throw msg;
    }
};

function get_worker() {
    if (thread_pool.length == 0) {
        let w = new Worker(`worker.js?v=${version}`, { type: "module" });
        w.onerror = (e) => {
            console.error(e);
            show(e.message);
            flush();
        };
        w.onmessage = (e) => console.error(e);
        if (wasm_module === undefined) throw "get_worker before load_wasm";
        w.postMessage({ tag: "setmodule", module: wasm_module });
        thread_pool.push(w);
    };
    return thread_pool.pop();
}


import { add_events } from "./target/app.js";

let real_start_time;
const start_message = "Run";
let worker = null;
let running = false;
let removers = [];
const toggle_worker = async () => {
    if (!running && worker !== null) {  // means it sent "done" so not stuck
        thread_pool.push(worker);
        worker = null;
    }
    for (let w of running_threads) {  // any children that were still running might be stuck
        w.terminate();
    }
    running_threads = []
    for (let w of thread_pool) {  // children that already returned from wasm_init_thread are not stuck
        w.postMessage({ tag: "reset" });
    }
    for (let a of removers) {
        for (let b of a) {
            b[0].removeEventListener(b[1], b[2]);
        }
    }
    running = worker === null;
    if (running) {
        real_start_time = performance.now();
        
        let compiler_i = document.getElementById("compiler").value;
        const compiler = manifest.compilers[parseInt(compiler_i)];
        document.getElementById("err").innerText = "";
        document.getElementById("time").innerText = ".";
        document.getElementById("out").value = "";
        document.getElementById("stale").hidden = true;
        const input = document.getElementById("in").value;
        const dbg = document.getElementById("dbg").value;
        
        {
            const url = new URL(window.location);
            url.searchParams.set("dbg", dbg);
            if (dbg === "") url.searchParams.delete("dbg");
            history.replaceState(null, '', url);
        }
        
        const args = [
            "demo.wasm",
            "---literal_input",
            input,
            "-lang",
            compiler.name,
            "-target",
            document.getElementById("target").value,
            ...(dbg.length == 0 ? [] : (dbg.includes("-") ? dbg.split(" ") : ["-d", dbg])),
        ];
        worker = get_worker();
        worker.onmessage = handle;
        let need_canvas = input.includes("graphics/lib.fr");  // :HackyGraphicsDetection
        
        // need to do this extra dance because you can't un-transferControlToOffscreen, 
        // and when you run a new program i give you a new worker. 
        let old_canvas = document.getElementById("canvas");
        let canvas = old_canvas.cloneNode();
        old_canvas.replaceWith(canvas);
        
        if (need_canvas && navigator.gpu === undefined) {
            document.getElementById("out").value = "Your browser doesn't support WebGPU.\nTry one of the non-graphical examples.\n\n";
            enable_graphics(false);
            document.getElementById("err").innerText = "Your browser doesn't support WebGPU.";
            worker.terminate();
            worker = null;
            document.getElementById("btn").innerText = start_message;
            return;
        }
        
        enable_graphics(need_canvas);
        
        let surface = canvas.transferControlToOffscreen();
        const send = (handler, ...args) => 
            worker.postMessage({ tag: "event", handler, args });
        if (need_canvas) removers.push(add_events(0, canvas, send));
        
        // TODO: i don't understand why you have to do this but it makes it not suck ass.
        //       (its not the same as multiplying framebuffer_(w/h)
        //        on the franca side because we don't pick the size of getcurrenttexture)
        surface.width = canvas.clientWidth * window.devicePixelRatio;
        surface.height = canvas.clientHeight * window.devicePixelRatio;
        if (!need_canvas) surface = undefined;
        let transfer = need_canvas ? [surface] : [];
        worker.postMessage({ tag: "start", args: args, canvas: surface, memory: new_memory(), devicePixelRatio: window.devicePixelRatio }, transfer);
        document.getElementById("btn").innerText = "Kill";
    } else {
        // don't just reset because if they pressed kill maybe its because it hung
        worker.terminate();
        worker = null;
        show("");
        document.getElementById("err").innerText += "KILLED";
        document.getElementById("btn").innerText = start_message;
    }
};

// this is a super slow way to do this, 
// eventually i'll have the worker be persistant and everything just live in one canvas 
async function get_file(path) {
    return new Promise((resolve) => {
        const args = ["demo.wasm", "-yield", path];
        let w = get_worker(); 
        w.onmessage = async (msg) => {
            if (msg.data.tag != "download") {
                console.log(msg.data);
                if (msg.data.tag === "err" || msg.data.tag === "show") handle(msg); 
                flush();
            } else {
                w.postMessage({ tag: "reset" });
                thread_pool.push(w);
                document.getElementById("time").innerText = ".";
                resolve(await msg.data.content.text());
            }
        };
        w.postMessage({ tag: "start",args: args, memory: new_memory() }, []);
    });
}

function new_memory() {
    return new WebAssembly.Memory({
        initial: 300,  // write_args grows it if needed
        maximum: 1 << (32 - 16),
        // TODO: if the module wants unshared, catch the exception and change this to a normal memory. this is stupid. 
        shared: true,
    }); 
}

let manifest = await (await fetch("target/manifest.json?v=" + manifest_version)).json();
const version = manifest.commit.slice(0, 8);
console.log(manifest);
document.getElementById("version").innerText = manifest.commit;

const load_example = async (path) => {
    document.getElementById("err").innerText = "SLOW\n";
    try {
        document.getElementById("in").value = await get_file(path);
    } catch (e) {
        document.getElementById("out").value += `${s.toString()}\n\n${s.stack}`;
    }
    document.getElementById("err").innerText = "";
    document.getElementById("stale").hidden = false;
};
const show_examples = (lang_i) => {
    let src = `<option selected disabled> Load Example </option>`;
    let c = manifest.compilers[lang_i];
    for (let it of c.examples) {
        src += `<option value="${it}"> ${it} </option>`;
    }
    const it = document.getElementById("example");
    it.innerHTML = src;
    
    document.getElementById("version").innerText = `${manifest.commit} ${c.about}`;
};
const show_compilers = () => {
    let src = "";
    for (const [i, it] of manifest.compilers.entries()) {
        src += `<option value="${i}"> ${it.name} </option>`;
    }
    let it = document.getElementById("compiler");
    it.innerHTML = src;
    it.options.selectedIndex = 0;
    show_examples(0);
};

document.getElementById("in").oninput = () => {
    document.getElementById("stale").hidden = false;
};
document.getElementById("btn").onclick = toggle_worker;

document.getElementById("compiler").onchange = function () {
    document.getElementById("stale").hidden = false;
    const i = parseInt(this.value);
    show_examples(i);
    
    const url = new URL(window.location);
    url.searchParams.delete("file");
    history.replaceState(null, '', url);
};
document.getElementById("example").onchange = function () {
    load_example(this.value);
    
    const url = new URL(window.location);
    url.searchParams.set("file", this.value);
    history.replaceState(null, '', url);
};
show_compilers();

let wasm_module = await load_wasm();
async function load_wasm() {
    let res = await fetch(`target/demo.wasm?v=${version}`);
    if (res.status !== 200) document.getElementById("err").innerText = res.status;
    // TODO: this is the compressed size but progress is uncompressed :(
    const total = res.headers.get("content-length");
    res = res.body.getReader();
    let bytes = [];
    let progress = 0;
    while (true) {
        const { done, value } = await res.read();
        if (done) break;
        progress += value.byteLength;
        document.getElementById("time").innerText = `Loading ${progress}/${total}`; 
        bytes.push(value);
    }
    document.getElementById("time").innerText = "Compiling"; 
    bytes = await (new Blob(bytes)).arrayBuffer();
    let result = await WebAssembly.compile(bytes);
    document.getElementById("time").innerText = "Ready"; 
    return result;
}
let line = "";

const p = new URLSearchParams(window.location.search);
const dbg = p.get("dbg");
if (dbg !== null) document.getElementById("dbg").value = dbg;

const path = p.get("file");
if (path !== null) {
    for (const [i, it] of manifest.compilers.entries()) {
        let idx = it.examples.indexOf(path);
        if (idx != -1) {
            show_examples(i);
            document.getElementById("compiler").options.selectedIndex = i;
            document.getElementById("example").options.selectedIndex = idx+1;
        }
    }
    await load_example(path);
} else {
    await load_example(manifest.compilers[0].examples[navigator.gpu === undefined ? 1 : 0]);
}

{
    let targets = ["wasm-jit", "wasm-aot", "arm64-macos", "amd64-macos", "arm64-linux", "amd64-linux", "riscv64-linux"];
    let src = "";
    for (const it of targets) {
        src += `<option value="${it}"> ${it} </option>`;
    }
    document.getElementById("target").innerHTML = src; 
}

enable_graphics(true);  // TODO: only do this if the program being compiled needs it
toggle_worker();

document.getElementById("all").onclick = async () => {
    let start = performance.now();
    document.getElementById("target").options.selectedIndex = 0;
    let saved = document.getElementById("in").value;
    let err = document.getElementById("err");
    let results = "";
    let all = 0;
    let passed = 0;

    let compiler = document.getElementById("compiler");
    let saved_c = compiler.options.selectedIndex;
    for (const [i, it] of manifest.compilers.entries()) {
        compiler.options.selectedIndex = i;
        let examples = it.examples;
        results += "=== " + it.name + " ===\n";
        for (let it of examples) {
            results += it;
            await load_example(it);
            toggle_worker();
            await wait(() => !running, 25, 10000);
            let ok = err.innerText.length == 0;
            results += " " + (ok ? "ok" : "FAIL") + "\n";
            if (ok) passed += 1;
            err.innerText = "";
            all += 1;
        }
    }
    compiler.options.selectedIndex = saved_c;
    let msg = `Passed ${passed}/${all} tests in ${Math.round(performance.now() - start)}ms.`;
    document.getElementById("out").value =
        `${msg}\n\n${results}`;
    document.getElementById("in").value = saved;
    document.getElementById("stale").hidden = false;
    document.getElementById("time").innerText = msg; 
};

function show(s) {
    line += s;
};

function flush() {
    if (line.length != 0) {
        document.getElementById("out").value += line;
        line = "";
    }
};
window.setInterval(flush, 200);

const wait = (f, step, timeout) => {
    return new Promise((resolve) => {
        var t = Date.now();
        const loop = () => {
            if (f()) {
                resolve();
            } else if (Date.now() > t + timeout) {
                document.getElementById("err").innerText += "timeout";
                resolve();
            } else {
                window.setTimeout(loop, step);
            }
        };
        loop();
    });
};

// who fucking knows man, i don't care. https://stackoverflow.com/questions/6637341/use-tab-to-indent-in-textarea
for (const it of document.getElementsByTagName("textarea")) {
    it.addEventListener('keydown', function (e) {
        const insert = (s) => document.execCommand("insertText", false, s);
        const tab = "    ";
        const start_line = /^/gm;
        if (e.key !== "Tab") return; 
        e.preventDefault();
        const [start, end] = [this.selectionStart, this.selectionEnd];
        if (start === end) {
            insert(tab);
            return;
        }
        const old = this.value.substring(start, end);
        const new_ = old.replace(start_line, tab);
        insert(new_);
        this.selectionStart = start;
        this.selectionEnd = end + (new_.length - old.length);
    });
}

document.getElementById("wisdom").innerText = manifest.wisdom[Math.floor(Math.random() * manifest.wisdom.length)];

document.getElementById("togglecanvas").addEventListener("click", (e) => enable_graphics(e.target.checked));
function enable_graphics(on) {
    document.getElementById("togglecanvas").checked = on;
    let out = document.getElementById("out");
    let c = document.getElementById("canvas");
    c.hidden = !on;
    if (on) {
        out.style.height = "29%";
        c.style.height = "70%";
        c.style.width = "100%";
        c.width = c.clientWidth * window.devicePixelRatio;
        c.height = c.clientHeight * window.devicePixelRatio;
    } else {
        out.style.height = "100%";
    }
}
