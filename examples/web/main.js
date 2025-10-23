let manifest_version = Math.floor(new Date().valueOf());
if (typeof WebAssembly === "undefined")
    alert("Your browser does not support web assembly.");
if (typeof Worker === "undefined")
    alert("Your browser does not support web workers.");

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
            document.getElementById("btn").innerText = start_message;
            running = false;
            let end = performance.now();
            document.getElementById("time").innerText += " Wall: " + Math.round(end - real_start_time) + "ms.";
            flush();
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
        default:
            throw msg;
    }
};


import { add_events } from "./target/app.js";

let real_start_time;
const start_message = "Run";
let worker = null;
let running = false;
const toggle_worker = () => {
    if (!running && worker !== null) {
        worker.terminate();
        worker = null;
    }
    running = worker === null;
    if (running) {
        let compiler_i = document.getElementById("compiler").value;
        const compiler = manifest.compilers[parseInt(compiler_i)];
        let url = "demo.wasm"; 
        document.getElementById("err").innerText = "";
        document.getElementById("time").innerText = "(" + url + ")";
        document.getElementById("out").value = "";
        document.getElementById("stale").hidden = true;
        const input = document.getElementById("in").value;
        const dbg = document.getElementById("dbg").value;
        const args = [
            url,
            "---literal_input",
            input,
            "-lang",
            compiler.name,
            "-target",
            document.getElementById("target").value,
            ...(dbg.length == 0 ? [] : ["-d", dbg]),
        ];
        worker = new Worker(`worker.js?v=${version}`, { type: "module" });
        worker.onmessage = handle;
        
        // need to do this extra dance because you can't un-transferControlToOffscreen, 
        // and when you run a new program i give you a new worker. 
        let old_canvas = document.getElementById("canvas");
        let canvas = old_canvas.cloneNode();
        old_canvas.replaceWith(canvas);
        let surface = canvas.transferControlToOffscreen();
        
        const send = (handler, ...args) => 
            worker.postMessage({ tag: "event", handler, args });
        add_events(0, canvas, send);
        
        // TODO: i don't understand why you have to do this but it makes it not suck ass.
        //       (its not the same as multiplying framebuffer_(w/h)
        //        on the franca side because we don't pick the size of getcurrenttexture)
        surface.width = canvas.clientWidth * window.devicePixelRatio;
        surface.height = canvas.clientHeight * window.devicePixelRatio;
        
        real_start_time = performance.now();
        worker.postMessage({ tag: "start", url: url, args: args, version: version, fs_bytes: fs_bytes, fs_index: manifest.filesystem, canvas: surface }, [surface]);
        document.getElementById("btn").innerText = "Kill";
    } else {
        worker.terminate();
        show("");
        document.getElementById("err").innerText += "KILLED";
        worker = null;
        document.getElementById("btn").innerText = start_message;
    }
};

let manifest = await (await fetch("target/manifest.json?v=" + manifest_version)).json();
const version = manifest.commit.slice(0, 8);
console.log(manifest);
document.getElementById("version").innerText = manifest.commit;
let fs_bytes = await (await fetch(`target/${manifest.commit}.txt`)).arrayBuffer();

const load_example = async (path) => {
    let it = manifest.filesystem[path];
    if (it === undefined) alert(path + " not found");
    let [off, len] = it;
    let src = new TextDecoder().decode(new Uint8Array(fs_bytes, off, len));
    document.getElementById("stale").hidden = false;
    document.getElementById("in").value = src;
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
    show_examples(parseInt(this.value));
};
document.getElementById("example").onchange = function () {
    load_example(this.value);
};
show_compilers();
await load_example(manifest.compilers[0].examples[0]);

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
    document.getElementById("out").value =
        `Passed ${passed}/${all} tests.\n\n${results}`;
    document.getElementById("in").value = saved;
    document.getElementById("stale").hidden = false;
};

let line = "";
const show = (s) => {
    line += s;
};

const flush = () => {
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
        // c.width = c.clientWidth * window.devicePixelRatio;
        // c.height = c.clientHeight * window.devicePixelRatio;
        console.log(c);
    } else {
        out.style.height = "100%";
    }
}
