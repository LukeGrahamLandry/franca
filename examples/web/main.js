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
            break;
        }
        default:
            throw msg;
    }
};

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
        let compiler = document.getElementById("compiler").value;
        const index = parseInt(compiler);
        let url = manifest.compilers[index].url;
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
            ...(dbg.length == 0 ? [] : ["-d", dbg]),
        ];
        worker = new Worker(manifest.worker);
        worker.onmessage = handle;
        worker.postMessage({ tag: "start", url: url, args: args });
        document.getElementById("btn").innerText = "Kill";
    } else {
        worker.terminate();
        show("");
        document.getElementById("err").innerText += "KILLED";
        worker = null;
        document.getElementById("btn").innerText = start_message;
    }
};

let manifest = await (await fetch("target/manifest.json")).json();
console.log(manifest);

const load_example = async (path) => {
    let src = await (await fetch("target/" + path)).text();
    document.getElementById("stale").hidden = false;
    document.getElementById("in").value = src;
};
const show_examples = (lang_i) => {
    let src = `<option selected disabled> Load Example </option>`;
    for (let it of manifest.compilers[lang_i].examples) {
        src += `<option value="${it}"> ${it} </option>`;
    }
    const it = document.getElementById("example");
    it.innerHTML = src;
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
toggle_worker();

document.getElementById("all").onclick = async () => {
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
            await wait(() => !running, 25, 1000);
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
    if (s.length == 1) {
        line += s;
        if (s.charCodeAt(0) != 10 && line.length < 128) return;
        s = line;
        line = "";
    }
    document.getElementById("out").value += s;
    s = "";
};

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
