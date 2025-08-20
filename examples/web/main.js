if (typeof(WebAssembly) === "undefined") alert("Your browser does not support web assembly.");
if (typeof(Worker) === "undefined") alert("Your browser does not support web workers.");

const handle = (_msg) => {
    const msg = _msg.data;
    switch(msg.tag) {
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
        default: throw msg;
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
        const url = document.getElementById("compiler").value;
        document.getElementById("err").innerText = "";
        document.getElementById("time").innerText = "(" + url + ")";
        document.getElementById("out").value = "";
        document.getElementById("stale").hidden = true;
        const args = ["---literal_input", document.getElementById("in").value];
        
        worker = new Worker("worker.js");
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

let kalidescope = await (await fetch("target/mandel.txt")).text();
document.getElementById("in").value = kalidescope;

document.getElementById("in").oninput = () => {
    document.getElementById("stale").hidden = false;
};
document.getElementById("btn").onclick = toggle_worker;
toggle_worker();

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
