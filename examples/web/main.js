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
            document.getElementById("btn").innerText = "Restart Process";
            running = false;
            break;
        }
        default: throw msg;
    }
};

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
        
        worker = new Worker("worker.js");
        worker.onmessage = handle;
        worker.postMessage({ tag: "start", url: url });
        document.getElementById("btn").innerText = "Kill Process";
    } else {
        worker.terminate();
        worker = null;
        document.getElementById("btn").innerText = "Restart Process";
    }
};
toggle_worker();

let line = "";
const show = (s) => {
    if (s.length == 1) {
        line += s;
        if (s.charCodeAt(0) != 10) return;
        s = line;
        line = "";
    }
    document.getElementById("out").value += s;
    s = "";
};
