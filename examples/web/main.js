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
        default: throw msg;
    }
};

let worker = new Worker("worker.js");
worker.onmessage = handle;
worker.postMessage({ tag: "start" });

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
