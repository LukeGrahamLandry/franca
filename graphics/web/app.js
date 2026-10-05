export const add_events = (canvas, send) => {
    const I = 0n;  // this is a placeholder for the (web: *Impl) which is patched by send. 
    const removers = [];
    const E = (element, name, f) => {
        element.addEventListener(name, f);
        removers.push([element, name, f]);
    }
    const event = (name, f) => E(canvas, name, f);
    event("contextmenu", (e) => { 
        e.preventDefault();
    });
    
    const button = (e) => { const b = [0, 2, 1][e.button]; return b === undefined ? 255 : b; };
    event("mousedown", (e) => {
        send("mouse_event", I, 4, button(e), e.offsetX, e.offsetY, 0.0, 0.0);
    });
    event("mouseup", (e) => {
        send("mouse_event", I, 5, button(e), e.offsetX, e.offsetY, 0.0, 0.0);
    });
    event("mousemove", (e) => {
       send("mouse_event", I, 7, button(e), e.offsetX, e.offsetY, e.movementX, e.movementY);
    });
    // TODO: you're not supposed to use keyCode anymore. 
    //      for now i just want to get something on the screen. 
    event("keydown", (e) => {
        send("key_event", I, 1, e.keyCode, e.altKey, e.ctrlKey, e.metaKey, e.shiftKey, e.repeat);
        // e.preventDefault(); TODO: i want this for tab but not for cmd+r and i still want to get keypress
    });
    event("keyup", (e) => {
        send("key_event", I, 2, e.keyCode, e.altKey, e.ctrlKey, e.metaKey, e.shiftKey, e.repeat);
    });
    event("keypress", (e) => {
        if (e.key == "Enter") {
            send("key_event", I, 3, 13, e.altKey, e.ctrlKey, e.metaKey, e.shiftKey, e.repeat);
            return;
        }
        for (let c of e.key) {
            send("key_event", I, 3, c.codePointAt(0), e.altKey, e.ctrlKey, e.metaKey, e.shiftKey, e.repeat);
        }
    });
    E(window, "resize", () => {
        let [w, h] = [canvas.clientWidth, canvas.clientHeight];
        send("resize_event", I, 10, w, h, window.devicePixelRatio);
    });
    event("wheel", (e) => {
        send("scroll_event", I, e.deltaX, e.deltaY);
    });
    // TODO: this works in chrome but firefox won't send paste to a canvas? 
    //       doing `E(window, "paste",` works but then you get all not just when the canvas is selected. 
    event("paste", (e) => {
        e.preventDefault();
        let s = (e.clipboardData || window.clipboardData).getData("text");
        s = new TextEncoder().encode(s);
        send("paste_event", I, s.byteLength, s, 0);
    });
    
    event("dragover", (e) => {
        e.preventDefault();
        e.dataTransfer.dropEffect = "copy";
    });
    event("drop", async (e) => {
        let files = [...e.dataTransfer.items].filter((it) => it.kind === "file");
        if (files.length == 0) return; 
        e.preventDefault();
        files = await Promise.all(files.map(async (it) => {
            it = it.getAsFile();
            return [new TextEncoder().encode(it.name), await it.arrayBuffer()];
        }));
        // [(name_len: i64, name: blob, data_len: i64, data: blob)]
        let size = files.reduce((size, it) => size + 8 + it[0].byteLength + 8 + it[1].byteLength, 0);
        let buf = new ArrayBuffer(size);
        let b = new DataView(buf);
        let off = 0;
        for (const [name, contents] of files) {
            b.setBigInt64(off, BigInt(name.byteLength), true); 
            off += 8;
            new Uint8Array(buf, off, name.byteLength).set(new Uint8Array(name));
            off += name.byteLength;
            b.setBigInt64(off, BigInt(contents.byteLength), true); 
            off += 8;
            new Uint8Array(buf, off, contents.byteLength).set(new Uint8Array(contents));
            off += contents.byteLength;
        }
        send("drop_files_event", I, buf.byteLength, new Uint8Array(buf), files.length);
    });
    
    event("mouseenter", (e) => send("simple_event", I, 8));
    event("mouseleave", (e) => send("simple_event", I, 9));
    document.addEventListener("visibilitychange", () => {
        // TODO: these both fire when going fullscreen which isn't really what i want but not worth hacking around
        if (document.hidden) send("simple_event", I, 11);  // ICONIFIED
        else                 send("simple_event", I, 12);  // RESTORED
    });
    event("focus", (e) => send("simple_event", I, 13));
    event("focusout", (e) => send("simple_event", I, 14));
    // TODO: QUIT_REQUESTED
    
    const handle_app_request = (data) => {
        const msg = data[1];
        switch (data[0]) {
            case "set_clipboard_string": {
                /*await*/ navigator.clipboard.write([new ClipboardItem({ "text/plain": data[1] })]);
                break
            }
            case "get_clipboard_string": {
                navigator.clipboard.readText().then((s) => {
                    s = new TextEncoder().encode(s);
                    send("paste_event", I, s.byteLength, s, 1);
                });
            }
            case "set_local_storage": {
                try { localStorage.setItem(msg.key, msg.value); } catch (e) { console.error(e); }
                break
            }
            case "remove_local_storage": {
                try { localStorage.removeItem(msg.key); } catch (e) { console.error(e); }
                break
            }
            case "get_local_storage": {
                const buf = new Int32Array(msg.memory.buffer, Number(msg.futex_ptr), 1);
                if (buf[0] != -2) return;
                try { 
                    const value_s = localStorage.getItem(msg.key);
                    if (value_s === null) {
                        buf[0] = -1;
                    } else {
                        const value = new TextEncoder().encode(value_s);
                        buf[0] = value.byteLength;
                        if (BigInt(value.byteLength) <= msg.value_len) {
                            const dest = new Uint8Array(msg.memory.buffer, Number(msg.value_ptr), value.byteLength);
                            dest.set(new Uint8Array(value.buffer));
                        }
                    }
                } catch (e) {
                    buf[0] = -1;
                };
                Atomics.notify(buf, 0);
                break
            }
            default: console.error("bad handle_app_request", data);
        }
    };
    
    return [removers, handle_app_request];
}
