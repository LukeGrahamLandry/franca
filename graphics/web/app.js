// THIS IS UNFINISHED


export const add_events = (I, canvas, send) => {
    const removers = [];
    const E = (element, name, f) => {
        element.addEventListener(name, f);
        removers.push([element, name, f]);
    }
    const event = (name, f) => E(canvas, name, f);
    event("contextmenu", (e) => { 
        e.preventDefault();
    });
    
    event("mousedown", (e) => {
        send("mouse_event", I, 4, e.button, e.offsetX, e.offsetY, 0.0, 0.0);
    });
    event("mouseup", (e) => {
        send("mouse_event", I, 5, e.button, e.offsetX, e.offsetY, 0.0, 0.0);
    });
    event("mousemove", (e) => {
       send("mouse_event", I, 7, e.button, e.offsetX, e.offsetY, e.movementX, e.movementY);
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
        for (let c of e.key) {
            send("key_event", I, 3, c.codePointAt(0), e.altKey, e.ctrlKey, e.metaKey, e.shiftKey, e.repeat);
        }
    });
    E(window, "resize", () => {
        let [w, h] = [canvas.clientWidth, canvas.clientHeight];
        send("resize_event", I, 10, w, h, w, h, window.devicePixelRatio);
    });
    event("wheel", (e) => {
        send("scroll_event", I, e.deltaX, e.deltaY);
    });
    
    // TODO
    // MOUSE_ENTER, MOUSE_LEAVE,
    // ICONIFIED, RESTORED, FOCUSED, UNFOCUSED, QUIT_REQUESTED,
    // CLIPBOARD_PASTED, FILES_DROPPED
    
    return removers;
}

export const set_icon = () => {
    
}

export const set_window_title = (cstr) => {
    
}

export const get_clipboard_string = () => {
    
}


export const set_clipboard_string = (cstr) => {
    
}

export const lock_mouse = (lock) => {
    
}

export const set_mouse_cursor = (cursor) => {
    
}

export const toggle_fullscreen = () => {
    
}

export const update_cursor = (cursor, shown) => {
    
}
