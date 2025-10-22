// THIS IS UNFINISHED

export const js_init = (I, wasm, canvas) => {
    const F = wasm.instance.exports;
    const event = (name, f) => canvas.addEventListener(name, f);
    
    event("mousedown", (e) => {
        F.mouse_event(I, 4, e.button, e.offsetX, e.offsetY, 0.0, 0.0);
    });
    event("mouseup", (e) => {
        F.mouse_event(I, 5, e.button, e.offsetX, e.offsetY, 0.0, 0.0);
    });
    event("mousemove", (e) => {
        F.mouse_event(I, 7, e.button, e.offsetX, e.offsetY, e.movementX, e.movementY);
    });
    // TODO: you're not supposed to use keyCode anymore. 
    //      for now i just want to get something on the screen. 
    event("keydown", (e) => {
        F.key_event(I, 1, e.keyCode, e.altKey, e.ctrlKey, e.metaKey, e.shiftKey, e.repeat);
    });
    event("keyup", (e) => {
        F.key_event(I, 2, e.keyCode, e.altKey, e.ctrlKey, e.metaKey, e.shiftKey, e.repeat);
    });
    event("keypress", (e) => {
        for (let c of e.key) {
            F.key_event(I, 3, c.codePointAt(0), e.altKey, e.ctrlKey, e.metaKey, e.shiftKey, e.repeat);
        }
    });
    
    // TODO
    // MOUSE_SCROLL,
    // MOUSE_ENTER, MOUSE_LEAVE, RESIZED,
    // ICONIFIED, RESTORED, FOCUSED, UNFOCUSED, QUIT_REQUESTED,
    // CLIPBOARD_PASTED, FILES_DROPPED
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
