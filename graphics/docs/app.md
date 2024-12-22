sokol_app.h -- cross-platform application wrapper

Project URL: https://github.com/floooh/sokol

Link with the following system libraries:

- on macOS with Metal: Cocoa, QuartzCore, Metal, MetalKit

FEATURE OVERVIEW
================
provides a minimalistic API which implements the 'application-wrapper' parts of a 3D application:

- a common application entry function
- creates a window and 3D-API context/device with a 'default framebuffer'
- makes the rendered frame visible
- provides keyboard-, mouse- and low-level touch-events
- platforms: MacOS
- 3D-APIs: Metal

STEP BY STEP
============
--- Add a sokol_main() function to your code which returns a sapp_desc structure
    with initialization parameters and callback function pointers. This
    function is called very early, usually at the start of the
    platform's entry function (e.g. main or WinMain). You should do as
    little as possible here, since the rest of your code might be called
    from another thread (this depends on the platform):

        sapp_desc sokol_main(int argc, char* argv[]) {
            return (sapp_desc) {
                .width = 640,
                .height = 480,
                .init_cb = my_init_func,
                .frame_cb = my_frame_func,
                .cleanup_cb = my_cleanup_func,
                .event_cb = my_event_func,
                ...
            };
        }

    To get any logging output in case of errors you need to provide a log
    callback. The easiest way is via sokol_log.h:

        #include "sokol_log.h"

        sapp_desc sokol_main(int argc, char* argv[]) {
            return (sapp_desc) {
                ...
                .logger.func = slog_func,
            };
        }

    There are many more setup parameters, but these are the most important.
    For a complete list search for the sapp_desc structure declaration
    below.

    DO NOT call any sokol-app function from inside sokol_main(), since
    sokol-app will not be initialized at this point.

    The .width and .height parameters are the preferred size of the 3D
    rendering canvas. The actual size may differ from this depending on
    platform and other circumstances. Also the canvas size may change at
    any time (for instance when the user resizes the application window,
    or rotates the mobile device). You can just keep .width and .height
    zero-initialized to open a default-sized window (what "default-size"
    exactly means is platform-specific, but usually it's a size that covers
    most of, but not all, of the display).

    All provided function callbacks will be called from the same thread,
    but this may be different from the thread where sokol_main() was called.

    .init_cb (void (*)(void))
        This function is called once after the application window,
        3D rendering context and swap chain have been created. The
        function takes no arguments and has no return value.
    .frame_cb (void (*)(void))
        This is the per-frame callback, which is usually called 60
        times per second. This is where your application would update
        most of its state and perform all rendering.
    .cleanup_cb (void (*)(void))
        The cleanup callback is called once right before the application
        quits.
    .event_cb (void (*)(const sapp_event* event))
        The event callback is mainly for input handling, but is also
        used to communicate other types of events to the application. Keep the
        event_cb struct member zero-initialized if your application doesn't require
        event handling.

    As you can see, those 'standard callbacks' don't have a user_data
    argument, so any data that needs to be preserved between callbacks
    must live in global variables. If keeping state in global variables
    is not an option, there's an alternative set of callbacks with
    an additional user_data pointer argument:

    .user_data (void*)
        The user-data argument for the callbacks below
    .init_userdata_cb (void (*)(void* user_data))
    .frame_userdata_cb (void (*)(void* user_data))
    .cleanup_userdata_cb (void (*)(void* user_data))
    .event_userdata_cb (void(*)(const sapp_event* event, void* user_data))

    The function sapp_userdata() can be used to query the user_data
    pointer provided in the sapp_desc struct.

    You can also call sapp_query_desc() to get a copy of the
    original sapp_desc structure.

    NOTE that there's also an alternative compile mode where sokol_app.h
    doesn't "hijack" the main() function. Search below for SOKOL_NO_ENTRY.

--- Implement the initialization callback function (init_cb), this is called
    once after the rendering surface, 3D API and swap chain have been
    initialized by sokol_app. All sokol-app functions can be called
    from inside the initialization callback, the most useful functions
    at this point are:

    int sapp_width(void)
    int sapp_height(void)
        Returns the current width and height of the default framebuffer in pixels,
        this may change from one frame to the next, and it may be different
        from the initial size provided in the sapp_desc struct.

    float sapp_widthf(void)
    float sapp_heightf(void)
        These are alternatives to sapp_width() and sapp_height() which return
        the default framebuffer size as float values instead of integer. This
        may help to prevent casting back and forth between int and float
        in more strongly typed languages than C and C++.

    double sapp_frame_duration(void)
        Returns the frame duration in seconds averaged over a number of
        frames to smooth out any jittering spikes.

    int sapp_color_format(void)
    int sapp_depth_format(void)
        The color and depth-stencil pixelformats of the default framebuffer,
        as integer values which are compatible with sokol-gfx's
        sg_pixel_format enum (so that they can be plugged directly in places
        where sg_pixel_format is expected). Possible values are:

            23 == SG_PIXELFORMAT_RGBA8
            28 == SG_PIXELFORMAT_BGRA8
            42 == SG_PIXELFORMAT_DEPTH
            43 == SG_PIXELFORMAT_DEPTH_STENCIL

    int sapp_sample_count(void)
        Return the MSAA sample count of the default framebuffer.

    const void* sapp_metal_get_device(void)
    const void* sapp_metal_get_current_drawable(void)
    const void* sapp_metal_get_depth_stencil_texture(void)
    const void* sapp_metal_get_msaa_color_texture(void)
        If the Metal backend has been selected, these functions return pointers
        to various Metal API objects required for rendering, otherwise
        they return a null pointer. These void pointers are actually
        Objective-C ids converted with a (ARC) __bridge cast so that
        the ids can be tunneled through C code. Also note that the returned
        pointers may change from one frame to the next, only the Metal device
        object is guaranteed to stay the same.

    const void* sapp_macos_get_window(void)
        On macOS, get the NSWindow object pointer, otherwise a null pointer.
        Before being used as Objective-C object, the void* must be converted
        back with a (ARC) __bridge cast.

    const void* sapp_ios_get_window(void)
        On iOS, get the UIWindow object pointer, otherwise a null pointer.
        Before being used as Objective-C object, the void* must be converted
        back with a (ARC) __bridge cast.

    const void* sapp_d3d11_get_device(void)
    const void* sapp_d3d11_get_device_context(void)
    const void* sapp_d3d11_get_render_view(void)
    const void* sapp_d3d11_get_resolve_view(void);
    const void* sapp_d3d11_get_depth_stencil_view(void)
        Similar to the sapp_metal_* functions, the sapp_d3d11_* functions
        return pointers to D3D11 API objects required for rendering,
        only if the D3D11 backend has been selected. Otherwise they
        return a null pointer. Note that the returned pointers to the
        render-target-view and depth-stencil-view may change from one
        frame to the next!

    const void* sapp_win32_get_hwnd(void)
        On Windows, get the window's HWND, otherwise a null pointer. The
        HWND has been cast to a void pointer in order to be tunneled
        through code which doesn't include Windows.h.

    const void* sapp_wgpu_get_device(void)
    const void* sapp_wgpu_get_render_view(void)
    const void* sapp_wgpu_get_resolve_view(void)
    const void* sapp_wgpu_get_depth_stencil_view(void)
        These are the WebGPU-specific functions to get the WebGPU
        objects and values required for rendering. If sokol_app.h
        is not compiled with SOKOL_WGPU, these functions return null.

    uint32_t sapp_gl_get_framebuffer(void)
        This returns the 'default framebuffer' of the GL context.
        Typically this will be zero.

    int sapp_gl_get_major_version(void)
    int sapp_gl_get_minor_version(void)
        Returns the major and minor version of the GL context
        (only for SOKOL_GLCORE, all other backends return zero here, including SOKOL_GLES3)

    const void* sapp_android_get_native_activity(void);
        On Android, get the native activity ANativeActivity pointer, otherwise
        a null pointer.

--- Implement the frame-callback function, this function will be called
    on the same thread as the init callback, but might be on a different
    thread than the sokol_main() function. Note that the size of
    the rendering framebuffer might have changed since the frame callback
    was called last. Call the functions sapp_width() and sapp_height()
    each frame to get the current size.

--- Optionally implement the event-callback to handle input events.
    sokol-app provides the following type of input events:
        - a 'virtual key' was pressed down or released
        - a single text character was entered (provided as UTF-32 encoded
          UNICODE code point)
        - a mouse button was pressed down or released (left, right, middle)
        - mouse-wheel or 2D scrolling events
        - the mouse was moved
        - the mouse has entered or left the application window boundaries
        - low-level, portable multi-touch events (began, moved, ended, cancelled)
        - the application window was resized, iconified or restored
        - the application was suspended or restored (on mobile platforms)
        - the user or application code has asked to quit the application
        - a string was pasted to the system clipboard
        - one or more files have been dropped onto the application window

    To explicitly 'consume' an event and prevent that the event is
    forwarded for further handling to the operating system, call
    sapp_consume_event() from inside the event handler (NOTE that
    this behaviour is currently only implemented for some HTML5
    events, support for other platforms and event types will
    be added as needed, please open a GitHub ticket and/or provide
    a PR if needed).

    NOTE: Do *not* call any 3D API rendering functions in the event
    callback function, since the 3D API context may not be active when the
    event callback is called (it may work on some platforms and 3D APIs,
    but not others, and the exact behaviour may change between
    sokol-app versions).

--- Implement the cleanup-callback function, this is called once
    after the user quits the application (see the section
    "APPLICATION QUIT" for detailed information on quitting
    behaviour, and how to intercept a pending quit - for instance to show a
    "Really Quit?" dialog box). Note that the cleanup-callback isn't
    guaranteed to be called on the web and mobile platforms.

MOUSE CURSOR TYPE AND VISIBILITY
================================
You can show and hide the mouse cursor with

    void sapp_show_mouse(bool show)

And to get the current shown status:

    bool sapp_mouse_shown(void)

NOTE that hiding the mouse cursor is different and independent from
the MOUSE/POINTER LOCK feature which will also hide the mouse pointer when
active (MOUSE LOCK is described below).

To change the mouse cursor to one of several predefined types, call
the function:

    void sapp_set_mouse_cursor(sapp_mouse_cursor cursor)

Setting the default mouse cursor SAPP_MOUSECURSOR_DEFAULT will restore
the standard look.

To get the currently active mouse cursor type, call:

    sapp_mouse_cursor sapp_get_mouse_cursor(void)

MOUSE LOCK (AKA POINTER LOCK, AKA MOUSE CAPTURE)
================================================
In normal mouse mode, no mouse movement events are reported when the
mouse leaves the windows client area or hits the screen border (whether
it's one or the other depends on the platform), and the mouse move events
(SAPP_EVENTTYPE_MOUSE_MOVE) contain absolute mouse positions in
framebuffer pixels in the sapp_event items mouse_x and mouse_y, and
relative movement in framebuffer pixels in the sapp_event items mouse_dx
and mouse_dy.

To get continuous mouse movement (also when the mouse leaves the window
client area or hits the screen border), activate mouse-lock mode
by calling:

    sapp_lock_mouse(true)

When mouse lock is activated, the mouse pointer is hidden, the
reported absolute mouse position (sapp_event.mouse_x/y) appears
frozen, and the relative mouse movement in sapp_event.mouse_dx/dy
no longer has a direct relation to framebuffer pixels but instead
uses "raw mouse input" (what "raw mouse input" exactly means also
differs by platform).

To deactivate mouse lock and return to normal mouse mode, call

    sapp_lock_mouse(false)

And finally, to check if mouse lock is currently active, call

    if (sapp_mouse_locked()) { ... }

On native platforms, the sapp_lock_mouse() and sapp_mouse_locked()
functions work as expected (mouse lock is activated or deactivated
immediately when sapp_lock_mouse() is called, and sapp_mouse_locked()
also immediately returns the new state after sapp_lock_mouse()
is called.

On the web platform, sapp_lock_mouse() and sapp_mouse_locked() behave
differently, as dictated by the limitations of the HTML5 Pointer Lock API:

    - sapp_lock_mouse(true) can be called at any time, but it will
      only take effect in a 'short-lived input event handler of a specific
      type', meaning when one of the following events happens:
        - SAPP_EVENTTYPE_MOUSE_DOWN
        - SAPP_EVENTTYPE_MOUSE_UP
        - SAPP_EVENTTYPE_MOUSE_SCROLL
        - SAPP_EVENTTYPE_KEY_UP
        - SAPP_EVENTTYPE_KEY_DOWN
    - The mouse lock/unlock action on the web platform is asynchronous,
      this means that sapp_mouse_locked() won't immediately return
      the new status after calling sapp_lock_mouse(), instead the
      reported status will only change when the pointer lock has actually
      been activated or deactivated in the browser.
    - On the web, mouse lock can be deactivated by the user at any time
      by pressing the Esc key. When this happens, sokol_app.h behaves
      the same as if sapp_lock_mouse(false) is called.

For things like camera manipulation it's most straightforward to lock
and unlock the mouse right from the sokol_app.h event handler, for
instance the following code enters and leaves mouse lock when the
left mouse button is pressed and released, and then uses the relative
movement information to manipulate a camera (taken from the
cgltf-sapp.c sample in the sokol-samples repository
at https://github.com/floooh/sokol-samples):

    static void input(const sapp_event* ev) {
        switch (ev->type) {
            case SAPP_EVENTTYPE_MOUSE_DOWN:
                if (ev->mouse_button == SAPP_MOUSEBUTTON_LEFT) {
                    sapp_lock_mouse(true);
                }
                break;

            case SAPP_EVENTTYPE_MOUSE_UP:
                if (ev->mouse_button == SAPP_MOUSEBUTTON_LEFT) {
                    sapp_lock_mouse(false);
                }
                break;

            case SAPP_EVENTTYPE_MOUSE_MOVE:
                if (sapp_mouse_locked()) {
                    cam_orbit(&state.camera, ev->mouse_dx * 0.25f, ev->mouse_dy * 0.25f);
                }
                break;

            default:
                break;
        }
    }

CLIPBOARD SUPPORT
=================
Applications can send and receive UTF-8 encoded text data from and to the
system clipboard. By default, clipboard support is disabled and
must be enabled at startup via the following sapp_desc struct
members:

    sapp_desc.enable_clipboard  - set to true to enable clipboard support
    sapp_desc.clipboard_size    - size of the internal clipboard buffer in bytes

Enabling the clipboard will dynamically allocate a clipboard buffer
for UTF-8 encoded text data of the requested size in bytes, the default
size is 8 KBytes. Strings that don't fit into the clipboard buffer
(including the terminating zero) will be silently clipped, so it's
important that you provide a big enough clipboard size for your
use case.

To send data to the clipboard, call sapp_set_clipboard_string() with
a pointer to an UTF-8 encoded, null-terminated C-string.

NOTE that on the HTML5 platform, sapp_set_clipboard_string() must be
called from inside a 'short-lived event handler', and there are a few
other HTML5-specific caveats to workaround. You'll basically have to
tinker until it works in all browsers :/ (maybe the situation will
improve when all browsers agree on and implement the new
HTML5 navigator.clipboard API).

To get data from the clipboard, check for the SAPP_EVENTTYPE_CLIPBOARD_PASTED
event in your event handler function, and then call sapp_get_clipboard_string()
to obtain the pasted UTF-8 encoded text.

NOTE that behaviour of sapp_get_clipboard_string() is slightly different
depending on platform:

    - on the HTML5 platform, the internal clipboard buffer will only be updated
      right before the SAPP_EVENTTYPE_CLIPBOARD_PASTED event is sent,
      and sapp_get_clipboard_string() will simply return the current content
      of the clipboard buffer
    - on 'native' platforms, the call to sapp_get_clipboard_string() will
      update the internal clipboard buffer with the most recent data
      from the system clipboard

Portable code should check for the SAPP_EVENTTYPE_CLIPBOARD_PASTED event,
and then call sapp_get_clipboard_string() right in the event handler.

The SAPP_EVENTTYPE_CLIPBOARD_PASTED event will be generated by sokol-app
as follows:

    - on macOS: when the Cmd+V key is pressed down
    - on HTML5: when the browser sends a 'paste' event to the global 'window' object
    - on all other platforms: when the Ctrl+V key is pressed down

DRAG AND DROP SUPPORT
=====================
PLEASE NOTE: the drag'n'drop feature works differently on WASM/HTML5
and on the native desktop platforms (Win32, Linux and macOS) because
of security-related restrictions in the HTML5 drag'n'drop API. The
WASM/HTML5 specifics are described at the end of this documentation
section:

Like clipboard support, drag'n'drop support must be explicitly enabled
at startup in the sapp_desc struct.

    sapp_desc sokol_main(void) {
        return (sapp_desc) {
            .enable_dragndrop = true,   // default is false
            ...
        };
    }

You can also adjust the maximum number of files that are accepted
in a drop operation, and the maximum path length in bytes if needed:

    sapp_desc sokol_main(void) {
        return (sapp_desc) {
            .enable_dragndrop = true,               // default is false
            .max_dropped_files = 8,                 // default is 1
            .max_dropped_file_path_length = 8192,   // in bytes, default is 2048
            ...
        };
    }

When drag'n'drop is enabled, the event callback will be invoked with an
event of type SAPP_EVENTTYPE_FILES_DROPPED whenever the user drops files on
the application window.

After the SAPP_EVENTTYPE_FILES_DROPPED is received, you can query the
number of dropped files, and their absolute paths by calling separate
functions:

    void on_event(const sapp_event* ev) {
        if (ev->type == SAPP_EVENTTYPE_FILES_DROPPED) {

            // the mouse position where the drop happened
            float x = ev->mouse_x;
            float y = ev->mouse_y;

            // get the number of files and their paths like this:
            const int num_dropped_files = sapp_get_num_dropped_files();
            for (int i = 0; i < num_dropped_files; i++) {
                const char* path = sapp_get_dropped_file_path(i);
                ...
            }
        }
    }

The returned file paths are UTF-8 encoded strings.

You can call sapp_get_num_dropped_files() and sapp_get_dropped_file_path()
anywhere, also outside the event handler callback, but be aware that the
file path strings will be overwritten with the next drop operation.

In any case, sapp_get_dropped_file_path() will never return a null pointer,
instead an empty string "" will be returned if the drag'n'drop feature
hasn't been enabled, the last drop-operation failed, or the file path index
is out of range.

Drag'n'drop caveats:

    - if more files are dropped in a single drop-action
      than sapp_desc.max_dropped_files, the additional
      files will be silently ignored
    - if any of the file paths is longer than
      sapp_desc.max_dropped_file_path_length (in number of bytes, after UTF-8
      encoding) the entire drop operation will be silently ignored (this
      needs some sort of error feedback in the future)
    - no mouse positions are reported while the drag is in
      process, this may change in the future

Drag'n'drop on HTML5/WASM:

The HTML5 drag'n'drop API doesn't return file paths, but instead
black-box 'file objects' which must be used to load the content
of dropped files. This is the reason why sokol_app.h adds two
HTML5-specific functions to the drag'n'drop API:

    uint32_t sapp_html5_get_dropped_file_size(int index)
        Returns the size in bytes of a dropped file.

    void sapp_html5_fetch_dropped_file(const sapp_html5_fetch_request* request)
        Asynchronously loads the content of a dropped file into a
        provided memory buffer (which must be big enough to hold
        the file content)

To start loading the first dropped file after an SAPP_EVENTTYPE_FILES_DROPPED
event is received:

    sapp_html5_fetch_dropped_file(&(sapp_html5_fetch_request){
        .dropped_file_index = 0,
        .callback = fetch_cb
        .buffer = {
            .ptr = buf,
            .size = sizeof(buf)
        },
        .user_data = ...
    });

Make sure that the memory pointed to by 'buf' stays valid until the
callback function is called!

As result of the asynchronous loading operation (no matter if succeeded or
failed) the 'fetch_cb' function will be called:

    void fetch_cb(const sapp_html5_fetch_response* response) {
        // IMPORTANT: check if the loading operation actually succeeded:
        if (response->succeeded) {
            // the size of the loaded file:
            const size_t num_bytes = response->data.size;
            // and the pointer to the data (same as 'buf' in the fetch-call):
            const void* ptr = response->data.ptr;
        }
        else {
            // on error check the error code:
            switch (response->error_code) {
                case SAPP_HTML5_FETCH_ERROR_BUFFER_TOO_SMALL:
                    ...
                    break;
                case SAPP_HTML5_FETCH_ERROR_OTHER:
                    ...
                    break;
            }
        }
    }

Check the droptest-sapp example for a real-world example which works
both on native platforms and the web:

https://github.com/floooh/sokol-samples/blob/master/sapp/droptest-sapp.c

HIGH-DPI RENDERING
==================
You can set the sapp_desc.high_dpi flag during initialization to request
a full-resolution framebuffer on HighDPI displays. The default behaviour
is sapp_desc.high_dpi=false, this means that the application will
render to a lower-resolution framebuffer on HighDPI displays and the
rendered content will be upscaled by the window system composer.

In a HighDPI scenario, you still request the same window size during
sokol_main(), but the framebuffer sizes returned by sapp_width()
and sapp_height() will be scaled up according to the DPI scaling
ratio.

Note that on some platforms the DPI scaling factor may change at any
time (for instance when a window is moved from a high-dpi display
to a low-dpi display).

To query the current DPI scaling factor, call the function:

float sapp_dpi_scale(void);

For instance on a Retina Mac, returning the following sapp_desc
struct from sokol_main():

sapp_desc sokol_main(void) {
    return (sapp_desc) {
        .width = 640,
        .height = 480,
        .high_dpi = true,
        ...
    };
}

...the functions the functions sapp_width(), sapp_height()
and sapp_dpi_scale() will return the following values:

sapp_width:     1280
sapp_height:    960
sapp_dpi_scale: 2.0

If the high_dpi flag is false, or you're not running on a Retina display,
the values would be:

sapp_width:     640
sapp_height:    480
sapp_dpi_scale: 1.0

If the window is moved from the Retina display to a low-dpi external display,
the values would change as follows:

sapp_width:     1280 => 640
sapp_height:    960  => 480
sapp_dpi_scale: 2.0  => 1.0

Currently there is no event associated with a DPI change, but an
SAPP_EVENTTYPE_RESIZED will be sent as a side effect of the
framebuffer size changing.

Per-monitor DPI is currently supported on macOS and Windows.

APPLICATION QUIT
================
Without special quit handling, a sokol_app.h application will quit
'gracefully' when the user clicks the window close-button unless a
platform's application model prevents this (e.g. on web or mobile).
'Graceful exit' means that the application-provided cleanup callback will
be called before the application quits.

On native desktop platforms sokol_app.h provides more control over the
application-quit-process. It's possible to initiate a 'programmatic quit'
from the application code, and a quit initiated by the application user can
be intercepted (for instance to show a custom dialog box).

This 'programmatic quit protocol' is implemented through 3 functions
and 1 event:

    - sapp_quit(): This function simply quits the application without
      giving the user a chance to intervene. Usually this might
      be called when the user clicks the 'Ok' button in a 'Really Quit?'
      dialog box
    - sapp_request_quit(): Calling sapp_request_quit() will send the
      event SAPP_EVENTTYPE_QUIT_REQUESTED to the applications event handler
      callback, giving the user code a chance to intervene and cancel the
      pending quit process (for instance to show a 'Really Quit?' dialog
      box). If the event handler callback does nothing, the application
      will be quit as usual. To prevent this, call the function
      sapp_cancel_quit() from inside the event handler.
    - sapp_cancel_quit(): Cancels a pending quit request, either initiated
      by the user clicking the window close button, or programmatically
      by calling sapp_request_quit(). The only place where calling this
      function makes sense is from inside the event handler callback when
      the SAPP_EVENTTYPE_QUIT_REQUESTED event has been received.
    - SAPP_EVENTTYPE_QUIT_REQUESTED: this event is sent when the user
      clicks the window's close button or application code calls the
      sapp_request_quit() function. The event handler callback code can handle
      this event by calling sapp_cancel_quit() to cancel the quit.
      If the event is ignored, the application will quit as usual.

On the web platform, the quit behaviour differs from native platforms,
because of web-specific restrictions:

A `programmatic quit` initiated by calling sapp_quit() or
sapp_request_quit() will work as described above: the cleanup callback is
called, platform-specific cleanup is performed (on the web
this means that JS event handlers are unregistered), and then
the request-animation-loop will be exited. However that's all. The
web page itself will continue to exist (e.g. it's not possible to
programmatically close the browser tab).

On the web it's also not possible to run custom code when the user
closes a browser tab, so it's not possible to prevent this with a
fancy custom dialog box.

Instead the standard "Leave Site?" dialog box can be activated (or
deactivated) with the following function:

    sapp_html5_ask_leave_site(bool ask);

The initial state of the associated internal flag can be provided
at startup via sapp_desc.html5_ask_leave_site.

This feature should only be used sparingly in critical situations - for
instance when the user would loose data - since popping up modal dialog
boxes is considered quite rude in the web world. Note that there's no way
to customize the content of this dialog box or run any code as a result
of the user's decision. Also note that the user must have interacted with
the site before the dialog box will appear. These are all security measures
to prevent fishing.

The Dear ImGui HighDPI sample contains example code of how to
implement a 'Really Quit?' dialog box with Dear ImGui (native desktop
platforms only), and for showing the hardwired "Leave Site?" dialog box
when running on the web platform:

    https://floooh.github.io/sokol-html5/wasm/imgui-highdpi-sapp.html

FULLSCREEN
==========
If the sapp_desc.fullscreen flag is true, sokol-app will try to create
a fullscreen window on platforms with a 'proper' window system
(mobile devices will always use fullscreen). The implementation details
depend on the target platform, in general sokol-app will use a
'soft approach' which doesn't interfere too much with the platform's
window system (for instance borderless fullscreen window instead of
a 'real' fullscreen mode). Such details might change over time
as sokol-app is adapted for different needs.

The most important effect of fullscreen mode to keep in mind is that
the requested canvas width and height will be ignored for the initial
window size, calling sapp_width() and sapp_height() will instead return
the resolution of the fullscreen canvas (however the provided size
might still be used for the non-fullscreen window, in case the user can
switch back from fullscreen- to windowed-mode).

To toggle fullscreen mode programmatically, call sapp_toggle_fullscreen().

To check if the application window is currently in fullscreen mode,
call sapp_is_fullscreen().

WINDOW ICON SUPPORT
===================
Some sokol_app.h backends allow to change the window icon programmatically:

    - on Win32: the small icon in the window's title bar, and the
      bigger icon in the task bar
    - on Linux: highly dependent on the used window manager, but usually
      the window's title bar icon and/or the task bar icon
    - on HTML5: the favicon shown in the page's browser tab
    - on macOS: the application icon shown in the dock, but only
      for currently running applications

NOTE that it is not possible to set the actual application icon which is
displayed by the operating system on the desktop or 'home screen'. Those
icons must be provided 'traditionally' through operating-system-specific
resources which are associated with the application (sokol_app.h might
later support setting the window icon from platform specific resource data
though).

There are two ways to set the window icon:

    - at application start in the sokol_main() function by initializing
      the sapp_desc.icon nested struct
    - or later by calling the function sapp_set_icon()

As a convenient shortcut, sokol_app.h comes with a builtin default-icon
(a rainbow-colored 'S', which at least looks a bit better than the Windows
default icon for applications), which can be activated like this:

At startup in sokol_main():

    sapp_desc sokol_main(...) {
        return (sapp_desc){
            ...
            icon.sokol_default = true
        };
    }

Or later by calling:

    sapp_set_icon(&(sapp_icon_desc){ .sokol_default = true });

NOTE that a completely zero-initialized sapp_icon_desc struct will not
update the window icon in any way. This is an 'escape hatch' so that you
can handle the window icon update yourself (or if you do this already,
sokol_app.h won't get in your way, in this case just leave the
sapp_desc.icon struct zero-initialized).

Providing your own icon images works exactly like in GLFW (down to the
data format):

You provide one or more 'candidate images' in different sizes, and the
sokol_app.h platform backends pick the best match for the specific backend
and icon type.

For each candidate image, you need to provide:

    - the width in pixels
    - the height in pixels
    - and the actual pixel data in RGBA8 pixel format (e.g. 0xFFCC8844
      on a little-endian CPU means: alpha=0xFF, blue=0xCC, green=0x88, red=0x44)

For instance, if you have 3 candidate images (small, medium, big) of
sizes 16x16, 32x32 and 64x64 the corresponding sapp_icon_desc struct is setup
like this:

    // the actual pixel data (RGBA8, origin top-left)
    const uint32_t small[16][16]  = { ... };
    const uint32_t medium[32][32] = { ... };
    const uint32_t big[64][64]    = { ... };

    const sapp_icon_desc icon_desc = {
        .images = {
            { .width = 16, .height = 16, .pixels = SAPP_RANGE(small) },
            { .width = 32, .height = 32, .pixels = SAPP_RANGE(medium) },
            // ...or without the SAPP_RANGE helper macro:
            { .width = 64, .height = 64, .pixels = { .ptr=big, .size=sizeof(big) } }
        }
    };

An sapp_icon_desc struct initialized like this can then either be applied
at application start in sokol_main:

    sapp_desc sokol_main(...) {
        return (sapp_desc){
            ...
            icon = icon_desc
        };
    }

...or later by calling sapp_set_icon():

    sapp_set_icon(&icon_desc);

Some window icon caveats:

    - once the window icon has been updated, there's no way to go back to
      the platform's default icon, this is because some platforms (Linux
      and HTML5) don't switch the icon visual back to the default even if
      the custom icon is deleted or removed
    - on HTML5, if the sokol_app.h icon doesn't show up in the browser
      tab, check that there's no traditional favicon 'link' element
      is defined in the page's index.html, sokol_app.h will only
      append a new favicon link element, but not delete any manually
      defined favicon in the page

For an example and test of the window icon feature, check out the
'icon-sapp' sample on the sokol-samples git repository.

ONSCREEN KEYBOARD
=================
On some platforms which don't provide a physical keyboard, sokol-app
can display the platform's integrated onscreen keyboard for text
input. To request that the onscreen keyboard is shown, call

    sapp_show_keyboard(true);

Likewise, to hide the keyboard call:

    sapp_show_keyboard(false);

Note that onscreen keyboard functionality is no longer supported
on the browser platform (the previous hacks and workarounds to make browser
keyboards work for on web applications that don't use HTML UIs
never really worked across browsers).

INPUT EVENT BUBBLING ON THE WEB PLATFORM
========================================
By default, input event bubbling on the web platform is configured in
a way that makes the most sense for 'full-canvas' apps that cover the
entire browser client window area:

- mouse, touch and wheel events do not bubble up, this prevents various
  ugly side events, like:
    - HTML text overlays being selected on double- or triple-click into
      the canvas
    - 'scroll bumping' even when the canvas covers the entire client area
- key_up/down events for 'character keys' *do* bubble up (otherwise
  the browser will not generate UNICODE character events)
- all other key events *do not* bubble up by default (this prevents side effects
  like F1 opening help, or F7 starting 'caret browsing')
- character events do not bubble up (although I haven't noticed any side effects
  otherwise)

Event bubbling can be enabled for input event categories during initialization
in the sapp_desc struct:

    sapp_desc sokol_main(int argc, char* argv[]) {
        return (sapp_desc){
            //...
            .html5_bubble_mouse_events = true,
            .html5_bubble_touch_events = true,
            .html5_bubble_wheel_events = true,
            .html5_bubble_key_events = true,
            .html5_bubble_char_events = true,
        };
    }

This basically opens the floodgates and lets *all* input events bubble up to the browser.

To prevent individual events from bubbling, call sapp_consume_event() from within
the sokol_app.h event callback when that specific event is reported.


SETTING THE CANVAS OBJECT ON THE WEB PLATFORM
=============================================
On the web, sokol_app.h and the Emscripten SDK functions need to find
the WebGL/WebGPU canvas intended for rendering and attaching event
handlers. This can happen in four ways:

1. do nothing and just set the id of the canvas object to 'canvas' (preferred)
2. via a CSS Selector string (preferred)
3. by setting the `Module.canvas` property to the canvas object
4. by adding the canvas object to the global variable `specialHTMLTargets[]`
    (this is a special variable used by the Emscripten runtime to lookup
    event target objects for which document.querySelector() cannot be used)

The easiest way is to just name your canvas object 'canvas':

    <canvas id="canvas" ...></canvas>

This works because the default css selector string used by sokol_app.h
is '#canvas'.

If you name your canvas differently, you need to communicate that name to
sokol_app.h via `sapp_desc.html5_canvas_selector` as a regular css selector
string that's compatible with `document.querySelector()`. E.g. if your canvas
object looks like this:

    <canvas id="bla" ...></canvas>

The `sapp_desc.html5_canvas_selector` string must be set to '#bla':

    .html5_canvas_selector = "#bla"

If the canvas object cannot be looked up via `document.querySelector()` you
need to use one of the alternative methods, both involve the special
Emscripten runtime `Module` object which is usually setup in the index.html
like this before the WASM blob is loaded and instantiated:

    <script type='text/javascript'>
        var Module = {
            // ...
        };
    </script>

The first option is to set the `Module.canvas` property to your canvas object:

    <script type='text/javascript'>
        var Module = {
            canvas: my_canvas_object,
        };
    </script>

When sokol_app.h initializes, it will check the global Module object whether
a `Module.canvas` property exists and is an object. This method will add
a new entry to the `specialHTMLTargets[]` object

The other option is to add the canvas under a name chosen by you to the
special `specialHTMLTargets[]` map, which is used by the Emscripten runtime
to lookup 'event target objects' which are not visible to `document.querySelector()`.
Note that `specialHTMLTargets[]` must be updated after the Emscripten runtime
has started but before the WASM code is running. A good place for this is
the special `Module.preRun` array in index.html:

    <script type='text/javascript'>
        var Module = {
            preRun: [
                () => {
                    specialHTMLTargets['my_canvas'] = my_canvas_object;
                }
            ],
        };
    </script>

In that case, pass the same string to sokol_app.h which is used as key
in the specialHTMLTargets[] map:

    .html5_canvas_selector = "my_canvas"

If sokol_app.h can't find your canvas for some reason check for warning
messages on the browser console.


OPTIONAL: DON'T HIJACK main() (#define SOKOL_NO_ENTRY)
======================================================
NOTE: SOKOL_NO_ENTRY and sapp_run() is currently not supported on Android.

In its default configuration, sokol_app.h "hijacks" the platform's
standard main() function. This was done because different platforms
have different entry point conventions which are not compatible with
C's main() (for instance WinMain on Windows has completely different
arguments). However, this "main hijacking" posed a problem for
usage scenarios like integrating sokol_app.h with other languages than
C or C++, so an alternative SOKOL_NO_ENTRY mode has been added
in which the user code provides the platform's main function:

- define SOKOL_NO_ENTRY before including the sokol_app.h implementation
- do *not* provide a sokol_main() function
- instead provide the standard main() function of the platform
- from the main function, call the function ```sapp_run()``` which
  takes a pointer to an ```sapp_desc``` structure.
- from here on```sapp_run()``` takes over control and calls the provided
  init-, frame-, event- and cleanup-callbacks just like in the default model.

sapp_run() behaves differently across platforms:

    - on some platforms, sapp_run() will return when the application quits
    - on other platforms, sapp_run() will never return, even when the
      application quits (the operating system is free to simply terminate
      the application at any time)
    - on Emscripten specifically, sapp_run() will return immediately while
      the frame callback keeps being called

This different behaviour of sapp_run() essentially means that there shouldn't
be any code *after* sapp_run(), because that may either never be called, or in
case of Emscripten will be called at an unexpected time (at application start).

An application also should not depend on the cleanup-callback being called
when cross-platform compatibility is required.

Since sapp_run() returns immediately on Emscripten you shouldn't activate
the 'EXIT_RUNTIME' linker option (this is disabled by default when compiling
for the browser target), since the C/C++ exit runtime would be called immediately at
application start, causing any global objects to be destroyed and global
variables to be zeroed.

WINDOWS CONSOLE OUTPUT
======================
On Windows, regular windowed applications don't show any stdout/stderr text
output, which can be a bit of a hassle for printf() debugging or generally
logging text to the console. Also, console output by default uses a local
codepage setting and thus international UTF-8 encoded text is printed
as garbage.

To help with these issues, sokol_app.h can be configured at startup
via the following Windows-specific sapp_desc flags:

    sapp_desc.win32_console_utf8 (default: false)
        When set to true, the output console codepage will be switched
        to UTF-8 (and restored to the original codepage on exit)

    sapp_desc.win32_console_attach (default: false)
        When set to true, stdout and stderr will be attached to the
        console of the parent process (if the parent process actually
        has a console). This means that if the application was started
        in a command line window, stdout and stderr output will be printed
        to the terminal, just like a regular command line program. But if
        the application is started via double-click, it will behave like
        a regular UI application, and stdout/stderr will not be visible.

    sapp_desc.win32_console_create (default: false)
        When set to true, a new console window will be created and
        stdout/stderr will be redirected to that console window. It
        doesn't matter if the application is started from the command
        line or via double-click.

ERROR REPORTING AND LOGGING
===========================
To get any logging information at all you need to provide a logging callback in the setup call
the easiest way is to use sokol_log.h:

    #include "sokol_log.h"

    sapp_desc sokol_main(int argc, char* argv[]) {
        return (sapp_desc) {
            ...
            .logger.func = slog_func,
        };
    }

To override logging with your own callback, first write a logging function like this:

    void my_log(const char* tag,                // e.g. 'sapp'
                uint32_t log_level,             // 0=panic, 1=error, 2=warn, 3=info
                uint32_t log_item_id,           // SAPP_LOGITEM_*
                const char* message_or_null,    // a message string, may be nullptr in release mode
                uint32_t line_nr,               // line number in sokol_app.h
                const char* filename_or_null,   // source filename, may be nullptr in release mode
                void* user_data)
    {
        ...
    }

...and then setup sokol-app like this:

    sapp_desc sokol_main(int argc, char* argv[]) {
        return (sapp_desc) {
            ...
            .logger = {
                .func = my_log,
                .user_data = my_user_data,
            }
        };
    }

The provided logging function must be reentrant (e.g. be callable from
different threads).

If you don't want to provide your own custom logger it is highly recommended to use
the standard logger in sokol_log.h instead, otherwise you won't see any warnings or
errors.

TEMP NOTE DUMP
==============
- sapp_desc needs a bool whether to initialize depth-stencil surface
- the Android implementation calls cleanup_cb() and destroys the egl context in onDestroy
  at the latest but should do it earlier, in onStop, as an app is "killable" after onStop
  on Android Honeycomb and later (it can't be done at the moment as the app may be started
  again after onStop and the sokol lifecycle does not yet handle context teardown/bringup)


LICENSE
=======
zlib/libpng license

Copyright (c) 2018 Andre Weissflog

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from the
use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in a
    product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not
    be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
