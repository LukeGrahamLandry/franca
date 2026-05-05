// shows the mistake if top_level_fn.con is in temp() instead of long_life. 
// manifests reliably if threaded=false and SLOW_MEMORY_DEBUGGING=true.  
const static int a = 2;
int f(void) {
    const static int b = 3;
    const static int bb = b + 4;
    const static int bbb = bb + 5;
    const static int bbbb = bbb + 6;
    return bbbb;
}
const static int c = 2;
int main() {
    int puts(char*);
    puts("OK");
    return !(a == 2 && f() == 18 && c == 2);
}