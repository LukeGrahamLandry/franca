// this segfaults while trying to panic if you jit it with `cc.out -r`
// the problem is that JitOnly doesn't use ZeroInitData and `data $ttf_buffer = { z 33554432 }` doesn't fit in my default 2^25 MutableData

char ttf_buffer[1<<25] = {0};
int main() {
    return ttf_buffer[0] + ttf_buffer[99999] + ttf_buffer[9999999];
}
