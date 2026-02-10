#include "test.h"

_Static_assert(1 > 0 || 0);
int f(int a) { return a; }

int main(void) {
    _Static_assert(1, "unreachable");
    
    ASSERT(1, __builtin_constant_p(1 + 2 * 3 >> 4 / 5));
    ASSERT(0, __builtin_constant_p(f(1)));
    ASSERT(0, __builtin_constant_p((f(1), 123)));
    ASSERT(1, __builtin_constant_p((1, 123)));
    
    ASSERT(1, __builtin_expect(1, 2));
    ASSERT(1, __builtin_ctz(2));
    ASSERT(30, __builtin_clz(2));
    ASSERT(62, __builtin_clzl(2));
    ASSERT(1, __builtin_popcount(2));
    ASSERT(1, __builtin_ctz(0) == 32 && __builtin_clz(0) == 32 && __builtin_popcount(0) == 0);
    ASSERT(0x81234567, __builtin_rotateright32(0x12345678, 4));
    ASSERT(0x23456781, __builtin_rotateleft32(0x12345678, 4));
    ASSERT(0x78563412, __builtin_bswap32(0x12345678));
    
    // this doesn't test that it actually works, just that i supply something. 
    // (the real test is compiling luajit without using someone else's linker)
    static char very_important_machine_code[512]; 
    __builtin___clear_cache(very_important_machine_code, very_important_machine_code + 512);
    
    printf("OK\n");
    return 0;
}
