// i do the casting wrong for `c` it truncates to u8 instead of promoting to int

#include "stdio.h"
int main() {
    unsigned char a = 0x12;
    unsigned char b = 0x34;
    unsigned int c = (a<<8) + b;
    unsigned int d = 0x1234;
    unsigned int e = ((int)a<<8) + b;
    printf("%#04x %#04x %#04x\n", c, d, e);
}
