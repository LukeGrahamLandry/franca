// TODO: make this work without just disabling elide_abi_slots for variadic

#include "stdio.h"
#include "stdarg.h"

int f(int a, ...);
int main() {
    signed short x = 0x8080;
    int result = !f((unsigned short) x, (unsigned short) x);
    printf("%04x", (unsigned short) x);
    return result;
}

int f(int a, ...) {
    va_list va;
    va_start(va, a);
    int x = va_arg(va, int);
    return x == 0x8080 && a == 0x8080;
}
