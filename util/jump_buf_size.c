
#include <setjmp.h>
#include <stdio.h>
int main() { printf("sizeof(jumpbuf) = %ld\n", sizeof(jmp_buf)); }