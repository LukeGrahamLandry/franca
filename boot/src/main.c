
typedef char u8;
typedef unsigned int u32;
typedef long i64;
typedef u8 bool;
typedef struct { u8 *ptr; i64 len; } Str;
#define true 1
#define false 0

void panic(char *msg) { 
    int puts(char *msg);
    puts(msg);
    while(1);
}

#include "lex.c"

int main() {
    return 0;
}
