#include "test.h"
// TODO: whose job is it to mangle with an '_' on macos?

int foo(int a) { return a * 3; }
int bar(int a);
typeof(bar) bar asm("foo");

int b = 123;
extern int a asm("b");

int main() {
    ASSERT(21, bar(7));
    ASSERT(24, foo(8));
    
    ASSERT(a, 123);
    b = 456;
    ASSERT(a, 456);
    a = 789;
    ASSERT(b, 789);
    
    printf("OK\n");
    return 0;
}
