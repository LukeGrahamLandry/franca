#include "test.h"

static const int var0 = 123 + 456;
static const int var1 = var0 + 1;        // apple's CGFont.h kCGGlyphMax does this. 
static const char *not_a_constant = "";  // still allow const as a type modifier. 
typedef const int *(*unused_fn_ty)(int a);

int main() {
    const int var2 = 1;                // local + no static, still usable in constexpr. 
    static const int var3 = var2 + 1;  // static. initializer must be constant. 
    int var4 = var3 + 1;               // not const,
    const int var5 = var4 + 1;         // so this one can't be static. 
    ASSERT(580, var1);
    ASSERT(3, var4);
    ASSERT(0, strlen(not_a_constant));
    not_a_constant = "abc";
    ASSERT(3, strlen(not_a_constant));
    
    { const x; }
    { int const x; }
    { const int x; }
    { const int const const x; }
    ASSERT(5, ({ const x = 5; x; }));
    ASSERT(8, ({ const x = 8; int *const y=&x; *y; }));
    ASSERT(6, ({ const x = 6; *(const * const)&x; }));
    
    printf("OK\n");
    return 0;
}
