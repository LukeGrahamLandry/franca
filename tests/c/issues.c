
#define stringify2(x) #x
#define stringify(x) stringify2(x)

// #153
#define EMPTY()
#define DEFER(id) id EMPTY()
#define EXPAND(...) __VA_ARGS__
#define BAR_I() BAR
#define BAR()  DEFER(BAR_I)()() 1
char *i156 = stringify(EXPAND(EXPAND(BAR())));

#include "test.h"
int main() {
    ASSERT(0, strcmp("BAR_I ()() 1 1 1", i156));
    printf("OK\n");
}
