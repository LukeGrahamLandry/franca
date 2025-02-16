#include "test.h"

typedef struct empty_struct { char not_empty; } empty_struct;
empty_struct make_empty_struct() { empty_struct ret; ret.not_empty = 0; return ret; }
empty_struct make_empty_struct2() { return make_empty_struct(); }

int main() {
    make_empty_struct2(); make_empty_struct();
    
    // :ThisShouldBeValidUndef
    ASSERT(1, ({
        int t_3;
        if (1) while (1) if (1) { t_3 = 1; break; }
        else t_3 = 2;
        t_3;  // definitely initialized
    }));
    
    printf("OK\n");
    return 0;
}
