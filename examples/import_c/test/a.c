#include "test.h"

typedef struct empty_struct { char not_empty; } empty_struct;
empty_struct make_empty_struct() { empty_struct ret; ret.not_empty = 0; return ret; }
empty_struct make_empty_struct2() { return make_empty_struct(); }

// this broke i was doing jnz on a constant with a tmp undefined on the unreachable branch
int inp(int p) {
    return (1 == 4 ? 1 : p == 0);
}

// These both need to get treated as function forward declarations. 
// TODO: run these tests aot as well because this bug (backend getting redeclaration) didn't matter when jitting. 
int foo(void), bar(char *);

int main() {
    make_empty_struct2(); make_empty_struct();
    
    // :ThisShouldBeValidUndef
    ASSERT(1, ({
        int t_3;
        if (1) while (1) if (1) { t_3 = 1; break; }
        else t_3 = 2;
        t_3;  // definitely initialized
    }));
    
    ASSERT(9, ({
        typedef struct { char nactvar; } X;
        X _fs = { 10 }; X *fs = &_fs;
        char x = --fs->nactvar;
        x;
    }));
    
    inp(0);
    
    ASSERT(0, ({
        char *a = *& "abcdefg" + 4;
        strcmp(a, "efg");
    }));
    ASSERT(2, foo() + bar(""));
    
    printf("OK\n");
    return 0;
}

int foo(void) { return 1; }
int bar(char* a) { return 1; }
