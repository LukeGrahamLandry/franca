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

// This is the shape of musl's syscall.h that confused my hide stack. 
long __syscall_cp(long a, long b) { return 0; }
#define __SYSCALL_CONCAT_X(a,b) a##b
#define __SYSCALL_CONCAT(a) __SYSCALL_CONCAT_X(a,1)
#define __SYSCALL_DISP(b,...) __SYSCALL_CONCAT(b)(__VA_ARGS__)
#define __syscall_cp1(n,a) (__syscall_cp)(n,(a))
#define __syscall_cp(...) __SYSCALL_DISP(__syscall_cp,__VA_ARGS__)
#define syscall_cp(...) __syscall_cp(__VA_ARGS__)

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
    
    // call twice to make sure it's not disabled
    syscall_cp(1, 2);
    syscall_cp(3, 4);
    
    {
        float x = 0.0;
        if (x) return 1;
        x = x / x;
        if (!x) return 2;
        x = 1.0;
        if (!x) return 3;
    }
    
    ASSERT(7, ({
        union val { int i; void *p; };
        union val a = {7};
        union val b = a;
        struct val2 { union val d; int e; };
        struct val2 c = {
            .d = b,
            .e = 123,
        };
        c.d.i;
    }));

    // still set .ty when speculate=true
    if (sizeof(1.0 < 2.0) == sizeof(float));
    
    // don't let speculate() stomp live block's jmp. 
    ASSERT(4, sizeof(({ return 1; 0; })));
    // 0 ? ({ return 1; }) : 0;  // TODO: this crashes
    
    ASSERT(0, ({ long *a = (long[]){0}; *a; }));
    
    (1 ? 0 : ({ while (1) 0; }) );
    
    if (0) return 1;  // can't speculate() because:
    
    printf("OK\n");
    
    goto ok;
    if (0) ok: return 0;
    return 1;
}

int foo(void) { return 1; }
int bar(char* a) { return 1; }

#define unused_macro_with_invalid_token ..hello_world..

// chibicc does defined() in #if before expanding macros. apple wants to use it in a macro. 
#define macro1() defined(macro2)
#if macro1()
#endif
