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

void voidexpr2() {}
void voidexpr() { return voidexpr2(); }

typedef void V; 
void voidtypedef(V) {}

int main() {
    make_empty_struct2(); make_empty_struct();
    
    // :ThisShouldBeValidUndef
    ASSERT(1, ({
        int t_3;
        if (1) while (1) if (1) { t_3 = 1; break; }
        else t_3 = 2;
        t_3;  // definitely initialized
    }));
    int unused_undef; (void) unused_undef;
    
    // not sure i want to commit to UNDEF having a consistant value like this. 
    // it's fine if you break this test (and remove it) on purpose, 
    // just then need to be careful to allow legal cases like the above. 
    ASSERT(0, ({ int undef; undef ^ undef; }));
    
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
    0 ? ({ return 1; }) : 0;
    
    ASSERT(0, ({ long *a = (long[]){0}; *a; }));
    
    (1 ? 0 : ({ while (1) 0; }) );
    int nonconstant = 0; nonconstant = 1;
    (nonconstant ? voidexpr() : voidexpr());
    voidtypedef();
    
    ASSERT(0, ({ void *to_void = 0; to_void == to_void + 1; }));
    ASSERT(0, ({ struct { char a; } a = {0}; (0 ? a : a).a; }));  // get_common_type
    
    enum { value_0 };  // shadowed by a label
    switch(1) {
        value_0: case value_0: break;
        default: goto value_0;
    }
    
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
