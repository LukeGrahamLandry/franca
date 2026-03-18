#include "test.h"

int blits0() {
    typedef struct { int d[8]; } D;
    typedef struct { long a[4]; int b; D c; } F;
	D c = { -1, -1, -1, -1, -1, -1, -1, -1, };
	F b = (F) { .c = c, };
	for (int i=0;i<8;i++) if (c.d[i] != b.c.d[i]) return 1;
	return 0;
}
typedef struct A { int b; int c; char d; } A;
typedef struct B { A a; long e; } B;
void blits1_check(B *a);
int blits1() {
	A v1 = (A) { .b = 0, .c = 0, .d = 1, };
	B v2 = (B) { .a = v1, .e = 0, };
	blits1_check(&v2);
	return 0;
}
void blits1_check(B *a) { if (!a->a.d) exit(1); }

int main() {
    ASSERT(blits0(), 0);
    ASSERT(blits1(), 0);
    printf("OK\n");
    return 0;
}