
// TODO: clang says `error: expression is not assignable` but i allow
struct b { int a; };
int f(struct b *a, struct b *b, int c) {
    return (c ? *a : *b).a++;
}
