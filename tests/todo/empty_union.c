// franca examples/import_c/cc.fr a.c -arch x86_64
// panic! Assertion Failed: type '' has no cases
struct Foo { long tag; union {}; };
void foo(struct Foo it) { }
