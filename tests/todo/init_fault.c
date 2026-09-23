struct B { int foo; };
struct A { int foo[4]; struct B baz; } bar;
int main() {
    bar.baz = /*(struct B)*/ { 123 };  // missing the type but shouldn't fault
    bar.foo = { 0 };  // arrays aren't assignable but shouldn't fault
}
