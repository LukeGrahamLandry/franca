int bar(int a) {  return a - 1; }
// note the missing *
struct { int (foo)(int); } vtable = { bar };
int main() { return vtable.foo(1); }
