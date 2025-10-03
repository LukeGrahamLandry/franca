int puts(char*);

int foo(int a) { return a * 3; }
int bar(int a);
typeof(bar) bar asm("foo");

int main() {
    int result = bar(7) != 21 || foo(8) != 24;
    puts("OK");
    return result;
}
