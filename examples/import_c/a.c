int printf(const char *, ...);

static int a = 0;

int *foo() {
    printf("called foo()");
    return &a;
}

int bar() {
    printf("called bar()");
    return 1;
}

int main() {
    *foo() += bar();  // unsequenced 
    return 0;
}
