#include "test.h"
#include <stdarg.h>
#include <stddef.h>

int vsprintf(char *buf, char *fmt, va_list ap);

int sum1(int x, ...) {
  va_list ap;
  va_start(ap, x);

  for (;;) {
    int y = va_arg(ap, int);
    if (y == 0)
      return x;
    x += y;
  }
}

int sum2(int x, ...) {
  va_list ap;
  va_start(ap, x);

  for (;;) {
    double y = va_arg(ap, double);
    x += y;

    int z = va_arg(ap, int);
    if (z == 0)
      return x;
    x += z;
  }
}

void fmt(char *buf, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);

  va_list ap2;
  va_copy(ap2, ap);
  vsprintf(buf, fmt, ap2);
}

int is_8080(int a, ...) {
    va_list va;
    va_start(va, a);
    int x = va_arg(va, int);
    return x == 0x8080 && a == 0x8080;
}

// define function with va_list as by-value parameter.
// catches mistake with extra gen_addr on amd_sysv. 
int sum_va_list(int foo, va_list ap) {
    int *val;
    int sum = 0;
    while ((val = va_arg(ap, int *))) {
        sum += *val;
    }
    return sum;
}

int sum_va(int foo, ...) {
    va_list ap;
    va_start(ap, foo);
    int sum = sum_va_list(foo, ap);
    va_end(ap);
    return sum;
}

int main() {
  ASSERT(6, sum1(1, 2, 3, 0));
  ASSERT(55, sum1(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0));
  ASSERT(21, sum2(1, 2.0, 3, 4.0, 5, 6.0, 0));
  ASSERT(21, sum2(1, 2.0, 3, 4.0, 5, 6.0, 0));
  ASSERT(210, sum2(1, 2.0, 3, 4.0, 5, 6.0, 7, 8.0, 9, 10.0, 11, 12.0, 13, 14.0, 15, 16.0, 17, 18.0, 19, 20.0, 0));
  ASSERT(0, ({ char buf[100]; fmt(buf, "%d %d", 2, 3); strcmp(buf, "2 3"); }));
  ASSERT(1, ({ short x = 0x8080; is_8080((unsigned short) x, (unsigned short) x); }));
  
  // default-argument-promotion for literal 0 is to int, so if you do that instead of NULL, 
  // i think its legal that i don't zero the alignment padding for apple-arm. 
  ASSERT(123, ({ int x = 123; sum_va(0, &x, NULL); }));

  printf("OK\n");
  return 0;
}
