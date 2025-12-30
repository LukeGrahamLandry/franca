#include "test.h"

void *fn(int x, void *p, int y) { return p; }

// makes this work on other compilers with -Wimplicit-function-declaration
void *alloca(unsigned long);

int main() {
  int i = 0;

  char *p1 = alloca(16);
  char *p2 = alloca(16);
  char *p3 = 1 + (char *)alloca(3) + 1;
  p3 -= 2;
  char *p4 = fn(1, alloca(16), 3);

  // node: chibicc test was asserting the difference was exactly 16 (clang didn't pass that either). 
  ASSERT(0, (p1 - p2) % 16);
  ASSERT(0, (p2 - p3) % 16);
  ASSERT(0, (p3 - p4) % 16);

  memcpy(p1, "0123456789abcdef", 16);
  memcpy(p2, "ghijklmnopqrstuv", 16);
  memcpy(p3, "wxy", 3);

  ASSERT(0, memcmp(p1, "0123456789abcdef", 16));
  ASSERT(0, memcmp(p2, "ghijklmnopqrstuv", 16));
  ASSERT(0, memcmp(p3, "wxy", 3));

  printf("OK\n");
  return 0;
}
