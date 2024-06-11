#include <stdlib.h>

long c_add(long a, long b) { return a + b; }

// Loops need both forward and backward jumps.
long c_fac(long n) {
  long a = 1;
  long b = 1;
  for (; n > 0; n--) {
    long c = a + b;
    a = b;
    b = c;
  }
  return a;
}

// Use adr/adrp to access a .asciz constant.
char *c_str_lit() { return "Hello World"; }

// Use adr/adrp to access a .quad array (only in -O2).
long c_switch_expr(long a) {
  switch (a) {
  case 5:
    return 123;
  case 7:
    return 456;
  case 9:
    return 789;
  default:
    return 0;
  }
}

// Call an external function.
long *c_call_malloc(long a) { return malloc(sizeof(long) * a); }

// Mutable global .zero_fill data
long c_shared() {
  static long state = 0;
  state += 1;
  return state;
}

// Mutable global initilized data
long c_shared2() {
  static long state = 123;
  state += 1;
  return state;
}
