#include "test.h"

int main() {
  ASSERT(1, _Generic(100.0, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(2, _Generic((int *)0, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(2, _Generic((int[3]){}, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(3, _Generic(100, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(4, _Generic(100f, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(2, _Generic('a', char: 1, int: 2));
  
  /* 
  {  // :BrokenCGeneric long is exactly the same as long long except that clang can tell them apart:
      long l = 0;
      long long ll = 0;
      #define is_ll(x) _Generic(x, long: 0, long long: 1)
      ASSERT(0, is_ll(l));
      ASSERT(1, is_ll(ll));
  };
  */
  
  printf("OK\n");
  return 0;
}
