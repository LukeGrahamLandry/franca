#include "test.h"

int main() {
  ASSERT(1, _Generic(100.0, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(2, _Generic((int *)0, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(2, _Generic((int[3]){}, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(3, _Generic(100, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(4, _Generic(100.0f, double: 1, int *: 2, int: 3, float: 4));
  ASSERT(2, _Generic('a', char: 1, int: 2));
  ASSERT(2, _Generic(1 || "abc", _Bool: 1, int: 2));
  
  ASSERT(2, ({ int *a, b; _Generic(b, int*: 1, int: 2); }));
  ASSERT(1, ({ typeof(int*) a, b; _Generic(b, int*: 1, int: 2); }));
  /* 
  {  // :BrokenCGeneric long is exactly the same as long long except that clang can tell them apart:
      long l = 0;
      long long ll = 0;
      #define is_ll(x) _Generic(x, long: 0, long long: 1)
      ASSERT(0, is_ll(l));
      ASSERT(1, is_ll(ll));
  };
  */
  
  // enum tags are a new type but the enum's constants don't have that type. 
  // [/n1570#6.7.2.2p3] "The identifiers in an enumerator list are declared as constants that have type int".
  ASSERT(2, ({ enum foo { a = 0 }; _Generic(a, enum foo: 1, int: 2); }));
  ASSERT(1, ({ enum foo { a = 0 }; enum foo b = a; _Generic(b, enum foo: 1, int: 2); }));
  
  printf("OK\n");
  return 0;
}
