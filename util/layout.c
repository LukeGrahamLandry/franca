#include <stdio.h>

typedef struct Nested {
  struct {
    long big;
    char small;
  } inner;
  char last;
} Nested;

typedef struct Together {
  long big;
  char small;
  char last;
} Together;

int main() {
  {
    Nested value;
    printf("Nested: \n");
    printf("big: %ld\n", (long)&value.inner.big - (long)&value);
    printf("small: %ld\n", (long)&value.inner.small - (long)&value);
    printf("last: %ld\n", (long)&value.last - (long)&value);
  }
  {

    Together value;
    printf("---\nTogether: \n");
    printf("big: %ld\n", (long)&value.big - (long)&value);
    printf("small: %ld\n", (long)&value.small - (long)&value);
    printf("last: %ld\n", (long)&value.last - (long)&value);
  }
  return 0;
}
