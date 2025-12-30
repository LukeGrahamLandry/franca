#include "test.h"

static int ret10(void) { return 10; }

int main() {
  ASSERT((long)-5, -10 + (long)5);
  ASSERT((long)-15, -10 - (long)5);
  ASSERT((long)-50, -10 * (long)5);
  ASSERT((long)-2, -10 / (long)5);

  ASSERT(1, -2 < (long)-1);
  ASSERT(1, -2 <= (long)-1);
  ASSERT(0, -2 > (long)-1);
  ASSERT(0, -2 >= (long)-1);

  ASSERT(1, (long)-2 < -1);
  ASSERT(1, (long)-2 <= -1);
  ASSERT(0, (long)-2 > -1);
  ASSERT(0, (long)-2 >= -1);

  ASSERT(0, 2147483647 + 2147483647 + 2);
  ASSERT((long)-1, ({ long x; x=-1; x; }));

  ASSERT(1, ({ char x[3]; x[0]=0; x[1]=1; x[2]=2; char *y=x+1; y[0]; }));
  ASSERT(0, ({ char x[3]; x[0]=0; x[1]=1; x[2]=2; char *y=x+1; y[-1]; }));
  ASSERT(5, ({ struct t {char a;} x, y; x.a=5; y=x; y.a; }));

  ASSERT(10, (1 ? ret10 : (void *)0)());
  
  ASSERT(4, ({ short x[1] = { 5 }; x[0] -= 1; x[0]; }));
  
  // kinda related casting mistake. i was truncating to char too enthusiastically. 
  ASSERT(0, ({ 
    unsigned char a = 0x12;
    unsigned char b = 0x34;
    unsigned int c = (a<<8) + b;
    unsigned int d = 0x1234;
    unsigned int e = ((int)a<<8) + b;
    
    unsigned char y = 5;
    int x = ~y;
    unsigned long z = 5;
    int xx = ~z;
    
    // since im testing casting stuff here, it feels like using `==` is tempting fate. 
    char result[64];
    sprintf(result, "%#04x %#04x %#04x %d %d", c, d, e, x, xx);
    strcmp(result, "0x1234 0x1234 0x1234 -6 -6");
  }));

  printf("OK\n");
  return 0;
}

// :ParamTypeList
static int foo(unsigned long nonconstant) {
    unsigned long remaining = nonconstant / 10;
    nonconstant -= remaining * 10;
    return nonconstant;
}
