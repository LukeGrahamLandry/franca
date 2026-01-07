#include "test.h"

// TODO: this is trying to test VLAs, and it does test that they work in the frontend, 
//       but since all the sizes are actually constants, they get folded by the backend, 
//       so it doesn't test that i generate correct code for dynamic stack allocations.   

int main() {
  ASSERT(20, ({ int n=5; int x[n]; sizeof(x); }));
  ASSERT((5+1)*(8*2)*4, ({ int m=5, n=8; int x[m+1][n*2]; sizeof(x); }));

  ASSERT(8, ({ char n=10; int (*x)[n][n+2]; sizeof(x); }));
  ASSERT(480, ({ char n=10; int (*x)[n][n+2]; sizeof(*x); }));
  ASSERT(48, ({ char n=10; int (*x)[n][n+2]; sizeof(**x); }));
  ASSERT(4, ({ char n=10; int (*x)[n][n+2]; sizeof(***x); }));

  ASSERT(60, ({ char n=3; int x[5][n]; sizeof(x); }));
  ASSERT(12, ({ char n=3; int x[5][n]; sizeof(*x); }));

  ASSERT(60, ({ char n=3; int x[n][5]; sizeof(x); }));
  ASSERT(20, ({ char n=3; int x[n][5]; sizeof(*x); }));

  ASSERT(0, ({ int n=10; int x[n+1][n+6]; int *p=x; for (int i = 0; i<sizeof(x)/4; i++) p[i]=i; x[0][0]; }));
  ASSERT(5, ({ int n=10; int x[n+1][n+6]; int *p=x; for (int i = 0; i<sizeof(x)/4; i++) p[i]=i; x[0][5]; }));
  ASSERT(5*16+2, ({ int n=10; int x[n+1][n+6]; int *p=x; for (int i = 0; i<sizeof(x)/4; i++) p[i]=i; x[5][2]; }));

  ASSERT(10, ({ int n=5; sizeof(char[2][n]); }));
  
  // make sure prefix '*' as vla size isn't mistaken as bare [*] (which is allowed on parameters)
  ASSERT(123 * sizeof(int), ({ int x = 123; int *xx = &x; int a[*xx]; sizeof(a); }));

  printf("OK\n");
  return 0;
}

/* TODO: rn i say (undefined variable: 'x') for this. 
// you're allowed to claim you're passing a vla 
// but it decays to a pointer without remembering the size. 
int fake_vla_parameter(size_t x, int a[x]) { return sizeof a; }
    ASSERT(sizeof(int*), ({ int x = 123; int a[x]; fake_vla_parameter(x, a); }));
*/
