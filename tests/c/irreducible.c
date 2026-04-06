#include "test.h"
// see backend/wasm/isel.fr/fix_multientry_loops

int A(int a) {
    int j = 0;
    if (a % 3 == 0) goto bar;
    for (;j<a; j++) bar:;
    return j;
}

// this is the shape of ir fix_multientry_loops made for A(). it's reducible but broke my wasm_flow. 
// thus that fix is still needed even if i give up and use loop(switch) if detect irreducible cfg. 
int A_fixed(int a) {
    int label, cond, j, c;
    A: j = 0; if (a % 3) goto B; else goto C;
    B: label = 4; goto D;
    C: label = 3; goto D;
    D: if(label == 3) goto H; else goto E;
    E: c = j + 1; if (j) goto F; else goto G;
    F: label = 3; goto D;
    G: return c;
    H: j = c; label = 4; goto D;
}

/* // TODO
int B(int a) {
    int i = 0; int j = 0;
    for (;i<a; i++) {
        if (0) foo: j++; 
        if (i % 3 == 0) goto bar;
    }
    for (;j<a; j++) {
        if (0) bar: i++; 
        if (j % 5 == 0) goto foo;
    }
    return i + j;
}
*/

// same shape as stbi__hdr_load
int C(int x) {
   int j;
   if (x < 8) {
      for (j=0; j<x; ++j) if (x) JUMP:;
   } else {
      for (j=0; j<x; ++j) goto JUMP; 
   }
   return x+j;
}

/* // TODO
// same shape as stbtt__run_charstring
int D(int i, int x) {
    while (i) {
        switch (i) {
        case 1: goto JUMP;
        case 2: 
            for (;;) { JUMP: if (x) break; x++; }
        }
        if (x) return i;
    }
    return x;
}
*/

// the important thing is the isel debug assertions and whether it makes a valid wasm module. 
// i don't care enough to make the tests actually do stuff and try all interesting inputs.  
int main() {
    ASSERT(1, A(1));
    ASSERT(1, A_fixed(1));
    // TODO: ASSERT(25, B(15));
    ASSERT(2, C(1));
    // TODO: ASSERT(1, D(1, 1));
    printf("OK\n");
    return 0;
}
