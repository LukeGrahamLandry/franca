#include "test.h"

typedef struct empty_struct { char not_empty; } empty_struct;
empty_struct make_empty_struct() { empty_struct ret; ret.not_empty = 0; return ret; }
empty_struct make_empty_struct2() { return make_empty_struct(); }

int main() {
    // TODO: this fails
    // int argc = 1;
    // char *argv[2] = {"a", 0 };
    // int c = (argc > 0) ? 1 : 0;  // Skip argv[0], the program name.
    // for (; c < argc; c++) {
    //   char* arg = argv[c];
    //   if (*arg++ != '-') {
    //     break;
    //   }
    // }
      
    make_empty_struct2(); make_empty_struct();
    printf("OK\n");
    return 0;
}
