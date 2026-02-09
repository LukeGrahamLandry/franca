#include "test.h"

int fifty(void) {
#if __x86_64
    asm(".byte 0x48\n.byte 0xc7\n.byte 0xc0\n.byte 0x32\n.byte 0x0\n.byte 0x0\n.byte 0x0\n.byte 0xc3\n");
#elif __aarch64
    asm("mov x0, 50\n\t"
        "ret");
#elif __rv64
    asm("li a0, 50\n\t"
        "ret");
#elif __wasm32
    // 4 bytes, 0 locals, i32.const(50), end
    asm(".byte 4\n.byte 0x00\n.byte 0x41\n.byte 50\n.byte 0x0B\n");
#endif
}

int main() {
  ASSERT(50, fifty());

  printf("OK\n");
  return 0;
}
