#define ASSERT(x, y) assert(x, y, #y)

int printf(const char *fmt, ...);
int memcmp(const void *p, const void *q, unsigned long n);
void exit(int n);
void *memcpy(void *dest, const void *src, unsigned long n);
void abort();
int strcmp(char *p, char *q);
int sprintf(char *buf, char *fmt, ...);
int strlen(char*);

static void assert(int expected, int actual, char *code) {
  if (expected != actual) {
    printf("%d != %d; %s\n", expected, actual, code);
    abort();
  }
}

