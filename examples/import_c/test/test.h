#define ASSERT(x, y) assert(x, y, #y)

int printf(const char *fmt, ...);
int memcmp(const void *p, const void *q, unsigned long n);
void exit(int n);
void *memcpy(void *dest, const void *src, unsigned long n);
int strcmp(char *p, char *q);
int sprintf(char *buf, char *fmt, ...);
int strlen(char*);
int strncmp ( const char * str1, const char * str2, long num);

static void assert(int expected, int actual, char *code) {
  if (expected != actual) {
    printf("%d != %d; %s\n", expected, actual, code);
    
    // note: not calling abort here!
    // if you call abort, all the tests after the first one that fails say they fail too???
    // it's like it's caching that it crashed and just doesn't bother checking if the program changed after that??
    // i assume that's a macos specific complete garbage? what's going on?!
    exit(1);
  }
}

