#include "../../bindings/chibicc/chibicc.h"

StringArray include_paths;
bool opt_fcommon = true;
bool opt_fpic;
char *base_file;

bool file_exists(char *path) {
  struct stat st;
  return !stat(path, &st);
}
