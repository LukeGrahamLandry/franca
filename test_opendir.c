#include <dirent.h>
#include <stdio.h>
int main() {
  struct dirent entry;
  DIR *dir = opendir(".");
  struct dirent *out = &entry;
  while (1) {
    readdir_r(dir, &entry, &out);
    if (out == 0) {
      closedir(dir);
      break;
    };
    printf("%d; %s\n", out->d_namlen, out->d_name);
  };

  return 0;
}
