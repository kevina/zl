#include <stdio.h>

char str[] = "abc";
char str2[4] = "def";

int main() {
  printf("%d %d\n", sizeof(str), sizeof(str2));
  printf("%s %s\n", str, str2);
  return 0;
}
