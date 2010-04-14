
#include <stdio.h>

void foo(float x) {
  printf("FLOAT\n");
}
void foo(char x) {
  printf("CHAR\n");
}
void foo(int x) {
  printf("INT\n");
}

int main() {
  foo((char)0);
  foo((int)0);
  foo((float)0.0);
  foo((short)0);
  return 0;
}
