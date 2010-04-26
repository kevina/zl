#include <stdio.h>

enum Foo {A, B = 9, C};


int main() {
  Foo foo = B;
  printf("%d\n", foo);
  return 0;
}
