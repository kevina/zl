#include <stdio.h>

int main() {
  int x = 0;
  ({x;})= 20;
  printf("%d\n", x);
}
