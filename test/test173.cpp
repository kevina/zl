#include <stdio.h>

class X {
public:
  int x;
  X(int x) : x(x) {}
};

int main() {
  X x(35);
  printf("%d\n", x.x);
}
