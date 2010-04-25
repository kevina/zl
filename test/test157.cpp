#include <stdio.h>

class X {
public:
  int x;
  X(int x0) : x(x0) {}
};

class Y : public X {
public:
  int y;
  Y() : X(10), y(20) {}
};

int main() {
  Y x;
  printf("%d %d\n", x.x, x.y);
  return 0;
}

