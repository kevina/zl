#include <stdio.h>

class X {
public:
  int x;
};

class Z {
public:
  X x;
  int y;
};

int main() {
  Z z0;
  z0.x.x = 40;
  z0.y = 20;
  Z z1 = z0;
  printf("z1.x.x = %d\n", z1.x.x);
  printf("z1.y = %d\n", z1.y);
  z0.x.x = 50;
  z0.y = 30;
  printf("z1.x.x = %d\n", z1.x.x);
  printf("z1.y = %d\n", z1.y);
  z1 = z0;
  printf("z1.x.x = %d\n", z1.x.x);
  printf("z1.y = %d\n", z1.y);
  return 0;
}
