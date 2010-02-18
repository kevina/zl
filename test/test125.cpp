#include <stdio.h>

class X {
public:
  int x;
  X() {printf("X DEFAULT CONSTRUCTOR\n");}
  X(const X &) {printf("X COPY CONSTRUCTOR\n");}
  ~X() {printf("X DESTRUCTOR\n");}
  void operator=(const X &) {printf("X ASSIGN\n");}
};

class Z {
public:
  X x;
  int y;
};

int main() {
  Z z0;
  z0.y = 20;
  Z z1 = z0;
  printf("z1.y = %d\n", z1.y);
  z0.y = 30;
  printf("z1.y = %d\n", z1.y);
  z1 = z0;
  printf("z1.y = %d\n", z1.y);
  return 0;
}
