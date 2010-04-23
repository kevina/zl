
#include <stdio.h>

class X {
public:
  int x;
  X() {x = 20;}
  ~X() {printf("BYBY X %d\n", x);}
};

class Y : public X {
public:
  X y;
  Y();
  ~Y();
};

Y::Y() {
  printf("HELLO Y\n");
  y.x = 30;
}

Y::~Y() {
  printf("BYBY Y\n");
}

int main() {
  Y y;
  return 0;
}
