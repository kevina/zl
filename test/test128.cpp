#include <stdio.h>

class X {
public:
  int x;
  X(int x0) {x = x0;}
  X(int x0, int x1) {x = x0 + x1;}
};

int main() {
  X x1(10);
  X x2(10,10);
  X x3(x2);
  //X x4; this should fail
}
