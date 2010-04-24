
#include <stdio.h>

class X {
public:
  int x;
  void operator=(const X &);
};

class Y {
public:
  X x;
};

class Z {
public:
  X x;
  void operator=(const Z &);
};
  
void X::operator=(const X &) {
  printf("ASSIGN X\n");
}

void Z::operator=(const Z &) {
  printf("ASSIGN Z\n");
}

int main() {
  X x0,x1;
  Y y0,y1;
  Z z0,z1;
  x0 = x1;
  y0 = y1;
  z0 = z1;
  return 0;
}
