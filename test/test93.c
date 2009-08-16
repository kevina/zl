#include <stdio.h>

class X {
 public:
  X() {printf("Hello X\n");}
  virtual ~X() {printf("BY BY X\n");}
};

class Y : public X {
 public:
  Y() {printf("Hello Y\n");}
  ~Y() {printf("BY BY Y\n");}
};

class Zb {
 public:
  virtual ~Zb() {}
};

class Z : public Zb {
 public:
  X x;
  Y y;
};

int main() {
  X();
  printf("---\n");
  Y();
  printf("---\n");
  Z();
  printf("---\n");
  X * x;
  x = new X;
  delete x;
  printf("---\n");
  x = new Y;
  delete x;
  printf("---\n");
  Zb * z = new Z;
  delete z;
}


