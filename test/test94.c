#include <stdio.h>

class X {
  char dummy;
 public:
  X() {printf("Hello X\n");}
  X(const X &) {printf("COPY X\n");}
  ~X() {printf("BY BY X\n");}
};

class Y : public X {
 public:
  Y() {printf("Hello Y\n");}
  Y(const Y &) {printf("COPY Y\n");}
  ~Y() {printf("BY BY Y\n");}
};

class Z {
 public:
  X x;
  Y y;
  Z() {printf("Hello Z\n");}
  Z(const Z &) {printf("COPY Z\n");}
  ~Z() {printf("BY BY Z\n");}
};

int main() {
  X();
  printf("===\n");
  Y();
  printf("===\n");
  Z();
  printf("===\n");
  Z z1;
  printf("---\n");
  Z z2 = z1;
  printf("---\n");
}

