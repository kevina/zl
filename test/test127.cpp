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
  Z() {printf("Z DEFAULT CONSTRUCTOR\n");}
  Z(const Z &) {printf("Z COPY CONSTRUCTOR\n");}
  ~Z() {printf("Z DESTRUCTOR\n");}
  void operator=(const Z &) {printf("Z ASSIGN\n");}
};

int main() {
  printf("---\n");
  Z z0;
  printf("---\n");
  Z z1 = z0;
  printf("---\n");
  z1 = z0;
  printf("---\n");
  return 0;
}
