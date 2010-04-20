
#include <stdio.h>

class X {
public:
  int x;
  int f();
  static int Y;
  static int f(int);
};

int X::f() {return x;}

static int X::f(int i) {return i + 10;}

int X::Y = 98;

int main() {
  X x;
  x.x = 39;
  printf("%d %d %d\n", x.f(), x.f(11), x.Y);
  return 0;
}
