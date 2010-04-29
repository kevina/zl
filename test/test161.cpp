#include <stdio.h>

class X {
public:
  int dummy;
  int foo6(int x = 20) {
    return x;
  }
  double foo6(double x) {
    return x;
  }
  int foo7(int x, int y = 30) {
    return x + y;
  }
  int foo8(int x = 9, int y = 30) {
    return x + y;
  }
  void foo9(int x, ...) {}
};

int main() {
  X x;
  printf("%d %d\n", x.foo6(), x.foo6(34));
  printf("%f\n", x.foo6(34.1));
  printf("%d %d\n", x.foo7(100), x.foo7(100, 34));
  printf("%d %d %d\n", x.foo8(), x.foo8(100), x.foo8(100, 34));
  x.foo9(10);
  x.foo9(20, "abc");
  return 0;
}
