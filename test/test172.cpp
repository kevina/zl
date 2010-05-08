#include <stdio.h>

class Foo {
public:
  int x;
  Foo(int x) :x(x) {printf("SETTING %d\n", x);}
  ~Foo() {printf("BY BY FOO, WAS %d\n", x); x = -1;}
};

int foo(int x, const Foo &f) {
  printf("foo %d\n", f.x);
}

int main() {
  ({ Foo ff(10); foo(20,ff);});
  foo(20, Foo(10));
  return 0;
}
