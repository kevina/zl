#include <stdio.h>

class X {
public:
  int d;
  virtual void foo() {printf("X FOO\n");}
  virtual void foo(int x) {printf("X FOO %d\n", x);}
};

class Y : public X {
public:
  void foo() {printf("Y FOO\n");}
  void foo(int x) {printf("Y FOO %d\n", x);}
};

int main() {
  { X x;
    x.foo();
    x.foo(10);}
  printf("---\n");
  { Y y;
    y.foo();
    y.foo(10);}
  printf("---\n");
  { X * x = new X;
    x->foo();
    x->foo(10); }
  printf("---\n");
  { Y * y = new Y;
    y->foo();
    y->foo(10);
    X * x = y;
    x->foo();
    x->foo(10); }
  printf("---\n");
  { X * x = new Y;
    x->foo();
    x->foo(10); }
  return 0;
}
