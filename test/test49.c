//#include <stdio.h>

class X {
  int x_;
  static int x;
  static int foo() {return x;}
};

class Y : public X {
  static int y;
  static int bar() {return y;}
};

int main() {
  X::x;
  Y::x;
  Y::y;
  X::foo();
  Y::foo();
  Y::bar();
  X x;
  x.x;
  x.foo();
  Y y;
  y.x;
  y.y;
  y.foo();
  y.bar();
}
