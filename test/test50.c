class X {
  int x_;
  virtual int foo() {}
};

class Y : public X {
  int y_;
  virtual int bar() {}
};

int main() {
  X x;
  x.x_;
  x.foo();
  Y y;
  y.foo();
  y.bar();
}
