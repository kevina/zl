class X {
  virtual void foo() {printf("X\n");}
};

class Y : public X {
  virtual void foo() {printf("Y\n");}
};

int main() {
  X x;
  Y y;
  x.foo();
  y.foo();
  X * xp = & y;
  xp->foo();
}
