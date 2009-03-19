class X {
  virtual void foo() {printf("X\n");}
};

class Y : public X {
  virtual void foo() {printf("Y\n");}
};

int main() {
  X xo;
  X & x = xo;
  Y yo;
  Y & y = yo;
  x.foo();
  y.foo();
  X * xp = & y;
  xp->foo();
}
