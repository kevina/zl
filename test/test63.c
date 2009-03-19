class X {
  int x_;
  virtual void foo() {printf("foo\n");}
};

class Y : public X {
  int y_;
  virtual void bar() {printf("bar\n");}
};

int main() {
  X xo; 
  X & x = xo;
  x.x_;
  x.foo();
  Y yo;
  Y & y = yo;
  y.foo();
  y.bar();
}
