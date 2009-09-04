class X {
  int x_;
  virtual void foo() {printf("foo\n");}
};

class Y : public X {
  int y_;
  virtual void bar() {printf("bar\n");}
};

int main() {
  X x; 
  x.x_; 
  x.foo(); 
  Y y; 
  y.foo(); 
  y.bar(); 
  return 0;
}
