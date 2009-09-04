class X {
  virtual void foo() {printf("X\n");}
};

class Y : public X {
  void foo() {printf("Y\n");}
};

int main() {
  X * x = new Y;
  x->foo();
  delete x;
  return 0;
}
