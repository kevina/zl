class X {
  int x;
  X() {}
  X(const X &) {}
  ~X() {}
  void operator=(const X &) {}
};

class Y {
  X x;
  virtual void foo() {}
};

