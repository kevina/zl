class X {
  int x;
  X() {}
  X(const X &) {}
  virtual ~X() {}
  void operator=(const X &) {}
};

class Y : public X {
  int i;
  int main() {}
};

class Z {
  X x;
  Y y;
};
