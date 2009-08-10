class X {
  int x;
  X() {}
  X(const X &) {}
  ~X() {}
  void operator=(const X &) {}
};

class Y {
  int i;
  X x;
  int main() {}
};
