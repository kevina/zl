module M {
  export x, foo, INT;
  int x = 10;
  int foo() {return x;}
  typedef int INT;
}

int main() {
  import M;
  static INT y;
  module N {
    export x0, foo0, INT0;
    int x0 = 20;
    int foo0() {return y;}
    typedef int INT0;
  }
  import N;
  INT0 z = x;
  return x0 + foo0() + x + foo();
}
