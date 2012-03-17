
class X {
  int x;
  int foo(int y = 10, int z = 30) {return x + y + z;}
};

int main() {
  X x;
  x.x = 20;
  printf("%d\n", x.foo());
  return 0;
}

