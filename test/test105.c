
struct X {
  int x;
};

void foo(struct X x);

int main() {
  __raw(var x (.tprop foo (parm 0)));
  x.x = 20;
  foo(x);
}
