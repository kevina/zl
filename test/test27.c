
int x = 10;

map foo (v) {
  v = x;
}

int main() {
  int x = 20;
  foo(x);
}
