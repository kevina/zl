
int global = 20;

int & f() {
  return global;
}

void g(int & x) {
  x = 20;
}

int main() {
  f() = 20;
  int x;
  g(x);
  return 0;
}
