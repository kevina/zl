
int foo(const int & x) {
  return x;
}

const int & X = 34;
const int & Y = foo(X);

int main() {
  int x = foo(22) + foo(34);
  const int & xr1 = 22;
  const int & xr2 = foo(22) + foo(34);
  printf("%d %d %d %d %d\n", x, xr1, xr2, X, Y);
}

