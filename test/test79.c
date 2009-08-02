
int foo(const int & x) {
  return x;
}

const int & X = foo(22) + foo(34);

int main() {
  printf("%d\n", X);
}
