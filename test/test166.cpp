void foo(const int * const & bla) {
}

void foo2(const void * const & bla) {
}

int main() {
  int * x0;
  const int * x1;
  foo(x0);
  foo(x1);
  foo2(x0);
  foo2(x1);
}
