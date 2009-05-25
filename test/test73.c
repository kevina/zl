int main() {
  int x = 1;
  macro foo(y) {
    x + y;
  }
  return foo(2);
}
