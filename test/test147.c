int f() {}
int g(int) {}

int main() {
  int x;
  if (x = f(), g(x)) {}
}
