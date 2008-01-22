
int f0() {
  return 0;
}

int f1(int x) {
  return x * 2;
}

int f2(int x, int y) {
  return x + y;
}

int main() {
  return f0() + f1(1) + f2(2, 3);
}
