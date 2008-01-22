void foo();
void foo2();

int main() {
  int x = 0;
  while (x != 20 && (x != 30 || x != 40)) 
    x += 1;
  for (int i = 9, j = 11;;) {
    x = i + x;
  }
  x |= 3;
}
