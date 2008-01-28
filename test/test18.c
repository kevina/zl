
enum E {C0, C1, C3 = 3, C4};

int main() {
  enum E e = C0;
  enum E e1 = C1 + 1;
  return e1;
}
