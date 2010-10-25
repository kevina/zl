
void f(int);

class X {
  void (*fp)(int);
};

int main() {
  X x;
  x.fp = f;
  x.fp(20);
}
