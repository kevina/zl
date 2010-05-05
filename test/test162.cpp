
class X {
  int dummy;
};

class Y : public X {
  int dummy;
};

void foo1(X & x);
void foo2(const X & x);

int main() {
  Y y1;
  const Y y2;
  foo1(y1);
  foo2(y1);
  foo2(y2);
}
