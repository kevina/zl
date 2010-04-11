
class X {
  int d;
} x;
typedef class X X;

X x2;

void foo(class Y * y) {
  Y * y2;
}

int main() {
  Y * yp;
  foo(yp);
}
