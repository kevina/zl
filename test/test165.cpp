
class X {
  int dummy;
};

class Y : public X {
  int dummy;
};

void foo(X &);
void bar(X *);

int main() {
  X x;
  foo(x);
  bar(&x);
  Y y;
  foo(y);
  bar(&y);
}
