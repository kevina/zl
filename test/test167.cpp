
class X {
  int x;
};

void foo(X & x, X & y);
void bar(X & x, X & y) {
  foo(x,y);
}

int main() {
}
