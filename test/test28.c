
int x = 10;

map gen() : (foo) {
  int y = x;
  map foo (v) {
    v = x + y;
  }
}

int y = 30;

gen();

int main() {
  int x = 20;
  foo(x);
}
