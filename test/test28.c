
int x = 10;

macro gen() : (foo) {
  int y = x;
  macro foo (v) {
    v = x + y;
  }
}

int y = 30;

gen();

int main() {
  int x = 20;
  foo(x);
}
