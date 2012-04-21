
macro gen(f1, f2) : (S, init_s) {
  struct S {
    int priv;
    int f1;
    int f2;
  };

  void init_s(struct S * s) {
    s->priv = 0;
    s->f1 = 1;
    s->f2 = 2;
  }
}

gen(x,y);

int main() {
  struct S s;
  init_s(&s);
}

