
macro gen(m1, m2) : (S, init_s, use_s, foo) {
  struct S {
    int m1;
    int m2;
    int priv;
  };
  void init_s(struct S * s) {
    s->m1 = 0;
    s->m2 = 0;
    s->priv = 20;
  }
  void use_s(struct S s) {
    s.m1 + s.m2 + s.priv;
  }
  macro foo(p) {
    p.mine + p.priv;
  }
}

macro foo2(p) {
  p.mine + p.priv;
}

gen(mine, priv);

int main() {
  struct S s;
  init_s(&s); 
  use_s(s);
  foo(s);
  foo2(s);

  struct T {
    int mine;
    int priv;
  }
  struct T t;
  foo(t);
  foo2(t);
}
