
Syntax * foo(Syntax * syn, Environ * env) {
  Mark * mark = new_mark();
  Match * m = match(0, syntax (t), syn);
  const Syntax * s = replace(syntax{sizeof(t);}, m, mark);
  size_t sz = ct_value(s, env);
  printf("::: %u\n", sz);
  return replace(syntax{0;}, 0, new_empty_mark());
}

make_macro foo;

struct X {
  int x;
}

int main() {
  foo(struct X, 9);
}
