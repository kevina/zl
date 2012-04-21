
struct X {
  int x;
  int y;
  char a[10];
};

class Y {
  int x;
  int y;
  char a[10];
};

Syntax * f(Syntax * p, Environ * env) {
  Mark * mark = new_mark();
  size_t v1 = ct_value(replace(syntax (offsetof(struct X, y)), NULL, mark), env);
  size_t v2 = ct_value(replace(syntax (offsetof(struct X, a)), NULL, mark), env);
  //size_t v3 = ct_value(replace(syntax (offsetof(struct X, a[1])), NULL, mark), env);
  size_t v4 = ct_value(replace(syntax (offsetof(Y, y)), NULL, mark), env);
  size_t v5 = ct_value(replace(syntax (offsetof(Y, a)), NULL, mark), env);
  //size_t v6 = ct_value(replace(syntax (offsetof(Y, a[1])), NULL, mark), env);
  char buf[64];
  snprintf(buf, 64, "\"%u %u %u  %u %u %u\"", v1, v2, 0, v4, v5, 0);
  return replace(string_to_syntax(buf), NULL, mark);
}

make_macro f;

int main() {
  const char * str = f();
  printf("%s\n", str);
  return 0;
}
