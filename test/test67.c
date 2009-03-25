Syntax * parse_myclass(Syntax * p, Environ * env) {
  Mark * mark = new_mark();

  Match * m = match_args(0, raw_syntax (name @ (pattern ({...} @body)) :(fix_size fix_size) @rest), p);

  Syntax * body = m->var(syntax body);
  Syntax * fix_size_s = m->var(syntax fix_size);

  if (!body || !fix_size_s)
    return parse_class(p, env);

  size_t fix_size = ct_value(fix_size_s, env);

  m = match(m, syntax dummy_decl, replace(syntax {char dummy;}, NULL, mark));
  
  Environ * lenv = temp_environ(env);
  Syntax * r = partly_expand(replace(raw_syntax (class (mid name) ({...} (mid body) (mid dummy_decl)) (mid rest)), m, mark),
                             TopLevel, lenv);
  pre_parse(r, lenv);

  size_t size = ct_value(replace(syntax (offsetof(name, dummy)), m, mark), lenv);
  
  if (size == fix_size) {
    return replace(raw_syntax (class (mid name) ({...} (mid body) (mid rest))), m, mark);
  } else if (size < fix_size) {
    char buf[32];
    snprintf(buf, 32, "{char dummy[%u];}", fix_size - size);
    m = match(m, syntax buffer, replace(string_to_syntax(buf), NULL, mark));
    return replace(raw_syntax (class (mid name) ({...} (mid body) (mid buffer)) (mid rest)), m, mark);
  } else {
    return NULL;
    //return error("sizeof class larger than fix_size");
  }
}

make_syntax_macro class parse_myclass;

class X;

class X : fix_size(16) {
  int x;
  char c;
};

class Y : public X : fix_size(32) {
  int j;
};

int main() {
  printf("%d\n", sizeof(X));
  Y y;
  y.x;
  printf("%d\n", sizeof(Y));
}
