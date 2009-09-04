user_type M {
  struct _data;
  finalize_user_type struct _data;
  add_prop foo (12);
}

Syntax * get_prop(Syntax * syn, Environ * env) {
  Mark * mark = new_mark();
  Match * m = match_args(0, syntax (where, what), syn);
  m = match_args(m, syntax (where), partly_expand(m->var(syntax where), ExpPos, env));
  m = match_args(m, syntax (what), partly_expand(m->var(syntax what), ExpPos, env));
  Syntax * res = get_symbol_prop(m->var(syntax where), m->var(syntax what), env);
  return res;
}

make_macro get_prop;

int main() {
  printf("%d\n", get_prop(M, foo));
  return 0;
}
