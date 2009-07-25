
make_inner_ns myns;

Syntax * add_namespace(Syntax * syn, Environ * env) {
  Mark * mark = new_mark();
  Match * m = match_args(0, syntax(ns, @rest), syn);
  SyntaxEnum * i = partly_expand_list(m->varl(syntax rest), StmtDeclPos, env);
  SyntaxList * l = new_syntax_list();
  Syntax * s;
  while ((s = i->next())) {
    Match * m0 = match(m, syntax ns, reparse(m->var(syntax ns), "RAW_ID", env));
    m0 = match(m0, raw_syntax(what name @rest), s);
    l->append(replace(raw_syntax((mid what) (` (mid name) (mid ns)) (mid @rest)), m0, mark));
  }
  return l;
}

make_macro add_namespace;

add_namespace(myns, int x;, int f() {return 0;});

int main() {
  x`myns = f`myns();
}
