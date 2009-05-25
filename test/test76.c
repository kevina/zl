// This should fail with "Container lacks begin or end method."

Syntax * foreach (Syntax * syn, Environ * env) {
  Mark * mark = new_mark();
  Match * m = match(0, syntax(_,VAR,WHAT,BODY), syn);
  Syntax * what = match_var(m, syntax WHAT);
  if (!symbol_exists(syntax begin,what,mark,env) || 
      !symbol_exists(syntax end,what,mark,env))
    return error(what,
                 "Container lacks begin or end method.");
  UnmarkedSyntax * repl = syntax {
    typeof(WHAT) & what = WHAT;
    typeof(what.begin()) i = what.begin(), e = what.end();
    for (;i != e; ++i) {typeof(*i) & VAR = *i; BODY;}
  };
  return replace(repl, m, mark);
}
make_macro foreach;

class X {
  int * data;
};

int main() {
  X x;
  foreach(el, x, {printf("%d\n", el);});
}
