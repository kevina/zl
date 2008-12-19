
Syntax * map(Syntax * syn, Environ * env) {
  Mark * mark = new_mark();
  Match * m = match_args(0, syntax (name, parms, repl), syn);
  Syntax * res = replace(syntax {
      Syntax * name(Syntax * syn, Environ * env) {
        Mark * mark = new_mark();
        Match * m = match_args(0, syntax parms, syn);
        Syntax * res = replace(syntax repl, m, mark);
        return res;
      }
      macro name;
    }, m, mark);
}

syntax_macro map;

int y = 30;

map foo (v,z) {
  int x = v + y; 
  z = x * x;
}

/* Expands to (ignoring marks):
 *
 * Syntax * foo(Syntax * syn, Environ * env) {
 *   Mark * mark = new_mark();
 *   Match * m = match_args(0, syntax (v, z), syn);
 *   m = match(m, syntax, replace_context(syntax, get_context(syn)));
 *   Syntax * res = replace(syntax {int x = v + y; z = x * x;}, m, mark);
 *   return res;
 * }
 * 
 * Which is the same as foo2:
 */

Syntax * foo2(Syntax * syn, Environ * env) {
  Match * m = match_args(0, syntax (v, z), syn);
  Syntax * res = replace( syntax {int x = v + y; z = x * x;}, m, new_mark());
  return res;
}

macro foo2;

int main() {
  int x,y,z;
  foo(x,z);
  foo2(z,z);
  return z;
}

/* EXPANDED OUTPUT:
 *
 * int main()
 * {
 *   int x$1;
 *   int y$1;
 *   int z$1;
 *   int x$2 = (x$1 + y);
 *   (z$1 = (x$2 * x$2));
 *   int x$3 = (z$1 + y);
 *   (z$1 = (x$3 * x$3));
 *   return z$1;
 * }
 */

