
macro times2 (x) {
  x *= 2;
}

macro stmts (x, y) {{
  x *= 20;
  y *= 30;
}}

macro decl0 (t, v) {
  t zzz = 0;
  zzz = 20;
  t v = zzz;
}

macro loop (body) {
  for (;;) body;
}

macro make_macro (name, op) {
  macro name (x) {
    x op x;
  }
}

make_macro (double_, +);
make_macro (square_, *);

int main() {
  int x = 0;
  int zzz;
  decl0(int, y);
  y += 1;
  y = times2(y) + 4;
  //y = decl0(int, z);
  loop (x+1);
  if (x)
    stmts(x,y);
  else 
    times2(y);
  zzz = double_(5);
  zzz = square_(2+3);
  double_(square_(y));
}
