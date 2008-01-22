
map times2 (x)
  x *= 2;

map decl0 (t, v)
  t v = 0;

int main() {
  decl0(int, y);
  y += 1;
  times2(y);
}

