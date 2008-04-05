
make_inner_ns priv;

int main() {
  int x`priv = 20;
  int x = 30;
  x`priv = x;
  return x`priv;
}
