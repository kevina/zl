
module N (z) {
  int z = 20;
}

map foo(var) {
  module M (var, priv) {
    int var = 20;
    int priv = 30;
  }
  printf("%d %d %d %d\n", N::z, M::var, M::priv, M::x);
  import M;
  import N;
  printf("%d %d %d\n", z, var, priv);
}


int main() {
  foo(x);
  return x;
  //return z; // this should fail
}
