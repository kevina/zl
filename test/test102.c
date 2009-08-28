
module M {
  macro foo() {x;}
  int x;
}

int main() {
  M::foo = 20;
  printf("%d\n", M::foo);
}
