
int main() {
  int * x = new int;
  *x = 20;
  printf("%d\n", *x);
  delete x;
  return 0;
}
