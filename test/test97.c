int main() {
  int z = ({int x = 0; x += 2; x;});
  printf("%d\n", z);
}
