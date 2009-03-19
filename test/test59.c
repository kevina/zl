
int main() {
  int x = 20;
  int & xr = x;
  int y = 30;
  int & yr = y;
  xr = 15;
  yr++;
  xr += 3;
  printf("%d\n", xr + yr);
  return 0;
}
