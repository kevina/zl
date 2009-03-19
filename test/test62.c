
int main() {
  int x = 20;
  int * p = &x;
  int * & pr = p;
  *pr = 30;
  pr++;
  return 30;
}
