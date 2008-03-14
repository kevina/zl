
int main() {
  __label__ bla;
  int x = 20;
bla: 
  switch (x) {
  case 1:
    x = 10;
    break;
  default:
    break;
  }
  goto bla;
}
