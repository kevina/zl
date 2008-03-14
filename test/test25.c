
int main() {
  int x;
  if (0)
    goto inner;
  else
    goto outer;
  {
  .loop:
    {
    inner: x = 20;
      goto loop;
    .loop: x = 15;
    }
    goto loop;
  }
 outer: x = 30;
}
