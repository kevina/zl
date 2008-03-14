
int main() {
  int x;
  if (0)
    goto inner;
  else
    goto outer;
  {
    __label__ loop;
  loop:
    {
      __label__ loop;
    inner: 
      {
        x = 20;
        goto loop;
      }
    loop: x = 15;
    }
    goto loop;
  }
 outer: x = 30;
}
