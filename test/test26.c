
int x = 20;

int foo(int x, int y) {
  {
    int x = 20;
    if (0) return x + y;
    else goto x;
  }
  return x + y;
  int x = 2;
 x:
  return 10;
}

typedef struct s1 {
  int x;
  int y;
} s1;

int main() {
  typedef struct s2 {
    int x;
    int y;
  } s2;
  s1 v1;
  s2 v2;
  return v1.x + v2.y + foo(2,3);
}

