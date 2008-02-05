struct X {
  char x;
  int y;
};

struct Y {
  char x;
};

enum Z {A, B, C};

int main() {
  int a[20];
  sizeof(int);
  sizeof(double);
  sizeof(struct X);
  sizeof(struct Y);
  sizeof(enum Z);
  int b[sizeof a  / sizeof(int)];
  int c[sizeof(a) / sizeof(int)];
  (int *) a;
  (void *) (int *) (a);
}
