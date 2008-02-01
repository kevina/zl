
typedef struct X X;

int main() {
  struct X * x;
  int i = x->x;
}

struct X {
  int x;
  int y;
};

int main2() {
  X * x;
  int i = x->x;
}

