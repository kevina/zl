
typedef const int * const INTP ;

int i;
const INTP x = &i;
int y[9];

int main() {
  x[4];
  y[8];
}

