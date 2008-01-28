
// this test is to check for unnecessary casts

typedef double DOUBLE;

int main() {
  const double x = 20;
  DOUBLE y = 21;
  double z = x + y + 1; // x and y should not need a cast in this exp
}
