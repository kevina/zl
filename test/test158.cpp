struct Y * yp;

struct Y {
  struct X * x;
  struct X2;
  struct X2 * x2;
  struct Z {
    X * x;
    struct X3 * x3;
  } z;
};

X * xp;

struct X {
  int x;
};

struct X3 {
  int x;
};

int main() {
   Y y;
   yp = &y;
   xp = y.x;
   X x = *y.x;
   Y::X2 * x2 = y.x2;
   x = *y.z.x;
   X3 x3 = *y.z.x3;
}
