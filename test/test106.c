
class X {
  int x;
  void dump_x() {
    printf("X: %d\n", x);
  }
};

class Y : public X {
  int y;
  void dump_y() {
    printf("Y: %d\n", y);
  }
};

int main() {
  Y y;
  y.y = 10;
  y.x = 20;
  printf("%d %d\n", y.y, y.x);
  y.dump_y();
  y.dump_x();
}
