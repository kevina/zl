
class X {
  int x;
  int f() {return 20;}
  int g() {return f();}
  int h(int j) {return x + j;}
  int y() {return this->x;}
  int z() {return this->g();}
};

int main() {
  X v;
  return /*v.x + */v.h(20) + v.y() + v.z();
}
