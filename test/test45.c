
class X {
  int x;
  int f() {return 20;}
  int g() {return f();}
  int h0(int j) {return 20;}
  int h1(int j) {return x + j;}
  int y() {return this->x;}
  int z() {return this->g();}
};

int main() {
  X v;
  int x;
  x += v.f();
  x += v.g();
  x += v.h0(20);
  x += v.h1(20);
  x += v.y();
  x += v.z();
  return x;
}
