
class Parent {
  int x;
  int y;
  int f0() {return x;}
};

class Child : public Parent {
  int z;
  int f1() {return z;}
  int f2() {return z + f0();}
  int f3() {return z + x;}
};

class SuperChild : public Child {
  int z2;
  int f4() {return z2;}
  int f5() {return z2 + f0() + f1();}
  int f6() {return z2 + z + x;}
};

void foo_p(Parent * p) {}
void foo_c(Child * p) {}
void foo_s(SuperChild * p) {}

int main() {
  Parent p;
  foo_p(&p);
  Child c;
  foo_c(&c);
  foo_p(&c);
  SuperChild s;
  foo_s(&s);
  foo_c(&s);
  foo_p(&s);

  p.x;
  p.y;
  p.f0();

  c.z;
  c.x;
  c.y;
  c.f0();
  c.f1();
  c.f2();
  c.f3();

  s.z2;
  s.z;
  s.x;
  s.y;
  s.f0();
  s.f1();
  s.f2();
  s.f3();
  s.f4();
  s.f5();
  s.f6();

  return 0;
}
