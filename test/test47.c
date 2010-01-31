
class Parent {
  int x;
  int y;
};

class Child : public Parent {
  int z;
};

class SuperChild : public Child {
  int z2;
};

void foo_p(Parent * p) {}
void foo_c(Child * p) {}
void foo_s(SuperChild * p) {}

int main() {
  //Parent p;
  //foo_p(&p);
  Child c;
  //foo_c(&c);
  foo_p(&c);
  //SuperChild s;
  //foo_s(&s);
  //foo_c(&s);
  //foo_p(&s);

  //p.x;
  //p.y;

  //c.z;
  //c.x;
  //c.y;

  //s.z2;
  //s.z;
  //s.x;
  //s.y;

  return 0;
}
