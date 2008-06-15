
class Parent {
  int x;
  int y;
};

class Child {
  Parent parent;
  int z;
};

map child_to_parent (child) {
  &child->parent;
}

make_subtype Parent Child child_to_parent;

class SuperChild {
  Child parent;
  int z2;
};

make_subtype Child SuperChild child_to_parent;

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
  return 0;
}
