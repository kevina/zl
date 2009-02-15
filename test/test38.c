
macro foo1(:p p = 20) {
  p;
}

macro foo2(p = 20) {
  p;
}

macro foo3(q, p = 20) {
  q + p;
}

int main() {
  foo1(:p 30);
  foo1(:p = 30); // same as foo(:p 30)
  foo2(30);
  foo1();
  foo2();
  foo3(10);
  foo3(10, 30);
}
