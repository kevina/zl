
typedef int INT2[2];

void bar(int is[2]) {}

void foo(INT2 is) {}

int main() {
  int * ip;
  bar(ip);
  foo(ip);
}
