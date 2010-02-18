#include <stdio.h>

class X {
public:
  int x;
  X() {printf("X DEFAULT CONSTRUCTOR\n");}
  X(const X &) {printf("X COPY CONSTRUCTOR\n");}
  ~X() {printf("X DESTRUCTOR\n");}
  void operator=(const X &) {printf("X ASSIGN\n");}
};

int main() {
  X x0;
  X x1 = x0;
  x1 = x0;
  return 0;
}
