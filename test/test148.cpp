#include <stdio.h>

class X {
public:
  int x;
  bool val() {return true;}
  ~X() {printf("By by X\n");}
};

int main() {
  if (X().val()) {
    printf("Yeah\n");
  }
  return 0;
}
