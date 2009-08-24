#include <stdio.h>

/* this test is maily on the order of evaluation, which right now will
   produce the bizarre behavior of
     foo: 1
     foo: 2
     foo: 3
     foo: 8
     foo: 4
     foo2: 4 8
   which I belve is still compient with C++ seq rules but not entirely sure
*/

int foo2(int x, int y) {
  printf("foo2: %d %d\n", x, y);
  return x + y;
}

int foo(int x) {
  printf("foo: %d\n", x);
  return x;
}

int main() {
  foo2(({(foo(1), foo(2)); foo(3), foo(4);}), foo(8));
}
