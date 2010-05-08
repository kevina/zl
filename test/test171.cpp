#include <iostream>
#include <string>
#include <sstream>
#include <stdlib.h>

using namespace std;

int main() {
  string x("abc");
  string y("def");
  string z1;
  z1 += x;
  z1 += y;
  string z2 = x + y;
  ostringstream str;
  str << x << y;
  string z3 = "ABC" + y + '?';
  cout << x << y << endl;
  cout << z1 << endl;
  cout << z2 << endl;
  cout << str.str() << endl;
  cout << z3 << endl;
  return 0;
}
