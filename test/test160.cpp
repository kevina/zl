#include <stdio.h>

class X {
  int dummy;
  void foo() const;
  void foo2() const {}
  virtual void foo3() const;
  virtual void foo4() = 0;
  virtual void foo5() const = 0;
};
