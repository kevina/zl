macro foreach (VAR, WHAT, BODY) {
  typeof(WHAT) & what = WHAT;
  typeof(what.begin()) i=what.begin(), e=what.end();
  for (; i != e; ++i) {
    typeof(*i) & VAR = *i;
    BODY;
  }
}

class X {
  int * data;
  size_t sz;
  int * begin() {return data;}
  int * end() {return data + sz;}
};

int main() {
  X x;
  int d[4] = {1, 5, 3, 2};
  x.data = d;
  x.sz = 4;
  foreach(el, x, {printf("%d\n", el);});
}
