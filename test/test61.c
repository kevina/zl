
macro foreach (VAR, WHAT, BODY) {
  typeof(WHAT) & what = WHAT;
  typeof(what.begin()) i = what.begin();
  typeof(what.end())   e = what.end();
  while (i != e) {
    typeof(*i) & VAR = *i;
    BODY;
    ++i;
  }
}

class Container {
  typedef int * iterator;
  iterator begin() {return NULL;}
  iterator end() {return NULL;}
};

int main() {
  Container con;
  foreach(x, con, {printf("%p", x);});
}
