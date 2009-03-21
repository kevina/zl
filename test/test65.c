
// Note this test requires modification of the grammer
// STMT = 
//     ...
//     / <foreach> "foreach" "(" {ID} "in" {EXP} ")" {STMT}
//     ...


smacro foreach (VAR, WHAT, BODY) {
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
  foreach (y in con) {
    printf("%p", x);
  }
}
