
fluid_binding z;

map f(body) {
  int fluid z = 20;
  body;
}

/*
map f(body) : (z) {
  int z = 20;
  body;
}
*/

map g(body) {
  f({
      z = 40;
      body;
      {
        int fluid z = 50;
        body;
      }
      {
        int z = 60;
        body;
      }
      body;
    });
  z = 60;
}

int main() {
  g({printf("%d", z);});
}
