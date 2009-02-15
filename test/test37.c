
fluid_binding z;

macro f(body) {
  int fluid z = 20;
  body;
}

/*
macro f(body) : (z) {
  int z = 20;
  body;
}
*/

macro g(body) {
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
