
void f() {
  smacro or(x, y) {
    ({typeof(x) t = x; t ? t : y;});
  }
  float x = 0.0 || 6.8;
  printf("%f\n", x);
}

void g() {
  float x = 0.0 || 6.8;
  printf("%f\n", x);
}

int main() {
  f(); 
  g();
  return 0;
}

