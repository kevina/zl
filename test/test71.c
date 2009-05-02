
macro or(x, y) {
  ({typeof(x) t = x; t ? t : y;});
}

int main() {
  float x = or(0.0, 6.8);
  float y = 0.0 || 6.8;
  printf("%f %f\n", x, y);
}
