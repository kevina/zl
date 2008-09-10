
int f1(int);
int f2(int, int);
int f3(int, int, int);

map mycall (fun, @parms) {
  fun(1, parms);
}

int main() {
  mycall(f1);
  mycall(f2, 2);
  mycall(f3, 2, 3);
}
