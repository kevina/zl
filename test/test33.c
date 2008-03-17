
map gen(z) : (f) {
  enum E {X, Y, z};
  enum E foo() {
    return X + Y + z;
  }
  int f() {
    return foo();
  }
}

gen(Z);

int main() {
  return f() + Z;
}

