class X {
  int x;
  X(const X & o) {printf("COPY X\n");}
  ~X() {printf("KILL X\n");}
};

int main() {
  ({X x0;
    X x = X();});
}


