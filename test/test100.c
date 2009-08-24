
class X {
  int num;
  X() {printf("HELLO X\n");}
  X(const X & o) {num = o.num; printf("COPY X\n");}
  ~X() {printf("BY BY X\n");}
};

int main() {
  X x = ({X t1 = ({X t2; t2.num=20; t2;}); t1.num += 30; t1;});
  printf("%d\n", x.num);
}
