struct X {
  int num;
};

user_type X {
  static int last_num = 0;
  finalize_user_type struct X;
  macro num(:this this = this) {(*this)..num;}
  void constructor(X * this) {
    num = last_num++;
    printf("HELLO %d\n", num);
  }
  macro _constructor(:this this) {constructor(this);}
  void destructor(X * this) {
    printf("BY BY %d\n", num);
    num = -1;
  }
  macro _destructor(:this this = this) {destructor(this);}
  void copy_constructor(X * this, const X & x) {
    num = last_num++;
    printf("COPY %d = %d\n", num, x.num);
  }
  macro _copy_constructor(lhs, :this this  = this) {copy_constructor(this, lhs);}
}

int main() {
  X x;
  const int & n = x.num;
  printf("%d\n", n);
  return 0;
}
