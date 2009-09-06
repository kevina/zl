struct X {
  int num;
};

user_type X {
  static int last_num = 0;
  finalize_user_type struct X;
  macro num(:this ths = this) {(*ths)..num;}
  void constructor(X * fluid this) {
    num = last_num++;
    printf("HELLO %d\n", num);
  }
  macro _constructor(:this ths) {constructor(ths);}
  void destructor(X * fluid this) {
    printf("BY BY %d\n", num);
  }
  macro _destructor(:this ths = this) {destructor(ths);}
  void copy_constructor(X * fluid this, const X & x) {
    num = last_num++;
    printf("COPY %d = %d\n", num, x.num);
  }
  macro _copy_constructor(lhs, :this ths  = this) {copy_constructor(ths, lhs);}
}

const X & x = X();

int main() {
  const X & x = X();
  return 0;
}
