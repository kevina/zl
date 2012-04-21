struct X {
  int dummy;
};

user_type X {
  finalize_user_type struct X;
  void _destructor() {
    printf("BY BY\n");
  }
}

const X & x = X();

int main() {
  const X & x = X();
}
