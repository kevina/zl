struct X {
  int dummy;
};

user_type X {
  finalize_user_type struct X;
  void _destructor() {
    printf("BY BY\n");
  }
}

int main() {
  X x;
  printf("HELLO WORLD\n");
}
