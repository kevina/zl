struct X {
  int dummy;
};

user_type X {
  finalize_user_type struct X;
  void _destructor() {
    printf("BY BY\n");
  }
}

X x;

int main() {
  printf("HELLO WORLD\n");
}
