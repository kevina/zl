module M {
  export g, h;
  void g() {
    import M;
    f();
  }
  void f() {}
  void h() {
    import M;
    f();
  }
}

int main() {
  import M;
  g();
  h();
}
