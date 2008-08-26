struct Foo {
  int x;
};

module Foo {
  export x;
  map x(:this this = this) {(*this)..x;}
}

make_user_type Foo;

int main() {
  Foo foo;
  foo.x();
  foo..x;
}

