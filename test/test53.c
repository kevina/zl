
module M {
  export S, S`tag, M`outer;
  struct S {
    int x;
  };
  typedef struct S S;
  module M {
    export S, S`tag;
    struct S {
      int y;
    };
    typedef struct S S;
  }
  M::S x;
  struct M::S y;
}

M::S x0;
struct M::S s0;

M::M::S x1;
struct M::M::S s1;

int main() {
  x0.x;
  s1.y;
  s0.x;
  s1.y;

  import M::M;
  S x;
  struct S m;
  x.y;
  m.y;
}

