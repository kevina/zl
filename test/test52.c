
module M {
  export S, S`tag, M0`outer;
  struct S {
    int x;
  };
  typedef struct S S;
  module M0 {
    export S0, S0`tag; 
    struct S0 {
      int y;
    };
    typedef struct S0 S0;
  }
  M0::S0 x;
  struct M0::S0 y;
}

M::S x0;
struct M::S s0;

M::M0::S0 x1;
struct M::M0::S0 s1;

int main() {
  x0.x;
  s1.y;
  s0.x;
  s1.y;

  import M::M0;
  S0 x;
  struct S0 m;
  x.y;
  m.y;
}

