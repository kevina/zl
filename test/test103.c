
user_type X {
  struct D;
  finalize_user_type struct D;
}

user_type Y {
  struct D;
  macro up_cast(p) {({printf("up cast Y->X\n"); reinterpret_cast<X *>(p);});}
  macro down_cast(p) {({printf("down cast X->Y\n"); reinterpret_cast<Y *>(p);});}
  finalize_user_type struct D;
  make_subtype X up_cast down_cast;
}  

user_type Z {
  struct D;
  macro up_cast(p) {({printf("up cast Z->Y\n"); reinterpret_cast<Y *>(p);});}
  macro down_cast(p) {({printf("down cast Y->Z\n"); reinterpret_cast<Z *>(p);});}
  finalize_user_type struct D;
  make_subtype Y up_cast down_cast;
}  


int main() {
  X * x = NULL;
  printf("---\n");
  Y * y = (Y *)x;
  printf("---\n");
  Z * z = (Z *)x;
  printf("---\n");
  y = z;
  printf("---\n");
  x = z;
}
