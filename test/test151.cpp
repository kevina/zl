#include <stdio.h>

struct OStream {
  FILE * unused;
};

OStream & operator<< (const OStream &, int d) {
  printf("d:%d ", d);
}

OStream & operator<< (const OStream &, unsigned d) {
  printf("u:%u ", d);
}

OStream & operator<< (const OStream &, const char * str) {
  printf("%s", str);
}

OStream COUT;

int main() {
  COUT << 20 << "abc\n";
  unsigned a = 1;
  short b = 2;
  int c = 3;
  unsigned short d = 4;
  COUT << a << b << c << d << "\n";
  return 0;
}


