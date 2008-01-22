
int x0;
int * y1;
static const int * x2 = &x0, y2 = 4, * const z3 = &x0;
int x3[30];
int (*x4)[30], y4;
extern int f5();
static inline int f6(int x6, int);
int (*f7)(int x7, int);
int (*f8)();
int (*f9[30])();
int x10[20][30];
unsigned x11[20];
float (*f12(int x))();
int * f13();
typedef unsigned int t14;
t14 x15;
const t14 x16 = 160;
typedef const t14 t17;
t17 x18 = 180;

//float f = f12()(23); ERROR
int main() {
  float f = f12(23)();
  x3[5];
}
