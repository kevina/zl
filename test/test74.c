extern "C" double fabs(double x);

double r = 1.61803399;

Syntax * make_golden(Syntax * syn, Environ * env) { 
  Mark * mark = new_mark(); 
  Match * m = match_f(0, syntax (D,A,B,FIX), syn); 
  UnmarkedSyntax * r = syntax { 
    for (;;) { double a = A, b = B; 
               double D = (a - r*b)/(1 + r); 
               if (fabs(D/(a+b)) > 0.01) FIX; else break; } 
  }; 
  return replace(r, m, mark); 
} 
make_macro make_golden; 

int main() {
  double q = 3, r = 2;

  make_golden(a, q, r, {q -= a; r += a;});

  printf("%f %f\n", q, r);

  return 0;
}
