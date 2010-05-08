include_file "../c++-include/vector.zlh";

mk_vector(int *,);

int main() {

  vector<int *> vi;

  vi.size();
  vi.empty();
  
  vi.push_back((int *)NULL);

  vector<int *>::iterator i;

  vi[0];

}

