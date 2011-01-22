struct MyStringObj;

extern "C" MyStringObj * my_to_external_name(Symbol * sym) {
  return NULL;
}

struct MyMangler {
  const char * abi_name;
  MyStringObj * (*mangler)(Symbol *);
};

unsigned _manglers_size = 1;
MyMangler _manglers[] = {{"gcc", my_to_external_name}};

