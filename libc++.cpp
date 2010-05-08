
#include <iostream>
#include <sstream>

#include <stdlib.h>

fstream::fstream() : file(NULL) {}

fstream::fstream(FILE * f) : file(f) {}

void fstream::write(const char * str, int len) {
  if (len == -1) 
    fprintf(file, "%s", str);
  else
    fwrite(str, len, 1, file);
}

void fstream::write_int(int i) {
  fprintf(file, "%d", i);
}

fstream cout(stdout);
fstream cerr(stderr);
fstream cin(stdin);

void ostringstream::write(const char * str, int len = -1) {
  if (len == -1) len = strlen(str);
  buf.append(str, len);
}

void ostringstream::write_int(int i) {
  char s[32];
  snprintf(s, 32, "%d", i);
  buf.append(s, strlen(s));
}
