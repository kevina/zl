#ifndef CHARSET_HPP
#define CHARSET_HPP

#include <string.h>

static const unsigned SET_SIZE = 256;

class CharSet {
  bool d[SET_SIZE];
public:
  CharSet() : d() {}
  CharSet & clear() {memset(d, 0, sizeof(d)); return *this;}
  CharSet & add(unsigned char c) {d[c] = true; return *this;}
  CharSet & add(unsigned char b, unsigned char e) {
    for (unsigned i = b; i <= e; ++i) d[i] = true;
    return *this;
  }
  CharSet & add(const CharSet & other) {
    for (unsigned i = 0; i != SET_SIZE; ++i) {
      if (other[i]) d[i] = true;
    }
    return *this;
  }
  CharSet & remove(unsigned char c) {d[c] = false; return *this;}
  CharSet & remove(unsigned char b, unsigned char e) {
    for (unsigned i = b; i != e; ++i) d[i] = false;
    return *this;
  }
  CharSet & remove(const CharSet & other) {
    for (unsigned i = 0; i != SET_SIZE; ++i) {
      if (other[i]) d[i] = false;
    }
    return *this;
  }
  CharSet & invert() {
    for (unsigned i = 0; i != SET_SIZE; ++i) {
      d[i] = !d[i];
    }
    return *this;
  }
  bool operator[] (unsigned char c) const {return d[c];}
};

static inline bool operator==(const CharSet & x, const CharSet & y) 
{
  for (unsigned i = 0; i != SET_SIZE; ++i)
    if (x[i] != y[i]) return false;
  return true;
}

extern CharSet alpha,digit,space,special,symbol;
extern const CharSet * backspace_map[SET_SIZE];

void init_charsets();

#endif
