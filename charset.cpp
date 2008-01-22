#include "charset.hpp"

CharSet alpha,digit,space,special,symbol;
CharSet newline;
const CharSet * backspace_map[SET_SIZE] = {0};

void init_charsets() __attribute__((constructor));

void init_charsets() {
  alpha.add('a', 'z').add('A', 'Z');
  digit.add('0','9');
  space.add(' ').add('\n').add('\t').add('\r');
  special.add('(').add(')').add('[').add(']').add('{').add('}');
  special.add('\'').add('"').add('`');
  special.add('_');
  newline.add('\n');
  symbol.add(32, 126).remove(alpha).remove(digit).remove(space).remove(special);
  backspace_map[(unsigned char)'a'] = &alpha;
  backspace_map[(unsigned char)'d'] = &digit;
  backspace_map[(unsigned char)'s'] = &space;
  backspace_map[(unsigned char)'y'] = &symbol;
  backspace_map[(unsigned char)'n'] = &newline;
}

