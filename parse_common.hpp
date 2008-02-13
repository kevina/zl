#ifndef PARSE_COMMON__HPP
#define PARSE_COMMON__HPP

#include "asc_ctype.hpp"

// defined in parse.cpp

namespace parse_common {

  static inline const char * spacing(const char * str, const char * end) {
    while (str != end) {
      if (asc_isspace(*str)) ++str;
      else if (*str == '#') {++str; while (str != end && *str != '\n') ++str;}
      else break;
    }
    return str;
  }

  static inline const char * spacing(SubStr str) {
    return spacing(str.begin, str.end);
  }

  const char * id(const char * str, const char * end, String & res);
 
  const char * quote(char close, const char * str, const char * end, SubStr &);

  void unescape(const char *, const char *, StringBuf & out, char quote = '\0');
  static inline String unescape(const char * b, const char * e, char quote = '\0') {
    StringBuf buf;
    unescape(b, e, buf, quote);
    return buf.freeze();
  }
  static inline String unescape(SubStr str, char quote = '\0') {
    return unescape(str.begin, str.end, quote);
  }
 
  const char * symbol(char sym, const char * str, const char * end);
    
  const char * require_symbol(char sym, const char * str, const char * end);
}

#endif
