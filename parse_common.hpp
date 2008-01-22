#ifndef PARSE_COMMON__HPP
#define PARSE_COMMON__HPP

// defined in parse.cpp

namespace parse_common {

  static inline const char * spacing(const char * str, const char * end) {
    while (str != end) {
      if (isspace(*str)) ++str;
      else if (*str == '#') {++str; while (str != end && *str != '\n') ++str;}
      else break;
    }
    return str;
  }

  static inline const char * spacing(SubStr str) {
    return spacing(str.begin, str.end);
  }

  const char * id(const char * str, const char * end, String & res);

  const char * quote(char close, const char * str, const char * end, String & val);
  
  const char * symbol(char sym, const char * str, const char * end);
    
  const char * require_symbol(char sym, const char * str, const char * end);
}

#endif
