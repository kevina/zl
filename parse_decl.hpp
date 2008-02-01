#ifndef parse_decl__hpp
#define parse_decl__hpp

#include "parse.hpp"

struct ExpandEnviron;

class ParseDecl {
public:
  virtual const Parse * parse_decl(const Parse * p, ExpandEnviron &) = 0;
  virtual const Parse * parse_type(const Parse * p, ExpandEnviron &) = 0;
  virtual void init() = 0;
  virtual ~ParseDecl() {}
};

ParseDecl * new_parse_decl();

extern ParseDecl * parse_decl_;

#endif
