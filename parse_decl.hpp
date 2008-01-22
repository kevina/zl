#ifndef parse_decl__hpp
#define parse_decl__hpp

#include "parse.hpp"

struct ExpandEnviron;

class ParseDecl {
public:
  virtual const Parse * parse(const Parse * p, ExpandEnviron &) = 0;
  virtual void init() = 0;
  virtual ~ParseDecl() {}
};

ParseDecl * new_parse_decl();

extern ParseDecl * parse_decl_;

#endif
