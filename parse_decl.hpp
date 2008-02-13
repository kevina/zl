#ifndef parse_decl__hpp
#define parse_decl__hpp

#include "parse.hpp"

namespace ast {struct Environ;}
using ast::Environ;

class ParseDecl {
public:
  virtual const Parse * parse_decl(const Parse * p, Environ &) = 0;
  virtual const Parse * parse_type(const Parse * p, Environ &) = 0;
  virtual void init() = 0;
  virtual ~ParseDecl() {}
};

ParseDecl * new_parse_decl();

extern ParseDecl * parse_decl_;

#endif
