#ifndef parse_decl__hpp
#define parse_decl__hpp

#include "parse.hpp"

namespace ast {struct Environ;}
using ast::Environ;

class ParseDecl {
public:
  virtual const Syntax * parse_decl(const Syntax * p, Environ &) = 0;
  virtual const Syntax * parse_type(Parts::const_iterator & i, Parts::const_iterator e, Environ &) = 0;
  virtual const Syntax * parse_type(const Syntax * p, Environ &) = 0;
  virtual void init() = 0;
  virtual ~ParseDecl() {}
};

ParseDecl * new_parse_decl();

extern ParseDecl * parse_decl_;

#endif
