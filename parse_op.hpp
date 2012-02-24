
#include "syntax.hpp"


class ParseExp {
public:
  virtual const Syntax * parse(const Syntax * p, const char * list_is) = 0;
  virtual void init() = 0;
  virtual ~ParseExp() {}
};

ParseExp * new_parse_exp();

extern ParseExp * parse_exp_;
