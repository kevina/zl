
#include "parse.hpp"


class ParseExp {
public:
  virtual const Parse * parse(const Parse * p) = 0;
  virtual void init() = 0;
  virtual ~ParseExp() {}
};

ParseExp * new_parse_exp();

extern ParseExp * parse_exp_;
