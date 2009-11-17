#ifndef PEG__HPP
#define PEG__HPP

#include "util.hpp"
#include "source_str.hpp"

void parse_peg(const char * fn);

struct Replacements;
const Syntax * parse_str(String what, SourceStr str, const Replacements * repls = 0);

#endif
