#ifndef PEG__HPP
#define PEG__HPP

#include "util.hpp"
#include "source_str.hpp"

namespace ast {
  struct Environ;
  // defined in ast.cpp
  bool template_id(const Syntax * syn, Environ * env);
}

void parse_peg(const char * fn);

struct Replacements;
const Syntax * parse_prod(String what, SourceStr & str, 
                          ast::Environ * ast_env = NULL,
                          const Replacements * repls = NULL, 
                          void * cache = NULL,
                          bool match_complete_str = false);

static inline const Syntax * parse_str(String what, SourceStr str, 
                                       ast::Environ * env = NULL,
                                       const Replacements * repls = NULL, 
                                       void * cache = NULL) 
{
  return parse_prod(what, str, env, repls, cache, true);
}

#endif
