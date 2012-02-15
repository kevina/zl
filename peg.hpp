#ifndef PEG__HPP
#define PEG__HPP

#include "util.hpp"
#include "source_str.hpp"
#include "syntax.hpp"

namespace ast {
  struct Environ;
  // defined in ast.cpp
  bool template_id(const Syntax * syn, Environ * env);
}

void parse_peg(const char * fn);

struct Replacements;
struct ParseAsQuasiQuote {
  bool answer;
  MutableSyntax * aql;
  ParseAsQuasiQuote(bool a = false) : answer(a), aql(NULL) {}
  ParseAsQuasiQuote(MutableSyntax * aql) : answer(true), aql(aql) {}
  operator bool() const {return answer;}
};

const Syntax * parse_prod(String what, SourceStr & str, 
                          ast::Environ * ast_env = NULL,
                          const Replacements * repls = NULL, 
                          void * cache = NULL,
                          bool match_complete_str = false,
                          ParseAsQuasiQuote = ParseAsQuasiQuote());

static inline const Syntax * parse_str(String what, SourceStr str, 
                                       ast::Environ * env = NULL,
                                       const Replacements * repls = NULL, 
                                       void * cache = NULL)
{
  return parse_prod(what, str, env, repls, cache, true);
}

static inline const Syntax * parse_str_as_quasiquote(String what, SourceStr str)
{
  return parse_prod(what, str, NULL, NULL, NULL, true, true);
}
#endif
