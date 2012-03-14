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

PEG * parse_peg(const char * fn);

PEG * extend_peg(const PEG * orig, SourceStr);

struct Replacements;
struct ParseAsQuasiQuote {
  bool answer;
  MutableSyntax * aql;
  ParseAsQuasiQuote(bool a = false) : answer(a), aql(NULL) {}
  ParseAsQuasiQuote(MutableSyntax * aql) : answer(true), aql(aql) {}
  operator bool() const {return answer;}
};

const Syntax * parse_prod(String what, SourceStr & str, ParseInfo,
                          ast::Environ * ast_env = NULL,
                          const Replacements * repls = NULL, 
                          bool match_complete_str = false,
                          ParseAsQuasiQuote = ParseAsQuasiQuote());

static inline const Syntax * parse_str(String what, SourceStr str, ParseInfo pinfo,
                                       ast::Environ * env = NULL,
                                       const Replacements * repls = NULL, 
                                       void * cache = NULL)
{
  return parse_prod(what, str, pinfo, env, repls, true);
}

static inline const Syntax * parse_str_as_quasiquote(String what, SourceStr str, ParseInfo pinfo)
{
  return parse_prod(what, str, pinfo, NULL, NULL, true, true);
}

#endif
