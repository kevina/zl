#ifndef PARSE__HPP
#define PARSE__HPP

#include <stdio.h>

#include "syntax.hpp"

// common structure used for parse results

struct Annon;
struct SyntaxGather;

struct ParseSourceInfo : public SourceInfo {
  SourceStr str;
  String what;
  ParseSourceInfo(const SourceStr & s, String w) 
    : SourceInfo(s.source), str(s), what(w) {}
  bool dump_info_self(OStream &) const;
};

namespace ast {
  inline SymbolKey::SymbolKey(const Syntax & p, const InnerNS * ns0) 
    : SymbolName(p.as_symbol_name()), ns(ns0 ? ns0 : DEFAULT_NS) {}


}

namespace parse_parse {

  struct Res {
    const char * end;
    Syntax * parse;
    Res(const char * e, Syntax * r) : end(e), parse(r) {}
  };
  
  Res parse(SourceStr);

}

#endif
