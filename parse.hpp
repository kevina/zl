#ifndef PARSE__HPP
#define PARSE__HPP

#include <stdio.h>

#include "syntax.hpp"

// common structure used for parse results

struct Annon;
struct SyntaxGather;

struct Error {
  typedef ::TypeInfo<Error> TypeInfo;
  const SourceInfo * source;
  const char * pos;
  String msg;
  String message();
  String extra; // extra information after expand backtrace
};

Error * verror(const SourceInfo * s, const char * pos, 
               const char * fmt, va_list ap);
Error * error(const SourceInfo * s, const char * pos, 
              const char * fmt, ...)
  __attribute__ ((format (printf, 3, 4)));
Error * error(const SourceStr & str, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
Error * error(const Syntax *, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
Error * error(const char * pos, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));

#define NO_LOC (const Syntax *)NULL

#define unknown_error(pos) error(pos, "Unknown Error (%s:%d %s)", __FILE__, __LINE__, __FUNCTION__)

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
