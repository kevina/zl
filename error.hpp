#ifndef ERROR__HPP
#define ERROR__HPP

// defined in syntax.cpp

#include "type_info.hpp"
#include "source_str.hpp"

struct OriginBacktraces;

struct ErrorLine {
  unsigned indent;
  String prefix;
  SourceStr span;
  String suffix;
  bool maybe_origin; // also show origin if same location not already
                     // in backtrace
  //bool loc_best_guess; // the location was a best guess, likely added
  //                     // during stack unwinding
  enum PosLevel {LocOnly, Sample, FullLine} pos_level;
  const BacktraceInfo * res_bt;
  ErrorLine * next;
  ErrorLine(unsigned indt, String s)
    : indent(indt), prefix(s), span(), maybe_origin(false), pos_level(LocOnly), res_bt(NULL), next(NULL) {}
  ErrorLine(unsigned indt, SourceStr sp, String sfx)
    : indent(indt), span(sp), suffix(sfx), maybe_origin(false), pos_level(LocOnly), res_bt(NULL), next(NULL) {}
  ErrorLine(unsigned indt, String pre, SourceStr sp, String sfx)
    : indent(indt), prefix(pre), span(sp), suffix(sfx), maybe_origin(false), pos_level(Sample), res_bt(NULL), next(NULL) {}
  void to_string(StringBuf & res, const OriginBacktraces & origin_backtraces);
};

struct Error {
  typedef ::TypeInfo<Error> TypeInfo;
  ErrorLine msg;
  ErrorLine * * ip;
  String message();
  String extra; // extra information after expand backtrace
  Error * add_note(ErrorLine * n) {*ip = n; ip = &(*ip)->next; return this;}
  Error(const SourceStr & str, const char * fmt, va_list ap);
};

//Error * verror(const SourceStr & str, 
//               const char * fmt, va_list ap);
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
#define zl_assert(exp) do {if (!(exp)) throw error(NO_LOC, "Assert failed (%s:%d %s): " #exp, __FILE__, __LINE__, __FUNCTION__);} while (false)

#endif
