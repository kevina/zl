#ifndef ERROR__HPP
#define ERROR__HPP

// defined in syntax.cpp

#include "type_info.hpp"
#include "source_str.hpp"

struct Error {
  typedef ::TypeInfo<Error> TypeInfo;
  const SourceInfo * source;
  const char * pos;
  String msg;
  String note; // extra information before backtrace
  String message();
  String extra; // extra information after expand backtrace
  Error * add_note(String n) {note = n; return this;}
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
#define zl_assert(exp) do {if (!(exp)) throw error(NO_LOC, "Assert failed (%s:%d %s): " #exp, __FILE__, __LINE__, __FUNCTION__);} while (false)

#endif
