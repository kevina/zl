#ifndef INDENT_OSTREAM__HPP
#define INDENT_OSTREAM__HPP

#include "string_buf.hpp"
#include "ostream.hpp"

class IndentOStream : public OStream {
public:
  StringBuf buf;
  char indent_char;
  unsigned indent_level;
  bool need_indent;
  OStream * out;

  IndentOStream(OStream * o = NULL)
    : indent_char(' '), indent_level(1), need_indent(true), out(o) {}

  void indent() {
    for (unsigned i = 0; i < indent_level; ++i)
      out->write(indent_char);
    need_indent = false;
  }

  void flush() {
    for (const char * str = buf.begin(); str != buf.end(); ++str) {
      if (need_indent) indent();
      out->write(*str);
      if (*str == '\n') need_indent = true;
    }
    buf.clear();
  }

  void write (char c) {
    buf.write(c);
    if (c == '\n')
      flush();
  }
  void write (ParmStr str) {
    buf.write(str);
    flush();
  }
  void write (const void * str, unsigned int sz) {
    buf.write(str,sz);
    flush();
  }

  int vprintf(const char *format, va_list ap) {
    int res = buf.vprintf(format, ap);
    flush();
    return res;
  }
  
};

extern IndentOStream IOUT;

struct IncreaseIndent {
  unsigned level;
  IndentOStream & out;
  IncreaseIndent(unsigned l, IndentOStream & o) 
    : level(l), out(o) {out.indent_level += level;}
  IncreaseIndent(IndentOStream & o) 
    : level(2), out(o) {out.indent_level += level;}
  ~IncreaseIndent() {
    out.indent_level -= level;
  }
};

#define INCREASE_INDENT(out) IncreaseIndent increase_ind_dummy(out)

#endif
