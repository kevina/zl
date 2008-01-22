#include <stdio.h>
#include <stdarg.h>

#include "parse.hpp"
#include "parse_common.hpp"
#include "string_buf.hpp"
#include "asc_ctype.hpp"
#include "expand.hpp"     // FIXME: elim dep

using namespace parse_common;

Error * verror(const SourceEntity * s, const char * pos, 
               const char * fmt, va_list ap) {
  StringBuf buf;
  buf.vprintf(fmt, ap);
  Error * error = new Error;
  error->source = s;
  error->pos = pos;
  error->msg = buf.freeze();
  return error;
}

Error * error(const SourceEntity * s, const char * pos, const char * fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  Error * res = verror(s, pos, fmt, ap);
  va_end(ap);
  return res;
}

Error * error(SourceStr str, const char * fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  Error * res = verror(str.source, str.begin, fmt, ap);
  va_end(ap);
  return res;
}

Error * error(const Parse * p, const char * fmt, ...) {
  SourceStr str = p->str();
  va_list ap;
  va_start(ap, fmt);
  Error * res = verror(str.source, str.begin, fmt, ap);
  va_end(ap);
  return res;
}

Error * error(const char * pos, const char * fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  Error * res = verror(0, pos, fmt, ap);
  va_end(ap);
  return res;
}

void Parse::set_src_from_parts() const {
  printf("SET SRC FROM PARTS\n");
  SourceStr s;
  for (unsigned i = 0; i != d->parts.size(); ++i) {
    SourceStr other = d->parts[i]->str();
    printf(">%d> %p %.*s\n", i, other.source, other.end - other.begin, other.begin);
    if (!s.source) {
      s = other;
    } else if (s.source == other.source) {
      // enlarge str
      if (other.begin < s.begin)
        s.begin = other.begin;
      if (other.end > s.end)
        s.end = other.end;
    } else if (i == 1) { // ignore source string for first part, only use the args
      s = other;
    } else {
      s.clear();
      break;
    }
  }
  if (s.source) {
    str_ = s;
  } else {
    str_ = d->parts[0]->str();
  }
  print();
  printf("\n>%.*s<\n", str_.end - str_.begin, str_.begin);
}


void Parts::print() const {
  const_iterator i = begin(), e = end(); 
  if (i == e) return;
  (*i)->print();
  ++i;
  while (i != e) {
    printf(" ");
    (*i)->print();
    ++i;
  }
}

void Flags::print() const {
  const_iterator i = begin(), e = end();
  if (i == e) return;
  while (i != e) {
    printf(" :");
    (*i)->print();
    ++i;
  }
}

String escape(String n) {
  if (n.empty()) return String("\"\"");
  bool require_quotes = false;
  bool need_escape    = false;
  if (n[0] == ':') require_quotes=true;
  for (String::iterator i = n.begin(), e = n.end(); i != e; ++i) {
    if (asc_isspace(*i) || *i == '(' || *i == ')') require_quotes = true;
    if (*i == '"' || *i == '\\') need_escape = true;
  }
  if (require_quotes || need_escape) {
    StringBuf res;
    if (require_quotes) res += '"';
    for (String::iterator i = n.begin(), e = n.end(); i != e; ++i) {
      if (*i == '"' || *i == '\\') res += '\\';
      res += *i;
    }
    if (require_quotes) res += '"';
    return res.freeze();
  } else {
    return n;
  }
}

void Parse::print() const {
  if (simple()) {
    if (!name.defined()) 
      printf("()");
    else if (name.empty()) 
      printf("\"\"");
    else
      printf("%s", ~escape(name));
  } else {
    printf("(");
    d->parts.print();
    d->flags.print();
    printf(")");
    if (repl)
      repl->print();
  }
}

static const char * s_id(SourceStr str, String & res) {
  bool have_quotes = false;
  bool in_quote = false;
  StringBuf buf;
  while (!str.empty()) {
    if ((isspace(*str) || *str == ')' || *str == '#') && !in_quote) break;
    if (*str == '"') {
      have_quotes = true;
      in_quote = !in_quote;
      ++str;
      continue;
    }
    if (*str == '\\') {
      ++str;
      if (!str.empty())
        throw error(str, "Unexpected end of identifier");
    }
    buf += *str;
    ++str;
  }
  res = buf.freeze();
  if (res.empty() && !have_quotes)
    res = String();
  return str;
}

namespace parse_parse {

  Res parse(SourceStr str)
  {
    Parse * res = new Parse();
    String name;
    const char * start = str.begin;
    str = spacing(str);
    if (*str != '(') throw error(str, "Expected '('");
    ++str;
    str = spacing(str);
    /*const char * name_start = str.begin;
    str = s_id(str, name);
    const char * name_end = str.end;
    str = spacing(str);
    if (str.empty())
    else
    res = new Parse(new Parse(name, str, name_start, name_end));*/
    while (!str.empty() && *str != ')') {
      if (*str == '(') {
        Res r = parse(str);
        str = r.end;
        res->add_part(r.parse);
      } else {
        Parse * r = new Parse();
        r->str_.begin = str;
        str = s_id(str, r->name);
        r->str_.end = str;
        res->add_part(r);
      }
      str = spacing(str);
    }
    if (str.empty() || *str != ')') throw error(str.source, start, "Unterminated '('");
    ++str;
    res->str_.assign(start, str);
    str = spacing(str);
    return Res(str, res);
  }
}

namespace parse_common {

  const char * id(const char * str, const char * end, String & res) {
    const char * start = str;
    if (str != end && (isalpha(*str) || *str == '_'))
      ++str;
    while (str != end && (isalpha(*str) || isdigit(*str) || *str == '_'))
        ++str;
    if (str == start) 
      throw error(str, "identifer expected");
    res.assign(start, str);
    str = spacing(str, end);
    return str;
  }

  const char * quote(char close, const char * str, const char * end, String & val) {
    StringBuf buf;
    const char * start = str;
    ++str;
    while (*str != close && str != end) {
      if (*str == '\\') {
        ++str;
          if (str == end)
            throw error(str, "Unexpected end of string");
      }
      buf += *str;
      ++str;
    }
    if (str == end)
      throw error(start, "Unterminated %c", close);
    ++str;
    str = spacing(str, end);
    val = buf.freeze();
    return str;
  }
  
  const char * symbol(char sym, const char * str, const char * end) {
    if (*str != sym) return NULL;
    ++str;
    str = spacing(str, end);
    return str;
  }
    
  const char * require_symbol(char sym, const char * str, const char * end) {
    const char * res = symbol(sym, str, end);
    if (!res) throw error(str, "'%c' expected", sym);
    return res;
  }

}


