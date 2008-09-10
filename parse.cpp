#include <stdio.h>
#include <stdarg.h>

#include "parse.hpp"
#include "parse_common.hpp"
#include "string_buf.hpp"
#include "asc_ctype.hpp"
#include "expand.hpp"     // FIXME: elim dep
#include "asc_ctype.hpp"
#include "iostream.hpp"

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

Error * error(const SourceStr & str, const char * fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  Error * res = verror(str.source, str.begin, fmt, ap);
  va_end(ap);
  return res;
}

Error * error(const Syntax * p, const char * fmt, ...) {
  SourceStr str = p ? p->str() : SourceStr();
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

void Syntax::set_src_from_parts() const {
  //printf("SET SRC FROM PARTS\n");
  SourceStr s;
  for (unsigned i = 0; i != d->parts.size(); ++i) {
    SourceStr other = d->parts[i]->str();
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
  } else if (d->parts.size() > 0) { // FIXME: Is this check really necessary ...
    str_ = d->parts[0]->str();
  }
}


void Parts::to_string(OStream & o) const {
  const_iterator i = begin(), e = end(); 
  if (i == e) return;
  (*i)->to_string(o);
  ++i;
  while (i != e) {
    o.printf(" ");
    (*i)->to_string(o);
    ++i;
  }
}

void Flags::to_string(OStream & o) const {
  const_iterator i = begin(), e = end();
  if (i == e) return;
  while (i != e) {
    o.printf(" :");
    (*i)->to_string(o);
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

void Syntax::print() const {
  to_string(COUT);
}

void Syntax::to_string(OStream & o) const {
  if (!d) { 
    if (entity_)
      o.printf("(%s)", ~escape(what_.to_string()));
    else if (!what_.defined()) 
      o.printf("()");
    else if (what_.empty()) 
      o.printf("\"\"");
    else
      o.printf("%s", ~escape(what_.to_string()));
  } else {
    o.printf("(");
    d->parts.to_string(o);
    d->flags.to_string(o);
    o.printf(")");
    if (repl)
      repl->to_string(o);
  }
}

String Syntax::to_string() const {
  StringBuf buf;
  to_string(buf);
  return buf.freeze();
}

static const char * s_id(SourceStr str, String & res) {
  bool have_quotes = false;
  bool in_quote = false;
  StringBuf buf;
  while (!str.empty()) {
    if ((asc_isspace(*str) || *str == ')' || *str == '#') && !in_quote) break;
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
    Syntax * res = new Syntax();
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
    res = new Syntax(new Syntax(name, str, name_start, name_end));*/
    while (!str.empty() && *str != ')') {
      if (*str == '(') {
        Res r = parse(str);
        str = r.end;
        res->add_part(r.parse);
      } else if (*str == ':') {
        ++str;
        Res r = parse(str);
        str = r.end;
        res->add_flag(r.parse);
      } else {
        Syntax * r = new Syntax();
        r->str_.begin = str;
        str = s_id(str, r->what_);
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
    if (str != end && (asc_isalpha(*str) || *str == '_'))
      ++str;
    while (str != end && (asc_isalpha(*str) || asc_isdigit(*str) || *str == '_'))
        ++str;
    if (str == start) 
      throw error(str, "identifer expected");
    res.assign(start, str);
    str = spacing(str, end);
    return str;
  }

  const char * quote(char close, const char * str, const char * end, SubStr & val) {
    const char * start = str;
    ++str;
    val.begin = str;
    while (*str != close && str != end) {
      if (*str == '\\') {
        ++str;
        if (str == end)
          throw error(str, "Unexpected end of string");
      }
      ++str;
    }
    val.end = str;
    if (str == end)
      throw error(start, "Unterminated %c", close);
    ++str;
    str = spacing(str, end);
    return str;
  }

  void unescape(const char * s, const char * end, StringBuf & out, char quote) {
    for (; s != end; ++s)
      no_inc:
      if (*s == '\\') {
        ++s;
        assert(s != end);
        switch (*s) {
        case 'a': out += '\a'; break;
        case 'b': out += '\b'; break;
        case 'f': out += '\f'; break;
        case 'n': out += '\n'; break;
        case 't': out += '\t'; break;
        case 'v': out += '\v'; break;
        case 'x': {
          // hex 
          ++s;
          char * e = (char *)s;
          unsigned val = strtol(s, &e, 16);
          if (s == e) abort(); // FIXME: Error
          s = e;
          if (val > 255) abort(); // FIXME: Error message, out of range 
          out += (char)val;
          goto no_inc;
       } case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
          // oct
          unsigned val = *s - '0'; ++s;
          if (s != end && '0' <= *s && *s <= '7') {val *= 8; val += *s - '0'; ++s;}
          if (s != end && '0' <= *s && *s <= '7') {val *= 8; val += *s - '0'; ++s;}
          if (val > 255) abort(); // FIXME: Error message, out of range
          out += (char)val;
          goto no_inc;
        } default:
          out += *s;
        }
      } else if (*s != quote) {
        out += *s;
      }
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


static inline Syntax::D * make_as_entity() {
  Syntax::D * d = new Syntax::D;
  d->parts.push_back(new Syntax("<entity>"));
  return d;
}
Syntax::D * const Syntax::AS_ENTITY = make_as_entity();
