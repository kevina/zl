#include <stdio.h>
#include <stdarg.h>

#include "parse.hpp"
#include "parse_common.hpp"
#include "string_buf.hpp"
#include "asc_ctype.hpp"
#include "error.hpp"

static const char * s_id(const SourceStr & str, String & res) {
  bool have_quotes = false;
  bool in_quote = false;
  StringBuf buf;
  const char * p = str.begin;
  while (p != str.end) {
    if ((asc_isspace(*p) || *p == '(' || *p == ')' || *p == '#') && !in_quote) break;
    if (*p == '"') {
      have_quotes = true;
      in_quote = !in_quote;
      ++p;
      continue;
    }
    if (*p == '\\') {
      buf += *p;
      ++p;
      if (p == str.end)
        throw error(str.source, p, "Unexpected end of identifier");
    }
    buf += *p;
    ++p;
  }
  res = buf.freeze();
  if (res.empty() && !have_quotes)
    res = String();
  return p;
}

ErrorLine * extra_parse_info(const SourceStr & str, String what) {
  return new ErrorLine(4, "when parsing ", str, sbprintf(" as %s", ~what));
}

namespace parse_parse {

  using namespace parse_common;

  using parse_common::spacing;
  static inline const char * spacing(const SourceStr & str) {
    return spacing(str.begin, str.end);
  }

  Res parse_grp_or_id(const SourceStr & str) 
  {
    if (*str.begin == '(') {
      return parse(str);
    } else {
      SyntaxLeaf * r = new SyntaxLeaf(str);
      const char * p = str.begin;
      p = s_id(str, r->what_);
      r->str_.end = p;
      return Res(p, r);
    }
  }

  Res parse(SourceStr str)
  {
    String name;
    const char * start = str.begin;
    const char * p = str.begin;
    str.begin = spacing(str);
    if (*str != '(') throw error(str, "Expected '('");
    SyntaxBuilder res;
    SourceStr rstr = str;
    ++str.begin;
    str.begin = spacing(str);
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
        str.begin = r.end;
        res.add_part(r.parse);
      } else if (str[0] == ':' && str[1] != ':') {
        ++str.begin;
        Res r = parse_grp_or_id(str);
        str.begin = r.end;
        res.add_flag(r.parse);
      } else {
        SyntaxLeaf * r = new SyntaxLeaf(str);
        str.begin = s_id(str, r->what_);
        r->str_.end = str.begin;
        res.add_part(r);
      }
      str.begin = spacing(str);
    }
    if (str.empty() || *str != ')') throw error(str.source, start, "Unterminated '('");
    ++str.begin;
    rstr.end = str.begin;
    str.begin = spacing(str);
    return Res(str.begin, res.build(rstr));
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
    while (str != end && *str != close) {
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
    while (s != end) {
      if (*s == '\\') {
        ++s;
        assert(s != end); // FIXME: Error message, unexpected end-of-string
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
          continue;
       } case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
          // oct
          unsigned val = *s - '0'; ++s;
          if (s != end && '0' <= *s && *s <= '7') {val *= 8; val += *s - '0'; ++s;}
          if (s != end && '0' <= *s && *s <= '7') {val *= 8; val += *s - '0'; ++s;}
          if (val > 255) abort(); // FIXME: Error message, out of range
          out += (char)val;
          continue;
        } default:
          out += *s;
        }
      } else if (*s != quote) {
        out += *s;
      }
      ++s;
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
