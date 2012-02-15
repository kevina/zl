#include <stdio.h>
#include <stdarg.h>

#include "parse.hpp"
#include "parse_common.hpp"
#include "string_buf.hpp"
#include "asc_ctype.hpp"
#include "expand.hpp"     // FIXME: elim dep
#include "asc_ctype.hpp"
#include "iostream.hpp"
#include "syntax_gather.hpp"

using namespace parse_common;

using syntax_ns::SyntaxBase;

namespace syntax_ns {
  SymbolName UNKNOWN_WHAT("<unknown>");
  SymbolName SynEntity::WHAT("<entity>");

  const Leaf SYN_AT("@");
  const Leaf SYN_DOT(".");
  const Leaf SYN_ATB("@{}");
  const Leaf SYN_ID("id");
}


void SyntaxBase::dump_type_info() {
  if ((type_inf & NUM_PARTS_INLINED) || (type_inf & NUM_PARTS_MASK)) 
    printf("PARTS:%u ", type_inf & NUM_PARTS_MASK);
  if (type_inf & NUM_FLAGS_MASK) 
    printf("FLAGS:%u ", (type_inf & NUM_FLAGS_MASK) << NUM_FLAGS_SHIFT);
  if (type_inf & PARTS_INLINED) printf("PARTS_INLINED ");
  else if (type_inf & NUM_PARTS_INLINED) printf("NUM_PARTS_INLINED ");
  else printf("PARTS_SEPARATE ");
  if (type_inf & EXPANDABLE) printf("EXPANDABLE ");
  if (type_inf & SIMPLE) printf("SIMPLE ");
  if (type_inf & EXTRA_INFO_MASK) {
    if ((type_inf & TYPE_ID_MASK) == IS_SYN_ENTITY) printf("IS_SYN_ENTITY ");
    else if ((type_inf & IS_REPARSE) == IS_REPARSE) printf("IS_REPARSE ");
    else printf("<UNKNOWN:%u> ", (type_inf & EXTRA_INFO_MASK) >> EXTRA_INFO_SHIFT );
  }
  if (type_inf & FIRST_PART_SIMPLE) printf("(FIRST_PART_SIMPLE) ");
  if (type_inf >> 15) printf("<GARBAGE AT END>");
  printf("\n");
}

//
// SourceStr
//

bool pos_str(const SourceFile * source, const char * pos,
             const char * pre, OStream & o, const char * post)
{
  if (source) {
    o << pre;
    source->get_pos_str(pos, o);
    o << post;
    return true;
  } else {
    return false;
  }
}

String sample(const char * begin, const char * end, unsigned max_len)
{ 
  StringBuf buf;
  const char * cur = begin;
  while (cur < end && asc_isspace(*cur))
    ++cur;
  while (cur < end && buf.size() < max_len) {
    if (*cur == '\n') break;
    buf << *cur;
    ++cur;
  }
  if (cur < end && (buf.size() == max_len || *cur == '\n')) {
    if (buf.size() > max_len - 3)
      buf.resize(max_len - 3);
    buf += "...";
  }
  return buf.freeze();
}

void SourceStr::sample_w_loc(OStream & o, unsigned max_len) const {
  pos_str("", o, ":");
  o << '"' << sample(begin, end, max_len) << '"';
  //o << end_pos_str(":", o, "");
}

void SyntaxBase::sample_w_loc(OStream & o, unsigned max_len, unsigned syn_len) const {
  if (!str().empty())
    str().sample_w_loc(o, max_len);
  else
    o.printf("a %s", ~what(SPECIAL_OK).name);
  if (syn_len > 0) {
    o << ": ";
    StringBuf buf;
    to_string(buf);
    if (buf.size() > syn_len)
      buf.resize(syn_len);
    o << buf.freeze();
  }
}

String SyntaxBase::sample_w_loc(unsigned max_len, unsigned syn_len) const {
  StringBuf buf;
  sample_w_loc(buf, max_len, syn_len);
  return buf.freeze();
}

//
// Error
//

Error * verror(const SourceInfo * s, const char * pos, 
               const char * fmt, va_list ap) {
  StringBuf buf;
  buf.vprintf(fmt, ap);
  Error * error = new Error;
  error->source = s;
  error->pos = pos;
  error->msg = buf.freeze();
  return error;
}

Error * error(const SourceInfo * s, const char * pos, const char * fmt, ...) {
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
  if (p) {
    StringBuf buf = res->extra;
    buf.printf(">>%p %s %s\n", p, ~p->to_string(), ~p->sample_w_loc(80));
    res->extra = buf.freeze();
  }
  return res;
}

Error * error(const char * pos, const char * fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  Error * res = verror(NULL, pos, fmt, ap);
  va_end(ap);
  return res;
}

String Error::message() {
  StringBuf res;
  if (source)
    pos_str(source->file(), pos, "", res, ": ");
  res += msg;
  res += "\n";
  if (source)
    source->dump_info(res, "  ");
  res += extra;
  return res.freeze();
}


//
//
//

// note: if the source info is diffrent for the parts but the source
//       block is the same, it will use the source info of the first part
void SyntaxBase::set_src_from_parts() const {
  //printf("SET SRC FROM PARTS\n");
  SourceStr s = str_; // even though the SubStr is empty it might
                      // contain useful source info
  for (unsigned i = 0, sz = num_parts(); i != sz; ++i) {
    SourceStr other = part(i)->str();
    if (!s.source) {
      s = other;
    } else if (s.source_block() == other.source_block()) {
      // enlarge str
      if (!s.begin || (other.begin && other.begin < s.begin))
        s.begin = other.begin;
      if (other.end > s.end)
        s.end = other.end;
    } else if (i == 1) { // ignore source string for first part, only use the args
      s = other;
    } else {
      s = str_;
      break;
    }
  }
  assert((!s.begin && !s.end) || (s.begin && s.end));
  if (s.source) {
    str_ = s;
  } else if (num_parts() > 0) { // FIXME: Is this check really necessary ...
    str_ = part(0)->str();
  }
}


/*
void Parts::to_string(OStream & o, PrintFlags f, char sep, SyntaxGather * g) const {
  const_iterator i = begin(), e = end(); 
  if (sep == '\n')
    f.indent += 2;
  if (i == e) return;
  (*i)->to_string(o, f, g);
  ++i;
  while (i != e) {
    if (sep == '\n') {
      o.put('\n');
      for (unsigned i = 0; i < f.indent; ++i)
        o.put(' ');
    } else {
      o.put(sep);
    }
    (*i)->to_string(o, f, g);
    //o << "\n";
    ++i;
  }
}

void Flags::to_string(OStream & o, PrintFlags f, SyntaxGather * g) const {
  const_iterator i = begin(), e = end();
  if (i == e) return;
  while (i != e) {
    o.printf(" :");
    (*i)->to_string(o, f, g);
    ++i;
  }
}
*/

bool ParseSourceInfo::dump_info_self(OStream & o) const {
  o << "  when parsing ";
  str.sample_w_loc(o);
  o << " as " << what;
  return true;
  // FIXME: ... below, make parent
  //str.source->dump_info(o, prefix); 
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

void SyntaxBase::to_string(OStream & o, PrintFlags f, SyntaxGather * g) const {
  if (is_leaf()) {
    SymbolName what_ = what();
    assert(what_.defined());
    if (what_.empty()) 
      o.printf("\"\"");
    else
      o.printf("%s", ~escape(what().to_string(g)));
  } else if (have_entity()) {
    o << "(";
    as_syn_entity()->desc(o);
    o << ")";
  } else if (is_reparse()) {
    Syntax * r = as_reparse()->cached_val;
    if (r) {
      o.printf("=");
      r->to_string(o,f,g);
    } else {
      o.printf("(%s ...)", ~escape(rwhat().to_string(g)));
    }
  } else if (have_parts()) {
    SymbolName what_ = what();
    o.printf("(");
      char sep = ' ';
    if (what_ == "{...}" || what_ == "@")
      sep = '\n';
    if (num_parts() > 0) {
      parts_iterator i = parts_begin(), e = parts_end(); 
      if (sep == '\n')
        f.indent += 2;
      (*i)->to_string(o, f, g);
      ++i;
      while (i != e) {
        if (sep == '\n') {
          o.put('\n');
          for (unsigned i = 0; i < f.indent; ++i)
            o.put(' ');
        } else {
          o.put(sep);
        }
        (*i)->to_string(o, f, g);
        //o << "\n";
        ++i;
      }
    }
    if (sep == '\n') {
      o.put('\n');
      for (unsigned i = 0; i < f.indent; ++i)
        o.put(' ');
    }
    if (have_flags()) {
      flags_iterator i = flags_begin(), e = flags_end();
      while (i != e) {
        o.printf(" :");
        (*i)->to_string(o, f, g);
        ++i;
      }
    }
    o.printf(")");
    if (repl)
      repl->to_string(o, f, g);
  } else {
    abort();
  }
}

String SyntaxBase::to_string() const {
  StringBuf buf;
  to_string(buf);
  return buf.freeze();
}

void SynEntity::desc(OStream & o) const {
  switch (d.type_id) {
  case 0x1FF: 
    o << "<error>"; 
    break;
  case 0x2FF: 
    o << "<symbol: ";
    o << entity<ast::Symbol>()->uniq_name();
    o << ">";
    break;
  case 0x3FF: 
    o << "<key>";
    break;
  case 0x401: 
    o << "<exp>";
    break;
  case 0x402: 
    o << "<stmt>";
    break;
  case 0x5FF: 
    o << "<type>";
    break;
  case 0x7FF:
    o << "<decl handle: ";
    entity<ast::DeclHandle>()->desc(o);
    o << ">";
    break;
  default: 
    o << WHAT;
  }
}

void ReparseSyntax::do_instantiate(bool no_throw) const {
  if (rwhat().name == "sexp")
    assert(parse_as);
  if (parse_as.defined()) {
    //printf("reparsing as %s\n", ~parse_as);
    cached_val = reparse(parse_as, outer());
  } else if (!no_throw) {
    fprintf(stderr, "Can't Inst %s\n", ~sample_w_loc());
    abort();
  }
}

static const char * s_id(SourceStr str, String & res) {
  bool have_quotes = false;
  bool in_quote = false;
  StringBuf buf;
  while (!str.empty()) {
    if ((asc_isspace(*str) || *str == '(' || *str == ')' || *str == '#') && !in_quote) break;
    if (*str == '"') {
      have_quotes = true;
      in_quote = !in_quote;
      ++str;
      continue;
    }
    if (*str == '\\') {
      buf += *str;
      ++str;
      if (str.empty())
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

  Res parse_grp_or_id(SourceStr str) 
  {
    if (*str == '(') {
      return parse(str);
    } else {
      SyntaxLeaf * r = new SyntaxLeaf(str);
      str = s_id(str, r->what_);
      r->str_.end = str;
      return Res(str, r);
    }
  }

  Res parse(SourceStr str)
  {
    String name;
    const char * start = str.begin;
    str = spacing(str);
    if (*str != '(') throw error(str, "Expected '('");
    SyntaxBuilder res;
    SourceStr rstr = str;
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
        res.add_part(r.parse);
      } else if (str[0] == ':' && str[1] != ':') {
        ++str;
        Res r = parse_grp_or_id(str);
        str = r.end;
        res.add_flag(r.parse);
      } else {
        SyntaxLeaf * r = new SyntaxLeaf(str);
        str = s_id(str, r->what_);
        r->str_.end = str;
        res.add_part(r);
      }
      str = spacing(str);
    }
    if (str.empty() || *str != ')') throw error(str.source, start, "Unterminated '('");
    ++str;
    rstr.end = str;
    str = spacing(str);
    return Res(str, res.build(rstr));
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





