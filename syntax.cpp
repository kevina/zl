#include <stdio.h>
#include <stdarg.h>

#include "syntax.hpp"
#include "string_buf.hpp"
#include "asc_ctype.hpp"
#include "iostream.hpp"
#include "syntax_gather.hpp"
#include "symbol_table.hpp"
#include "error.hpp"
#include "expand.hpp"

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

Error::Error(const SourceStr & str, const char * fmt, va_list ap)
  : msg(0, str, vsbprintf(fmt, ap)), ip(&msg.next)
{
  msg.pos_level = ErrorLine::FullLine; 
  msg.maybe_origin = true;
}

Error * error(const SourceInfo * s, const char * pos, const char * fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  Error * res = new Error(SourceStr(s,pos,pos), fmt, ap);
  va_end(ap);
  return res;
}

Error * error(const SourceStr & str, const char * fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  Error * res = new Error(str, fmt, ap);
  va_end(ap);
  return res;
}

Error * error(const Syntax * p, const char * fmt, ...) {
  SourceStr str = p ? p->str() : SourceStr();
  va_list ap;
  va_start(ap, fmt);
  Error * res = new Error(str, fmt, ap);
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
  Error * res = new Error(SourceStr(NULL,pos,pos), fmt, ap);
  va_end(ap);
  return res;
}

struct OriginBacktraces {
  unsigned start;
  unsigned stop;
  struct Data {
    const BacktraceInfo * bt;
    ErrorLine * msg;
    Data(const BacktraceInfo * bt) : bt(bt), msg() {}
  };
  Vector<Data> data;
  OriginBacktraces() : start(0), stop(0) {}
  unsigned find(const BacktraceInfo * to_find) const {
    if (to_find == NULL) return NPOS;
    for (unsigned i = start; i != stop; ++i)
      if (data[i].bt == to_find) return i;
    return NPOS;
  }
  void push_back(const BacktraceInfo * to_add) {
    if (to_add == NULL) return;
    if (find(to_add) == NPOS)
      data.insert(data.begin() + stop, to_add);
    stop++;
  }
  void kill(const BacktraceInfo * to_kill) {
    unsigned pos = find(to_kill);
    if (pos == NPOS) return;
    data.erase(data.begin() + pos);
    stop--;
  }
  void clean() {
    for (unsigned i = start; i != stop; ++i) {
      for (unsigned j = 0; j < start; ++j) 
        if (data[i].bt == data[j].bt) {
          printf("PURGE SELF %u (%u)\n", i, j);
          assert(data[i].msg == NULL);
          data[i].bt = NULL;
        }
      for (unsigned j = stop, sz = data.size(); j < sz; ++j)
        if (data[i].bt == data[j].bt) {
          printf("PURGE LATER %u (%u)\n", j, i);
          assert(data[j].msg == NULL);
          data[j].bt = NULL;
        }
    }
  }
  void purge() {
    Vector<Data>::iterator ip = data.begin(), i = data.begin(), e = data.end();
    while (i != e) {
      if (i->bt == NULL) {
        ++i;
      } else {
        *ip = *i;
        ++ip;
        ++i;
      }
    }
    data.erase(ip, e);
  }
};

void ErrorLine::to_string(StringBuf & res, const OriginBacktraces & obts) 
{
  for (unsigned i = 0; i < indent; ++i)
    res += ' ';
  res << prefix;
  switch (pos_level) {
  case LocOnly:
  case FullLine:
    pos_str(span.file(), span.begin, "", res, prefix.defined() ? "" : ": ");
    break;
  case Sample:
    if (suffix.defined())
      span.sample_w_loc(res);
    break;
  }
  res << suffix;
  unsigned origin_idx = maybe_origin ? obts.find(span.origin()) : NPOS;
  if (origin_idx != NPOS)
    res.printf(" [%u]", origin_idx + 1);
  res += '\n';
  if (pos_level == FullLine) {
    const char * line_start = span.begin;
    const char * line_stop = span.end;
    const SourceBlock * block = span.block();
    if (block) {
      const char * begin = block->box.begin;
      const char * end = block->box.end;
      if (begin <= line_start && line_start <= end
          && begin <= line_stop  && line_stop  <= end) 
      {
        do {
          --line_start;
        } while (line_start >= begin && *line_start != '\n');
        ++line_start;
        while (line_stop < end && *line_stop != '\n')
          ++line_stop;
      } else {
        fprintf(stderr, "WARNING: INVALID BLOCK\n");
      }
    }
    unsigned line_count = 1;
    while (line_start < line_stop && line_count <= 3) {
      for (unsigned i = 0; i < indent+4; ++i) res += ' ';
      const char * p = line_start;
      for (;p < line_stop && *p != '\n'; ++p)
        res += *p;
      res += '\n';
      for (unsigned i = 0; i < indent+4; ++i) res += ' ';
      const char * q = line_start;
      if (span.begin == span.end) {
        for (;q < p; ++q)
          if (q == span.begin)
            res += '^';
          else
            res += ' ';
      } else {
        for (;q < p; ++q)
          if (span.begin <= q && q < span.end)
            res += '^';
          else
            res += ' ';
      }
      if (q == span.begin)
        res += '^';
      res += '\n';
      line_start = p + 1;
      line_count++;
    }
  }
  if (next)
    next->to_string(res, obts);
}

void collect_origin(ErrorLine * msg, OriginBacktraces & obts) {
  for (; msg; msg = msg->next)
    if (msg->maybe_origin) 
      obts.push_back(msg->span.origin());
}

void get_backtrace(const BacktraceInfo * bti, ErrorLine * * ip, OriginBacktraces & obts) {
  if (!bti) return;
  ErrorLine * & head = *ip;
  bti->get_info(&head);
  for (ErrorLine * msg = head; msg; msg = msg->next) {
    if (msg->maybe_origin) 
      obts.push_back(msg->span.origin());
    if (msg->res_bt && msg->res_bt != bti)
      obts.kill(msg->res_bt);
  }
}

String Error::message() {
  OriginBacktraces obts;
  ErrorLine * bt = NULL;
  collect_origin(&msg, obts);
  get_backtrace(msg.span.backtrace(), &bt, obts);
  for (unsigned i = 0; i < obts.data.size(); ++i) {
    obts.start = i;
    obts.stop = i + 1;
    get_backtrace(obts.data[i].bt, &obts.data[i].msg, obts);
    obts.clean();
  }
  obts.purge();
  obts.start = 0;
  obts.stop = obts.data.size();
  StringBuf res;
  msg.to_string(res, obts);
  if (bt)
    bt->to_string(res, obts);
  for (unsigned i = 0, sz = obts.data.size(); i != sz; ++i) {
    res.printf("[%u] source origin:\n", i + 1);
    obts.data[i].msg->to_string(res, obts);
  }
  res += extra;
  return res.freeze();
}

void BacktraceInfo::get_info(ErrorLine * * ip) const {
  switch (action) {
  case EXPANSION_OF: {
    const ExpansionOf * ths = static_cast<const ExpansionOf *>(this);
    ErrorLine * msg = new ErrorLine(2, "in expansion of ", ths->call_site, "");
    msg->res_bt = this;
    *ip = msg;
    ip = &msg->next;
    const char * name = ~ths->macro->real_name;
    const SourceFile * f = ths->macro->def->str().file();
    if (f) {
      msg = new ErrorLine(4, sbprintf("(macro \"%s\", defined at ", name), ths->macro->def->str(), ")");
      msg->pos_level = ErrorLine::LocOnly;
      msg->maybe_origin = true;
    } else {
      msg = new ErrorLine(4, sbprintf("(macro \"%s\")", name));
    } 
    *ip = msg;
    ip = &msg->next;
    const BacktraceInfo * parent = ths->parent();
    if (parent) parent->get_info(ip);
    break;
  } default:
    abort();
  }
}

//
//
//

// note: if the source info is diffrent for the parts but the source
//       block is the same, it will use the source info of the first part

SourceStr SyntaxBase::get_inner_src(const SourceStr & base) const {
  SourceStr s = base;
  for (unsigned i = 0, sz = num_parts(); i != sz; ++i) {
    SourceStr other = part(i)->str();
    if (!s.source) {
      s = other;
    } else if (s.block() == other.block()) {
      // enlarge str
      if (!s.begin || (other.begin && other.begin < s.begin))
        s.begin = other.begin;
      if (other.end > s.end)
        s.end = other.end;
    } else if (i == 1) { // ignore source string for first part, only use the args
      s = other;
    } else {
      s = base;
      break;
    }
  }
  assert((!s.begin && !s.end) || (s.begin && s.end));
  return s;
}

void SyntaxBase::set_src_from_parts() const {
  //printf("SET SRC FROM PARTS\n");
  // even though the SubStr is empty it might
  // contain useful source info
  SourceStr s = get_inner_src(str_);
  if (s.source) {
    str_ = s;
  } else if (num_parts() > 0) {
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
    const ReparseSyntax * rs = as_reparse();
    Syntax * r = rs->cached_val;
    if (r) {
      o.printf("=");
      r->to_string(o,f,g);
    } else {
      o.printf("(%s ...)", ~escape(rwhat().to_string(g)));
    }
    if (rs->repl)
      rs->repl->to_string(o, f, g);
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

