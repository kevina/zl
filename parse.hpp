#ifndef PARSE__HPP
#define PARSE__HPP

#include <stdio.h>

#include "gc.hpp"
#include "util.hpp"
#include "string_buf.hpp" // FIXME: elim dep
#include "entity.hpp"
#include "symbol_table.hpp"

// common structure used for parse results

using ast::SymbolName;

struct Parse;

class SourceEntity : public gc {
public: // but don't use
  const SourceFile * file_;
  const Parse * expansion_;
public:
  explicit SourceEntity(const SourceFile * f = 0)
    : file_(f), expansion_(0) {}
  explicit inline SourceEntity(const Parse * e);
  String file_name() const {return file_->file_name();}
  const Parse * expansion() const {return expansion_;}
  Pos get_pos(const char * s) const {return file_->get_pos(s);}
  char * get_pos_str(const char * s, char * buf) const {
    return file_->get_pos_str(s, buf);
  }
  unsigned size() const {return file_->size();}
  const char * begin() const {return file_->begin();}
  const char * end() const {return file_->end();}
};

inline const SourceEntity * SourceFile::entity() const {
  if (!entity_) entity_ = new SourceEntity(this);
  return entity_;
}

struct SourceStr : public SubStr {
  const SourceEntity * source;
  SourceStr() : source() {}
  SourceStr(const char * b, const char * e) : SubStr(b,e), source() {}
  SourceStr(String s) : SubStr(s), source() {}
  SourceStr(const SourceEntity * f) : SubStr(f->begin(), f->end()), source(f) {}
  SourceStr(const SourceEntity * f, const char * b, const char * e) 
    : SubStr(b,e), source(f) {}
  SourceStr(const SourceEntity * f, String s) 
    : SubStr(s), source(f) {}
  operator const char * & () {return begin;}
  operator const char * () const {return begin;}
  void clear() {
    SubStr::clear();
    source = 0;
  }
  void assign(const char * str, const SourceStr & end) {
    SubStr::assign(str, end.begin);
    source = end.source;
  }
  void adj(const SourceStr & other) {
    if (!source && other.source) source = other.source;
    if (source != other.source) return;
    if (!begin || begin > other.begin) begin = other.begin;
    if (!end || end < other.end) end = other.end;
  }
  SourceStr & operator=(const char * s) {begin = s; return *this;}
};

struct ErrorInfo : public gc {
  const SourceEntity * source;
  const char * pos;
  String msg;
};

struct Error : public ErrorInfo {
  String message() {
    char buf[24]; // FIXME: Should be no need
    StringBuf res;
    if (source) {
      if (!source->file_name().empty()) {
        res += source->file_name();
        res += ": ";
      }
      if (pos) {
        Pos p = source->get_pos(pos);
        pos_to_str(p, buf);
        res += buf;
        res += ": ";
      }
    }
    res += msg;
    return res.freeze();
  }
};

Error * verror(const SourceEntity * s, const char * pos, 
               const char * fmt, va_list ap);
Error * error(const SourceEntity * s, const char * pos, 
              const char * fmt, ...)
  __attribute__ ((format (printf, 3, 4)));
Error * error(SourceStr str, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
Error * error(const Parse *, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
Error * error(const char * pos, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
struct Parse;

struct Parts : public Vector<const Parse *> {
  void append(const Parse * x) {
    push_back(x);
  }
  void append(const Parts & other) {
    insert(end(), other.begin(), other.end());
  }
  void print() const;
};

struct Flags : public gc {
  Vector<const Parse *> data;
  typedef Vector<const Parse *>::const_iterator iterator;
  typedef Vector<const Parse *>::const_iterator const_iterator;
  const_iterator begin() const {return data.begin();}
  const_iterator end() const {return data.end();}
  bool empty() const {return data.empty();}
  unsigned size() const {return data.size();}
  inline const Parse * lookup(SymbolName d);
  bool have(String d) {
    return lookup(d) != NULL;
  }
  inline bool insert(const Parse * p);
  void print() const;
};


// A parse is:                                      d  repl  entity_   marks
//   Branch                                         X   -       -        -
//   Symbol -- w/ marks                             -   -       -        x
//   Literal -- w/o marks                           -   -       -        -
//   Unparsed Text -- w/ repl and marks closure (1) x   X       -        -
//   An Entity                                      2   -       X        -
//   NOTES: (1) ...
//          (2) set to AS_ENTITY

// A simple parse is one with only one part which is itself
// If not a simple parse than the name IS the name of the first
//   part, provided it is simple, otherwise it doesn't exist

struct Replacements;


struct Parse : public gc {
  struct D : public gc_cleanup {
    Parts parts;
    Flags flags;
  };
  static D * const AS_ENTITY;
  SymbolName what_;
  SymbolName what() const {return what_;}
  //operator String () const {assert(simple() || part(0)->simple()); return what_;}
  operator SymbolName () const {assert(simple() || part(0)->simple()); return what_;}
  const char * operator ~ () const {return ~operator SymbolName();}
  String string_if_simple() const {return simple() ? what() : String();}
  mutable SourceStr str_;
  const SourceStr & str() const {if (d && str_.empty()) set_src_from_parts(); return str_;}
  D * d;
  const Replacements * repl;
  Entity * entity_;
  Entity * entity() const {return entity_;}

  Parse(const Parse & other) 
    : what_(other.what_), str_(other.str_), d(other.d ? new D(*d) : 0), repl(other.repl), entity_(other.entity_)
  {}
  Parse & operator= (const Parse & other) {
    what_ = other.what_;
    str_ = other.str_;
    d = other.d ? new D(*other.d) : 0;
    repl = other.repl;
    entity_ = other.entity_;
    return *this;
  }

  Parse() : d(), repl(0), entity_() {}
  explicit Parse(String n) : what_(n), d(), repl(), entity_() {}
  Parse(String n, const SourceStr & s) 
    : what_(n), str_(s), d(), repl(), entity_() {}
  Parse(String n, const SourceStr & s, const char * e)
    : what_(n), str_(s.source, s.begin, e), d(), repl(), entity_() {}
  Parse(String n, const SourceStr & s, const char * b, const char * e)
    : what_(n), str_(s.source, b, e), d(), repl(), entity_() {}
  Parse(const SourceStr & s) 
    : str_(s), d(), repl(0), entity_() {}

  Parse(const SourceStr & s, const Parse * p) : str_(s), repl(), entity_() {
    d = new D; 
    what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  explicit Parse(const Parse * p) : repl(), entity_() {
    d = new D; 
    what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  Parse(const Parse * p, const Parse * x) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
  }
  Parse(const Parse * p, const Parse * x, const Parse * y) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
  }
  Parse(const Parse * p, const Parse * x, const Parse * y, const Parse * z) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
    d->parts.push_back(z);
  }
  Parse(const SourceStr & s, const Parse * p, const Parse * x) : str_(s), repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
  }
  Parse(const SourceStr & s, const Parse * p, const Parse * x, const Parse * y) : str_(s), repl(0), entity_()  {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
  }
  Parse(const SourceStr & s, const Parse * p, const Parse * x, const Parse * y, const Parse * z) : str_(s), repl(0), entity_()  {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
    d->parts.push_back(z);
  }
  Parse(const Entity * e) : what_("<entity>"), str_(), d(), repl(), entity_(const_cast<Entity*>(e)) 
    {}

  bool simple() const {return !d && !entity_;}
  void make_branch() {
    if (entity_) {
      const Parse * p = new Parse(*this);
      d = new D;
      d->parts.push_back(p);
      what_ = String();
      entity_ = NULL;
    } else if (what_.defined()) {
      const Parse * p = new Parse(*this);
      d = new D;
      d->parts.push_back(p);
    } else {
      d = new D;
    }
  }

  unsigned num_parts() const {
    if (!d && what_.defined()) return 1;
    if (!d) return 0;
    else return d->parts.size();
  }
  unsigned num_args() const {
    return num_parts() - 1;
  }
  const Parse * part(unsigned i) const {
    if (!d) return this;
    else return d->parts[i];
  }
  const Parse * arg(unsigned i) const {
    return d->parts[i+1];
  }
  const Parse * & part(unsigned i) {
    return d->parts[i];
  }
  const Parse * & arg(unsigned i) {
    return d->parts[i+1];
  }
  Parts::const_iterator args_begin() const {return d->parts.begin() + 1;}
  Parts::const_iterator args_end()   const {return d->parts.end();}
  const Parse * flag(String n) const {
    if (!d) return NULL;
    else return d->flags.lookup(n);
  }

  void add_part(const Parse * p) {
    if (!d) make_branch();
    if (d->parts.empty()) what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  void add_args(const Parse * other) {
    if (!d) make_branch();
    assert(what_.defined());
    d->parts.insert(d->parts.end(), other->args_begin(), other->args_end());
  }
  void add_parts(Parts::const_iterator i, Parts::const_iterator end) {
    if (i == end) return;
    if (!d) make_branch();
    if (d->parts.empty()) what_ = (*i)->string_if_simple();
    d->parts.insert(d->parts.end(), i, end);
  }
  void add_parts(const Parts & p) {
    add_parts(p.begin(), p.end());
  }

  void add_flag(const Parse * p) {
    if (!d) make_branch();
    d->flags.insert(p);
  }
  void set_flags(const Flags & p) {
    if (p.empty()) return;
    if (!d) make_branch();
    d->flags = p;
  }
  void set_flags(const Parse * p) {
    if (!p->d) return;
    if (!d) make_branch();
    d->flags = p->d->flags;
  }
  void set_src_from_parts() const; // const is a lie

  void print() const;
  bool is_a(SymbolName n) const {return what_ == n;}
  bool is_a(SymbolName n, SymbolName p) const {return what_ == n && num_args() > 0 && arg(0)->is_a(p);}

  virtual ~Parse() {}
};

static inline bool operator==(const Parse & p, const char * str) {
  //printf("%s %d == %s\n", ~p.what(), p.simple(), str);
  if (!p.simple() && !p.part(0)->simple()) return false;
  return (String)p == str;
}

inline SourceEntity::SourceEntity(const Parse * e)
  : file_(e->str().source->file_), expansion_(e) {}

inline const Parse * Flags::lookup(SymbolName d) {
  for (iterator i = begin(); i != end(); ++i) 
    if ((*i)->what() == d) return *i;
  return NULL;
}

inline bool Flags::insert(const Parse * p) {
  assert(p->what().defined());
  if (have(p->what())) return false;
  data.push_back(p);
  return true;
}

namespace ast {
  inline SymbolKey::SymbolKey(const Parse & p) : SymbolName(p), ns() {}
}

namespace parse_parse {

  struct Res {
    const char * end;
    Parse * parse;
    Res(const char * e, Parse * r) : end(e), parse(r) {}
  };
  
  Res parse(SourceStr);

}

#endif
