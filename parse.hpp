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

struct Syntax;

class SourceEntity : public gc {
public: // but don't use
  const SourceFile * file_;
  const Syntax * expansion_;
public:
  explicit SourceEntity(const SourceFile * f = 0)
    : file_(f), expansion_(0) {}
  explicit inline SourceEntity(const Syntax * e);
  String file_name() const {return file_->file_name();}
  const Syntax * expansion() const {return expansion_;}
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
Error * error(const SourceStr & str, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
Error * error(const Syntax *, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
Error * error(const char * pos, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
struct Syntax;

struct Parts : public Vector<const Syntax *> {
  void append(const Syntax * x) {
    push_back(x);
  }
  void append(const Parts & other) {
    insert(end(), other.begin(), other.end());
  }
  void print() const;
};

struct Flags : public gc {
  Vector<const Syntax *> data;
  typedef Vector<const Syntax *>::const_iterator iterator;
  typedef Vector<const Syntax *>::const_iterator const_iterator;
  const_iterator begin() const {return data.begin();}
  const_iterator end() const {return data.end();}
  bool empty() const {return data.empty();}
  unsigned size() const {return data.size();}
  inline const Syntax * lookup(SymbolName d) const;
  bool have(SymbolName d) const {
    return lookup(d) != NULL;
  }
  inline bool insert(const Syntax * p);
  void merge(const Flags & others) {
    for (Vector<const Syntax *>::const_iterator 
           i = others.data.begin(), e = others.data.end();
         i != e; ++i)
      insert(*i);
  }
  void print() const;
};


// A Syntax object is:                              d  repl  entity_
//   Branch                                         X   -       -   
//   Symbol                                         -   -       -   
//   Unparsed Text -- w/ repl and marks closure (1) x   X       -   
//   An Entity                                      2   -       X   
//   NOTES: (1) ...
//          (2) set to AS_ENTITY

// A simple syntax obj is one with only one part which is itself
// If not a simple parse than the name is the name of the first
//   part, provided it is simple, otherwise it doesn't exist

struct Replacements;


struct Syntax : public gc {
  struct D : public gc_cleanup {
    Parts parts;
    Flags flags;
  };
  static D * const AS_ENTITY;
  SymbolName what_;
  SymbolName what() const {return what_;}
  String as_string() const {assert(simple() || part(0)->simple()); return what_;}
  const SymbolName & as_symbol_name() const {assert(simple() || part(0)->simple()); return what_;}
  operator String () const {return as_string();}
  operator const SymbolName & () const {return as_symbol_name();}
  const char * operator ~ () const {return ~as_string();}
  SymbolName string_if_simple() const {return simple() ? what() : SymbolName();}
  mutable SourceStr str_;
  const SourceStr & str() const {if (d && str_.empty()) set_src_from_parts(); return str_;}
  D * d;
  const Replacements * repl;
  Entity * entity_;
  Entity * entity() const {return entity_;}

  Syntax(const Syntax & other) 
    : what_(other.what_), str_(other.str_), d(other.d ? new D(*d) : 0), repl(other.repl), entity_(other.entity_)
  {}
  Syntax & operator= (const Syntax & other) {
    what_ = other.what_;
    str_ = other.str_;
    d = other.d ? new D(*other.d) : 0;
    repl = other.repl;
    entity_ = other.entity_;
    return *this;
  }

  Syntax() : d(), repl(0), entity_() {}
  explicit Syntax(const char * n) : what_(n), d(), repl(), entity_() {}
  explicit Syntax(String n) : what_(n), d(), repl(), entity_() {}
  explicit Syntax(SymbolName n) : what_(n), d(), repl(), entity_() {}
  Syntax(SymbolName n, const SourceStr & s) 
    : what_(n), str_(s), d(), repl(), entity_() {}
  Syntax(SymbolName n, const SourceStr & s, const char * e)
    : what_(n), str_(s.source, s.begin, e), d(), repl(), entity_() {}
  Syntax(SymbolName n, const SourceStr & s, const char * b, const char * e)
    : what_(n), str_(s.source, b, e), d(), repl(), entity_() {}
  Syntax(const SourceStr & s) 
    : str_(s), d(), repl(0), entity_() {}

  Syntax(const Syntax * o, const ast::Mark * m)
    : what_(ast::mark(o->what_, m)), str_(o->str_), d(), repl(), entity_()
    {assert(o->simple()); /*printf(">>%s %p\n", ~what_, what_.marks);*/}

  Syntax(const Syntax * o, const ast::Marks * m)
    : what_(o->what_.name, m), str_(o->str_), d(), repl(), entity_()
    {assert(o->simple()); /*printf(">>%s %p\n", ~what_, what_.marks);*/}

  Syntax(const SourceStr & s, const Syntax * p) : str_(s), repl(), entity_() {
    d = new D; 
    what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  explicit Syntax(const Syntax * p) : repl(), entity_() {
    d = new D; 
    what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  Syntax(const Syntax * p, const Syntax * x) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
  }
  Syntax(const Syntax * p, const Syntax * x, const Syntax * y) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
  }
  Syntax(const Syntax * p, const Syntax * x, const Syntax * y, const Syntax * z) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
    d->parts.push_back(z);
  }
  Syntax(const SourceStr & s, const Syntax * p, const Syntax * x) : str_(s), repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
  }
  Syntax(const SourceStr & s, const Syntax * p, const Syntax * x, const Syntax * y) : str_(s), repl(0), entity_()  {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
  }
  Syntax(const SourceStr & s, const Syntax * p, const Syntax * x, const Syntax * y, const Syntax * z) : str_(s), repl(0), entity_()  {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
    d->parts.push_back(z);
  }
  Syntax(const Entity * e) : what_("<entity>"), str_(), d(), repl(), entity_(const_cast<Entity*>(e)) 
    {}

  bool simple() const {return !d && !entity_;}
  void make_branch() {
    if (entity_) {
      const Syntax * p = new Syntax(*this);
      d = new D;
      d->parts.push_back(p);
      what_ = String();
      entity_ = NULL;
    } else if (what_.defined()) {
      const Syntax * p = new Syntax(*this);
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
  const Syntax * part(unsigned i) const {
    if (!d) return this;
    else return d->parts[i];
  }
  const Syntax * arg(unsigned i) const {
    return d->parts[i+1];
  }
  const Syntax * & part(unsigned i) {
    return d->parts[i];
  }
  const Syntax * & arg(unsigned i) {
    return d->parts[i+1];
  }
  Parts::const_iterator args_begin() const {return d->parts.begin() + 1;}
  Parts::const_iterator args_end()   const {return d->parts.end();}
  const Syntax * flag(SymbolName n) const {
    if (!d) return NULL;
    else return d->flags.lookup(n);
  }

  void add_part(const Syntax * p) {
    if (!d) make_branch();
    if (d->parts.empty()) what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  void add_args(const Syntax * other) {
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

  void add_flag(const Syntax * p) {
    if (!d) make_branch();
    d->flags.insert(p);
  }
  void set_flags(const Flags & p) {
    if (p.empty()) return;
    if (!d) make_branch();
    d->flags = p;
  }
  void set_flags(const Syntax * p) {
    if (!p->d) return;
    if (!d) make_branch();
    d->flags = p->d->flags;
  }
  void set_src_from_parts() const; // const is a lie

  void print() const;
  bool is_a(const char * n) const {return what_.name == n;}
  bool is_a(String n) const {return what_.name == n;}
  bool is_a(SymbolName n) const {return what_ == n;}
  bool is_a(const char * n, const char * p) const {return what_.name == n && num_args() > 0 && arg(0)->is_a(p);}

  virtual ~Syntax() {}
};

static inline bool operator==(const Syntax & p, const char * str) {
  //printf("%s %d == %s\n", ~p.what(), p.simple(), str);
  if (!p.simple() && !p.part(0)->simple()) return false;
  return p.what().name == str;
}

inline SourceEntity::SourceEntity(const Syntax * e)
  : file_(e->str().source ? e->str().source->file_ : NULL), expansion_(e) {}

inline const Syntax * Flags::lookup(SymbolName d) const {
  for (iterator i = begin(); i != end(); ++i) 
    if ((*i)->what() == d) return *i;
  return NULL;
}

inline bool Flags::insert(const Syntax * p) {
  assert(p->what().defined());
  if (have(p->what())) return false;
  data.push_back(p);
  return true;
}

namespace ast {
  inline SymbolKey::SymbolKey(const Syntax & p, unsigned ns0) 
    : SymbolName(static_cast<const SymbolName &>(p)), ns(ns0) {}

  template <typename T> 
  inline const T * SymbolTable::lookup(const Syntax * p, unsigned ns) {
    return lookup<T>(SymbolKey(*p, ns), p->str());
  }
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
