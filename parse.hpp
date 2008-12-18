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

namespace ast {
  struct AST;
  class TypeSymbol;
  class TypeInst;
}

struct Syntax;
struct Annon;

bool pos_str(const SourceFile * source, const char * pos,
             const char * pre, OStream & o, const char * post,
             bool w_source = true);

static inline bool pos_str(const SourceInfo * source, const char * pos,
                           const char * pre, OStream & o, const char * post,
                           bool w_source = true) 
{
  return pos_str(source ? source->file() : NULL, pos, pre, o, post, w_source);
}

String sample(const char * begin, const char * end, unsigned max_len = 20);
static inline String sample(String str, unsigned max_len = 20) {
  return sample(str.begin(), str.end(), max_len);
}

struct SourceStr : public SubStr {
  const SourceInfo * source;
  const SourceInfo * source_block() const {return source ? source->block() : NULL;}
  SourceStr() : source() {}
  SourceStr(const char * b, const char * e) : SubStr(b,e), source() {}
  SourceStr(String s) : SubStr(s), source() {}
  SourceStr(const SourceInfo * f) : SubStr(f->begin(), f->end()), source(f) {}
  SourceStr(const SourceInfo * f, const char * b, const char * e) 
    : SubStr(b,e), source(f) {}
  SourceStr(const SourceInfo * f, String s) 
    : SubStr(s), source(f) {}
  SourceStr(const SourceInfo * f, SubStr s) 
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
    if (source_block() != other.source_block()) return;
    if (!begin || begin > other.begin) begin = other.begin;
    if (!end || end < other.end) end = other.end;
  }
  SourceStr & operator=(const char * s) {begin = s; return *this;}
  bool pos_str(const char * pre, OStream & o, const char * pos) const {
    return ::pos_str(source, begin, pre, o, pos);
  }
  bool end_pos_str(const char * pre, OStream & o, const char * pos) const {
    return ::pos_str(source, end, pre, o, pos, false);
  }
  void sample_w_loc(OStream & o, unsigned max_len = 20) const;
};

struct ErrorInfo : public gc {
  const SourceInfo * source;
  const char * pos;
  String msg;
};

struct Error : public ErrorInfo {
  String message();
  String extra; // extra information after expand backtrace
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
struct Syntax;

struct PrintFlags {
  unsigned indent;
  PrintFlags() : indent(0) {}
};

template <typename T>
struct ChangeSrc;

struct Parts : public Vector<const Syntax *> {
  typedef Vector<const Syntax *> Base;
  typedef const value_type * const_iterator;
  const_iterator begin() const {return &*Base::begin();}
  const_iterator end()   const {return &*Base::end();}
  void push_back(const Syntax * x) {
    assert(x);
    Base::push_back(x);
  }
  void append(const Syntax * x) {
    push_back(x);
  }
  void append(const_iterator i, const_iterator end) {
    insert(Base::end(), i, end);
  }
  void append(const Parts & other) {
    append(other.begin(), other.end());
  }
  void to_string(OStream & o, PrintFlags f = PrintFlags(), char sep = ' ') const;
  Parts() {}
  template <typename T> Parts(ChangeSrc<T> &, const Parts & o);
};

struct Flags : public gc {
  static Vector<const Syntax *> EMPTY;
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
  void to_string(OStream & o, PrintFlags f = PrintFlags()) const;
  Flags() {}
  template <typename T> Flags(ChangeSrc<T> &, const Flags & o);
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
    D() {}
    template <typename T> D(ChangeSrc<T> & f, const D & o)
      : parts(f, o.parts), flags(f, o.flags) {}
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
  mutable const Syntax * self; // hack, see parts_begin()
  const Replacements * repl;
  Entity * entity_;
  Entity * entity() const {return entity_;}

  Syntax(const Syntax & other) 
    : what_(other.what_), str_(other.str_), d(other.d ? new D(*other.d) : 0), repl(other.repl), entity_(other.entity_)
  {}
  template <typename T> Syntax(ChangeSrc<T> & f, const Syntax & other); // defined in expand.cpp
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

  Syntax(const Syntax * o, const ast::Mark * m, const SourceInfo * s)
    : what_(ast::mark(o->what_, m)), str_(s, o->str_), d(), repl(), entity_()
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
  Syntax(const Syntax * p, const Syntax * x, const Syntax * y, const Syntax * z, const Syntax * a) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
    d->parts.push_back(z);
    d->parts.push_back(a);
  }
  Syntax(const Syntax * p, const Syntax * x, const Syntax * y, const Syntax * z, const Syntax * a, const Syntax * b) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
    d->parts.push_back(z);
    d->parts.push_back(a);
    d->parts.push_back(b);
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
  Syntax(const Entity * e) 
    : what_("<entity>"), str_(), d(), repl(), entity_(const_cast<Entity*>(e)) {}
  Syntax(const Syntax * s, const Entity * e) 
    : what_("<entity>"), str_(s->str()), d(), repl(), entity_(const_cast<Entity*>(e)) {}
  inline Syntax(const ast::AST * e);
  Syntax(const Parts & ps) : repl(0), entity_() {
    d = new D;
    d->parts.append(ps);
  }
  Syntax(const Syntax * p, Parts::const_iterator b, Parts::const_iterator e) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p);
    d->parts.append(b, e);
  }
  Syntax(const Syntax * p, const Parts & ps) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p);
    d->parts.append(ps);
  }
  Syntax(const Parts & ps, const Parts & ps2) : repl(0), entity_() {
    d = new D;
    d->parts.append(ps);
    d->parts.append(ps2);
  }
  Syntax(const Syntax * p, const Parts & ps, const Parts & ps2) : repl(0), entity_() {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p);
    d->parts.append(ps);
    d->parts.append(ps2);
  }

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
  const Syntax * ensure_branch() const {
    if (d) return this;
    Syntax * s = new Syntax(*this);
    s->make_branch();
    return s;
  }

  unsigned num_parts() const {
    if (!d && what_.defined()) return 1;
    if (!d) return 0;
    else return d->parts.size();
  }
  unsigned num_args() const {
    assert(num_parts() > 0);
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
  Parts::const_iterator parts_begin() const {
    if (d) return d->parts.begin();
    self = this;
    return &self;
  }
  Parts::const_iterator parts_end()   const {
    if (d) return d->parts.end();
    self = this;
    if (what_.defined()) return &self + 1;
    else return &self;
  }
  Parts::const_iterator args_begin() const {
    if (d) return d->parts.begin() + 1;
    return &self + 1;
  }
  Parts::const_iterator args_end()   const {
    if (d) return d->parts.end();
    return &self + 1;
  }
  const Syntax * flag(SymbolName n) const {
    if (!d) return NULL;
    else return d->flags.lookup(n);
  }
  Flags::const_iterator flags_begin() const {
    if (d) return d->flags.begin();
    else return Flags::EMPTY.begin();
  }
  Flags::const_iterator flags_end()   const {
    if (d) return d->flags.end();
    else return Flags::EMPTY.end();
  }

  void add_part(const Syntax * p) {
    if (!d) make_branch();
    if (d->parts.empty()) what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  void add_args(const Syntax * other) {
    if (!d) make_branch();
    assert(what_.defined());
    d->parts.append(other->args_begin(), other->args_end());
  }
  void add_parts(Parts::const_iterator i, Parts::const_iterator end) {
    if (i == end) return;
    if (!d) make_branch();
    if (d->parts.empty()) what_ = (*i)->string_if_simple();
    d->parts.append(i, end);
  }
  void add_parts(const Parts & p) {
    add_parts(p.begin(), p.end());
  }

  void add_flag(const Syntax * p) {
    if (!d) make_branch();
    d->flags.insert(p);
  }
  void add_flags(const Flags & f) {
    if (!d) make_branch();
    d->flags.merge(f);
  }
  void add_flags(const Syntax * p) {
    if (!p->d) return;
    if (!d) make_branch();
    d->flags.merge(p->d->flags);
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
  void to_string(OStream & o, PrintFlags f = PrintFlags()) const;
  String to_string() const;
  bool is_a(const char * n) const {return what_.name == n;}
  bool is_a(String n) const {return what_.name == n;}
  bool is_a(SymbolName n) const {return what_ == n;}
  bool is_a(const char * n, const char * p) const {return what_.name == n && num_args() > 0 && arg(0)->is_a(p);}
  void sample_w_loc(OStream & o, unsigned max_len = 20) const;
  String sample_w_loc(unsigned max_len = 20) const;
  virtual ~Syntax() {}
};

static inline bool operator==(const Syntax & p, const char * str) {
  //printf("%s %d == %s\n", ~p.what(), p.simple(), str);
  if (!p.simple() && !p.part(0)->simple()) return false;
  return p.what().name == str;
}

inline const Syntax * Flags::lookup(SymbolName d) const {
  // FIXME: Handle marks correctly rather than ignoring them
  for (iterator i = begin(); i != end(); ++i) 
    if ((*i)->what().name == d.name) return *i;
  return NULL;
}

inline bool Flags::insert(const Syntax * p) {
  assert(p->what().defined());
  if (have(p->what())) return false;
  data.push_back(p);
  return true;
}

struct ParseSourceInfo : public SourceInfo {
  SourceStr str;
  String what;
  ParseSourceInfo(const SourceStr & s, String w) 
    : SourceInfo(s.source), str(s), what(w) {}
  void dump_info(OStream &, const char * prefix) const;
};

namespace ast {
  inline SymbolKey::SymbolKey(const Syntax & p, const InnerNS * ns0) 
    : SymbolName(static_cast<const SymbolName &>(p)), ns(ns0 ? ns0 : DEFAULT_NS) {}


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
