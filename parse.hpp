#ifndef PARSE__HPP
#define PARSE__HPP

#include <stdio.h>

#include "gc.hpp"
#include "util.hpp"
#include "string_buf.hpp" // FIXME: elim dep
#include "symbol_table.hpp"
#include "type_info.hpp"
#include "source_str.hpp"

// common structure used for parse results

using ast::SymbolName;

namespace ast {
  struct AST;
  class TypeSymbol;
  class TypeInst;
}

struct Syntax;
struct Annon;
struct SyntaxGather;

struct Error {
  typedef ::TypeInfo<Error> TypeInfo;
  const SourceInfo * source;
  const char * pos;
  String msg;
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
  PrintFlags(unsigned i = 0) : indent(i) {}
};

template <typename T>
struct ChangeSrc;

struct Parts : public Vector<const Syntax *> {
  typedef Vector<const Syntax *> Base;
  typedef const value_type * const_iterator;
  typedef value_type * iterator;
  const_iterator begin() const {return &*Base::begin();}
  const_iterator end()   const {return &*Base::end();}
  iterator begin() {return &*Base::begin();}
  iterator end()   {return &*Base::end();}
  void push_back(const Syntax * x) {
    //assert(x); // allow NULL x as placeholder
    Base::push_back(x);
  }
  void append(const Syntax * x) {
    push_back(x);
  }
  void append(const_iterator i, const_iterator end) {
    if (i < end)
      insert(Base::end(), i, end);
  }
  void append(const Parts & other) {
    append(other.begin(), other.end());
  }
  void to_string(OStream & o, PrintFlags f = PrintFlags(), 
                 char sep = ' ', SyntaxGather * = NULL) const;
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
  void to_string(OStream & o, PrintFlags f = PrintFlags(), SyntaxGather * = 0) const;
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
  static const unsigned D_T = 1;
  struct D : public gc_cleanup {
    struct TypeInfo {typedef D type; static const int id = D_T;};
    Parts parts;
    Flags flags;
    D() {}
    template <typename T> D(ChangeSrc<T> & f, const D & o)
      : parts(f, o.parts), flags(f, o.flags) {}
  };
  struct Data : public gc {
    unsigned type_id;
    void * data;
    Data() : type_id(), data() {}
    Data(const Data & o)
      : type_id(o.type_id)
      , data(o.type_id == D_T ? new D(*static_cast<D *>(o.data)) : o.data) 
      {}
    Data & operator=(const Data & o) {
      type_id = o.type_id;
      data = o.type_id == D_T ? new D(*static_cast<D *>(o.data)) : o.data;
      return *this;
    }
    bool empty() const {return type_id == 0;}
    bool have_d() const {return type_id == D_T;}
    bool have_entity() const {return type_id != 0 && type_id != D_T;}
    template <typename T, bool base> struct Set;
    template <typename T> struct Set<T, false> {
      static void f(Data & ths, T * d) {
        ths.type_id = T::TypeInfo::id;
        ths.data = static_cast<typename T::TypeInfo::type *>(d);
      }
    };
    template <typename T> struct Set<T, true> {
      static void f(Data & ths, T * d) {
        d->set_syntax_data(ths);
      }
    };
    template <typename T> void set(T * d) {
      Set<T, (T::TypeInfo::id & 0xFFu) == 0>::f(*this, d);
    }
    template <typename T> Data(T * d) {set(d);}
    template <typename T> Data(const T * d) {set(const_cast<T *>(d));}
    template <typename T> Data & operator=(T * d) {
      set(d);
      return *this;
    }
    template <typename T> bool is_a() const {
      return type_id == T::TypeInfo::id || (type_id & ~0xFFu) == T::TypeInfo::id;
    }
    template <typename T> operator T * () const {
      if (is_a<T>()) 
        return static_cast<typename T::TypeInfo::type *>(data);
      else 
        return NULL;
    }
    D * operator-> () {
      return static_cast<D *>(data);
    }
    const D * operator-> () const {
      return static_cast<const D *>(data);
    }
  };
  SymbolName what_;
  SymbolName what() const {return what_;}
  String as_string() const {assert(simple() || part(0)->simple()); return what_;}
  const SymbolName & as_symbol_name() const {assert(simple() || part(0)->simple()); return what_;}
  operator String () const {return as_string();}
  operator const SymbolName & () const {return as_symbol_name();}
  const char * operator ~ () const {return ~as_string();}
  SymbolName string_if_simple() const {return simple() ? what() : SymbolName();}
  mutable SourceStr str_;
  const SourceStr & str() const {if (d.have_d() && str_.empty()) set_src_from_parts(); return str_;}
  Data d;
  mutable const Syntax * self; // hack, see parts_begin()
  const Replacements * repl;
  bool have_entity() const {return d.have_entity();}
  template <typename T> T * entity() const {return d;}

  Syntax(const Syntax & other) 
    : what_(other.what_), str_(other.str_), d(other.d), repl(other.repl)
  {}
  Syntax(const SourceStr & s, const Syntax & other) 
    : what_(other.what_), str_(s), d(other.d), repl(other.repl)
  {}
  template <typename T> Syntax(ChangeSrc<T> & f, const Syntax & other); // defined in expand.cpp
  Syntax & operator= (const Syntax & other) {
    what_ = other.what_;
    str_ = other.str_;
    d = other.d;
    repl = other.repl;
    return *this;
  }

  Syntax() : d(), repl(0) {}
  explicit Syntax(const char * n) : what_(n), d(), repl() {}
  explicit Syntax(String n) : what_(n), d(), repl() {}
  explicit Syntax(SymbolName n) : what_(n), d(), repl() {}
  Syntax(SymbolName n, const SourceStr & s) 
    : what_(n), str_(s), d(), repl() {}
  Syntax(SymbolName n, const SourceStr & s, const char * e)
    : what_(n), str_(s.source, s.begin, e), d(), repl() {}
  Syntax(SymbolName n, const SourceStr & s, const char * b, const char * e)
    : what_(n), str_(s.source, b, e), d(), repl() {}
  Syntax(const SourceStr & s) 
    : str_(s), d(), repl(0) {}

  Syntax(const Syntax * o, const ast::Mark * m, const SourceInfo * s)
    : what_(ast::mark(o->what_, m)), str_(s, o->str_), d(), repl()
    {assert(o->simple()); /*printf(">>%s %p\n", ~what_, what_.marks);*/}

  Syntax(const Syntax * o, const ast::Marks * m)
    : what_(o->what_.name, m), str_(o->str_), d(), repl()
    {assert(o->simple()); /*printf(">>%s %p\n", ~what_, what_.marks);*/}

  Syntax(const SourceStr & s, const Syntax * p) : str_(s), repl() {
    d = new D; 
    what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  explicit Syntax(const Syntax * p) : repl() {
    d = new D; 
    what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  Syntax(const Syntax * p, const Syntax * x) : repl(0) {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
  }
  Syntax(const Syntax * p, const Syntax * x, const Syntax * y) : repl(0) {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
  }
//   Syntax(const Syntax * p, const Syntax * x, const Syntax * y, const Syntax * z) : repl(0) {
//     d = new D;
//     what_ = p->string_if_simple();
//     d->parts.push_back(p); 
//     d->parts.push_back(x);
//     d->parts.push_back(y);
//     d->parts.push_back(z);
//   }
//   Syntax(const Syntax * p, const Syntax * x, const Syntax * y, const Syntax * z, const Syntax * a) : repl(0) {
//     d = new D;
//     what_ = p->string_if_simple();
//     d->parts.push_back(p); 
//     d->parts.push_back(x);
//     d->parts.push_back(y);
//     d->parts.push_back(z);
//     d->parts.push_back(a);
//   }
//   Syntax(const Syntax * p, const Syntax * x, const Syntax * y, const Syntax * z, const Syntax * a, const Syntax * b) : repl(0) {
//     d = new D;
//     what_ = p->string_if_simple();
//     d->parts.push_back(p); 
//     d->parts.push_back(x);
//     d->parts.push_back(y);
//     d->parts.push_back(z);
//     d->parts.push_back(a);
//     d->parts.push_back(b);
//   }
  Syntax(const SourceStr & s, const Syntax * p, const Syntax * x) : str_(s), repl(0) {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
  }
  Syntax(const SourceStr & s, const Syntax * p, const Syntax * x, const Syntax * y) : str_(s), repl(0)  {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p); 
    d->parts.push_back(x);
    d->parts.push_back(y);
  }
//   Syntax(const SourceStr & s, const Syntax * p, const Syntax * x, const Syntax * y, const Syntax * z) : str_(s), repl(0)  {
//     d = new D;
//     what_ = p->string_if_simple();
//     d->parts.push_back(p); 
//     d->parts.push_back(x);
//     d->parts.push_back(y);
//     d->parts.push_back(z);
//   }
  template <typename T>
  Syntax(const T * e) 
    : what_("<entity>"), str_(e->source_str()), d(e), repl() {}

  template <typename T>
  Syntax(const Syntax * s, const T * e) 
    : what_("<entity>"), str_(s->str()), d(e), repl() {}
  Syntax(const Parts & ps) : repl(0) {
    d = new D;
    d->parts.append(ps);
  }
  Syntax(const Syntax * p, Parts::const_iterator b, Parts::const_iterator e) : repl(0) {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p);
    d->parts.append(b, e);
  }
  Syntax(const Syntax * p, const Parts & ps) : repl(0) {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p);
    d->parts.append(ps);
  }
  Syntax(const Parts & ps, const Parts & ps2) : repl(0) {
    d = new D;
    d->parts.append(ps);
    d->parts.append(ps2);
  }
  Syntax(const Syntax * p, const Parts & ps, const Parts & ps2) : repl(0) {
    d = new D;
    what_ = p->string_if_simple();
    d->parts.push_back(p);
    d->parts.append(ps);
    d->parts.append(ps2);
  }

  bool simple() const {return d.empty();}
  void make_branch() {
    if (have_entity()) {
      const Syntax * p = new Syntax(*this);
      d = new D;
      d->parts.push_back(p);
      what_ = String();
    } else if (what_.defined()) {
      const Syntax * p = new Syntax(*this);
      d = new D;
      d->parts.push_back(p);
    } else {
      d = new D;
    }
  }
  const Syntax * ensure_branch() const {
    if (d.have_d()) return this;
    Syntax * s = new Syntax(*this);
    s->make_branch();
    return s;
  }

  unsigned num_parts() const {
    if (!d.have_d() && what_.defined()) return 1;
    if (!d.have_d()) return 0;
    else return d->parts.size();
  }
  unsigned num_args() const {
    assert(num_parts() > 0);
    return num_parts() - 1;
  }
  const Syntax * part(unsigned i) const {
    if (!d.have_d()) return this;
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
    if (d.have_d()) return d->parts.begin();
    self = this;
    return &self;
  }
  Parts::const_iterator parts_end()   const {
    if (d.have_d()) return d->parts.end();
    self = this;
    if (what_.defined()) return &self + 1;
    else return &self;
  }
  Parts::const_iterator args_begin() const {
    if (d.have_d()) return d->parts.begin() + 1;
    return &self + 1;
  }
  Parts::const_iterator args_end()   const {
    if (d.have_d()) return d->parts.end();
    return &self + 1;
  }
  Parts::iterator args_begin() {
    if (d.have_d()) return d->parts.begin() + 1;
    return &self + 1;
  }
  Parts::iterator args_end() {
    if (d.have_d()) return d->parts.end();
    return &self + 1;
  }
  const Syntax * flag(SymbolName n) const {
    if (!d.have_d()) return NULL;
    else return d->flags.lookup(n);
  }
  Flags::const_iterator flags_begin() const {
    if (d.have_d()) return d->flags.begin();
    else return Flags::EMPTY.begin();
  }
  Flags::const_iterator flags_end()   const {
    if (d.have_d()) return d->flags.end();
    else return Flags::EMPTY.end();
  }

  void add_part(const Syntax * p) {
    if (!d.have_d()) make_branch();
    if (d->parts.empty()) what_ = p->string_if_simple();
    d->parts.push_back(p);
  }
  void add_args(const Syntax * other) {
    if (!d.have_d()) make_branch();
    assert(what_.defined());
    d->parts.append(other->args_begin(), other->args_end());
  }
  void add_parts(Parts::const_iterator i, Parts::const_iterator end) {
    if (i == end) return;
    if (!d.have_d()) make_branch();
    if (d->parts.empty()) what_ = (*i)->string_if_simple();
    d->parts.append(i, end);
  }
  void add_parts(const Parts & p) {
    add_parts(p.begin(), p.end());
  }

  void add_flag(const Syntax * p) {
    if (!d.have_d()) make_branch();
    d->flags.insert(p);
  }
  void add_flags(const Flags & f) {
    if (!d.have_d()) make_branch();
    d->flags.merge(f);
  }
  void add_flags(const Syntax * p) {
    if (!p->d.have_d()) return;
    if (!d.have_d()) make_branch();
    d->flags.merge(p->d->flags);
  }
  void set_flags(const Flags & p) {
    if (p.empty()) return;
    if (!d.have_d()) make_branch();
    d->flags = p;
  }
  void set_flags(const Syntax * p) {
    if (!p->d.have_d()) return;
    if (!d.have_d()) make_branch();
    d->flags = p->d->flags;
  }
  void set_src_from_parts() const; // const is a lie

  void print() const;
  void to_string(OStream & o, PrintFlags f = PrintFlags(), SyntaxGather * = 0) const;
  String to_string() const;
  bool eq(const char * n) const {return simple() && what_.name == n;}
  bool eq(const char * n1, const char * n2) const 
    {return simple() && (what_.name == n1 || what_.name == n2);}
  bool eq(const char * n1, const char * n2, const char * n3) const 
    {return simple() && (what_.name == n1 || what_.name == n2 || what_.name == n3);}
  bool ne(const char * n) const {return !eq(n);}
  bool ne(const char * n1, const char * n2) const {return !eq(n1,n2);}
  bool ne(const char * n1, const char * n2, const char * n3) const 
    {return !eq(n1,n2,n3);}
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
  bool dump_info_self(OStream &) const;
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
