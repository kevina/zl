#include <set> // FIXME: Should't need to include here

#include "gc.hpp"
#include "parse.hpp"
#include "parse_common.hpp"
#include "hash-t.hpp"

#include <typeinfo>
#include <utility>

using std::pair;

struct Prod;

struct Syntax;

// FIXME: Need more effect representation for const strings
//        (Pool of strings, hash lookup, ...)

// FIXME: Need an optimization pass to eliminate unnecessary "Capture"s

struct ParseError {
  const char * grammer_pos;
  const char * pos;
  String expected; 
  ParseError(const char * gp, const char * p, String e)
    : grammer_pos(gp), pos(p), expected(e) 
    {
      //printf(">>ERROR %p: Expected %s<<\n", pos, e.c_str());
    }
};

struct ParseErrors : public Vector<const ParseError *> {
  void add(const ParseError * err) {
    if (empty() || front()->pos == err->pos) {
      push_unique(err);
    } else if (front()->pos < err->pos) {
      clear();
      push_unique(err);
    } else {
      // nothing to do
    }
  }
  void add(const ParseErrors & other) {
    if (other.empty()) {
      // nothing to do;
    } else if (empty() || front()->pos == other.front()->pos) {
      for (const_iterator i = other.begin(); i != other.end(); ++i)
        push_unique(*i);
    } else if (front()->pos < other.front()->pos) {
      *this = other;
    } else {
      // nothing to do;
    }
  }
  void push_unique(const ParseError * err) {
    for(const_iterator i = begin(); i != end(); ++i) {
      if ((*i)->expected == err->expected)
        return;
    }
    push_back(err);
  }
  Error * to_error(const SourceFile *, const SourceFile * grammer);
  void print(const SourceFile * file, const SourceFile * grammer);
};

struct Res {
  const char * end; // NULL on FALSE
  Parts parts;
  Flags flags;
  ParseErrors errors;
};

struct CaptureType {
  enum Type {Explicit, ExplicitMulti, Implicit, None};
  operator Type () const {return type;}
  CaptureType(Type t = None) : type(t) {}
  bool is_explicit() const {return type == Explicit || type == ExplicitMulti;}
  bool is_implicit() const {return type == Implicit;}
  bool is_none()     const {return type == None;}
  bool is_single()   const {return type == Explicit || type == Implicit;}
  bool is_multi()    const {return type == ExplicitMulti;}
  void set_to_explicit(bool single = true) {type = single ? Explicit : ExplicitMulti;}
  void set_to_implicit()              {type = Implicit;}
  void set_to_none()                  {type = None;}
private:
  Type type;
};

static const char * FAIL = 0;

struct PartsFlags {
  Parts * parts;
  Flags * flags;
  PartsFlags()
    : parts(), flags() {}
  PartsFlags(Parts * p, Flags * f)
    : parts(p), flags(f) {}
  operator bool() const {return parts;}
};

class Prod : public gc_cleanup {
public:
  virtual const char * match(SourceStr str, PartsFlags parts, ParseErrors & errs) = 0;
  virtual Prod * clone(Prod * = 0) = 0; // the paramater is the new
                                        // prod to use in place of the
                                        // placeholder prod "_self"
  virtual void end_with(Prod * p) {}
  virtual void dump() {}
  virtual void verify() {}
  virtual ~Prod() {}
  Prod(const char * p, const char * e) : pos(p), end(e) {}
public: // but don't use
  CaptureType capture_type;
  const char * pos;
  const char * end;
};

class SymProd : public Prod {
public:
  const char * match(SourceStr str, PartsFlags parts, ParseErrors & errs) {
    return prod->match(str,parts,errs);
  }
  SymProd(const char * s, const char * e, String n, Prod * p = 0) 
    : Prod(s,e), name(n), prod(p) {
    capture_type.set_to_implicit();
  }
  void set_prod(Prod * p) {prod = p;}
  void verify() {
    //assert(prod->capture_type.is_implicit() && prod->capture_type.is_single());
  }
  SymProd(const SymProd & other, Prod * p = 0) : Prod(other), name(other.name) 
  {
    if (name == "_self")
      set_prod(p);
    else
      prod = other.prod->clone(p);
  }
  virtual Prod * clone(Prod * p) {return new SymProd(*this, p);} 
  void dump() {printf("%s", name.c_str());}
public: // but treat as protected
  String name;
//protected:
  Prod * prod;
};

struct PtrLt {
  bool operator() (const char * x, const char * y) const {return x < y;}
};

class NamedProd : public SymProd {
  // named productions are memorized
public:
  const char * match(SourceStr str, PartsFlags parts, ParseErrors & errs) {
    Lookup::iterator i = lookup.find(str.begin);
    Res * r;
    if (i == lookup.end()) {
      r = &lookup[str];
      r->end = FAIL; // to avoid infinite recursion
      r->end = prod->match(str, PartsFlags(&r->parts, &r->flags), r->errors);
      //assert(r->end == FAIL || r->parts.size() == 1);
    } else {
      r = &i->second;
    }
    errs.add(r->errors);
    if (parts) {
      parts.parts->append(r->parts);
      parts.flags->merge(r->flags);
    } 
    return r->end;
  };
  NamedProd(String n) 
    : SymProd(0,0,n) {}
  virtual Prod * clone(Prod *) {
    return this; // don't copy prod with name
  }
  void dump() {printf("%s", name.c_str());}
  void clear_cache() {lookup.clear();}
private:
  typedef hash_map<const char *, Res, hash<void *> > Lookup;
  Lookup lookup;
};

class TokenProd : public SymProd {
public:
  TokenProd(const char * s, const char * e, String n, Prod * p = 0) 
    : SymProd(s, e, n, p) {}
  TokenProd(const TokenProd & o, Prod * p = 0) : SymProd(o,p) {}
  virtual Prod * clone(Prod * p) {return new TokenProd(*this, p);}
  void dump() {printf("\"%s\"", name.c_str());}
};

namespace ParsePeg {

  using namespace parse_common;
  using parse_common::symbol;

  struct Res {
    const char * end;
    Prod * prod;
    Res() {}
    Res(const char * e, Prod * p) : end(e), prod(p) {}
    Res(Prod * p) : end(p->end), prod(p) {assert(end);}
  };

  template <typename T>
  struct Sym {
    const char * pos;
    T * prod;
    Sym(const char * p1, T * p2) : pos(p1), prod(p2) {}
  };

  struct TokenRule {
    Prod * to_match;
    Prod * if_matched;
    TokenRule(Prod * p1, Prod * p2) : to_match(p1), if_matched(p2) {}
  };

  class Parse {
  public:
    const char * begin;

    hash_map<String, NamedProd *> named_prods;
    Vector< Sym<NamedProd> > unresolved_syms;
    Vector< Sym<TokenProd> > token_syms;
    Vector<TokenRule>        token_rules;

    void clear_cache() {
      hash_map<String, NamedProd *>::iterator i = named_prods.begin(), e = named_prods.end();
      for (; i != e; ++i)
        i->second->clear_cache();
    }

    void top(const char * str, const char * end);

    Res peg(const char * str, const char * end, char eos, bool capture = false, bool implicit = true);
    Res sequence(const char * str, const char * end, char eos);
    Res sequence2(const char * str, const char * end);
    Res desc_label(const char * str, const char * end);
    Res prefix(const char * str, const char * end);
    Res suffix(const char * str, const char * end);
    Res primary(const char * str, const char * end);
    Res literal(const char * str, const char * end);
    Res token(const char * str, const char * end);
    Res char_class(const char * str, const char * end);
    Res identifier(const char * str, const char * end);

    void resolve_token_symbol(TokenProd *, const char *);
  };

}

// FIXME: No global
extern ParsePeg::Parse parse;
extern SourceFile * file;

struct Replacements;
const Syntax * parse_str(String what, SourceStr str, const Replacements * repls = 0);

// FIXME: No global
//extern ReplTable * mids;
