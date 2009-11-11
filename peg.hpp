#include <set> // FIXME: Should't need to include here

#include "gc.hpp"
#include "parse.hpp"
#include "parse_common.hpp"
#include "hash-t.hpp"
#include "syntax-f.hpp"

#include <typeinfo>
#include <utility>

using std::pair;

struct Prod;

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
  Error * to_error(const SourceInfo *, const SourceFile * grammer);
  void print(const SourceInfo * file, const SourceFile * grammer);
};

// Some cached productions are persistent between parses, which saves
// time, when reparsing.  However, in order to use the cached result
// two condations need to hold:
//   1) The end of the string being parsed is past the last character
//      read (for any reason) of the cached result
///  2) If the cached result read past the end of the string, the end of the 
//      new string being parsed string is at the same position of the
//      cached result.
// To make sure (1) holds, we keep track of the last character read
// with "read_to".  It is important that this is really the last
// character read and not one-past as with a typical end pointer,
// otherwise we couldn't tell the difference between reading the last
// character, and reading past the end of the string.  (2) is a
// special case.  For (2) we set read_to to NULL and store the current
// end-of-string position in str_end.

struct MatchRes {
  const char * end;     // NULL on FALSE
  const char * read_to; // last charater read, _not_ one past, if NULL
                        // then either 1) no characters are read or 2)
                        // read past the end.  (2) is only holds when
                        // used as a cached result and str_end is
                        // non-NULL (see Res below)
  explicit MatchRes(const char * e) : end(e), read_to(e) {}
  MatchRes() {}
  MatchRes(const char * e, const char * r) : end(e), read_to(r) {}
  operator bool() const {return end;}
  bool operator!() const {return !end;}
};

typedef SyntaxBuilderBase SynBuilder;

struct Res : public MatchRes {
  const char * str_end; // if not NULL than read past end-of-string to this point
  Res() : str_end() {}
  SyntaxBuilderN<2> res;
  ParseErrors errors;
};

struct CaptureType {
  enum Type {Explicit, Implicit, None};
  operator Type () const {return type;}
  CaptureType(Type t = None) : type(t) {}
  bool is_explicit() const {return type == Explicit;}
  bool is_implicit() const {return type == Implicit;}
  bool is_none()     const {return type == None;}
  bool is_any()      const {return type != None;}
  void set_to_explicit() {type = Explicit;}
  void set_to_implicit() {type = Implicit;}
  void set_to_none()     {type = None;}
private:
  Type type;
};

static const char * FAIL = 0;

class Prod {
public:
  virtual MatchRes match(SourceStr str, SynBuilder * parts, ParseErrors & errs) = 0;
  virtual Prod * clone(Prod * = 0) = 0; // the paramater is the new
                                        // prod to use in place of the
                                        // placeholder prod "_self"
  virtual void end_with(Prod * p) {}
  virtual void dump() {}
  virtual void verify() {}
  bool persistent() const {if (persistent_ == -1) set_persistent(); return persistent_;}
  virtual void set_persistent() const {}
  virtual ~Prod() {}
  Prod(const char * p, const char * e) : pos(p), end(e), persistent_(-1) {}
public: // but don't use
  CaptureType capture_type;
  const char * pos;
  const char * end;
protected:
  mutable int persistent_;
};

class SymProd : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, ParseErrors & errs) {
    return prod->match(str,parts,errs);
  }
  SymProd(const char * s, const char * e, String n, Prod * p = 0) 
    : Prod(s,e), name(n) 
  {
    if (p) set_prod(p);
    capture_type.set_to_implicit();
  }
  void set_persistent() const {persistent_ = prod->persistent();}
  void set_prod(Prod * p) {
    prod = p;
    //if (p->capture_type.is_any())
  }
  void verify() {
    //assert(prod->capture_type.is_implicit() && prod->capture_type.is_single());
  }
  SymProd(const SymProd & other, Prod * p = 0) : Prod(other), name(other.name) 
  {
    if (name == "_self")
      set_prod(p);
    else
      set_prod(other.prod->clone(p));
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
  MatchRes match(SourceStr str, SynBuilder * parts, ParseErrors & errs);
  NamedProd(String n) 
    : SymProd(0,0,n) {}
  virtual Prod * clone(Prod *) {
    return this; // don't copy prod with name
  }
  void dump() {printf("%s", name.c_str());}
  void clear_cache() {lookup.clear();}
private:
  typedef hash_multimap<const char *, Res, hash<void *> > Lookup;
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

    void clear_cache();

    void top(const char * str, const char * end);

    Res peg(const char * str, const char * end, char eos, 
            bool & explicit_capture,
            bool capture = false, bool implicit = true);
    Res sequence(const char * str, const char * end, char eos, bool & explicit_capture);
    Res sequence2(const char * str, const char * end, bool & explicit_capture);
    Res desc_label(const char * str, const char * end);
    Res prefix(const char * str, const char * end, bool & explicit_capture);
    Res suffix(const char * str, const char * end, bool & explicit_capture);
    Res primary(const char * str, const char * end, bool & explicit_capture);
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
