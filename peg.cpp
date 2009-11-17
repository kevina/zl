//#include <sys/types.h>
//#include <sys/stat.h>
//#include <fcntl.h>
#include <assert.h>
#include <time.h>
#include <stdlib.h>

#include <algorithm>
#include <functional>
#include <set>

#include "peg.hpp"

#include "parse.hpp"
#include "parse_common.hpp"
#include "charset.hpp"
#include "vector.hpp"
#include "expand.hpp"

#include "hash-t.hpp"


//#define DUMP_PERFORMANCE_INFO

#ifdef DUMP_PERFORMANCE_INFO
#  define pprintf(...) printf(__VA_ARGS__)
#else
#  define pprintf
#endif

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
};


static const char * FAIL = 0;

//class Prod {
//  virtual ~Prod() = 0;
//};

enum CaptureType {NoCapture, CanGiveCapture, ExplicitCapture, ReparseCapture};

class Prod;

struct PersistentRes {
  // "val" is only final if deps is empty
  bool val;
  Vector<Prod *> deps;
  PersistentRes(bool v = true) : val(v) {}
  PersistentRes(Prod * d) : val(true), deps() {deps.push_back(d);}
  bool have_dep(Prod * p) const {
    for (Vector<Prod *>::const_iterator 
           i = deps.begin(), e = deps.end(); i != e; ++i)
      if (*i == p) return true;
    return false;
  }
  void add_dep(Prod * p) {
    if (!have_dep(p)) deps.push_back(p);
  }
  void remove_dep(Prod * p) {
    for (Vector<Prod *>::iterator 
           i = deps.begin(), e = deps.end(); i != e; ++i) {
      if (*i == p) {
        deps.erase(i);
        return;
      }
    }
  }
  void operator |= (const PersistentRes & o) {
    if (o.val == false) val = false;
    for (Vector<Prod *>::const_iterator 
           i = o.deps.begin(), e = o.deps.end(); i != e; ++i)
      add_dep(*i);
  }
};

class Prod {
public:
  virtual MatchRes match(SourceStr str, SynBuilder * parts) = 0;
  // FIXME: explain match_f: ...
  virtual const char * match_f(SourceStr str, ParseErrors & errs) = 0;
  virtual Prod * clone(Prod * = 0) = 0; // the paramater is the new
                                        // prod to use in place of the
                                        // placeholder prod "_self"
  virtual void end_with(Prod * p) {}
  virtual void dump() {}
  virtual void verify() {}
  bool persistent() const {assert(persistent_ >= 0); return persistent_;}
  PersistentRes calc_persistent() {
    if (persistent_ >= 0) return PersistentRes(persistent_);
    if (persistent_ == -2) return PersistentRes(this);
    persistent_ = -2;
    PersistentRes r = calc_just_persistent();
    r.remove_dep(this);
    if (r.deps.empty()) 
      persistent_ = r.val;
    else
      persistent_ = -1; // need to try again
    return r;
  }
  virtual PersistentRes calc_just_persistent() = 0;
  virtual ~Prod() {}
  virtual int first_char() const {return -2;}
  Prod(const char * p, const char * e) 
    : capture_type(NoCapture), pos(p), end(e), persistent_(-1) {}
public: // but don't use
  CaptureType capture_type;
  const char * pos;
  const char * end;
protected:
  int persistent_;
};

enum CaptureQ {DontCapture, DoCapture};

namespace ParsePeg {struct Res;}

struct ProdWrap {
  Prod * prod;
  bool capture;
  ProdWrap() : prod(NULL), capture(false) {}
  ProdWrap(Prod * p, CaptureQ c) : prod(p), capture(c) {}
  void get_capture_from_prod() {
    capture = prod->capture_type >= ExplicitCapture;}
  explicit ProdWrap(Prod * p) : prod(p) {get_capture_from_prod();}
  //void operator=(const Prod * p) {prod = p; get_capture_from_prod();}
  MatchRes match(SourceStr str, SynBuilder * parts) {
    return prod->match(str, capture ? parts : NULL);}
  const char * match_f(SourceStr str, ParseErrors & errs) {
    return prod->match_f(str, errs);}
  ProdWrap clone(Prod * o) const {return ProdWrap(prod->clone(o), capture);}
  void verify() {prod->verify();}
  bool persistent() const {return prod->persistent();}
  PersistentRes calc_persistent() {return prod->calc_persistent();}
  int first_char() const {return prod->first_char();}
private:
  ProdWrap(Prod * p, bool c) : prod(p), capture(c) {}
};

typedef Prod ProdImpl;

class SymProd : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder * parts);
  const char * match_f(SourceStr str, ParseErrors & errs);
  SymProd(const char * s, const char * e, String n) 
    : Prod(s,e), name(n) {capture_type = CanGiveCapture;}
  SymProd(const char * s, const char * e, String n, const ProdWrap & p) 
    : Prod(s,e), name(n) {capture_type = CanGiveCapture; set_prod(p);}
  PersistentRes calc_just_persistent() {return prod.calc_persistent();}
  int first_char() const {return prod.first_char();}
  void set_prod(const ProdWrap & p) {prod = p;}
  void verify() {
    //assert(prod->capture_type.is_implicit() && prod->capture_type.is_single());
  }
  SymProd(const SymProd & other, Prod * p = 0) : Prod(other), name(other.name) 
  {
    if (name == "_self")
      set_prod(ProdWrap(p));
    else
      set_prod(other.prod.clone(p));
  }
  virtual Prod * clone(Prod * p) {return new SymProd(*this, p);} 
  void dump() {printf("%s", name.c_str());}
public: // but treat as protected
  String name;
//protected:
  ProdWrap prod;
};

struct PtrLt {
  bool operator() (const char * x, const char * y) const {return x < y;}
};

class NamedProd : public SymProd {
public:
  NamedProd(String n) : SymProd(0, 0, n) {}
  NamedProd(const NamedProd & o, Prod * p = 0) : SymProd(o,p) {}
  virtual Prod * clone(Prod *) {
    return this; // don't copy prod with name
  }
  void dump() {printf("%s", name.c_str());}
  virtual void finalize() {
    calc_persistent();
  }
  virtual void clear_cache() {}
};

class CachedProd : public NamedProd {
  // named productions are memorized
public:
  MatchRes match(SourceStr str, SynBuilder * parts);
  const char * match_f(SourceStr str, ParseErrors & errs);
  CachedProd(String n) : NamedProd(n), first_char_(-3) {/*cs = (char *) GC_MALLOC(256);*/}
  void clear_cache() {lookup.clear();}
  int first_char() const {return first_char_ > -3 ? first_char_ : prod.first_char();}
  void finalize() {
    NamedProd::finalize();
    first_char_ = prod.first_char();
  }
private:
  typedef hash_multimap<const char *, Res, hash<void *> > Lookup;
  Lookup lookup;
  int first_char_;
  //CharSet cs;
};

class TokenProd : public SymProd {
public:
  TokenProd(const char * s, const char * e, String n) 
    : SymProd(s, e, n) {}
  TokenProd(const TokenProd & o, Prod * p = 0) : SymProd(o,p) {}
  virtual Prod * clone(Prod * p) {return new TokenProd(*this, p);}
  void dump() {printf("\"%s\"", name.c_str());}
};

namespace ParsePeg {

  using namespace parse_common;
  using parse_common::symbol;

  enum Context {NormalContext, ReparseContext};
  enum CaptureNeed {DontNeedCapture, NeedCapture, NeedReparseCapture};

  struct Res : public ProdWrap {
    const char * end;
    Res() {}
    Res(const char * e, Prod * p, CaptureQ c) : ProdWrap(p, c), end(e) {}
    Res(const char * e, Prod * p) : ProdWrap(p), end(e) {}
    Res(const char * e, const ProdWrap & p) : ProdWrap(p), end(e) {}
    Res(Prod * p, CaptureQ c) : ProdWrap(p, c), end(p->end) {assert(end);}
    explicit Res(Prod * p) : ProdWrap(p), end(p->end) {assert(end);}
    explicit Res(const ProdWrap & p) : ProdWrap(p), end(p.prod->end) {assert(end);}
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
    hash_set<String> trans_prods;
    Vector< Sym<NamedProd> > unresolved_syms;
    Vector< Sym<TokenProd> > token_syms;
    Vector<TokenRule>        token_rules;

    void clear_cache();

    void top(const char * str, const char * end);

    Res peg(const char * str, const char * end, char eos, CaptureNeed = DontNeedCapture);
    Res sequence(const char * str, const char * end, char eos);
    Res sequence2(const char * str, const char * end, Context);
    Res desc_label(const char * str, const char * end);
    Res prefix(const char * str, const char * end, Context);
    Res suffix(const char * str, const char * end, Context);
    Res primary(const char * str, const char * end, Context);
    Res literal(const char * str, const char * end);
    Res token(const char * str, const char * end);
    Res char_class(const char * str, const char * end);
    Res identifier(const char * str, const char * end);

    void resolve_token_symbol(TokenProd *, const char *);

    NamedProd * new_named_prod(String n) {
      if (trans_prods.have(n)) {
        return new NamedProd(n);
      } else {
        return new CachedProd(n);
      }
    };

    void parse_hints_file(const char * str, const char * end);
  };

}

ParsePeg::Parse parse;
SourceFile * file = NULL;

void parse_peg(const char * fn) {
  file = new_source_file(fn);
  try {
    parse.top(file->begin(), file->end());
  } catch (Error * err) {
    err->source = file;
    throw err;
  }
}

namespace ParsePeg {class Parse;}

//namespace Peg {

class Capture;

static unsigned indent_level = 0;

MatchRes SymProd::match(SourceStr str, SynBuilder * parts) {
  if (!parts) return prod.match(str,NULL);
  MatchRes r = prod.match(str,parts);
  if (!r) return r;
  if (!prod.capture)
    parts->add_part(SYN(String(str.begin, r.end), str, r.end));
  return r;
}

const char * SymProd::match_f(SourceStr str, ParseErrors & errs) {
  return prod.match_f(str,errs);
}

MatchRes CachedProd::match(SourceStr str, SynBuilder * parts) {
  //printf("%*cMATCH %s\n", indent_level, ' ', ~name);
  if (first_char_ >= 0 && 
      (str.begin == str.end || first_char_ != *str.begin)) {
    //printf("%s FAST FAIL ON '%c'\n", ~name, prod.first_char());
    return MatchRes(FAIL, str.begin);
  }
  //if (cs[(unsigned char)*str.begin]) abort();
  //if (prod.first_char() >= 0)
  //  printf("%s COULD FAST FAIL ON '%c'\n", ~name, prod.first_char());
  pair<Lookup::iterator, Lookup::iterator>
    cached = lookup.equal_range(str.begin);
  //if (cached.second - cached.first > 1) 
  //  printf("MORE THAN ONE in %s\n", ~name);
  Lookup::iterator i = cached.first;
  //int num = -1;
  for (; i != cached.second; ++i) {
    //++num;
    //if (num > 1) printf("XXX %s %d\n", ~name, num);
    if (i->second.str_end) 
      if (str.end == i->second.str_end) break;
      else continue;
    if (str.end > i->second.read_to) break;
    else {
      //printf("XXX SHOULD'T USE %s %p %p !> %p \"%s\"\n", ~name, str.begin, str.end, i->second.read_to, ~sample(str.begin, i->second.read_to+1));
      //break;
    }
  }
  Res * r;
  if (i == cached.second) {
    r = &(*((lookup.insert(str.begin).first))).second;
    r->end = FAIL; // to avoid infinite recursion
    Res & r0 = *r;
    //Res r0;
    pprintf("%*cNamedProd MISS %s %p %p\n", indent_level, ' ', ~name, str.begin, str.end);
    //if (prod->capture_type.is_none())
    //  printf("NP: %s: %d %d\n", ~name, (bool)parts, (CaptureType::Type)prod->capture_type);
    indent_level++;
    *static_cast<MatchRes *>(&r0) = prod.match(str, &r0.res);
    indent_level--;
    //if (!r0) {
    //  assert(r0.res.empty());
    //  errs.add(r0.errors);
    //  return r0;
    //}
    //assert(r0.errors.empty());
    //r = &(*((lookup.insert(str, r0)).first)).second;
    if (!r0.res.empty()) assert(!r->res.empty());
    // XXX: MEGA HACK!
    //if (name == "SPACING")
    //  r->read_to = str.begin - 1;
    if (r->read_to >= str.end) {
      //printf("PAST END ON %s\n", ~name);
      r->read_to = NULL;
      r->str_end = str.end;
    }
    if (r->end != FAIL)
      pprintf("%*cNamedProd DONE %s %p (%p %p) \"%s\" %p\n", indent_level, ' ', ~name, str.begin, r->end, r->read_to, ~sample(str.begin, r->end), str.end);
    else
      pprintf("%*cNamedProd DONE %s %p (FAIL %p) %p\n", indent_level, ' ', ~name, str.begin, r->read_to, str.end);
    //assert(r->end == FAIL || r->parts.size() == 1);
  } else {
    r = &i->second;
    if (r->end != FAIL)
      pprintf("%*cNamedProd HIT %s %p (%p) %p\n", indent_level, ' ', ~name, str.begin, r->end, str.end);
    else
      pprintf("%*cNamedProd HIT %s %p (FAIL) %p\n", indent_level, ' ', ~name, str.begin, str.end);
  }
  if (parts) {
    if (!prod.capture && *r)
      r->res.add_part(SYN(String(str.begin, r->end), str, r->end));
    parts->add_parts(r->res.parts_begin(), r->res.parts_end());
    parts->merge_flags(r->res.flags_begin(), r->res.flags_end());
  }
  //printf("%*cMATCH %s RES = %p\n", indent_level, ' ', ~name, r->end);
  return *r;
}

const char * CachedProd::match_f(SourceStr str, ParseErrors & errs) {
  pair<Lookup::iterator, Lookup::iterator>
    cached = lookup.equal_range(str.begin);
  Lookup::iterator i = cached.first;
  for (; i != cached.second; ++i) {
    if (i->second.str_end) 
      if (str.end == i->second.str_end) break;
      else continue;
    if (str.end > i->second.read_to) break;
  }
  if (i == cached.second || i->second.end == FAIL) {
    return prod.match_f(str,errs);
  } else {
    return i->second.end;
  }
}

void ParsePeg::Parse::clear_cache() {
  pprintf("NamedProd CLEAR\n");
  hash_map<String, NamedProd *>::iterator i = named_prods.begin(), e = named_prods.end();
  for (; i != e; ++i) {
    if (!i->second->persistent())
      i->second->clear_cache();
  }
}

class AlwaysTrue : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder *) {
    return MatchRes(str.begin, NULL);
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    return str.begin;
  }
  AlwaysTrue(const char * p, const char * e) : Prod(p,e) {}
  virtual Prod * clone(Prod *) {return new AlwaysTrue(*this);}
  PersistentRes calc_just_persistent() {return PersistentRes(true);}
  void dump() {printf(" _TRUE ");}
};

class Capture : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder * res) {
    if (!res) return prod->match(str, NULL);
    MatchRes r = prod->match(str, NULL);
    if (!r) return r;
    Syntax * parse = SYN(String(str.begin, r.end), str, r.end);
    //printf("NONE: %s\n", ~parse->to_string());
    res->add_part(parse);
    return r;
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    return prod->match_f(str, errs);
  }
  Capture(const char * s, const char * e, Prod * p)
    : Prod(s,e), prod(p) {capture_type = ExplicitCapture;}
  Capture(Prod * p)
    : Prod(p->pos, p->end), prod(p) {capture_type = ExplicitCapture;}
  PersistentRes calc_just_persistent() {return prod->calc_persistent();}
  int first_char() const {return prod->first_char();}
  void verify() {
    // FIXME: Make error message
    //assert(prod->capture_type.is_none() || prod->capture_type.is_single());
    prod->verify();
  }
  Capture(const Capture & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)) {}
  virtual Capture * clone(Prod * p) {return new Capture(*this, p);}
  void dump() {printf("{"); prod->dump(); printf("}");}
protected:
  Prod * prod;
};

class ReparseInner : public Capture {
public:
  MatchRes match(SourceStr str, SynBuilder * res) {
    if (!res) return prod->match(str, NULL);
    MatchRes r = prod->match(str, NULL);
    if (!r) return r;
    Syntax * parse = new ReparseSyntax(SourceStr(str, r.end));
    //printf("REPARSE_CAPTURE with %p\n", parse);
    res->add_part(parse);
    return r;
  }
  ReparseInner(const char * s, const char * e, Prod * p)
    : Capture(s,e,p) {capture_type = ReparseCapture;}
  PersistentRes calc_just_persistent() {return prod->calc_persistent();}
  ReparseInner(const ReparseInner & o, Prod * p = 0) : Capture(o, p) {}
  virtual ReparseInner * clone(Prod * p) {return new ReparseInner(*this, p);}
};

static inline const char * first_space(const char * s) 
{
  bool in_quote = false;
  while (*s && (in_quote || *s != ' ')) {
    if (*s == '"') 
      in_quote = !in_quote;
    if (*s == '\\') {
      ++s;
      assert(*s);
    }
    ++s;
  }
  return s;
}

static inline const char * first_non_space(const char * s)
{
  while (*s && *s == ' ') {
    if (*s == '\\') {
      ++s;
      assert(*s);
    }
    ++s;
  }
  return s;
}

class GatherParts {
protected:
  struct Parm { // one or the other
    Parm() : start(NPOS), stop(NPOS) {}
    const Syntax * name;
    unsigned start;
    unsigned stop;
  };
public:
  Syntax * gather(SourceStr str, SynBuilder & in);
  void add_part(Syntax * syn, SynBuilder * res) {
    if (capture_as_flag)
      res->add_flag(syn);
    else
      res->add_part(syn);
  }
  GatherParts(String n, bool caf = false)
    : name(!n.empty() ? SYN(n) : 0), 
      capture_as_flag(caf) 
    {parse_name();}
private:
  void parse_name() {
    if (!name) return;
    const char * s = ~*name;
    if (strcmp(s, "%") == 0) {
      // special case, a single "%" is the same as not having a name
      name = NULL;
      return;
    }
    const char * e = first_space(s);
    if (s[0] != '%' && !*e) {
      name = SYN(parse_common::unescape(s, e, '"')); // FIXME: Add source info
      return;
    }
    unsigned last_used = 0; // FIXME: Misnamed
    unsigned dots = NPOS;
    unsigned i = 0;
    for (;;) {
      Parm p;
      p.name = SYN(parse_common::unescape(s, e, '"')); // FIXME: Add source info
      String n = ~*p.name;
      if (n == "...") {
        assert(dots == NPOS); // FIXME: Error message
        dots = i;
      } else if (n[0] == '%') {
        unsigned j;
        if (n == "%") {
          assert(dots == NPOS); // FIXME Error message, parms after "..." must be numbered
          j = last_used++;
        } else {
          j = atoi(~n + 1); // FIXME: Error check
          last_used = j+1;
        }
        p.start = j;
        p.stop = j+1;
      }
      parms.push_back(p);
      ++i;
      s = first_non_space(e);
      if (!*s) break;
      e = first_space(s);
    }
    if (dots == NPOS) {
      Parm p;
      p.name = SYN("...");
      dots = parms.size();
      parms.push_back(p);
    }
    parms[dots].start = last_used;
    parms[dots].stop = NPOS;
  }
protected:
  const Syntax * name;
  Vector<Parm> parms;
  bool capture_as_flag;
};

Syntax * GatherParts::gather(SourceStr str, SynBuilder & in) {
  SyntaxBuilder res2;
  Vector<Parm>::const_iterator i = parms.begin(), e = parms.end();
  if (i->start == NPOS) {
    res2.add_part(i->name);
    } else {
    res2.add_part(in.part(i->start));
  }
  ++i;
  for (; i != e; ++i) {
    if (i->start == NPOS) {
      res2.add_part(i->name);
    } else if (i->start != NPOS) {
      for (int j = i->start; j < i->stop && j < in.num_parts(); ++j) {
        res2.add_part(in.part(j));
      }
    }
  }
  res2.set_flags(in.flags_begin(), in.flags_end());
  return res2.build(str);
}


class NamedCapture : public ProdImpl, public GatherParts {
// NamedCapture can also be used as a "forced capture" if name is empty
public:
  MatchRes match(SourceStr str, SynBuilder * parts) {
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    if (!parts) return prod.match(str, NULL); 
    SyntaxBuilder res;
    if (parms.empty() && name) res.add_part(name);
    MatchRes r = prod.match(str, &res);
    if (!r) return r;
    str.end = r.end;
    Syntax * res_syn = parms.empty() ? res.build(str) : gather(str, res);
    add_part(res_syn, parts);
    return r;
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    return prod.match_f(str, errs);
  }
  // FIXME: Should take in Syntax or SourceStr rater than String so we
  // can keep track of where the name came from
  NamedCapture(const char * s, const char * e, const ProdWrap & p, String n, bool caf = false)
    : Prod(s,e), GatherParts(n, caf), prod(p)
    {capture_type = ExplicitCapture;}
  NamedCapture(const NamedCapture & o, Prod * p = 0)
    : Prod(o), GatherParts(o), prod(o.prod.clone(p)) {}
  virtual Prod * clone(Prod * p) {return new NamedCapture(*this, p);}
  PersistentRes calc_just_persistent() {return prod.calc_persistent();}
  int first_char() const {return prod.first_char();}
  void verify() {
    prod.verify();
  }
  void dump() {/*printf("{"); prod->dump(); printf("}");*/}
private:
  ProdWrap prod;
};

class PlaceHolderCapture : public ProdImpl {
// NamedCapture can also be used as a "forced capture" if name is empty
public:
  MatchRes match(SourceStr str, SynBuilder * parts) {
    // FIXME: Explain what the hell is going on here
    if (!parts) return prod.match(str, NULL); 
    syntax_ns::SynEntity placeholder(str, parts);
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    SyntaxBuilder res;
    res.add_part(&placeholder);
    MatchRes r = prod.match(str, &res);
    if (!r) return r;
    if (!res.empty()) {
      //fprintf(stderr, "EMPTY!\n");
      //fprintf(stderr, ">>%s\n", ~res.build()->to_string());
      // we didn't grab the parts with GatherPartsProd and make it
      // into a syntax object, so just merge the results
      parts->add_parts(res.parts_begin() + 1, res.parts_end());
      parts->merge_flags(res.flags_begin(), res.flags_end());
    } 
    return r;
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    return prod.match_f(str, errs);
  }
    
  // FIXME: Should take in Syntax or SourceStr rater than String so we
  // can keep track of where the name came from
  PlaceHolderCapture(const char * s, const char * e, const ProdWrap & p)
    : Prod(s,e), prod(p)
    {capture_type = ExplicitCapture;}
  PlaceHolderCapture(const PlaceHolderCapture & o, Prod * p = 0)
    : Prod(o), prod(o.prod.clone(p)) {}
  virtual Prod * clone(Prod * p) {return new PlaceHolderCapture(*this, p);}
  PersistentRes calc_just_persistent() {return prod.calc_persistent();}
  int first_char() const {return prod.first_char();}
  void verify() {
    prod.verify();
  }
  void dump() {/*printf("{"); prod->dump(); printf("}");*/}
private:
  ProdWrap prod;
};


class GatherPartsProd : public ProdImpl, public GatherParts {
// NamedCapture can also be used as a "forced capture" if name is empty
public:
  MatchRes match(SourceStr str, SynBuilder * parts) {
    //fprintf(stderr, "NOW COMES THE FUN!\n");
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    if (!parts) return MatchRes(str.begin, NULL);
    // FIXME: Explain what the hell is going on here
    SynBuilder * res = parts->part(0)->entity<SynBuilder>();
    SourceStr rstr(parts->part(0)->str(), str.begin);
    if (parms.empty() && name) parts->part(0) = name;
    else                       ++parts->parts_;
    Syntax * res_syn = parms.empty() ? parts->build(str) : gather(str, *parts);
    parts->invalidate();
    add_part(res_syn, res);
    return MatchRes(str.begin, NULL);
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    return str.begin;
  }
  // FIXME: Should take in Syntax or SourceStr rater than String so we
  // can keep track of where the name came from
  GatherPartsProd(const char * s, const char * e, String n, bool caf = false)
    : Prod(s,e), GatherParts(n, caf)
    {capture_type = ExplicitCapture;}
  GatherPartsProd(const GatherPartsProd & o, Prod * p = 0)
    : Prod(o), GatherParts(o) {}
  virtual Prod * clone(Prod * p) {return new GatherPartsProd(*this, p);}
  PersistentRes calc_just_persistent() {return PersistentRes(true);}
  void verify() {}
  void dump() {/*printf("{"); prod->dump(); printf("}");*/}
};

class ReparseOuter : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder * parts) {
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    if (!parts) return prod->match(str, NULL); 
    SyntaxBuilderN<2> res;
    MatchRes r = prod->match(str, &res);
    if (!r) return r;
    assert(res.num_parts() == 1);
    ReparseSyntax * syn = const_cast<ReparseSyntax *>(res.part(0)->as_reparse());
    syn->str_ = SourceStr(str, r.end);
    syn->parts_[0] = name;
    syn->finalize();
    parts->add_part(syn);
    return r;
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    return prod->match_f(str, errs);
  }
  ReparseOuter(const char * s, const char * e, Prod * p, String n)
    : Prod(s,e), prod(p), name(SYN(n)) 
    {
      capture_type = ExplicitCapture;
      name = SYN(parse_common::unescape(n.begin(), n.end(), '"')); // FIXME: Add source info
    }
  ReparseOuter(const ReparseOuter & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)), name(o.name) {}
  virtual Prod * clone(Prod * p) {return new ReparseOuter(*this, p);}
  void verify() {
    prod->verify();
  }
  PersistentRes calc_just_persistent() {return prod->calc_persistent();}
  int first_char() const {return prod->first_char();}
  void dump() {printf("{"); prod->dump(); printf("}");}
private:
  Prod * prod;
  const Syntax * name;
};

class DescProd : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder * parts) {
    return prod.match(str, parts);
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    ParseErrors my_errs;
    const char * r = prod.match_f(str, my_errs);
    if (my_errs.size() > 0 && !desc.empty() && my_errs.front()->pos <= str) {
      errs.add(new ParseError(pos, str, desc));
    } else if (my_errs.size() > 0 && !desc.empty() && my_errs.front()->expected == "<charset>") {
      errs.add(new ParseError(pos, str, desc));
    } else {
      errs.add(my_errs);
    }
    return r;
  }
  DescProd(const char * s, const char * e, String n, const ProdWrap & p) 
    : Prod(s, e),  desc(n), prod(p) {capture_type = p.capture ? ExplicitCapture : NoCapture;}
  DescProd(const DescProd & o, Prod * p = 0) : Prod(o), desc(o.desc) {
    prod = o.prod.clone(p);
  }
  virtual Prod * clone(Prod * p) {return new DescProd(*this, p);}
  PersistentRes calc_just_persistent() {return prod.calc_persistent();}
  int first_char() const {return prod.first_char();}
  void verify() {prod.verify();}
private:
  String desc;
  ProdWrap prod;
};

// similar to std::equal but will also keep track of the last
// character read by modifying "with".
bool prefix_equal(const char * i, const char * e,
                  const char * & with, const char * end) 
{
  if (e - i <= end - with) {
    while (i != e && *i == *with)
      ++i, ++with;
  } else {
    // we can't possible match, but still need to advance with
    while (with != end && *i == *with)
      ++i, ++with;
  }
  return i == e;
}

class Literal : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder *) {
    const char * e = str.begin;
    if (prefix_equal(literal.begin(), literal.end(), e, str.end))
      return MatchRes(e, e-1);
    else
      return MatchRes(FAIL, e);
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    const char * e = str.begin;
    if (prefix_equal(literal.begin(), literal.end(), e, str.end)) {
      return e;
    } else {
      StringBuf buf;
      buf << "\"" << literal << "\"";
      errs.add(new ParseError(pos, str, buf.freeze()));
      return FAIL;
    }
  }  
  Literal(const char * s, const char * e, String l)
    : Prod(s,e), literal(l) {}
  virtual Prod * clone(Prod * p) {return new Literal(*this);}
  void dump() {printf("'%s'", literal.c_str());}
  PersistentRes calc_just_persistent() {return PersistentRes(true);}
  int first_char() const {return literal[0];}
private:
  String literal;
};

class CharClass : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder *) {
    if (!str.empty() && cs[*str])
      return MatchRes(str + 1, str);
    else
      return MatchRes(FAIL, str);
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    if (!str.empty() && cs[*str]) {
      return str + 1;
    } else {
      errs.add(new ParseError(pos, str, "<charset>")); // FIXME 
      return FAIL;
    }
  }
  CharClass(const char * s, const char * e, const CharSet & cs0) 
    : Prod(s,e), cs(cs0) {}
  virtual Prod * clone(Prod * p) {return new CharClass(*this);}
  void dump() {printf("[]");}
  PersistentRes calc_just_persistent() {return PersistentRes(true);}
private:
  CharSet cs;
};

class Any : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder *)  {
    if (!str.empty())
      return MatchRes(str+1, str);
    return MatchRes(FAIL, str);
  }
  const char * match_f(SourceStr str, ParseErrors & errs)  {
    if (!str.empty())
      return str+1;
    errs.push_back(new ParseError(pos, str, "<EOF>"));
    return FAIL;
  }
  Any(const char * s, const char * e) : Prod(s,e) {}
  virtual Prod * clone(Prod * p) {return new Any(*this);}
  void dump() {printf("_");}
  PersistentRes calc_just_persistent() {return PersistentRes(true);}
};

class Repeat : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder * parts) {
    const char * read_to = NULL;
    bool matched = false;
    for (;;) {
      if (end_with_) {
        MatchRes r = end_with_->match(str, NULL);
        read_to = std::max(read_to, r.read_to);
        if (r) break;
      }
      MatchRes r = prod.match(str, parts);
      read_to = std::max(read_to, r.read_to);
      if (!r) break;
      str.begin = r.end;
      matched = true;
      if (once) break;
    }
    if (matched || optional)
      return MatchRes(str, read_to);
    else 
      return MatchRes(FAIL, read_to);
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    bool matched = false;
    for (;;) {
      if (end_with_) {
        const char * r = end_with_->match_f(str, errs);
        if (r) break;
      }
      const char * r = prod.match_f(str, errs);
      if (!r) break;
      str.begin = r;
      matched = true;
      if (once) break;
    }
    if (matched || optional)
      return str;
    else 
      return FAIL;
  }
  Repeat(const char * s, const char * e, const ProdWrap & p, bool o1, bool o2) 
    : Prod(s,e), prod(p), optional(o1), once(o2), end_with_() 
    {capture_type = p.capture ? ExplicitCapture : NoCapture;}
  Repeat(const Repeat & o, Prod * p = 0) 
    : Prod(o), prod(o.prod.clone(p)), optional(o.optional), once(o.once) 
  { 
    if (o.end_with_) {
      end_with_ = o.end_with_->clone(p);
    }
  }
  virtual void end_with(Prod * p);
  virtual Prod * clone(Prod * p) {return new Repeat(*this, p);}
  void dump() {}
  PersistentRes calc_just_persistent() {
    PersistentRes r = prod.calc_persistent();
    if (end_with_)
      r |= end_with_->calc_persistent();
    return r;
  }
  int first_char() const {return optional ? -2 : prod.first_char();}
  void verify() {prod.verify(); if (end_with_) end_with_->verify();}
private:
  ProdWrap prod;
  bool optional;
  bool once;
  Prod * end_with_;
};

class Predicate : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder *) {
    MatchRes r = prod->match(str, NULL);
    if (dont_match_empty && r.end == str.begin)
      r.end = FAIL;
    if (r) {
      return MatchRes(invert ? FAIL : str, r.read_to);
    } else {
      return MatchRes(invert ? str : FAIL, r.read_to);
    }
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    // FIXME: Correctly handle errors;
    return Predicate::match(str, NULL).end;
  }
  Predicate(const char * s, const char * e, Prod * p, bool inv, bool dme = false) 
    : Prod(s,e), prod(p), invert(inv), dont_match_empty(dme) {}
  Predicate(const Predicate & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)), 
      invert(o.invert), dont_match_empty(o.dont_match_empty) {}
  virtual Prod * clone(Prod * p) {return new Predicate(*this, p);}
  void dump() {}
  PersistentRes calc_just_persistent() {return prod->calc_persistent();}
  void verify() {prod->verify();}
private:
  Prod * prod;
  bool invert;
  bool dont_match_empty;
};

void Repeat::end_with(Prod * p) {
  assert(!end_with_);
  end_with_ = new Predicate(p->pos, p->end, p, false, true);
}

static inline CaptureType get_capture_type(const Vector<ProdWrap> & p) 
{
  CaptureType t = NoCapture;
  for (Vector<ProdWrap>::const_iterator 
         i = p.begin(), e = p.end(); i != e; ++i)
    if (i->capture) t = ExplicitCapture;
  return t;
}

class Seq : public ProdImpl {
  // NOTE: A Seq _must_ have more than one element
public: 
  MatchRes match(SourceStr str, SynBuilder * res) {
    unsigned orig_parts_sz, orig_flags_sz;
    if (res)
      orig_parts_sz = res->num_parts(), orig_flags_sz = res->num_flags();
    Vector<ProdWrap>::iterator 
      i = prods.begin(), e = prods.end();
    const char * read_to = NULL;
    while (i != e) {
      MatchRes r = i->match(str, res);
      read_to = std::max(read_to, r.read_to);
      if (!r) {
        if (res) res->truncate_parts(orig_parts_sz), res->truncate_flags(orig_flags_sz);
        return MatchRes(FAIL, read_to);
      }
      str.begin = r.end;
      ++i;
    }
    return MatchRes(str.begin, read_to);
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    Vector<ProdWrap>::iterator 
      i = prods.begin(), e = prods.end();
    while (i != e) {
      const char * r = i->match_f(str, errs);
      if (!r)
        return FAIL;
      str.begin = r;
      ++i;
    }
    return str.begin;
  }
  Seq(const char * s, const char * e, const Vector<ProdWrap> & p)
    : Prod(s, e), prods(p) 
    {capture_type = get_capture_type(prods);}
  PersistentRes calc_just_persistent() {
    PersistentRes r;
    for (Vector<ProdWrap>::iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
      r |= i->calc_persistent();
    return r;
  }
  int first_char() const {return prods[0].first_char();}
  void verify() {
    for (Vector<ProdWrap>::iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
      i->verify();
  }

  Seq(const Seq & o, Prod * p = 0) : Prod(o) {
    prods.reserve(o.prods.size());
    for (Vector<ProdWrap>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i) 
    {
      prods.push_back(i->clone(p));
    }
  }
  virtual Prod * clone(Prod * p) {return new Seq(*this, p);}
  void dump() {}
private:
  Vector<ProdWrap> prods;
};

class Choice : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder * parts) {
    Vector<ProdWrap>::iterator 
      i = prods.begin(), e = prods.end();
    const char * read_to = NULL;
    while (i != e) {
      MatchRes r = i->match(str, parts);
      read_to = std::max(read_to, r.read_to);
      if (r) return MatchRes(r.end, read_to);
      ++i;
    }
    return MatchRes(FAIL, read_to);
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    Vector<ProdWrap>::iterator 
      i = prods.begin(), e = prods.end();
    while (i != e) {
      const char * r = i->match_f(str, errs);
      if (r) return r;
      ++i;
    }
    return FAIL;
  }
  Choice(const char * s, const char * e, const Vector<ProdWrap> & p)
    : Prod(s, e), prods(p) 
    {capture_type = get_capture_type(prods);}
  Choice(const Choice & o, Prod * p = 0) : Prod(o) {
    prods.reserve(o.prods.size());
    for (Vector<ProdWrap>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i) 
    {
      prods.push_back(i->clone(p));
    }
  }
  PersistentRes calc_just_persistent() {
    PersistentRes r;
    for (Vector<ProdWrap>::iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
      r |= i->calc_persistent();
    return r;
  }
  void verify() {
    for (Vector<ProdWrap>::iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i) {
      i->verify();
    }
  }
  virtual Prod * clone(Prod * p) {return new Choice(*this, p);}
  void dump() {}
private:
  Vector<ProdWrap> prods;
};

// FIXME: These should't be global
bool in_repl = false;
const Replacements * mids;
String cur_named_prod;

class S_MId : public ProdImpl {
public:
  MatchRes match(SourceStr str, SynBuilder * res) {
    //if (!in_repl) return FAIL;
    SyntaxBuilder res0;
    MatchRes r = prod->match(str, &res0);
    if (!r) return r;
    res0.make_flags_parts();
    assert(res0.single_part());
    Syntax * r0 = res0.part(0);
    if (mids && mids->anywhere(*r0->arg(0)) > 0) {
      Syntax * p = SYN(r0->str(), r0->part(0), r0->arg(0), SYN(in_named_prod));
      //printf("MATCH MID %s\n", ~p->to_string());
      if (res) res->add_part(p);
      return r;
    } else {
      return MatchRes(FAIL, r.read_to);
    }
  }
  const char * match_f(SourceStr str, ParseErrors & errs) {
    //if (!in_repl) return FAIL;
    SyntaxBuilder res0;
    MatchRes r = prod->match(str, &res0);
    if (!r) {
      const char * r0 = prod->match_f(str, errs);
      assert(!r0);
      return FAIL;
    }
    res0.make_flags_parts();
    assert(res0.single_part());
    Syntax * r0 = res0.part(0);
    if (mids && mids->anywhere(*r0->arg(0)) > 0) {
      return r.end;
    } else {
      return FAIL; // FIXME: Inject Error
    }

  }
  S_MId(const char * s, const char * e, const ProdWrap & p, String inp = String())
    : Prod(s,e), in_named_prod(inp.empty() ? cur_named_prod : inp), prod(new NamedCapture(s, e, p, "mid")) {capture_type = ExplicitCapture;}
  S_MId(const S_MId & o, Prod * p = 0)
    : Prod(o), in_named_prod(o.in_named_prod), prod(o.prod->clone(p)) {}
  virtual Prod * clone(Prod * p) {return new S_MId(*this, p);}
  PersistentRes calc_just_persistent() {return PersistentRes(false);}
private:
  String in_named_prod;
  Prod * prod;
};

const Syntax * parse_str(String what, SourceStr str, const Replacements * repls) {
  pprintf("BEGIN %s\n", ~what);
  //printf("PARSE STR %.*s as %s\n", str.end - str.begin, str.begin, ~what);
  clock_t start = clock();
  //str.source = new ParseSourceInfo(str, what);
  mids = repls;
  Prod * p = parse.named_prods[what];
  parse.clear_cache();
  SyntaxBuilder dummy;
  const char * s = str.begin;
  const char * e = p->match(str, &dummy).end;
  mids = 0;
  //assert(s != e);
  //printf("%p %p %p : %.*s\n", s, e, str.end, str.end - str.begin, str.begin);
  if (e == str.end) {
    //printf(">>%.*s<<\n", e-s, s);
    //dummy[0]->print();
    //printf("\n");
  } else {
    //printf("GETTING ERROR\n");
    ParseErrors errors;
    const char * e0 = p->match_f(str, errors);
    assert(e0 == e);
    throw errors.to_error(new ParseSourceInfo(str, what), file);
  }
  clock_t stop = clock();
  //printf("PARSE STR ... as %s time: %f\n", ~what, (stop - start)/1000000.0);
  dummy.make_flags_parts();
  assert(dummy.single_part());
  return dummy.part(0);
}

namespace ParsePeg {

  const char * opt_desc(const char * str, const char * end, String & res) {
    if (*str == ':') {
      str = symbol(':', str, end);
      if (str == end || *str != '"') throw error(str, "'\"' expected");
      SubStr res0;
      str = quote('"', str, end, res0);
      res = unescape(res0);
    }
    return str;
  }

  void Parse::top(const char * str, const char * end) {
    String name;
    str = spacing(str, end);
    while (str != end) {
      if (*str == '@') {
        ++str;
        // a derivative
        str = id(str, end, name);
        if (name == "hint_file") {
          SubStr file0;
          str = spacing(str, end);
          assert(*str == '"'); // fixme error message
          str = quote('"', str, end, file0);
          StringBuf file1;
          unescape(file0.begin, file0.end, file1, '"');
          String file = file1.freeze();
          //fprintf(stderr, "hint_file = %s\n", ~file);
          SourceFile * hints = new_source_file(file);
          try {
            parse_hints_file(hints->begin(), hints->end());
          } catch (Error * err) {
            // OK this is very bad, fix latter
            err->source = hints;
            //fprintf(stderr, "%s\n", err->message().c_str());
            exit(1);
          }
        } else {
          throw error(str, "Unknown directive: %s\n", ~name);
        }
      } else if (*str == '"') {
        // ie a defination of a special token
        str = symbol('"', str, end);
        Res sr = peg(str, end, '"');
        pprintf("PARSING TOKEN PROD: %s\n", ~sample(str, sr.end));
        str = sr.end;
        str = require_symbol('"', str, end);
        sr.prod->verify();
        String desc;
        str = opt_desc(str, end, desc);
        str = require_symbol('=', str, end);
        Res r = peg(str, end, ';');
        r.prod->verify();
        token_rules.push_back(TokenRule(sr.prod, 
                                        desc.empty() ? r.prod : new DescProd(str, r.end, desc, r)));
        str = r.end;
      } else {
        str = id(str, end, name);
        pprintf("PARSING NAMED PROD: %s\n", ~name);
        cur_named_prod = name; 
        NamedProd * & p = named_prods[name];
        //printf("NAME: %s %p\n", name.c_str(), p);
        if (p == 0) p = new_named_prod(name);
        String desc;
        str = opt_desc(str, end, desc);
        str = require_symbol('=', str, end);
        //bool explicit_capture = false;
        Res r = peg(str, end, ';');
        r.prod->verify();
        //if (name == "SPLIT_FLAG") stop();
        p->set_prod(desc.empty() ? r : ProdWrap(new DescProd(str, r.end, desc, r)));
        str = r.end;
      }
      str = require_symbol(';', str, end);
    }
    pprintf("PARSING DONE\n");
    // resolve symbols
    for (Vector< Sym<NamedProd> >::const_iterator 
           i = unresolved_syms.begin(), e = unresolved_syms.end(); i != e; ++i) 
    {
      // make sure the symbol got resolved
      if (!i->prod->prod.prod)
        throw error(i->pos, "Unresolved Symbol:: %s", i->prod->name.c_str());
    }

    for (Vector< Sym<TokenProd> >::const_iterator 
           i = token_syms.begin(), e = token_syms.end(); i != e; ++i) 
    {
      resolve_token_symbol(i->prod, i->pos);
    }
    
    hash_map<String, NamedProd *>::iterator i = named_prods.begin(), e = named_prods.end();
    for (; i != e; ++i) {
      i->second->finalize();
      if (i->second->persistent())
        pprintf("IS PERSISTENT: %s\n", ~i->second->name);
      else
        pprintf("is not persistent: %s\n", ~i->second->name);
    }
    pprintf("PERSISTENT DONE\n");
  }

  void Parse::resolve_token_symbol(TokenProd * p, const char * pos) 
  {
    Vector<TokenRule>::iterator i = token_rules.begin(), e = token_rules.end();
    for (;i != e; ++i) {
      //ParseErrors errors;
      SourceStr str(p->name);
      str.begin = i->to_match->match(str, NULL).end;
      if (str.empty()) {
        p->set_prod(ProdWrap(i->if_matched->clone(new Capture(new Literal(p->pos, p->end, p->name)))));
        return;
      }
    }
    throw error(pos, "Could not find a match for token symbol: %s", p->name.c_str());
  }

  Res Parse::peg(const char * str, const char * end, char eos, CaptureNeed nc)
  {
    const char * start = str;
    str = spacing(str, end);
    Vector<ProdWrap> prods;
    while (str != end) {
      Res r = sequence(str, end, eos);
      str = r.end;
      //if (capture)
      //  prods.push_back(new_capture(start, end, r.prod, implicit));
      //else 
      prods.push_back(r);
      const char * s = symbol('/', str, end);
      if (!s) break;
      str = s;
    }
    assert(prods.size() > 0);
    if (prods.size() == 1) {
      switch (nc) {
      case DontNeedCapture:
        return Res(str, prods[0]);
      case NeedCapture:
        if (prods[0].prod->capture_type != NoCapture)
          return Res(str, prods[0].prod, DoCapture);
        else
          return Res(str, new Capture(start, str, prods[0].prod));
      case NeedReparseCapture:
        return Res(str, new ReparseInner(start, str, prods[0].prod));
      }
    } else {
      switch (nc) {
      case DontNeedCapture:
        return Res(str, new Choice(start, str, prods));
      case NeedCapture: 
        for (Vector<ProdWrap>::iterator 
               i = prods.begin(), e = prods.end(); i != e; ++i)
        {
          if (i->prod->capture_type == NoCapture)
            i->prod = new Capture(i->prod);
          i->capture = true;
        }
        return Res(str, new Choice(start, str, prods));
      case NeedReparseCapture:
        return Res(str, new ReparseInner(start, str, new Choice(start, end, prods)));
      }
    }
    abort(); // Should never happen, to make gcc happy.
  }
  
  Res Parse::sequence(const char * str, const char * end, char eos) 
  {
    const char * start = str;
    Vector<ProdWrap> prods;
    bool capture_as_flag = false;
    bool named_capture = false;
    SubStr name;
    SubStr special;
    SubStr special_arg;
    enum SpecialType {SP_NONE, SP_NAME_LATER, SP_MID, SP_REPARSE} sp = SP_NONE;
    if (*str == ':') {
      capture_as_flag = true;
      named_capture = true;
      ++str;
    }
    if (*str == '<') {
      named_capture = true;
      const char * str2 = str + 1;
      if (str2 != end && *str2 == '<') {
        str = str2;
        ++str;
        str = spacing(str, end);
        special.begin = str;
        while (str != end && *str != '>' && !asc_isspace(*str))
          ++str;
        special.end = str;
        if (str == end) throw error(str, "Unterminated >");
        if (asc_isspace(*str)) {
          str = spacing(str, end);
          str = quote('>', str-1, end, special_arg);
          if (str == end) throw error(str, "Unterminated >");
        } else {
          ++str;
        }
        if (special == "") {
          sp = SP_NAME_LATER;
        } else if (special == "mid") {
          name = special;
          sp = SP_MID;
        } else if (special == "reparse") {
          name = special_arg;
          sp = SP_REPARSE;
        }
        else
          throw error(start, "Unknown special");
        str = require_symbol('>', str, end);
      } else {
        str = quote('>', str, end, name);
      }
    }
    bool found_capture = false;
    Context context = NormalContext;
    if (sp == SP_REPARSE) context = ReparseContext;
    while (str != end && *str != eos && *str != '/') {
      Res r = sequence2(str, end, context);
      str = r.end;
      prods.push_back(r);
      //FIXME: Rewrite
      //if (sp == SP_REPARSE) {
      //  if (Capture * c = dynamic_cast<Capture *>(r.prod)) {
      //    if (found_capture)
      //      throw error(start, "Only one capture allowed for reparse special.");
      //    c->reparse_capture = true;
      //    found_capture = true;
      //  }
      //}
    }
    ProdWrap prod;
    if (prods.size() == 0)
      prod = ProdWrap(new AlwaysTrue(start, str));
    else if (prods.size() == 1)
      prod = prods[0];
    else
      prod = ProdWrap(new Seq(start, str, prods));
    if (sp == SP_NAME_LATER)
      return Res(new PlaceHolderCapture(start, str, prod));
    else if (sp == SP_MID)
      return Res(new S_MId(start, str, prod, String(special_arg)));
    else if (sp == SP_REPARSE)
      return Res(new ReparseOuter(start, str, prod.prod, name));
    if (named_capture)
      return Res(new NamedCapture(start, str, prod, name, capture_as_flag));
    else
      return Res(str, prod);
  }

  Res Parse::sequence2(const char * str, const char * end, Context context)
  {
    const char * start = str;
    Res r1 = prefix(str, end, context);
    str = r1.end;
    if (*str == '.') {
      str = symbol('.', str, end);
      Res r2 = prefix(str, end, context);
      str = r2.end;
      r1.prod->end_with(r2.prod); // FIXME: Maybe should memorize r2
      Vector<ProdWrap> prods;
      prods.push_back(r1);
      prods.push_back(r2);
      return Res(new Seq(start, str, prods));
    } else {
      return r1;
    }
  }

  Res Parse::desc_label(const char * str, const char * end) 
  {
    //return str;
    abort();
  }

  Res Parse::prefix(const char * str, const char * end, Context context) 
  {
    const char * start = str;
    if (*str == '!') {
      str = symbol('!', str, end);
      Res r = suffix(str, end, NormalContext);
      str = r.end;
      return Res(new Predicate(start, str, r.prod, true));
    } else if (*str == '&') {
      str = symbol('&', str, end);
      Res r = suffix(str, end, NormalContext);
      str = r.end;
      return Res(new Predicate(start, str, r.prod, false));
    } else if (*str == '=') {
      str = symbol('=', str, end);
      str = spacing(str, end);
      if (*str != '<') throw error (str, "'<' expected");
      SubStr name;
      str = quote('>', str, end, name);
      return Res(new GatherPartsProd(start, str, String(name)));
    } else {
      return suffix(str, end, context);
    }
  }

  Res Parse::suffix(const char * str, const char * end, Context context) 
  {
    const char * start = str;
    Res r = primary(str, end, context);
    str = r.end;
    if (*str == '?') {
      str = symbol('?', str, end);
      return Res(new Repeat(start, str, r, true, true));
    } else if (*str == '*') { 
      str = symbol('*', str, end);
      return Res(new Repeat(start, str, r, true, false));
    } else if (*str == '+') {
      str = symbol('+', str, end);
      return Res(new Repeat(start, str, r, false, false));
    } else {
      return r;
    }
  }

  Res Parse::primary(const char * str, const char * end, Context context) 
  {
    const char * start = str;
    if (*str == '(') {
      str = symbol('(', str, end);
      Res r = peg(str, end, ')');
      str = require_symbol(')', r.end, end);
      return Res(str, r.prod);
    } else if (*str == '{') {
      str = symbol('{', str, end);
      Res r = peg(str, end, '}', context == NormalContext ? NeedCapture : NeedReparseCapture);
      str = require_symbol('}', r.end, end);
      return Res(str, r);
    } else if (*str == '\'') {
      return literal(str, end);
    } else if (*str == '"') {
      return token(str, end);
    } else if (*str == '[') {
      return char_class(str, end);
    } else {
      return identifier(str, end);
    }
  }

  Res Parse::literal(const char * str, const char * end) {
    const char * start = str;
    SubStr lit0;
    str = quote('\'', str, end, lit0);
    String lit = unescape(lit0);
    return Res(new Literal(start, str, lit));
  }
  
  Res Parse::char_class(const char * str, const char * end) {
    const char * start = str;
    CharSet set1,set2;
    CharSet * set = &set1;
    bool range = false;
    char prev = '\0';
    ++str;
    if (*str == '^') {
      set1.add(0,SET_SIZE-1);
      set = &set2;
      ++str;
    }
    while (*str != ']' && *str != '\0') {
      if (*str == '^') {
        set = &set2;
      } else if (*str == '-' && !range && prev != '\0') {
        range = true;
      } else {
        if (*str == '\\') {
          ++str;
          if (!*str) {
            throw error(str, "Unexpected end of string");
          }
          const CharSet * s = backspace_map[(unsigned char)*str];
          if (s) {
            set->add(*s);
            ++str;
            continue;
          }
        }
        if (!range) {
          set->add(*str);
        } else {
          set->add(prev, *str);
          range = false;
        }
        prev = *str;
      }
      ++str;
    }
    if (range) 
      set->add('-');
    set1.remove(set2);
    if (str == end)
      throw error(start, "Unterminated '['");
    ++str;
    str = spacing(str, end);
    return Res(new CharClass(start, str, set1));
  }

  Res Parse::token(const char * str, const char * end) {
    const char * start = str;
    SubStr name0;
    str = quote('"', str, end, name0);
    String name = unescape(name0);
    TokenProd * p = new TokenProd(start, str, name);
    token_syms.push_back(Sym<TokenProd>(start, p));
    return Res(str, p);
  }

  Res Parse::identifier(const char * str, const char * end) {
    const char * start = str;
    String name;
    str = id(str, end, name);
    if (name == "_") {
      return Res(new Any(start, str));
    } else if (name == "_self") {
      // special symbol for "tokens"
      // FIXME: Check that we really can use this symbol here
      return Res(new SymProd(start, str, name));
    } else {
      pprintf("PARSING    DEP: %s\n", ~name);
      NamedProd * & p = named_prods[name];
      if (p == 0) p = new_named_prod(name);
      unresolved_syms.push_back(Sym<NamedProd>(start, p));
      return Res(str, p);
      //return Res(str, p);
    }
  }

  void Parse::parse_hints_file(const char * str, const char * end) {
    str = spacing(str, end);
    while (str != end) {
      String prod;
      str = id(str, end, prod);
      str = require_symbol(':', str, end);
      while (str != end && *str != ';') {
        String hint;
        str = id(str, end, hint);
        if (hint == "trans") {
          //printf("trans %s\n", ~prod);
          trans_prods.insert(prod);
        } else {
          throw error(str, "Unknown hint: %s\n", ~hint);
        }
      }
      str = require_symbol(';', str, end);
    }
    if (str != end)
      throw error(str, "Expected EOF\n");
  }
}

Error * ParseErrors::to_error(const SourceInfo * source, const SourceFile * grammer)
{    
  if (empty()) {
    return error(source, 0, "Parse Failed (no specific error)\n");
  } else {
    StringBuf buf;
    //Pos pos = file->get_pos(front()->pos);
    //printf("Error %d.%d: Expected ", pos.line, pos.col);
    buf.printf("Expected ");
    int i = 0; 
    for (;;) {
      buf.printf("%s", (*this)[i]->expected.c_str());
      if (grammer) {
        Pos pos = grammer->get_pos((*this)[i]->grammer_pos);
        buf.printf(" (%d.%d)", pos.line, pos.col);
      }
      ++i;
      if (i == size()) break;
      buf.printf(" or ");
    }
    return error(source, front()->pos, ~buf.freeze());
  }
}

void ParseErrors::print(const SourceInfo * file, const SourceFile * grammer)
{
  if (empty()) {
    printf("Parse Failed (no specific error)\n");
  } else {
    Pos pos = file->get_pos(front()->pos);
    printf("Error %d.%d: Expected ", pos.line, pos.col);
    int i = 0; 
    for (;;) {
      printf("%s", (*this)[i]->expected.c_str());
      if (grammer) {
        pos = grammer->get_pos((*this)[i]->grammer_pos);
        printf(" (%d.%d)", pos.line, pos.col);
      }
      ++i;
      if (i == size()) break;
      printf(" or ");
    }
    printf("\n");
  }
}

