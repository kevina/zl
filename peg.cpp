//#include <sys/types.h>
//#include <sys/stat.h>
//#include <fcntl.h>
#include <assert.h>
#include <time.h>
#include <stdlib.h>

#include <algorithm>
#include <functional>
#include <bitset>
#include <typeinfo>

#include "peg.hpp"

#include "parse.hpp"
#include "error.hpp"
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

void my_breakpoint(int x) {}

using std::pair;

struct Prod;

static unsigned indent_level = 0;

// FIXME: Need more effect representation for const strings
//        (Pool of strings, hash lookup, ...)

struct ParseError {
  const char * grammer_pos;
  const char * pos;
  String expected; 
  ParseError(const char * gp, const char * p, String e)
    : grammer_pos(gp), pos(p), expected(e) 
    {
      //printf(">>ERROR %p: Expected %s<<\n", pos, e.c_str());
    }
  ParseError(const SourceStr & gp, const SourceStr & str, String e)
    : grammer_pos(gp.begin), pos(str.begin), expected(e) 
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

typedef const char * MatchRes;

typedef SyntaxBuilderBase SynBuilder;

static const char * FAIL = 0;

struct Res {
  const char * end;
  unsigned run_id;
  SyntaxBuilderN<2> res;
  Res() : end(FAIL /* to avoid infinite recursion when matching */) {}
};

//class Prod {
//  virtual ~Prod() = 0;
//};

enum CaptureType {NoCapture, CanGiveCapture, ExplicitCapture, ReparseCapture};

class Prod;

typedef std::bitset<128> BitSet;

enum {PP_OPTIONAL, PP_NORMAL, PP_PREDICATE};

struct ProdPropsBase {
  BitSet first_char;
  unsigned char what; // PP_*
  unsigned char first_char_high; // 0 none, 1 some, 2 all
  bool first_char_eof;
  static const unsigned char USES_MID = 1;
  static const unsigned char USES_ANTIQUOTE = 2;
  unsigned char uses_special; // bit mask 0 = mid, other = reserved
  ProdPropsBase() 
    : what(PP_NORMAL), first_char_high(0), first_char_eof(false), 
      uses_special(0) {}
  void reset() {
    what = PP_NORMAL;
    first_char.reset();
    first_char_high = 0;
    first_char_eof = false;
  }
};

// a dependency is represented as a position on the stack frame

unsigned stack_depth = UINT_MAX - 1;

struct ProdProps : public ProdPropsBase {
  // values are only valid if deps is empty
  unsigned deps;
  ProdProps() : deps() {}
  ProdProps(const ProdPropsBase & o) : ProdPropsBase(o) {}
  ProdProps(unsigned d) {deps = d;}
  // merge_info does not merge char info
  bool no_deps() const {return deps == 0;}
  bool have_deps() const {return deps != 0;}
  bool no_deps_but(unsigned d) const {return deps <= d;}
  void clear_deps() {deps = 0;}
  void merge_info(const ProdProps & o) { 
    uses_special |= o.uses_special;
    deps = std::max(deps, o.deps);
  }
  void invert_char_info() {
    first_char.flip();
    first_char_high = 2 - first_char_high;
    first_char_eof = !first_char_eof;
  }
  void reset() {
    ProdPropsBase::reset();
    deps = 0;
  }
  int single_char() const {
    if (what == PP_OPTIONAL) return -1;
    if (first_char_high || first_char_eof) return -1;
    if (first_char.count() > 1) return -1;
    return first_char._Find_first();
  }
  bool may_match(const char * str, const char * end) const {
    if (what == PP_OPTIONAL) return true;
    if (str == end) return first_char_eof;
    unsigned char c = *str;
    if (c < 128) return first_char[c];
    else return first_char_high;
  }
};

static bool operator==(const ProdProps & x, const ProdProps & y) {
  return x.first_char == y.first_char && 
    x.first_char_high == y.first_char_high && 
    x.first_char_eof == y.first_char_eof &&
    x.uses_special == y.uses_special &&
    x.deps == y.deps;
}

static void merge_for_choice(ProdProps & x, const ProdProps & y) {
  x.what = std::min(x.what, y.what);
  x.first_char |= y.first_char;
  x.first_char_high = std::max(x.first_char_high,y.first_char_high);
  x.first_char_eof |= y.first_char_eof;
  x.merge_info(y);
}

// sequenced need to be merged in the inverse order!
static void merge_for_seq(const ProdProps & x, ProdProps & y, bool & first) {
  if (x.what == PP_NORMAL || first) {
    y.what = x.what;
    y.first_char = x.first_char;
    y.first_char_high = x.first_char_high;
    y.first_char_eof  = x.first_char_eof;
    first = false;
  } else if (x.what == PP_PREDICATE) {
    y.what = PP_PREDICATE;
    y.first_char &= x.first_char;
      y.first_char_high = std::min(y.first_char_high, x.first_char_high);
      y.first_char_eof &= x.first_char_eof;
  } else if (x.what == PP_OPTIONAL) {
    y.what = std::max(y.what, x.what);
    y.first_char |= x.first_char;
    y.first_char_high = std::max(y.first_char_high, x.first_char_high);
    y.first_char_eof |= x.first_char_eof;
  }
  y.merge_info(x);
}
class NamedProd;

struct MatchEnviron;

class Prod;
struct Mapper {
  struct Data;
  const Data & data;
  Mapper(const Data & d) : data(d) {}
  const Prod * operator() (const Prod *);
};

class Prod {
public:
  virtual MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const = 0;
  // used when doing a "parse walk"
  void match_check(SourceStr str, MatchRes res, MatchEnviron & env) const {
    MatchRes r = match(str, NULL, env);
    assert(r == res);
  }
  virtual void match_anyway(SourceStr str, Res & res, MatchEnviron & env) const {
    match_check(str,res.end,env);
  }
  // FIXME: explain match_f: ...
  virtual const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const = 0;
  // NOTE: A cloned productions needs to re-finalized.
  virtual const Prod * clone(Prod * = 0) const = 0; 
  // ^^ The paramater is the new prod to use in place of the
  // placeholder prod "_self"
  virtual const Prod * clone(Mapper) const = 0;
  // ^^ The paramter is a mapping of prod which need to change
  virtual void end_with(const Prod * p) {}
  virtual void dump() const {printf("|?|");}
  // calc_props should also set props_ if the value is final (ie no
  // deps)
  virtual const ProdProps & calc_props() = 0;
  const ProdProps & props() const {assert (props_); return *props_;}
  const ProdProps & get_props() {
    //printf("%*c:(%s):%p(%p):%s\n", indent_level, ' ', typeid(*this).
    //       name(), props_, (props_ ? props_->dep : NULL),
    //       pos ? ~sample(pos, end, 40) : "");
    if (!props_) {
      //indent_level++;
      const ProdProps & r = calc_props(); 
      //indent_level--;
      return r;
    } else {
      //if (!d_name().empty()) printf("%*c#%s\n", indent_level, ' ', ~d_name());
      return *props_;
    }
  }
  virtual void finalize() = 0;
  virtual ~Prod() {}
  virtual String d_name() const {return String();}
  //virtual bool optional() const = 0;
  enum IsSpecial {IS_OTHER, IS_REPARSE};
  virtual IsSpecial is_special() const {return IS_OTHER;}
  Prod(const SourceStr & s) 
    : capture_type(NoCapture), g_span(s), finalized(false), props_(NULL) {}
  Prod(const Prod & o)
    : capture_type(o.capture_type), g_span(o.g_span), finalized(false), props_(NULL) {}
public: // but don't useq
  CaptureType capture_type;
  SourceStr g_span; // grammar span
  bool finalized; // once finalized can't be modified
protected:
  void set_props(const ProdProps & o) {if (o.no_deps()) props_ = &o;}
  const ProdProps * props_;
};

enum CaptureQ {DontCapture, DoCapture};

namespace ParsePeg {struct Res;}

struct ProdWrapBase {
  const Prod * prod;
  explicit ProdWrapBase(const Prod * p = NULL) : prod(p) {}
  void match_check(SourceStr str, MatchRes res, MatchEnviron & env) const {
    prod->match_check(str, res, env);}
  void match_anyway(SourceStr str, Res & res, MatchEnviron & env) const {
    prod->match_anyway(str, res, env);}
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    return prod->match_f(str, errs, env);}
  const ProdProps & props() const {return prod->props();}
  const ProdProps & get_props() {
    if (prod->finalized) 
      return prod->props();
    else
      return const_cast<Prod *>(prod)->get_props();
  }
  void finalize() {
    if (prod->finalized) return; 
    const_cast<Prod *>(prod)->finalize(); 
    assert(prod->finalized);
  }
  void end_with(const Prod * p) {
    assert(!prod->finalized);
    const_cast<Prod *>(prod)->end_with(p);
  }
  void dump() const {prod->dump();}
  ProdWrapBase(const ProdWrapBase & o, Mapper m) {
    if (o.prod) {
      prod = m(o.prod);
      if (!prod) prod = o.prod->clone(m);
    } else {
      prod = NULL;
    }
  }
};

struct ProdPtr : public ProdWrapBase {
  explicit ProdPtr(const Prod * p = NULL) : ProdWrapBase(p) {}
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    return prod->match(str, parts, env);}
  ProdPtr clone(Prod * o) const {return ProdPtr(prod->clone(o));}
  Prod::IsSpecial is_special() const {return prod->is_special();}
  operator const Prod * () const {return prod;}
  bool operator! () const {return !prod;}
  const Prod * operator-> () const {return prod;}
  ProdPtr & operator=(const Prod * p) {prod = p; return *this;}
  ProdPtr(const ProdPtr & o, Mapper m) : ProdWrapBase(o,m) {}
};

struct ProdWrap : public ProdWrapBase {
  bool capture;
  ProdWrap() : ProdWrapBase(NULL), capture(false) {}
  ProdWrap(const Prod * p, CaptureQ c) : ProdWrapBase(p), capture(c) {}
  void get_capture_from_prod() {
    capture = prod->capture_type >= ExplicitCapture;}
  explicit ProdWrap(const Prod * p) : ProdWrapBase(p) {get_capture_from_prod();}
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    return prod->match(str, capture ? parts : NULL, env);}
  ProdWrap clone(Prod * o) const {return ProdWrap(prod->clone(o), capture);}
  Prod::IsSpecial is_special() const 
    {return capture ? prod->is_special() : Prod::IS_OTHER;}
  ProdWrap(const ProdWrap & o, Mapper m) : ProdWrapBase(o,m), capture(o.capture) {}
private:
  ProdWrap(const Prod * p, bool c) : ProdWrapBase(p), capture(c) {}
};

class SymProd : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const;
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const;
  SymProd(const SourceStr & s, String n) 
    : Prod(s), name(n) {capture_type = CanGiveCapture;}
  SymProd(const SourceStr & s, String n, const ProdWrap & p) 
    : Prod(s), name(n) {capture_type = CanGiveCapture; prod = p;}
  const ProdProps & calc_props() {
    const ProdProps & r = prod.get_props();
    set_props(r);
    return r;
  }
  void finalize() {finalized = true; prod.finalize();}
public: // but treat as protected
  String name;
  String desc;
//protected:
  ProdWrap prod;
protected:
  SymProd(const SymProd & o, ProdWrap p) 
    : Prod(o), name(o.name), desc(o.desc), prod(p) {}
};

class SelfSymProd : public SymProd {
public:
  SelfSymProd(const SourceStr & s, String n) 
    : SymProd(s,n) {}
  SelfSymProd(const SymProd & other, Prod * p = 0) 
    : SymProd(other, ProdWrap(p)) {}
  const Prod * clone(Prod * p) const {return new SelfSymProd(*this, p);} 
  const Prod * clone(Mapper m) const {return this;}
  void dump() const {printf("_self");}
};

struct PtrLt {
  bool operator() (const char * x, const char * y) const {return x < y;}
};

//register unsigned esp asm("esp");

class NamedProd : public SymProd {
public:
  NamedProd(String n) : SymProd(SourceStr(), n) {}
  const Prod * clone(Prod *) const {return this;} // don't copy prod with name
  const Prod * clone(Mapper) const {return this;}

  NamedProd(const NamedProd & o) 
    : SymProd(o, o.prod) {}
  virtual NamedProd * shallow_clone() const {return new NamedProd(*this);}
    
  void dump() const {printf("%s", ~name);}
  const ProdProps & calc_props() {
    //unsigned esp0 = esp;
    props_obj.deps = stack_depth;
    props_ = &props_obj;
    //printf("%*cFINALIZING %s %p\n", indent_level, ' ', ~name, this);
    //indent_level += 2;
    //printf("YEAH BABY!\n");
    stack_depth--;
    const ProdProps & r = prod.get_props();
    stack_depth++;
    //indent_level -= 2;
    if (r.no_deps_but(stack_depth)) {
      //printf("%*cSUCCESS %s\n", indent_level, ' ', ~name);
      props_obj = r;
      props_obj.clear_deps();
      stack_depth--;
      const ProdProps & r2 = prod.get_props();
      stack_depth++;
      assert(props_obj == r2);
      return props_obj;
    } else {
      //printf("%*cFAILED %s\n", indent_level, ' ', ~name);
      props_ = NULL;
      return r;
    }
  }
  String d_name() const {return name;}
  bool persistent() const {return ! (props_obj.uses_special & props_obj.USES_MID);}
  bool w_antiquote() const {return props_obj.uses_special & props_obj.USES_ANTIQUOTE;}
  void finalize() {
    if (finalized) return;
    finalized = true;
    prod.finalize();
  }
public:
  ProdProps props_obj;
};


unsigned run_id = 1; // persistent prod always have a run_id of 0

struct CachedProd;
struct CacheKey {
  const CachedProd * prod;
  const char * str;
  CacheKey(const CachedProd * p, const char * s) : prod(p), str(s) {}
};

template <> struct hash<CacheKey>   {
  unsigned long operator()(const CacheKey & k) const {
    return (unsigned long)k.prod ^ (unsigned long)k.str;
  }
};

bool operator==(const CacheKey & x, const CacheKey & y) {
  return x.prod == y.prod && x.str == y.str;
}

struct CacheData : public tiny_hash<hash_map<CacheKey,Res>,2,32> {};

struct Cache {
  typedef CacheKey Key;
  typedef CacheData Data;
  Data * data;
  Cache() : data() {}
  void reset(Data * p) {
    pprintf("NamedProd CLEAR\n");
    run_id++;
    data = p;
  }
  struct LookupRes {
    Res & res;
    bool exists;
    LookupRes(Res & r, bool e) : res(r), exists(e) {}
  };
  inline LookupRes lookup(const CachedProd * prod, SourceStr str);
};

extern "C" 
namespace macro_abi {
  typedef MutableSyntax SyntaxList;
  SyntaxList * new_syntax_list();
}

struct AntiQuoteList {
  MutableSyntax * els;
  unsigned pos;
  AntiQuoteList() : els(macro_abi::new_syntax_list()), pos(0) {}
  AntiQuoteList(MutableSyntax * e) : els(e), pos(0) {}
  void set(const Syntax * s) {
    if (pos > els->num_args()) {
      abort();
    } else if (pos == els->num_args()) {
      els->add_part(s);
    } else {
      const Syntax * orig = els->part(pos);
      assert(orig == s);
    }
  }
  const Syntax * get() {
    if (pos >= els->num_args()) return NULL;
    else return els->arg(pos);
  }
  void adv() {
    ++pos;
  }
};

struct MatchEnviron {
  const PEG * peg;
  const Replacements * mids;
  Cache cache;
  ast::Environ * ast_env;
  AntiQuoteList * antiquotes;
  MatchEnviron() : peg(), mids(), ast_env(), antiquotes() {}
};


class CachedProd : public NamedProd {
  // named productions are memorized
public:
  MatchRes match_i(SourceStr str, SynBuilder * parts, Res * given_res, MatchEnviron & env) const ;
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    return match_i(str, parts, NULL, env);
  }
  void match_anyway(SourceStr str, Res & res, MatchEnviron & env) const {
    match_i(str, NULL, &res, env);
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const;
  CachedProd(String n) : NamedProd(n), first_char_(-3) {/*cs = (char *) GC_MALLOC(256);*/}
  void finalize() {
    if (finalized) return;
    finalized = true;
    prod.finalize();
    int c = props_->single_char();
    if (c >= 0)
      first_char_ = c;
    //printf("XXX %s\n", ~name);
    //for (unsigned j = 0; j != 128; ++j) {
    //  printf("  xxx %u (%c) %u\n", j, (j >= 32 && j < 127 ? j : '?'), props_->first_char[j]);
    //}
  }
  CachedProd(const CachedProd & o) 
    : NamedProd(o), first_char_(-3) {}
  virtual NamedProd * shallow_clone() const {return new CachedProd(*this);}
private:
  int first_char_;
  //CharSet cs;
};

Cache::LookupRes Cache::lookup(const CachedProd * prod, SourceStr str) {
  pair<Data::value_type *, bool> 
    cached = data->insert(Key(prod, str.begin));
  Res & r = cached.first->second;
  unsigned run_id = prod->persistent() ? 0 : ::run_id;
  if (cached.second) {
    r.run_id = run_id;
    return LookupRes(r, false);
  } else if (r.run_id == run_id) {
    return LookupRes(r, true);
  } else {
    r.~Res();
    new (&r) Res;
    r.run_id = run_id;
    return LookupRes(r, false);
  }
}

class TokenProd : public SymProd {
public:
  TokenProd(const SourceStr & s, String n) 
    : SymProd(s, n) {}
  TokenProd(const TokenProd & o, Prod * p = 0) : SymProd(o,o.prod.clone(p)) {}
  TokenProd(const TokenProd & o, Mapper m) : SymProd(o,ProdWrap(o.prod, m)) {}
  const Prod * clone(Prod * p) const {return new TokenProd(*this, p);}
  const Prod * clone(Mapper m) const {return new TokenProd(*this, m);}
  void dump() const {printf("\"%s\"", ~name);}
};

namespace ParsePeg {

  using namespace parse_common;
  using parse_common::symbol;

  enum Context {NormalContext, ReparseContext};
  enum CaptureNeed {DontNeedCapture, NeedCapture, NeedReparseCapture};

  struct Res : public ProdWrap {
    const char * end;
    Res() {}
    Res(const char * e, const Prod * p, CaptureQ c) : ProdWrap(p, c), end(e) {}
    Res(const char * e, const Prod * p) : ProdWrap(p), end(e) {}
    Res(const char * e, const ProdWrap & p) : ProdWrap(p), end(e) {}
    Res(Prod * p, CaptureQ c) : ProdWrap(p, c), end(p->g_span.end) {assert(end);}
    explicit Res(Prod * p) : ProdWrap(p), end(p->g_span.end) {assert(end);}
    explicit Res(const ProdWrap & p) : ProdWrap(p), end(p.prod->g_span.end) {assert(end);}
  };

  template <typename T>
  struct Sym {
    const char * pos;
    T * prod;
    Sym(const char * p1, T * p2) : pos(p1), prod(p2) {}
  };

  struct TokenRule {
    const Prod * to_match;
    const Prod * if_matched;
    String desc;
    TokenRule(const Prod * p1, const Prod * p2, String d) : to_match(p1), if_matched(p2), desc(d) {}
  };

  class Parse {
  public:
    Parse(const SourceFile * f, PEG * p) : file(f), peg(p) {}
    const SourceFile * file;
    PEG * peg;

    SourceStr mk_src(const char * b, const char * e) 
      {return SourceStr(&file->base_block.base_info, b, e);}

    const char * begin;

    hash_set<String> trans_prods;
    Vector< Sym<NamedProd> > unresolved_syms;
    Vector< Sym<TokenProd> > token_syms;
    ProdWrap                 existing; // used when redefining

    void top(const char * str, const char * end);

    Res p_peg(const char * str, const char * end, char eos, CaptureNeed = DontNeedCapture);
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

struct PEG {
  const SourceFile * file;
  hash_map<String, NamedProd *> named_prods;
  Vector<ParsePeg::TokenRule>   token_rules;
  PEG() : file() {}
  PEG(const PEG &);
  void dump() const;
};

PEG * parse_peg(const char * fn) {
  PEG * peg = new PEG;
  printf("THE PEG=%p\n", peg);
  peg->file = new_source_file(fn);
  ParsePeg::Parse parse(peg->file, peg);
  try {
    parse.top(peg->file->begin(), peg->file->end());
  } catch (Error * err) {
    err->msg.span.source = &peg->file->base_block.base_info;
    throw err;
  }
  return peg;
}

PEG * extend_peg(const PEG * orig, SourceStr str) {
  PEG * peg = new PEG(*orig);
  peg->file = str.file();
  ParsePeg::Parse parse(peg->file, peg);
  try {
    parse.top(str.begin, str.end);
  } catch (Error * err) {
    err->msg.span.source = str.source;
    throw err;
  }
  return peg;
}

struct Mapper::Data : public hash_map<const Prod *, const Prod *> {};
const Prod * Mapper::operator() (const Prod * old) {
  Data::const_iterator i = data.find(old);
  if (i == data.end()) return NULL;
  return i->second;
};

PEG::PEG(const PEG & o) 
  : file(o.file) 
{
  Mapper::Data old_new;

  for (hash_map<String, NamedProd *>::const_iterator 
         i = o.named_prods.begin(), e = o.named_prods.end(); i != e; ++i) 
  {
    NamedProd * p = i->second->shallow_clone();
    named_prods.insert(i->first, p);
    old_new.insert(i->second, p);
  }

  for (hash_map<String, NamedProd *>::iterator 
         i = named_prods.begin(), e = named_prods.end(); i != e; ++i) 
    i->second->prod = ProdWrap(i->second->prod, Mapper(old_new));

  token_rules.reserve(o.token_rules.size());
  for (Vector<ParsePeg::TokenRule>::const_iterator 
         i = o.token_rules.begin(), e = o.token_rules.end(); i != e; ++i)
    token_rules.push_back(ParsePeg::TokenRule(i->to_match->clone(Mapper(old_new)),
                                              i->if_matched->clone(Mapper(old_new)),
                                              i->desc));

  // for (hash_map<String, NamedProd *>::iterator 
  //        i = named_prods.begin(), e = named_prods.end(); i != e; ++i) 
  //   i->second->get_props();

  // for (hash_map<String, NamedProd *>::iterator 
  //        i = named_prods.begin(), e = named_prods.end(); i != e; ++i) 
  //   i->second->finalize();
}

void PEG::dump() const {
  for (hash_map<String, NamedProd *>::const_iterator 
         i = named_prods.begin(), e = named_prods.end(); i != e; ++i) 
  {
    printf("%s = ", ~i->first);
    i->second->prod.dump();
    printf(";\n");
  }
  
}

namespace ParsePeg {class Parse;}

//namespace Peg {

class Capture;

MatchRes SymProd::match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
  if (!parts) return prod.match(str,NULL,env);
  MatchRes r = prod.match(str,parts,env);
  if (!r) return r;
  if (!prod.capture)
    parts->add_part(SYN(String(str.begin, r), str, r));
  return r;
}

const char * SymProd::match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
  if (desc.empty()) {
    return prod.match_f(str,errs, env);
  } else {
    ParseErrors my_errs;
    const char * r = prod.match_f(str, my_errs, env);
    if (my_errs.size() > 0 && my_errs.front()->pos <= str.begin) {
      errs.add(new ParseError(g_span, str, desc));
    } else if (my_errs.size() > 0 && my_errs.front()->expected == "<charset>") {
      errs.add(new ParseError(g_span, str, desc));
    } else {
      errs.add(my_errs);
    }
    return r;
  }
}

// if given_res is defined than don't use cache for _this_ production
MatchRes CachedProd::match_i(SourceStr str, SynBuilder * parts, Res * given_res, MatchEnviron & env) const {
  //printf("%*cMATCH %s\n", indent_level, ' ', ~name);
  if (first_char_ >= 0 && 
      (str.begin == str.end || first_char_ != *str.begin)) {
    //printf("%s FAST FAIL ON '%c'\n", ~name, first_char_);
    return FAIL;
  } else if (!props_->may_match(str.begin, str.end)) {
    //printf("%s fast fail on '%c'\n", ~name, str.begin < str.end ? *str.begin : '\0');
    return FAIL;
  }
  bool is_cached = false;
  Res * r = given_res;
  if (!given_res) {
    Cache::LookupRes lookup_res = env.cache.lookup(this, str);
    is_cached = lookup_res.exists;
    r = &lookup_res.res;
  } 
  if (!is_cached) {
    if (!given_res)
      pprintf("%*cNamedProd MISS %s %p %p\n", indent_level, ' ', ~name, str.begin, str.end);
    indent_level++;
    if (given_res) {
      prod.match_anyway(str, *given_res, env);
    } else if (prod.is_special() == IS_REPARSE) {
      MatchEnviron nenv = env;
      nenv.cache.data = new Cache::Data;
      r->end = prod.match(str, &r->res, nenv);
      if (r->end != FAIL) {
        ReparseSyntax * p = const_cast<ReparseSyntax *>(r->res.part_as_reparse());
        assert(p);
        p->parse_info.cache = nenv.cache.data;
        p->origin = name;
      }
    } else {
      r->end = prod.match(str, &r->res, env);
    }
    indent_level--;
  } else if (r->end != FAIL) {
    pprintf("%*cNamedProd HIT %s %p (%p) %p\n", indent_level, ' ', ~name, str.begin, r->end, str.end);
    if (env.antiquotes && w_antiquote())
      prod.match_anyway(str, *r, env);
  } else {
    pprintf("%*cNamedProd HIT %s %p (FAIL) %p\n", indent_level, ' ', ~name, str.begin, str.end);
  }
  if (parts) {
    if (!prod.capture && r->end)
      r->res.add_part(SYN(String(str.begin, r->end), str, r->end));
    parts->add_parts(r->res.parts_begin(), r->res.parts_end());
    parts->merge_flags(r->res.flags_begin(), r->res.flags_end());
  }
  //printf("%*cMATCH %s RES = %p\n", indent_level, ' ', ~name, r->end);
  return r->end;
}

const char * CachedProd::match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
  Cache::LookupRes r = env.cache.lookup(this, str);
  if (!r.exists || r.res.end == FAIL) {
    return NamedProd::match_f(str,errs,env);
  } else {
    return r.res.end;
  }
}

class AlwaysTrue : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder *, MatchEnviron &) const {
    return str.begin;
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron &) const {
    return str.begin;
  }
  AlwaysTrue(const SourceStr & s) : Prod(s) {
    props_obj.what = PP_OPTIONAL;
    props_obj.first_char.set();
    props_obj.first_char_high = 2;
    props_obj.first_char_eof = true;
    props_ = &props_obj;
  }
  const ProdProps & calc_props() {abort();}
  const Prod * clone(Prod *) const {return this;}
  const Prod * clone(Mapper) const {return this;}
  ProdProps props_obj;
  void finalize() {finalized = true; }
  void dump() const {printf("__");}
};

class Capture : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * res, MatchEnviron & env) const {
    if (!res) return prod.match(str, NULL, env);
    MatchRes r = prod.match(str, NULL, env);
    if (!r) return r;
    Syntax * parse = SYN(String(str.begin, r), str, r);
    //printf("NONE: %s\n", ~parse->to_string());
    res->add_part(parse);
    return r;
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    return prod.match_f(str, errs, env);
  }
  Capture(const SourceStr & s, const Prod * p)
    : Prod(s), prod(p) {capture_type = ExplicitCapture;}
  Capture(const Prod * p)
    : Prod(p->g_span), prod(p) {capture_type = ExplicitCapture;}
  const ProdProps & calc_props() {
    const ProdProps & r = prod.get_props();
    set_props(r);
    return r;
  }
  void finalize() {prod.finalize(); finalized = true;}
  //bool optional() const {return prod->optional();}
  Capture(const Capture & o, Prod * p = 0)
    : Prod(o), prod(o.prod.clone(p)) {}
  Capture(const Capture & o, Mapper m) 
    : Prod(o), prod(o.prod, m) {}
  Capture * clone(Prod * p) const {return new Capture(*this, p);}
  Capture * clone(Mapper m) const {return new Capture(*this, m);}
  void dump() const {printf("{"); prod.dump(); printf("}");}
protected:
  ProdPtr prod;
};

class ReparseInner : public Capture {
public:
  MatchRes match(SourceStr str, SynBuilder * res, MatchEnviron & env) const {
    if (!res) return prod.match(str, NULL, env);
    MatchRes r = prod.match(str, NULL, env);
    if (!r) return r;
    Syntax * parse = new ReparseSyntax(SourceStr(str, r), env.peg);
    //printf("REPARSE_CAPTURE with %p\n", parse);
    res->add_part(parse);
    return r;
  }
  ReparseInner(const SourceStr & s, const Prod * p)
    : Capture(s,p) {capture_type = ReparseCapture;}
  ReparseInner(const ReparseInner & o, Prod * p = 0) : Capture(o, p) {}
  ReparseInner(const ReparseInner & o, Mapper m) : Capture(o, m) {}
  ReparseInner * clone(Prod * p) const {return new ReparseInner(*this, p);}
  ReparseInner * clone(Mapper m) const {return new ReparseInner(*this, m);}
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
  Syntax * gather(SourceStr str, SynBuilder & in) const;
  void add_part(Syntax * syn, SynBuilder * res) const {
    if (collapse) // yeah, a bit of a hack
      syn = syn->part(0);
    if (capture_as_flag)
      res->add_flag(syn);
    else
      res->add_part(syn);
  }
  GatherParts(String n, bool caf = false)
    : name(!n.empty() ? SYN(n) : 0), 
      capture_as_flag(caf), collapse(false)
    {parse_name();}
private:
  void parse_name() {
    if (!name) return;
    const char * s = ~*name;
    if (strcmp(s, "%") == 0) {
      // special case, a single "%" is the same as not having a name
      name = NULL;
      return;
    } else if (strcmp(s, "=") == 0) {
      name = NULL;
      collapse = true;
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
  bool collapse;
};

Syntax * GatherParts::gather(SourceStr str, SynBuilder & in) const {
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

class NamedCaptureBase : public Prod {
public:
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    return prod.match_f(str, errs, env);
  }
  // FIXME: Should take in Syntax or SourceStr rater than String so we
  // can keep track of where the name came from
  NamedCaptureBase(const SourceStr & s, const ProdWrap & p)
    : Prod(s), prod(p)
    {capture_type = ExplicitCapture;}
  NamedCaptureBase(const NamedCaptureBase & o, Prod * p = 0)
    : Prod(o), prod(o.prod.clone(p)) {}
  NamedCaptureBase(const NamedCaptureBase & o, Mapper m)
    : Prod(o), prod(o.prod, m) {}
  const ProdProps & calc_props() {
    const ProdProps & r = prod.get_props();
    set_props(r);
    return r;
  }
  void finalize() {prod.finalize(); finalized = true;}
protected:
  ProdWrap prod;
};

class NamedCapture : public NamedCaptureBase , public GatherParts {
// NamedCapture can also be used as a "forced capture" if name is empty
public:
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    if (!parts) return prod.match(str, NULL, env); 
    SyntaxBuilder res;
    if (parms.empty() && name) res.add_part(name);
    MatchRes r = prod.match(str, &res, env);
    if (!r) return r;
    str.end = r;
    Syntax * res_syn = parms.empty() ? res.build(str) : gather(str, res);
    add_part(res_syn, parts);
    return r;
  }
  // FIXME: Should take in Syntax or SourceStr rater than String so we
  // can keep track of where the name came from
  NamedCapture(const SourceStr & s, const ProdWrap & p, String n, bool caf = false)
    : NamedCaptureBase(s,p), GatherParts(n, caf) {}
  NamedCapture(const NamedCapture & o, Prod * p = 0)
    : NamedCaptureBase(o, p), GatherParts(o) {}
  NamedCapture(const NamedCapture & o, Mapper m)
    : NamedCaptureBase(o, m), GatherParts(o) {}
  const Prod * clone(Prod * p) const {return new NamedCapture(*this, p);}
  const Prod * clone(Mapper m) const {return new NamedCapture(*this, m);}
  void finalize() {
    prod.finalize();
    finalized = true;
    //printf("NP: %s: %p: %u\n", name ? ~name->to_string() : "", this, parms.size());
  }
  String d_name() const {return name ? name->to_string() : String("<>");}
  void dump() const {printf("(<%s> ", name ? ~name->to_string() : ""); prod.dump(); printf(")");}
};

class S_TId : public NamedCapture {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    SyntaxBuilder res;
    if (parms.empty() && name) res.add_part(name);
    MatchRes r = prod.match(str, &res, env);
    if (!r) return r;
    const Syntax * id = res.part(1);
    //printf("TEMPLATE? %s\n", ~id->to_string());
    if (!ast::template_id(id, env.ast_env))
      return FAIL;
    //printf("TEMPLATE YES: %s\n", ~id->to_string());
    str.end = r;
    if (parts) {
      Syntax * res_syn = parms.empty() ? res.build(str) : gather(str, res);
      add_part(res_syn, parts);
    }
    return r;
  }
  S_TId(const SourceStr & s, const ProdWrap & p, String n)
    : NamedCapture(s,p,n) {}
  S_TId(const NamedCapture & o, Prod * p = 0)
    : NamedCapture(o,p) {}
  S_TId(const NamedCapture & o, Mapper m)
    : NamedCapture(o,m) {}
  const Prod * clone(Prod * p) const {return new S_TId(*this, p);}
  const Prod * clone(Mapper m) const {return new S_TId(*this, m);}
};

class PlaceHolderCapture : public NamedCaptureBase {
// NamedCapture can also be used as a "forced capture" if name is empty
public:
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    // FIXME: Explain what the hell is going on here
    if (!parts) return prod.match(str, NULL, env); 
    syntax_ns::SynEntity placeholder(str, parts);
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    SyntaxBuilder res;
    res.add_part(&placeholder);
    MatchRes r = prod.match(str, &res, env);
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
  // FIXME: Should take in Syntax or SourceStr rater than String so we
  // can keep track of where the name came from
  PlaceHolderCapture(const SourceStr & s, const ProdWrap & p)
    : NamedCaptureBase(s,p) {}
  PlaceHolderCapture(const PlaceHolderCapture & o, Prod * p = 0)
    : NamedCaptureBase(o, p) {}
  PlaceHolderCapture(const PlaceHolderCapture & o, Mapper m)
    : NamedCaptureBase(o, m) {}
  const Prod * clone(Prod * p) const {return new PlaceHolderCapture(*this, p);}
  const Prod * clone(Mapper m) const {return new PlaceHolderCapture(*this, m);}
};


class GatherPartsProd : public AlwaysTrue, public GatherParts {
// NamedCapture can also be used as a "forced capture" if name is empty
public:
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    //fprintf(stderr, "NOW COMES THE FUN!\n");
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    if (!parts) return str.begin;
    // FIXME: Explain what the hell is going on here
    SynBuilder * res = parts->part(0)->entity<SynBuilder>();
    SourceStr rstr(parts->part(0)->str(), str.begin);
    if (parms.empty() && name) parts->part(0) = name;
    else                       ++parts->parts_;
    Syntax * res_syn = parms.empty() ? parts->build(str) : gather(str, *parts);
    parts->invalidate();
    add_part(res_syn, res);
    return str.begin;
  }
  // FIXME: Should take in Syntax or SourceStr rater than String so we
  // can keep track of where the name came from
  GatherPartsProd(const SourceStr & s, String n, bool caf = false)
    : AlwaysTrue(s), GatherParts(n, caf)
    {capture_type = ExplicitCapture;}
  GatherPartsProd(const GatherPartsProd & o, Prod * p = 0)
    : AlwaysTrue(o), GatherParts(o) {}
  const Prod * clone(Prod * p) const {return this;}
  const Prod * clone(Mapper m) const {return this;}
};

class ReparseOuter : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    if (!parts) return prod.match(str, NULL, env); 
    SyntaxBuilderN<2> res;
    MatchRes r = prod.match(str, &res, env);
    if (!r) return r;
    assert(res.num_parts() == 1);
    ReparseSyntax * syn = const_cast<ReparseSyntax *>(res.part(0)->as_reparse());
    syn->str_ = syn->outer_ = SourceStr(str, r);
    syn->what_ = name;
    syn->parse_as = parse_as;
    //printf("ro: %s %s\n", ~syn->rwhat().name, syn->parse_as ? ~String(syn->parse_as) : NULL);
    parts->add_part(syn);
    return r;
  }
  void match_anyway(SourceStr str, Res & res, MatchEnviron & env) const {
    const ReparseSyntax * p = res.res.part_as_reparse();
    assert(p);
    MatchEnviron nenv = env;
    // Possible FIXME: What if a different PEG was used?
    nenv.cache.data = (Cache::Data *)p->parse_info.cache;
    prod.match_check(str, res.end, nenv);
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    return prod.match_f(str, errs, env);
  }
  ReparseOuter(const SourceStr & s, const Prod * p, String n, String pa)
    : Prod(s), prod(p), name(SYN(n)), parse_as(pa)
    {
      capture_type = ExplicitCapture;
      name = SYN(parse_common::unescape(n.begin(), n.end(), '"')); // FIXME: Add source info
      //printf("RO: %s %s\n", ~n, ~parse_as);
    }
  ReparseOuter(const ReparseOuter & o, Prod * p = 0)
    : Prod(o), prod(o.prod.clone(p)), name(o.name), parse_as(o.parse_as) {}
  ReparseOuter(const ReparseOuter & o, Mapper m)
    : Prod(o), prod(o.prod, m), name(o.name), parse_as(o.parse_as) {}
  const Prod * clone(Prod * p) const {return new ReparseOuter(*this, p);}
  const Prod * clone(Mapper m) const {return new ReparseOuter(*this, m);}
  const ProdProps & calc_props() {
    const ProdProps & r = prod.get_props();
    set_props(r);
    return r;
  }
  void finalize() {
    prod.finalize();
    finalized = true; 
  }
  void dump() const {printf("(<<reparse %s>> ", ~name->to_string()); prod->dump(); printf(")");}
  IsSpecial is_special() const {return IS_REPARSE;}
private:
  ProdPtr prod;
  const Syntax * name;
  String parse_as;
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
  MatchRes match(SourceStr str, SynBuilder *, MatchEnviron &) const {
    const char * e = str.begin;
    if (prefix_equal(literal.begin(), literal.end(), e, str.end))
      return e;
    else
      return FAIL;
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron &) const {
    const char * e = str.begin;
    if (prefix_equal(literal.begin(), literal.end(), e, str.end)) {
      return e;
    } else {
      StringBuf buf;
      buf << "\"" << literal << "\"";
      errs.add(new ParseError(g_span, str, buf.freeze()));
      return FAIL;
    }
  }  
  Literal(const SourceStr & s, String l)
    : Prod(s), literal(l) 
    {
      unsigned char c = literal[0];
      if (c < 128)
        props_obj.first_char.set(c, 1);
      else
        props_obj.first_char_high = true;
      props_ = &props_obj;
    }
  const ProdProps & calc_props() {abort();}
  const Prod * clone(Prod * p) const {return this;}
  const Prod * clone(Mapper)   const {return this;}
  void dump() const {printf("'%s'", ~literal);}
  void finalize() {finalized = true;}
private:
  String literal;
  ProdProps props_obj;
};

class CharClass : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder *, MatchEnviron &) const {
    if (!str.empty() && cs[*str])
      return str.begin + 1;
    else
      return FAIL;
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    if (!str.empty() && cs[*str]) {
      return str.begin + 1;
    } else {
      errs.add(new ParseError(g_span, str, "<charset>")); // FIXME 
      return FAIL;
    }
  }
  CharClass(const SourceStr & s, const CharSet & cs0) 
    : Prod(s), cs(cs0)
    {
      for (unsigned i = 0; i < 128; ++i)
        if (cs[i]) props_obj.first_char.set(i, 1);
      int c = 0;
      for (unsigned i = 128; i < 256; ++i)
        if (cs[i]) ++c;
      if (c == 0) props_obj.first_char_high = 0;
      else if (c < 128) props_obj.first_char_high = 1;
      else props_obj.first_char_high = 2;
      props_ = &props_obj;
    }
  const ProdProps & calc_props() {abort();}
  const Prod * clone(Prod *) const {return this;}
  const Prod * clone(Mapper) const {return this;}
  void finalize() {finalized = true;}
  void dump() const {printf("[]");}
private:
  CharSet cs;
  ProdProps props_obj;
};

class Any : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder *, MatchEnviron &) const {
    if (!str.empty())
      return str.begin+1;
    return FAIL;
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron &) const {
    if (!str.empty())
      return str.begin+1;
    errs.push_back(new ParseError(g_span, str, "<EOF>"));
    return FAIL;
  }
  Any(const SourceStr & s) : Prod(s) 
    {
      props_obj.first_char.set();
      props_obj.first_char_high = true;
      props_ = &props_obj;
    }
  const ProdProps & calc_props() {abort();}
  const Prod * clone(Prod * p) const {return this;}
  const Prod * clone(Mapper)   const {return this;}
  void dump() const {printf("_");}
  void finalize() {finalized = true;}
  ProdProps props_obj;
};

class Repeat : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    bool matched = false;
    for (;;) {
      if (end_with_) {
        MatchRes r = end_with_.match(str, NULL, env);
        if (r) break;
      }
      MatchRes r = prod.match(str, parts, env);
      if (!r) break;
      str.begin = r;
      matched = true;
      if (once) break;
    }
    if (matched || optional)
      return str.begin;
    else 
      return FAIL;
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    bool matched = false;
    for (;;) {
      if (end_with_) {
        const char * r = end_with_.match_f(str, errs, env);
        if (r) break;
      }
      const char * r = prod.match_f(str, errs, env);
      if (!r) break;
      str.begin = r;
      matched = true;
      if (once) break;
    }
    if (matched || optional)
      return str.begin;
    else 
      return FAIL;
  }
  Repeat(const SourceStr & s, const ProdWrap & p, bool o1, bool o2) 
    : Prod(s), prod(p), optional(o1), once(o2), end_with_() 
    {capture_type = p.capture ? ExplicitCapture : NoCapture;}
  Repeat(const Repeat & o, Prod * p = 0) 
    : Prod(o), prod(o.prod.clone(p)), optional(o.optional), once(o.once) 
  { 
    if (o.end_with_) {
      end_with_ = o.end_with_.clone(p);
    }
  }
  Repeat(const Repeat & o, Mapper m) 
    : Prod(o), prod(o.prod,m), optional(o.optional), once(o.once), 
      end_with_(o.end_with_, m) {}
  virtual void end_with(const Prod * p);
  const Prod * clone(Prod * p) const {return new Repeat(*this, p);}
  const Prod * clone(Mapper m) const {return new Repeat(*this, m);}
  void dump() const {
    prod.dump();
    if (optional && once)
      printf("?");
    else if (optional && !once)
      printf("*");
    else if (!optional && !once)
      printf("+");
    if (end_with_) {
      printf(" .");
    }
  }
  const ProdProps & calc_props() {
    props_obj.reset();
    bool first = true;
    merge_for_seq(prod.get_props(), props_obj, first);
    if (end_with_) {
      ProdProps r0 = end_with_.get_props();
      r0.invert_char_info();
      merge_for_seq(r0, props_obj, first);
    }
    if (optional)
      props_obj.what = PP_OPTIONAL;
    set_props(props_obj);
    return props_obj;
  }
  void finalize() {prod.finalize(); if (end_with_) end_with_.finalize(); finalized = true;}
private:
  ProdWrap prod;
  ProdProps props_obj;
  bool optional;
  bool once;
  ProdPtr end_with_;
};

class Predicate : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder *, MatchEnviron & env) const {
    MatchRes r = prod.match(str, NULL, env);
    if (dont_match_empty && r == str.begin)
      r = FAIL;
    if (r) {
      return MatchRes(invert ? FAIL : str.begin);
    } else {
      return MatchRes(invert ? str.begin : FAIL);
    }
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    // FIXME: Correctly handle errors;
    return Predicate::match(str, NULL, env);
  }
  Predicate(const SourceStr & s, const Prod * p, bool inv, bool dme = false) 
    : Prod(s), prod(p), invert(inv), dont_match_empty(dme) {}
  Predicate(const Predicate & o, Prod * p = 0)
    : Prod(o), prod(o.prod.clone(p)), 
      invert(o.invert), dont_match_empty(o.dont_match_empty) {}
  Predicate(const Predicate & o, Mapper m)
    : Prod(o), prod(o.prod,m), 
      invert(o.invert), dont_match_empty(o.dont_match_empty) {}
  const Prod * clone(Prod * p) const {return new Predicate(*this, p);}
  const Prod * clone(Mapper m) const {return new Predicate(*this, m);}
  void dump() const {
    if (invert)
      printf("!");
    else
      printf("&");
    prod.dump();        
  }
  const ProdProps & calc_props() {
    props_obj = prod.get_props();
    props_obj.what = PP_PREDICATE;
    if (dont_match_empty)
      props_obj.first_char_eof = false;
    if (invert) {
      props_obj.invert_char_info();
    }
    set_props(props_obj);
    return props_obj;
  }
  void finalize() {prod.finalize(); finalized = true;}
private:
  ProdPtr prod;
  ProdProps props_obj;
  bool invert;
  bool dont_match_empty;
};

void Repeat::end_with(const Prod * p) {
  assert(!end_with_);
  end_with_ = new Predicate(p->g_span, p, false, true);
}

static inline CaptureType get_capture_type(const Vector<ProdWrap> & p) 
{
  CaptureType t = NoCapture;
  for (Vector<ProdWrap>::const_iterator 
         i = p.begin(), e = p.end(); i != e; ++i)
    if (i->capture) t = ExplicitCapture;
  return t;
}

class Seq : public Prod {
  // NOTE: A Seq _must_ have more than one element
public: 
  MatchRes match(SourceStr str, SynBuilder * res, MatchEnviron & env) const {
    unsigned orig_parts_sz, orig_flags_sz;
    if (res)
      orig_parts_sz = res->num_parts(), orig_flags_sz = res->num_flags();
    Vector<ProdWrap>::const_iterator 
      i = prods.begin(), e = prods.end();
    while (i != e) {
      MatchRes r = i->match(str, res, env);
      if (!r) {
        if (res) res->truncate_parts(orig_parts_sz), res->truncate_flags(orig_flags_sz);
        return FAIL;
      }
      str.begin = r;
      ++i;
    }
    return str.begin;
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    Vector<ProdWrap>::const_iterator 
      i = prods.begin(), e = prods.end();
    while (i != e) {
      const char * r = i->match_f(str, errs, env);
      if (!r)
        return FAIL;
      str.begin = r;
      ++i;
    }
    return str.begin;
  }
  Seq(const SourceStr & s, const Vector<ProdWrap> & p)
    : Prod(s), prods(p) 
    {capture_type = get_capture_type(prods);}
  const ProdProps & calc_props() {
    Vector<ProdWrap>::reverse_iterator 
      i = prods.rbegin(), e = prods.rend();
    props_obj.reset();
    bool first = true;
    for (; i != e; ++i)
      merge_for_seq(i->get_props(), props_obj, first);
    set_props(props_obj);
    return props_obj;
  }
  void finalize() {
    for (Vector<ProdWrap>::iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
      i->finalize();
    finalized = true;
  }
  Seq(const Seq & o, Prod * p = 0) : Prod(o) {
    prods.reserve(o.prods.size());
    for (Vector<ProdWrap>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i) 
    {
      prods.push_back(i->clone(p));
    }
  }
  Seq(const Seq & o, Mapper m) : Prod(o) {
    prods.reserve(o.prods.size());
    for (Vector<ProdWrap>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i) 
    {
      prods.push_back(ProdWrap(*i,m));
    }
  }
  const Prod * clone(Prod * p) const {return new Seq(*this, p);}
  const Prod * clone(Mapper m) const {return new Seq(*this, m);}
  void dump() const {
    printf("(");
    for (Vector<ProdWrap>::const_iterator 
           i = prods.begin(), e = prods.end(); i != e;) 
    {
      i->dump();
      ++i;
      if (i != e) printf(" ");
    }
    printf(")");
  }
private:
  Vector<ProdWrap> prods;
  ProdProps props_obj;
};

class Choice : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    if (jump_table) {
      //printf("using jump table\n");
      ProdWrap * i;
      if (str.empty())
        i = jump_table[CHAR_EOF];
      else {
        unsigned c = str[0];
        if (c < 128)
          i = jump_table[c];
        else
          i = jump_table[CHAR_HIGH];
      }
      while (i->prod) {
        MatchRes r = i->match(str, parts, env);
        if (r) return r;
        ++i;
      }
    } else {
      Vector<ProdWrap>::const_iterator 
        i = prods.begin(), e = prods.end();
      while (i != e) {
        MatchRes r = i->match(str, parts, env);
        if (r) return r;
        ++i;
      }
    }
    return FAIL;
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    Vector<ProdWrap>::const_iterator 
      i = prods.begin(), e = prods.end();
    while (i != e) {
      const char * r = i->match_f(str, errs, env);
      if (r) return r;
      ++i;
    }
    return FAIL;
  }
  Choice(const SourceStr & s, const Vector<ProdWrap> & p)
    : Prod(s), prods(p), jump_table() 
    {capture_type = get_capture_type(prods);}
  Choice(const Choice & o, Prod * p = 0) : Prod(o), jump_table() {
    prods.reserve(o.prods.size());
    for (Vector<ProdWrap>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i) 
    {
      prods.push_back(i->clone(p));
    }
  }
  Choice(const Choice & o, Mapper m) : Prod(o), jump_table() {
    prods.reserve(o.prods.size());
    for (Vector<ProdWrap>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i) 
    {
      prods.push_back(ProdWrap(*i,m));
    }
  }
  const ProdProps & calc_props() {
    //printf("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ\n");
    props_obj.reset();
    for (Vector<ProdWrap>::iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i) 
    {
      const ProdProps & r = i->get_props();
      //printf("ZZZ <%s>\n", ~i->prod->d_name());
      //for (unsigned j = 0; j != 128; ++j) {
      //  printf("  zzz %u (%c) %u\n", j, (j >= 32 && j < 127 ? j : '?'), r.first_char[j]);
      //}
      //printf(" zzz %d\n", UINT_MAX - r.deps);
      merge_for_choice(props_obj, r);
    }
    //printf("zzzzzzzzzzzzzzzzzzzzzzzzzzzzz\n");
    set_props(props_obj);
    return props_obj;
  }
  void finalize();
  const Prod * clone(Prod * p) const {return new Choice(*this, p);}
  const Prod * clone(Mapper m) const {return new Choice(*this, m);}
  void dump() const {
    printf("(");
    for (Vector<ProdWrap>::const_iterator 
           i = prods.begin(), e = prods.end(); i != e;) 
    {
      i->dump();
      ++i;
      if (i != e) printf(" / ");
    }
    printf(")");
  }
private:
  static const unsigned CHAR_HIGH = 128;
  static const unsigned CHAR_EOF = 129;
  Vector<ProdWrap> prods;
  ProdProps props_obj;
  ProdWrap * * jump_table;
  ProdWrap * jump_table_data;
};

struct WorkingNode {
  ProdWrap * prod;
  enum {LAST_PREPENDED, LEN, JUMP_POS} tag;
  union {
    WorkingNode * last_prepended;
    unsigned len;
    ProdWrap * jump_pos;
  };
  WorkingNode * next;
  WorkingNode(ProdWrap * p, WorkingNode * n = NULL)
    : prod(p), tag(LAST_PREPENDED), next(n) {last_prepended = NULL;}
};

static inline void prepend_node(WorkingNode * & t, ProdWrap * p) {
  WorkingNode * n = t->last_prepended;
  if (!n || n->prod != p) {
    n = new WorkingNode(p, t);
    t->tag = WorkingNode::LAST_PREPENDED;
    t->last_prepended = n;
  }
  t = n;
}


void Choice::finalize() {
  typedef WorkingNode Node;
  ProdProps res;
  Vector<ProdWrap>::iterator 
    i = prods.begin(), e = prods.end();
  Node * dummy = NULL;
  Node * sel[130];
  bool do_jump_table = prods.size() > 3;
  //printf("DO JUMP TABLE: %d\n", do_jump_table);
  if (do_jump_table) {
    dummy = new Node(NULL);
    for (unsigned i = 0; i < 130; ++i)
      sel[i] = dummy;
  }
  for (; i != e; ++i) {
    i->finalize();
    if (do_jump_table) {
      const ProdProps & r = i->props();
      if (r.what == PP_OPTIONAL) {
        for (unsigned j = 0; j < 130; ++j)
          prepend_node(sel[j], &*i);
      } else {
        //printf("%*c<>%d\n", indent_level, ' ', i - prods.begin());
        for (unsigned j = 0; j < 128; ++j) {
          if (r.first_char[j]) {
            //printf("%*c  HAVE %d\n", indent_level, ' ', j); 
            prepend_node(sel[j], &*i);
          }
        }
        if (r.first_char_high) 
          prepend_node(sel[CHAR_HIGH], &*i);
        if (r.first_char_eof)
          prepend_node(sel[CHAR_EOF], &*i);
      }
    }
  }
  if (do_jump_table) {
    unsigned num = 0;
    unsigned total_len = 0;
    unsigned min_len = UINT_MAX;
    dummy->tag = Node::LEN;
    dummy->len = 0;
    for (unsigned j = 0; j < 130; ++j) {
      if (sel[j]->tag == Node::LAST_PREPENDED) {
        unsigned len = 0;
        for (Node * n = sel[j]; n != dummy; n = n->next)
          len++;
        num += 1;
        total_len += len;
        min_len = std::min(min_len, len);
        assert(sel[j]->prod);
        sel[j]->tag = Node::LEN;
        sel[j]->len = len;
      }
    }
    if (min_len >= prods.size() - 1) {
      goto ret;
    }
    jump_table = (ProdWrap * *)GC_MALLOC(sizeof(void *) * 130);
    unsigned jump_table_data_size = num + total_len + 1;
    jump_table_data = (ProdWrap *)GC_MALLOC(sizeof(Prod) * jump_table_data_size);
    new (jump_table_data + 0) ProdWrap;
    ProdWrap * pos = jump_table_data + 1;
    for (unsigned j = 0; j < 130; ++j) {
      if (sel[j]->tag == Node::JUMP_POS) {
        jump_table[j] = sel[j]->jump_pos;
      } else if (sel[j]->len == 0) {
        jump_table[j] = jump_table_data + 0;
      } else if (sel[j]->len > 0) {
        assert(pos - jump_table_data < jump_table_data_size);
        unsigned len = sel[j]->len;
        // the data is in the reverse order...
        pos += len + 1;
        new (--pos) ProdWrap;
        for (Node * n = sel[j]; n != dummy; n = n->next) {
          new (--pos) ProdWrap(*n->prod);
        }
        jump_table[j] = pos;
        sel[j]->tag = Node::JUMP_POS;
        sel[j]->jump_pos = pos;
        pos += len + 1;
      }
    }
//     printf("\n%*c---\nww %u\n", indent_level, ' ', prods.size());
//     for (unsigned j = 0; j != 130; ++j) {
//       printf("xx %u (%c) %u\n", j, (j >= 32 && j < 127 ? j : '?'), jump_table[j] - jump_table_data);
//     }
//     for (unsigned j = 0; j != jump_table_data_size; ++j) {
//       if (!jump_table_data[j].prod) printf("\n" "yy %u:", j + 1);
//       else {
//         Vector<ProdWrap>::iterator i = prods.begin(), e = prods.end();
//         while (i != e && i->prod != jump_table_data[j].prod) ++i;
//         assert (i != e);
//         printf(" %d", i - prods.begin());
//       }
//     }
//     printf("\n");
  }
ret: 0;
  finalized = true;
}

String cur_named_prod;

class S_MId : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * res, MatchEnviron & env) const {
    //if (!in_repl) return FAIL;
    SyntaxBuilder res0;
    MatchRes r = prod.match(str, &res0, env);
    if (!r) return r;
    res0.make_flags_parts();
    assert(res0.single_part());
    Syntax * r0 = res0.part(0);
    Syntax * arg = r0->arg(0);
    bool have_mid = false;
    if (arg->is_a("antiquote")) {
      arg = arg->arg(0);
      have_mid = true;
    } else if (env.mids && env.mids->anywhere(*arg) > 0) {
      have_mid = true;
    }
    if (have_mid) {
      Syntax * p = in_named_prod == "-" 
        ? SYN(r0->str(), r0->part(0), arg)
        : SYN(r0->str(), r0->part(0), arg, SYN(in_named_prod));
      //printf("MATCH MID %s %s\n", ~p->to_string(), ~p->sample_w_loc());
      if (res) res->add_part(p);
      return r;
    } else {
      return MatchRes(FAIL);
    }
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    //if (!in_repl) return FAIL;
    SyntaxBuilder res0;
    MatchRes r = prod.match(str, &res0, env);
    if (!r) {
      const char * r0 = prod.match_f(str, errs, env);
      //assert(!r0);
      return FAIL;
    }
    res0.make_flags_parts();
    assert(res0.single_part());
    Syntax * r0 = res0.part(0);
    if (env.mids && env.mids->anywhere(*r0->arg(0)) > 0) {
      return r;
    } else {
      return FAIL; // FIXME: Inject Error
    }

  }
  S_MId(const SourceStr & s, const ProdWrap & p, String inp = String())
    : Prod(s), in_named_prod(inp.empty() ? cur_named_prod : inp), prod(new NamedCapture(s, p, "mid")) {capture_type = ExplicitCapture;}
  S_MId(const S_MId & o, Prod * p = 0)
    : Prod(o), in_named_prod(o.in_named_prod), prod(o.prod.clone(p)) {}
  S_MId(const S_MId & o, Mapper m)
    : Prod(o), in_named_prod(o.in_named_prod), prod(o.prod, m) {}
  const Prod * clone(Prod * p) const {return new S_MId(*this, p);}
  const Prod * clone(Mapper m) const {return new S_MId(*this, m);}
  const ProdProps & calc_props() {
    props_obj = prod.get_props(); 
    props_obj.uses_special |= props_obj.USES_MID;
    set_props(props_obj);
    return props_obj;
  }
  void finalize() {prod.finalize(); finalized = true;}
  void dump() const {prod.dump();}
private:
  String in_named_prod;
  ProdPtr prod;
  ProdProps props_obj;
};

MatchRes parse_as_quasiquote(SourceStr str, SynBuilder * parts, 
                             ProdWrap prod, const Syntax * name,
                             MatchEnviron & env, MutableSyntax * given_aql = NULL)
{
  //fprintf(stdout, "IN QUASIQUOTE: %s\n", ~sample(str.begin, str.end));
  if (!parts) return prod.match(str, NULL, env); 
  
  MatchEnviron nenv = env;
  AntiQuoteList aql(given_aql ? given_aql : macro_abi::new_syntax_list());
  nenv.antiquotes = &aql;
  
  SyntaxBuilderN<3> res;
  res.add_part(name);
  MatchRes r = prod.match(str, &res, nenv);
  if (!r) return r;
  str.end = r;
  res.add_part(aql.els);
  
  Syntax * res_syn = res.build(str);
  //fprintf(stdout, "QUASIQUOTE RES: %s\n", ~res_syn->to_string());
  parts->add_part(res_syn);
  return r;
}

class S_QuasiQuote : public NamedCaptureBase {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, MatchEnviron & env) const {
    return parse_as_quasiquote(str, parts, prod, name, env);
  }
  void match_anyway(SourceStr str, Res & res, MatchEnviron & env) const {
    //fprintf(stdout, "in quasiquote: %s\n", ~sample(str.begin, str.end));
    assert(res.res.single_part());
    const Syntax * syn = res.res.part(0);
    MatchEnviron nenv = env;
    AntiQuoteList aql(const_cast<MutableSyntax *>(syn->arg(1)->as_expandable()));
    nenv.antiquotes = &aql;
    prod.match_check(str, res.end, nenv);
  }
  S_QuasiQuote(const SourceStr & s, const ProdWrap & p)
    : NamedCaptureBase(s,p), name(SYN("quasiquote")) {capture_type = ExplicitCapture;}
  S_QuasiQuote(const S_QuasiQuote & o, Mapper m)
    : NamedCaptureBase(o, m), name(o.name) {}
  Prod * clone(Prod *) const {return new S_QuasiQuote(*this);}
  Prod * clone(Mapper m) const {return new S_QuasiQuote(*this, m);}
public: // but don't use
  const Syntax * name;
};

class S_AntiQuote : public Prod {
public:
  MatchRes match_i(SourceStr str, const Syntax * * the_res, MatchEnviron & env) const {
    //fprintf(stdout, "IN ACTIVE ANTIQUOTE: %s\n", ~sample(str.begin, str.end));
    MatchRes r;
    Syntax * syn = env.antiquotes->get();
    MatchEnviron nenv = env;
    nenv.antiquotes = NULL;
    if (!syn) {
      SyntaxBuilderN<1> syn_buf;
      r = prod.match(str, &syn_buf, nenv);
      if (!r) return r;
      assert(syn_buf.single_part());
      syn = syn_buf.part(0);
      env.antiquotes->set(syn);
    } else {
      r = prod.match(str, NULL, nenv);
      if (!r) return r;
    }
    if (the_res) 
      *the_res = mk_antiquote(str, env.antiquotes->pos);
    env.antiquotes->adv();
    return r;
  }
  MatchRes match(SourceStr str, SynBuilder * the_res, MatchEnviron & env) const {
    if (env.antiquotes) {
      const Syntax * * new_syn = NULL;
      if (the_res) {
        assert(the_res->empty());
        the_res->add_part(NULL);
        new_syn = &the_res->part(0);
      }
      return match_i(str, new_syn, env);
    } else if (!the_res) {
      //fprintf(stdout, "antiquote no res\n");
      return prod.match(str, NULL, env);
    } else {
      SyntaxBuilderN<1> syn_buf;
      MatchRes r = prod.match(str, &syn_buf, env);
      if (r) {
        assert(syn_buf.single_part());
        const ReparseSyntax * s = syn_buf.part(0)->maybe_reparse();
        unsigned idx = env.mids 
          ? env.mids->lookup_antiquote(s->outer_.begin) 
          : NPOS;
        if (idx != NPOS) {
          //fprintf(stdout, "antiquote found res\n");
          the_res->add_part(mk_antiquote(str, idx));
        } else {
          //printf("antiquote dummy res: %s\n", ~sample(str.begin, str.end));
          the_res->add_part(AQ_NONE);
        }
      }
      return r;
    }
  }
  const Syntax * mk_antiquote(const SourceStr & str, unsigned idx) const {
    bool with_at = str.begin[0] == '@';
    char buf[16];
    snprintf(buf, 16, "%saq.%u", with_at ? "@" : "", idx);
    return SYN(ANTIQUOTE, SYN(buf));
  }
  void match_anyway(SourceStr str, Res & res, MatchEnviron & env) const {
    if (env.antiquotes) {
      assert(res.res.single_part());
      const Syntax * syn = res.res.part(0);
      const Syntax * * new_syn = syn == AQ_NONE ? &res.res.part(0) : NULL;
      MatchRes r = match_i(str, new_syn, env);
      assert(r == res.end);
    } else {
      //fprintf(stderr, "antiquote match_anyway\n");
      prod.match_check(str,res.end,env);
    }
  }
  const char * match_f(SourceStr str, ParseErrors & errs, MatchEnviron & env) const {
    MatchEnviron nenv = env;
    nenv.antiquotes = NULL;
    return prod.match_f(str, errs, nenv);
  }
  S_AntiQuote(const SourceStr & s, const ProdWrap & p)
    : Prod(s), prod(p.prod)
    {
      assert(p.capture); 
      assert(prod->capture_type == ExplicitCapture); 
      capture_type = ExplicitCapture;
      ANTIQUOTE = SYN("antiquote");
      AQ_NONE = SYN("aq.none");
      // FIXME: Check to make sure sub-prod doesn't contain MID
    }
  const ProdProps & calc_props() {
    props_obj = prod.get_props(); 
    props_obj.uses_special |= props_obj.USES_ANTIQUOTE;
    set_props(props_obj);
    return props_obj;
  }
  void finalize() {prod.finalize(); finalized = true;}
  S_AntiQuote(const S_AntiQuote & o, Mapper m) 
    : Prod(o), prod(o.prod, m), ANTIQUOTE(o.ANTIQUOTE), AQ_NONE(o.AQ_NONE) {}
  S_AntiQuote * clone(Prod * p) const {return new S_AntiQuote(*this);}
  S_AntiQuote * clone(Mapper m) const {return new S_AntiQuote(*this,m);}
  void dump() {/*prod.dump();*/}
protected:
  ProdPtr prod;
  Syntax * ANTIQUOTE;
  const Syntax * AQ_NONE;
  ProdProps props_obj;
};

const Syntax * parse_prod(String what, SourceStr & str, ParseInfo p_i,
                          ast::Environ * ast_env,
                          const Replacements * repls,
                          bool match_complete_str,
                          ParseAsQuasiQuote pqq)
{
  pprintf("BEGIN %s: (%p) %s\n", ~what, str.begin, ~sample(str.begin, str.end));
  //printf("PARSE STR %.*s as %s\n", str.end - str.begin, str.begin, ~what);
  //clock_t start = clock();
  MatchEnviron env;
  env.peg = p_i.peg;
  env.mids = repls;
  env.ast_env = ast_env;
  hash_map<String, NamedProd *>::const_iterator 
    i = p_i.peg->named_prods.find(what);
  if (i == p_i.peg->named_prods.end()) abort(); // FIXME: Error message
  const Prod * p = i->second;
  env.cache.reset(p_i.cache ? (Cache::Data *)p_i.cache : new Cache::Data);
  SyntaxBuilder dummy;
  const char * s = str.begin;
  const char * e = pqq
    ? parse_as_quasiquote(str, &dummy, ProdWrap(p, DoCapture), SYN("quasiquote"), env, pqq.aql)
    : p->match(str, &dummy, env);
  env.mids = 0;
  if (!e || (match_complete_str && e != str.end)) {
    ParseErrors errors;
    const char * e0 = p->match_f(str, errors, env);
    assert(e0 == e);
    //if (match_complete_str) abort();
    //if (what == "STMT") abort();
    throw errors.to_error(str.source, p_i.peg->file)->add_note(extra_parse_info(str, what));
  }
  str.begin = e;
  //clock_t stop = clock();
  //printf("PARSE STR ... as %s time: %f\n", ~what, (stop - start)/1000000.0);
  dummy.make_flags_parts();
  assert(dummy.single_part());
  return dummy.part(0);
}

namespace ParsePeg {

  const char * opt_desc(const char * str, const char * end, String & res) {
    if (*str == ':') {
      if (end - str > 1 && str[1] == '=') return str;
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
          SubStr fn0;
          str = spacing(str, end);
          assert(*str == '"'); // fixme error message
          str = quote('"', str, end, fn0);
          StringBuf fn1;
          unescape(fn0.begin, fn0.end, fn1, '"');
          String fn = fn1.freeze();
          fn = add_dir_if_needed(fn, peg->file);
          //fprintf(stderr, "hint_file = %s\n", ~file);
          SourceFile * hints = new_source_file(fn);
          try {
            parse_hints_file(hints->begin(), hints->end());
          } catch (Error * err) {
            // OK this is very bad, fix latter
            err->msg.span.source = &hints->base_block.base_info;
            //fprintf(stderr, "%s\n", err->message().c_str());
            exit(1);
          }
        } else {
          throw error(str, "Unknown directive: %s\n", ~name);
        }
      } else if (*str == '"') {
        // ie a defination of a special token
        str = symbol('"', str, end);
        Res sr = p_peg(str, end, '"');
        pprintf("PARSING TOKEN PROD: %s\n", ~sample(str, sr.end));
        str = sr.end;
        str = require_symbol('"', str, end);
        //sr.prod->verify();
        String desc;
        str = opt_desc(str, end, desc);
        str = require_symbol('=', str, end);
        Res r = p_peg(str, end, ';');
        //r.prod->verify();
        peg->token_rules.push_back(TokenRule(sr.prod, r.prod, desc));
        str = r.end;
      } else {
        str = id(str, end, name);
        pprintf("PARSING NAMED PROD: %s\n", ~name);
        cur_named_prod = name; 
        NamedProd * & p = peg->named_prods[name];
        //printf("NAME: %s %p\n", name.c_str(), p);
        if (p == 0) p = new_named_prod(name);
        String desc;
        str = opt_desc(str, end, desc);
        bool redefine = false;
        if (*str == '=') { 
          if (p->prod.prod)
            throw error(str, "Redefining %s", ~name);
          str += 1;
        } else if (end - str > 1 && str[0] == ':' && str[1] == '=') {
          if (!p->prod.prod)
            throw error(str, "Redefining undefined prod: %s", ~name);
          str += 2;
          redefine = true;
          existing = p->prod;
        } else {
          throw error(str, "Expected '=' or ':='");
        }
        str = spacing(str, end);
        //bool explicit_capture = false;
        Res r = p_peg(str, end, ';');
        //r.prod->verify();
        //if (name == "SPLIT_FLAG") stop();
        existing = ProdWrap();
        p->desc = desc;
        p->prod = r;
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
    
    hash_map<String, NamedProd *>::iterator i = peg->named_prods.begin(), e = peg->named_prods.end();
    for (i = peg->named_prods.begin(); i != e; ++i) {
      //printf("AAA %s\n", ~i->second->name);
      i->second->get_props();
      if (i->second->persistent())
        pprintf("IS PERSISTENT: %s\n", ~i->second->name);
      else
        pprintf("is not persistent: %s\n", ~i->second->name);
      //printf("%s INFO:\n", ~i->second->name);
      //for (unsigned j = 0; j < 128; ++j) {
      //  if (i->second->props.first_char[j]) 
      //    printf("  may have %u '%c'\n", j, (j >= 32 && j < 127 ? j : ' '));
      //}
      //if (i->second->props.first_char_high)
      //  printf("  may have first char high (%u)\n", i->second->props.first_char_high);
      //if (i->second->props.may_match_empty)
      //  printf("  may match empty\n");
    }
    pprintf("PERSISTENT DONE\n");
    //for (i = named_prods.begin(); i != e; ++i) {
    //  printf("MAY MATCH EMPTY? %s %d\n", ~i->second->name, i->second->props().may_match_empty);
    //}
    for (i = peg->named_prods.begin(); i != e; ++i)
      i->second->finalize();
  }

  void Parse::resolve_token_symbol(TokenProd * p, const char * pos) 
  {
    MatchEnviron dummy_env;
    Vector<TokenRule>::iterator i = peg->token_rules.begin(), e = peg->token_rules.end();
    for (;i != e; ++i) {
      //ParseErrors errors;
      SourceStr str(new SourceBlock(p->name));
      str.begin = i->to_match->match(str, NULL, dummy_env);
      if (str.empty()) {
        p->desc = i->desc;
        p->prod = ProdWrap(i->if_matched->clone(new Capture(new Literal(p->g_span, p->name))));
        return;
      }
    }
    throw error(pos, "Could not find a match for token symbol: %s", p->name.c_str());
  }

  Res Parse::p_peg(const char * str, const char * end, char eos, CaptureNeed nc)
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
          return Res(str, new Capture(mk_src(start, str), prods[0].prod));
      case NeedReparseCapture:
        return Res(str, new ReparseInner(mk_src(start, str), prods[0].prod));
      }
    } else {
      switch (nc) {
      case DontNeedCapture:
        return Res(str, new Choice(mk_src(start, str), prods));
      case NeedCapture: 
        for (Vector<ProdWrap>::iterator 
               i = prods.begin(), e = prods.end(); i != e; ++i)
        {
          if (i->prod->capture_type == NoCapture)
            i->prod = new Capture(i->prod);
          i->capture = true;
        }
        return Res(str, new Choice(mk_src(start, str), prods));
      case NeedReparseCapture:
        return Res(str, new ReparseInner(mk_src(start, str), new Choice(mk_src(start, end), prods)));
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
    static const unsigned MAX_PARTS = 3;
    SubStr special[MAX_PARTS];
    enum SpecialType {SP_NONE, SP_NAME_LATER, SP_MID, SP_REPARSE, SP_TID, 
                      SP_QUASIQUOTE, SP_QUASIQUOTE_PART, SP_ANTIQUOTE} sp = SP_NONE;
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
        unsigned i = 0;
        while (*str != '>') {
          if (i >= MAX_PARTS) throw error(str, "Too many parts in << >>");
          str = spacing(str, end);
          special[i].begin = str;
          while (str != end && *str != '>' && !asc_isspace(*str)) {
            if (*str == '\\') {
              ++str;
              if (str == end)
                throw error(str, "Unexpected end of string");
            }
            ++str;
          }
          special[i].end = str;
          if (str == end) throw error(str, "Unterminated >");
          ++i;
        }
        ++str;
        if (special[0] == "") {
          sp = SP_NAME_LATER;
        } else if (special[0] == "mid") {
          name = special[0];
          sp = SP_MID;
        } else if (special[0] == "reparse") {
          name = special[1];
          sp = SP_REPARSE;
        } else if (special[0] == "tid") {
          name = special[0];
          sp = SP_TID;
        } else if (special[0] == "quasiquote") {
          name = special[0];
          sp = SP_QUASIQUOTE;
        } else if (special[0] == "antiquote") {
          name = special[0];
          sp = SP_ANTIQUOTE;
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
      prod = ProdWrap(new AlwaysTrue(mk_src(start, str)));
    else if (prods.size() == 1)
      prod = prods[0];
    else
      prod = ProdWrap(new Seq(mk_src(start, str), prods));
    if (sp == SP_NAME_LATER)
      return Res(new PlaceHolderCapture(mk_src(start, str), prod));
    else if (sp == SP_MID)
      return Res(new S_MId(mk_src(start, str), prod, String(special[1])));
    else if (sp == SP_REPARSE)
      return Res(new ReparseOuter(mk_src(start, str), 
                                  prod.prod, name, special[2] ? String(special[2]) : String()));
    else if (sp == SP_TID) 
      return Res(new S_TId(mk_src(start, str), prod, name));
    else if (sp == SP_QUASIQUOTE)
      return Res(new S_QuasiQuote(mk_src(start, str), prod));
    else if (sp == SP_ANTIQUOTE)
      return Res(new S_AntiQuote(mk_src(start, str), prod));
    if (named_capture)
      return Res(new NamedCapture(mk_src(start, str), prod, name, capture_as_flag));
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
      r1.end_with(r2.prod); // FIXME: Maybe should memorize r2
      Vector<ProdWrap> prods;
      prods.push_back(r1);
      prods.push_back(r2);
      return Res(new Seq(mk_src(start, str), prods));
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
      return Res(new Predicate(mk_src(start, str), r.prod, true));
    } else if (*str == '&') {
      str = symbol('&', str, end);
      Res r = suffix(str, end, NormalContext);
      str = r.end;
      return Res(new Predicate(mk_src(start, str), r.prod, false));
    } else if (*str == '=') {
      str = symbol('=', str, end);
      str = spacing(str, end);
      if (*str != '<') throw error (str, "'<' expected");
      SubStr name;
      str = quote('>', str, end, name);
      return Res(new GatherPartsProd(mk_src(start, str), String(name)));
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
      return Res(new Repeat(mk_src(start, str), r, true, true));
    } else if (*str == '*') { 
      str = symbol('*', str, end);
      return Res(new Repeat(mk_src(start, str), r, true, false));
    } else if (*str == '+') {
      str = symbol('+', str, end);
      return Res(new Repeat(mk_src(start, str), r, false, false));
    } else {
      return r;
    }
  }

  Res Parse::primary(const char * str, const char * end, Context context) 
  {
    const char * start = str;
    if (*str == '(') {
      str = symbol('(', str, end);
      Res r = p_peg(str, end, ')');
      str = require_symbol(')', r.end, end);
      return Res(str, r.prod);
    } else if (*str == '{') {
      str = symbol('{', str, end);
      Res r = p_peg(str, end, '}', context == NormalContext ? NeedCapture : NeedReparseCapture);
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
    return Res(new Literal(mk_src(start, str), lit));
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
    return Res(new CharClass(mk_src(start, str), set1));
  }

  Res Parse::token(const char * str, const char * end) {
    const char * start = str;
    SubStr name0;
    str = quote('"', str, end, name0);
    String name = unescape(name0);
    TokenProd * p = new TokenProd(mk_src(start, str), name);
    token_syms.push_back(Sym<TokenProd>(start, p));
    return Res(str, p);
  }

  Res Parse::identifier(const char * str, const char * end) {
    const char * start = str;
    String name;
    str = id(str, end, name);
    if (name == "_") {
      return Res(new Any(mk_src(start, str)));
    } else if (name == "__") {
      return Res(new AlwaysTrue(mk_src(start, str)));
    } else if (name == "_self") {
      // special symbol for "tokens"
      // FIXME: Check that we really can use this symbol here
      return Res(new SelfSymProd(mk_src(start, str), name));
    } else if (name == "_cur") {
      return Res(str,existing);
    } else {
      pprintf("PARSING    DEP: %s\n", ~name);
      NamedProd * & p = peg->named_prods[name];
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
    return error(source, front()->pos, "%s", ~buf.freeze());
  }
}

void ParseErrors::print(const SourceInfo * file, const SourceFile * grammer)
{
  if (empty()) {
    printf("Parse Failed (no specific error)\n");
  } else {
    Pos pos = file->file()->get_pos(front()->pos);
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

