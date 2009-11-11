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

namespace ParsePeg {class Parse;}

//namespace Peg {

class Capture;

static unsigned indent_level = 0;

MatchRes NamedProd::match(SourceStr str, SynBuilder * parts, ParseErrors & errs) {
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
    //else printf("XXX COULD'T USE %s\n", ~name);
  }
  Res * r;
  if (i == cached.second) {
    r = &(*((lookup.insert(str, Res())).first)).second;
    r->end = FAIL; // to avoid infinite recursion
    //printf("%*cNamedProd MISS %s %p %p\n", indent_level, ' ', ~name, str.begin, str.end);
    //indent_level++;
    //if (prod->capture_type.is_none())
    //  printf("NP: %s: %d %d\n", ~name, (bool)parts, (CaptureType::Type)prod->capture_type);
    *static_cast<MatchRes *>(r) = prod->match(str, (prod->capture_type.is_any() ? &r->res : NULL), r->errors);
    if (r->read_to >= str.end) {
      //printf("PAST END ON %s\n", ~name);
      r->read_to = NULL;
      r->str_end = str.end;
    }
    //indent_level--;
    //if (r->end != FAIL) {
    //  printf("%*cNamedProd DONE %s %p (%p) \"%s\" %p\n", indent_level, ' ', ~name, str.begin, r->end, ~sample(str.begin, r->end), str.end);
    //  if (SubStr(str.begin, r->end) == "environ_snapshot()")
    //    printf("  SYN: %s\n", ~r->res.build()->to_string());
    //}
    // else
    //  printf("%*cNamedProd DONE %s %p (FAIL) %p\n", indent_level, ' ', ~name, str.begin, str.end);
    //assert(r->end == FAIL || r->parts.size() == 1);
  } else {
    r = &i->second;
    //if (r->end != FAIL)
    //  printf("%*cNamedProd HIT %s %p (%p) %p\n", indent_level, ' ', ~name, str.begin, r->end, str.end);
    //else
    //  printf("%*cNamedProd HIT %s %p (FAIL) %p\n", indent_level, ' ', ~name, str.begin, str.end);
  }
  errs.add(r->errors);
  if (parts) {
    parts->add_parts(r->res.parts_begin(), r->res.parts_end());
    parts->merge_flags(r->res.flags_begin(), r->res.flags_end());
  }
  return *r;
}

void ParsePeg::Parse::clear_cache() {
  //printf("NamedProd CLEAR\n");
  hash_map<String, NamedProd *>::iterator i = named_prods.begin(), e = named_prods.end();
  for (; i != e; ++i) {
    // printf("%s %s persistent\n", ~i->second->name, i->second->persistent() ? "is" : "is not");
    if (!i->second->persistent()) {
      //printf("CLEARING %s\n", ~i->second->name);
      i->second->clear_cache();
    }
  }
}


class AlwaysTrue : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder *, ParseErrors & errs) {
    return MatchRes(str.begin, NULL);
  }
  AlwaysTrue(const char * p, const char * e) : Prod(p,e) {}
  virtual Prod * clone(Prod *) {return new AlwaysTrue(*this);}
  void dump() {printf(" _TRUE ");}
};

class Capture : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * res, ParseErrors & errs) {
    if (!res) return prod->match(str, NULL, errs);
    if (reparse_capture) {
      MatchRes r = prod->match(str, NULL, errs);
      if (!r) return r;
      Syntax * parse = new ReparseSyntax(SourceStr(str, r.end));
      //printf("REPARSE_CAPTURE with %p\n", parse);
      res->add_part(parse);
      return r;
    } else if (prod->capture_type.is_any()) {
      //assert(prod->capture_type == capture_type);
      return prod->match(str, res, errs);
    } else { // type is None
      MatchRes r = prod->match(str, NULL, errs);
      if (!r) return r;
      Syntax * parse = SYN(String(str.begin, r.end), str, r.end);
      //printf("NONE: %s\n", ~parse->to_string());
      res->add_part(parse);
      return r;
    }
  }
  Capture(const char * s, const char * e, Prod * p, bool implicit = false)
    : Prod(s,e), prod(p), reparse_capture(false) {
    if (implicit && p->capture_type.is_none())
      capture_type.set_to_implicit();
    else
      capture_type.set_to_explicit();
  }
  void set_persistent() const {persistent_ = prod->persistent();}
  void verify() {
    // FIXME: Make error message
    //assert(prod->capture_type.is_none() || prod->capture_type.is_single());
    prod->verify();
  }
  Capture(const Capture & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)) {}
  virtual Capture * clone(Prod * p) {return new Capture(*this, p);}
  void dump() {printf("{"); prod->dump(); printf("}");}
private:
  Prod * prod;
public: // but don't use
  bool reparse_capture;
};

Prod * new_capture(const char * s, const char * e, Prod * p, bool implicit = false) {
  if (implicit && p->capture_type.is_any()) return p;
  return new Capture(s,e,p,implicit);
}

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

void stop() {}

class NamedCapture : public Prod {
// NamedCapture can also be used as a "forced capture" if name is empty
private:
  struct Parm { // one or the other
    Parm() : start(NPOS), stop(NPOS) {}
    const Syntax * name;
    unsigned start;
    unsigned stop;
  };
public:
  MatchRes match(SourceStr str, SynBuilder * parts, ParseErrors & errs) {
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    if (!parts) return prod->match(str, NULL, errs); 
    SyntaxBuilder res;
    if (parms.empty() && name) res.add_part(name);
    MatchRes r = prod->match(str, prod->capture_type.is_explicit() ? &res : NULL, errs);
    if (!r) return r;
    Syntax * res_syn;
    str.end = r.end;
    if (parms.empty()) {
      res_syn = res.build(str);
    } else {
      SyntaxBuilder res2;
      Vector<Parm>::const_iterator i = parms.begin(), e = parms.end();
      if (i->start == NPOS) {
        res2.add_part(i->name);
      } else {
        res2.add_part(res.part(i->start));
      }
      ++i;
      for (; i != e; ++i) {
        if (i->start == NPOS) {
          res2.add_part(i->name);
        } else if (i->start != NPOS) {
          for (int j = i->start; j < i->stop && j < res.num_parts(); ++j) {
            res2.add_part(res.part(j));
          }
        }
      }
      res2.set_flags(res.flags_begin(), res.flags_end());
      res_syn = res2.build(str);
    }
    if (capture_as_flag) {
      parts->add_flag(res_syn);
      //printf("FLAG! %s\n", ~parse->to_string());
    } else
      parts->add_part(res_syn);
    return r;
  }

  // FIXME: Should take in Syntax or SourceStr rater than String so we
  // can keep track of where the name came from
  NamedCapture(const char * s, const char * e, Prod * p, String n, bool caf = false)
    : Prod(s,e), prod(p), name(!n.empty() ? SYN(n) : 0), capture_as_flag(caf) 
    {capture_type.set_to_explicit(); parse_name();}
  NamedCapture(const NamedCapture & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)), name(o.name), parms(o.parms) {}
  virtual Prod * clone(Prod * p) {return new NamedCapture(*this, p);}
  void set_persistent() const {persistent_ = prod->persistent();}
  void verify() {
    prod->verify();
  }
  void dump() {printf("{"); prod->dump(); printf("}");}
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
  Prod * prod;
  const Syntax * name;
  Vector<Parm> parms;
  bool capture_as_flag;
};

class ReparseOuter : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, ParseErrors & errs) {
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    if (!parts) return prod->match(str, NULL, errs); 
    SyntaxBuilderN<2> res;
    MatchRes r = prod->match(str, &res, errs);
    if (!r) return r;
    assert(res.num_parts() == 1);
    ReparseSyntax * syn = const_cast<ReparseSyntax *>(res.part(0)->as_reparse());
    syn->str_ = SourceStr(str, r.end);
    syn->parts_[0] = name;
    syn->finalize();
    parts->add_part(syn);
    return r;
  }
  ReparseOuter(const char * s, const char * e, Prod * p, String n)
    : Prod(s,e), prod(p), name(SYN(n)) 
    {
      capture_type.set_to_explicit();
      name = SYN(parse_common::unescape(n.begin(), n.end(), '"')); // FIXME: Add source info
    }
  ReparseOuter(const ReparseOuter & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)), name(o.name) {}
  virtual Prod * clone(Prod * p) {return new ReparseOuter(*this, p);}
  void verify() {
    prod->verify();
  }
  void dump() {printf("{"); prod->dump(); printf("}");}
private:
  Prod * prod;
  const Syntax * name;
};

class DescProd : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, ParseErrors & errs) {
    ParseErrors my_errs;
    MatchRes r = prod->match(str, parts, my_errs);
    if (my_errs.size() > 0 && !desc.empty() && my_errs.front()->pos <= str) {
      errs.add(new ParseError(pos, str, desc));
    } else if (my_errs.size() > 0 && !desc.empty() && my_errs.front()->expected == "<charset>") {
      errs.add(new ParseError(pos, str, desc));
    } else {
      errs.add(my_errs);
    }
    return r;
  }
  DescProd(const char * s, const char * e, String n, Prod * p) 
    : Prod(s, e),  desc(n), prod(p) {capture_type = p->capture_type;}
  DescProd(const DescProd & o, Prod * p = 0) : Prod(o), desc(o.desc) {
    prod = o.prod->clone(p);
  }
  virtual Prod * clone(Prod * p) {return new DescProd(*this, p);}
  void set_persistent() const {persistent_ = prod->persistent();}
  void verify() {prod->verify();}
private:
  String desc;
  Prod * prod;
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
  MatchRes match(SourceStr str, SynBuilder *, ParseErrors & errs) {
    const char * e = str.begin;
    if (prefix_equal(literal.begin(), literal.end(), e, str.end)) {
      return MatchRes(e, e-1);
    } else {
      StringBuf buf;
      buf << "\"" << literal << "\"";
      errs.add(new ParseError(pos, str, buf.freeze()));
      return MatchRes(FAIL, e);
    }
  }
  Literal(const char * s, const char * e, String l)
    : Prod(s,e), literal(l) {}
  virtual Prod * clone(Prod * p) {return new Literal(*this);}
  void dump() {printf("'%s'", literal.c_str());}
  void set_persistent() const {persistent_ = true;}
private:
  String literal;
};

class CharClass : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder *, ParseErrors & errs) {
    if (!str.empty() && cs[*str]) {
      return MatchRes(str + 1, str);
    } else {
      errs.add(new ParseError(pos, str, "<charset>")); // FIXME 
      return MatchRes(FAIL, str);
    }
  }
  CharClass(const char * s, const char * e, const CharSet & cs0) 
    : Prod(s,e), cs(cs0) {}
  virtual Prod * clone(Prod * p) {return new CharClass(*this);}
  void dump() {printf("[]");}
  void set_persistent() const {persistent_ = true;}
private:
  CharSet cs;
};

class Any : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder *, ParseErrors & errs)  {
    if (!str.empty())
      return MatchRes(str+1, str);
    errs.push_back(new ParseError(pos, str, "<EOF>"));
    return MatchRes(FAIL, str);
  }
  Any(const char * s, const char * e) : Prod(s,e) {}
  virtual Prod * clone(Prod * p) {return new Any(*this);}
  void dump() {printf("_");}
  void set_persistent() const {persistent_ = true;}
};

class Repeat : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, ParseErrors & errs) {
    const char * read_to = NULL;
    bool matched = false;
    for (;;) {
      if (end_with_) {
        MatchRes r = end_with_->match(str, NULL, errs);
        read_to = std::max(read_to, r.read_to);
        if (r) break;
      }
      MatchRes r = prod->match(str, prod->capture_type.is_explicit() ? parts : NULL, errs);
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
  Repeat(const char * s, const char * e, Prod * p, bool o1, bool o2) 
    : Prod(s,e), prod(p), optional(o1), once(o2), end_with_() {
    if (p->capture_type.is_explicit())
      capture_type.set_to_explicit();
    else
      capture_type.set_to_none();
  }
  Repeat(const Repeat & o, Prod * p = 0) 
    : Prod(o), prod(o.prod->clone(p)), optional(o.optional), once(o.once) 
  { 
    if (o.end_with_) {
      end_with_ = o.end_with_->clone(p);
    }
  }
  virtual void end_with(Prod * p);
  virtual Prod * clone(Prod * p) {return new Repeat(*this, p);}
  void dump() {}
  void set_persistent() const {
    persistent_ = prod->persistent();
    if (end_with_)
      if (!end_with_->persistent()) persistent_ = false;
  }
  void verify() {prod->verify(); if (end_with_) end_with_->verify();}
private:
  Prod * prod;
  bool optional;
  bool once;
  Prod * end_with_;
};

class Predicate : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder *, ParseErrors & errs) {
    // FIXME: Correctly handle errors;
    ParseErrors dummy_errs = errs;
    MatchRes r = prod->match(str, NULL, dummy_errs);
    if (dont_match_empty && r.end == str.begin)
      r.end = FAIL;
    if (r) {
      return MatchRes(invert ? FAIL : str, r.read_to);
    } else {
      return MatchRes(invert ? str : FAIL, r.read_to);
    }
  }
  Predicate(const char * s, const char * e, Prod * p, bool inv, bool dme = false) 
    : Prod(s,e), prod(p), invert(inv), dont_match_empty(dme) {}
  Predicate(const Predicate & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)), 
      invert(o.invert), dont_match_empty(o.dont_match_empty) {}
  virtual Prod * clone(Prod * p) {return new Predicate(*this, p);}
  void dump() {}
  void set_persistent() const {persistent_ = prod->persistent();}
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

class Seq : public Prod {
  // NOTE: A Seq _must_ have more than one element
public: 
  MatchRes match(SourceStr str, SynBuilder * res, ParseErrors & errs) {
    unsigned orig_parts_sz, orig_flags_sz;
    if (res)
      orig_parts_sz = res->num_parts(), orig_flags_sz = res->num_flags();
    Vector<Prod *>::iterator 
      i = prods.begin(), e = prods.end();
    const char * read_to = NULL;
    while (i != e) {
      MatchRes r = (*i)->match(str, (*i)->capture_type.is_explicit() ? res : NULL, errs);
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
  Seq(const char * s, const char * e, const Vector<Prod *> & p) 
    : Prod(s, e), prods(p) {
    int num = 0;
    for (Vector<Prod *>::const_iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
    {
      if ((*i)->capture_type.is_explicit()) 
        ++num;
    }
    if (num == 0)
      capture_type.set_to_none();
    else if (num >= 1)
      capture_type.set_to_explicit();
  }
  void set_persistent() const {
    persistent_ = true;
    for (Vector<Prod *>::const_iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
    {
      if (!(*i)->persistent()) persistent_ = false;
    }
  }
  void verify() {
    for (Vector<Prod *>::const_iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
      (*i)->verify();
  }

  Seq(const Seq & o, Prod * p = 0) : Prod(o) {
    for (Vector<Prod *>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i) 
    {
      prods.push_back((*i)->clone(p));
    }
  }
  virtual Prod * clone(Prod * p) {return new Seq(*this, p);}
  void dump() {}
private:
  Vector<Prod *> prods;
};

class Choice : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * parts, ParseErrors & errs) {
    Vector<Prod *>::iterator 
      i = prods.begin(), e = prods.end();
    const char * read_to = NULL;
    while (i != e) {
      SynBuilder * p = parts;
      if (capture_type.is_explicit() && !(*i)->capture_type.is_explicit())
        p = NULL;
      MatchRes r = (*i)->match(str, p, errs);
      read_to = std::max(read_to, r.read_to);
      if (r) return MatchRes(r.end, read_to);
      ++i;
    }
    return MatchRes(FAIL, read_to);
  }
  Choice(const char * s, const char * e, const Vector<Prod *> & p) 
    : Prod(s,e), prods(p) {
    Vector<Prod *>::iterator i = prods.begin(), end = prods.end();
    CaptureType x = (*i)->capture_type;
    ++i;
    //printf("CHOISE:\n");
    for (; i != end; ++i) {
      //printf("  %p %s %s persistent\n", *i, typeid(**i).name(), (*i)->persistent ? "is" : "is not");
      CaptureType y = (*i)->capture_type;
      if (x == y) {
        // noop
      } else if (x.is_explicit() || y.is_explicit()) {
        x = CaptureType::Explicit;
        //printf("ct: %d\n", (int)x);
      } else {
        x = CaptureType::Implicit;
      }
    }
    //if (x == CaptureType::Implicit) {
    //  for (i = prods.begin(); i != end; ++i) {
    //    if ((*i)->capture_type.is_none())
    //      *i = new Capture((*i)->pos, (*i)->end, *i, true);
    //  }
    //}
    capture_type = x;
  }
  Choice(const Choice & o, Prod * p = 0) : Prod(o) {
    for (Vector<Prod *>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i) 
    {
      prods.push_back((*i)->clone(p));
    }
  }
  void set_persistent() const {
    persistent_ = true;
    for (Vector<Prod *>::const_iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
    {
      if (!(*i)->persistent()) persistent_ = false;
    }
  }
  void verify() {
    for (Vector<Prod *>::const_iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i) {
      (*i)->verify();
    }
  }
  virtual Prod * clone(Prod * p) {return new Choice(*this, p);}
  void dump() {}
private:
  Vector<Prod *> prods;
};

// FIXME: These should't be global
bool in_repl = false;
const Replacements * mids;
String cur_named_prod;

class S_MId : public Prod {
public:
  MatchRes match(SourceStr str, SynBuilder * res, ParseErrors & errs) {
    //if (!in_repl) return FAIL;
    SyntaxBuilder res0;
    MatchRes r = prod->match(str, &res0, errs);
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
      return MatchRes(FAIL, r.read_to); // FIXME Inject Error
    }
  }
  S_MId(const char * s, const char * e, Prod * p, String inp = String())
    : Prod(s,e), in_named_prod(inp.empty() ? cur_named_prod : inp), prod(new NamedCapture(s, e, p, "mid")) {capture_type.set_to_explicit();}
  S_MId(const S_MId & o, Prod * p = 0)
    : Prod(o), in_named_prod(o.in_named_prod), prod(o.prod->clone(p)) {}
  virtual Prod * clone(Prod * p) {return new S_MId(*this, p);}
  void set_persistent() const {persistent_ = false;}
private:
  String in_named_prod;
  Prod * prod;
};

const Syntax * parse_str(String what, SourceStr str, const Replacements * repls) {
  //printf("PARSE STR %.*s as %s\n", str.end - str.begin, str.begin, ~what);
  clock_t start = clock();
  //str.source = new ParseSourceInfo(str, what);
  mids = repls;
  Prod * p = parse.named_prods[what];
  parse.clear_cache();
  SyntaxBuilder dummy;
  ParseErrors errors;
  const char * s = str.begin;
  const char * e = p->match(str, &dummy, errors).end;
  mids = 0;
  //assert(s != e);
  //printf("%p %p %p : %.*s\n", s, e, str.end, str.end - str.begin, str.begin);
  if (e == str.end) {
    //printf(">>%.*s<<\n", e-s, s);
    //dummy[0]->print();
    //printf("\n");
  } else {
    //printf("FAIL\n");
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
      bool explicit_capture = false;
      if (*str == '"') {
        // ie a defination of a special token
        str = symbol('"', str, end);
        Res sr = peg(str, end, '"', explicit_capture);
        str = sr.end;
        str = require_symbol('"', str, end);
        sr.prod->verify();
        String desc;
        str = opt_desc(str, end, desc);
        str = require_symbol('=', str, end);
        Res r = peg(str, end, ';', explicit_capture);
        r.prod->verify();
        token_rules.push_back(TokenRule(sr.prod, 
                                        desc.empty() ? r.prod : new DescProd(str, r.end, desc, r.prod)));
        str = r.end;
      } else {
        explicit_capture = false;
        str = id(str, end, name);
        cur_named_prod = name; 
        NamedProd * & p = named_prods[name];
        //printf("NAME: %s %p\n", name.c_str(), p);
        if (p == 0) p = new NamedProd(name);
        String desc;
        str = opt_desc(str, end, desc);
        str = require_symbol('=', str, end);
        //bool explicit_capture = false;
        Res r = peg(str, end, ';', explicit_capture/*, true*/);
        r.prod->verify();
        //if (name == "SPLIT_FLAG") stop();
        p->set_prod(desc.empty() ? r.prod : new DescProd(str, r.end, desc, r.prod));
        str = r.end;
      }
      str = require_symbol(';', str, end);
    }

    // resolve symbols
    for (Vector< Sym<NamedProd> >::const_iterator 
           i = unresolved_syms.begin(), e = unresolved_syms.end(); i != e; ++i) 
    {
      // make sure the symbol got resolved
      if (!i->prod->prod)
        throw error(i->pos, "Unresolved Symbol:: %s", i->prod->name.c_str());
    }

    for (Vector< Sym<TokenProd> >::const_iterator 
           i = token_syms.begin(), e = token_syms.end(); i != e; ++i) 
    {
      resolve_token_symbol(i->prod, i->pos);
    }
  }

  void Parse::resolve_token_symbol(TokenProd * p, const char * pos) 
  {
    Vector<TokenRule>::iterator i = token_rules.begin(), e = token_rules.end();
    for (;i != e; ++i) {
      ParseErrors errors;
      SourceStr str(p->name);
      str.begin = i->to_match->match(str, NULL, errors).end;
      if (str.empty()) {
        p->set_prod(i->if_matched->clone(new Capture(p->pos, p->end, 
                                                     new Literal(p->pos, p->end, p->name),
                                                     true)));
        return;
      }
    }
    throw error(pos, "Could not find a match for token symbol: %s", p->name.c_str());
  }

  Res Parse::peg(const char * str, const char * end, char eos, 
                 bool & explicit_capture, bool capture, bool implicit) 
  {
    const char * start = str;
    str = spacing(str, end);
    Vector<Prod *> prods;
    while (str != end) {
      Res r = sequence(str, end, eos, explicit_capture);
      str = r.end;
      if (capture)
        prods.push_back(new_capture(start, end, r.prod, implicit));
      else 
        prods.push_back(r.prod);
      const char * s = symbol('/', str, end);
      if (!s) break;
      str = s;
    }
    assert(prods.size() > 0);
    if (prods.size() == 1) {
      return Res(str, prods[0]);
    } else {
      return Res(new Choice(start, str, prods));
    }
  }
  
  Res Parse::sequence(const char * str, const char * end, char eos, bool & explicit_capture) 
  {
    const char * start = str;
    Vector<Prod *> prods;
    bool capture_as_flag = false;
    bool named_capture = false;
    SubStr name;
    SubStr special;
    SubStr special_arg;
    enum SpecialType {SP_NONE, SP_MID, SP_REPARSE} sp = SP_NONE;
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
        if (special == "mid") {
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
    while (str != end && *str != eos && *str != '/') {
      Res r = sequence2(str, end, explicit_capture);
      str = r.end;
      prods.push_back(r.prod);
      if (sp == SP_REPARSE) {
        if (Capture * c = dynamic_cast<Capture *>(r.prod)) {
          if (found_capture)
            throw error(start, "Only one capture allowed for reparse special.");
          c->reparse_capture = true;
          found_capture = true;
        }
      }
    }
    Prod * prod;
    if (prods.size() == 0)
      prod = new AlwaysTrue(start, str);
    else if (prods.size() == 1)
      prod = prods[0];
    else
      prod = new Seq(start, str, prods);
    if (sp == SP_MID)
      return Res(new S_MId(start, str, prod, String(special_arg)));
    else if (sp == SP_REPARSE)
      return Res(new ReparseOuter(start, str, prod, name));
    if (named_capture)
      return Res(new NamedCapture(start, str, prod, name, capture_as_flag));
    else
      return Res(str, prod);
  }

  Res Parse::sequence2(const char * str, const char * end, bool & explicit_capture)
  {
    const char * start = str;
    Res r1 = prefix(str, end, explicit_capture);
    str = r1.end;
    if (*str == '.') {
      str = symbol('.', str, end);
      Res r2 = prefix(str, end, explicit_capture);
      str = r2.end;
      r1.prod->end_with(r2.prod); // FIXME: Maybe should memorize r2
      Vector<Prod *> prods;
      prods.push_back(r1.prod);
      prods.push_back(r2.prod);
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

  Res Parse::prefix(const char * str, const char * end, bool & explicit_capture) 
  {
    const char * start = str;
    if (*str == '!') {
      str = symbol('!', str, end);
      Res r = suffix(str, end, explicit_capture);
      str = r.end;
      return Res(new Predicate(start, str, r.prod, true));
    } else if (*str == '&') {
      str = symbol('&', str, end);
      Res r = suffix(str, end, explicit_capture);
      str = r.end;
      return Res(new Predicate(start, str, r.prod, false));
    } else {
      return suffix(str, end, explicit_capture);
    }
  }

  Res Parse::suffix(const char * str, const char * end, bool & explicit_capture) 
  {
    const char * start = str;
    Res r = primary(str, end, explicit_capture);
    str = r.end;
    if (*str == '?') {
      str = symbol('?', str, end);
      return Res(new Repeat(start, str, r.prod, true, true));
    } else if (*str == '*') { 
      str = symbol('*', str, end);
      return Res(new Repeat(start, str, r.prod, true, false));
    } else if (*str == '+') {
      str = symbol('+', str, end);
      return Res(new Repeat(start, str, r.prod, false, false));
    } else {
      return Res(str, r.prod);
    }
  }

  Res Parse::primary(const char * str, const char * end, bool & explicit_capture) 
  {
    const char * start = str;
    if (*str == '(') {
      str = symbol('(', str, end);
      Res r = peg(str, end, ')', explicit_capture);
      str = require_symbol(')', r.end, end);
      return Res(str, r.prod);
    } else if (*str == '{') {
      explicit_capture = true;
      str = symbol('{', str, end);
      Res r = peg(str, end, '}', explicit_capture, true, false);
      str = require_symbol('}', r.end, end);
      return Res(new_capture(start, str, r.prod));
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
      NamedProd * & p = named_prods[name];
      if (p == 0) p = new NamedProd(name);
      unresolved_syms.push_back(Sym<NamedProd>(start, p));
      return Res(str, p);
      //return Res(str, p);
    }
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

