//#include <sys/types.h>
//#include <sys/stat.h>
//#include <fcntl.h>
#include <assert.h>

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

class AlwaysTrue : public Prod {
public:
  const char * match(SourceStr str, Parts * parse, ParseErrors & errs) {
    return str.begin;
  }
  AlwaysTrue(const char * p, const char * e) : Prod(p,e) {}
  virtual Prod * clone(Prod *) {return new AlwaysTrue(*this);}
  void dump() {printf(" _TRUE ");}
};

class Capture : public Prod {
public:
  const char * match(SourceStr str, Parts * parts, ParseErrors & errs) {
    if (!parts) return prod->match(str, 0, errs);
    if (prod->capture_type.is_single()) {
      //return prod->match(str, parts, errs);
      const char * r = prod->match(str, parts, errs);
      return r;
    } else if (prod->capture_type.is_multi()) {
      const char * r = prod->match(str, parts, errs);
      return r;
    } else { // type is None
      const char * s = prod->match(str, 0, errs);
      if (!s) return FAIL;
      Syntax * parse = new Syntax(String(str.begin, s), str, s);
      parts->append(parse);
      return s;
    }
  }
  Capture(const char * s, const char * e, Prod * p, bool implicit = false)
    : Prod(s,e), prod(p) {
    if (implicit)
      capture_type.set_to_implicit();
    else
      capture_type.set_to_explicit();
  }
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
  const char * match(SourceStr str, Parts * parts, ParseErrors & errs) {
    //printf("NAMED CAPTURE (%s) %p\n", name ? ~name->name : "", this);
    if (!parts) return prod->match(str, 0, errs); 
    Parts lparts;
    const char * s = prod->match(str, prod->capture_type.is_explicit() ? &lparts : 0, errs);
    if (!s) return FAIL;
    Syntax * parse;
    if (parms.empty()) {
      if (name)
        parse = new Syntax(name);
      else
        parse = new Syntax();
      parse->add_parts(lparts);
    } else {
      Vector<Parm>::const_iterator i = parms.begin(), e = parms.end();
      if (i->start == NPOS) {
        parse = new Syntax(i->name);
      } else {
        parse = new Syntax(lparts[i->start]);
      }
      ++i;
      for (; i != e; ++i) {
        if (i->start == NPOS) {
          parse->add_part(i->name);
        } else if (i->start != NPOS) {
          for (int j = i->start; j < i->stop && j < lparts.size(); ++j) {
            parse->add_part(lparts[j]);
          }
        }
      }
    }
    parse->str_ = str;
    parse->str_.end = s;
    parts->append(parse);
    return s;
  }

  // FIXME: Should take in Syntax or SourceStr rater than String so we
  // can keep track of where the name came from
  NamedCapture(const char * s, const char * e, Prod * p, String n)
    : Prod(s,e), prod(p), name(!n.empty() ? new Syntax(n) : 0) {capture_type.set_to_explicit(); parse_name();}
  NamedCapture(const NamedCapture & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)), name(o.name), parms(o.parms) {}
  virtual Prod * clone(Prod * p) {return new NamedCapture(*this, p);}
  void verify() {
    prod->verify();
  }
  void dump() {printf("{"); prod->dump(); printf("}");}
private:
  void parse_name() {
    if (!name) return;
    const char * s = ~*name;
    const char * e = first_space(s);
    if (s[0] != '%' && !*e) {
      name = new Syntax(parse_common::unescape(s, e, '"')); // FIXME: Add source info
      return;
    }
    unsigned last_used = 0; // FIXME: Misnamed
    unsigned dots = NPOS;
    unsigned i = 0;
    for (;;) {
      Parm p;
      p.name = new Syntax(parse_common::unescape(s, e, '"')); // FIXME: Add source info
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
      p.name = new Syntax("...");
      dots = parms.size();
      parms.push_back(p);
    }
    parms[dots].start = last_used;
    parms[dots].stop = NPOS;
  }
  Prod * prod;
  const Syntax * name;
  Vector<Parm> parms;
};

class DescProd : public Prod {
public:
  const char * match(SourceStr str, Parts * parts, ParseErrors & errs) {
    ParseErrors my_errs;
    const char * s = prod->match(str, parts, my_errs);
    if (my_errs.size() > 0 && !desc.empty() && my_errs.front()->pos <= str) {
      errs.add(new ParseError(pos, str, desc));
    } else if (my_errs.size() > 0 && !desc.empty() && my_errs.front()->expected == "<charset>") {
      errs.add(new ParseError(pos, str, desc));
    } else {
      errs.add(my_errs);
    }
    return s;
  }
  DescProd(const char * s, const char * e, String n, Prod * p) 
    : Prod(s, e),  desc(n), prod(p) {capture_type = p->capture_type;}
  DescProd(const DescProd & o, Prod * p = 0) : Prod(o), desc(o.desc) {
    prod = o.prod->clone(p);
  }
  virtual Prod * clone(Prod * p) {return new DescProd(*this, p);}
  void verify() {prod->verify();}
private:
  String desc;
  Prod * prod;
};

class Literal : public Prod {
public:
  const char * match(SourceStr str, Parts *, ParseErrors & errs) {
    if (str.size() >= literal.size() && std::equal(literal.begin(), literal.end(), str.begin)) {
      return str + literal.size();
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
private:
  String literal;
};

class CharClass : public Prod {
public:
  const char * match(SourceStr str, Parts *, ParseErrors & errs) {
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
private:
  CharSet cs;
};

class Any : public Prod {
public:
  const char * match(SourceStr str, Parts *, ParseErrors & errs)  {
    if (!str.empty())
      return str+1;
    errs.push_back(new ParseError(pos, str, "<EOF>"));
    return FAIL;
  }
  Any(const char * s, const char * e) : Prod(s,e) {}
  virtual Prod * clone(Prod * p) {return new Any(*this);}
  void dump() {printf("_");}
};

class Repeat : public Prod {
public:
  const char * match(SourceStr str, Parts * parts, ParseErrors & errs) {
    const char * s;
    bool matched = false;
    for (;;) {
      if (end_with_ && end_with_->match(str, 0, errs)) break;
      s = prod->match(str, prod->capture_type.is_explicit() ? parts : 0, errs);
      if (s == FAIL) break;
      str.begin = s;
      matched = true;
      if (once) break;
    }
    if (matched || optional)
      return str;
    else 
      return FAIL;
  }
  Repeat(const char * s, const char * e, Prod * p, bool o1, bool o2) 
    : Prod(s,e), prod(p), optional(o1), once(o2), end_with_() {
    if (p->capture_type.is_explicit())
      capture_type.set_to_explicit(false);
    else
      capture_type.set_to_none();
  }
  Repeat(const Repeat & o, Prod * p = 0) 
    : Prod(o), prod(o.prod->clone(p)), optional(o.optional), once(o.once) 
    {if (o.end_with_) end_with_ = o.end_with_->clone(p);}
  virtual void end_with(Prod * p);
  virtual Prod * clone(Prod * p) {return new Repeat(*this, p);}
  void dump() {}
  void verify() {prod->verify(); if (end_with_) end_with_->verify();}
private:
  Prod * prod;
  bool optional;
  bool once;
  Prod * end_with_;
};

class Predicate : public Prod {
public:
  const char * match(SourceStr str, Parts *, ParseErrors & errs) {
    // FIXME: Correctly handle errors;
    ParseErrors dummy_errs = errs;
    const char * s = prod->match(str, 0, dummy_errs);
    if (dont_match_empty && s == str.begin)
      s = FAIL;
    if (s) {
      return invert ? FAIL : str;
    } else {
      return invert ? str : FAIL;
    }
  }
  Predicate(const char * s, const char * e, Prod * p, bool inv, bool dme = false) 
    : Prod(s,e), prod(p), invert(inv), dont_match_empty(dme) {}
  Predicate(const Predicate & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)), 
      invert(o.invert), dont_match_empty(o.dont_match_empty) {}
  virtual Prod * clone(Prod * p) {return new Predicate(*this, p);}
  void dump() {}
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
  const char * match(SourceStr str, Parts * res, ParseErrors & errs) {
    unsigned orig_sz = res ? res->size() : (unsigned)-1;
    Vector<Prod *>::iterator 
      i = prods.begin(), e = prods.end();
    while (i != e) {
      str.begin = (*i)->match(str, (*i)->capture_type.is_explicit() ? res : 0, errs);
      if (str.begin == FAIL) {
        if (res) res->resize(orig_sz);
        return FAIL;
      }
      ++i;
    }
    return str.begin;
  }
  Seq(const char * s, const char * e, const Vector<Prod *> & p) 
    : Prod(s, e), prods(p) {
    int num = 0;
    for (Vector<Prod *>::const_iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
      if ((*i)->capture_type.is_explicit()) 
        ++num;
    if (num == 0)
      capture_type.set_to_none();
    else if (num == 1)
      capture_type.set_to_explicit(true);
    else
      capture_type.set_to_explicit(false);
  }
  void verify() {
    for (Vector<Prod *>::const_iterator 
           i = prods.begin(), e = prods.end(); i != e; ++i)
      (*i)->verify();
  }

  Seq(const Seq & o, Prod * p = 0) : Prod(o) {
    for (Vector<Prod *>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i) {
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
  const char * match(SourceStr str, Parts * parts, ParseErrors & errs) {
    Vector<Prod *>::iterator 
      i = prods.begin(), e = prods.end();
    while (i != e) {
      Parts * p = parts;
      if (capture_type.is_explicit() && !(*i)->capture_type.is_explicit())
        p = 0;
      const char * s = (*i)->match(str, p, errs);
      if (s) return s;
      ++i;
    }
    return FAIL;
  }
  Choice(const char * s, const char * e, const Vector<Prod *> & p) 
    : Prod(s,e), prods(p) {
    Vector<Prod *>::iterator i = prods.begin(), end = prods.end();
    CaptureType x = (*i)->capture_type;
    ++i;
    for (; i != end; ++i) {
      CaptureType y = (*i)->capture_type;
      if (x == y) {
        // noop
      } else if (x.is_explicit() || y.is_explicit()) {
        x = CaptureType::ExplicitMulti;
        //printf("ct: %d\n", (int)x);
      } else {
        x = CaptureType::Implicit;
      }
    }
    if (x == CaptureType::Implicit) {
      for (i = prods.begin(); i != end; ++i) {
        if ((*i)->capture_type.is_none())
          *i = new Capture((*i)->pos, (*i)->end, *i, true);
      }
    }
    capture_type = x;
  }
  Choice(const Choice & o, Prod * p = 0) : Prod(o) {
    for (Vector<Prod *>::const_iterator 
           i = o.prods.begin(), e = o.prods.end(); i != e; ++i)
      prods.push_back((*i)->clone(p));
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
  const char * match(SourceStr str, Parts * res, ParseErrors & errs) {
    //if (!in_repl) return FAIL;
    Parts prts;
    const char * r = prod->match(str, &prts, errs);
    if (r == FAIL) return r;
    assert(prts.size() == 1);
    if (mids && mids->anywhere(*prts[0]->arg(0)) > 0) {
      Syntax * p = new Syntax(prts[0]->str(), prts[0]->part(0), prts[0]->arg(0), new Syntax(in_named_prod));
      if (res) res->append(p);
      return r;
    } else {
      return FAIL; // FIXME Inject Error
    }
  }
  S_MId(const char * s, const char * e, Prod * p)
    : Prod(s,e), in_named_prod(cur_named_prod), prod(new NamedCapture(s, e, p, "mid")) {capture_type.set_to_explicit();}
  S_MId(const S_MId & o, Prod * p = 0)
    : Prod(o), in_named_prod(o.in_named_prod), prod(o.prod->clone(p)) {}
  virtual Prod * clone(Prod * p) {return new S_MId(*this, p);}
private:
  String in_named_prod;
  Prod * prod;
};

#if 0
class S_Map : public Prod {
public:
  const char * match(SourceStr str, Parts * res, ParseErrors & errs) {
    const char * r = prod->match(str,res, errs);
    if (r == FAIL) return r;
    mids.clear();
    return r;
  }
  S_Map(const char * s, const char * e, Prod * p)
    : Prod(s,e), prod(p) {capture_type = p->capture_type;}
  S_Map(const S_Map & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)) {}
  virtual Prod * clone(Prod * p) {return new S_Map(*this, p);}
private:
  Prod * prod;
};

class S_MParm : public Prod {
public:
  const char * match(SourceStr str, Parts * res, ParseErrors & errs) {
    Parts prts;
    const char * r = prod->match(str, &prts, errs);
    if (r == FAIL) return r;
    assert (prts.size() == 1);
    mids.insert(prts[0]->name);
    if (res) res->append(prts);
    return r;
  }
  S_MParm(const char * s, const char * e, Prod * p)
    : Prod(s,e), prod(p) {capture_type = p->capture_type;}
  S_MParm(const S_MParm & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)) {}
  virtual Prod * clone(Prod * p) {return new S_MParm(*this, p);}
private:
  Prod * prod;
};

class S_Repl : public Prod {
public:
  const char * match(SourceStr str, Parts * res, ParseErrors & errs) {
    in_repl = true;
    const char * r = prod->match(str, res, errs);
    in_repl = false;
    return r;
  }
  S_Repl(const char * s, const char * e, Prod * p)
    : Prod(s,e), prod(p) {capture_type = p->capture_type;}
  S_Repl(const S_Repl & o, Prod * p = 0)
    : Prod(o), prod(o.prod->clone(p)) {}
  virtual Prod * clone(Prod * p) {return new S_Repl(*this, p);}
private:
  Prod * prod;
};

#endif

const Syntax * parse_str(String what, SourceStr str, const Replacements * repls) {
  //printf("PARSE STR as %s\n", ~what);
  mids = repls;
  Prod * p = parse.named_prods[what];
  parse.clear_cache();
  Parts dummy;
  ParseErrors errors;
  //const char * s = str.begin;
  const char * e = p->match(str, &dummy, errors);
  mids = 0;
  //assert(s != e);
  //printf("%p %p %p : %.*s\n", s, e, str.end, str.end - str.begin, str.begin);
  if (e == str.end) {
    //printf(">>%.*s<<\n", e-s, s);
    //dummy.print();
    //printf("\n");
  } else {
    //printf("FAIL\n");
    throw errors.to_error(str.source->file_, file);
  }
  return dummy[0];
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
      if (*str == '"') {
        str = symbol('"', str, end);
        Res sr = peg(str, end, '"', true);
        str = sr.end;
        str = require_symbol('"', str, end);
        sr.prod->verify();
        String desc;
        str = opt_desc(str, end, desc);
        str = require_symbol('=', str, end);
        Res r = peg(str, end, ';', true);
        r.prod->verify();
        token_rules.push_back(TokenRule(sr.prod, 
                                        desc.empty() ? r.prod : new DescProd(str, r.end, desc, r.prod)));
        str = r.end;
      } else {
        str = id(str, end, name);
        cur_named_prod = name; 
        NamedProd * & p = named_prods[name];
        //printf("NAME: %s %p\n", name.c_str(), p);
        if (p == 0) p = new NamedProd(name);
        String desc;
        str = opt_desc(str, end, desc);
        str = require_symbol('=', str, end);
        Res r = peg(str, end, ';', true);
        r.prod->verify();
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
      str.begin = i->to_match->match(str, 0, errors);
      if (str.empty()) {
        p->set_prod(i->if_matched->clone(new Capture(p->pos, p->end, 
                                                     new Literal(p->pos, p->end, p->name),
                                                     true)));
        return;
      }
    }
    throw error(pos, "Could not find a match for token symbol: %s", p->name.c_str());
  }

  Res Parse::peg(const char * str, const char * end, char eos, bool capture, bool implicit) 
  {
    const char * start = str;
    str = spacing(str, end);
    Vector<Prod *> prods;
    while (str != end) {
      Res r = sequence(str, end, eos);
      str = r.end;
      if (capture)
        prods.push_back(new Capture(start, end, r.prod, implicit));
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
  
  Res Parse::sequence(const char * str, const char * end, char eos) 
  {
    const char * start = str;
    Vector<Prod *> prods;
    bool named_capture = false;
    SubStr name;
    SubStr special;
    if (*str == '<') {
      named_capture = true;
      const char * str2 = str + 1;
      if (str2 != end && *str2 == '<') {
        str = quote('>', str2, end, special);
        name = special;
        str = require_symbol('>', str, end);
      } else {
        str = quote('>', str, end, name);
      }
    }
    while (str != end && *str != eos && *str != '/') {
      Res r = sequence2(str, end);
      str = r.end;
      prods.push_back(r.prod);
    }
    Prod * prod;
    if (prods.size() == 0)
      prod = new AlwaysTrue(start, str);
    else if (prods.size() == 1)
      prod = prods[0];
    else
      prod = new Seq(start, str, prods);
    if (String(special) == "mid") 
      return Res(new S_MId(start, str, prod));
    //else if (special == "map") 
    //  return Res(new S_Map(start, str, prod));
    //else if (special == "mparm") 
    //  return Res(new S_MParm(start, str, prod));
    //else if (special == "repl") 
    //  return Res(new S_Repl(start, str, prod));
    else if (!special.empty()) 
      throw error(start, "Unknown special");
    if (named_capture)
      return Res(new NamedCapture(start, str, prod, name));
    else
      return Res(str, prod);
  }

  Res Parse::sequence2(const char * str, const char * end)
  {
    const char * start = str;
    Res r1 = prefix(str, end);
    str = r1.end;
    if (*str == '.') {
      str = symbol('.', str, end);
      Res r2 = prefix(str, end);
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

  Res Parse::prefix(const char * str, const char * end) 
  {
    const char * start = str;
    if (*str == '!') {
      str = symbol('!', str, end);
      Res r = suffix(str, end);
      str = r.end;
      return Res(new Predicate(start, str, r.prod, true));
    } else if (*str == '&') {
      str = symbol('&', str, end);
      Res r = suffix(str, end);
      str = r.end;
      return Res(new Predicate(start, str, r.prod, false));
    } else {
      return suffix(str, end);
    }
  }

  Res Parse::suffix(const char * str, const char * end) 
  {
    const char * start = str;
    Res r = primary(str, end);
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

  Res Parse::primary(const char * str, const char * end) 
  {
    const char * start = str;
    if (*str == '(') {
      str = symbol('(', str, end);
      Res r = peg(str, end, ')');
      str = require_symbol(')', r.end, end);
      return Res(str, r.prod);
    } else if (*str == '{') {
      str = symbol('{', str, end);
      Res r = peg(str, end, '}', true, false);
      str = require_symbol('}', r.end, end);
      return Res(new Capture(start, str, r.prod));
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

Error * ParseErrors::to_error(const SourceFile * file, const SourceFile * grammer)
{    
  if (empty()) {
    return error(file->entity(), 0, "Parse Failed (no specific error)\n");
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
    return error(file->entity(), front()->pos, ~buf.freeze());
  }
}

void ParseErrors::print(const SourceFile * file, const SourceFile * grammer)
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

