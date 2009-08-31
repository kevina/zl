#include <set>
#include <algorithm>

#include <stdio.h>
#include <dlfcn.h>

#include "iostream.hpp"
#include "peg.hpp"
#include "expand.hpp"
#include "ast.hpp"
#include "parse_op.hpp"
#include "parse_decl.hpp"
#include "syntax_gather.hpp"

#include "hash-t.hpp"

// 
//   EXP
//   DECL
//   STMT

//   TYPE


// (top DECL+)

// DECL:
//   (var ID TYPE EXP)
//   (talias alias TYPE)
//   (fun ID RET PARMS [BODY])
//   (struct ID [BODY])
//   (union ID [BODY])

//
// 
//

class SplitSourceInfo : public SourceInfo {
public:
  const SourceInfo * source;
  SplitSourceInfo(const SourceInfo * p, const SourceInfo * s) : SourceInfo(p), source(s) {}
  virtual const SourceFile * file() const  {return source ? source->file()  : NULL;}
  virtual const SourceInfo * block() const {return source ? source->block() : NULL;}
  virtual const SourceInfo * clone_until(const SourceInfo * stop, 
                                         const SourceInfo * new_parent) const {
    return new SplitSourceInfo(parent->clone_until(stop, new_parent), source);
  }
  bool dump_info_self(OStream & o) const {abort();}
  void dump_info(OStream & o, AlreadySeen & as, const char * prefix) const {
    //o.printf("%sSPLIT SOURCE %p %p\n", prefix, prefix, source);
    bool seen_self = as.have(this);
    as.insert(this);
    StringBuf buf;
    if (parent)
      parent->dump_info(buf, as, prefix);
    if (!seen_self && source) {
      AlreadySeen as2(as);
      StringBuf new_prefix = prefix;
      new_prefix += ". ";
      source->dump_info(o, as2, new_prefix.freeze());
    }
    o << buf.freeze();
  }
};

class ResultSourceInfo : public SourceInfo {
public:
  SourceStr orig;
  ResultSourceInfo(const SourceInfo * p, const SourceStr & o) : SourceInfo(p), orig(o) {}
  virtual const SourceInfo * clone_until(const SourceInfo * stop, 
                                         const SourceInfo * new_parent) const {
    return new ResultSourceInfo(parent->clone_until(stop, new_parent), orig);
  }
  bool dump_info_self(OStream & o) const {
    return true;
  }
};

struct ChangeSrcBase {
  virtual const SourceInfo * operator() (const SourceInfo * o) = 0;
  virtual ~ChangeSrcBase() {}
};

template <>
struct ChangeSrc<SplitSourceInfo> : public ChangeSrcBase {
  struct CacheItem {
    const SourceInfo * key;
    const SourceInfo * value;
  };
  Vector<CacheItem> cache;
  const Syntax * outer;
  const SourceInfo * new_;
  ChangeSrc(const Syntax * o, const SourceInfo * n) 
    : outer(o), new_(n) {cache.reserve(2);}
  const SourceInfo * operator() (const SourceInfo * o) {
    //printf("CHANGE SPLIT SRC\n");
    //if (o)
    //  o->dump_info(COUT, "   in>");
    for (Vector<CacheItem>::const_iterator i = cache.begin(), e = cache.end(); i != e; ++i) {
      if (i->key == o) return i->value;
    }
    CacheItem n = {n.key};
    const SourceInfo * ip = o && outer ? o->find_insertion_point(outer) : NULL; // insert just above
    //const SourceInfo * ip = NULL;
    //if (ip != NULL) printf("FOUND SOMETHING\n");
    //printf("FIND I POINT (%s): %p %p = %p\n", outer ? ~outer->sample_w_loc() : "", o, outer, ip);
    if (ip == NULL)
      n.value = new SplitSourceInfo(new_, o);
    else
      n.value = o->clone_until(ip, new SplitSourceInfo(new_,ip->parent));
    cache.push_back(n);
    //n.value->dump_info(COUT, "  out>");
    return n.value;
  }
};

//
//
//

class ReplaceSourceInfo : public SourceInfo {
public:
  const Syntax * mid;
  //const Syntax * repl;
  //const Syntax * call;
  ReplaceSourceInfo(const SourceInfo * p, const Syntax * m)
    : SourceInfo(p), mid(m) {}
  const SourceInfo * clone_until(const SourceInfo * stop, 
                                 const SourceInfo * new_parent) const {
    return new ReplaceSourceInfo(parent->clone_until(stop, new_parent), mid);
  }
  bool dump_info_self(OStream &) const;
  //ReplaceSourceInfo(const SourceInfo * s, const Syntax * m, const Syntax * r, const Syntax * c)
  //  : source(s), mid(m), repl(r), call(c) {}
};

bool ReplaceSourceInfo::dump_info_self(OStream & o) const {
  o << "when replacing ";
  mid->sample_w_loc(o);
  //o << " with ";
  //repl->sample_w_loc(o);
  //o.printf("/%s/ ", ~sample(repl->to_string(), 40));
  //o << ", in expansion of ";
  //call->sample_w_loc(o);
  return true;
}

//
//
//

const SourceInfo * SyntaxSourceInfo::clone_until(const SourceInfo * stop, 
                                                 const SourceInfo * new_parent) const 
{
  return new SyntaxSourceInfo(parent->clone_until(stop, new_parent), syntax);
}

bool SyntaxSourceInfo::dump_info_self(OStream & o) const {
  return false;
  //o << "in syntax ";
  //syntax->sample_w_loc(o);
}

//
//
//

class ExpandSourceInfo : public SourceInfo {
public:
  const Syntax * call;
  const Syntax * def;
  const Syntax * outer_ip;
  ExpandSourceInfo()
    : call(), def(), outer_ip() {}
  void set(const Syntax * c, const Syntax * d) 
    { parent = c->str().source; call = c, def = d;}
  ExpandSourceInfo(const Syntax * c, const Syntax * d) 
    : SourceInfo(c->str().source), call(c), def(d), outer_ip(NULL) {}
  ExpandSourceInfo(const SourceInfo * p, const Syntax * c, const Syntax * d, const Syntax * o) 
    : SourceInfo(p), call(c), def(d), outer_ip(o) {}
  const SourceInfo * find_insertion_point(const Syntax * outer) const {
    //printf("ESI FIND I POINT: %p\n", outer_ip);
    if (outer == outer_ip) return this;
    return parent ? parent->find_insertion_point(outer) : NULL;
  }
  const SourceInfo * clone_until(const SourceInfo * stop, 
                                 const SourceInfo * new_parent) const {
    const SourceInfo * np = (stop == this) ? new_parent : parent->clone_until(stop, new_parent);
    ExpandSourceInfo * res = new ExpandSourceInfo(np, call, def, outer_ip);
    //if (!call) printf("WARNING 123 on %p now %p\n", this, res);
    return res;
  }
  const SourceInfo * clone(const SourceInfo * new_parent) const {
    return new ExpandSourceInfo(new_parent, call, def, outer_ip);
  }

  bool dump_info_self(OStream &) const;
};

bool ExpandSourceInfo::dump_info_self(OStream & o) const {
  if (call) {
    //o.printf("(%s) ", ~call->to_string());
    o << "in expansion of ";
    call->sample_w_loc(o);
  } else {
    o << "in expansion of <unknown>\n";
  }
  return true;
}

//
//
//

// misnamed, now replaces and marks and so much more
struct ReplTable : public gc_cleanup {
  typedef Vector<std::pair<SymbolName, const Syntax *> > Table;
  Table table;
  const Syntax * lookup(SymbolName n) const {
    for (Table::const_iterator i = table.begin(); i != table.end(); ++i) {
      if (i->first == n) return i->second;
    }
    return NULL;
  }
  bool have(SymbolName n) const {
    return lookup(n);
  }
  const Syntax * lookup(String n) const {
    for (Table::const_iterator i = table.begin(); i != table.end(); ++i) {
      if (i->first.name == n) return i->second;
    }
    return NULL;
  }
  const Syntax * lookup(const char * n) const {
    for (Table::const_iterator i = table.begin(); i != table.end(); ++i) {
      if (i->first.name == n) return i->second;
    }
    return NULL;
  }
  bool have(String n) const {
    return lookup(n);
  }
  bool have(const char * n) const {
    return lookup(n);
  }
  const Syntax * lookup(const Syntax & n) const {
    return lookup(static_cast<const SymbolName &>(n));
  }
  bool have(const Syntax & n) const {
    return have(static_cast<const SymbolName &>(n));
  }
  void insert(SymbolName n, const Syntax * p) {
    table.push_back(std::pair<SymbolName, const Syntax *>(n,p));
  }
  const ast::Mark * mark;
  ExpandSourceInfo * expand_si;
  ChangeSrc<SplitSourceInfo> ci;
  const SourceInfo * outer_si;
  const SourceInfo * expand_source_info(const SourceInfo * s) {
    return ci(s);
  }
  const SourceInfo * expand_source_info(const Syntax * s) {
    return expand_source_info(s->str().source);
  }
  inline SourceStr expand_source_info_str(const SourceStr & str) {
    return SourceStr(expand_source_info(str.source), str);
  }
  SourceStr expand_source_info_str(const Syntax * s) {
    return expand_source_info_str(s->str());
  }
  void to_string(OStream & o, PrintFlags f, SyntaxGather * g) const;
  String to_string() const {
    StringBuf buf;
    to_string(buf, PrintFlags(), NULL);
    return buf.freeze();
  }
  ReplTable(const ast::Mark * m, ExpandSourceInfo * e)
    : mark(m), expand_si(e), ci(NULL, e), outer_si(NULL) {}
};

void ReplTable::to_string(OStream & o, PrintFlags f, SyntaxGather * g) const {
  if (g) {
    std::pair<unsigned, bool> r = g->repl_table_map.insert(this);
    o.printf("{%u}", r.first);
    if (!r.second) {
      PrintFlags f(4);
      StringBuf buf;
      buf.printf("(%u", g->mark_map.insert(mark));
      f.indent += 2;
      for (Table::const_iterator i = table.begin(), e = table.end(); i != e; ++i) {
        buf << " (";
        i->first.to_string(buf, g);
        buf << ' ';
        i->second->to_string(buf, f, g);
        buf << ')';
      }
      buf << ")";
      g->repl_table_map.set_str(r.first, buf.freeze());
    }
  } else {
    o.printf("{...}");
    //o.printf("{");
    //for (Table::const_iterator i = table.begin(), e = table.end(); i != e; ++i) {
    //  //o.printf("%s", ~i->first.to_string());
    //  o.printf("%s=>", ~i->first.to_string());
    //  i->second->to_string(o, f);
    //  o.printf(",");
    //}
    //o.printf("}");
  }
}


//
//
//

bool Replacements::anywhere(String s0) const {
  //const char * s = ~s0;
  //if (*s == '@') ++s;
  for (const_iterator i = begin(), e = end(); i != e; ++i)
    if ((*i)->have(s0)) return true;
  return false;
}

void Replacements::to_string(OStream & o, PrintFlags f, SyntaxGather * g) const {
  for (const_iterator i = begin(), e = end(); i != e; ++i)
    (*i)->to_string(o, f, g);
}

//
//
//

template <>
struct ChangeSrc<ExpandSourceInfo> : public ChangeSrcBase {
  ReplTable * r;
  ChangeSrc(ReplTable * r0) : r(r0) {}
  const SourceInfo * operator() (const SourceInfo * o) {
    return r->expand_source_info(o);
  }
};

//
//
//

using namespace ast;

void assert_pos(const Syntax * p, Position have, unsigned need);

void compile_for_ct(Deps & deps, Environ & env);

// see prelude.zlh

// the slightly diffrent C++ version
typedef ReplTable::Table Match;
extern "C" namespace macro_abi {
  typedef const Marks Context;
  typedef Syntax SyntaxList;
  typedef const ::Syntax Syntax;
  typedef Syntax UnmarkedSyntax;
  //struct SyntaxEnum;
  Match * match(Match * m, const UnmarkedSyntax * pattern, Syntax * with);
  Match * match_args(Match * m, const UnmarkedSyntax * pattern, Syntax * with);
  Syntax * replace(const UnmarkedSyntax * p, Match * match, Mark * mark);
  Context * get_context(Syntax * p);
  Syntax * replace_context(Syntax * p, Context * context);
  Syntax * partly_expand(Syntax *, Position pos, Environ *);
  Syntax * reparse(Syntax *, const char *, Environ *);
  SyntaxEnum * partly_expand_list(SyntaxEnum *, Position pos, Environ *);
  Syntax * pre_parse(Syntax *, Environ *);
}

String gen_sym() {
  static unsigned uniq_num = 0;
  StringBuf buf;
  buf.printf("_m_%d_", uniq_num++);
  return buf.freeze();
}

static void flatten(const char * on, const Syntax * p, Syntax * res) {
  if (!p->simple() && p->is_a(on)) {
    for (Parts::const_iterator i = p->args_begin(), e = p->args_end(); i != e; ++i)
      flatten(on, *i, res);
    res->add_flags(p);
  } else {
    res->add_part(p);
  }
}

const Syntax * flatten(const Syntax * p) {
  if (p->simple()) return p;
  Syntax * res = new Syntax(p->str());
  for (Parts::const_iterator i = p->parts_begin(), e = p->parts_end(); i != e; ++i)
    flatten("@", *i, res);
  res->add_flags(p);
  return res;
}

static const Syntax * replace(const Syntax * p, ReplTable * r, const Replacements * rs, 
                              bool allow_plain_mids, bool * splice);

struct Macro : public Declaration, public Symbol {
  SymbolKey real_name;
  static const Syntax * macro_call;
  static const Syntax * macro_def;
  struct MacroInfo {
    const Syntax * orig_call;
    const Syntax * orig_def;
    MacroInfo(const Syntax * c, const Syntax * d) {
      orig_call = macro_call;
      orig_def = macro_def;
      macro_call = c;
      macro_def = d;
    }
    ~MacroInfo() {
      macro_call = orig_call;
      macro_def = orig_def;
    }
  };
  const Syntax * def;
  virtual const Syntax * expand(const Syntax *, Environ & env) const = 0;
  const Syntax * expand(const Syntax * s, const Syntax * p, Environ & env) const {
    static unsigned c0 = 0;
    unsigned c = ++c0;
    //printf("%d EXPAND: %s %s\n", c, ~s->sample_w_loc(), ~p->sample_w_loc());
    try {
      MacroInfo whocares(s,def);
      const Syntax * res = expand(p, env);
      // Don't do this for now, might cause weird problems
      //SourceStr n = s->str();
      //n.source = new ResultSourceInfo(n.source, res->str());
      //res = new Syntax(n, *res);
      // FIXME: What is this find_insertion_point doing, it fails for test56.c
      //ExpandSourceInfo * ip 
      //  = const_cast<ExpandSourceInfo *>
      //  (dynamic_cast<const ExpandSourceInfo *>
      //   (res->str().source->find_insertion_point(res)));
      //assert(ip); 
      //printf("SETTING 123 on %p\n", ip);
      //ip->set(s, def);
      //printf("%d EXPAND IP %s %s: %p\n", c, ~s->sample_w_loc(), ~p->sample_w_loc(), ip);
      return res;
    } catch (Error * err) {
      StringBuf buf = err->extra;
      buf.printf("  while expanding %s.\n", ~s->sample_w_loc());
      err->extra = buf.freeze();
      throw err;
    }
  }
};

const Syntax * Macro::macro_call = NULL;
const Syntax * Macro::macro_def = NULL;

struct SimpleMacro : public Macro {
  const char * what() const {return "simple-macro";}
  //const SourceFile * entity;
  const Syntax * parms;
  const Syntax * free;
  const Syntax * repl;
  const SymbolNode * env;
  SimpleMacro * parse_self(const Syntax * p, Environ & e) {
    //printf("PARSING MAP %s\n%s\n", ~p->arg(0)->what().name, ~p->to_string());
    env = e.symbols.front;
    //entity = p->str().source;
    def = syn = p;
    assert_num_args(3);
    real_name = expand_binding(p->arg(0), e);
    parms = flatten(p->arg(1));
    free = p->flag("free");
    if (free)
      free = free->arg(0);
    ChangeSrc<SyntaxSourceInfo> cs(p->arg(2));
    repl = new Syntax(cs,*p->arg(2));
    return this;
  }
  const Syntax * expand(const Syntax * p, Environ &) const {
    //printf("EXPANDING MAP %s\n", ~name);
    using namespace macro_abi;
    using macro_abi::Syntax;
    Match * m = match_args(NULL, parms, p);
    if (free)
      m = match(m, free, replace_context(free, get_context(p)));
    Syntax * res = replace(repl, m, new Mark(env));
    //printf("EXPANDING MAP %s RES: %s\n", ~name, ~res->to_string());
    //printf("  %s\n", ~res->sample_w_loc());
    //res->str().source->dump_info(COUT, "    ");
    //for (unsigned i = 0; i != res->num_args(); ++i) {
      //printf("==%s\n", ~res->arg(i)->to_string());
      //printf("  %s\n", ~res->arg(i)->sample_w_loc());
      //res->arg(i)->str().source->dump_info(COUT, "    ");
    //}
    //printf("  macro_call = %s\n", ~macro_call->sample_w_loc());
    //macro_call->str().source->dump_info(COUT, "    ");
    return res;
  }
  void compile(CompileWriter & f, Phase phase) const {
    f << indent << "(macro " << key << " ";
    parms->to_string(f, PrintFlags(f.indent_level), f.syntax_gather);
    f << "\n";
    f << indent << "  ";
    repl->to_string(f, PrintFlags(f.indent_level + 2), f.syntax_gather);
    f << ")\n";
  }
};

struct ProcMacro : public Macro {
  const char * what() const {return "proc-macro";}
  const Fun * fun;
  typedef const Syntax * (*MacroCall)(const Syntax *, Environ * env);
  ProcMacro * parse_self(const Syntax * p, Environ & e) {
    //printf("PARSING MACRO %s\n", ~p->arg(0)->name);
    //p->print();
    //printf("\n");
    syn = p;
    assert_num_args(1, 2);
    real_name = expand_binding(p->arg(0), e);
    fun = e.symbols.lookup<Fun>(p->num_args() == 1 ? p->arg(0) : p->arg(1));
    fun->is_macro = true;
    def = fun->syn;
    return this;
  }
  const Syntax * expand(const Syntax * p, Environ & env) const {
    if (!fun->ct_ptr) {
      Deps deps;
      deps.push_back(fun);
      compile_for_ct(deps, env);
      assert(fun->ct_ptr);
    }
    const Syntax * res = ((MacroCall)fun->ct_ptr)(p, &env);
    //printf("MACRO EXP RES\n");
    //res->print();
    //printf("\n");
    return res;
  }
  void compile(CompileWriter & f, Phase phase) const {
    f << indent << "(make_macro " << key << " " << fun->uniq_name() << ")\n";
  }
};

//
//
//

struct SimpleSyntaxEnum : public SyntaxEnum {
  Parts::const_iterator i;
  Parts::const_iterator end;
  SimpleSyntaxEnum(Parts::const_iterator i0, Parts::const_iterator e0)
    : i(i0), end(e0) {}
  const Syntax * next() {
    if (i == end) return NULL;
    const Syntax * res = *i;
    ++i;
    return res;
  }
};

extern "C" namespace macro_abi {

  Mark * new_mark_f(SymbolNode * e) {
    return new Mark(e);
  }

  int syntax_simple(Syntax * s) {
    return s->simple();
  }
  
  const Syntax * syntax_part(const Syntax * s, unsigned i) {
    return s->part(i);
  }

  unsigned syntax_num_args(Syntax * s) {
    return s->num_args();
  }
  
  const Syntax * syntax_flag(const Syntax * s, UnmarkedSyntax * n) {
    return s->flag(*n);
  }

  int syntax_eq(Syntax * lhs, UnmarkedSyntax * rhs) {
    if (lhs->simple() && rhs->simple()) {
      if (lhs->what_ == rhs->what_) return true;
      SymbolName n = lhs->what_;
      while (n.marks) {
        n.marks = n.marks->prev;
        if (n == rhs->what_) return true;
      }
      return false;
    }
    if (lhs->simple() || rhs->simple()) return false;
    assert(rhs->num_parts() == 2);
    if (lhs->num_parts() != 2) return false;
    return lhs->part(0)->what() == rhs->part(0)->what()
      && lhs->part(1)->what() == rhs->part(1)->what();
  }

  SyntaxEnum * syntax_args(Syntax * p) {
    return new SimpleSyntaxEnum(p->args_begin(), p->args_end());
  }
  
  SyntaxList * new_syntax_list() {
    return new ::Syntax(new Syntax("@"));
  }
  
  int syntax_list_empty(const SyntaxList * p) {
    return p->num_args() == 0;
  }
  
  size_t syntax_list_append(SyntaxList * l, const Syntax * p) {
    size_t pos = l->num_args();
    l->add_part(p);
    return pos;
  }

  void syntax_list_append_flag(SyntaxList * l, const Syntax * p) {
    l->add_flag(p);
  }

  void syntax_list_append_all(SyntaxList * l, SyntaxEnum * els) {
    while (const Syntax * el = els->next()) {
      l->add_part(el);
    }
  }

  void syntax_list_reverse(SyntaxList * l) {
    std::reverse(l->args_begin(), l->args_end());
  }

  void syntax_list_replace(SyntaxList * l, size_t pos, const Syntax * p) {
    l->arg(pos) = p;
  }

  const Syntax * syntax_enum_next(SyntaxEnum * e) {
    return e->next();
  }

}

//
//
//

void add_match_var(Match * m, const SymbolName & n, const Syntax * repl) {
  if (n == "_") return; // "_" is a special variable meaning: don't care
  m->push_back(Match::value_type(n, repl));
}

bool match_list(Match * m, 
                const Syntax * p, Parts::const_iterator p_i, Parts::const_iterator p_end, 
                const Syntax * r, Parts::const_iterator r_i, Parts::const_iterator r_end);

// if match_prep sets p == 0 then there is nothing more to do
bool match_prep(Match * m, const Syntax * & p, const Syntax * & repl) {
  //printf("match_parm <<: %s %s\n", ~p->to_string(), repl ? ~repl->to_string() : "<null>");
  if (p->num_args() > 0) {
    if (p->is_a("pattern")) {
      if (!repl) return true;
      p = p->arg(0);
      assert(p->what().name == repl->what().name);
      bool res = match_list(m, p, p->args_begin(), p->args_end(),
                            repl, repl->args_begin(), repl->args_end());
      p = 0;
      return res;
    } else if (p->is_a("reparse")) {
      if (!repl) {
        if (p->num_args() > 1)
          repl = p->arg(1);
      }
      p = p->arg(0);
    } else {
      //printf(stderr, "??%s\n", ~p->to_string());
      abort();
    }
  }
  return true;
}


bool match_parm(Match * m, const Syntax * p, const Syntax * repl) {
  //printf("match_parm <<: %s :: %s\n", ~p->to_string(), repl ? ~repl->to_string() : "<null>");
  bool cont = match_prep(m, p, repl);
  if (!cont) return true;
  if (repl) {
    add_match_var(m, *p, repl);
    return true;
  } else {
    return false;
  }
}

bool match_list(Match * m, 
                const Syntax * pattern, Parts::const_iterator p_i, Parts::const_iterator p_end, 
                const Syntax * with,    Parts::const_iterator r_i, Parts::const_iterator r_end)
{
  bool now_optional = false;
  while (p_i != p_end) {
    const Syntax * p = *p_i;
    const Syntax * r = r_i < r_end ? *r_i : NULL;
    bool ok = match_prep(m, p, r);
    if (!ok) return false;
    if (p) {
      SymbolName v = *p;
      if (v == "@") {
        now_optional = true;
        ++p_i; 
        continue;
      }
      if (v.name[0] == '@') {
        v.name = v.name.c_str() + 1;
        Syntax * n = new Syntax(new Syntax("@"), r_i, r_end);
        if (with->d.have_d()) {
          const Flags & flags = with->d->flags;
          for (Flags::const_iterator i = flags.begin(), e = flags.end(); i != e; ++i) {
            if (!pattern->flag((*i)->what()))
              n->add_flag(*i);
          }
        }
        r = n;
      }
      if (r) {
        add_match_var(m, v, r);
      } else {
        if (!now_optional) return false;
      }
    }
    ++p_i;
    ++r_i;
  }
  const Flags & flags = pattern->d->flags;
  for (Flags::const_iterator i = flags.begin(), e = flags.end(); i != e; ++i) {
    const Syntax * w = with->flag((*i)->what());
    match_parm(m, (*i)->arg(0), w ? w->arg(0) : NULL);
  }
  return true;
}

Match * match(Match * orig_m, const Syntax * pattern, const Syntax * with, unsigned shift) {
  Match * m = new Match();
  if (pattern->is_a("()")) {
    //printf("YES!\n");
    pattern = reparse("MATCH_LIST", pattern->arg(0));
  } else {
    //printf("NO! >>%s<<\n", ~pattern->what());
  }
  //printf("MATCH\n");
  //printf("%s %s\n", ~pattern->sample_w_loc(), ~pattern->to_string());
  //printf("%s %s\n", ~with->sample_w_loc(), ~with->to_string());
  //printf("---\n");
  if (pattern->simple()) {
    add_match_var(m, pattern->what(), with);
  } else {
    with = with->ensure_branch();
    bool ok = match_list(m, pattern, pattern->parts_begin(), pattern->parts_end(),
                         with, with->parts_begin() + shift, with->parts_end());
    if (!ok) return NULL;
  }
  //printf("MATCH RES:: ");
  //for (Match::const_iterator i = m->begin(), e = m->end(); i != e; ++i) {
    //printf("%s", ~i->first.to_string());
    //printf("%s=>", ~i->first.to_string());
    //i->second->to_string(COUT);
    //printf(",");
  //}
  //printf("\n");
  if (orig_m) 
    m->insert(m->end(), orig_m->begin(), orig_m->end());
  return m;
}

extern "C" namespace macro_abi {

  Match * match(Match * m, const Syntax * pattern, const Syntax * with) {
    return match(m, pattern, with, 0);
  }
  
  Match * match_args(Match * m, const Syntax * pattern, const Syntax * with) {
    return match(m, pattern, with, 1);
  }
  
  const Syntax * match_var(Match * m, UnmarkedSyntax * n) {
    Match::const_iterator i = m->begin(), e = m->end();
    for (; i != e; ++i)
    if (i->first == *n) return i->second;
    return NULL;
  }
  
  SyntaxEnum * match_varl(Match * m, UnmarkedSyntax * n) {
    const Syntax * p = match_var(m, n);
    if (p) {
      assert(p->is_a("@"));
      return new SimpleSyntaxEnum(p->args_begin(), p->args_end());
    } else {
      return NULL;
    }
  }

  Match * match_local(Match * orig_m, ...) {
    Match * m = new Match;
    va_list parms;
    const Syntax * p;
    char buf[8];
    unsigned i = 1;
    va_start(parms, orig_m);
    while ((p = va_arg(parms, const Syntax *))) {
      snprintf(buf, 8, "$%d", i);
      ++i;
      add_match_var(m, buf, p);
    }
    va_end(parms);
    if (orig_m) 
      m->insert(m->end(), orig_m->begin(), orig_m->end());
    return m;
  }
}

const Syntax * replace(const Syntax * p, Match * match, Mark * mark) {
  ReplTable * rparms 
    = new ReplTable(mark, 
                    new ExpandSourceInfo(Macro::macro_call, Macro::macro_def));
  //new ExpandSourceInfo);
  if (match)
    rparms->table = *match;
  //printf("tREPLACE: %s\n", ~p->to_string());
  //rparms->macro_call = Macro::macro_call;
  //rparms->macro_def = Macro::macro_def;
  const Syntax * res;
  if (p->is_a("{}")) {
    res = reparse("STMTS", p->arg(0), rparms);
    if (res->num_args() == 1)
      res = res->arg(0);
  } else {
    res = replace(p, rparms, NULL, true, NULL);
  }
  rparms->expand_si->outer_ip = res;
  // fixme: why am i doing this? commented it out for now
  //res->str_ = p->str();
  //printf("tREPLACE res: %s\n", ~res->to_string());
  return res;
}

const Syntax * macro_abi::replace(const UnmarkedSyntax * p, Match * match, Mark * mark) {
  return ::replace(p, match, mark);
}

const Syntax * reparse(String what, const Syntax * p, ReplTable * r, const Replacements * additional_repls) {
  //printf("REPARSE %s %s AS %s\n", ~p->sample_w_loc(), ~p->to_string(), ~what);
  const Replacements * repls = combine_repl(p->repl, r);
  Replacements combined_repls;
  if (repls) {
    //printf("repls = %s\n", ~repls->to_string());
    combined_repls = *repls;
  }
  if (additional_repls) {
    //printf("combined_repls = %s\n", ~additional_repls->to_string());
    combined_repls.insert(combined_repls.end(), additional_repls->begin(), additional_repls->end());
  }
  const Syntax * res;
  try {
    res = parse_str(what, p->str(), &combined_repls);
  } catch (Error * err) {
    //Annon * annon = new ReparseAnnon(p, what);
    //annon->prev = p->str().annon; // FIXME: Is this right
    //err->annon = annon;
    //puts(~err->message());
    throw err;
  }
  //res = new Syntax(new ReparseAnnon(p, what), res);
  //printf("PARSED STRING %s\n", ~res->to_string());
  if (repls) {
    //printf("# repls = %d\n", repls->size());
    for (Replacements::const_iterator i = repls->begin(), e = repls->end(); 
         i != e; ++i) 
    {
      combined_repls.erase(combined_repls.begin());
      //printf("?REPLACE %i\n", i - repls->begin());
      //res->print();
      //printf("\n");
      res = replace(res, *i, &combined_repls, false, NULL);
      //printf("?replace %i\n", i - repls->begin());
      //res->print();
      //printf("\n");
    }
  }
  //printf("REPARSE %s RES: %s\n", ~p->to_string(), ~res->to_string());
  return res;
}

const Syntax * macro_abi::reparse(const Syntax * s, const char * what, Environ * env) {
  return reparse(what, s);
}

static const Syntax * replace_mid(const Syntax * mid, const Syntax * repl, ReplTable * r, const Replacements * rs, bool splice=false);

static const Syntax * try_mid(const Syntax * mid_p, const Syntax * p, ReplTable * r, const Replacements * rs, bool * splice_r) {
  SymbolName mid = *mid_p;
  bool splice = false;
  if (mid.name[0] == '@' && mid.name.size() > 1) {
    mid.name = mid.name.c_str() + 1;
    assert(splice_r);
    splice = true;
  } 
  const Syntax * p0 = r->lookup(mid);
  if (!p0) return NULL;
  if (splice_r) *splice_r = splice;
  //printf("MID ");
  //p->str().sample_w_loc(COUT);
  //printf(" ");
  //p->arg(0)->str().sample_w_loc(COUT);
  //printf("\n");
  const Syntax * res = replace_mid(p, p0, r, rs, splice);
    //printf("REPLACE res %d: %s %s\n", seql, ~res->sample_w_loc(), ~res->to_string());
  return res;
}

static const Syntax * replace(const Syntax * p, 
                              ReplTable * r, const Replacements * rs, 
                              bool allow_plain_mids, bool * splice_r) 
{
  allow_plain_mids = true;
  // FIXME: Do I need to handle the case where the entity is a symbol name?
  static unsigned seq=0;
  unsigned seql = seq++;
  //printf("REPLACE %d: %s\n", seql, ~p->to_string());
  //r->to_string(COUT, PrintFlags());
  //printf("\n");
  SymbolName mid;
  if (p->simple()) {
    //mid = *p;
    //goto try_mid;
    //return p;
    //printf("MARK %s\n", ~p->what());
    const Syntax * res = NULL;
    if (allow_plain_mids) res = try_mid(p, new Syntax(new Syntax("mid"), p), r, rs, splice_r);
    if (!res) res = new Syntax(p, r->mark, r->expand_source_info(p));
    return res;
  } else if (p->is_a("mid")/* && r->have(*p->arg(0))*/) {
    const Syntax * res = try_mid(p->arg(0), p, r, rs, splice_r);
    if (!res) goto def;
#if 0
    SymbolName mid = *p->arg(0);
    bool splice = false;
    if (mid.name[0] == '@') {
      mid.name = mid.name.c_str() + 1;
      assert(splice_r);
      splice = true;
    } 
    const Syntax * p0 = r->lookup(mid);
    if (!p0) goto def;
    if (splice_r) *splice_r = splice;
    //printf("MID ");
    //p->str().sample_w_loc(COUT);
    //printf(" ");
    //p->arg(0)->str().sample_w_loc(COUT);
    //printf("\n");
    const Syntax * res = replace_mid(p, p0, r, rs, splice);
    //printf("REPLACE res %d: %s %s\n", seql, ~res->sample_w_loc(), ~res->to_string());
#endif
    return res;
  } else if (p->is_a("s") || p->is_a("c") || p->is_a("n") || p->is_a("f")) {
    ChangeSrc<ExpandSourceInfo> ci(r);
    return new Syntax(ci, *p);
  } else if (p->is_a("{}") || p->is_a("()") || p->is_a("[]") || p->is_a("<>") || p->is_a("parm")) {
    // raw tokens
    assert(p->num_args() == 1);
    assert(p->arg(0)->simple());
    assert(p->repl == p->arg(0)->repl);
    Syntax * res = new Syntax(r->expand_source_info_str(p->str()), 
                              new Syntax(p->part(0), r->mark, r->expand_source_info(p->part(0))));
    res->repl = combine_repl(p->repl, r);
    Syntax * r0 = new Syntax(String(p->arg(0)->str()), r->expand_source_info_str(p->arg(0)->str()));
    r0->repl = res->repl;
    res->add_part(r0);
    //printf("REPLACE RES %d: %s %s\n", seql, ~res->sample_w_loc(), ~res->to_string());
    return res;
  } else {
  def:
    Syntax * res = new Syntax(r->expand_source_info_str(p));
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      //const Syntax * q = (i == 0 && p->part(0)->simple()) ? p->part(0) : replace(p->part(i), r); // HACK
      bool splice = false;
      //static int x = 0;
      //++x;
      //printf("<<%d %s\n", x, ~p->part(i)->to_string());
      const Syntax * q = replace(p->part(i), r, rs, allow_plain_mids, &splice);
      if (splice) {
        //printf("??%d %s\n", x, ~q->to_string());
        assert(q->is_a("@")); // FIXME: Error message
        res->add_parts(q->args_begin(), q->args_end());
        res->add_flags(q);
      } else {
        res->add_part(q);
      }
    }
    for (Flags::const_iterator i = p->flags_begin(), e = p->flags_end(); i != e; ++i) {
      //printf("~~%s\n", ~(*i)->to_string());
      const Syntax * q = *i;
      Syntax * r0 = new Syntax(q->part(0)); // FIXME: I think I need to do more with first part
      if (q->num_args() > 0) // FIXME: Can there me more than one arg?
        r0->add_part(replace(q->arg(0), r, rs, allow_plain_mids, NULL));
      res->add_flag(r0);
    }
    //printf("REPLACE Res %d: %s\n", seql, ~res->to_string());
    return res;
  }
}

const Syntax * replace_context(const Syntax * p, const Marks * context);

const Syntax * replace_mid(const Syntax * mid, const Syntax * repl, ReplTable * r, const Replacements * rs, bool splice) {
  if (repl->is_a("@")) {
    Syntax * res = new Syntax(repl->str(), repl->part(0)); 
    for (Parts::const_iterator i = repl->args_begin(), e = repl->args_end(); i != e; ++i) {
      res->add_part(replace_mid(mid, *i, r, rs));
    }
    for (Flags::const_iterator i = repl->flags_begin(), e = repl->flags_end(); i != e; ++i) {
      const Syntax * q = *i;
      Syntax * r0 = new Syntax(q->part(0)); // FIXME: I think I need to do more with first part
      if (q->num_args() > 0) // FIXME: Can there me more than one arg?
        r0->add_part(replace_mid(mid, (*i)->part(1), r, rs));
      res->add_flag(r0);
    }
    return res;
  } else {
    ChangeSrc<SplitSourceInfo> cs(repl, r->outer_si ? r->outer_si : new ReplaceSourceInfo(r->expand_si,mid));
    const Syntax * orig_repl = repl;
    repl = new Syntax(cs, *repl);

    if (mid->num_args() > 1) {
      String what = mid->arg(1)->as_symbol_name().name;
      if (repl->is_a("parm") || repl->is_a("()")) {
        if (repl->arg(0)->repl) {
          for (Replacements::const_iterator i = repl->arg(0)->repl->begin(), e = repl->arg(0)->repl->end(); 
               i != e; ++i) 
          {
            // FIXME: This seams like the correct thing to do,
            //        however, I have no idea if it is.....
            (*i)->outer_si = cs.new_;
          }
        }
        repl = reparse(what, repl->arg(0), NULL, rs);
      }
    }
    return repl;
  }
}

const Syntax * replace_context(const Syntax * p, const Marks * context) {
  if (p->simple()) {
    //printf("REPLACE CONTEXT: %s\n", ~p->to_string());
    Syntax * res = new Syntax(p, context);
    //printf("REPLACE CONTEXT RES: %s\n", ~res->to_string());
    return res;
  } else if (p->is_a("s") || p->is_a("c") || p->is_a("n") || p->is_a("f")) {
    return p;
  } else if (p->is_a("{}") || p->is_a("()") || p->is_a("[]") || p->is_a("parm")) {
    // raw tokens
    fprintf(stderr, "Unhandled Case\n");
    abort();
  } else {
    Syntax * res = new Syntax(p->str());
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      res->add_part(replace_context(p->part(i), context));
    }
    return res;
  }
}

extern "C" namespace macro_abi {

  Context * empty_context() {
    return NULL;
  }

  Context * get_context(Syntax * p) {
    return p->what().marks;
  }

  Syntax * replace_context(Syntax * p, Context * context) {
    return ::replace_context(p, context);
  }
}

//
//
//

extern "C" namespace macro_abi {
  
  const UnmarkedSyntax * string_to_syntax(const char * str) {
    return parse_str("SYNTAX_STR", SourceStr(str));
  }
  
  const char * syntax_to_string(const UnmarkedSyntax * s) {
    if (s->simple()) {
      return s->what().name;
    } else {
      return ~s->to_string();
    }
  }
  
  void dump_syntax(const UnmarkedSyntax * s) {
    s->print();
    printf("\n");
  }
}

// three type of macros, two namespaces
// syntax macros in there own name space
//   (m ...) or (m)
// function macros
//   (call m (list ...))
// identifier macros
//   m
// function and identifier macros in the same namespace
// NOTE: may also want macros for tags...

//static SymbolTable syntax_maps;

/*
struct BuildIn {
  const char * name;
  unsigned pos;
  unsigned min;
  unsigned max;
  Position parms[4];
}
*/

void read_macro(const Syntax * p, Environ & env) {
  parse_top_level(p, env);
}

const Syntax * ID = new Syntax("id");
const Syntax * ESTMT = new Syntax("estmt");
const Syntax * NUM = new Syntax("n");

// should't override following primatives
//   "exp" "stmt" "estmt" and TOKENS
// FIXME: need to add check

static const Syntax * handle_paran(Parts::const_iterator & i, 
                                   Parts::const_iterator e, 
                                   Environ & env) 
{
  const Syntax * p = *i;
  if (!p->is_a("()")) return NULL;
  ++i;
  //printf("handle_paran: %s\n", ~p->to_string());
  try {
    const Syntax * exp = reparse("PARAN_EXP", p);
    //printf("handle_paran:: %s\n", ~exp->to_string());
    const Syntax * type = parse_decl_->parse_type(exp, env);
    if (type) return new Syntax(p->str(), new Syntax(".type"), type);
    // Since the raw string might need to be reparsed we can't use an
    // exp here.  Unfortunately this will likely mean duplicate work.
    // Avoiding that will take more thought
    else return p;
  } catch (...) {
    return p;
  }
}

static const Syntax * handle_new(Parts::const_iterator & i, 
                                 Parts::const_iterator e, 
                                 Environ & env)
{
  const Syntax * p = *i;
  if (!p->is_a("new")) return NULL;
  ++i;
  if (!p->simple()) return p; // already handled
  assert(i != e); // FIXME: error message
  const Syntax * type = parse_decl_->parse_type(i, e, env);
  assert(type); // FIXME: error message
  //printf(">>%s<<\n", ~type->to_string());
  return new Syntax(p, type);
}

const Syntax * handle_operator_fun_id(Parts::const_iterator & i, 
                                      Parts::const_iterator e,
                                      Environ & env) 
{
  const Syntax * p = *i;
  if (!p->is_a("operator")) return NULL;
  ++i;
  if (!p->simple()) return p; // already handled
  assert(i != e); // FIXME: error message
  const Syntax * type = parse_decl_->parse_type(i, e, env);
  if (type) {
    return new Syntax(p, type);
  } else {
    return new Syntax(p, *i++);
  }
}

const Syntax * e_parse_exp(const Syntax * p0, Environ & env, const char * list_is) {
  //printf("e_parse_exp: %s\n", ~p0->to_string());
  Syntax * tmp = new Syntax(p0->str(), p0->part(0));
  Parts::const_iterator i = p0->args_begin(), e = p0->args_end();
  while (i != e) {
    const Syntax * p = NULL;
    if (!p) p = handle_paran(i, e, env);
    if (!p) p = handle_new(i, e, env);
    if (!p) p = handle_operator_fun_id(i, e, env);
    if (!p) p = *i++;
    tmp->add_part(p);
  }
  const Syntax * res = parse_exp_->parse(tmp, list_is);
  // FIXME: if really an expression than "list" needs to become the
  //        comma operator
  return res;
}

const Syntax * partly_expand(const Syntax * p, Position pos, Environ & env, unsigned flags) {
  if (p->have_entity()) return p;
  //printf("\n>expand>%s//\n", ~p->part(0)->to_string());
  SymbolName what = p->what();
  //printf("\n>expand>%s//\n", ~what);
  //p->str().sample_w_loc(COUT);
  //COUT << "\n";
  //p->print();
  //printf("\n////\n");
  if (p->simple() && asc_isdigit(what.name[0])) {
    return new Syntax(p->str(), NUM, p);
  } else if (is_raw_id(p)) {
    return partly_expand(new Syntax(p->str(), ID, p), pos, env, flags);
  } else if (what == "{}") {
    if (pos == ExpPos)
      return partly_expand(reparse("INIT", p->arg(0)), pos, env, flags);
    else
      return partly_expand(reparse("BLOCK", p), pos, env, flags);
  } else if (what == "()") {
    return partly_expand(reparse("PARAN_EXP", p), pos, env, flags);
  } else if (what == "[]") {
    return partly_expand(reparse("EXP", p->arg(0)), pos, env, flags);
  } else if (what == "parm") {
    //abort();
    if (pos & ExpPos)
      return partly_expand(reparse("EXP", p), pos, env, flags);
    else
      return partly_expand(reparse("STMT", p), pos, env, flags);
  } else if (env.symbols.exists(SymbolKey(what, SYNTAX_NS))) { // syntax macros
    p = env.symbols.lookup<Macro>(SymbolKey(what, SYNTAX_NS), p->str())->expand(p, p, env);
    return partly_expand(p, pos, env, flags);
  } else if (what == "id" && !(flags & EXPAND_NO_ID_MACRO_CALL)) { 
    // NOTE: ID macros can have flags, since there is no parameter list
    //       they are passed in as flags as (id <id> :(flag1 val1))
    assert_num_args(p, 1);
    const Syntax * n = p;
    const Macro * m = env.symbols.find<Macro>(n->arg(0));
    if (m) { // id macro
      Syntax * a = new Syntax(new Syntax("."));
      a->set_flags(p);
      p = m->expand(p, a, env);
      return partly_expand(p, pos, env, flags);
    }
  } else if (what == "call") { 
    assert_num_args(p, 2);
    const Syntax * n = partly_expand(p->arg(0), OtherPos, env, flags | EXPAND_NO_ID_MACRO_CALL);
    const Syntax * a = p->arg(1);
    if (a->is_a("()")) a = reparse("SPLIT", p->arg(1)->arg(0));
    if (n && n->is_a("id")) {
      if (!(flags & EXPAND_NO_FUN_MACRO_CALL)) {
        const Macro * m = env.symbols.find<Macro>(n->arg(0));
        if (m) { // function macros
          //  (call (id fun) (list parm1 parm2 ...))?
          p = m->expand(p, a, env);
          return partly_expand(p, pos, env, flags);
        } else if (*n->arg(0) == "environ_snapshot") {
          if (a->num_args() > 0)
            throw error(a, "environ_snapshot does not take any paramaters");
          return new Syntax(n->arg(0));
        }
      }
      if (TypeSymbol * t = env.symbols.find<TypeSymbol>(n->arg(0))) {
        Syntax * res = new Syntax("anon");
        res->add_part(new Syntax(t));
        res->add_part(a);
        return res;
      }
    }
    Syntax * res = new Syntax(p->str(), p->part(0));
    res->add_part(p->arg(0));
    res->add_part(a);
    p = res;
  } else if (what == "stmt") {
    assert_pos(p, pos, TopLevel|FieldPos|StmtPos|StmtDeclPos);
    //printf("TRYING STMT PARSE on %s\n", ~p->sample_w_loc()); 
    const Syntax * res = parse_decl_->parse_decl(p, env);
    //printf("STMT PARSE %p on %s\n", res, ~p->sample_w_loc()); 
    if (!res)
      res = e_parse_exp(p, env, "seq");
    //printf("expand stmt res0 %s\n", res ? ~res->to_string() : "<?>");
    return partly_expand(res, pos, env, flags);
  } else if (what == "exp" || what == "init") {
    //printf("PARSE EXP %s\n", ~p->to_string());
    assert_pos(p, pos, ExpPos);
    p = e_parse_exp(p, env, what == "exp" ? "seq" : ".");
    return partly_expand(p, pos, env, flags);
  }
  // we should have a primitive
  return p;
}

const Syntax * macro_abi::partly_expand(const Syntax * p, Position pos, Environ * env) {
  return partly_expand(p, pos, *env);
}

struct PartlyExpandSyntaxEnum : public SyntaxEnum {
  typedef Vector<SyntaxEnum *> VD;
  VD stack;
  SyntaxEnum * top;
  Position pos;
  Environ * env;
  PartlyExpandSyntaxEnum(SyntaxEnum * e0, Position p0, Environ * env0)
    : top(e0), pos(p0), env(env0) {}
  const Syntax * next() {
  try_again:
    const Syntax * res = top->next();
    if (!res) {
      if (stack.empty()) {
        return NULL;
      } else {
        top = stack.back();
        stack.pop_back();
        goto try_again;
      }
    }
    res = partly_expand(res, pos, *env);
    if (res->is_a("@")) {
      stack.push_back(top);
      top = new SimpleSyntaxEnum(res->args_begin(), res->args_end());
      goto try_again;
    }
    return res;
  }
};

SyntaxEnum * partly_expand_list(const Syntax * p, Position pos, Environ & env) {
  return new PartlyExpandSyntaxEnum(new SimpleSyntaxEnum(p->args_begin(), p->args_end()), pos, &env);
}

extern "C" namespace macro_abi {
  
  SyntaxEnum * partly_expand_list(SyntaxEnum * l, Position pos, Environ * env) {
    return new PartlyExpandSyntaxEnum(l, pos, env);
  }
  
  const Syntax * pre_parse(const Syntax * p, Environ * env) {
    p = partly_expand(p, TopLevel, env);
    if (p->is_a("@")) {
      ::Syntax * res = new ::Syntax(p->str(), p->part(0));
      for (Parts::const_iterator i = p->args_begin(), e = p->args_end(); i != e; ++i)
        res->add_part(pre_parse(*i, env));
      return res;
    } else {
      return pre_parse_decl(p, *env);
    }
  }

}

SymbolKey expand_binding(const Syntax * p, const InnerNS * ns, Environ & env) {
  //printf("EB::"); p->print(); printf("\n");
  if (p->simple()) {
    return SymbolKey(*p, ns);
  } else if (p->is_a("fluid")) {
    assert_num_args(p, 1);
    const FluidBinding * b = env.symbols.lookup<FluidBinding>(p->arg(0), ns);
    return SymbolKey(b->rebind, ns);
  } else if (p->is_a("`")) {
    assert_num_args(p, 2);
    const InnerNS * ns = env.symbols.lookup<InnerNS>(p->arg(1), INNER_NS);
    return expand_binding(p->arg(0), ns, env);
  } else if (p->is_a("::")) {
    throw error(p, "Can not use outer namespaces in binding form");
  } else if (const SymbolKeyEntity * s = p->entity<SymbolKeyEntity>()) {
    return s->name;
  } else {
    fprintf(stderr, "Unsupported Binding Form: %s\n", ~p->to_string());
    abort();
  }
}

const Syntax * handle_w_tilda(Parts::const_iterator & i, 
                              Parts::const_iterator e,
                              Environ & env) 
{
  const Syntax * p = *i;
  if (!p->is_a("~")) return NULL;
  ++i;
  if (!p->simple()) return p; // already handled
  assert(i != e); // FIXME: error message
  return new Syntax(p, *i++);
}

Stmt * parse_map(const Syntax * p, Environ & env) {
  //printf("MAP>>%s\n", ~p->to_string());
  SimpleMacro * m = new SimpleMacro;
  m->parse_self(p, env);
  env.add(m->real_name, m);
  return empty_stmt();
}

Stmt * parse_macro(const Syntax * p, Environ & env) {
  ProcMacro * m = new ProcMacro;
  m->parse_self(p, env);
  env.add(m->real_name, m);
  if (p->flag("w_snapshot"))
    m->fun->env_ss = *env.top_level_environ;
  return empty_stmt();
}

void assert_pos(const Syntax * p, Position have, unsigned need) {
  //if (!(have & need)) {
  //  p->print(); printf("\n");
  //  abort();
  //}
  // FIXME Better Error Message
  //if (!(have & need)) 
  //  throw error(p, "syntax error");
}

void assert_num_args(const Syntax * p, unsigned num) {
  if (p->num_args() != num)
    throw error(p, "Expected %d arguments but got %d for \"%s\"", num, p->num_args(), ~p->what());
}

void assert_num_args(const Syntax * p, unsigned min, unsigned max) {
  if (p->num_args() < min)
    throw error(p, "Expected at least %d arguments but got %d for \"%s\"", min, p->num_args(), ~p->what());
  if (p->num_args() > max)
    throw error(p->arg(max), "Too many arguments for \"%s\"", ~p->what());
}

void expand_fun_parms(const Syntax * args, Environ & env, Syntax * res) {
  for (unsigned i = 0; i != args->num_args(); ++i) {
    const Syntax * p = args->arg(i);
    if (p->is_a("@")) {
      expand_fun_parms(p, env, res);
    } else {
      if (p->is_a("parm") || p->is_a("()"))
        p = reparse("TOKENS", p->arg(0));
      //printf("FUN_PARM: %s\n", ~p->to_string());
      if (!p->is_a("...")) {
        Syntax * r = new Syntax(p->part(0), parse_type(p->part(0), env));
        if (p->num_parts() == 2) 
          r->add_part(p->part(1));
        res->add_part(r);
      } else {
        res->add_part(p);
      }
    }
  }
}

Tuple * expand_fun_parms(const Syntax * parse, Environ & env) {
  //printf("FUN_PARMS: %s\n", ~parse->to_string());
  Syntax * res = new Syntax(parse->part(0));
  expand_fun_parms(parse, env, res);
  Type * type = parse_type(res, env);
  Tuple * tuple = dynamic_cast<Tuple *>(type);
  assert(tuple); // FIXME: Error Message?
  return tuple;
}



void compile_for_ct(Deps & deps, Environ & env) {

  static unsigned cntr = 0;
  
  for (unsigned i = 0, sz = deps.size(); i != sz; ++i) {
    deps.merge(deps[i]->deps());
  }

  //printf("COMPILE FOR CT DEPS: %d\n", deps.size());
  //for (Deps::iterator i = deps.begin(), e = deps.end(); i != e; ++i)
  //  printf("  %s\n", ~(*i)->name);
  //printf("---\n");
  
  printf("COMPILE FOR CT: zlfct%03d\n", cntr);
  StringBuf buf;
  buf.printf("./zlfct%03d.zls", cntr);
  String source = buf.freeze();
  buf.printf("./zlfct%03d.so", cntr);
  String lib = buf.freeze();
  buf.printf("zls -g -fexceptions -shared -fpic -o zlfct%03d.so zlfct%03d.zls", cntr, cntr);
  String cmd = buf.freeze();
  cntr++;
  
  CompileWriter cw;
  cw.open(source, "w");
  cw.deps = &deps;
  compile(env.top_level_symbols, cw);
  cw.close();
  
  system(cmd);
 
  void * lh = dlopen(lib, RTLD_NOW | RTLD_GLOBAL);
  if (!lh) {
    fprintf(stderr, "ERROR: %s\n", dlerror());
    abort();
  }
  
  for (Deps::const_iterator i = deps.begin(), e = deps.end(); i != e; ++i) {
    if (!(*i)->ct_ptr) { // FIXME: I need a better test
      //printf(">>%s\n", ~(*i)->uniq_name());
      void * p = dlsym(lh, (*i)->uniq_name());
      //assert(p);
      (*i)->ct_ptr = p;
      SymbolNode * env_ss = dynamic_cast<const Fun *>(*i)->env_ss;
      if (env_ss) {
        StringBuf buf;
        buf.printf("%s$env_ss", ~(*i)->uniq_name());
        String n = buf.freeze();
        //printf("LOOKUP %s\n", ~n);
        void * p = dlsym(lh, ~n);
        *static_cast<void * *>(p) = env_ss;
      }
    }
  }
}

struct Syntaxes {const char * str; const Syntax * syn;};

void load_macro_lib(ParmString lib, Environ & env) {
  void * lh = dlopen(lib, RTLD_NOW | RTLD_GLOBAL);
  if (!lh) {
    fprintf(stderr, "ERROR: %s\n", dlerror());
    abort();
  }
  unsigned macro_funs_size = *(unsigned *)dlsym(lh, "_macro_funs_size");
  if (macro_funs_size > 0) {
    const char * * i = (const char * *)dlsym(lh, "_macro_funs");
    const char * * e = i + macro_funs_size;
    for (; i != e; ++i) {
      const Fun * fun = dynamic_cast<const Fun *>(env.find_tls(*i));
      String uniq_name = fun->uniq_name();
      if (fun->is_macro) {
        fun->ct_ptr = dlsym(lh, ~uniq_name);
      }
      if (fun->env_ss) {
        void * * p = (void * *)dlsym(lh, ~sbprintf("%s$env_ss", ~uniq_name));
        *p = fun->env_ss;
      }
    }
  }
  unsigned syntaxes_size = *(unsigned *)dlsym(lh, "_syntaxes_size");
  if (syntaxes_size > 0) {
    Syntaxes * i = (Syntaxes *)dlsym(lh, "_syntaxes");
    Syntaxes * e = i + syntaxes_size;
    for (; i != e; ++i) {
      i->syn = parse_syntax_c(parse_str("SYNTAX", SourceStr(i->str, i->str+strlen(i->str))));
    }
  }
}


struct FluidBindingDecl : public Declaration, public FluidBinding {
  const char * what() const {return "fluid_binding";}
  FluidBindingDecl(SymbolName r) : FluidBinding(r) {}
  void finalize(FinalizeEnviron &) {};
  void compile_prep(CompileEnviron &) {};
  void compile(CompileWriter & f, Phase phase) const {
    assert(f.target_lang = CompileWriter::ZLE);
    if (phase == Forward || phase == Normal) {
      f << indent << "(" << "fluid_binding " << uniq_name() << ")\n";
    }
  }
};

Stmt * parse_fluid_binding(const Syntax * p, Environ & env) {
  assert_num_args(p, 1);
  SymbolKey n = expand_binding(p->arg(0), DEFAULT_NS, env);
  FluidBindingDecl * b = new FluidBindingDecl(mark(n, new Mark(NULL)));
  env.add(n, b);
  return empty_stmt();
}

//
// 
//

extern "C" namespace macro_abi {

  typedef const UserType UserTypeInfo;
  typedef const Module ModuleInfo;
  
  const UserTypeInfo * user_type_info(const Syntax * s, Environ * env) {
    return dynamic_cast<const UserType *>(env->types.inst(s));
  }
  
  const ModuleInfo * user_type_module(const UserTypeInfo * t) {
    return t->module;
  }
  
  const ModuleInfo * module_info(const Syntax *, Environ * env) {
    abort();
  }

  struct ModuleSymbolsEnum : public SyntaxEnum {
    SymbolNode * cur;
    Syntax * next() {
      if (!cur) return NULL;
      Syntax * res = new Syntax(new SymbolKeyEntity(cur->key));
      cur = cur->next;
      return res;
    }
    ModuleSymbolsEnum(SymbolNode * c) : cur(c) {}
  };
  
  SyntaxEnum * module_symbols(const ModuleInfo * m) {
    return new ModuleSymbolsEnum(m->syms);
  }
  
  bool module_have_symbol(const ModuleInfo * m, const Syntax * s) {
    return find_symbol<Symbol>(s, DEFAULT_NS, m->syms);
  }
}

//
//
//

template <typename T>
Syntax::Syntax(ChangeSrc<T> & f, const Syntax & other)
  : what_(other.what_), 
    str_(f(other.str_.source), other.str_), 
    d(other.d)
{
  if (other.repl) {
    repl = new Replacements(*other.repl);
    for (Replacements::const_iterator i = repl->begin(), e = repl->end(); 
         i != e; ++i) 
    {
      for (ReplTable::Table::iterator j = (*i)->table.begin(), e = (*i)->table.end();
           j != e; ++j)
      {
        //j->second = new Syntax(f, *j->second);
      }
    }
  } else {
    repl = NULL;
  }
}

template <typename T>
Parts::Parts(ChangeSrc<T> & f, const Parts & o) {
  reserve(o.size());
  for (const_iterator i = o.begin(), e = o.end(); i != e; ++i)
    push_back(new Syntax(f, **i));
}

template <typename T>
Flags::Flags(ChangeSrc<T> & f, const Flags & o) {
  data.reserve(o.size());
  for (const_iterator i = o.begin(), e = o.end(); i != e; ++i)
    data.push_back(new Syntax(f, **i));
}

//
//
//

extern "C" namespace macro_abi {

  Environ * temp_environ(Environ * env) {
    env = new Environ(env->new_scope());
    env->top_level_symbols = NULL;
    return env;
  }
  
  size_t ct_value(const Syntax * p, Environ * env) {
    Exp * ast = parse_exp(p, *env);
    return ast->ct_value<size_t>();
  }
  
  const Syntax * error(Syntax * p, const char * fmt, ...) {
    SourceStr str = p ? p->str() : SourceStr();
    va_list ap;
    va_start(ap, fmt);
    Error * res = verror(str.source, str.begin, fmt, ap);
    va_end(ap);
    return new Syntax(p, res);
  }

  const Syntax * get_symbol_prop(const Syntax * sym, const Syntax * prop, Environ * env) {
    return env->symbols.lookup<TopLevelSymbol>(sym)->get_prop(*prop);
  }

}
