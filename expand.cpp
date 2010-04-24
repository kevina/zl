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
#include "asc_ctype.hpp"

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

static const Syntax * const NO_MATCH = SYN(SYN("@"));

// misnamed, now replaces and marks and so much more
struct ReplTable {
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
  typedef MutableSyntax SyntaxList;
  typedef const ::Syntax Syntax;
  typedef Syntax UnmarkedSyntax;
  //struct SyntaxEnum;
  Match * match_f(Match * m, const UnmarkedSyntax * pattern, Syntax * with, Mark * mark);
  Match * match_args_f(Match * m, const UnmarkedSyntax * pattern, Syntax * with, Mark * mark);
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

static void flatten(const char * on, const Syntax * p, SyntaxBuilder & res) {
  if (!p->simple() && p->is_a(on)) {
    for (parts_iterator i = p->args_begin(), e = p->args_end(); i != e; ++i)
      flatten(on, *i, res);
    res.merge_flags(p);
  } else {
    res.add_part(p);
  }
}

const Syntax * flatten(const Syntax * p) {
  if (p->simple()) return p;
  SyntaxBuilder res;
  for (parts_iterator i = p->parts_begin(), e = p->parts_end(); i != e; ++i)
    flatten("@", *i, res);
  res.merge_flags(p);
  return res.build(p->str());
} 

static const Syntax * replace(const Syntax * p, ReplTable * r, const Replacements * rs, 
                              bool allow_plain_mids, bool * splice);
static const Syntax * reparse_replace(const Syntax * new_name, 
                                      const Syntax * p, ReplTable * r);

struct Macro : public Declaration, public Symbol {
  SymbolKey real_name;
  static const Syntax * macro_call;
  static const Syntax * macro_def;
  const Tuple * typed_parms;
  Macro() : typed_parms() {}
  const class Tuple * overloadable() const {return typed_parms;}
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
      //res = SYN(n, *res);
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
  Props props;
  void add_prop(SymbolName n, const Syntax * s) {props.add_prop(n, s);}
  const Syntax * get_prop(SymbolName n) const {return props.get_prop(n);}
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
    //printf("PARSING MAP %s\n%s\n", ~p->arg(0)->to_string(), ~p->to_string());
    env = e.symbols.front;
    //entity = p->str().source;
    def = syn = p;
    assert_num_args(3);
    real_name = expand_binding(p->arg(0), e);
    parms = flatten(p->arg(1));
    //printf("MAP PARMS %s: %s\n", ~p->arg(0)->what().name, ~parms->to_string());
    free = p->flag("free");
    if (free)
      free = free->arg(0);
    const Syntax * typed_parms_syn = p->flag("typed-parms");
    if (typed_parms_syn) {
      typed_parms = expand_fun_parms(typed_parms_syn->arg(0), e); 
    }
    ChangeSrc<SyntaxSourceInfo> cs(p->arg(2));
    repl = new_syntax(cs,*p->arg(2));
    return this;
  }
  const Syntax * expand(const Syntax * p, Environ &) const {
    //printf("EXPANDING MAP %s\n", ~name);
    using namespace macro_abi;
    using macro_abi::Syntax;
    Mark * mark = new Mark(env);
    Match * m = match_args_f(NULL, parms, p, mark);
    if (free)
      m = match_f(m, free, replace_context(free, get_context(p)), mark);
    Syntax * res = replace(repl, m, mark);
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
    fun = lookup_overloaded_symbol<Fun>
      (NULL, p->num_args() == 1 ? p->arg(0) : p->arg(1), &e);
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

const Syntax * expand_macro(const Syntax * p, const ast::Symbol * sym, 
                            const Vector<ast::Exp *> & parms, Environ & env) 
{
  const Macro * macro = dynamic_cast<const Macro *>(sym);
  if (!macro) return NULL;
  SyntaxBuilder synb;
  synb.add_part(SYN("."));
  for (Vector<ast::Exp *>::const_iterator i = parms.begin(), e = parms.end();
       i != e; ++i)
    synb.add_part(SYN(*i));
  synb.set_flags(p->arg(1)->flags_begin(), p->arg(1)->flags_end());
  return macro->expand(p, synb.build(), env);
}

//
//
//

struct SimpleSyntaxEnum : public SyntaxEnum {
  parts_iterator i;
  parts_iterator end;
  SimpleSyntaxEnum(parts_iterator i0, parts_iterator e0)
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

  // FIXME: should't this be in "syntax.hpp"?
  int syntax_eq(Syntax * lhs, UnmarkedSyntax * rhs) {
    if (lhs->simple() && rhs->simple()) {
      if (lhs->what() == rhs->what()) return true;
      SymbolName n = lhs->what();
      while (n.marks) {
        n.marks = n.marks->prev;
        if (n == rhs->what()) return true;
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
    MutableSyntax * syn = new_syntax();
    syn->add_part(SYN("@"));
    return syn;
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
  //printf("AM: x%p %p\n", NO_MATCH, repl);
  m->push_back(Match::value_type(n, repl));
}

bool match_list(Match * m, 
                const Syntax * p, parts_iterator p_i, parts_iterator p_end, 
                const Syntax * r, parts_iterator r_i, parts_iterator r_end,
                ReplTable * rt);

// if match_prep sets p == 0 then there is nothing more to do
bool match_prep(Match * m, const Syntax * & p, const Syntax * & repl, ReplTable * rt) {
  //printf("match_parm <<: %s %s\n", ~p->to_string(), repl ? ~repl->to_string() : "<null>");
  if (p->num_args() > 0) {
    if (p->is_a("pattern")) {
      if (!repl) return true;
      p = p->arg(0);
      assert(p->what().name == repl->what().name);
      bool res = match_list(m, p, p->args_begin(), p->args_end(),
                            repl, repl->args_begin(), repl->args_end(), rt);
      p = 0;
      return res;
    } else if (p->is_a("reparse")) {
      if (!repl) {
        if (p->num_args() > 1)
          repl = replace(p->arg(1), rt, NULL, false, NULL);
      }
      p = p->arg(0);
    } else {
      fprintf(stderr, "??%s\n", ~p->to_string());
      abort();
    }
  }
  return true;
}


bool match_parm(Match * m, const Syntax * p, const Syntax * repl, ReplTable * rt) {
  //printf("match_parm <<: %s :: %s\n", ~p->to_string(), repl ? ~repl->to_string() : "<null>");
  bool cont = match_prep(m, p, repl, rt);
  if (!cont) return true;
  if (repl) {
    add_match_var(m, *p, repl);
    return true;
  } else {
    return false;
  }
}

bool match_list(Match * m, 
                const Syntax * pattern, parts_iterator p_i, parts_iterator p_end, 
                const Syntax * with,    parts_iterator r_i, parts_iterator r_end,
                ReplTable * rt)
{
  bool now_optional = false;
  while (p_i != p_end) {
    const Syntax * p = *p_i;
    const Syntax * r = r_i < r_end ? *r_i : NULL;
    bool ok = match_prep(m, p, r, rt);
    if (!ok) return false;
    if (p) {
      SymbolName v = p->what();
      if (v == "@") {
        now_optional = true;
        ++p_i; 
        continue;
      }
      if (v.name[0] == '@') {
        v.name = v.name.c_str() + 1;
        SyntaxBuilder n(SYN("@"));
        n.add_parts(r_i, r_end);
        for (flags_iterator i = with->flags_begin(), e = with->flags_end(); i != e; ++i) {
          if (!pattern->flag((*i)->what()))
            n.add_flag(*i);
        }
        r = n.build();
      }
      if (!r && now_optional)
        r = NO_MATCH;
      add_match_var(m, v, r);
      if (!r/* && !now_optional*/) 
        return false;
    }
    ++p_i;
    ++r_i;
  }
  for (flags_iterator i = pattern->flags_begin(), e = pattern->flags_end(); i != e; ++i) {
    const Syntax * w = with->flag((*i)->what());
    match_parm(m, (*i)->arg(0), w ? w->arg(0) : NULL, rt);
  }
  return true;
}

Match * match(Match * orig_m, const Syntax * pattern, const Syntax * with, unsigned shift, Mark * mark) {
  Match * m = new Match();
  ReplTable * rt 
    = new ReplTable(mark, 
                    new ExpandSourceInfo(Macro::macro_call, Macro::macro_def));
  if (pattern->is_a("()")) {
    //printf("YES!\n");
    pattern = reparse("MATCH_LIST", pattern->inner());
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
    //with = with->ensure_branch(); NOT NEEDED ANYMORE?
    bool ok = match_list(m, pattern, pattern->parts_begin(), pattern->parts_end(),
                         with, with->parts_begin() + shift, with->parts_end(), rt);
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

  Match * match_f(Match * m, const Syntax * pattern, const Syntax * with, Mark * mark) {
    return match(m, pattern, with, 0, mark);
  }
  
  Match * match_args_f(Match * m, const Syntax * pattern, const Syntax * with, Mark * mark) {
    return match(m, pattern, with, 1, mark);
  }
  
  const Syntax * match_var(Match * m, UnmarkedSyntax * n) {
    Match::const_iterator i = m->begin(), e = m->end();
    for (; i != e; ++i) {
      if (i->first == *n) {
        //printf("%s %p %p %s\n", ~n->to_string(), NO_MATCH, i->second, ~i->second->to_string());
        if (i->second == NO_MATCH)
          return NULL;
        else
          return i->second;
      }
    }
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
    //res = reparse("STMTS", p->inner(), NULL, rparms);
    //if (res->num_args() == 1)
    //  res = res->arg(0);
    res = reparse_replace(SYN("@{}"), p, rparms);
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

const Syntax * reparse_prod(String what, ReparseInfo & p, Environ * env,
                            bool match_complete_str,
                            ReplTable * r, const Replacements * additional_repls)
{
  //printf("REPARSE %s %s AS %s\n", ~p->sample_w_loc(), ~p->to_string(), ~what);
  const Replacements * repls = combine_repl(p.repl, r);
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
    res = parse_prod(what, p.str, env, &combined_repls, p.cache, match_complete_str);
  } catch (Error * err) {
    //Annon * annon = new ReparseAnnon(p, what);
    //annon->prev = p->str().annon; // FIXME: Is this right
    //err->annon = annon;
    //puts(~err->message());
    throw err;
  }
  //res = SYN(new ReparseAnnon(p, what), res);
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
  return ::reparse(what, s->outer(), env);
}

static const Syntax * replace_mid(const Syntax * mid, const Syntax * repl, ReplTable * r, const Replacements * rs);

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
  if (p0 == NO_MATCH) splice = true;
  if (splice_r) *splice_r = splice;
  //printf("MID ");
  //p->str().sample_w_loc(COUT);
  //printf(" ");
  //p->arg(0)->str().sample_w_loc(COUT);
  //printf("\n");
  const Syntax * res = replace_mid(p, p0, r, rs);
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
  if (p->have_entity()) {
    return p;
  } else if (p->simple()) {
    //mid = *p;
    //goto try_mid;
    //return p;
    //printf("MARK %s\n", ~p->what());
    const Syntax * res = NULL;
    if (allow_plain_mids) res = try_mid(p, SYN(SYN("mid"), p), r, rs, splice_r);
    if (!res) res = SYN(p, r->mark, r->expand_source_info(p));
    return res;
  } else if (p->is_a("mid")/* && r->have(*p->arg(0))*/) {
    const Syntax * res = try_mid(p->arg(0), p, r, rs, splice_r);
    if (!res) goto def;
    return res;
  } else if (p->is_a("s") || p->is_a("c") || p->is_a("n") || p->is_a("f")) {
    ChangeSrc<ExpandSourceInfo> ci(r);
    return SYN(ci, *p);
  } else if (p->is_reparse()) {
    Syntax * res = 
      reparse_replace(p->part(0), p, r);
    //printf("REPLACE RES %d: %s %s\n", seql, ~res->sample_w_loc(), ~res->to_string());
    return res;
  } else {
  def:
    SyntaxBuilder res;
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      //const Syntax * q = (i == 0 && p->part(0)->simple()) ? p->part(0) : replace(p->part(i), r); // HACK
      bool splice = false;
      //static int x = 0;
      //++x;
      //printf("<<%d %s\n", x, ~p->part(i)->to_string());
      const Syntax * q = replace(p->part(i), r, rs, allow_plain_mids, &splice);
      if (splice) {
        //printf("??%d %s\n", x, ~q->to_string());
        if (!q->is_a("@")) throw unknown_error(q);
        res.add_parts(q->args_begin(), q->args_end());
        res.merge_flags(q);
      } else {
        res.add_part(q);
      }
    }
    for (flags_iterator i = p->flags_begin(), e = p->flags_end(); i != e; ++i) {
      //printf("~~%s\n", ~(*i)->to_string());
      const Syntax * q = *i;
      SyntaxBuilder r0(q->part(0)); // FIXME: I think I need to do more with first part
      if (q->num_args() > 0) // FIXME: Can there me more than one arg?
        r0.add_part(replace(q->arg(0), r, rs, allow_plain_mids, NULL));
      res.add_flag(r0.build());
    }
    //printf("REPLACE Res %d: %s\n", seql, ~res->to_string());
    return res.build(r->expand_source_info_str(p));
  }
}

static const Syntax * reparse_replace(const Syntax * new_name, 
                                      const Syntax * p, ReplTable * r)
{
  return new ReparseSyntax(SYN(new_name, r->mark, r->expand_source_info(p->part(0))),
                           combine_repl(p->repl, r),
                           p->as_reparse()->cache,
                           r->expand_source_info_str(p->outer().str),
                           r->expand_source_info_str(p->inner().str));
}

const Syntax * replace_context(const Syntax * p, const Marks * context);

const Syntax * replace_mid(const Syntax * mid, const Syntax * repl, ReplTable * r, const Replacements * rs) {
  if (repl->is_a("@")) {
    SyntaxBuilder res(repl->part(0));
    for (parts_iterator i = repl->args_begin(), e = repl->args_end(); i != e; ++i) {
      res.add_part(replace_mid(mid, *i, r, rs));
    }
    for (flags_iterator i = repl->flags_begin(), e = repl->flags_end(); i != e; ++i) {
      const Syntax * q = *i;
      SyntaxBuilder r0(q->part(0)); // FIXME: I think I need to do more with first part
      if (q->num_args() > 0) // FIXME: Can there me more than one arg?
        r0.add_part(replace_mid(mid, (*i)->part(1), r, rs));
      res.add_flag(r0.build());
    }
    return res.build(repl->str());
  } else {
    ChangeSrc<SplitSourceInfo> cs(repl, r->outer_si ? r->outer_si : new ReplaceSourceInfo(r->expand_si,mid));
    const Syntax * orig_repl = repl;
    repl = SYN(cs, *repl);

    if (mid->num_args() > 1) {
      String what = mid->arg(1)->as_symbol_name().name;
      if (repl->is_a("parm") || repl->is_a("()")) {
        if (repl->repl) {
          for (Replacements::const_iterator i = repl->repl->begin(), e = repl->repl->end(); 
               i != e; ++i) 
          {
            // FIXME: This seams like the correct thing to do,
            //        however, I have no idea if it is.....
            (*i)->outer_si = cs.new_;
          }
        }
        repl = reparse(what, repl->inner(), NULL, NULL, rs);
      }
    }
    return repl;
  }
}

const Syntax * replace_context(const Syntax * p, const Marks * context) {
  if (p->simple()) {
    //printf("REPLACE CONTEXT: %s\n", ~p->to_string());
    Syntax * res = SYN(p, context);
    //printf("REPLACE CONTEXT RES: %s\n", ~res->to_string());
    return res;
  } else if (p->is_a("s") || p->is_a("c") || p->is_a("n") || p->is_a("f")) {
    return p;
  } else if (p->is_a("{}") || p->is_a("()") || p->is_a("[]") || p->is_a("parm")) {
    // raw tokens
    fprintf(stderr, "Unhandled Case\n");
    abort();
  } else {
    SyntaxBuilder res;
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      res.add_part(replace_context(p->part(i), context));
    }
    return res.build(p->str());
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

const Syntax * ID = SYN("id");
const Syntax * ESTMT = SYN("estmt");
const Syntax * NUM = SYN("n");

// should't override following primatives
//   "exp" "stmt" "estmt" and TOKENS
// FIXME: need to add check

static const Syntax * handle_paran(parts_iterator & i, 
                                   parts_iterator e, 
                                   Environ & env) 
{
  const Syntax * p = *i;
  if (!p->is_a("()")) return NULL;
  ++i;
  //printf("handle_paran: %s\n", ~p->to_string());
  try {
    const Syntax * exp = reparse("PARAN_EXP", p->outer(), &env);
    //printf("handle_paran:: %s\n", ~exp->to_string());
    const Syntax * type = parse_decl_->parse_type(exp, env);
    if (type) return SYN(p->str(), SYN(".type"), type);
    // Since the raw string might need to be reparsed we can't use an
    // exp here.  Unfortunately this will likely mean duplicate work.
    // Avoiding that will take more thought
    else return p;
  } catch (...) {
    return p;
  }
}

static const Syntax * handle_new(parts_iterator & i, 
                                 parts_iterator e, 
                                 Environ & env)
{
  const Syntax * p = *i;
  if (!p->is_a("new")) return NULL;
  ++i;
  if (!p->simple()) return p; // already handled
  assert(i != e); // FIXME: error message
  const Syntax * type = parse_decl_->parse_type_for_new(i, e, env);
  assert(type); // FIXME: error message
  //printf(">>%s<<\n", ~type->to_string());
  const Syntax * parms = NULL;
  if (i != e && ((*i)->is_a("()") || (*i)->is_a(".")))
    parms = *i++;
  if (parms)
    return SYN(p, type, SYN("."), parms);
  else
    return SYN(p, type);
}

static Syntax * handle_operator_fun_id(const Syntax * p,
                                       parts_iterator & i, 
                                       parts_iterator e,
                                       Environ & env);

const Syntax * handle_operator_fun_id(parts_iterator & i, 
                                      parts_iterator e,
                                      Environ & env) 
{
  const Syntax * p = *i;
  if (!p->is_a("operator")) return NULL;
  ++i;
  if (!p->simple()) return p; // already handled
  return handle_operator_fun_id(p, i, e, env);
}

static Syntax * handle_operator_fun_id(const Syntax * p,
                                       parts_iterator & i, 
                                       parts_iterator e,
                                       Environ & env) 
{
  assert(i != e); // FIXME: error message
  const Syntax * type = parse_decl_->parse_type(i, e, env);
  if (type) {
    return SYN(p, type);
  } else {
    return SYN(p, *i++);
  }
}


const Syntax * handle_w_tilda(parts_iterator & i, 
                              parts_iterator e,
                              Environ & env) 
{
  const Syntax * p = *i;
  if (!p->is_a("~")) return NULL;
  ++i;
  if (!p->simple()) return p; // already handled
  assert(i != e); // FIXME: error message
  return SYN(p, *i++);
}

const Syntax * handle_unparsed_scope_op(parts_iterator & i,
                                        parts_iterator e,
                                        Environ & env)
{
  const Syntax * p = i[0];
  const Syntax * op;
  const Syntax * lhs;
  // First check for the case when "X::operator =" will incorrectly
  // parse as ((:: X operator) =)
  if (p->is_a("::") && !p->simple() && p->arg(1)->eq("operator")) {
    op = p->part(0);
    lhs = p->arg(0);
    ++i;
    p = handle_operator_fun_id(p->arg(1), i, e, env);
    printf("YES %s\n", ~p->to_string());
  } else {
    if (i + 2 >= e) return NULL;
    const Syntax * op  = i[1];
    if (op->ne("::")) return NULL;
    i += 2;
    const Syntax * lhs = p;
    p = NULL;
    if (!p) p = handle_operator_fun_id(i, e, env);
    if (!p) p = handle_w_tilda(i, e, env);
    if (!p) {
      // This case shouldn't normally happen, but you never know...
      p = try_id(*i);
      if (p) {
        const Syntax * q = handle_unparsed_scope_op(i, e, env);
        if (q) p = q;
      }
    }
  }
  if (!p) throw unknown_error(*i);
  return SYN(op, lhs, p);
}

const Syntax * e_parse_exp(const Syntax * p0, Environ & env, const char * list_is) {
  SyntaxBuilder tmp(p0->part(0));
  parts_iterator i = p0->args_begin(), e = p0->args_end();
  while (i != e) {
    const Syntax * p = NULL;
    if (!p) p = handle_paran(i, e, env);
    if (!p) p = handle_unparsed_scope_op(i, e, env);
    if (!p) p = handle_new(i, e, env);
    if (!p) p = handle_operator_fun_id(i, e, env);
    if (!p) p = *i++;
    tmp.add_part(p);
  }
  const Syntax * res = parse_exp_->parse(tmp.build(p0->str()), list_is);
  // FIXME: if really an expression than "list" needs to become the
  //        comma operator
  return res;
}

static const char * const BUILTINS[] = {
  "environ_snapshot",
  "__builtin_va_start",
  "__builtin_va_end",
  "__builtin_va_arg",
  "__builtin_va_copy"
};
static const unsigned NUM_BUILTINS = 5;

static const Syntax * try_syntax_macro_or_primitive(const SymbolName & what, 
                                                    const Syntax * p, 
                                                    Environ & env) 
{
  if (Symbol * sym = env.symbols.find<Symbol>(SymbolKey(what, SYNTAX_NS))) {
    if (Macro * m = dynamic_cast<Macro *>(sym)) {
      //printf("MACRO ON %s\n", ~what.to_string());
      return m->expand(p, p, env);
    } else {
      //printf("OTHER ON %s\n", ~what.to_string());
      return NULL;
    }
  } else {
    return NULL;
  }
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
  if (p->simple() && pos == NoPos) {
    return p;
  } if (p->simple() && asc_isdigit(what.name[0])) {
    return SYN(p->str(), NUM, p);
  } else if (is_raw_id(p)) {
    return partly_expand(SYN(p->str(), ID, p), pos, env, flags);
  } else if (what == "{}") {
    if (pos == ExpPos)
      return partly_expand(reparse("INIT", p->inner(), &env), pos, env, flags);
    else
      return partly_expand(reparse("BLOCK", p->outer(), &env), pos, env, flags);
  } else if (what == "@{}" && !(flags & EXPAND_NO_BLOCK_LIST)) {
    ReparseInfo r = p->inner();
    const Syntax * p0 = reparse_prod("STMTE", r, &env);
    if (r.str.empty()) {
      return partly_expand(p0, pos, env, flags);
    } else {
      p0 = partly_expand(p0, pos, env, flags);
      ReparseSyntax * p1 = p->as_reparse()->clone();
      p1->inner_.begin = r.str.begin;
      return SYN(SYN("@"), p0, p1);
    }
  } else if (what == "()") {
    return partly_expand(reparse("PARAN_EXP", p->outer(), &env), pos, env, flags);
  } else if (what == "[]") {
    return partly_expand(reparse("EXP", p->inner(), &env), pos, env, flags);
  } else if (what == "parm") {
    //abort();
    if (pos == NoPos)
      return partly_expand(reparse("ID", p->outer(), &env), pos, env, flags);
    else if (pos & ExpPos)
      return partly_expand(reparse("EXP", p->outer(), &env), pos, env, flags);
    else
      return partly_expand(reparse("STMT", p->outer(), &env), pos, env, flags);
  } else if (Syntax * n = try_syntax_macro_or_primitive(what, p, env)) {
    return partly_expand(n, pos, env, flags);
  } else if (what == "id") {
    p = expand_id(p, env);
    if (!(flags & EXPAND_NO_ID_MACRO_CALL)) { 
      // NOTE: ID macros can have flags, since there is no parameter list
    //       they are passed in as flags as (id <id> :(flag1 val1))
      assert_num_args(p, 1);
      const Syntax * n = p;
      const Macro * m = env.symbols.find<Macro>(n->arg(0));
      if (m && !m->overloadable()) { // id macro
        Syntax * a = SYN(PARTS(SYN(".")), FLAGS(p->flags_begin(), p->flags_end()));
        p = m->expand(p, a, env);
        return partly_expand(p, pos, env, flags);
      }
    }
  } else if (what == "call") {
    assert_num_args(p, 2);
    const Syntax * n = partly_expand(p->arg(0), OtherPos, env, flags | EXPAND_NO_ID_MACRO_CALL);
    const Syntax * a = p->arg(1);
    if (a->is_a("()")) a = reparse("SPLIT", a->inner(), &env);
    if (n && n->is_a("id")) {
      if (!(flags & EXPAND_NO_FUN_MACRO_CALL)) {
        const Macro * m = env.symbols.find<Macro>(n->arg(0));
        if (m && !m->overloadable()) { // function macros
          //  (call (id fun) (list parm1 parm2 ...))?
          p = m->expand(p, a, env);
          return partly_expand(p, pos, env, flags);
        } else if (n->arg(0)->eq(BUILTINS, NUM_BUILTINS)) {
          return SYN(p->str(), n->arg(0), PARTS(a->args_begin(), a->args_end()));
        } 
      }
      if (TypeSymbol * t = env.symbols.find<TypeSymbol>(n->arg(0))) {
        Syntax * res = SYN(SYN("anon"), SYN(t), a);
        return res;
      }
    }
    Syntax * res = SYN(p->str(), PARTS(p->part(0), p->arg(0), a));
    p = res;
  } else if (what == "raw") {
    parse_parse::Res r = parse_parse::parse(p->part(1)->str());
    return partly_expand(r.parse, pos, env, flags);
  } else if (what == "stmt") {
    assert_pos(p, pos, TopLevel|FieldPos|StmtPos|StmtDeclPos);
    //printf("TRYING STMT PARSE on %s\n", ~p->sample_w_loc()); 
    const Syntax * res = parse_decl_->parse_decl(p, env, pos == FieldPos);
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
  } else if (what == "<@") {
    if (p->num_args() < 2) {
      throw error(p, "<@ requires 2 or more args\n");
    } else if (p->num_args() == 2) {
      p = p->arg(0);
    } else {
      SyntaxBuilder res(SYN("@"));
      res.insure_space(p->num_parts() - 1 + p->num_flags());
      res.add_parts(p->args_begin(), p->args_end()-1);
      res.set_flags(p->flags_begin(), p->flags_end());
      p = res.build(p->str());
    }
  }
  // we should have a primitive
  return p;
}

const Syntax * limited_expand(const Syntax * p, Environ & env) {
  if (p->have_entity()) return p;
  SymbolName what = p->what();
  if (p->simple()) {
    return p;
  } else if (const Syntax * n = try_syntax_macro_or_primitive(what, p, env)) {
    return limited_expand(n, env);
  } else {
    return p;
  }
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
    res = partly_expand(res, pos, *env, EXPAND_NO_BLOCK_LIST);
    if (res->is_a("@")) {
      stack.push_back(top);
      top = new SimpleSyntaxEnum(res->args_begin(), res->args_end());
      goto try_again;
    } else if (res->is_a("@{}")) {
      stack.push_back(top);
      const Syntax * r = reparse("STMTS", res->inner());
      top = new SimpleSyntaxEnum(r->args_begin(), r->args_end());
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
      SyntaxBuilder res(p->part(0));
      for (parts_iterator i = p->args_begin(), e = p->args_end(); i != e; ++i)
        res.add_part(pre_parse(*i, env));
      return res.build(p->str());
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
    const InnerNS * ns = lookup_inner_ns(p, env.symbols.front);
    return expand_binding(p->arg(0), ns, env);
  } else if (p->is_a("::")) {
    throw error(p, "Can not use outer namespaces in binding form");
  } else if (p->is_a("tid")) {
    return SymbolKey(flatten_template_id(p, env), ns);
  } else if (const SymbolKeyEntity * s = p->entity<SymbolKeyEntity>()) {
    return s->name;
  } else {
    fprintf(stderr, "Unsupported Binding Form: %s\n", ~p->to_string());
    abort();
  }
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
  if (p->flag("w_snapshot")) {
    m->fun->env_ss = *env.top_level_environ;
  }
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

void expand_fun_parms(const Syntax * args, Environ & env, SyntaxBuilder & res) {
  for (unsigned i = 0; i != args->num_args(); ++i) {
    const Syntax * p = args->arg(i);
    if (p->is_a("@")) {
      expand_fun_parms(p, env, res);
    } else {
      if (p->is_a("parm") || p->is_a("()"))
        p = reparse("TOKENS", p->inner(), &env);
      //printf("FUN_PARM: %s\n", ~p->to_string());
      if (!p->is_a("...")) {
        const Type * t = parse_type(p->part(0), env);
        // an array of x as parm is really a pointer to x:
        if (const Array * a = dynamic_cast<const Array *>(t->root))
          t = env.types.inst(".ptr", a->subtype);
        Syntax * r = SYN(p->part(0)->str(), t);
        if (p->num_parts() == 2) 
          r = SYN(r, p->part(1));
        res.add_part(r);
      } else {
        res.add_part(p);
      }
    }
  }
}

Tuple * expand_fun_parms(const Syntax * parse, Environ & env) {
  //printf("FUN_PARMS: %s\n", ~parse->to_string());
  if (parse->is_a("(...)")) 
    parse = parse_decl_->parse_fun_parms(parse, env);
  SyntaxBuilder res(parse->part(0));
  expand_fun_parms(parse, env, res);
  Type * type = parse_type(res.build(), env);
  Tuple * tuple = dynamic_cast<Tuple *>(type);
  assert(tuple); // FIXME: Error Message?
  return tuple;
}

void expand_template_parms(const Syntax * p, SyntaxBuilder & res, Environ & env) {
  assert(p->is_a("<>"));
  parts_iterator i = p->args_begin(), e = p->args_end();
  while (i != e) {
    const Syntax * type = parse_decl_->parse_type(*i, env);
    if (type) res.add_part(SYN(p->str(), SYN(".type"), type));
    else res.add_part(parse_exp_->parse(*i, "seq"));
    ++i;
  }
}

const Syntax * expand_template_id(const Syntax * p, Environ & env) {
  assert(p->is_a("tid"));
  if (p->arg(1)->is_a("<>")) {
    SyntaxBuilder res(p->part(0));
    res.add_part(p->arg(0));
    expand_template_parms(p->arg(1), res, env);
    return res.build(p->str());
  } else {
    return p;
  }
}

SymbolName flatten_template_id(const Syntax * p, Environ & env) {
  p = expand_template_id(p, env);
  StringBuf buf;
  SymbolKey n = expand_binding(p->arg(0), NULL, env);
  buf.printf("%s$t%u", ~n.name, p->num_args() - 1);
  for (unsigned i = 1; i < p->num_args(); ++i) {
    const Syntax * q = p->arg(i);
    if (q->is_a(".type")) {
      Type * t = parse_type(q->arg(0), env);
      mangle_print_inst->to_string(*t, buf);
    } else {
      int v = parse_ct_value(q, env);
      buf.printf("$i%d", v);
    }
  }
  return SymbolName(buf.freeze(), n.marks);
}

const Syntax * expand_id(const Syntax * p, Environ & env) {
  if (p->simple()) {
    return p;
  } else if (p->is_a("fluid") || p->is_a("id")) {
    const Syntax * q = expand_id(p->arg(0), env);
    if (q == p->arg(0))
      return p;
    else
      return SYN(p->str(), p->part(0), q);
  } else if (p->is_a("`")) {
    const Syntax * q = expand_id(p->arg(0), env);
    if (q == p->arg(0))
      return p;
    else if (q->num_args() == 2) 
      return SYN(p->str(), p->part(0), q, p->arg(1));
    else {
      SyntaxBuilder res(p->part(0));
      res.add_part(q);
      res.add_parts(p->args_begin() + 1, p->args_end());
      return res.build();
    }
  } else if (p->is_a("::")) {
    const Syntax * q = expand_id(p->arg(0), env);
    const Syntax * r = expand_id(p->arg(1), env);
    if (q == p->arg(0) && r == p->arg(1))
      return p;
    else
      return SYN(p->str(), p->part(0), q, r);
  } else if (p->is_a("tid")) {
    return SYN(flatten_template_id(p, env));
  } else {
    return p;
  }
}

void compile_for_ct(Deps & deps, Environ & env) {

  static unsigned cntr = 0;
  
  for (unsigned i = 0, sz = deps.size(); i != sz; ++i) {
    deps.merge(deps[i]->deps());
  }
  deps.insert(find_overloaded_symbol<TopLevelVarDecl>(NULL, NULL, env.find_tls("ct_malloc"), NULL));
  deps.insert(find_overloaded_symbol<TopLevelVarDecl>(NULL, NULL, env.find_tls("ct_free"), NULL));

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
      //printf("%p %s\n", *i, ~(*i)->uniq_name());
      (*i)->ct_ptr = p;
      const Fun * f = dynamic_cast<const Fun *>(*i);
      SymbolNode * env_ss = f ? f->env_ss : NULL;
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
  printf("LOADING: %s\n", lib.str());
  void * lh = dlopen(lib, RTLD_NOW | RTLD_GLOBAL);
  if (!lh) {
    fprintf(stderr, "ERROR: %s\n", dlerror());
    abort();
  }
  unsigned macro_funs_size = *(unsigned *)dlsym(lh, "_macro_funs_size");
  if (macro_funs_size > 0) {
    const char * * i = (const char * *)dlsym(lh, "_macro_funs");
    const char * * e = i + macro_funs_size;
    //printf("---\n");
    for (; i != e; ++i) {
      //printf(">>%s\n", *i);
      const Fun * fun = find_overloaded_symbol<Fun>(NULL, NULL, env.find_tls(*i), &env);
      String uniq_name = fun->uniq_name();
      if (fun->is_macro) {
        fun->ct_ptr = dlsym(lh, ~uniq_name);
      }
      if (fun->env_ss) {
        void * * p = (void * *)dlsym(lh, ~sbprintf("%s$env_ss", ~uniq_name));
        *p = fun->env_ss;
      }
    }
    //printf("^^^\n");
  }
  unsigned ct_init_size = *(unsigned *)dlsym(lh, "_ct_init_size");
  if (ct_init_size > 0) {
    const char * * i = (const char * *)dlsym(lh, "_ct_init");
    const char * * e = i + ct_init_size;
    //printf("---\n");
    for (; i != e; ++i) {
      void * * p = (void * *)dlsym(lh, *i);
      init_ct_var(*i, p, env);
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

  typedef ast::UserType UserType;
  typedef ast::Module Module;
  
  const UserType * user_type_info(const Syntax * s, Environ * env) {
    // first see if we have a symbol name
    const UserType * ut = dynamic_cast<const UserType *>(env->types.inst(s));
    // otherwise we have a type, so try to parse it
    if (ut) return ut;
    try {
      Type * type = parse_type(s, *env);
      return dynamic_cast<const UserType *>(type->unqualified->root);
    } catch (...) {
      return NULL;
    }
  }
  
  const Module * user_type_module(const UserType * t) {
    return t->module;
  }
  
  const Module * module_info(const Syntax *, Environ * env) {
    abort();
  }

  struct ModuleSymbolsEnum : public SyntaxEnum {
    SymbolNode * cur;
    SymbolNode * stop;
    Syntax * next() {
      if (!cur) return NULL;
      Syntax * res = SYN(new SymbolKeyEntity(cur->key));
      cur = cur->next;
      return res;
    }
    ModuleSymbolsEnum(SymbolNode * c, SymbolNode * s) : cur(c), stop(s) {}
  };
  
  SyntaxEnum * module_symbols(const Module * m) {
    return new ModuleSymbolsEnum(m->syms.front, m->syms.back);
  }
  
  bool module_have_symbol(const Module * m, const Syntax * s) {
    return find_symbol<Symbol>(s, DEFAULT_NS, m->syms.front, m->syms.back);
  }
}

//
//
//

// template <typename T>
// SyntaxBase::SyntaxBase(ChangeSrc<T> & f, const Syntax & other)
//   : what_(other.what_), 
//     str_(f(other.str_.source), other.str_), 
//     d(other.d)
// {
//   if (other.repl) {
//     repl = new Replacements(*other.repl);
//     for (Replacements::const_iterator i = repl->begin(), e = repl->end(); 
//          i != e; ++i) 
//     {
//       for (ReplTable::Table::iterator j = (*i)->table.begin(), e = (*i)->table.end();
//            j != e; ++j)
//       {
//         //j->second = SYN(f, *j->second);
//       }
//     }
//   } else {
//     repl = NULL;
//   }
// }

namespace syntax_ns {
  template <typename T>
  SyntaxBase * new_syntax(ChangeSrc<T> & f, const Syntax & other) {
    SyntaxBase * syn = other.shallow_clone();
    syn->str_.source = f(other.str_.source);
    if (other.repl) {
      syn->repl = new Replacements(*other.repl);
    } else {
      syn->repl = NULL;
    }
    return syn;
  }
}
//
//
//

struct PointerEntity {
  typedef TypeInfo<PointerEntity> TypeInfo;
};

extern "C" namespace macro_abi {

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
    return SYN(p->str(), res);
  }

  const Syntax * get_symbol_prop(const Syntax * sym, const Syntax * prop, Environ * env) {
    if (sym->is_a("id")) sym = sym->arg(0);
    if (prop->is_a("id")) prop = prop->arg(0);
    return lookup_fancy_symbol<Symbol>(sym, NULL, *env)->get_prop(*prop);
  }

  Syntax * stash_ptr_in_syntax(void * ptr) {
    return SYN(SourceStr(), static_cast<PointerEntity *>(ptr));
  }

  void * extract_ptr_from_syntax(Syntax * syn) {
    return syn->entity<PointerEntity>();
  }

  const char * mangle_fun_parms(const Syntax * p, Environ * env) {
    Tuple * parms = expand_fun_parms(p, *env);
    StringBuf buf;
    for (unsigned i = 0; i != parms->parms.size(); ++i) {
      mangle_print_inst->to_string(*parms->parms[i].type, buf);
    }
    return ~buf.freeze();
  }

}

extern "C" void gdb_breakpoint() {}

extern "C" {

  void * ct_malloc(size_t size) {
    return GC_MALLOC(size);
  }

  void ct_free(void * ptr) {
    return GC_FREE(ptr);
  }

}

