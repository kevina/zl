#include <set>

#include <stdio.h>
#include <dlfcn.h>

#include "iostream.hpp"
#include "peg.hpp"
#include "expand.hpp"
#include "ast.hpp"
#include "parse_op.hpp"
#include "parse_decl.hpp"

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
    //o.printf("(%p) ", this);
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
  bool have(String n) const {
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
  void to_string(OStream & o, PrintFlags f) const {
    o.printf("{");
    //o.printf("...");
    for (Table::const_iterator i = table.begin(), e = table.end(); i != e; ++i) {
      //o.printf("%s", ~i->first.to_string());
      o.printf("%s=>", ~i->first.to_string());
      i->second->to_string(o, f);
      o.printf(",");
    }
    o.printf("}");
  }
  ReplTable(const ast::Mark * m, ExpandSourceInfo * e)
    : mark(m), expand_si(e), ci(NULL, e), outer_si(NULL) {}
};

//
//
//

bool Replacements::anywhere(String s) const {
  for (const_iterator i = begin(), e = end(); i != e; ++i)
    if ((*i)->have(s)) return true;
  return false;
}

void Replacements::to_string(OStream & o, PrintFlags f) const {
  for (const_iterator i = begin(), e = end(); i != e; ++i)
    (*i)->to_string(o, f);
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
namespace macro_abi {
  typedef const Marks Context;
  typedef Syntax SyntaxList;
  typedef const ::Syntax Syntax;
  typedef Syntax UnmarkedSyntax;
  //struct SyntaxEnum;
  extern "C" Mark * new_mark_f(SymbolNode *);
  extern "C" Syntax * syntax_flag(Syntax *, UnmarkedSyntax *);
  extern "C" SyntaxList * new_syntax_list();
  extern "C" int syntax_list_empty(const SyntaxList *);
  extern "C" void syntax_list_append(SyntaxList *, Syntax *);
  extern "C" Syntax * syntax_enum_next(SyntaxEnum *);
  extern "C" Match * match(Match * m, const UnmarkedSyntax * pattern, Syntax * with);
  extern "C" Match * match_args(Match * m, const UnmarkedSyntax * pattern, Syntax * with);
  extern "C" Match * match_local(Match *, ...);
  extern "C" Syntax * match_var(Match *, UnmarkedSyntax *);
  extern "C" SyntaxEnum * match_varl(Match *, UnmarkedSyntax *);
  extern "C" Syntax * replace(const UnmarkedSyntax * p, Match * match, Mark * mark);
  extern "C" Context * get_context(Syntax * p);
  extern "C" Syntax * replace_context(Syntax * p, Context * context);
  extern "C" Syntax * partly_expand(Syntax *, Position pos, Environ *);
  extern "C" Syntax * reparse(Syntax *, const char *, Environ *);
  extern "C" SyntaxEnum * partly_expand_list(SyntaxEnum *, Position pos, Environ *);
  extern "C" Syntax * pre_parse(Syntax *, Environ *);
  extern "C" const UnmarkedSyntax * string_to_syntax(const char *);
  extern "C" const char * syntax_to_string(const UnmarkedSyntax *);
  extern "C" void dump_syntax(const UnmarkedSyntax * s);
  typedef const UserType UserTypeInfo;
  typedef const Module ModuleInfo;
  extern "C" const UserTypeInfo * user_type_info(Syntax *, Environ *);
  extern "C" const ModuleInfo * user_type_module(const UserTypeInfo *);
  extern "C" const ModuleInfo * module_info(Syntax *, Environ *);
  extern "C" SyntaxEnum * module_symbols(const ModuleInfo *);
  extern "C" bool module_have_symbol(const ModuleInfo *, Syntax *);
  extern "C" Environ * temp_environ(Environ *);
  extern "C" size_t ct_value(Syntax *, Environ *);
  extern "C" Syntax * error(Syntax *, const char *, ...); 
  extern "C" Syntax * get_symbol_prop(Syntax * sym, Syntax * prop, Environ * env);
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

const Syntax * replace(const Syntax * p, ReplTable * r, const Replacements * rs);

struct MacroSymbol : public Symbol {
  SymbolName real_name;
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

const Syntax * MacroSymbol::macro_call = NULL;
const Syntax * MacroSymbol::macro_def = NULL;

struct SimpleMacro : public MacroSymbol {
  //const SourceFile * entity;
  const Syntax * parse;
  const Syntax * parms;
  const Syntax * free;
  const Syntax * repl;
  const SymbolNode * env;
  SimpleMacro * parse_self(const Syntax * p, Environ & e) {
    //printf("PARSING MAP %s\n%s\n", ~p->arg(0)->what().name, ~p->to_string());
    env = e.symbols.front;
    //entity = p->str().source;
    def = parse = p;
    assert_num_args(p, 3);
    real_name = expand_binding(p->arg(0), e);
    name = real_name.name;
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
};

struct Macro : public MacroSymbol {
  const Syntax * parse;
  const TopLevelVarSymbol * fun;
  typedef const Syntax * (*MacroCall)(const Syntax *, Environ * env);
  Macro * parse_self(const Syntax * p, Environ & e) {
    //printf("PARSING MACRO %s\n", ~p->arg(0)->name);
    //p->print();
    //printf("\n");
    parse = p;
    assert_num_args(p, 1, 2);
    real_name = expand_binding(p->arg(0), e);
    name = real_name.name;
    fun = e.symbols.lookup<TopLevelVarSymbol>(p->num_args() == 1 ? p->arg(0) : p->arg(1));
    dynamic_cast<const Fun *>(fun->decl)->is_macro = true;
    def = fun->decl->parse_;
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
};

//
//
//

namespace macro_abi {

  Mark * new_mark_f(SymbolNode * e) {
    return new Mark(e);
  }
  
  const Syntax * syntax_flag(const Syntax * s, UnmarkedSyntax * n) {
    return s->flag(*n);
  }
  
  SyntaxList * new_syntax_list() {
    return new ::Syntax(new Syntax("@"));
  }
  
  int syntax_list_empty(const SyntaxList * p) {
    return p->num_args() == 0;
  }
  
  void syntax_list_append(SyntaxList * l, const Syntax * p) {
    l->add_part(p);
  }

  const Syntax * syntax_enum_next(SyntaxEnum * e) {
    return e->next();
  }

}
  
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
      //printf("??%s\n", ~p->to_string());
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
        if (with->d) {
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
  if (pattern->is_a("()"))
    pattern = reparse("MATCH_LIST", pattern->arg(0));
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
    //o.printf("%s=>", ~i->first.to_string());
    //i->second->to_string(o);
    //printf(",");
  //}
  //printf("\n");
  if (orig_m) 
    m->insert(m->end(), orig_m->begin(), orig_m->end());
  return m;
}

namespace macro_abi {

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
                    new ExpandSourceInfo(MacroSymbol::macro_call, MacroSymbol::macro_def));
  //new ExpandSourceInfo);
  if (match)
    rparms->table = *match;
  //printf("tREPLACE: %s\n", ~p->to_string());
  //rparms->macro_call = MacroSymbol::macro_call;
  //rparms->macro_def = MacroSymbol::macro_def;
  const Syntax * res;
  if (p->is_a("{}")) {
    res = reparse("STMTS", p->arg(0), rparms);
    if (res->num_args() == 1)
      res = res->arg(0);
  } else if (p->simple() || p->is_a("w/inner") || p->is_a("w/outer") || p->is_a("fluid")) {
    // FIXME: This is a hack, in same cases this may not be the correct thing to do.
    // It is needed because sometimes we want "syntax ID" to be just a raw ID (ie a simple
    // syntax) but othertimes it needs to be reparsed so that it can become a mid
    res = reparse("ID", p, rparms);
  } else {
    res = replace(p, rparms, NULL);
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
      res = replace(res, *i, &combined_repls);
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

static const Syntax * replace_mid(const Syntax * mid, const Syntax * repl, ReplTable * r, const Replacements * rs);
static inline void add_repl(const Syntax * p, Syntax * res) {flatten("@@", p, res);}

const Syntax * replace(const Syntax * p, ReplTable * r, const Replacements * rs) {
  // FIXME: Do I need to handle the case where the entity is a symbol name?
  static unsigned seq=0;
  unsigned seql = seq++;
  //printf("REPLACE %d: %s\n", seql, ~p->to_string());
  //r->to_string(COUT, PrintFlags());
  //printf("\n");
  if (p->simple()) {
    //return p;
    //printf("MARK %s\n", ~p->what());
    return new Syntax(p, r->mark, r->expand_source_info(p));
  } else if (p->is_a("mid") && r->have(*p->arg(0))) {
    const Syntax * p0 = r->lookup(*p->arg(0));
    //printf("MID ");
    //p->str().sample_w_loc(COUT);
    //printf(" ");
    //p->arg(0)->str().sample_w_loc(COUT);
    //printf("\n");
    const Syntax * res = replace_mid(p, p0, r, rs);
    //printf("REPLACE res %d: %s %s\n", seql, ~res->sample_w_loc(), ~res->to_string());
    return res;
  } else if (p->is_a("string") || p->is_a("char") || p->is_a("literal") || p->is_a("float") || p->is_a("sym")) {
    ChangeSrc<ExpandSourceInfo> ci(r);
    return new Syntax(ci, *p);
  } else if (p->is_a("{}") || p->is_a("()") || p->is_a("[]") || p->is_a("parm")) {
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
    Syntax * res = new Syntax(r->expand_source_info_str(p));
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      //const Syntax * q = (i == 0 && p->part(0)->simple()) ? p->part(0) : replace(p->part(i), r); // HACK
      const Syntax * q = replace(p->part(i), r, rs);
      add_repl(q, res);
    }
    for (Flags::const_iterator i = p->flags_begin(), e = p->flags_end(); i != e; ++i) {
      //printf("~~%s\n", ~(*i)->to_string());
      const Syntax * q = *i;
      Syntax * r0 = new Syntax(q->part(0)); // FIXME: I think I need to do more with first part
      if (q->num_args() > 0) // FIXME: Can there me more than one arg?
        r0->add_part(replace(q->arg(0), r, rs));
      res->add_flag(r0);
    }
    //printf("REPLACE Res %d: %s\n", seql, ~res->to_string());
    return res;
  }
}

const Syntax * replace_context(const Syntax * p, const Marks * context);

const Syntax * replace_mid(const Syntax * mid, const Syntax * repl, ReplTable * r, const Replacements * rs) {
  if (repl->is_a("@")) {
    // We should only flatten the results of a replacement inside the
    // replace function if the mid is a "raw" mid, that is one where
    // reparse should't get called, and generally is found in raw
    // syntax.  This is becuase we may flatten the results too early
    // otherwise.  For example if we flatten the parameters of a
    // function call now, which is just a list of tokens, we will end
    // most likely end up with a list of expressions which will not
    // parse correctly as it is expected to be tokens seperated by
    // ",".  In this case the list should be flattened higher up when
    // a list of expressions is expected (ie after it is parrsed by
    // ParseExp::parse).
    //
    // Thus, we change the "@" to a "@@" when it should be flattened
    // inside the replace function, otherwise we leave it as a "@", in
    // the assumption that that it will be correctly interpreted
    // latter.
    Syntax * res = new Syntax(mid->num_args() > 1 ? new Syntax("@") : new Syntax("@@")); 
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
    //printf(">>%s %s\n", ~mid->sample_w_loc(), ~repl->sample_w_loc());
    //mid->str().source->dump_info(COUT, "  mid> ");
    if (mid->repl) {
      //printf("<mid>");
      //mid->repl->to_string(COUT, PrintFlags());
      //printf("\n");
    }
    //repl->str().source->dump_info(COUT, " orep> ");
    //r->expand_si->dump_info(COUT, "  exp> ");
    //printf("<  r>");
    //r->to_string(COUT, PrintFlags());
    //printf("\n");

    ChangeSrc<SplitSourceInfo> cs(repl, r->outer_si ? r->outer_si : new ReplaceSourceInfo(r->expand_si,mid));
    //ChangeSrc<void> cs;
    //ChangeSrc<SplitSourceInfo> cs(repl, new ReplaceSourceInfo(r->expand_si->clone(new SplitSourceInfo(mid->str().source, r->expand_si->parent)),mid));

    const Syntax * orig_repl = repl;
    repl = new Syntax(cs, *repl);
    //cs.new_->dump_info(COUT, "  new> ");

//     if (rs) {
//       printf("RSRSRSRSRS\n");
//       for (Replacements::const_iterator i = rs->begin(), e = rs->end(); 
//            i != e; ++i) 
//       {
//         for (ReplTable::Table::iterator j = (*i)->table.begin(), e = (*i)->table.end();
//              j != e; ++j)
//         {
//           printf("<><>%s\n", ~j->first.to_string());
//           j->second = new Syntax(cs, *j->second);
//         }
//       }
//     }

    //printf(">>> %s %s\n", ~repl->sample_w_loc(), ~repl->to_string());
    //repl->str().source->dump_info(COUT, "  rep> ");

    if (mid->num_args() > 1) {
      String what = mid->arg(1)->as_symbol_name().name;
      if (what == "TOKEN" || what == "EXP" || what == "STMT")
        what = "PARM";
      if (repl->simple() && !repl->str().empty()) {
        // if p0 has marks, they must be preserved
        //printf("REPL SIMPLE %s %s\n", ~mid->to_string(), ~repl->to_string());
        repl = replace_context(reparse(what, repl, NULL, rs), repl->what().marks);
        //printf("REPL SIMPLE RES %s %s\n", ~mid->to_string(), ~repl->to_string());
      } else if (repl->is_a("parm") || repl->is_a("()")) {
        //printf("REPL REPARSE %s %s %s\n", ~mid->to_string(), ~repl->sample_w_loc(), ~repl->to_string());
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
        //printf("repl reparse %s %s %s\n", ~mid->to_string(), ~repl->sample_w_loc(), ~repl->to_string());
      } else {
        //printf("REPL OTHER %s %s\n", ~mid->to_string(), ~repl->to_string());
      }
    }
    //printf("REPLACE RES %d: %s\n", seql, ~p0->to_string());
    return repl;
  }
}

const Syntax * replace_context(const Syntax * p, const Marks * context) {
  if (p->simple()) {
    //printf("REPLACE CONTEXT: %s\n", ~p->to_string());
    Syntax * res = new Syntax(p, context);
    //printf("REPLACE CONTEXT RES: %s\n", ~res->to_string());
    return res;
  } else if (p->is_a("string") || p->is_a("char") || p->is_a("literal") || p->is_a("float") || p->is_a("sym")) {
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

namespace macro_abi {

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

namespace macro_abi {
  
  extern "C" const UnmarkedSyntax * string_to_syntax(const char * str) {
    return parse_str("SYNTAX_STR", SourceStr(str));
  }
  
  extern "C" const char * syntax_to_string(const UnmarkedSyntax * s) {
    if (s->simple()) {
      return s->what().name;
    } else {
      return ~s->to_string();
    }
  }
  
  extern "C" void dump_syntax(const UnmarkedSyntax * s) {
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

// should't override following primatives
//   "exp" "stmt" "estmt" and TOKENS
// FIXME: need to add check

const Syntax * handle_paran(const Syntax * p, Environ & env) {
  //printf("handle_paran: %s\n", ~p->to_string());
  try {
    const Syntax * exp = reparse("PARAN_EXP", p);
    const Syntax * type = parse_decl_->parse_type(exp, env);
    if (type) return new Syntax(p->str(), new Syntax("(type)"), type);
    // Since the raw string might need to be reparsed we can't use an
    // exp here.  Unfortunately this will likely mean duplicate work.
    // Avoiding that will take more thought
    else return p;
  } catch (...) {
    return p;
  }
}

const Syntax * e_parse_exp(const Syntax * p, Environ & env) {
  //printf("e_parse_exp: %s\n", ~p->to_string());
  Syntax * tmp = new Syntax(p->str(), p->part(0));
  for (unsigned i = 0; i != p->num_args(); ++i) {
    const Syntax * t = p->arg(i);
    if (t->is_a("()")) t = handle_paran(t, env);
    tmp->add_part(t);
  }
  const Syntax * res = parse_exp_->parse(tmp);
  // FIXME: if really an expression than "list" needs to become the
  //        comma operator
  return res;
}

const Syntax * partly_expand(const Syntax * p, Position pos, Environ & env, unsigned flags) {
  if (p->entity()) return p;
  SymbolName what = p->what().name;
  //printf("\n>expand>%s//\n", ~what);
  //p->str().sample_w_loc(COUT);
  //COUT << "\n";
  //p->print();
  //printf("\n////\n");
  if (p->simple()) {
    throw error(p, "partly_expand can't be simple: %s", ~p->to_string());
    //fprintf(stderr, "partly_expand can't be simple: %s\n", ~p->to_string());
    //abort(); // FIXME: Error Message
    //return p;
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
    p = env.symbols.lookup<MacroSymbol>(SymbolKey(what, SYNTAX_NS), p->str())->expand(p, p, env);
    return partly_expand(p, pos, env, flags);
  } else if (what == "id" && !(flags & EXPAND_NO_ID_MACRO_CALL)) { 
    // NOTE: ID macros can have flags, since there is no parameter list
    //       they are passed in as flags as (id <id> :(flag1 val1))
    assert_num_args(p, 1);
    const Syntax * n = p;
    const MacroSymbol * m = env.symbols.find<MacroSymbol>(n->arg(0));
    if (m) { // id macro
      Syntax * a = new Syntax(new Syntax("list"));
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
        const MacroSymbol * m = env.symbols.find<MacroSymbol>(n->arg(0));
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
      res = e_parse_exp(p, env);
    //printf("expand stmt res0 %s\n", res ? ~res->to_string() : "<?>");
    return partly_expand(res, pos, env, flags);
  } else if (what == "exp" || what == "init") {
    //printf("PARSE EXP %s\n", ~p->to_string());
    assert_pos(p, pos, ExpPos);
    p = e_parse_exp(p, env);
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

namespace macro_abi {
  
  SyntaxEnum * partly_expand_list(SyntaxEnum * l, Position pos, Environ * env) {
    return new PartlyExpandSyntaxEnum(l, pos, env);
  }
  
  const Syntax * pre_parse(const Syntax * p, Environ * env) {
    //printf("PRE_PARSE\n");
    return pre_parse_decl(p, *env);
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
  } else if (p->is_a("w/inner")) {
    assert_num_args(p, 2);
    const InnerNS * ns = env.symbols.lookup<InnerNS>(p->arg(1), INNER_NS);
    return expand_binding(p->arg(0), ns, env);
  } else if (p->is_a("w/outer")) {
    throw error(p, "Can not use outer namespaces in binding form");
  } else if (const SymbolKeyEntity * s = dynamic_cast<const SymbolKeyEntity *>(p->entity())) {
    return s->name;
  } else {
    fprintf(stderr, "Unsupported Binding Form: %s\n", ~p->to_string());
    abort();
  }
}

AST * parse_map(const Syntax * p, Environ & env) {
  //printf("MAP>>%s\n", ~p->to_string());
  SimpleMacro * m = new SimpleMacro;
  m->parse_self(p, env);
  if (p->is_a("smacro"))
    env.add(SymbolKey(m->real_name, SYNTAX_NS), m);
  else
    env.add(m->real_name, m);
  return new Empty();
}

AST * parse_macro(const Syntax * p, Environ & env) {
  Macro * m = new Macro;
  m->parse_self(p, env);
  if (p->is_a("make_syntax_macro"))
    env.add(SymbolKey(m->real_name, SYNTAX_NS), m);
  else
    env.add(m->real_name, m);
  return new Empty();
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

Tuple * expand_fun_parms(const Syntax * parse, Environ & env) {
  //printf("FUN_PARMS: %s\n", ~parse->to_string());
  Syntax * res = new Syntax(parse->part(0));
  for (unsigned i = 0; i != parse->num_args(); ++i) {
    const Syntax * p = parse->arg(i);
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
  Type * type = parse_type(res, env);
  Tuple * tuple = dynamic_cast<Tuple *>(type);
  assert(tuple); // FIXME: Error Message?
  return tuple;
}



void compile_for_ct(Deps & deps, Environ & env) {

  static unsigned cntr = 0;
  
  for (unsigned i = 0, sz = deps.size(); i != sz; ++i) {
    deps.merge(deps[i]->decl->deps());
  }

  //printf("COMPILE FOR CT DEPS: %d\n", deps.size());
  //for (Deps::iterator i = deps.begin(), e = deps.end(); i != e; ++i)
  //  printf("  %s\n", ~(*i)->name);
  //printf("---\n");
  
  printf("COMPILE FOR CT: zlfct%03d\n", cntr);
  StringBuf buf;
  buf.printf("./zlfct%03d.c", cntr);
  String source = buf.freeze();
  buf.printf("./zlfct%03d.so", cntr);
  String lib = buf.freeze();
  buf.printf("gcc -g -fexceptions -shared -fpic -o zlfct%03d.so zlfct%03d.c", cntr, cntr);
  String cmd = buf.freeze();
  cntr++;
  
  CompileWriter cw;
  cw.open(source, "w");
  cw.deps = &deps;
  compile(*env.top_level_symbols, cw);
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
      SymbolNode * env_ss = dynamic_cast<const Fun *>((*i)->decl)->env_ss;
      if (env_ss) {
        StringBuf buf;
        buf.printf("%s$env_ss", ~(*i)->uniq_name());
        void * p = dlsym(lh, ~buf.freeze());
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
      const TopLevelVarSymbol * sym = dynamic_cast<const TopLevelVarSymbol *>(env.find_tls(*i));
      const Fun * fun = dynamic_cast<const Fun *>(sym->decl);
      String uniq_name = sym->uniq_name();
      if (fun->is_macro) {
        sym->ct_ptr = dlsym(lh, ~uniq_name);
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


AST * parse_fluid_binding(const Syntax * p, Environ & env) {
  assert_num_args(p, 1);
  SymbolKey n = expand_binding(p->arg(0), DEFAULT_NS, env);
  FluidBinding * b = new FluidBinding(n.name, mark(n, new Mark(NULL)));
  env.add(n, b);
  return new Empty();
}


//
// 
//

namespace macro_abi {
  
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
    d(other.d ? new D(f, *other.d) : 0), 
    entity_(other.entity_) 
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

namespace macro_abi {

  Environ * temp_environ(Environ * env) {
    env = new Environ(env->new_scope());
    env->top_level_symbols = NULL;
    return env;
  }
  
  size_t ct_value(const Syntax * p, Environ * env) {
    AST * ast = parse_exp(p, *env);
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
  
  extern "C" const Syntax * get_symbol_prop(const Syntax * sym, const Syntax * prop, Environ * env) {
    return env->symbols.lookup<TopLevelSymbol>(sym)->get_prop(*prop);
  }

}
