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

//
//
//

// class ReplaceSourceInfo : public SourceInfo {
// public:
//   const Syntax * mid;
//   //const Syntax * repl;
//   //const Syntax * call;
//   ReplaceSourceInfo(const SourceInfo * p, const Syntax * m)
//     : SourceInfo(p), mid(m) {}
//   const SourceInfo * clone_until(const SourceInfo * stop, 
//                                  const SourceInfo * new_parent) const {
//     return new ReplaceSourceInfo(parent->clone_until(stop, new_parent), mid);
//   }
//   bool dump_info_self(OStream &) const;
//   //ReplaceSourceInfo(const SourceInfo * s, const Syntax * m, const Syntax * r, const Syntax * c)
//   //  : source(s), mid(m), repl(r), call(c) {}
// };

// bool ReplaceSourceInfo::dump_info_self(OStream & o) const {
//   o << "when replacing ";
//   mid->sample_w_loc(o);
//   //o << " with ";
//   //repl->sample_w_loc(o);
//   //o.printf("/%s/ ", ~sample(repl->to_string(), 40));
//   //o << ", in expansion of ";
//   //call->sample_w_loc(o);
//   return true;
// }

//
//
//

// const SourceInfo * SyntaxSourceInfo::clone_until(const SourceInfo * stop, 
//                                                  const SourceInfo * new_parent) const 
// {
//   return new SyntaxSourceInfo(parent->clone_until(stop, new_parent), syntax);
// }

// bool SyntaxSourceInfo::dump_info_self(OStream & o) const {
//   return false;
//   //o << "in syntax ";
//   //syntax->sample_w_loc(o);
// }

//
//
//

// class ExpandSourceInfo : public SourceInfo {
// public:
//   const Syntax * call;
//   const Syntax * def;
//   const Syntax * outer_ip;
//   ExpandSourceInfo()
//     : call(), def(), outer_ip() {}
//   void set(const Syntax * c, const Syntax * d) 
//     { parent = c->str().source; call = c, def = d;}
//   ExpandSourceInfo(const Syntax * c, const Syntax * d) 
//     : SourceInfo(c->str().source), call(c), def(d), outer_ip(NULL) {}
//   ExpandSourceInfo(const SourceInfo * p, const Syntax * c, const Syntax * d, const Syntax * o) 
//     : SourceInfo(p), call(c), def(d), outer_ip(o) {}
//   const SourceInfo * find_insertion_point(const Syntax * outer) const {
//     //printf("ESI FIND I POINT: %p\n", outer_ip);
//     if (outer == outer_ip) return this;
//     return parent ? parent->find_insertion_point(outer) : NULL;
//   }
//   const SourceInfo * clone_until(const SourceInfo * stop, 
//                                  const SourceInfo * new_parent) const {
//     const SourceInfo * np = (stop == this) ? new_parent : parent->clone_until(stop, new_parent);
//     ExpandSourceInfo * res = new ExpandSourceInfo(np, call, def, outer_ip);
//     //if (!call) printf("WARNING 123 on %p now %p\n", this, res);
//     return res;
//   }
//   const SourceInfo * clone(const SourceInfo * new_parent) const {
//     return new ExpandSourceInfo(new_parent, call, def, outer_ip);
//   }

//   bool dump_info_self(OStream &) const;
// };

// bool ExpandSourceInfo::dump_info_self(OStream & o) const {
//   if (call) {
//     //o.printf("(%s) ", ~call->to_string());
//     o << "in expansion of ";
//     call->sample_w_loc(o);
//   } else {
//     o << "in expansion of <unknown>\n";
//   }
//   return true;
// }

//
//
//

static const Syntax * const NO_MATCH = SYN(SYN("@"));

//
//
//

struct MapSource_ExpansionOf {
  const ExpansionOf * expansion_of;
  struct CacheItem {const SourceBlock * key; const SourceInfo * value;};
  typedef Vector<CacheItem> Cache;
  Cache cache;
  MapSource_ExpansionOf(const ExpansionOf * eo) : expansion_of(eo) {}
  const SourceInfo * f (const SourceInfo * o) {
    if (!o) return NULL;
    CacheItem item;
    item.key = &o->block;
    for (Cache::const_iterator i = cache.begin(), e = cache.end(); i != e; ++i)
      if (i->key == item.key) return i->value;
    item.value = new SourceInfo(*item.key, expansion_of);
    cache.push_back(item);
    return item.value;
  }
  MapSourceRes operator() (const Syntax * syn) {
    return MapSourceRes(f(syn->str_.source));
  }
  MapSourceRes operator() (const ReparseSyntax * syn, const Replacements * repl);
};

//
//
//

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
    for (Table::const_iterator i = table.begin(); i != table.end(); ++i) {
      if (i->first == n) return true;
    }
    return false;
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
    for (Table::const_iterator i = table.begin(); i != table.end(); ++i) {
      if (i->first.name == n) return true;
    }
    return false;
  }
  bool have(const char * n) const {
    for (Table::const_iterator i = table.begin(); i != table.end(); ++i) {
      if (i->first.name == n) return true;
    }
    return false;
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

  typedef Vector<const void *> AntiQuotes;
  AntiQuotes antiquotes;
  unsigned lookup_antiquote(const void * key) const {
    for (unsigned i = 0, sz = antiquotes.size(); i < sz; ++i) {
      if (antiquotes[i] == key) return i;
    }
    return NPOS;
  }

  const ast::Mark * mark;
  MapSource_ExpansionOf * ci;
  MapSource_QuotedSyntax * qcs;
  bool no_repl;
  const SourceInfo * expand_source_info(const SourceInfo * s) {
    return ci->f(s);
  }
  const SourceInfo * expand_source_info(const Syntax * s) {
    return expand_source_info(s->str().source);
  }
  inline SourceStr expand_source_info_str(const SourceStr & str) {
    return SourceStr(expand_source_info(str.source), str.begin, str.end);
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
  ReplTable(const ast::Mark * m, MapSource_ExpansionOf * e)
    : mark(m), ci(e), qcs(NULL), no_repl(false) {}
  ReplTable(MapSource_ExpansionOf * e) 
    : mark(NULL), ci(e), qcs(NULL), no_repl(true) {}
  ReplTable(MapSource_QuotedSyntax * qcs) 
    : mark(NULL), ci(NULL), qcs(qcs), no_repl(true) {}
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
  const char * s = ~s0;
  if (*s == '@') ++s;
  for (const_iterator i = begin(), e = end(); i != e; ++i)
    if ((*i)->have(s)) return true;
  return false;
}

unsigned Replacements::lookup_antiquote(const void * key) const {
  for (const_iterator i = begin(), e = end(); i != e; ++i) {
    unsigned idx = (*i)->lookup_antiquote(key);
    if (idx != NPOS) return idx;
  }
  return NPOS;
}

void Replacements::to_string(OStream & o, PrintFlags f, SyntaxGather * g) const {
  for (const_iterator i = begin(), e = end(); i != e; ++i)
    (*i)->to_string(o, f, g);
}

//
//
//

MapSourceRes MapSource_QuotedSyntax::operator() 
  (const ReparseSyntax * other, const Replacements * repl) 
{
  ReplTable * rt = new ReplTable(this);
  return MapSourceRes(f(other->str_.source), 
                      combine_repl(repl, new ReplTable(this)));
}

MapSourceRes MapSource_ExpansionOf::operator() 
  (const ReparseSyntax * syn, const Replacements * repl) 
{
  if (!repl || repl->empty() || repl->back()->ci != this)
    repl = combine_repl(repl, new ReplTable(this));
  return MapSourceRes(f(syn->str_.source), repl);
}

//
//
//

using namespace ast;

void assert_pos(const Syntax * p, Position have, unsigned need);

void compile_for_ct(Deps & deps, Environ & env);

// see prelude.zlh

// the slightly different C++ version
typedef ReplTable::Table Match;
extern "C" 
namespace macro_abi {
  typedef const Marks Context;
  typedef MutableSyntax SyntaxList;
  typedef const ::Syntax Syntax;
  typedef Syntax UnmarkedSyntax;
  //struct SyntaxEnum;
  Match * match_parts_f_(Match * m, const UnmarkedSyntax * pattern, Syntax * with, Mark * mark);
  Match * match_f_(Match * m, const UnmarkedSyntax * pattern, Syntax * with, Mark * mark);
  Syntax * replace(const UnmarkedSyntax * p, Match * match, Mark * mark);
  Context * get_context(Syntax * p);
  Syntax * replace_context(Syntax * p, Context * context);
  Syntax * partly_expand(Syntax *, Position pos, Environ *);
  Syntax * reparse(Syntax *, const char *, Environ *);
  SyntaxEnum * partly_expand_list(SyntaxEnum *, Position pos, Environ *);
  Syntax * pre_parse(Syntax *, Environ *);
}

Match * match(Match * orig_m, const Syntax * pattern, const Syntax * with, unsigned shift, Mark * mark);

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
                              bool * splice);
const Syntax * replace(const Syntax * p, ReplTable::Table * match, 
                       ReplTable::AntiQuotes * aqs, Mark * mark);

static const Syntax * reparse_replace(const Syntax * new_name, 
                                      const Syntax * p, ReplTable * r);

struct Macro : public MacroInfo, public Declaration, public Symbol {
  static MapSource_ExpansionOf * macro_ci;
  Overloadable overloadable_;
  Macro() {}
  Overloadable overloadable() const {return overloadable_;}
  struct MacroInfo {
    MapSource_ExpansionOf * orig_ci;
    MacroInfo(const Macro * m, const Syntax * c) {
      orig_ci = macro_ci;
      macro_ci = new MapSource_ExpansionOf(new ExpansionOf(m, c->str()));
    }
    ~MacroInfo() {
      macro_ci = orig_ci;
    }
  };
  virtual const Syntax * expand(const Syntax *, Environ & env) const = 0;
  const Syntax * expand(const Syntax * s, const Syntax * p, Environ & env) const {
    static unsigned c0 = 0;
    unsigned c = ++c0;
    //printf("%d EXPAND: %s %s\n", c, ~s->sample_w_loc(), ~p->sample_w_loc());
    try {
      MacroInfo whocares(this, s);
      Syntax * res = expand(p, env);
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

MapSource_ExpansionOf * Macro::macro_ci = NULL;

struct SimpleMacro : public Macro {
  const char * what() const {return "simple-macro";}
  //const SourceFile * entity;
  const Syntax * parms;
  const Syntax * free;
  const Syntax * repl;
  const SymbolNode * env;
  SimpleMacro * parse_self(const Syntax * p, Environ & e) {
    env = e.symbols.front;
    //entity = p->str().source;
    def = syn = p;
    assert_num_args(3);
    real_name = expand_binding(p->arg(0), e);
    if (real_name.name == "+") real_name.ns = OPERATOR_NS; // FIXME: HACK!
    parms = flatten(p->arg(1));
    //printf("PARSING MAP %s\n%s\n", ~real_name.to_string(), ~p->to_string());
    //printf("MAP PARMS %s: %s\n", ~p->arg(0)->what().name, ~parms->to_string());
    free = p->flag("free");
    if (free)
      free = free->arg(0);
    const Syntax * typed_parms_syn = p->flag("typed-parms");
    if (typed_parms_syn) {
      //printf("TYPED PARMS = %s\n", ~typed_parms_syn->to_string());
      overloadable_ = expand_fun_parms(typed_parms_syn->arg(0), e); 
    }
    const Syntax * id_macro = p->flag("id");
    if (id_macro) {
      overloadable_ = Overloadable::AS_ID;
    }
    repl = p->arg(2)->map_source(*new MapSource_QuotedSyntax(p->arg(2)->str()));
    return this;
  }
  const Syntax * expand(const Syntax * p, Environ &) const {
    //if (syn)
    //  printf("EXPANDING MAP %s\n", ~syn->to_string());
    //printf("EXPANDING PARMS %s\n", ~p->to_string());
    using namespace macro_abi;
    using macro_abi::Syntax;
    Mark * mark = new Mark(env);
    Match * m = match(NULL, parms, p, 1, mark);
    if (!m)
      throw error(p, "Wrong number of arguments or other mismatch in call to %s.", ~real_name.name);
    if (free)
      m = match(m, free, replace_context(free, get_context(p)), 0, mark);
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
  const Syntax * res = synb.build();
  //printf("EXPAND MACRO: %s %s\n", ~p->to_string(), ~res->to_string());
  return macro->expand(p, res, env);
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
  SyntaxEnum * clone() const {
    return new SimpleSyntaxEnum(*this);
  }
};

extern "C" 
namespace macro_abi {

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
  
  unsigned syntax_list_size(const SyntaxList * p) {
    return p->num_args();
  }
  
  size_t syntax_list_append(SyntaxList * l, const Syntax * p) {
    size_t pos = l->num_args();
    l->add_part(p);
    return pos;
  }

  void syntax_list_append_flag(SyntaxList * l, const Syntax * p) {
    if (!p) return;
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

  SyntaxEnum * syntax_enum_clone(SyntaxEnum * e) {
    return e->clone();
  }

}

//
//
//

void add_match_var(Match * m, const SymbolName & n, const Syntax * repl) {
  if (n == "_") return; // "_" is a special variable meaning: don't care
  //printf("AM: x%p %p\n", NO_MATCH, repl);
  if (repl && repl->str().source)
    repl = repl->map_source(*Macro::macro_ci);
  m->push_back(Match::value_type(n, repl));
}

bool match_list(Match * m, 
                const Syntax * p, parts_iterator p_i, parts_iterator p_end, 
                const Syntax * r, parts_iterator r_i, parts_iterator r_end,
                ReplTable * rt);

// if match_prep sets p == 0 then there is nothing more to do
bool match_prep(Match * m, const Syntax * & p, const Syntax * & repl, ReplTable * rt) {
  //printf("match_prep <<: %s %s\n", ~p->to_string(), repl ? ~repl->to_string() : "<null>");
  if (p->num_args() > 0) {
    if (p->is_a("mid")) {
      p = p->arg(0);
    } else if (p->is_a("pattern")) {
      if (!repl) return true;
      p = p->arg(0);
      assert(p->what().name == repl->what().name);
      bool res = match_list(m, p, p->args_begin(), p->args_end(),
                            repl, repl->args_begin(), repl->args_end(), rt);
      p = 0;
      return res;
    } else if (p->is_a("reparse")) {
      if (!repl) {
        if (p->num_args() > 1) {
          repl = replace(p->arg(1), rt, NULL, NULL);
          //printf("REPL = %s\n" , ~repl->to_string());
        }
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
  //static unsigned dc = 0;
  //printf("match_list %d\n", dc++);
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
        r_i = r_end;
        for (flags_iterator i = with->flags_begin(), e = with->flags_end(); i != e; ++i) {
          if (!pattern->flag((*i)->what()))
            n.add_flag(*i);
        }
        r = n.build();
      }
      if (!r && now_optional)
        r = NO_MATCH;
      add_match_var(m, v, r);
      if (!r) 
        return false;
    }
    ++p_i;
    ++r_i;
  }
  if (r_i < r_end)
    return false;
  for (flags_iterator i = pattern->flags_begin(), e = pattern->flags_end(); i != e; ++i) {
    const Syntax * w = with->flag((*i)->what());
    match_parm(m, (*i)->arg(0), w ? w->arg(0) : NULL, rt);
  }
  return true;
}

Match * match(Match * orig_m, const Syntax * pattern, const Syntax * with, unsigned shift, Mark * mark) {
  Match * m = new Match();
  ReplTable * rt 
    = new ReplTable(mark, Macro::macro_ci);
  if (pattern->is_a("quasiquote")) {
    pattern = pattern->arg(0);
  }
  if (pattern->is_reparse("()") || pattern->is_reparse("[]")) {
    //printf("YES!\n");
    pattern = reparse("MATCH_LIST", pattern->inner());//, NULL, NULL, NULL, pqq);
  } else {
    //printf("NO! >>%s<<\n", ~pattern->what());
  }
  //printf("MATCH\n");
  //printf("%s %s\n", ~pattern->sample_w_loc(), ~pattern->to_string());
  //printf("%s %s\n", ~with->sample_w_loc(), ~with->to_string());
  //printf("---\n");
  if (pattern->simple()) {
    add_match_var(m, pattern->what(), with);
  } else if (!with->simple()) {
    //with = with->ensure_branch(); NOT NEEDED ANYMORE?
    bool ok = match_list(m, pattern, pattern->parts_begin(), pattern->parts_end(),
                         with, with->parts_begin() + shift, with->parts_end(), rt);
    if (!ok) return NULL;
  } else {
    return NULL;
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

extern "C" 
namespace macro_abi {

  Match * match_parts_f_(Match * m, const Syntax * pattern, const Syntax * with, Mark * mark) {
    return match(m, pattern, with, 0, mark);
  }
  
  Match * match_f_(Match * m, const Syntax * pattern, const Syntax * with, Mark * mark) {
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
      assert(!p->simple() && p->is_a("@"));
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

  Syntax * replace_w_antiquotes(Syntax * qq, Match * orig_m, Mark * mark, unsigned num, ...)
  {
    Match * m = new Match;
    va_list parms;
    va_start(parms, num);
    char buf[16];
    for (unsigned i = 0; i != num; ++i) {
      Syntax * val = va_arg(parms, const Syntax *);
      snprintf(buf, 16, "aq.%u", i);
      add_match_var(m, buf, val);
    }
    va_end(parms);
    if (orig_m) 
      m->insert(m->end(), orig_m->begin(), orig_m->end());
    
    assert(qq->is_a("quasiquote"));
    const Syntax * p = qq->arg(0);
    const Syntax * aql = qq->arg(1);

    ReplTable::AntiQuotes aqs;
    for (parts_iterator i = aql->args_begin(), e = aql->args_end(); 
         i != e; ++i)
      aqs.push_back((*i)->as_reparse()->outer_.begin);
    
    return replace(p, m, &aqs, mark);
  }

  const Syntax * match_aq_var(Match * m, unsigned idx) {
    char buf[16];
    snprintf(buf, 16, "aq.%u", idx);
    Match::const_iterator i = m->begin(), e = m->end();
    for (; i != e; ++i) {
      if (i->first == buf) {
        return i->second;
      }
    }
    return NULL;
  }

}

//
//
//

const Syntax * replace(const Syntax * p, Match * match, ReplTable::AntiQuotes * aqs, Mark * mark) {
  ReplTable * rparms 
    = new ReplTable(mark, Macro::macro_ci);
  if (match)
    rparms->table = *match;
  if (aqs)
    rparms->antiquotes = *aqs;
  //printf("tREPLACE: %s\n", ~p->to_string());
  //rparms->macro_call = Macro::macro_call;
  //rparms->macro_def = Macro::macro_def;
  const Syntax * res;
  if (p->is_reparse("{}")) {
    //res = reparse("STMTS", p->inner(), NULL, rparms);
    //if (res->num_args() == 1)
    //  res = res->arg(0);
    res = reparse_replace(SYN("@{}"), p, rparms);
  } else {
    res = replace(p, rparms, NULL, NULL);
  }
  //rparms->expand_si->outer_ip = res; // XXX
  // fixme: why am i doing this? commented it out for now
  //res->str_ = p->str();
  //printf("tREPLACE res: %s\n", ~res->to_string());
  return res;
}

const Syntax * macro_abi::replace(const UnmarkedSyntax * p, Match * match, Mark * mark) {
  return ::replace(p, match, NULL, mark);
}

struct ReplToApply {
  const Replacements * repls;
  Replacements combined_repls;
  ReplToApply() : repls() {}
  void init(const Replacements * repl, ReplTable * table, const Replacements * additional) {
    repls = combine_repl(repl, table);
    if (repls)
      combined_repls = *repls;
    if (additional)
      combined_repls.insert(combined_repls.end(), additional->begin(), additional->end());
  }
  ReplToApply(const Replacements * repl, ReplTable * table = NULL, const Replacements * additional = NULL) 
    {init(repl,table,additional);}
  const Syntax * apply(const Syntax * res) {
    if (repls) {
      // res->sample_w_loc(COUT);
      // printf("::");
      // for (Replacements::const_iterator i = repls->begin(), e = repls->end(); 
      //      i != e; ++i) 
      // {
      //   if (!(*i)->no_repl) 
      //     printf(" NORMAL-REPL");
      //   if ((*i)->no_repl && (*i)->ci)
      //     printf(" CI-ONLY");
      //   if ((*i)->qcs)
      //     printf(" QCS");
      //   printf(" / ");
      // }
      // printf("\n");
      for (Replacements::const_iterator i = repls->begin(), e = repls->end(); 
           i != e; ++i) 
      {
        combined_repls.erase(combined_repls.begin());
        if (!(*i)->no_repl)
          res = replace(res, *i, &combined_repls, NULL);
        if ((*i)->no_repl && (*i)->ci)
          res = res->map_source(*(*i)->ci);
        if ((*i)->qcs)
          res = res->map_source(*(*i)->qcs);
      }
    }
    return res;
  }
};

const Syntax * reparse_prod(String what, ReparseInfo & p, Environ * env,
                            bool match_complete_str,
                            ReplTable * r, const Replacements * additional_repls,
                            ParseAsQuasiQuote pqq)
{
  //printf("REPARSE %s AS %s\n", ~p.str.to_string(), ~what);
  //printf("....... %s\n", ~p.orig->sample_w_loc());
  ReplToApply to_apply(p.repl, r, additional_repls);
  const Syntax * res;
  res = parse_prod(what, p.str, env, &to_apply.combined_repls, p.cache, match_complete_str, pqq);
  //printf("PARSED STRING %s: %s\n", ~res->to_string(), ~res->sample_w_loc());
  res = to_apply.apply(res);
  //printf("REPARSE RES: %s: %s\n", ~res->to_string(), ~res->sample_w_loc());
  //printf("....... %s\n", ~res->sample_w_loc());
  return res;
}

const Syntax * macro_abi::reparse(const Syntax * s, const char * what, Environ * env) {
  return ::reparse(what, s->outer(), env);
}

static const Syntax * replace_mid(const Syntax * mid, const Syntax * repl, ReplTable * r, const Replacements * rs);

//static unsigned seql9 = 0;
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
  //printf("REPLACE res %d: %s %s\n", seql9++, ~res->sample_w_loc(), ~res->to_string());
  return res;
}

static const Syntax * replace(const Syntax * p, 
                              ReplTable * r, const Replacements * rs, 
                              bool * splice_r) 
{
  // FIXME: Do I need to handle the case where the entity is a symbol name?
  static unsigned seq=0;
  unsigned seql = seq++;
  //printf("REPLACE %d: %s\n", seql, ~p->to_string());
  //r->to_string(COUT, PrintFlags());
  //printf("\n");
  SymbolName mid;
  if (p->have_entity()) {
    return p;
  } else if (p->is_reparse()) {
    Syntax * res = 
      reparse_replace(p->what_part(), p, r);
    //printf("REPLACE RES %d: %s %s\n", seql, ~res->sample_w_loc(), ~res->to_string());
    return res;
  } else if (p->simple()) {
    //if (try_mid(p, SYN(SYN("mid"), p), r, rs, splice_r) {
    //  fprintf(stderr, "*** APM NEEDED: %s -> %s\n", ~p->to_string(), ~res->to_string());
    //  printf("*** APM NEEDED: %s -> %s\n", ~p->to_string(), ~res->to_string());
    //  CERR.write(error(p->str(), "NOTE: var info")->message());
    //  CERR.write(error(res->str(), "NOTE: repl info")->message());
    //  abort();
    //}
    return SYN(p, r->mark, r->expand_source_info(p));
  } else if (p->is_a("mid")/* && r->have(*p->arg(0))*/) {
    const Syntax * res = try_mid(p->arg(0), p, r, rs, splice_r);
    if (!res) {
      //printf("MID MATCH FAILED %s\n", ~p->arg(0)->to_string());
      goto def;
    }
    return res;
  } else if (p->is_a("s") || p->is_a("c") || p->is_a("n") || p->is_a("f")) {
    // This special case should no longer be needed, however, without
    // it the adding of marks causes problems
    return p->map_source(*r->ci);
  } else {
  def:
    SyntaxBuilder res;
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      //const Syntax * q = (i == 0 && p->part(0)->simple()) ? p->part(0) : replace(p->part(i), r); // HACK
      bool splice = false;
      //static int x = 0;
      //++x;
      //printf("<<%d %s\n", x, ~p->part(i)->to_string());
      const Syntax * q = replace(p->part(i), r, rs, &splice);
      if (splice) {
        //printf("??%d %s\n", x, ~q->to_string());
        if (q->simple() || !q->is_a("@")) throw unknown_error(q);
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
        r0.add_part(replace(q->arg(0), r, rs, NULL));
      res.add_flag(r0.build());
    }
    //printf("REPLACE Res %d: %s\n", seql, ~res->to_string());
    SourceStr outer = p->str();
    SourceStr inner = p->get_inner_src();
    SourceStr str;
    // If in the template the source string for the outer syntax is
    // the same the combination of the inner, don't set the source
    // string of the replacement, instead let it be derived from the
    // parts.  Important so that things like (exp (mid X)) will get
    // the source string of the replacement (i.e., (mid X)) and not
    // the template as the later is rarely what's wanted.
    if (outer.begin == inner.begin && outer.end == inner.end)
      ; // nothing to do
    else
      str = r->expand_source_info_str(p);
    return res.build(str);
  }
}

static const Syntax * reparse_replace(const Syntax * new_name, 
                                      const Syntax * p, ReplTable * r)
{
  const ReparseSyntax * rs = p->as_reparse();
  SourceStr str = r->expand_source_info_str(rs->str_);
  return new ReparseSyntax(SYN(new_name, r->mark, r->expand_source_info(p->what_part())),
                           combine_repl(rs->repl, r),
                           rs->cache, rs->parse_as,
                           rs->origin,
                           rs->outer_,
                           rs->inner_,
                           &str);
}

const Syntax * replace_context(const Syntax * p, const Marks * context);

const Syntax * replace_mid(const Syntax * mid, const Syntax * repl, ReplTable * r, const Replacements * rs) {
  if (!repl->simple() && repl->is_a("@")) {
    SyntaxBuilder res(repl->part(0));
    for (parts_iterator i = repl->args_begin(), e = repl->args_end(); i != e; ++i) {
      res.add_part(replace_mid(mid, *i, r, rs));
    }
    for (flags_iterator i = repl->flags_begin(), e = repl->flags_end(); i != e; ++i) {
      const Syntax * q = *i;
      SyntaxBuilder r0(q->part(0)); // FIXME: I think I need to do more with first part
      for (unsigned j = 0; j != q->num_args(); ++j)
        r0.add_part(replace_mid(mid, q->arg(j), r, rs));
      res.add_flag(r0.build());
    }
    return res.build(repl->str());
  } else {
    if (mid->num_args() > 1) {
      String what = mid->arg(1)->as_symbol_name().name;
      if (repl->is_reparse("parm") || repl->is_reparse("()")) { // FIXME: Just check for is_reparse()?
        // const Replacements * repls = repl->as_reparse()->repl;
        // if (repls) {
        //   for (Replacements::const_iterator i = repls->begin(), e = repls->end(); 
        //        i != e; ++i) 
        //   {
        //     // FIXME: This seams like the correct thing to do,
        //     //        however, I have no idea if it is.....
        //     (*i)->outer_si = cs.new_;
        //   }
        // }
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
  } else if (p->is_reparse()) {
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

extern "C" 
namespace macro_abi {

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

extern "C" 
namespace macro_abi {
  
  const UnmarkedSyntax * string_to_syntax(const char * str) {
    return parse_str("SYNTAX_STR", SourceStr(new SourceBlock(SubStr(str))));
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
  if (!p->is_reparse("()")) return NULL;
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
  const Syntax * o = *i;
  if (o->is_reparse("[]") || o->is_reparse("()")) {
    ++i;
    return SYN(p, o->what_part());
  }
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
    //printf("YES %s\n", ~p->to_string());
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
  //printf("\n>expand>%s//\n", ~what);
  //p->str().sample_w_loc(COUT);
  //COUT << "\n";
  //p->print();
  //printf("\n////\n");
  
  if (p->is_reparse()) {

    if (Syntax * r = p->as_reparse()->instantiate_no_throw())
      return partly_expand(r, pos, env, flags);

    SymbolName what = p->as_reparse()->rwhat();

    if (what == "{}") {
      if (pos == ExpPos)
        return partly_expand(reparse("INIT", p->inner(), &env), pos, env, flags);
      else
        return partly_expand(reparse("BLOCK", p->outer(), &env), pos, env, flags);
    } else if (what == "@{}" && !(flags & EXPAND_NO_BLOCK_LIST)) {
      ReparseInfo r = p->inner();
      const Syntax * p0 = pos == ExpPos 
        ? reparse_prod("EXP_", r, &env)
        : reparse_prod("STMTE", r, &env);
      if (r.str.empty()) {
        const_cast<Syntax *>(p0)->str_ = p->str();
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
    } else {
      return p;
    }
  }

  SymbolName what = p->what();
  if (what == "class") { // XXX: A Bit of a hack, there needs to be a
                         // better way to handle "class Module::X {...}", 
                         // that is defining a class outside the module.
    if (p->arg(0)->is_a("::")) {
      SyntaxBuilder tmp;
      tmp.add_part(p->part(0));
      tmp.add_part(p->arg(0)->arg(1));
      tmp.add_parts(p->args_begin() + 1, p->args_end());
      tmp.set_flags(p->flags_begin(), p->flags_end());
      return partly_expand(SYN(SYN("memberdecl"), 
                               partly_expand(p->arg(0)->arg(0), NoPos, env, flags), 
                               tmp.build(p->str())),
                           pos, env, flags);
    }
  }

  if (p->simple() && pos == NoPos) {
    return p;
  } if (p->simple() && asc_isdigit(what.name[0])) {
    return SYN(p->str(), NUM, p);
  } else if (is_raw_id(p)) {
    return partly_expand(SYN(p->str(), ID, p), pos, env, flags);
  } else if (Syntax * n = try_syntax_macro_or_primitive(what, p, env)) {
    return partly_expand(n, pos, env, flags);
  } else if (what == "id") {
    //printf("EXPANDING ID %s\n", ~p->to_string());
    p = expand_id(p, env);
    if (!(flags & EXPAND_NO_ID_MACRO_CALL)) { 
      // NOTE: ID macros can have flags, since there is no parameter list
      //       they are passed in as flags as (id <id> :(flag1 val1))
      assert_num_args(p, 1);
      const Syntax * n = p;
      const Macro * m = ast::find_overloaded_symbol<Macro>(Overloadable::AS_ID, n->arg(0), &env);
      if (m) { // id macro
        //printf("ID MACRO HIT %s\n", ~n->arg(0)->to_string());
        Syntax * a = SYN(PARTS(SYN(".")), FLAGS(p->flags_begin(), p->flags_end()));
        p = m->expand(p, a, env);
        //printf("ID MACRO HIT res: %s\n", ~p->to_string());
        return partly_expand(p, pos, env, flags);
      } else {
        //if (n->arg(0)->what().name == "data_") {
        //printf("ID MACRO FAILED %s\n", ~n->arg(0)->to_string());
          //abort();
          //}
      }
    }
  } else if (what == "call") {
    assert_num_args(p, 2);
    //printf("EXPANDING CALL %s\n", ~p->to_string());
    const Syntax * n = partly_expand(p->arg(0), OtherPos, env, flags | EXPAND_NO_ID_MACRO_CALL);
    const Syntax * a = p->arg(1);
    //printf("expanding call name: %s\n", ~n->to_string());
    if (a->is_reparse("()")) a = reparse("SPLIT", a->inner(), &env);
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
        Syntax * res = SYN(SYN("anon"), SYN(t), SYN("."), a);
        return res;
      }
    }
    Syntax * res = SYN(p->str(), PARTS(p->part(0), p->arg(0), a));
    p = res;
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
  } else if (what == "(...)") {
    const Syntax * type = parse_decl_->parse_type(p, env);
    if (type) return SYN(p->str(), SYN(".type"), type);
    p = e_parse_exp(p, env, "seq");
    return partly_expand(p, pos, env, flags);
  } else if (what == "<@") {
    //printf("GOT A <@: %s\n", ~p->to_string());
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
  
  if (p->is_reparse()) {
    if (Syntax * r = p->as_reparse()->instantiate_no_throw())
      return limited_expand(r, env);      
    SymbolName what = p->as_reparse()->rwhat();
    if (what == "parm") {
      return limited_expand(reparse("PARM_", p->outer(), &env), env);
    } else {
      return p;
    }
  }

  SymbolName what = p->what();
  p = expand_id(p, env);
  if (p->simple()) {
    return p;
  } else if (const Syntax * n = try_syntax_macro_or_primitive(what, p, env)) {
    return limited_expand(n, env);
  } else if (what == "(...)") {
    const Syntax * type = parse_decl_->parse_type(p, env);
    if (type) return SYN(p->str(), SYN(".type"), type);
    p = e_parse_exp(p, env, "seq");
    return limited_expand(p, env);
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
    res = partly_expand(res, pos, *env/*, EXPAND_NO_BLOCK_LIST*/);
    if (!res->simple() && res->is_a("@")) {
      stack.push_back(top);
      top = new SimpleSyntaxEnum(res->args_begin(), res->args_end());
      goto try_again;
      //} else if (res->is_a("@{}")) {
      //stack.push_back(top);
      //const Syntax * r = reparse("STMTS", res->inner(), env);
      //top = new SimpleSyntaxEnum(r->args_begin(), r->args_end());
      //goto try_again;
    }
    return res;
  }
  SyntaxEnum * clone() const {
    return new PartlyExpandSyntaxEnum(*this);
  }
};

SyntaxEnum * partly_expand_list(const Syntax * p, Position pos, Environ & env) {
  return new PartlyExpandSyntaxEnum(new SimpleSyntaxEnum(p->args_begin(), p->args_end()), pos, &env);
}

extern "C" 
namespace macro_abi {
  
  SyntaxEnum * partly_expand_list(SyntaxEnum * l, Position pos, Environ * env) {
    return new PartlyExpandSyntaxEnum(l, pos, env);
  }

  Syntax * expand_id(Syntax * p, Environ * env) {
    return ::expand_id(p, *env);
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
  if (Syntax * r = instantiate(p))
    p = r;
  if (p->simple()) {
    return SymbolKey(*p, ns);
  } else if (p->is_a("fluid")) {
    assert_num_args(p, 1);
    const FluidBinding * b = env.symbols.lookup<FluidBinding>(p->arg(0), ns);
    return SymbolKey(b->rebind, ns);
  } else if (p->is_a("operator")) {
    return SymbolKey(*p->arg(0), ns ? ns : OPERATOR_NS);
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
    throw error(p, "Unsupported Binding Form: %s\n", ~p->to_string());
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

static void expand_fun_parms(const Syntax * args, Environ & env, SyntaxBuilder & res, 
                             unsigned & num_required,  bool required = true) {
  //printf("EXPAND_FUN_PARMS: %s %d\n", ~args->to_string(), args->num_parts());
  for (unsigned i = 0; i != args->num_args(); ++i) {
    const Syntax * p = args->arg(i);
    if (p->is_a("@") && !p->simple()) {
      expand_fun_parms(p, env, res, num_required, required);
    } else {
      if (Syntax * r = instantiate(p))
        p = r;
      else if (p->is_reparse("(...)"))
        p = reparse("TOKENS", p->inner(), &env);
      //printf("FUN_PARM: %s\n", ~p->to_string());
      if (p->eq("@")) {
        //printf("GOT @\n");
        required = false;
      } else if (p->is_a("...")) {
        res.add_part(p);
        required = false;
      } else {
        const Type * t = parse_type(p->part(0), env);
        // an array of x as parm is really a pointer to x:
        if (const Array * a = dynamic_cast<const Array *>(t->root))
          t = env.types.inst(".ptr", a->subtype);
        Syntax * r = SYN(p->part(0)->str(), t);
        if (p->num_parts() == 2) 
          r = SYN(r, p->part(1));
        res.add_part(r);
        if (required) ++num_required;
      }
    }
  }
}

Tuple * expand_fun_parms(const Syntax * parse, Environ & env) {
  if (parse->is_a("(...)")) 
    parse = parse_decl_->parse_fun_parms(parse, env);
  //printf("FUN_PARMS: %s\n", ~parse->to_string());
  SyntaxBuilder res(parse->part(0));
  unsigned num_required = 0;
  expand_fun_parms(parse, env, res, num_required);
  Type * type = parse_type(res.build(), env);
  Tuple * tuple = dynamic_cast<Tuple *>(type);
  tuple->required = num_required;
  //printf("YES %d\n", tuple->required);
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
  deps.insert(find_overloaded_symbol<TopLevelVarDecl>(NULL, NULL, env.find_tls("ct_malloc_atomic"), NULL));
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

struct Syntaxes {
  const char * what; 
  const char * str;
  const char * parse_as;
  unsigned len;
  unsigned inner_offset;
  unsigned inner_len;
  const Syntax * syn;
};

AbiInfo ast::DEFAULT_ABI_INFO = {"default", NULL, NULL, NULL, NULL};

Vector<AbiInfo> ast::abi_list;
AbiInfo * ast::get_abi_info(String name) {
  AbiInfo * res = NULL;
  Vector<AbiInfo>::iterator i = ast::abi_list.begin(), e = ast::abi_list.end();
  for (; i != e; ++i) {
    if (i->abi_name == name) res = &*i;
  }
  return res;
}
extern "C" AbiInfo * environ_get_abi_info(const Environ * env) {
  return env->abi_info;
}


void load_macro_lib(ParmString lib, Environ & env) {
  printf("LOADING: %s\n", lib.str());
  void * lh = dlopen(lib, RTLD_NOW | RTLD_GLOBAL);
  if (!lh) {
    fprintf(stderr, "ERROR: %s\n", dlerror());
    abort();
  }
  void * abi_list_size_0 = dlsym(lh, "_abi_list_size");
  if (abi_list_size_0) {
    unsigned abi_list_size = *(unsigned *)abi_list_size_0;
    AbiInfo * i = (AbiInfo *)dlsym(lh, "_abi_list");
    AbiInfo * e = i + abi_list_size;
    for (; i != e; ++i) {
      //printf(stderr, "FOUND ALT MANGLER %s!\n", i->abi_name);
      AbiInfo * existing = ast::get_abi_info(i->abi_name);
      bool find_module = false;
      if (existing) {
        if (i->mangler) {
          assert(!existing->mangler); // FIXME: Error message
          existing->mangler = i->mangler;
        }
        if (i->parse_class) {
          assert(!existing->parse_class); // FIXME: Error message
          existing->parse_class = i->parse_class;
        }
        if (i->module_name) {
          assert(!existing->module_name);
          existing->module_name = i->module_name;
          find_module = true;
        }
      } else {
        ast::abi_list.push_back(*i);
        existing = &ast::abi_list.back();
      }
      if (find_module) {
        //printf("*** LOOKING FOR MODULE: %s\n", existing->module_name);
        existing->module = find_symbol<Module>(SymbolKey(existing->module_name, OUTER_NS), 
                                               env.symbols.front);
        //printf("*** RES: %p\n", existing->module);
      }
    }
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
  
  unsigned * syntaxes_size = (unsigned *)dlsym(lh, "_syntaxes_size");
  if (syntaxes_size && *syntaxes_size > 0) {
    Syntaxes * i = (Syntaxes *)dlsym(lh, "_syntaxes");
    Syntaxes * e = i + *syntaxes_size;
    for (; i != e; ++i) {
      SourceBlock * block = new SourceBlock(SubStr(i->str, i->str + i->len));
      if (i->what && strcmp(i->what, "quasiquote") == 0) {
        i->syn = parse_str_as_quasiquote(i->parse_as, SourceStr(block));
      } else if (i->what) {
        i->syn = new ReparseSyntax(SYN(i->what), NULL, NULL, 
                                   i->parse_as ? String(i->parse_as) : String(),
                                   String(), 
                                   SourceStr(block),
                                   SourceStr(&block->base_info,
                                             i->str + i->inner_offset,
                                             i->str + i->inner_offset + i->inner_len));
      } else {
        i->syn = parse_str(i->parse_as, SourceStr(block));
      }
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

Stmt * parse_kill_fluid(const Syntax * p, Environ & env) {
  assert_num_args(p, 1);
  SymbolKey sym = expand_binding(p->arg(0), env);
  FluidBinding * b = env.symbols.lookup<FluidBinding>(sym, p->arg(0)->str());
  env.add(SymbolKey(b->rebind, sym.ns), new NoSymbol());
  return empty_stmt();
}

//
// 
//

extern "C" 
namespace macro_abi {
  
  typedef ast::UserType UserType;
  typedef ast::Module Module;
  
  const UserType * user_type_info(const Syntax * s, Environ * env) {
    // first see if we have a symbol name
    s = expand_id(s, *env);
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
    SyntaxEnum * clone() const {
      return new ModuleSymbolsEnum(*this);
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

struct PointerEntity {
  typedef ::TypeInfo<PointerEntity> TypeInfo;
};

extern "C" { // brace so gcc doesn't complain about that static function
namespace macro_abi {

  Syntax * make_mid(Syntax * p) {
    return SYN(SYN("mid"), p);
  }

  size_t ct_value(const Syntax * p, Environ * env) {
    Exp * ast = parse_exp(p, *env);
    return ast->ct_value<size_t>();
  }
  
  const Syntax * error(Syntax * p, const char * fmt, ...) {
    SourceStr str = p ? p->str() : SourceStr();
    va_list ap;
    va_start(ap, fmt);
    Error * res = new Error(str, fmt, ap);
    va_end(ap);
    return SYN(p->str(), res);
  }

  static Syntax * prep_id(const Syntax * p, Environ * env) {
    if (p->what() == "parm") 
      p = reparse("ID", p->outer(), env);
    if (p->what() == "id")
      p = p->arg(0);
    return p;
  }

  const Syntax * get_symbol_prop(const Syntax * sym, const Syntax * prop, Environ * env) {
    sym = prep_id(sym, env);
    prop = prep_id(prop, env);
    return lookup_fancy_symbol<Symbol>(sym, NULL, *env)->get_prop(*prop);
  }

  Syntax * stash_ptr_in_syntax(void * ptr) {
    return SYN(SourceStr(), static_cast<PointerEntity *>(ptr));
  }

  void * extract_ptr_from_syntax(Syntax * syn) {
    return syn->entity<PointerEntity>();
  }

  const char * mangle_name(Syntax * p, Environ * env) {
    SymbolKey name = expand_binding(p, *env);
    StringBuf buf;
    asm_name(&name, buf);
    return buf.freeze();
  }

  const char * mangle_fun_parms(Syntax * p, Environ * env) {
    Tuple * parms = expand_fun_parms(p, *env);
    StringBuf buf;
    for (unsigned i = 0; i != parms->parms.size(); ++i) {
      mangle_print_inst->to_string(*parms->parms[i].type, buf);
    }
    return ~buf.freeze();
  }

  Syntax * stringify(Syntax * p) {
    return SYN(SYN("s"), SYN(p->to_string()));
  }

}}

extern "C" void gdb_breakpoint() {}

extern "C" {

  void * ct_malloc(size_t size) {
    return GC_MALLOC(size);
  }

  void * ct_malloc_atomic(size_t size) {
    return GC_MALLOC_ATOMIC(size);
  }

  void ct_free(void * ptr) {
    return GC_FREE(ptr);
  }

}

//
//
//

