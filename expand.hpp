#ifndef EXPAND__HPP
#define EXPAND__HPP

#include "symbol_table.hpp"
#include "hash.hpp"
#include "vector.hpp"
#include "peg.hpp"

namespace ast {
  struct Environ;
  struct AST;
  struct Stmt;
  class TypeSymbol;
  class TypeInst;
  typedef TypeInst Type;
  class Tuple;
  struct InnerNS;
};

using ast::Environ;
using ast::SymbolName;

void read_macro(const Syntax * p, Environ &);

struct ExpandSourceInfo;

struct ReplTable;

struct Replacements : public Vector<ReplTable *> {
  bool anywhere(String s) const;
  unsigned lookup_antiquote(const void * key) const;
  void to_string(OStream & o, PrintFlags f, SyntaxGather * = NULL) const;
  String to_string() const {
    StringBuf buf;
    to_string(buf, PrintFlags());
    return buf.freeze();
  }
};

static inline const Replacements * combine_repl(const Replacements * rs, const Replacements * r) {
  if (rs && r) {
    Replacements * res = new Replacements(*rs);
    res->insert(res->end(), r->begin(), r->end());
    return res;
  } else if (rs) {
    return rs;
  } else if (r) {
    return r;
  } else {
    return NULL;
  }
}

static inline const Replacements * combine_repl(const Replacements * rs, ReplTable * r) {
  if (rs && r) {
    Replacements * res = new Replacements(*rs);
    res->push_back(r);
    return res;
  } else if (rs) {
    return rs;
  } else if (r) {
    Replacements * res = new Replacements();
    res->push_back(r);
    return res;
  } else {
    return NULL;
  }
}

ast::Tuple * expand_fun_parms(const Syntax * parse, Environ & env);
void expand_template_parms(const Syntax *, SyntaxBuilder &, Environ & env);
const Syntax * expand_template_id(const Syntax * p, Environ & env);
SymbolName flatten_template_id(const Syntax * p, Environ & env);
const Syntax * expand_id(const Syntax * p, Environ & env);

enum Position {NoPos = 0, OtherPos = 1, TopLevel = 2, FieldPos = 4, 
               StmtDeclPos = 8, StmtPos = 16, ExpPos = 32};
static const unsigned EXPAND_NO_ID_MACRO_CALL = 1;
static const unsigned EXPAND_NO_FUN_MACRO_CALL = 2;
static const unsigned EXPAND_NO_MACRO_CALL = 1 | 2;
static const unsigned EXPAND_NO_BLOCK_LIST = 4;
const Syntax * partly_expand(const Syntax * p, Position pos, Environ & env, unsigned flags = 0);
const Syntax * limited_expand(const Syntax * p, Environ & env); // used in parse_decl

ast::SymbolKey expand_binding(const Syntax * p, const ast::InnerNS * ns, Environ & env);
static inline ast::SymbolKey expand_binding(const Syntax * p, Environ & env) {
  return expand_binding(p, NULL, env);
}

const Syntax * reparse_prod(String what, ReparseInfo & p, Environ * env = NULL,
                            bool match_complete_str = false, 
                            ReplTable * = NULL, 
                            const Replacements * additional_repls = NULL,
                            ParseAsQuasiQuote = ParseAsQuasiQuote());
static inline const Syntax * reparse(String what, ReparseInfo p, Environ * env = NULL,
                                     ReplTable * r = NULL, 
                                     const Replacements * additional_repls = NULL,
                                     ParseAsQuasiQuote = ParseAsQuasiQuote()) 
{
  const Syntax * res = reparse_prod(what, p, env, true, r, additional_repls);
  const_cast<Syntax *>(res)->str_ = p.orig->str();
  return res;
}

ast::Stmt * parse_map(const Syntax * p, Environ & env);
ast::Stmt * parse_macro(const Syntax * p, Environ & env);
ast::Stmt * parse_fluid_binding(const Syntax *, Environ &);
ast::Stmt * parse_kill_fluid(const Syntax *, Environ &);

void assert_num_args(const Syntax * p, unsigned num);
void assert_num_args(const Syntax * p, unsigned min, unsigned max);

String gen_sym();

const Syntax * flatten(const Syntax * p);

void load_macro_lib(ParmString lib, Environ & env);

// class SyntaxSourceInfo : public SourceInfo {
// public:
//   const Syntax * syntax;
//   SyntaxSourceInfo(const Syntax * s) : SourceInfo(s->str().source), syntax(s) {}
//   SyntaxSourceInfo(const SourceInfo * p, const Syntax * s) : SourceInfo(p), syntax(s) {}
//   const SourceInfo * block() const {return this;}
//   const SourceInfo * clone_until(const SourceInfo * stop, 
//                                  const SourceInfo * new_parent) const;
//   bool dump_info_self(OStream &) const;
// };

// template <>
// struct ChangeSrc<SyntaxSourceInfo> {
//   const SyntaxSourceInfo * cache;
//   ChangeSrc(const Syntax * s) : cache(new SyntaxSourceInfo(s)) {}
//   const SourceInfo * operator() (const SourceInfo * o) {
//     if (o == cache->syntax->str().source) return cache;
//     else return o;
//   }
// };


struct MapSource_QuotedSyntax {
  SourceStr box;
  MapSource_QuotedSyntax(const SourceStr & s) : box(s) {}
  struct CacheItem {const SourceFile * key; const SourceInfo * value;};
  typedef Vector<CacheItem> Cache;
  Cache cache;
  const SourceInfo * f(const SourceInfo * o) {
    if (!o) return NULL;
    CacheItem item;
    item.key = o->file();
    for (Cache::const_iterator i = cache.begin(), e = cache.end(); i != e; ++i)
      if (i->key == item.key) return i->value;
    SourceBlock * block = new SourceBlock(item.key, box);
    item.value = &block->base_info;
    cache.push_back(item);
    return item.value;
  }
  MapSourceRes operator() (const Syntax * other) {
    return MapSourceRes(f(other->str_.source));
  }
  MapSourceRes operator() (const ReparseSyntax * other, const Replacements * repl);
};

struct SyntaxEnum {
  virtual const Syntax * next() = 0;
  virtual SyntaxEnum * clone() const = 0;
  virtual ~SyntaxEnum() {}
};
SyntaxEnum * partly_expand_list(const Syntax * p, Position pos, Environ & env);

static inline const bool is_raw_id(const Syntax * p) 
{
  return (p->simple() || p->is_a("fluid") || p->is_a("`") || p->is_a("::")) || p->is_a("tid");
}

static inline const Syntax * try_id(const Syntax * p)
{
  return is_raw_id(p) ? p : p->is_a("id") ? p->arg(0) : NULL;
}

const Syntax * handle_operator_fun_id(parts_iterator & i, 
                                      parts_iterator e,
                                      Environ & env);

const Syntax * handle_w_tilda(parts_iterator & i, 
                              parts_iterator e,
                              Environ & env);

const Syntax * handle_unparsed_scope_op(parts_iterator & i,
                                        parts_iterator e,
                                        Environ & env);

const Syntax * expand_macro(const Syntax *, const ast::Symbol *, 
                            const Vector<ast::Exp *> &, Environ & env);

#endif
