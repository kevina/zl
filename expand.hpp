#ifndef EXPAND__HPP
#define EXPAND__HPP

#include "symbol_table.hpp"
#include "parse.hpp"
#include "hash.hpp"
#include "vector.hpp"

namespace ast {
  struct Environ;
  struct AST;
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

// misnamed, now replaces and marks
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
  const Syntax * macro_call;
  const Syntax * macro_def;
  const ast::Mark * mark;
  struct CacheItem {
    const SourceInfo * key;
    const ExpandSourceInfo * value;
  };
  Vector<CacheItem> cache;
  inline const ExpandSourceInfo * expand_source_info(const SourceInfo * s);
  inline const ExpandSourceInfo * expand_source_info(const Syntax * s);
  inline SourceStr expand_source_info_str(const SourceStr & str);
  inline SourceStr expand_source_info_str(const Syntax * s);
  void to_string(OStream & o, PrintFlags f) const {
    o.printf("{");
    //o.printf("...");
    for (Table::const_iterator i = table.begin(), e = table.end(); i != e; ++i) {
      o.printf("%s", ~i->first.to_string());
      //o.printf("%s=>", ~i->first.to_string());
      //i->second->to_string(o);
      o.printf(",");
    }
    o.printf("}");
  }
};

struct Replacements : public Vector<ReplTable *> {
  bool anywhere(String s) const {
    for (const_iterator i = begin(), e = end(); i != e; ++i)
      if ((*i)->have(s)) return true;
    return false;
  }
  void to_string(OStream & o, PrintFlags f) const {
    for (const_iterator i = begin(), e = end(); i != e; ++i)
      (*i)->to_string(o, f);
  }
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

enum Position {NoPos = 0, OtherPos = 1, TopLevel = 2, FieldPos = 4, 
               StmtDeclPos = 8, StmtPos = 16, ExpPos = 32};
ast::Tuple * expand_fun_parms(const Syntax * parse, Environ & env);

static const unsigned EXPAND_NO_ID_MACRO_CALL = 1;
static const unsigned EXPAND_NO_FUN_MACRO_CALL = 2;
static const unsigned EXPAND_NO_MACRO_CALL = 1 | 2;
const Syntax * partly_expand(const Syntax * p, Position pos, Environ & env, unsigned flags = 0);

ast::SymbolKey expand_binding(const Syntax * p, const ast::InnerNS * ns, Environ & env);
static inline ast::SymbolKey expand_binding(const Syntax * p, Environ & env) {
  return expand_binding(p, ast::DEFAULT_NS, env);
}

const Syntax * reparse(String what, const Syntax * p, ReplTable * r = NULL, 
                       const Replacements * additional_repls = NULL);

ast::AST * parse_map(const Syntax * p, Environ & env);
ast::AST * parse_macro(const Syntax * p, Environ & env);
ast::AST * parse_fluid_binding(const Syntax *, Environ &);

void assert_num_args(const Syntax * p, unsigned num);
void assert_num_args(const Syntax * p, unsigned min, unsigned max);

String gen_sym();

const Syntax * flatten(const Syntax * p);

class SyntaxSourceInfo : public SourceInfo {
public:
  //const MacroSymbol * macro;
  const Syntax * syntax;
  SyntaxSourceInfo(const Syntax * s) : syntax(s) {}
  const SourceFile * file() const {return syntax->str().source->file();}
  const SourceInfo * block() const {return this;}
  const SourceInfo * parent() const {return syntax->str().source;}
  void dump_info(OStream &, const char * prefix) const;
};

template <>
struct ChangeSrc<SyntaxSourceInfo> {
  const SyntaxSourceInfo * cache;
  ChangeSrc(const Syntax * s) : cache(new SyntaxSourceInfo(s)) {}
  const SourceInfo * operator() (const SourceInfo * o) {
    if (o == cache->syntax->str().source) return cache;
    else return o;
  }
};

#endif
