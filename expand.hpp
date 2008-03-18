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
};

using ast::Environ;
using ast::SymbolName;

ast::AST * expand_top(const Syntax * p, Environ &);
void read_macro(const Syntax * p, Environ &);

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
  const ast::Mark * mark;
  void print() const {
//    abort();
//#if 0
    printf("{");
    for (Table::const_iterator i = table.begin(), e = table.end(); i != e; ++i) {
      printf("%s=>", ~i->first.to_string());
      i->second->print();
      printf(",");
    }
    printf("}");
//#endif
  }
};

struct Replacements : public Vector<ReplTable *> {
  bool anywhere(String s) const {
    for (const_iterator i = begin(), e = end(); i != e; ++i)
      if ((*i)->have(s)) return true;
    return false;
  }
  void print() const {
    for (const_iterator i = begin(), e = end(); i != e; ++i)
      (*i)->print();
  }
};

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
ast::AST * expand(const Syntax * p, Position pos, Environ & env);
ast::Type * expand_type(const Syntax * p, Environ & env);
ast::Tuple * expand_fun_parms(const Syntax * parse, Environ & env);

static inline ast::AST * expand_top_level(const Syntax * p, Environ & env) {
  return expand(p, TopLevel, env);
}
static inline ast::AST * expand_member(const Syntax * p, Environ & env) {
  return expand(p, FieldPos, env);
}
static inline ast::AST * expand_stmt(const Syntax * p, Environ & env) {
  return expand(p, StmtPos, env);
}
static inline ast::AST * expand_stmt_decl(const Syntax * p, Environ & env) {
  return expand(p, StmtDeclPos, env);
}
static inline ast::AST * expand_exp(const Syntax * p, Environ & env) {
  return expand(p, ExpPos, env); 
}

const Syntax * reparse(String what, const Syntax * p, ReplTable * r = 0);

#endif
