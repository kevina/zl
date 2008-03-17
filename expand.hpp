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

ast::AST * expand_top(const Parse * p, Environ &);
void read_macro(const Parse * p, Environ &);

// misnamed, now replaces and marks
struct ReplTable : public gc_cleanup {
  typedef Vector<std::pair<SymbolName, const Parse *> > Table;
  Table table;
  const Parse * lookup(SymbolName n) const {
    for (Table::const_iterator i = table.begin(); i != table.end(); ++i) {
      if (i->first == n) return i->second;
    }
    return NULL;
  }
  bool have(SymbolName n) const {
    return lookup(n);
  }
  const Parse * lookup(String n) const {
    for (Table::const_iterator i = table.begin(); i != table.end(); ++i) {
      if (i->first.name == n) return i->second;
    }
    return NULL;
  }
  bool have(String n) const {
    return lookup(n);
  }
  const Parse * lookup(const Parse & n) const {
    return lookup(static_cast<const SymbolName &>(n));
  }
  bool have(const Parse & n) const {
    return have(static_cast<const SymbolName &>(n));
  }
  void insert(SymbolName n, const Parse * p) {
    table.push_back(std::pair<SymbolName, const Parse *>(n,p));
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
ast::AST * expand(const Parse * p, Position pos, Environ & env);
ast::Type * expand_type(const Parse * p, Environ & env);
ast::Tuple * expand_fun_parms(const Parse * parse, Environ & env);

static inline ast::AST * expand_top_level(const Parse * p, Environ & env) {
  return expand(p, TopLevel, env);
}
static inline ast::AST * expand_member(const Parse * p, Environ & env) {
  return expand(p, FieldPos, env);
}
static inline ast::AST * expand_stmt(const Parse * p, Environ & env) {
  return expand(p, StmtPos, env);
}
static inline ast::AST * expand_stmt_decl(const Parse * p, Environ & env) {
  return expand(p, StmtDeclPos, env);
}
static inline ast::AST * expand_exp(const Parse * p, Environ & env) {
  return expand(p, ExpPos, env); 
}

const Parse * reparse(String what, const Parse * p, ReplTable * r = 0);

#endif
