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
typedef Environ ExpandEnviron;

ast::AST * expand_top(const Parse * p);
void read_macro(const Parse * p);

struct ReplTable : public hash_map<String, const Parse *> {
  const Parse * lookup(String n) const {
    const_iterator i = find(n);
    if (i == end()) return 0;
    else return i->second;
  }
  void print() const {
    abort();
#if 0
    printf("{");
    for (const_iterator i = begin(), e = end(); i != e; ++i) {
      printf("%s=>", ~i->first);
      i->second->print();
      printf(",");
    }
    printf("}");
#endif
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
