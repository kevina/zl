#ifndef EXPAND__HPP
#define EXPAND__HPP

#include "symbol_table.hpp"
#include "parse.hpp"
#include "hash.hpp"
#include "vector.hpp"

enum SymbolType {VarSym, TypeSym};
typedef AST::SymbolTable<SymbolType> ExpandSymbolTable;

struct ExpandEnviron : public gc {
  ExpandSymbolTable * symbols;
  ExpandEnviron() {
    symbols = new ExpandSymbolTable;
  }
  ExpandEnviron new_scope() {
    ExpandEnviron env = *this;
    env.symbols  = new ExpandSymbolTable(env.symbols);
    return env;
  }
};

const Parse * expand_top(const Parse * p);
const Parse * read_macro(const Parse * p);

struct ReplTable : public hash_map<String, const Parse *> {
  const Parse * lookup(String n) const {
    const_iterator i = find(n);
    if (i == end()) return 0;
    else return i->second;
  }
  void print() const {
    printf("{");
    for (const_iterator i = begin(), e = end(); i != e; ++i) {
      printf("%s=>", ~i->first);
      i->second->print();
      printf(",");
    }
    printf("}");
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

static inline Replacements * combine_repl(Replacements * rs, ReplTable * r) {
  Replacements * res = 0;
  if (rs && r) {
    res = new Replacements(*rs);
    res->push_back(r);
  } else if (rs) {
    res = rs;
  } else if (r) {
    res = new Replacements();
    res->push_back(r);
  }
  return res;
}

const Parse * reparse(String what, const Parse * p, ReplTable * r = 0);

#endif
