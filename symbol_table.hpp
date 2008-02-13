#ifndef SYMBOL_TABLE__HPP
#define SYMBOL_TABLE__HPP

#include <utility>
#include "hash.hpp"

struct Parse;

namespace ast {

  struct NameSpace {
    String name;
    NameSpace(String n) : name(n) {}
  };

  extern const NameSpace * DEFAULT_NS;
  extern const NameSpace * TAG_NS;

  struct SymbolKey : public std::pair<const NameSpace *, String> {
    typedef std::pair<const NameSpace *, String> Base;
    SymbolKey() : Base() {}
    SymbolKey(String n) : Base(DEFAULT_NS, n) {}
    SymbolKey(const char * n) : Base(DEFAULT_NS, n) {}
    inline SymbolKey(const Parse & p); 
    SymbolKey(const NameSpace * ns, String n) : Base(ns, n) {}
  };

#ifdef PARSE__HPP
  inline SymbolKey::SymbolKey(const Parse & p) : Base(DEFAULT_NS, p) {}
#endif

  template <typename V>
  class SymbolTable : public gc_cleanup
  {
  public:
    typedef V Value;
    SymbolTable * const root;
    SymbolTable * const parent;
    bool is_root() {return root == this;}
  public: // but don't use
    typedef hash_map<SymbolKey, Value> Symbols;
    Symbols symbols;
  public:
    SymbolTable(SymbolTable * p = 0) 
      : root(p ? p->root : this), parent(p) {}
    SymbolTable * pop() {
      SymbolTable * r = parent;
      return r;
    }
    void add(SymbolKey k, const Value & v) {
      symbols[k] = v;
    }
    //void add(const NameSpace * ns, String n, const Value v) {
    //  add(SymbolKey(ns,n), v);
    //}
    //void add(String k, const Value v) {
    //  add(SymbolKey(DEFAULT_NS, k), v);
    //}
    bool exists(SymbolKey k) const {
      typename Symbols::const_iterator i = symbols.find(k);
      if (i != symbols.end()) return true;
      if (parent) return parent->exists(k);
      else return false;
    }
    //bool exists(const NameSpace * ns, String n) const {
    //  return exists(SymbolKey(ns,n));
    //}
    //bool exists(String n) const {
    //  return exists(SymbolKey(DEFAULT_NS, n));
    //}
    Value & lookup(SymbolKey k) const {
      typename Symbols::const_iterator i = symbols.find(k);
      if (i != symbols.end()) return i->second;
      if (parent) return parent->lookup(k);
      else abort();
    }
    //Value & lookup(const NameSpace * ns, String n) {
    //  return lookup(SymbolKey(ns,n));
    //}
    //Value & lookup(String n) {
    //  return lookup(SymbolKey(DEFAULT_NS,n));
    //}
  };

}

template <> struct hash<ast::SymbolKey>   {
  unsigned long operator()(const ast::SymbolKey & k) const {
    return ((unsigned long)k.first) ^ hash<String>()(k.second);
  }
};

#endif
