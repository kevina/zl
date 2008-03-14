#ifndef SYMBOL_TABLE__HPP
#define SYMBOL_TABLE__HPP

#include <utility>
#include "hash.hpp"
#include "parse.hpp"

namespace ast {

  static const unsigned DEFAULT_NS = 0;
  static const unsigned TAG_NS = 2;
  static const unsigned LABEL_NS = 3;

  struct SymbolKey {
    String name;
    unsigned ns;
    SymbolKey(const char * n, unsigned ns0 = 0)
      : name(n), ns(ns0) {}
    SymbolKey(String n = String(), unsigned ns0 = 0)
      : name(n), ns(ns0) {}
    SymbolKey(const Parse & p) : name(p), ns() {}
  };

  struct Symbol {
    virtual ~Symbol() {}
  };

  struct SymbolNode {
    SymbolKey key;
    const Symbol * value;
    SymbolNode * next;
    SymbolNode(const SymbolKey & k, const Symbol * v, SymbolNode * n) 
      : key(k), value(v), next(n) {}
  };

  static inline bool operator==(const SymbolKey & x, const SymbolKey & y) {
    return x.name == y.name && x.ns == y.ns;
  }
  static inline bool operator!=(const SymbolKey & x, const SymbolKey & y) {
    return !(x == y);
  }

  template <typename T>
  inline const T * find_symbol(const SymbolKey & k, SymbolNode * cur, SymbolNode * stop = 0) {
    for (; cur != stop; cur = cur->next) {
      if (k == cur->key) break;
    }
    if (cur == stop) return 0;
    return dynamic_cast<const T *>(cur->value);
  }

  class OpenSymbolTable : public gc
  {
  public:
    bool is_root() {return back == 0;}
  public: // but don't use
    SymbolNode * * front;
    SymbolNode * back;
  public:
    OpenSymbolTable() // This is a placeholder, it can't be used in this statex
      : front(), back() {}
    OpenSymbolTable(SymbolNode * * f, SymbolNode * b)
      : front(f), back(b) {}
    template <typename T> 
    const T * find(const SymbolKey & k) {
      return find_symbol<T>(k, *front);
    }
    bool exists(const SymbolKey & k) {
      return find_symbol<Symbol>(k, *front);
    }
    template <typename T>
    void add(const SymbolKey & k, const T * sym) {
      if (find_symbol<Symbol>(k, *front, back)) return; // FIXME: throw error
      *front = new SymbolNode(k, sym, *front);
    }
  };

  class SymbolTable : public gc
  {
  public:
    bool is_root() {return back == 0;}
  public: // but don't use
    SymbolNode * front;
    SymbolNode * back;
  public:
    SymbolTable() 
      : front(), back() {}
    SymbolTable(SymbolNode * f, SymbolNode * b)
      : front(f), back(b) {}
    SymbolTable new_scope() {
      return SymbolTable(front, front);
    }
    SymbolTable new_scope(OpenSymbolTable & o) {
      SymbolNode * placeholder = new SymbolNode(SymbolKey(), NULL, front);
      o.front = &placeholder->next;
      o.back = front;
      return SymbolTable(placeholder, placeholder);
    }
    template <typename T> 
    const T * find(const SymbolKey & k) {
      return find_symbol<T>(k, front);
    }
    bool exists(const SymbolKey & k) {
      return find_symbol<Symbol>(k, front);
    }
    void add(const SymbolKey & k, const Symbol * sym) {
      if (find_symbol<Symbol>(k, front, back)) return; // FIXME: throw error
      front = new SymbolNode(k, sym, front);
    }
  };

}

#endif
