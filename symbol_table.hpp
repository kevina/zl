#ifndef SYMBOL_TABLE__HPP
#define SYMBOL_TABLE__HPP

#include <utility>
#include "vector.hpp"
#include "string_buf.hpp"

namespace ast {

  struct Mark;
  struct SymbolNode;

  struct Marks {
    const Mark * mark;
    const Marks * prev;
    Marks(const Mark * m, const Marks * p) 
      : mark(m), prev(p) {}
  };

  struct Mark : public gc_cleanup {
    const SymbolNode * env;
    typedef std::pair<const Marks *, const Marks *> CacheNode;
    // Cache is a mapping
    // from: the mark set we are adding this mark to
    // to:   the new mark set with the mark added
    typedef Vector<CacheNode> Cache;
    mutable Cache cache;
    Mark(const SymbolNode * e) : env(e) {
      cache.reserve(1);
      cache.push_back(CacheNode(NULL, new Marks(this, NULL)));
    }
  };
  
  inline const Marks * mark(const Marks * ms, const Mark * m) {
    for (Mark::Cache::iterator i = m->cache.begin(), e = m->cache.end(); i != e; ++i) 
      if (i->first == ms) return i->second;
    Marks * nms = new Marks(m, ms);
    m->cache.push_back(Mark::CacheNode(ms, nms));
    return nms;
  }

  template <typename T>
  static inline T mark(const T & orig, const Mark * m) {
    T tmp = orig;
    tmp.marks = mark(tmp.marks, m);
    return tmp;
  }


  struct SymbolName : public String {
    String name() const {return *this;}
    //void set_name(String s) {String::operator=(s);}
    const Marks * marks;
    SymbolName() : marks() {}
    SymbolName(const char * n) : String(n), marks() {}
    SymbolName(String n) : String(n), marks() {}
  };

  static const unsigned DEFAULT_NS = 0;
  static const unsigned TAG_NS = 2;
  static const unsigned LABEL_NS = 3;

  struct SymbolKey : public SymbolName {
    unsigned ns;
    SymbolKey(const char * n, unsigned ns0 = 0)
      : SymbolName(n), ns(ns0) {}
    SymbolKey(String n = String(), unsigned ns0 = 0)
      : SymbolName(n), ns(ns0) {}
    inline SymbolKey(const Parse & p);
  };

  struct Symbol {
    String name;
    mutable unsigned num;
    Symbol() : num() {}
    Symbol(String n) : name(n), num() {}
    virtual void uniq_name(OStream & o) const {
      if (num > 0)
        o.printf("%s$%u", ~name, num);
      else
        o << name;
    }
    String uniq_name() const {
      StringBuf buf;
      uniq_name(buf);
      return buf.freeze();
    }
    virtual ~Symbol() {}
  };

  struct SymbolNode {
    SymbolKey key;
    const Symbol * value;
    SymbolNode * next;
    SymbolNode(const SymbolKey & k, const Symbol * v, SymbolNode * n) 
      : key(k), value(v), next(n) {}
  };

  static inline bool operator==(const SymbolName & x, const SymbolName & y) {
    return (const String &)x == (const String &)y && x.marks == y.marks;
  }
  static inline bool operator!=(const SymbolName & x, const SymbolName & y) {
    return !(x == y);
  }

  static inline bool operator==(const SymbolKey & x, const SymbolKey & y) {
    return (const SymbolName &)x == (const SymbolName &)y && x.ns == y.ns;
  }
  static inline bool operator!=(const SymbolKey & x, const SymbolKey & y) {
    return !(x == y);
  }

  enum MarksStrategy {ExactMatch, NormalMarksStrategy, StripMarks};

  template <typename T>
  inline const T * find_symbol(SymbolKey k, const SymbolNode * cur, const SymbolNode * stop = NULL,
                               MarksStrategy ms = ExactMatch) 
  {
    for (; cur != stop; cur = cur->next) {
      if (k == cur->key) break;
    }
    if (cur == stop) {
      if (ms ==  NormalMarksStrategy && k.marks) {
        cur = k.marks->mark->env;
        k.marks = k.marks->prev;
        return find_symbol<T>(k, cur, stop, ms);
      } else if (ms == StripMarks && k.marks) {
        k.marks = k.marks->prev;
        return find_symbol<T>(k, cur, stop, ms);
      } else {
        return NULL;
      }
    }
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
      return SymbolTable(placeholder, front);
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
    void rename() {
      Vector<SymbolNode *> nodes;
      for (SymbolNode * cur = front; cur != back; cur = cur->next) {
        if (!cur->value) continue;
        nodes.push_back(cur);
      }
      while (!nodes.empty()) {
        SymbolNode * cur = nodes.back();
        nodes.pop_back();
        SymbolNode * p = cur->next;
        for (; p; p = p->next) {
          if (p != cur && p->key.name() == cur->key.name()) break;
        }
        unsigned num = 1;
        if (p) num = p->value->num + 1;
        cur->value->num = num;
      }
    }
  };

}

#endif
