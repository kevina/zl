#ifndef SYMBOL_TABLE__HPP
#define SYMBOL_TABLE__HPP

#include <stdio.h>

#include <utility>
#include "ostream.hpp"
#include "vector.hpp"
#include "string_buf.hpp"

struct Syntax;

namespace ast {

  struct Mark;
  struct SymbolNode;

  struct Marks {
    const Mark * mark;
    const Marks * prev;
    Marks(const Mark * m, const Marks * p) 
      : mark(m), prev(p) {}
    void to_string(OStream & o) const;
  };

  struct Mark : public gc_cleanup {
    static unsigned last_id; 
    unsigned id;
    const SymbolNode * env;
    typedef std::pair<const Marks *, const Marks *> CacheNode;
    // Cache is a mapping
    // from: the mark set we are adding this mark to
    // to:   the new mark set with the mark added
    typedef Vector<CacheNode> Cache;
    mutable Cache cache;
    Mark(const SymbolNode * e) : id(last_id++), env(e) {
      cache.reserve(1);
      cache.push_back(CacheNode(NULL, new Marks(this, NULL)));
    }
  };
  
  static inline const Marks * mark(const Marks * ms, const Mark * m) {
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

  void marks_ignored(String name);

  struct SymbolName {
    String name;
    //void set_name(String s) {String::operator=(s);}
    const Marks * marks;
    SymbolName() : marks() {}
    SymbolName(const char * n) : name(n), marks() {}
    SymbolName(String n, const Marks * m = NULL) : name(n), marks(m) {}

    // Convert to a string with marks append as '<mark>
    void to_string(OStream & o) const;
    String to_string() const {
      StringBuf buf;
      to_string(buf);
      return buf.freeze();
    }

    void assert_no_marks() const {
      if (marks) marks_ignored(name);
    }

    bool defined() const {return name.defined();}
    bool empty() const {return name.empty();}
    operator const String & () const {assert_no_marks(); return name;}
    operator ParmString() const {assert_no_marks(); return name;}
    operator String & () {assert_no_marks(); return name;}
    const char * operator ~() const {assert_no_marks(); return ~name;}
    const char * c_str() const {assert_no_marks(); return name.c_str();}
  };

  //static inline OStream & operator<<(OStream & o, const SymbolName & n) {
  //  n.to_string(o);
  //  return o;
  //}

  static inline bool operator==(const SymbolName & rhs, const char * lhs) {
    return rhs.name == lhs;
  }

  static const unsigned DEFAULT_NS = 0;
  static const unsigned TAG_NS = 2;
  static const unsigned LABEL_NS = 3;
  static const unsigned SYNTAX_NS = 4;

  struct SymbolKey : public SymbolName {
    unsigned ns;
    SymbolKey(const char * n, unsigned ns0 = 0)
      : SymbolName(n), ns(ns0) {}
    SymbolKey(String n, unsigned ns0 = 0)
      : SymbolName(n), ns(ns0) {}
    SymbolKey(SymbolName n = SymbolName(), unsigned ns0 = 0)
      : SymbolName(n), ns(ns0) {}
    inline SymbolKey(const Syntax & p);
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
    return x.name == y.name && x.marks == y.marks;
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
  const T * find_symbol(SymbolKey k, const SymbolNode * start, const SymbolNode * stop = NULL,
                        MarksStrategy ms = ExactMatch) 
  {
    const SymbolNode * cur = start;
    //printf(">>%s %p\n", ~k, k.marks);
    for (; cur != stop; cur = cur->next) {
      if (k == cur->key) break;
    }
    if (cur == stop) {
      if (ms == NormalMarksStrategy && k.marks) {
        cur = k.marks->mark->env;
        k.marks = k.marks->prev;
        return find_symbol<T>(k, cur, stop, ms);
      } else if (ms == StripMarks && k.marks) {
        k.marks = k.marks->prev;
        return find_symbol<T>(k, start, stop, ms);
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
    const T * find(const SymbolKey & k, MarksStrategy ms = NormalMarksStrategy) {
      return find_symbol<T>(k, front, NULL, ms);
    }
    bool exists(const SymbolKey & k, MarksStrategy ms = NormalMarksStrategy) {
      return find_symbol<Symbol>(k, front, NULL, ms);
    }
    bool exists_this_scope(const SymbolKey & k, MarksStrategy ms = NormalMarksStrategy) {
      return find_symbol<Symbol>(k, front, back, ms);
    }
    void add(const SymbolKey & k, const Symbol * sym) {
      //if (find_symbol<Symbol>(k, front, back)) return; // FIXME: throw error
      front = new SymbolNode(k, sym, front);
    }
    void rename(bool if_marked, const SymbolNode * stop);
    void rename(const SymbolNode * stop = NULL) {rename(false, stop);}
    void rename_marked(const SymbolNode * stop = NULL) {rename(true, stop);}
  };

}

#endif
