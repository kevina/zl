#ifndef SYMBOL_TABLE__HPP
#define SYMBOL_TABLE__HPP

#include <stdio.h>

#include <typeinfo>
#include <utility>
#include "ostream.hpp"
#include "vector.hpp"
#include "string_buf.hpp"

struct Syntax;
struct Error;
struct SourceStr;
Error * error(const SourceStr & str, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));

namespace ast {

  struct AST;

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

  static inline const Marks * merge_marks(const Marks * a, const Marks * b) {
    Vector<const Mark *> to_add;
    for (; b; b = b->prev) 
      to_add.push_back(b->mark);
    for (Vector<const Mark *>::iterator i = to_add.begin(), e = to_add.end(); i != e; ++i)
      a = mark(a, *i);
    return a;
  }

  template <typename T>
  static inline T merge_marks(const T & orig, const Marks * ms) {
    T tmp = orig;
    tmp.marks = merge_marks(tmp.marks, ms);
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
  static const unsigned MODULE_NS = 5;
  static const unsigned INNERNS_NS = 6;

  struct SymbolKey : public SymbolName {
    unsigned ns;
    SymbolKey(const char * n, unsigned ns0 = 0)
      : SymbolName(n), ns(ns0) {}
    SymbolKey(String n, unsigned ns0 = 0)
      : SymbolName(n), ns(ns0) {}
    SymbolKey(SymbolName n = SymbolName(), unsigned ns0 = 0)
      : SymbolName(n), ns(ns0) {}
    inline SymbolKey(const Syntax & p, unsigned ns0 = 0);
  };

  struct SymbolNode;

  struct Environ;

  struct Symbol : public gc {
    String name;
    Symbol() {}
    virtual void uniq_name(OStream & o) const {
      o << name;
    }
    String uniq_name() const {
      StringBuf buf;
      uniq_name(buf);
      return buf.freeze();
    }
    virtual void add_to_env(const SymbolKey & k, Environ &) const;
    virtual void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const {}
    virtual ~Symbol() {}
  };

  // This if for any symbol which is not lexical and _might_
  // need to made externally visible, they are not necessary
  // global
  struct Declaration;
  struct TopLevelSymbol : virtual public Symbol {
    TopLevelSymbol(unsigned n = 0, const Declaration * d = NULL, TopLevelSymbol * w = NULL) 
      : num(n), decl(d), where(w) {}
    mutable unsigned num;     // 0 to avoid renaming, NPOS needs uniq num
    mutable const Declaration * decl; // NULL if internal
    TopLevelSymbol * where;           // NULL if global
    using Symbol::uniq_name;
    void uniq_name(OStream & o) const {
      if (num == 0)
        o << name;
      else
        o.printf("%s$$%u", ~name, num);
    }
    // if num is zero than leave alone, if NPOS assign uniq num.
    void add_to_env(const SymbolKey & k, Environ &) const;
    void add_to_local_env(const SymbolKey & k, Environ &) const;
    void add_to_top_level_env(const SymbolKey & k, Environ &) const;
    void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const;
  };

  struct LexicalSymbol : virtual public Symbol {
    LexicalSymbol() : num () {}
    mutable unsigned num;
    using Symbol::uniq_name;
    void uniq_name(OStream & o) const {
      o.printf("%s$%u", ~name, num);
    }
    void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const;
  };

  struct OtherSymbol : virtual public Symbol {
    OtherSymbol(unsigned n = 0) : num(n) {}
    mutable unsigned num;     // 0 to avoid renaming, NPOS needs uniq num
    using Symbol::uniq_name;
    void uniq_name(OStream & o) const {
      if (num == 0)
        o << name;
      else
        o.printf("%s$%u", ~name, num);
    }
    // if num is zero than leave alone, if NPOS assign uniq num.
    void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const;
  };

  struct FluidBinding : public TopLevelSymbol {
    FluidBinding(String n, SymbolName r) : rebind(r) {name = n;}
    SymbolName rebind;
  };
  
  struct InnerNS : public Symbol {
  };

  struct Module : public TopLevelSymbol {
    Module() : syms() {}
    SymbolNode * syms;
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

  enum Strategy {ThisScope, NormalStrategy, StripMarks};

  static inline
  const Symbol * find_symbol_p1(SymbolKey k, const SymbolNode * start, const SymbolNode * stop,
                                Strategy strategy)
  {
    const SymbolNode * cur = start;
    for (; cur != stop; cur = cur->next) {
      if (k == cur->key) break;
    }
    if (cur == stop) {
      if (strategy == NormalStrategy && k.marks) {
        cur = k.marks->mark->env;
        k.marks = k.marks->prev;
        return find_symbol_p1(k, cur, stop, strategy);
      } else if (strategy == StripMarks && k.marks) {
        k.marks = k.marks->prev;
        return find_symbol_p1(k, start, stop, strategy);
      } else {
        return NULL;
      }
    }
    return cur->value;
  }

  template <typename T>
  const Symbol * find_symbol_p2(SymbolKey k, const SymbolNode * start, const SymbolNode * stop = NULL,
                                Strategy strategy = NormalStrategy)
  {
    const Symbol * s = find_symbol_p1(k, start, stop, strategy);
    if (!s) return NULL;
    if (strategy != ThisScope)
      if (const FluidBinding * b = dynamic_cast<const FluidBinding *>(s))
        s = find_symbol_p1(SymbolKey(b->rebind, k.ns), start, stop, strategy);
    return s;
  }

  template <>
  inline
  const Symbol * find_symbol_p2<FluidBinding>(SymbolKey k, 
                                              const SymbolNode * start, const SymbolNode * stop,
                                              Strategy strategy) 
  {
    return find_symbol_p1(k, start, stop, strategy);
  }

  template <typename T>
  const T * find_symbol(SymbolKey k, const SymbolNode * start, const SymbolNode * stop = NULL,
                        Strategy strategy = NormalStrategy) 
  {
    const Symbol * s = find_symbol_p2<T>(k, start, stop, strategy);
    return dynamic_cast<const T *>(s);
  }

  template <typename T>
  const T * lookup_symbol(SymbolKey k, const SourceStr & str,
                          const SymbolNode * start, const SymbolNode * stop = NULL,
                          Strategy strategy = NormalStrategy)
  {
    const Symbol * s1 = find_symbol_p2<T>(k, start, stop, strategy);
    if (!s1)
      throw error(str, "Unknown Identifier \"%s\"", ~k.name);
    const T * s2 = dynamic_cast<const T *>(s1);
    if (!s2)
      throw error(str, "Identifier \"%s\" is of the wrong type.", ~k.name);
    return s2;
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
      //if (find_symbol<Symbol>(k, *front, back, ThisScope)) return; // FIXME: throw error
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
    const T * find(const SymbolKey & k, Strategy ms = NormalStrategy) {
      return find_symbol<T>(k, front, NULL, ms);
    }
    template <typename T> 
    const T * lookup(const SymbolKey & k, const SourceStr & str, Strategy ms = NormalStrategy) {
      return lookup_symbol<T>(k, str, front, NULL, ms);
    }
    template <typename T> 
    inline const T * lookup(const Syntax * p, unsigned ns = DEFAULT_NS);
    bool exists(const SymbolKey & k, Strategy ms = NormalStrategy) {
      return find_symbol<Symbol>(k, front, NULL, ms);
    }
    bool exists_this_scope(const SymbolKey & k) {
      return find_symbol<Symbol>(k, front, back, ThisScope);
    }
    void add(const SymbolKey & k, const Symbol * sym) {
      //if (exists_this_scope(k)) return; // FIXME: throw error
      front = new SymbolNode(k, sym, front);
    }
  };

  template <typename T>
  void assign_uniq_num(SymbolNode * cur, SymbolNode * stop = NULL) {
    const T * t = NULL;
    // we need to compare the actual symbol name, since it may be
    // aliases as a different name
    String name = cur->value->name;
    for (SymbolNode * p = cur->next; p != stop; p = p->next) {
      if (p->value && p->value->name == name && (t = dynamic_cast<const T *>(p->value))) 
        break;
    }
    unsigned num = 1;
    assert(!t || t->num != NPOS);
    if (t) num = t->num + 1;
    dynamic_cast<const T *>(cur->value)->num = num;
  }

  template <typename T>
  void assign_uniq_num(Vector<const TopLevelSymbol *> & syms) {
    const T * t = NULL;
    const T * cur = dynamic_cast<const T *>(syms.back());
    String name = cur->name;
    for (int i = syms.size() - 2; i >= 0; --i) {
      if (syms[i]->name == name && (t = dynamic_cast<const T *>(syms[i]))) 
        break;
    }
    unsigned num = 1;
    assert(!t || t->num != NPOS);
    if (t) num = t->num + 1;
    cur->num = num;
  }
}

#endif
