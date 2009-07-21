#ifndef SYMBOL_TABLE__HPP
#define SYMBOL_TABLE__HPP

#include <stdio.h>

#include <cxxabi.h>

#include <typeinfo>
#include <utility>
#include "ostream.hpp"
#include "vector.hpp"
#include "string_buf.hpp"
#include "type_info.hpp"

struct Syntax;
struct Error;
struct SourceStr;
Error * error(const SourceStr & str, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
struct SyntaxGather;

namespace ast {

  struct AST;

  struct Mark;
  struct SymbolNode;
  struct SymbolTable;

  struct Marks {
    const Mark * mark;
    const Marks * prev;
    Marks(const Mark * m, const Marks * p) 
      : mark(m), prev(p) {}
    void to_string(OStream & o, SyntaxGather * g) const;
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
    void to_string(OStream & o, SyntaxGather * g) const;
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
    void to_string(OStream & o, SyntaxGather * = NULL) const;
    String to_string(SyntaxGather * g = NULL) const {
      StringBuf buf;
      to_string(buf, g);
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

  struct InnerNS;
  extern const InnerNS * const DEFAULT_NS;
  extern const InnerNS * const TAG_NS;
  extern const InnerNS * const LABEL_NS;
  extern const InnerNS * const SYNTAX_NS;
  extern const InnerNS * const OUTER_NS;
  extern const InnerNS * const INNER_NS;
  extern const InnerNS * const CAST_NS;

  struct SymbolKey : public SymbolName {
    const InnerNS * ns;
    SymbolKey(const char * n, const InnerNS * ns0 = 0)
      : SymbolName(n), ns(ns0 ? ns0 : DEFAULT_NS) {}
    SymbolKey(String n, const InnerNS * ns0 = 0)
      : SymbolName(n), ns(ns0 ? ns0 : DEFAULT_NS) {}
    SymbolKey(SymbolName n = SymbolName(), const InnerNS * ns0 = 0)
      : SymbolName(n), ns(ns0 ? ns0 : DEFAULT_NS) {}
    inline SymbolKey(const Syntax & p, const InnerNS * ns0 = 0);

    void to_string(OStream & o) const;
    String to_string() const {
      StringBuf buf;
      to_string(buf);
      return buf.freeze();
    }
  };

  struct SymbolKeyEntity {
    typedef ::TypeInfo<SymbolKeyEntity> TypeInfo;
    SymbolKeyEntity(const SymbolKey n) : name(n) {}
    SymbolKey name;
  };

  struct SymbolNode;
  struct PropNode;

  struct Environ;

  enum Pass {AllPasses, FirstPass, SecondPass};

  struct Symbol {
    typedef ::TypeInfo<Symbol> TypeInfo;
    const SymbolKey * key;
    String name;
    mutable String uniq_name_;
    Symbol() {}
    String uniq_name() const {
      if (uniq_name_.defined())
        return uniq_name_;
      StringBuf buf;
      uniq_name(buf);
      uniq_name_ = buf.freeze();
      return uniq_name_;
    }
    virtual const InnerNS * tl_namespace() const {return DEFAULT_NS;}
    virtual void add_to_env(const SymbolKey & k, Environ &) const;
    virtual void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const {}
    virtual ~Symbol() {}
  protected:
    virtual void uniq_name(OStream & o) const {
      o << name;
    }
  };

  // This if for any symbol which is not lexical and _might_
  // need to made externally visible, they are not necessary
  // global

  struct TopLevelSymbol : virtual public Symbol {
    static unsigned last_order_num;
    TopLevelSymbol() : num(), props(), order_num() {}
    mutable unsigned num;     // 0 to avoid renaming, NPOS needs uniq num
    TopLevelSymbol * where;   // NULL if global
    PropNode * props;
    mutable unsigned order_num;
    using Symbol::uniq_name;
    void uniq_name(OStream & o) const {
      if (num == 0)
        o << name;
      else
        o.printf("%s$$%u", ~name, num);
    }
    // if num is zero than leave alone, if NPOS assign uniq num.
    void add_to_env(const SymbolKey & k, Environ &) const;
    void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const;
    virtual void add_prop(SymbolName n, const Syntax * s);
    virtual const Syntax * get_prop(SymbolName n) const;
    //virtual const Syntax * get_props(SymbolName n) const; // not implemented yet
  };

  struct FluidBinding : public TopLevelSymbol {
    FluidBinding(String n, SymbolName r) : rebind(r) {name = n;}
    SymbolName rebind;
  };
  
  struct InnerNS : public Symbol {
    InnerNS(String n) : Symbol() {name = n;}
  };

  void add_inner_nss(SymbolTable & sym);

  struct SymbolNode {
    SymbolKey key;
    const Symbol * value;
    SymbolNode * next;
    enum {IMPORTED = 1, ALIAS = 2, INTERNAL = 4};
    unsigned flags;
    bool imported() const {return flags & IMPORTED;}
    bool alias() const {return flags & ALIAS;}
    bool internal() const {return flags & INTERNAL;}
    void set_flags(unsigned f) {flags |= f;}
    void unset_flags(unsigned f) {flags &= ~f;}
    SymbolNode(const SymbolKey & k, const Symbol * v, SymbolNode * n = NULL) 
      : key(k), value(v), next(n), flags() {}
    SymbolNode(const SymbolKey & k, const Symbol * v, unsigned f, SymbolNode * n = NULL) 
      : key(k), value(v), next(n), flags(f) {}
    SymbolNode(const SymbolNode & n, SymbolNode * nx) 
      : key(n.key), value(n.value), next(nx), flags(n.flags) {}
  };

  struct PropNode {
    SymbolName name;
    const Syntax * value;
    PropNode * next;
    PropNode(SymbolName n, const Syntax * v, PropNode * nx = NULL)
      : name(n), value(v), next(nx) {}
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

  struct NoOpGather {
    void stripped_mark(const Mark * m) {}
  };

  struct AlwaysTrueExtraCmp {
    bool operator() (SymbolKey, const Symbol *) {return true;}
  };

  template <typename Gather, typename ExtraCmp>
  const SymbolNode * find_symbol_p1(SymbolKey k, const SymbolNode * start, const SymbolNode * stop,
                                    Strategy strategy, Gather & gather, ExtraCmp & cmp);
  template <typename Gather>
  static inline
  const SymbolNode * find_symbol_p1(SymbolKey k, const SymbolNode * start, const SymbolNode * stop,
                                    Strategy strategy, Gather & gather) {
    AlwaysTrueExtraCmp cmp;
    return find_symbol_p1(k, start, stop, strategy, gather, cmp);
  }
  static inline
  const SymbolNode * find_symbol_p1(SymbolKey k, const SymbolNode * start, const SymbolNode * stop,
                                    Strategy strategy) 
  {
    NoOpGather gather;
    AlwaysTrueExtraCmp cmp;
    return find_symbol_p1(k, start, stop, strategy, gather, cmp);
  }

  template <typename Gather, typename ExtraCmp>
  const SymbolNode * find_symbol_p1(SymbolKey k, const SymbolNode * start, const SymbolNode * stop,
                                    Strategy strategy, Gather & gather, ExtraCmp & cmp)
  {
    //printf("p1: %p %p\n", start, stop);
    const SymbolNode * cur = start;
    //printf ("*** %s\n", ~k.to_string());
    for (; cur != stop; cur = cur->next) {
      //if (k.ns->name == "internal") 
      //printf ("--- %s\n", ~cur->key.to_string());
      //printf("?? %s %d %d\n", ~cur->key.to_string(), k == cur->key, cmp(cur->key, cur->value));
      if (k == cur->key && cmp(cur->key, cur->value) && (strategy != ThisScope || !cur->imported())) break;
    }
    //printf("^^^ %d\n", cur == stop);
    if (cur == stop) {
      if (strategy == NormalStrategy && k.marks) {
        cur = k.marks->mark->env;
        gather.stripped_mark(k.marks->mark); 
        k.marks = k.marks->prev;
        return find_symbol_p1(k, cur, stop, strategy, gather, cmp);
      } else if (strategy == StripMarks && k.marks) {
        gather.stripped_mark(k.marks->mark); 
        k.marks = k.marks->prev;
        return find_symbol_p1(k, start, stop, strategy, gather, cmp);
      } else {
        return NULL;
      }
    }
    return cur;
  }

  template <typename T/*, typename ExtraCmp*/>
  inline
  const SymbolNode * find_symbol_p2(const SymbolNode * s, SymbolKey k, 
                                    const SymbolNode * start, const SymbolNode * stop,
                                    Strategy strategy)
  {
    if (!s) return NULL;
    if (strategy != ThisScope)
      if (const FluidBinding * b = dynamic_cast<const FluidBinding *>(s->value))
        s = find_symbol_p1(SymbolKey(b->rebind, k.ns), start, stop, strategy);
    return s;
  }
  template <>
  inline
  const SymbolNode * find_symbol_p2<FluidBinding>(const SymbolNode * s, SymbolKey, 
                                                  const SymbolNode *, const SymbolNode *,
                                                  Strategy)
  {
    return s;
  }
 
  template <typename T, typename Gather, typename ExtraCmp>
  const SymbolNode * find_symbol_p3(SymbolKey k, const SymbolNode * start, const SymbolNode * stop,
                                    Strategy strategy, Gather & gather, ExtraCmp & cmp)
  {
    //printf("p3: %p %p\n", start, stop);
    const SymbolNode * s = find_symbol_p1(k, start, stop, strategy, gather, cmp);
    return find_symbol_p2<T>(s, k, start, stop, strategy);
  }

  static inline
  SymbolNode * find_symbol_node(SymbolKey k, SymbolNode * start, SymbolNode * stop = NULL, 
                                Strategy strategy = NormalStrategy) 
  {
    const SymbolNode * s = find_symbol_p1(k, start, stop, strategy);
    return const_cast<SymbolNode *>(s);
  }

  template <typename T, typename Gather, typename ExtraCmp>
  const T * find_symbol(SymbolKey k, const SymbolNode * start, const SymbolNode * stop,
                        Strategy strategy, Gather & gather, ExtraCmp & cmp) 
  {
    const SymbolNode * s = find_symbol_p3<T>(k, start, stop, strategy, gather, cmp);
    if (!s) return NULL;
    return dynamic_cast<const T *>(s->value);
  }

  template <typename T, typename Gather, typename ExtraCmp>
  const T * lookup_symbol(SymbolKey k, const SourceStr & str,
                          const SymbolNode * start, const SymbolNode * stop,
                          Strategy strategy, Gather & gather, ExtraCmp & cmp)
  {
    const SymbolNode * s1 = find_symbol_p3<T>(k, start, stop, strategy, gather, cmp);
    if (!s1) {
      //fprintf(stderr, "Unknown Identifier \"%s\"", ~k.name); abort();
      //throw error(str, "Unknown Identifier \"%s\"", ~k.name);
      throw error(str, "Unknown Identifier \"%s\"", ~k.to_string());
    }

    const T * s2 = dynamic_cast<const T *>(s1->value);
    if (!s2) {
      //fprintf(stderr, "Identifier \"%s\" is of the wrong type.", ~k.name); abort();
      //throw error(str, "Identifier \"%s\" is of the wrong type.", ~k.name);
      throw error(str, "Identifier \"%s\" is of the wrong type (expected %s got %s).", 
                  ~k.name, 
                  abi::__cxa_demangle(typeid(const T).name(), NULL, NULL, NULL), 
                  abi::__cxa_demangle(typeid(*s1->value).name(), NULL, NULL, NULL));
    }
    return s2;
  }


  template <typename T>
  static inline
  const T * find_symbol(SymbolKey k, const SymbolNode * start, const SymbolNode * stop = NULL,
                        Strategy strategy = NormalStrategy) 
  {
    NoOpGather gather; 
    AlwaysTrueExtraCmp cmp;
    return find_symbol<T>(k, start, stop, strategy, gather, cmp);
  }

  template <typename T>
  static inline
  const T * lookup_symbol(SymbolKey k, const SourceStr & str,
                          const SymbolNode * start, const SymbolNode * stop = NULL,
                          Strategy strategy = NormalStrategy)
  {
    NoOpGather gather;
    AlwaysTrueExtraCmp cmp;
    return lookup_symbol<T>(k, str, start, stop, strategy, gather, cmp);
  }

  
  // SymbolList is a helper class to make copying symbol tables, with
  // out reversing the order, easier.  You copy the table into
  // SymbolList than splice the list into the SymbolTable.
  struct SymbolList {
    SymbolNode * first;
    SymbolNode * last;
    SymbolList() : first(), last() {}
    SymbolNode * push_back_i(SymbolNode * n) {
      if (last) {
        last->next = n;
        last = last->next;
      } else {
        first = last = n;
      }
      return n;
    }
    SymbolNode * push_back(const SymbolKey & k, const Symbol * sym) {
      return push_back_i(new SymbolNode(k, sym));
    }
    SymbolNode * push_back(const SymbolNode & n) {
      return push_back_i(new SymbolNode(n, NULL));
    }
  };

  class OpenSymbolTable : public gc
  {
  public:
    //bool is_root() {return back == 0;}
  public: // but don't use
    SymbolNode * * front;
    //SymbolNode * back;
  public:
    OpenSymbolTable() // This is a placeholder, it can't be used in this state
      : front() {}
    OpenSymbolTable(SymbolNode * * f)
      : front(f) {}
    template <typename T> 
    const T * find(const SymbolKey & k) const {
      return find_symbol<T>(k, *front);
    }
    bool exists(const SymbolKey & k) const {
      return find_symbol<Symbol>(k, *front);
    }
    template <typename T>
    SymbolNode * add(const SymbolKey & k, const T * sym, unsigned flags = 0) {
      //if (find_symbol<Symbol>(k, *front, back, ThisScope)) return; // FIXME: throw error
      *front = new SymbolNode(k, sym, flags, *front);
      return *front;
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
      //o.back = front;
      return SymbolTable(placeholder, front);
    }
    template <typename T> 
    const T * find(const SymbolKey & k, Strategy ms = NormalStrategy) const {
      return find_symbol<T>(k, front, NULL, ms);
    }
    template <typename T> 
    const T * lookup(const SymbolKey & k, const SourceStr & str, Strategy ms = NormalStrategy) const {
      return lookup_symbol<T>(k, str, front, NULL, ms);
    }
    template <typename T> 
    inline const T * lookup(const Syntax * p, const InnerNS * = DEFAULT_NS) const;
    template <typename T> 
    inline const T * find(const Syntax * p, const InnerNS * = DEFAULT_NS) const;
    bool exists(const SymbolKey & k, Strategy ms = NormalStrategy) const {
      return find_symbol<Symbol>(k, front, NULL, ms);
    }
    bool exists_this_scope(const SymbolKey & k) const {
      return find_symbol<Symbol>(k, front, back, ThisScope);
    }
    inline bool exists(const Syntax * p, const InnerNS * = DEFAULT_NS) const;
    inline bool exists_this_scope(const Syntax * p, const InnerNS * = DEFAULT_NS) const;
    SymbolNode * add(const SymbolKey & k, const Symbol * sym, unsigned flags = 0) {
      //if (exists_this_scope(k)) return; // FIXME: throw error
      front = new SymbolNode(k, sym, flags, front);
      return front;
    }
    SymbolNode * add_internal(const SymbolKey & k, const Symbol * sym) {
      return add(k, sym, SymbolNode::INTERNAL);
    }
    void splice(SymbolNode * first, SymbolNode * last) {
      last->next = front;
      front = first;
    }
    void dump_this_scope();
  };

  template <typename T>
  unsigned existing_uniq_num(T * sym) {
    unsigned num;
    int pos;
    int res = sscanf(~sym->name, "%*[^$]%*[$]%u%n", &num, &pos);
    if (pos == sym->name.size()) return num;
    else return NPOS;
  }

  template <typename T>
  void assign_uniq_num(unsigned num, T * sym) {
    unsigned existing_num = existing_uniq_num(sym);
    if (existing_num != NPOS) {
      assert(existing_num >= num);
      sym->uniq_name_ = sym->name;
      sym->num = existing_num;
    } else {
      sym->num = num;
    }
  }

  template <typename T>
  void assign_uniq_num(const T * sym, SymbolNode * cur, SymbolNode * stop = NULL) {
    const T * t = NULL;
    // we need to compare the actual symbol name, since it may be
    // aliases as a different name
    String name = sym->name;
    for (; cur != stop; cur = cur->next) {
      if (cur->value && cur->value->name == name && 
          (t = dynamic_cast<const T *>(cur->value)) && t != sym && t->num != 0) 
        break;
    }
    if (t == sym) t = NULL;
    unsigned num = 1;
    assert(!t || t->num != NPOS);
    if (t) num = t->num + 1;
    assign_uniq_num(num, sym);
  }

  template <>
  void assign_uniq_num<TopLevelSymbol>(const TopLevelSymbol * sym, SymbolNode * cur, SymbolNode * stop);

}

#endif
