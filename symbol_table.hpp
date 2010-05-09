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
#include "source_str.hpp"
#include "syntax-f.hpp"

struct Error;
struct SourceStr;
Error * error(const SourceStr & str, const char * fmt, ...)
  __attribute__ ((format (printf, 2, 3)));
struct SyntaxGather;

namespace ast {

  struct AST;
  struct Stmt;

  struct Mark;
  struct SymbolNode;
  struct SymbolTable;

  struct Marks : public gc {
    const Mark * mark;
    const Marks * prev;
    Marks(const Mark * m, const Marks * p) 
      : mark(m), prev(p) {}
    void to_string(OStream & o, SyntaxGather * g) const;
  };

  struct Mark {
    static unsigned last_id; 
    unsigned id;
    const SymbolNode * env;
    typedef std::pair<const Marks *, const Marks *> CacheNode;
    // Cache is a mapping
    // from: the mark set we are adding this mark to
    // to:   the new mark set with the mark added
    typedef Vector<CacheNode> Cache;
    mutable Cache cache;
    Marks self;
    Mark(const SymbolNode * e) : id(last_id++), env(e), self(this, NULL) {
      cache.reserve(1);
      cache.push_back(CacheNode(NULL, &self));
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

  struct SymbolName : public gc {
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
  extern const InnerNS * const SPECIAL_NS;
  extern const InnerNS * const OPERATOR_NS;
  extern const InnerNS * const INTERNAL_NS;

  struct SymbolKey : public SymbolName {
    const InnerNS * ns;
    SymbolKey(const char * n, const InnerNS * ns0 = 0)
      : SymbolName(n), ns(ns0 ? ns0 : DEFAULT_NS) {}
    SymbolKey(String n, const InnerNS * ns0 = 0)
      : SymbolName(n), ns(ns0 ? ns0 : DEFAULT_NS) {}
    SymbolKey(SymbolName n = SymbolName(), const InnerNS * ns0 = 0)
      : SymbolName(n), ns(ns0 ? ns0 : DEFAULT_NS) {}
    inline SymbolKey(const Syntax & p, const InnerNS * ns0 = 0);

    void to_string(OStream & o, const InnerNS * = DEFAULT_NS) const;
    String to_string(const InnerNS * def_ns = DEFAULT_NS) const {
      StringBuf buf;
      to_string(buf, def_ns);
      return buf.freeze();
    }
  };

  struct SymbolKeyEntity {
    typedef ::TypeInfo<SymbolKeyEntity> TypeInfo;
    SymbolKeyEntity(const SymbolKey n) : name(n) {}
    SymbolKey name;
    SourceStr source_str() const {return SourceStr();}
  };

  struct SymbolNode;
  struct PropNode;

  struct Environ;

  enum Pass {AllPasses, FirstPass, SecondPass};

  // defined in ast.cpp
  void asm_name(const SymbolKey * key, OStream &);

  struct Symbol : public gc {
    typedef ::TypeInfo<Symbol> TypeInfo;
    const SymbolKey * key;
    String name() const {return key->name;}
    mutable String uniq_name_;
    Symbol() : key() {}
    String uniq_name() const {
      if (uniq_name_.defined())
        return uniq_name_;
      StringBuf buf;
      bool cache = uniq_name(buf);
      if (cache) {
        uniq_name_ = buf.freeze();
        return uniq_name_;
      } else {
        return buf.freeze();
      }
    }
    virtual const InnerNS * tl_namespace() const {return DEFAULT_NS;}
    virtual const class Tuple * overloadable() const {return NULL;}
    virtual bool is_template() const {return false;}
    virtual SymbolNode * add_to_env(const SymbolKey & k, Environ &, bool shadow_ok);
    virtual void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const {}
    virtual ~Symbol() {}
    virtual SourceStr source_str() const {return SourceStr();}
    virtual void add_prop(SymbolName n, const Syntax * s) {abort();}
    virtual const Syntax * get_prop(SymbolName n) const {abort();}
  protected:
    virtual bool uniq_name(OStream & o) const {
      asm_name(key, o);
      return true;
    }
  };

  struct Primitive : public Symbol {
  };

  // This if for any symbol which is not lexical and _might_
  // need to made externally visible, they are not necessary
  // global

  struct Props : public gc {
    PropNode * props;
    Props() : props() {}
    void add_prop(SymbolName n, const Syntax * s);
    const Syntax * get_prop(SymbolName n) const;
  };

  struct TopLevelSymbol : virtual public Symbol {
    TopLevelSymbol() : num(), mangle(true), where(), props() {}
    mutable unsigned num;     // 0 to avoid renaming, NPOS needs uniq num
    bool mangle;
    TopLevelSymbol * where;   // NULL if global
    Props props;
    using Symbol::uniq_name;
    void uniq_name0(OStream & o) const {
      if (mangle && where) {
        where->uniq_name0(o);
        o << "$";
      }
      o << name();
      if (num != 0)
        o.printf("$$%u", num);
    }
    bool uniq_name(OStream & o) const {
      if (num == 0 && mangle && where) {
        where->uniq_name0(o);
        o << "$";
      }
      if (num == 0) {
        asm_name(key, o);
        if (where) 
          o << "$";
      } else {
        asm_name(key, o);
        o.printf("$$%u", num);
      }
      return num != NPOS;
    }
    // if num is zero than leave alone, if NPOS assign uniq num.
    SymbolNode * add_to_env(const SymbolKey & k, Environ &, bool shadow_ok);
    void finish_add_to_env(SymbolNode * local, Environ &);
    void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const;
    virtual void add_prop(SymbolName n, const Syntax * s) {props.add_prop(n, s);}
    virtual const Syntax * get_prop(SymbolName n) const {return props.get_prop(n);}
    void full_name(OStream & o) const {
      if (where) {
        where->full_name(o);
        o << "::";
      }
      key->to_string(o, tl_namespace());
    }
    String full_name() const {
      StringBuf buf;
      full_name(buf);
      return buf.freeze();
    }
    //virtual const Syntax * get_props(SymbolName n) const; // not implemented yet

    // if named_outer is true and a symbol is defined within the env
    // of this symbol, than there is probably no need to assign a uniq
    // number...
    virtual bool named_outer() const {return false;}
    // ok, so the const is a bit of a lie
    virtual void assign_uniq_num(SymbolNode * cur) const;
  };

  struct FluidBinding : public TopLevelSymbol {
    FluidBinding(SymbolName r) : rebind(r) {}
    SymbolName rebind;
  };

  struct InnerNS {
    struct Tag;
    const Tag * tag;
    const InnerNS * next;
    InnerNS(const Tag * t, const InnerNS * nx)
      : tag(t), next(nx) {}
  private:
    InnerNS(const InnerNS &);
    void operator=(const InnerNS &);
  };
  
  struct InnerNS::Tag : public Symbol, public InnerNS {
    static unsigned last_order_num;
    unsigned order_num;
    typedef std::pair<const InnerNS *, const InnerNS *> CacheNode;
    // Cache is a mapping
    // from: the mark set we are adding this mark to
    // to:   the new mark set with the mark added
    typedef Vector<CacheNode> Cache;
    mutable Cache cache;
    Tag() : InnerNS(this, NULL), order_num(last_order_num++) {
      cache.reserve(1);
      //cache.push_back(CacheNode(NULL, this));
      cache.push_back(CacheNode(NULL, this));
    }
  };

  struct InnerNSBuilder {
    Vector<const InnerNS::Tag *> data;
    // don't use, use "add" instead (InnerNS::Tag is also a InnerNS)
    void add_tag(const InnerNS::Tag * tag) {
      Vector<const InnerNS::Tag *>::iterator
        i = data.begin(), e = data.end();
      while (i != e) {
        if (*i == tag) return;
        if ((*i)->order_num >= tag->order_num) break;
        ++i;
      }
      data.insert(i, tag);
    }
    void add(const InnerNS * ns) {
      add_tag(ns->tag);
      if (ns->next)
        add(ns->next);
    }
    static const InnerNS * add_tag(const InnerNS * ns, const InnerNS::Tag * tag) {
      for (InnerNS::Tag::Cache::iterator 
             i = tag->cache.begin(), e = tag->cache.end(); 
           i != e; ++i) 
        if (i->first == ns) return i->second;
      InnerNS * nns = new InnerNS(tag, ns);
      tag->cache.push_back(InnerNS::Tag::CacheNode(ns, nns));
      return nns;
    }
    const InnerNS * build() {
      const InnerNS * ns = NULL;
      for (Vector<const InnerNS::Tag *>::iterator
             i = data.begin(), e = data.end();
           i != e; ++i)
        ns = add_tag(ns, *i);
      if (ns == NULL) ns = DEFAULT_NS;
      return ns;
    }
  };

  void add_inner_nss(Environ &);

  struct SymbolNode : public gc {
    SymbolKey key;
    typedef TopLevelSymbol * Scope;
    Scope scope;   // Effective scope, NULL if global.  Used primary
                   // for overload resolution.
    Symbol * value;
    SymbolNode * next;
    enum {ALIAS = 1, IMPORTED = 2, DIFF_SCOPE = 4, INTERNAL = 8, 
          PUSH_TO_OUTER_SCOPE = 16};
    unsigned flags;
    bool alias() const {return flags & ALIAS;}
    bool imported() const {return flags & IMPORTED;}
    bool diff_scope() const {return flags & DIFF_SCOPE;}
    bool internal() const {return flags & INTERNAL;}
    bool should_skip() const {return flags & (IMPORTED | DIFF_SCOPE | INTERNAL);}
    void set_flags(unsigned f) {flags |= f;}
    void unset_flags(unsigned f) {flags &= ~f;}
    SymbolNode(Scope s, const SymbolKey & k, Symbol * v, SymbolNode * n = NULL) 
      : key(k), scope(s), value(v), next(n), flags() {}
    SymbolNode(Scope s, const SymbolKey & k, Symbol * v, unsigned f, SymbolNode * n = NULL) 
      : key(k), scope(s), value(v), next(n), flags(f) {}
    SymbolNode(const SymbolNode & n, SymbolNode * nx) 
      : key(n.key), scope(n.scope), value(n.value), next(nx), flags(n.flags) {}
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
    bool operator() (const SymbolNode *) {return true;}
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
      //if (cur->value)
      //  printf("?? %s %d %d\n", ~cur->key.to_string(), k == cur->key, cmp(cur->key, cur->value));
      if (k == cur->key && cmp(cur) && (strategy != ThisScope || !cur->diff_scope())) break;
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
      if (const FluidBinding * b = dynamic_cast<const FluidBinding *>(s->value)) {
        s = find_symbol_p1(SymbolKey(b->rebind, k.ns), start, stop, strategy);
      }
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
  T * find_symbol(SymbolKey k, const SymbolNode * start, const SymbolNode * stop,
                        Strategy strategy, Gather & gather, ExtraCmp & cmp) 
  {
    const SymbolNode * s = find_symbol_p3<T>(k, start, stop, strategy, gather, cmp);
    if (!s) return NULL;
    return dynamic_cast<T *>(s->value);
  }

  template <typename T, typename Gather, typename ExtraCmp>
  T * lookup_symbol(SymbolKey k, const SourceStr & str,
                          const SymbolNode * start, const SymbolNode * stop,
                          Strategy strategy, Gather & gather, ExtraCmp & cmp)
  {
    const SymbolNode * s1 = find_symbol_p3<T>(k, start, stop, strategy, gather, cmp);
    if (!s1) {
      //fprintf(stderr, "Unknown Identifier \"%s\"", ~k.name); abort();
      //throw error(str, "Unknown Identifier \"%s\"", ~k.name);
      throw error(str, "Unknown Identifier \"%s\"", ~k.to_string());
    }

    T * s2 = dynamic_cast<T *>(s1->value);
    if (!s2) {
      //fprintf(stderr, "Identifier \"%s\" is of the wrong type.", ~k.name); abort();
      //throw error(str, "Identifier \"%s\" is of the wrong type.", ~k.name);
      throw error(str, "Identifier \"%s\" is of the wrong type (expected %s got %s).", 
                  ~k.name, 
                  abi::__cxa_demangle(typeid(const T).name(), NULL, NULL, NULL), 
                  abi::__cxa_demangle(typeid(*s1->value).name(), NULL, NULL, NULL));
    }
    //printf("LOOKUP %s OK\n", ~k.to_string());
    return s2;
  }


  template <typename T>
  static inline
  T * find_symbol(SymbolKey k, const SymbolNode * start, const SymbolNode * stop = NULL,
                        Strategy strategy = NormalStrategy) 
  {
    NoOpGather gather; 
    AlwaysTrueExtraCmp cmp;
    return find_symbol<T>(k, start, stop, strategy, gather, cmp);
  }

  template <typename T>
  static inline
  T * lookup_symbol(SymbolKey k, const SourceStr & str,
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
    SymbolNode * push_back(SymbolNode::Scope s, const SymbolKey & k, Symbol * sym) {
      return push_back_i(new SymbolNode(s, k, sym));
    }
    SymbolNode * push_back(const SymbolNode & n) {
      return push_back_i(new SymbolNode(n, NULL));
    }
  };

  class SymbolInsrPoint : public gc
  {
  public: // but don't use
    SymbolNode * * front;
  public:
    SymbolInsrPoint() // This is a placeholder, it can't be used in this state
      : front() {}
    SymbolInsrPoint(SymbolNode * * f)
      : front(f) {}
    template <typename T>
    SymbolNode * add(SymbolNode::Scope s, const SymbolKey & k, T * sym, unsigned flags = 0, SymbolNode * back = NULL) {
      //if (find_symbol<Symbol>(k, *front, back, ThisScope)) return; // FIXME: throw error
      //printf("ADDING %s (%p) to symbol table\n", ~k.name, dynamic_cast<Symbol *>(sym));
      *front = new SymbolNode(s, k, sym, flags, *front);
      return *front;
    }
    void splice(SymbolNode * first, SymbolNode * last) {
      last->next = *front;
      *front = first;
    }
  };

  class SymbolTableBase : public gc {
  public: // but don't use
    //friend class Environ;
    SymbolNode * front;
    SymbolNode * back;
  public:
    SymbolTableBase() 
      : front(), back() {}
    SymbolTableBase(SymbolNode * f, SymbolNode * b)
      : front(f), back(b) {}
    SymbolTableBase(const SymbolTableBase & o)
      : front(o.front), back(o.back) {}
    SymbolTableBase & operator=(const SymbolTableBase & o) {
      front = o.front;
      back = o.back;
      return *this;
    }
    void dump() const;
    void dump_this_scope() const;
  };

  class SymbolTable : public SymbolTableBase
  {
  public:
    bool is_root() {return back == 0;}
    SymbolInsrPoint ip;
    SymbolInsrPoint import_ip;
  public:
    SymbolTable() 
      : SymbolTableBase(), ip(&front) {}
    SymbolTable(SymbolNode * f, SymbolNode * b)
      : SymbolTableBase(f,b), ip(&front), import_ip(&front) {}
    SymbolTable(SymbolNode * f, SymbolNode * b, SymbolNode * * i, SymbolNode * * j)
      : SymbolTableBase(f,b), ip(i), import_ip(j) {}
    SymbolTable(const SymbolTable & o)
      : SymbolTableBase(o), ip(o.ip.front == &o.front ? &front : o.ip), 
        import_ip(o.import_ip.front == &o.front ? &front : o.import_ip) {}
    SymbolTable & operator=(const SymbolTable & o) {
      SymbolTableBase::operator=(o);
      ip = o.ip.front == &o.front ? &front : o.ip;
      import_ip = o.ip.front == &o.front ? &front : o.import_ip;
      return *this;
    }
    SymbolTable new_scope() const {
      return SymbolTable(front, front);
    }
    SymbolTable new_scope(SymbolInsrPoint & o) const {
      SymbolNode * placeholder = new SymbolNode(NULL, SymbolKey(), NULL, front);
      o.front = &placeholder->next;
      return SymbolTable(placeholder, front);
    }
    //SymbolTable new_open_scope() const {
    //  SymbolNode * import_placeholder = new SymbolNode(NULL, SymbolKey(), NULL, front);
    //  SymbolNode * placeholder = new SymbolNode(NULL, SymbolKey(), NULL, import_placeholder);
    //  return SymbolTable(placeholder, front, &placeholder->next, &import_placeholder->next);
    //}
    SymbolTable new_open_scope(SymbolInsrPoint * o = NULL) const {
      SymbolNode * outer_placeholder = front;
      //if (o) {
      //  outer_placeholder = new SymbolNode(NULL, SymbolKey(), NULL, front);
      //  o->front = &outer_placeholder->next;
      //}
      SymbolNode * import_placeholder = new SymbolNode(NULL, SymbolKey(), NULL, outer_placeholder);
      SymbolNode * placeholder = new SymbolNode(NULL, SymbolKey(), NULL, import_placeholder);
      return SymbolTable(placeholder, front, &placeholder->next, &import_placeholder->next);
    }
    template <typename T> 
    T * find(const SymbolKey & k, Strategy ms = NormalStrategy) const {
      return find_symbol<T>(k, front, NULL, ms);
    }
    template <typename T> 
    T * find_this_scope(const SymbolKey & k) const {
      return find_symbol<T>(k, front, back, ThisScope);
    }
    template <typename T> 
    T * lookup(const SymbolKey & k, const SourceStr & str, Strategy ms = NormalStrategy) const {
      return lookup_symbol<T>(k, str, front, NULL, ms);
    }
    template <typename T> 
    inline T * lookup(const Syntax * p, const InnerNS * = DEFAULT_NS, Strategy ms = NormalStrategy) const;
    template <typename T> 
    inline T * find(const Syntax * p, const InnerNS * = DEFAULT_NS, Strategy ms = NormalStrategy) const;
    template <typename T> 
    inline T * find_this_scope(const Syntax * p, const InnerNS * = DEFAULT_NS) const;
    bool exists(const SymbolKey & k, Strategy ms = NormalStrategy) const {
      return find_symbol<Symbol>(k, front, NULL, ms);
    }
    bool exists_this_scope(const SymbolKey & k) const {
      return find_symbol<Symbol>(k, front, back, ThisScope);
    }
    inline bool exists(const Syntax * p, const InnerNS * = DEFAULT_NS) const;
    inline bool exists_this_scope(const Syntax * p, const InnerNS * = DEFAULT_NS) const;
    SymbolNode * add(SymbolNode::Scope s, const SymbolKey & k, Symbol * sym, unsigned flags = 0) {
      return ip.add(s, k, sym, flags, back);
    }
    SymbolNode * add_internal(const SymbolKey & k, Symbol * sym) {
      return add(NULL, k, sym, SymbolNode::INTERNAL);
    }
    void splice(SymbolNode * first, SymbolNode * last) {
      return ip.splice(first, last);
    }
  };

  class TopLevelSymbolTable : public SymbolInsrPoint {
  public: // but don't use
    //TopLevelSymbolNode * defn_front;
    Stmt * first;
    Stmt * last;
  public:    
    TopLevelSymbolTable(SymbolNode * * f) 
      : SymbolInsrPoint(f), first(), last() {}
    template <typename T> 
    const T * find(const SymbolKey & k) const {
      return find_symbol<T>(k, *front);
    }
    bool exists(const SymbolKey & k) const {
      return find_symbol<Symbol>(k, *front);
    }
    inline void add_defn(Stmt *); // defined in ast.hpp
    inline void move_defn(Stmt *); // defined in ast.hpp
  };

  template <typename T>
  unsigned existing_uniq_num(T * sym) {
    unsigned num;
    int pos = -1;
    int res = sscanf(~sym->name(), "%*[^$]%*[$]%u%n", &num, &pos);
    if (pos == sym->name().size()) return num;
    else return NPOS;
  }

  template <typename T>
  void assign_uniq_num(unsigned num, T * sym) {
    unsigned existing_num = existing_uniq_num(sym);
    if (existing_num != NPOS) {
      //assert(existing_num >= num); // not always the case with temporaries
      sym->uniq_name_ = sym->name();
      sym->num = existing_num;
    } else {
      sym->num = num;
    }
  }

  template <typename T>
  void assign_uniq_num(const T * sym, SymbolNode * cur, SymbolNode * stop = NULL) {
    // FIXME: See assign_uniq_num<TopLevelSymbol> for model
    const T * t = NULL;
    // we need to compare the actual symbol name, since it may be
    // aliases as a different name
    String name = sym->name();
    for (; cur != stop; cur = cur->next) {
      if (cur->value && cur->value->name() == name && 
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

  struct DeclHandle {
    typedef ::TypeInfo<DeclHandle> TypeInfo;
    virtual Stmt * complete(Environ & env) = 0;
    virtual void desc(OStream &) = 0;
    virtual ~DeclHandle() {}
  };

}

#endif
