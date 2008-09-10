#include "environ.hpp"
#include "gc.hpp"
#include "hash-t.hpp" //FIXME
#include "parse.hpp"
#include "fstream.hpp"
#include "symbol_table.hpp"
#include "type.hpp"
#include "expand.hpp"

#include <typeinfo>
#include <map>

#undef NPOS

namespace ast {

#if 0
  struct Scope {
    TypeSymbolTable * types;
    VarSymbolTable  * vars;
    ResolveEnviron() 
      : types(), vars() {}
    ResolveEnviron(TypeSymbolTable * t, 
                   VarSymbolTable  * v)
      : types(t), vars(v) {}
  };
#endif

  struct PrepEvalEnviron : public gc {
#if 0
    SymbolTable<const Type *> * types;
    SymbolTable<Symbol *> * sym;
    Frame * frame;
    ResolveEnviron() {
      frame = new Frame();
    }
    ResolveEnviron new_frame(const Scope & scp) {
      ResolveEnviron env = *this;
      env       = scp;
      env.frame = new Frame();
      return env;
    }
#endif
  };

  class AST;

  struct ExecEnviron : public gc {
    union {
      struct {
        char * static_ptr;
        char * frame_ptr;
      };
      char * * var_ptrs[2];
    };
    unsigned * cur_frame_size;
    char * stack_ptr;
    char * end_of_stack;
    template <typename T> T & static_var(unsigned i)
      {return *reinterpret_cast<T *>(static_ptr + i);}
    template <typename T> T & local_var(unsigned i)
      {return *reinterpret_cast<T *>(frame_ptr + i);}
    //template <typename T> T & var(const VarLoc & l)
    //  {return *reinterpret_cast<T *>(var_ptrs[l.scope][l.offset]);}
    template <typename T> inline T & ret(const AST *);
    void * local_var(unsigned i)
      {return static_cast<void *>(static_ptr + i);}
    //void * var(const VarLoc & l) 
    //  {return static_cast<void *>(var_ptrs[l.scope][l.offset]);}
    inline void * ret(const AST * exp);
    //template <typename T> T * push_tmp(const Type * t) {
    //  T * p = reinterpret_cast<T *>(stack_ptr);
    //  stack_ptr += t->size;
    //  assert(stack_ptr <= end_of_stack);
    //}
    //template <typename T> T * pop_tmp(const Type * t) {
    //  T * p = reinterpret_cast<T *>(stack_ptr);
    //  stack_ptr -= t->size;
    //}
    //void pop_to(void * p) {
    //  stack_ptr = static_cast<char *>(p);
    //}
    //ExecEnviron new_frame(unsigned offset, unsigned sz) {
    //  ExecEnviron ret;
    //  ret.static_ptr = static_ptr;
    //  ret.frame_ptr = stack_ptr - offset;
    //  ret.stack_ptr = stack_ptr + sz - offset;
    //  ret.end_of_stack = end_of_stack;
    //  assert(ret.stack_ptr <= end_of_stack);
    //  return ret;
    //}
    ExecEnviron new_frame(unsigned start_at) {
      ExecEnviron ret;
      ret.static_ptr = static_ptr;
      ret.stack_ptr = ret.frame_ptr = stack_ptr + start_at;
      ret.end_of_stack = end_of_stack;
      assert(ret.stack_ptr <= end_of_stack);
      return ret;
    }
    void alloc(unsigned i) {
      stack_ptr += i;
      assert(stack_ptr <= end_of_stack);
    }
  };

  struct CompileEnviron {};

  struct FinalizeEnviron {
    SymbolNode * fun_symbols;
  };
  
  class CompileWriter : public FStream {
  public:
    unsigned indent_level;
    Deps * deps;
    bool for_compile_time() {return deps;}
    CompileWriter() : indent_level(0), deps() {}
    void indent() {
      for (int i = 0; i != indent_level; ++i)
        *this << ' ';
    }
  };

  static inline void copy_val(void * lhs, const void * rhs, const Type * t) {
    memcpy(lhs, rhs, t->size());
  }


  struct BreakException {};
  struct ReturnException {};

  
  struct CT_Value_Base : public gc {
    virtual void to_string(const AST *, OStream &) const = 0;
    virtual ~CT_Value_Base() {}
  };

  template <typename T> struct CT_Value : public CT_Value_Base {
    void to_string(const AST *, OStream & o) const;
    virtual T value(const AST *) const = 0;
  };

  struct AST : public Entity {
    String what_;
    String what() const {return what_;}
    virtual AST * part(unsigned i) {return 0;}
    const Syntax * parse_;
    const Type * type;
    bool lvalue;
    unsigned return_offset; // for rhs values: NPOS if not LHS
    VarLoc addr;            // for lhs values
    AST(String n, const Syntax * p = 0) : what_(n), parse_(p), type(), lvalue(false), return_offset(NPOS), addr(), ct_value_(0) {}
    void assert_num_args(int p) {
      if (parse_->num_args() != p) 
        //abort();
        throw error(parse_, "%s: Wrong Number of Arguments", ~what_);
    };
    void assert_num_args(int min, int max) {
      if (parse_->num_args() < min || parse_->num_args() > max) 
        throw error(0, parse_->str(), "%s: Wrong Number of Arguments", ~what_);
    };
    //virtual AST * parse_self(const Syntax * p, Environ &) = 0;
      // ^^ returns itself, to allow chaining ie 
      //      new Foo(p)->parse(env);
    virtual void finalize(FinalizeEnviron &) = 0;
    virtual void prep_eval(PrepEvalEnviron &) {abort();}
    virtual void eval(ExecEnviron &) {abort();}
    virtual void compile(CompileWriter &, CompileEnviron &) = 0; 
    virtual AST * resolve_to(const Type * type, Environ & env, 
                             TypeRelation::CastType rule = TypeRelation::Implicit) {
      return env.type_relation->resolve_to(this, type, env, rule);
    }
    virtual ~AST() {}
    //void print(OStream & o) const;

    const CT_Value_Base * ct_value_;
    template <typename T> 
    T ct_value() const {
      if (!ct_value_) throw error(parse_, "\"%s\" Can not be used in constant-expression", ~what_);
      return dynamic_cast<const CT_Value<T> *>(ct_value_)->value(this);
    }
  };

  struct OtherType {};

  template <typename T>
  T & ExecEnviron::ret(const AST * exp) {
    return local_var<T>(exp->return_offset);
  }
  
  void * ExecEnviron::ret(const AST * exp) {
    return local_var(exp->return_offset);
  }

  //
  //
  //

  static inline
  CompileWriter & operator<< (CompileWriter & o, const Symbol * sym) {
    sym->uniq_name(o);
    return o;
  }

  static inline 
  CompileWriter & operator<< (CompileWriter & o, const char * str) {
    static_cast<FStream &>(o) << str;
    return o;
  }

  static inline 
  CompileWriter & operator<< (CompileWriter & o, AST * v) {
    //printf("COMPILE %s\n", ~v->name());
    CompileEnviron dummy;
    v->compile(o, dummy);
    return o;
  }

  struct adj_indent {
    int adj;
    CompileWriter * o;
    explicit adj_indent(int a) : adj(a) {}
  };

  static inline 
  adj_indent operator<< (CompileWriter & o, adj_indent i) {
    i.o = &o;
    return i;
  }

  template <typename T>
  static inline 
  CompileWriter & operator<< (adj_indent i, T v) {
    i.o->indent_level += i.adj;
    *i.o << v;
    i.o->indent_level -= i.adj;
    return *i.o;
  }

  struct indent_ {};
  static const indent_ indent = indent_();

  static inline 
  CompileWriter & operator<< (CompileWriter & o, indent_) {
    o.indent();
    return o;
  }

  //
  //
  //

  const CT_Value_Base * cast_ct_value(const Type * f, const Type * t);

  struct Cast : public AST {
    Cast(String s) : AST(s) {}
    Cast(AST * e, const Type * t) 
      : AST("<cast>") {exp = e; type = t; ct_value_ = cast_ct_value(exp->type, t);}
    AST * exp;
    void compile(CompileWriter&, CompileEnviron&);
    void finalize(FinalizeEnviron &); 
    AST * parse_self(const Syntax * p, Environ &) {abort();}
  };

  //
  //
  //

  /*
  struct Parm : public gc {
    String name;
    const Type * type;
    const Syntax * type_parse;
    Parm(String n, const Syntax * t) : name(n), type(), type_parse(t) {}
  };
  */

  struct ForSecondPass {
    virtual void finish_parse(Environ & env) = 0;
    virtual ~ForSecondPass() {}
  };

  struct Block;
  struct VarSymbol;

  struct Declaration : public AST {
    enum Phase {Normal, Forward, Body};
    virtual void compile(CompileWriter &, Phase) const = 0;
    void compile(CompileWriter & cw, CompileEnviron &) {compile(cw, Normal);}
    Declaration(String n) : AST(n) {}
  };

  struct VarDeclaration : public Declaration, public ForSecondPass {
    VarDeclaration(String n) : Declaration(n) {}
    enum StorageClass {NONE, AUTO, STATIC, EXTERN, REGISTER};
    StorageClass storage_class;
    bool inline_;
    VarSymbol * sym;
    mutable bool deps_closed;
    mutable Deps deps_;       // only valid if deps_closed
    mutable bool for_ct_;     // if false, only valid if deps_closed
    bool ct_callback;
    bool static_constructor;
    void parse_flags(const Syntax * p);
    void write_flags(CompileWriter & f) const;
    //void forward_decl(CompileWriter & w) {compile(w, true);}
    void calc_deps_closure() const;
    const Deps & deps() const {
      if (!deps_closed) calc_deps_closure();
      return deps_;
    }
    bool for_ct() const {
      if (!deps_closed) calc_deps_closure();
      return for_ct_;
    }
    void finish_parse(Environ & env) {abort();}
  };
  
  typedef Vector<ForSecondPass *> Collect;

  struct Fun : public VarDeclaration {
    Fun() : VarDeclaration("fun") {}
    //AST * part(unsigned i);
    SymbolKey name;
    SymbolTable symbols;
    const Tuple * parms;
    const Type * ret_type;
    Block * body;
    //LabelSymbolTable * labels;
    unsigned frame_offset;
    unsigned frame_size;
    void finish_parse(Environ &);
    void eval(ExecEnviron & env);
    void compile(CompileWriter & f, Phase) const;
    void finalize(FinalizeEnviron &);
    
    // internal method, should only be called by parse_fun_forward
    AST * parse_forward_i(const Syntax * p, Environ &, Collect &); 
  };

  struct Literal : public AST {
    Literal() : AST("literal") {}
    //AST * part(unsigned i);
    //long long value;
    AST * parse_self(const Syntax * p, Environ &);
    //void eval(ExecEnviron & env);
    void compile(CompileWriter & f, CompileEnviron &);
    void finalize(FinalizeEnviron &) {}
  };

  struct FloatC : public AST {
    FloatC() : AST("float") {}
    //AST * part(unsigned i);
    //long double value;
    AST * parse_self(const Syntax * p, Environ &);
    void compile(CompileWriter & f, CompileEnviron &);
    void finalize(FinalizeEnviron &) {}
  };

  struct StringC : public AST {
    StringC() : AST("string") {}
    //AST * part(unsigned i);
    String orig;
    String value; // unused at the moment
    AST * parse_self(const Syntax * p, Environ &);
    void compile(CompileWriter & f, CompileEnviron &);
    void finalize(FinalizeEnviron &) {}
  };

  struct CharC : public AST {
    CharC() : AST("char") {}
    //AST * part(unsigned i);
    String orig;
    char value; // unused at the moment
    AST * parse_self(const Syntax * p, Environ &);
    void compile(CompileWriter & f, CompileEnviron &);
    void finalize(FinalizeEnviron &) {}
  };

  struct Empty : public AST {
    Empty() : AST("empty") {}
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(0);
      type = env.void_type();
      return this;
    }
    void eval(ExecEnviron &) {}
    void compile(CompileWriter & f, CompileEnviron &) {
      // do absolutely nothing
    }
    void finalize(FinalizeEnviron &) {}
  };

  //
  //
  //

  struct VarSymbol : virtual public Symbol {
    //SourceStr str;
    const Type * type;
    const struct CT_Value_Base * ct_value;
    mutable void * ct_ptr; // ...
  protected:
    friend VarSymbol * new_var_symbol(SymbolName n, Scope s);
    VarSymbol(String n) : ct_value(), ct_ptr() {name = n;}
  };

  struct OtherVarSymbol : public VarSymbol, public OtherSymbol {
    OtherVarSymbol(String n, bool mangle) : VarSymbol(n), OtherSymbol(mangle ? NPOS : 0) {}
  };

  struct TopLevelVarSymbol : public VarSymbol, public TopLevelSymbol {
    const VarDeclaration * decl;
    TopLevelVarSymbol(String n, const VarDeclaration * d, 
                      bool mangle, TopLevelSymbol * w) 
      : VarSymbol(n), TopLevelSymbol(mangle ? NPOS : 0, d, w), decl(d) {}
  };

  struct LexicalVarSymbol : public VarSymbol, public LexicalSymbol {
    LexicalVarSymbol(String n) : VarSymbol(n) {}
  };

  VarSymbol * new_var_symbol(SymbolName n, Scope s = OTHER, 
                             const VarDeclaration * d = NULL, 
                             TopLevelSymbol * w = NULL);

  struct LabelSymbol : public Symbol {};

  struct NormalLabelSymbol : public LabelSymbol {
    mutable unsigned num;
    NormalLabelSymbol(String n) : num() {name = n;}
    void uniq_name(OStream & o) const {
      o.printf("%s$$%u", ~name, num);
    }
    void add_to_env(const SymbolKey & k, Environ &, Pass pass) const;
    void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const {
      assign_uniq_num<NormalLabelSymbol>(self, stop);
    }
  };
  
  struct LocalLabelSymbol : public LabelSymbol {
    mutable unsigned num;
    LocalLabelSymbol(String n) : num() {name = n;}
    void uniq_name(OStream & o) const {
      o.printf("%s$%u", ~name, num);
    }
    void make_unique(SymbolNode * self, SymbolNode * stop) const {
      assign_uniq_num<LocalLabelSymbol>(self, stop);
    }
  };

  //
  //
  //

  //template <typename T>
  //static inline void resolve_to(Environ & env, AST * & exp, const Type * type) {
  //exp = exp->resolve_to(type, env);
    //exp = static_cast<T *>(env.type_relation->resolve_to(static_cast<AST *>(exp), type, env));
  //}

  //int ct_value(const Syntax * p, Environ &);

  AST * parse_top(const Syntax * p);
  AST * parse_top(const Syntax * p, Environ & env);

  AST * parse_top_level(const Syntax * p, Environ & env);
  AST * parse_top_level_first_pass(const Syntax * p, Environ & env, Collect & collect);
  AST * parse_member(const Syntax * p, Environ & env);
  AST * parse_stmt(const Syntax * p, Environ & env);
  AST * parse_stmt_decl(const Syntax * p, Environ & env);
  AST * parse_exp(const Syntax * p, Environ & env);

  void compile(const Vector<const TopLevelSymbol *> &, CompileWriter & cw);

  AST * cast_up(AST * exp, const Type * type, Environ & env);


  //
  // For lack of a better place
  //

  template <typename T, typename Gather, typename ExtraCmp>
  const T * lookup_symbol(const Syntax * p, const InnerNS * ns,
                          const SymbolNode * start, const SymbolNode * stop,
                          Strategy strategy, Gather & gather, ExtraCmp & cmp);
  template <typename T, typename Gather>
  const T * lookup_symbol(const Syntax * p, const InnerNS * ns,
                          const SymbolNode * start, const SymbolNode * stop,
                          Strategy strategy, Gather & gather) 
  {
    AlwaysTrueExtraCmp cmp;
    return lookup_symbol<T>(p, ns, start, stop, strategy, gather, cmp);
  }
  template <typename T>
  static inline
  const T * lookup_symbol(const Syntax * p, const InnerNS * ns,
                          const SymbolNode * start, const SymbolNode * stop = NULL,
                          Strategy strategy = NormalStrategy) 
  {
    NoOpGather gather;
    return lookup_symbol<T>(p, ns, start, stop, strategy, gather);
  }
  template <typename T, typename Gather, typename ExtraCmp>
  const T * lookup_symbol(const Syntax * p, const InnerNS * ns,
                          const SymbolNode * start, const SymbolNode * stop,
                          Strategy strategy, Gather & gather, ExtraCmp & cmp)
  {
    if (p->simple()) {
      return lookup_symbol<T>(SymbolKey(*p, ns), p->str(), start, stop, strategy, gather, cmp);
    } else if (p->entity()) {
      //printf(">%s\n", typeid(*p->entity()).name());
      if (const T * s = dynamic_cast<const T *>(p->entity())) {
        return s;
      } else if (const SymbolKeyEntity * s = dynamic_cast<const SymbolKeyEntity *>(p->entity())) {
        return lookup_symbol<T>(s->name, p->str(), start, stop, strategy, gather, cmp);
      } else {
        throw error(p, "Wrong type of symbol found...");
        //abort(); // FIXME Error Message
      }
    } else if (p->is_a("fluid")) {
      assert_num_args(p, 1);
      const FluidBinding * b = lookup_symbol<FluidBinding>(p->arg(0), ns, start, stop, strategy, gather);
      return lookup_symbol<T>(SymbolKey(b->rebind, ns), p->arg(0)->str(), start, stop, strategy);
    } else if (p->is_a("w/inner")) {
      assert_num_args(p, 2);
      const InnerNS * ns = lookup_symbol<InnerNS>(p->arg(1), INNER_NS, start);
      return lookup_symbol<T>(p->arg(0), ns, start, stop, strategy);
    } else if (p->is_a("w/outer")) {
      const Module * m = lookup_symbol<Module>(p->arg(0), OUTER_NS, start, stop, strategy);
      //printf("DIRECT %p %p\n", m, m->syms);
      unsigned last = p->num_args() - 1;
      //for (unsigned i = 1; i < last; ++i) {
      //  m = lookup_symbol<Module>(p->arg(1), OUTER_NS, m->syms, NULL, StripMarks);
      //}
      return lookup_symbol<T>(p->arg(last), ns, m->syms, NULL, StripMarks, gather);
    } else {
      p->print(); printf("?\n");
      return NULL;
    }
  }

  template <typename T> 
  inline const T * SymbolTable::lookup(const Syntax * p, const InnerNS * ns) {
    return lookup_symbol<T>(p, ns, front);
  }

  template <typename T>
  static inline
  const T * find_symbol(const Syntax * p, const InnerNS * ns,
                        const SymbolNode * start, const SymbolNode * stop = NULL,
                        Strategy strategy = NormalStrategy) 
  {
    try {
      return lookup_symbol<T>(p, ns, start, stop, strategy);
    } catch (Error * err) {
      //printf("note: %s\n", err->message().c_str());
      return NULL;
    }
  }

  template <typename T> 
  inline const T * SymbolTable::find(const Syntax * p, const InnerNS * ns) {
    return find_symbol<T>(p, ns, front);
  }

  inline bool SymbolTable::exists_this_scope(const Syntax * p, const InnerNS * ns) {
    return find_symbol<Symbol>(p, ns, front, back, ThisScope);
  }
  

}

#if 0


#endif
