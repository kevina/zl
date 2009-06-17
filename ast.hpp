#ifndef AST__HPP
#define AST__HPP

#include "ct_value.hpp"

#include "environ.hpp"
#include "gc.hpp"
#include "hash-t.hpp" //FIXME
#include "parse.hpp"
#include "fstream.hpp"
#include "symbol_table.hpp"
#include "type.hpp"
#include "expand.hpp"

#include <typeinfo>

#undef NPOS

namespace ast {

  class AST;

  struct SyntaxC;
  struct Fun;

  struct CompileEnviron {
    struct ForMacroSepC {
      Vector<Fun *> macro_funs;
      Vector<SyntaxC *> syntaxes;
    };
    ForMacroSepC * for_macro_sep_c;
    CompileEnviron() : for_macro_sep_c() {}
  };

  struct FinalizeEnviron {
    SymbolNode * fun_symbols;
  };

  class CompileWriter : public FStream, public CompileEnviron {
  public:
    bool to_c;
    const Fun * in_fun;
    unsigned indent_level;
    Deps * deps;
    bool for_compile_time() {return deps;}
    CompileWriter(bool to_c0 = false) : to_c(to_c0), in_fun(), indent_level(0), deps() {}
    void indent() {
      for (int i = 0; i != indent_level; ++i)
        *this << ' ';
    }
  };

  static inline void copy_val(void * lhs, const void * rhs, const Type * t) {
    memcpy(lhs, rhs, t->size());
  }

  struct AST : public Entity {
    String what_;
    String what() const {return what_;}
    virtual AST * part(unsigned i) {return 0;}
    const Syntax * parse_;
    AST(String n, const Syntax * p = 0) : what_(n), parse_(p) {}
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
    virtual void compile_prep(CompileEnviron &) = 0;
    virtual void compile_c(CompileWriter &) = 0; 
    virtual void compile(CompileWriter &) = 0; 
    virtual ~AST() {}
    //void print(OStream & o) const;
  };

  struct Stmt;

  struct Exp : public AST {
    static const int ast_type = 1;
    Exp(String n, const Syntax * p = 0) : AST(n,p), type(), lvalue(false), ct_value_(0), temps() {}
    const Type * type;
    int lvalue; // 0 false, 1 true, 2 true and addr ct_value
    const CT_Value_Base * ct_value_;
    Stmt * temps; // temporaries bound to res

    virtual Exp * resolve_to(const Type * type, Environ & env, 
                             TypeRelation::CastType rule = TypeRelation::Implicit) {
      return env.type_relation->resolve_to(this, type, env, rule);
    }
    Exp * to_effective(Environ & env) {
      return env.type_relation->to_effective(this, env);
    }
    Exp * def_arg_prom(Environ & env) {
      return env.type_relation->def_arg_prom(this, env);
    }

    template <typename T> T real_ct_value() const;
    template <typename T> T ct_value() const {
      return real_ct_value<typename CT_Type<T>::type>();
    }
    template <typename T> T ct_value_direct() const {
      return dynamic_cast<const CT_Value<T> *>(ct_value_)->val;
    }
  };

  struct ExpLeaf : public Exp {
    ExpLeaf(String n, const Syntax * p = 0) : Exp(n,p) {}
    void compile_prep(CompileEnviron &) {}
    void finalize(FinalizeEnviron &) {}
  };

  struct Stmt : public AST {
  public:
    static const int ast_type = 2;
    Stmt(String n, const Syntax * p = 0) : AST(n,p), next() {}
    Stmt * next;
  };

  struct StmtLeaf : public Stmt {
    StmtLeaf(String n, const Syntax * p = 0) : Stmt(n,p) {}
    void compile_prep(CompileEnviron &) {}
    void finalize(FinalizeEnviron &) {}
  };

  inline void InsrPoint::add(Stmt * to_add) {
    *ptr = to_add;
    ptr = &to_add->next;
  }

  struct FakeAST : public AST {
    FakeAST(String n, const Syntax * p = 0) : AST(n,p) {}
    void compile_c(CompileWriter &) {abort();}
    void compile(CompileWriter &) {abort();}
    void compile_prep(CompileEnviron &) {abort();}
    void finalize(FinalizeEnviron &) {abort();}
  };

  struct Empty : public StmtLeaf {
    Empty() : StmtLeaf("empty") {}
    Empty * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(0);
      return this;
    }
    void compile_c(CompileWriter & f) {
      // do absolutely nothing
    }
    void compile(CompileWriter & f) {
      // do absolutely nothing
    }
  };

  struct Literal : public ExpLeaf {
    Literal() : ExpLeaf("n") {}
    //AST * part(unsigned i);
    Literal * parse_self(const Syntax * p, Environ &);
    void compile_c(CompileWriter & f);
    void compile(CompileWriter & f);
  };

  struct FloatC : public ExpLeaf {
    FloatC() : ExpLeaf("f") {}
    //AST * part(unsigned i);
    FloatC * parse_self(const Syntax * p, Environ &);
    void compile_c(CompileWriter & f);
    void compile(CompileWriter & f);
  };

  struct StringC : public ExpLeaf {
    StringC() : ExpLeaf("s") {}
    //AST * part(unsigned i);
    String orig;
    //String value; // unused at the moment
    StringC * parse_self(const Syntax * p, Environ &);
    void compile_c(CompileWriter & f);
    void compile(CompileWriter & f);
  };

  struct CharC : public ExpLeaf {
    CharC() : ExpLeaf("c") {}
    //AST * part(unsigned i);
    String orig;
    //char value; // unused at the moment
    CharC * parse_self(const Syntax * p, Environ &);
    void compile_c(CompileWriter & f);
    void compile(CompileWriter & f);
  };

  struct EIf : public Exp {
    EIf() : Exp("eif") {}
    //AST * part(unsigned i) 
    //  {return i == 0 ? exp : i == 1 ? if_true : i == 2 ? if_false : 0;}
    Exp * exp;
    Exp * if_true;
    Exp * if_false;
    EIf * parse_self(const Syntax * p, Environ & env);
    void finalize(FinalizeEnviron & env);
    void compile_prep(CompileEnviron & env);
    void compile_c(CompileWriter & f);
    void compile(CompileWriter & f);
  };

  struct BinOp : public Exp {
    BinOp(String name, String op0) : Exp(name), op(op0) {}
    //AST * part(unsigned i) {return i == 0 ? lhs : rhs;}
    Exp * lhs;
    Exp * rhs;
    String op;
    BinOp * parse_self(const Syntax * p, Environ & env);
    virtual void resolve(Environ & env) = 0;
    virtual void make_ct_value();
    void finalize(FinalizeEnviron & env);
    void compile_prep(CompileEnviron & env);
    void compile_c(CompileWriter & f);
    void compile(CompileWriter & f);
  };

  struct UnOp : public Exp {
    UnOp(String name, String op0) : Exp(name), op(op0) {}
    //AST * part(unsigned i) {return exp;}
    Exp * exp;
    String op;
    UnOp * parse_self(const Syntax * p, Environ & env);
    virtual void resolve(Environ & env) = 0;
    virtual void make_ct_value();
    void finalize(FinalizeEnviron & env);
    void compile_prep(CompileEnviron & env);
    void compile_c(CompileWriter & f);
    void compile(CompileWriter & f);
  };

}

inline Syntax::Syntax(const ast::AST * e)
  : what_("<entity>"), str_(e->parse_->str()), d(), repl(), entity_(const_cast<ast::AST*>(e)) {}

namespace ast {
  
  struct OtherType {};

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
    if (o.to_c)
      v->compile_c(o);
    else
      v->compile(o);
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

  const CT_Value_Base * cast_ct_value(const Exp * from, const Type * to);

  struct Cast : public Exp {
    Cast(String s) : Exp(s) {}
    Cast(Exp * e, const Type * t) 
      : Exp("<cast>") {parse_ = e->parse_; exp = e; type = t; ct_value_ = cast_ct_value(e, t);}
    Exp * exp;
    void compile_prep(CompileEnviron&);
    void compile_c(CompileWriter&);
    void compile(CompileWriter&);
    void finalize(FinalizeEnviron &); 
    Cast * parse_self(const Syntax * p, Environ &) {abort();}
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

  struct Declaration : public Stmt {
    enum Phase {Normal, Forward, Body};
    virtual void compile_c(CompileWriter &, Phase) const = 0;
    virtual void compile(CompileWriter &, Phase) const = 0;
    void compile_c(CompileWriter & cw) {compile_c(cw, Normal);}
    void compile(CompileWriter & cw) {compile(cw, Normal);}
    Declaration(String n) : Stmt(n) {}
  };

  struct VarDeclaration : public Declaration, public ForSecondPass {
    VarDeclaration(String n) : Declaration(n) {}
    enum StorageClass {NONE, AUTO, STATIC, EXTERN, REGISTER};
    StorageClass storage_class;
    VarSymbol * sym;
    mutable bool deps_closed;
    mutable Deps deps_;       // only valid if deps_closed
    mutable bool for_ct_;     // if false, only valid if deps_closed
    bool inline_;
    bool ct_callback;
    bool static_constructor;
    void parse_flags(const Syntax * p);
    void write_flags_c(CompileWriter & f) const;
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
    Fun() : VarDeclaration("fun"), env_ss(), is_macro() {}
    //AST * part(unsigned i);
    SymbolKey name;
    SymbolTable symbols;
    SymbolNode * env_ss;
    mutable bool is_macro;
    const Tuple * parms;
    const Type * ret_type;
    Block * body;
    //LabelSymbolTable * labels;
    unsigned frame_offset;
    unsigned frame_size;
    void finish_parse(Environ &);
    void compile_prep(CompileEnviron &);
    void compile_c(CompileWriter & f, Phase) const;
    void compile(CompileWriter & f, Phase) const;
    void finalize(FinalizeEnviron &);
    // internal method, should only be called by parse_fun_forward
    AST * parse_forward_i(const Syntax * p, Environ &, Collect &); 
  };

  //
  //
  //

  struct VarSymbol : virtual public Symbol {
    //SourceStr str;
    const Type * type;
    const struct CT_Value_Base * ct_value;
  protected:
    friend VarSymbol * new_var_symbol(SymbolName n, Scope s);
    VarSymbol(String n) : ct_value() /*, ct_ptr()*/ {name = n;}
  };

  struct OtherVarSymbol : public VarSymbol, public OtherSymbol {
    OtherVarSymbol(String n, bool mangle) : VarSymbol(n), OtherSymbol(mangle ? NPOS : 0) {}
  };

  struct TopLevelVarSymbol : public VarSymbol, public TopLevelSymbol {
    mutable void * ct_ptr; // no relation to ct_value, pointer to
                           // compiled symbol, used for proc. macros
    const VarDeclaration * decl;
    AST * init;
    AST * cleanup;
    TopLevelVarSymbol(String n, const VarDeclaration * d, 
                      bool mangle, TopLevelSymbol * w) 
      : VarSymbol(n), TopLevelSymbol(mangle ? NPOS : 0, d, w), ct_ptr(), decl(d), init(), cleanup() {}
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
    void add_to_env(const SymbolKey & k, Environ &) const;
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
  void parse_stmts_raw(SourceStr, Environ & env);
  void parse_stmts(const Syntax * p, Environ & env);

  Stmt * parse_top_level(const Syntax * p, Environ & env);
  Stmt * parse_top_level_first_pass(const Syntax * p, Environ & env, Collect & collect);
  Stmt * parse_member(const Syntax * p, Environ & env);
  Stmt * parse_stmt(const Syntax * p, Environ & env);
  Stmt * parse_stmt_decl(const Syntax * p, Environ & env);
  Exp * parse_exp(const Syntax * p, Environ & env);

  const Syntax * pre_parse_decl(const Syntax * p, Environ & env);

  void compile_c(const Vector<const TopLevelSymbol *> &, CompileWriter & cw);
  void compile(const Vector<const TopLevelSymbol *> &, CompileWriter & cw);

  Exp * cast_up(Exp * exp, const Type * type, Environ & env);

  const Syntax * parse_syntax_c(const Syntax * p);

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
      //printf("w/outer %s %s\n", ~p->to_string(), ~p->sample_w_loc());
      const Module * m = lookup_symbol<Module>(p->arg(0), OUTER_NS, start, stop, strategy);
      //printf("W/OUTER %s %p %p\n", ~p->to_string(), m, m->syms);
      unsigned last = p->num_args() - 1;
      //for (unsigned i = 1; i < last; ++i) {
      //  m = lookup_symbol<Module>(p->arg(1), OUTER_NS, m->syms, NULL, StripMarks);
      //}
      return lookup_symbol<T>(p->arg(last), ns, m->syms, NULL, StripMarks, gather);
    } else {
      //p->print(); printf("?\n");
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

  inline bool SymbolTable::exists(const Syntax * p, const InnerNS * ns) {
    return find_symbol<Symbol>(p, ns, front, back);
  }

  inline bool SymbolTable::exists_this_scope(const Syntax * p, const InnerNS * ns) {
    return find_symbol<Symbol>(p, ns, front, back, ThisScope);
  }

  Exp * to_ref(Exp *, Environ &);
  Exp * from_ref(Exp *, Environ &);

  typedef int target_bool;
  typedef int target_int;
  typedef size_t target_size_t;
  typedef ptrdiff_t target_ptrdiff_t;
}

#endif
