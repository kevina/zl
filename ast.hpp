#ifndef AST__HPP
#define AST__HPP

#include "ct_value.hpp"

#include "environ.hpp"
#include "gc.hpp"
#include "parse.hpp"
#include "fstream.hpp"
#include "symbol_table.hpp"
#include "type.hpp"
#include "expand.hpp"

#include "indent_ostream.hpp"

#include <typeinfo>

#undef NPOS

namespace ast {

  typedef int target_bool;
  typedef int target_int;
  typedef size_t target_size_t;
  typedef ptrdiff_t target_ptrdiff_t;

  class AST;

  struct SyntaxC;
  struct Fun;

  struct ExpContext;

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

  class CompileWriter : public CompileEnviron {
  public:
    OStream * stream;
    enum TargetLang {ZLS, ZLE};
    TargetLang target_lang;
    const Fun * in_fun;
    unsigned indent_level;
    Deps * deps;
    SyntaxGather * syntax_gather;
    bool for_compile_time() {return deps;}
    CompileWriter(TargetLang tl = ZLS);
    void indent() {
      for (int i = 0; i != indent_level; ++i)
        stream->put(' ');
    }
    int vprintf(const char *format, va_list ap) {
      return stream->vprintf(format, ap);
    }

#ifdef __GNUC__
    __attribute__ ((format (printf,2,3)))
#endif
      int printf(const char * format, ...)
    {
      va_list ap;
      va_start(ap, format);
      int res = vprintf(format, ap);
      va_end(ap);
      return res;
    }
    operator OStream & () {return *stream;}
    void open(ParmStr str, const char * mode) {
      FStream * f = new FStream();
      f->open(str, mode);
      stream = f;
    }
    void close() {
      delete stream;
    }

    CompileWriter & operator<< (String str) {
      *stream << str;
      return *this;
    }
    CompileWriter & operator<< (const char * str) {
      *stream << str;
      return *this;
    }
    CompileWriter & operator<< (char c) {
      *stream << c;
      return *this;
    }
    CompileWriter & operator<< (int n) {
      *stream << n;
      return *this;
    }
    CompileWriter & operator<< (unsigned n) {
      *stream << n;
      return *this;
    }
  };

  static inline void copy_val(void * lhs, const void * rhs, const Type * t) {
    memcpy(lhs, rhs, t->size());
  }

  struct AST {
    //typedef ::TypeInfo<AST> TypeInfo;
    //virtual void set_syntax_data(Syntax::Data & d) {
    //  d.type_id = TypeInfo::id;
    //  d.data = this;
    //}
    virtual const char * what() const = 0;
    virtual void desc(OStream & o) const {
      o << what();
    }
    String desc() const  {
      StringBuf buf;
      desc(buf);
      return buf.freeze();
    }
    virtual AST * part(unsigned i) {return 0;}
    const Syntax * syn;
    SourceStr source_str() const {return syn ? syn->str() : SourceStr();}
    AST(const Syntax * p = 0) : syn(p) {}
    void assert_num_args(int p) {
      if (syn->num_args() != p) 
        //abort();
        throw error(syn, "%s: Wrong Number of Arguments", what());
    };
    void assert_num_args(int min, int max) {
      if (syn->num_args() < min || syn->num_args() > max) 
        throw error(0, syn->str(), "%s: Wrong Number of Arguments", what());
    };
    //virtual AST * synself(const Syntax * p, Environ &) = 0;
      // ^^ returns itself, to allow chaining ie 
      //      new Foo(p)->parse(env);
    virtual void finalize(FinalizeEnviron &) = 0;
    virtual void compile_prep(CompileEnviron &) = 0;
    virtual void compile(CompileWriter &) = 0; 
    virtual ~AST() {}
    //void print(OStream & o) const;
  };

  struct Stmt;
  struct EStmt;

  enum LValue { LV_FALSE, LV_EEXP, LV_NORMAL, LV_TOPLEVEL };

  struct Exp : public AST {
    typedef ::TypeInfo<Exp> TypeInfo;
    //struct TypeInfo {
    //  typedef Exp type; 
    //  static const unsigned id = AST::TypeInfo::id | 1;
    //};
    void set_syntax_data(SynEntity::Data & d) {
      d.type_id = TypeInfo::id;
      d.data = this;
    }
    static const int ast_type = 1;
    Exp(const Syntax * p = 0) : AST(p), type(), lvalue(), ct_value_(0) /*, temps()*/ {}
    const Type * type;
    LValue lvalue;
    const CT_Value_Base * ct_value_;
    //Stmt * temps; // temporaries bound to res
    
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
    inline EStmt * as_stmt(); 
  };

  struct ExpLeaf : public Exp {
    ExpLeaf(const Syntax * p = 0) : Exp(p) {}
    void compile_prep(CompileEnviron &) {}
    void finalize(FinalizeEnviron &) {}
  };

  struct Stmt : virtual public AST {
    typedef ::TypeInfo<Stmt> TypeInfo;
  public:
    //struct TypeInfo {
    //  typedef Stmt type; 
    //  static const unsigned id = AST::TypeInfo::id | 2;
    //};
    //void set_syntax_data(Syntax::Data & d) {
    //  d.type_id = TypeInfo::id;
    //  d.data = this;
    //}
    static const int ast_type = 2;
    Stmt(const Syntax * p = 0) : AST(p), next() {}
    Stmt * next;
    inline Exp * as_exp(Environ & env); 
  };

  struct StmtLeaf : public Stmt {
    StmtLeaf(const Syntax * p = 0) : Stmt(p) {}
    void compile_prep(CompileEnviron &) {}
    void finalize(FinalizeEnviron &) {}
  };

  struct FakeAST : public AST {
    FakeAST(const Syntax * p = 0) : AST(p) {}
    void compile(CompileWriter &) {abort();}
    void compile_prep(CompileEnviron &) {abort();}
    void finalize(FinalizeEnviron &) {abort();}
  };

  extern Stmt * const EMPTY_STMT;
  static inline Stmt * empty_stmt() {return EMPTY_STMT;}

  inline void InsrPoint::add(Stmt * to_add) {
    if (to_add == EMPTY_STMT) return;
    *ptr = to_add;
    ptr = &to_add->next;
  }

  struct Literal : public ExpLeaf {
    Literal() {}
    const char * what() const {return "n";}
    //AST * part(unsigned i);
    Literal * parse_self(const Syntax * p, Environ &);
    void compile(CompileWriter & f);
  };

  struct FloatC : public ExpLeaf {
    FloatC() {}
    const char * what() const {return "f";}
    //AST * part(unsigned i);
    FloatC * parse_self(const Syntax * p, Environ &);
    void compile(CompileWriter & f);
  };

  struct StringC : public ExpLeaf {
    StringC() {}
    const char * what() const {return "s";}
    //AST * part(unsigned i);
    String val;
    //String value; // unused at the moment
    StringC * parse_self(const Syntax * p, Environ &);
    void compile(CompileWriter & f);
  };

  struct CharC : public ExpLeaf {
    CharC() {}
    const char * what() const {return "c";}
    //AST * part(unsigned i);
    String orig;
    //char value; // unused at the moment
    CharC * parse_self(const Syntax * p, Environ &);
    void compile(CompileWriter & f);
  };

  struct EIf : public Exp {
    EIf() {}
    EIf(const Syntax * p, Exp * e, Exp * et, Exp * ef)
      : exp(e), if_true(et), if_false(ef) {syn = p;}
    const char * what() const {return "eif";}
    //AST * part(unsigned i) 
    //  {return i == 0 ? exp : i == 1 ? if_true : i == 2 ? if_false : 0;}
    Exp * exp;
    Exp * if_true;
    Exp * if_false;
    EIf * construct(Environ & env);
    void finalize(FinalizeEnviron & env);
    void compile_prep(CompileEnviron & env);
    void compile(CompileWriter & f);
  };

  struct BinOp : public Exp {
    BinOp(const char * name, String op0) : what_(name), op(op0) {}
    //AST * part(unsigned i) {return i == 0 ? lhs : rhs;}
    const char * what_;
    Exp * lhs;
    Exp * rhs;
    String op;
    const char * what() const {return what_;}
    Exp * parse_self(const Syntax * p, Environ & env);
    virtual void resolve(Environ & env) = 0;
    virtual void make_ct_value();
    BinOp * construct(Environ & env) {
      resolve(env);
      make_ct_value();
      return this;
    }
    void finalize(FinalizeEnviron & env);
    void compile_prep(CompileEnviron & env);
    void compile(CompileWriter & f);
  };

  struct UnOp : public Exp {
    UnOp(const char * name, String op0) : what_(name), op(op0) {}
    const char * what_;
    //AST * part(unsigned i) {return exp;}
    Exp * exp;
    String op;
    const char * what() const {return what_;}
    UnOp * parse_self(const Syntax * p, Environ & env);
    virtual void resolve(Environ & env) = 0;
    virtual void make_ct_value();
    UnOp * construct(Environ & env) {
      resolve(env);
      make_ct_value();
      return this;
    }
    void finalize(FinalizeEnviron & env);
    void compile_prep(CompileEnviron & env);
    void compile(CompileWriter & f);
  };

}

namespace ast {
  
  struct OtherType {};

  //
  //
  //

  static inline
  CompileWriter & operator<< (CompileWriter & o, const Symbol * sym) {
    o << sym->uniq_name();
    return o;
  }

  static inline 
  CompileWriter & operator<< (CompileWriter & o, const SymbolName & name) {
    name.to_string(o);
    return o;
  }


  void compile(CompileWriter & o, const SymbolKey & key, 
               const InnerNS * default_ns = DEFAULT_NS);

  static inline 
  CompileWriter & operator<< (CompileWriter & o, const SymbolKey & k) {
    compile(o, k);
    return o;
  }

  static inline 
  CompileWriter & operator<< (CompileWriter & o,  const SymbolKey * k) {
    compile(o, *k);
    return o;
  }

  struct KeyDefNS {
    const SymbolKey & key;
    const InnerNS * default_ns;
    KeyDefNS(const SymbolKey & k, const InnerNS * n = DEFAULT_NS)
      : key(k), default_ns(n) {}
    KeyDefNS(const SymbolKey * k, const InnerNS * n = DEFAULT_NS)
      : key(*k), default_ns(n) {}
  };

  static inline 
  CompileWriter & operator<< (CompileWriter & o, KeyDefNS k) {
    compile(o, k.key, k.default_ns);
    return o;
  }

  static inline 
  CompileWriter & operator<< (CompileWriter & o, AST * v) {
    //printf("COMPILE %s\n", ~v->name());
    v->compile(o);
    return o;
  }

  static inline 
  CompileWriter & operator<< (CompileWriter & o, const Type * v) {
    //printf("COMPILE %s\n", ~v->name());
    switch (o.target_lang) {
    case CompileWriter::ZLS:
      o << zls_print_inst->to_string(*v);
      break;
    case CompileWriter::ZLE:
      o << zle_print_inst->to_string(*v);
      break;
    }
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

  void compile(CompileWriter & o, const SymbolNode * n);

  static inline 
  CompileWriter & operator<< (CompileWriter & o, const SymbolNode * n) {
    compile (o, n);
    return o;
  }

  //
  //
  //

  const CT_Value_Base * cast_ct_value(const Exp * from, const Type * to);

  struct Cast : public Exp {
    Cast(const char * s) : what_(s) {}
    const char * what_;
    const char * what() const {return what_;}
    Cast(Exp * e, const Type * t) 
      : what_("<cast>") {syn = e->syn; exp = e; type = t; ct_value_ = cast_ct_value(e, t);}
    Exp * exp;
    void compile_prep(CompileEnviron&);
    void compile(CompileWriter&);
    void finalize(FinalizeEnviron &); 
    Cast * parse_self(const Syntax * p, Environ &) {abort();}
  };

  //
  // 
  //
  
  struct OverloadedSymbol : virtual public Symbol {
    Symbol * sym;
    const OverloadedSymbol * next;
    OverloadedSymbol(Symbol * s, const OverloadedSymbol * n)
      : sym(s), next(n) {}
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

  struct Block;
  struct TopLevelVarDecl;
  struct Id;

  struct BasicVar : virtual public Symbol {
    //SourceStr str;
    const Syntax * name_p;
    const Type * type;
    const struct CT_Value_Base * ct_value;
    LValue lvalue;
    mutable Id * ids; 
    inline Id * id_for(); 
    virtual const TopLevelVarDecl * top_level() const {return NULL;}
    virtual bool is_temp() const {return false;}
    // as_lvalue returnes the variable as an lvalue exp, normally this
    // means wrapping in an Id, but if we are not a real variable...
    virtual Exp * as_lvalue(Environ & env) const;
    virtual void compile_lvalue(CompileWriter & o) const {o << uniq_name();}
  protected:
    BasicVar() : name_p(), type(), ct_value(), lvalue(LV_NORMAL), ids(NULL) {}
    BasicVar(const Type * t, LValue lv = LV_NORMAL) : name_p(), type(t), ct_value(), lvalue(lv) {}
    //protected:
    //friend VarSymbol * new_var_symbol(SymbolName n, Scope s);
  };

  typedef BasicVar VarSymbol;

  struct OverloadedVar : public VarSymbol, public OverloadedSymbol {
    OverloadedVar(Symbol * s, const OverloadedSymbol * n)
      : OverloadedSymbol(s, n) {}
  };

  static inline
  CompileWriter & operator<< (CompileWriter & o, const VarSymbol * sym) {
    sym->compile_lvalue(o);
    return o;
  }

  struct Declaration : public Stmt {
    virtual Stmt * finish_parse(Environ & env) {return empty_stmt();}
    enum Phase {Normal, Forward, Body};
    virtual void compile(CompileWriter &, Phase) const = 0;
    void compile(CompileWriter & cw) {compile(cw, Normal);}
    void finalize(FinalizeEnviron &) {};
    void compile_prep(CompileEnviron &) {};
    Declaration() {}
  };

  enum StorageClass {SC_NONE, SC_AUTO, SC_STATIC, SC_EXTERN, SC_REGISTER};

  struct VarDeclaration : public Declaration, public BasicVar {
    VarDeclaration() {}
    StorageClass storage_class;
    //VarSymbol * sym;
    void write_storage_class_c(CompileWriter & f) const;
    void write_storage_class(CompileWriter & f) const;
    //void forward_decl(CompileWriter & w) {compile(w, true);}
    Stmt * finish_parse(Environ & env) {abort();}
    virtual void desc(OStream & o) const {
      o << what() << " ";
      key->to_string(o);
    }
  };

  typedef VarDeclaration VarDecl;

  struct TopLevelVarDecl : virtual public VarDecl, public TopLevelSymbol {
    mutable bool deps_closed;
    mutable Deps deps_;       // only valid if deps_closed
    mutable bool for_ct_;     // if false, only valid if deps_closed
    void calc_deps_closure() const;
    const Deps & deps() const {
      if (!deps_closed) calc_deps_closure();
      return deps_;
    }
    bool for_ct() const {
      if (!deps_closed) calc_deps_closure();
      return for_ct_;
    }
    mutable void * ct_ptr; // No relation to ct_value.  Pointer to
                           // compiled symbol, used for proc. macros.
    const TopLevelVarDecl * top_level() const {return this;}
    TopLevelVarDecl() : deps_closed(false), for_ct_(false), ct_ptr() {}
    virtual void desc(OStream & o) const {
      o << what() << " ";
      full_name(o);
    }
  };

  struct Fun : public TopLevelVarDecl {
    Fun() : env_ss(), is_macro(), overload(true) {}
    const char * what() const {return "fun";}
    //AST * part(unsigned i);
    SymbolTable symbols;
    mutable SymbolNode * env_ss;
    mutable bool is_macro;
    Tuple * parms;
    bool overload;
    const Tuple * overloadable() const {return overload ? parms : NULL;}
    const Type * ret_type;
    Block * body;
    bool inline_;
    bool ct_callback;
    bool static_constructor;
    //LabelSymbolTable * labels;
    Stmt * finish_parse(Environ &);
    void compile_prep(CompileEnviron &);
    void compile(CompileWriter & f, Phase) const;
    void finalize(FinalizeEnviron &);
    // internal method, should only be called by parse_fun_forward
    AST * parse_forward_i(const Syntax * p, Environ &, Collect *);
    using TopLevelVarDecl::uniq_name;
    bool uniq_name(OStream & o) const;
  };

  //
  //
  //

  struct Id : public ExpLeaf {
    Id() {}
    Id(const VarSymbol * s) : sym(s) {}
    const char * what() const {return "id";}
    //AST * part(unsigned i) {return new Terminal(parse_->arg(0));}
    const VarSymbol * sym;
    Id * next;
    Id * construct(Environ & env);
    Id * parse_self(const Syntax * p, Environ & env);
    void compile(CompileWriter & f);
  };

  static inline Exp * mk_id(const VarSymbol * s, Environ & env) {
    return s->as_lvalue(env);
  }


  //
  //
  //

  struct TypeDeclaration : public Declaration, virtual public TopLevelSymbol {
    using Declaration::desc;
    virtual void desc(OStream & o) const {
      o << what() << " ";
      full_name(o);
    }
    TypeDeclaration() {}
  };

  struct TypeAlias : public TypeDeclaration, public SimpleType {
    TypeAlias(const Type * st) : SimpleType(st), of(st) {}
    const Type * of;
    const char * what() const {return "talias";}
    void compile(CompileWriter & f, Phase phase) const;
    unsigned size() const {return of->size();}
    unsigned align() const {return of->align();}
    const TypeInst * root_nr() const {return of->root_nr();}
  protected:
    const Type * find_root() {return of->root;}
  };

  struct ForwardTypeDecl : public TypeDeclaration, public SimpleType {
    ForwardTypeDecl(const char * w) : of(), what_(w) {}
    const Type * of;
    const char * what_;
    const char * what() const {return what_;}
    void compile(CompileWriter & f, Phase phase) const;
    unsigned size() const {return of->size();}
    unsigned align() const {return of->align();}
  protected:
    const Type * find_root() {return of->root;}
  };

  struct Member {
    VarSymbol * sym;
    unsigned offset;
    Member(VarSymbol * s) : sym(s), offset(INT_MAX) {}
  };

  struct StructUnion : public TypeDeclaration, public SimpleType {
    enum Which {STRUCT, UNION} which;
    Vector<Member> members;
    StructUnion(Which w) 
      : which(w), have_body(false), env(OTHER), 
        defined(false), size_(NPOS), align_(NPOS), bit_field(false) {}
    bool have_body; // fixme: redundent, fixup "defined" and elinimate
    Environ env;
    void compile(CompileWriter & f, Phase phase) const;
    bool defined;
    unsigned size_;
    unsigned align_;
    bool bit_field;
    unsigned size() const {return size_;}
    unsigned align() const {return align_;}
    const InnerNS * tl_namespace() const {return TAG_NS;}
  };

  struct Struct : public StructUnion {
    Struct() : StructUnion(STRUCT) {}
    const char * what() const {return "struct";}
    const char * tag() const {return "struct";}
    void finalize_hook();
  };

  struct Union : public StructUnion {
    Union() : StructUnion(UNION) {}
    const char * what() const {return "union";}
    const char * tag() const {return "union";}
    void finalize_hook();
  };

  struct Enum : public TypeDeclaration, public Int {
    Enum() 
      : Int(INT_MIN, INT_MAX, Int::UNDEFINED, sizeof(int)), defined(false) {}
    const char * what() const {return "enum";}
    bool defined;
    const Syntax * body;
    struct Member {
      const Syntax * parse;
      VarSymbol * sym;
      CT_Value<target_int> ct_value;
      Member(const Syntax * p, VarSymbol * sym, int v) : parse(p), sym(sym), ct_value(v) {}
    };
    Vector<Member> members;
    //Stmt * parse_self(const Syntax * p, Environ & env);
    void compile(CompileWriter & f, Phase phase) const;
    const char * tag() const {return "enum";}
    void finalize_hook();
    unsigned size() const {return defined ? exact_type->size() : NPOS;}
    unsigned align() const {return defined ? exact_type->align() : NPOS;}
    const InnerNS * tl_namespace() const {return TAG_NS;}
  };

  //
  //
  //

  struct Module : public Declaration, public TopLevelSymbol {
    Module() {}
    SymbolTableBase syms;
    //Vector<const Syntax *> exports; // \...
    const char * what() const {return "module";}
    void compile(CompileWriter &, Phase) const;
    const InnerNS * tl_namespace() const {return OUTER_NS;}
    template <typename T> 
    inline T * find_symbol(const SymbolKey & k) const {
      return ast::find_symbol<T>(k, syms.front, syms.back);
    }
    virtual bool named_outer() const {return true;}
  };

  //
  //
  //

  class UserType : public TypeDeclaration, public SimpleType {
  public:
    UserType() : SimpleType(USER_C), parent(), type(), module(), lt_sizeof_(NULL), defined() {}
    const Type * parent;
    const Type * type;
    Module * module;
    Exp * lt_sizeof_;
    Exp * lt_sizeof() const {return lt_sizeof_;}
    bool defined;
    unsigned size() const {return type ? type->size() : NPOS;}
    unsigned align() const {return type ? type->align() : NPOS;}
    void add_prop(SymbolName n, const Syntax * s) {abort();}
    const Syntax * get_prop(SymbolName n) const {return module->get_prop(n);}
    const char * what() const {return "user_type";}
    using SimpleType::finalize;
    void compile(CompileWriter &, Phase) const;
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
  void parse_stmts(SourceStr str, Environ & env);

  Stmt * parse_top_level(const Syntax * p, Environ & env);
  Stmt * parse_top_level_first_pass(const Syntax * p, Environ & env, Collect & collect);
  Stmt * parse_member(const Syntax * p, Environ & env);
  Stmt * parse_stmt(const Syntax * p, Environ & env);
  Stmt * parse_stmt_decl(const Syntax * p, Environ & env);
  Exp * parse_exp(const Syntax * p, Environ & env);
  Exp * parse_exp_for_type(const Syntax * p, Environ & env);
  target_int parse_ct_value(const Syntax * syn, Environ & env);

  const Syntax * pre_parse_decl(const Syntax * p, Environ & env);

  void compile(TopLevelSymbolTable *, CompileWriter & cw);

  Exp * cast_up(Exp * exp, const Type * type, Environ & env);
  Exp * cast_down(Exp * exp, const UserType * type, Environ & env);

  const Syntax * parse_syntax_c(const Syntax * p);

  //
  // For lack of a better place
  //

  template <typename T, typename Gather, typename ExtraCmp>
  T * lookup_symbol(const Syntax * p, const InnerNS * ns,
                    const SymbolNode * start, const SymbolNode * stop,
                    Strategy strategy, Gather & gather, ExtraCmp & cmp);

  template <typename T, typename Gather>
  T * lookup_symbol(const Syntax * p, const InnerNS * ns,
                    const SymbolNode * start, const SymbolNode * stop,
                    Strategy strategy, Gather & gather) 
  {
    AlwaysTrueExtraCmp cmp;
    return lookup_symbol<T>(p, ns, start, stop, strategy, gather, cmp);
  }

  template <typename T>
  static inline
  T * lookup_symbol(const Syntax * p, const InnerNS * ns,
                    const SymbolNode * start, const SymbolNode * stop = NULL,
                    Strategy strategy = NormalStrategy) 
  {
    NoOpGather gather;
    return lookup_symbol<T>(p, ns, start, stop, strategy, gather);
  }

  inline const InnerNS * lookup_inner_ns(const Syntax * p, const SymbolNode * start) {
    InnerNSBuilder builder;
    for (unsigned i = 1; i < p->num_args(); ++i) {
      builder.add(lookup_symbol<InnerNS::Tag>(p->arg(i), INNER_NS, start));
    }
    return builder.build();
  }

  template <typename T, typename Gather, typename ExtraCmp>
  T * lookup_symbol(const Syntax * p, const InnerNS * ns,
                    const SymbolNode * start, const SymbolNode * stop,
                    Strategy strategy, Gather & gather, ExtraCmp & cmp)
  {
    if (p->simple()) {
      return lookup_symbol<T>(SymbolKey(*p, ns), p->str(), start, stop, strategy, gather, cmp);
    } else if (p->entity<Symbol>()) {
      //printf(">%s\n", typeid(*p->entity<Symbol>()).name());
      if (T * s = dynamic_cast<T *>(p->entity<Symbol>())) {
        return s;
      } else if (const SymbolKeyEntity * s = p->entity<SymbolKeyEntity>()) {
        return lookup_symbol<T>(s->name, p->str(), start, stop, strategy, gather, cmp);
      } else {
        //fprintf(stderr, "Got %s expected %s. (%p)\n", 
        //        typeid(*p->entity<Symbol>()).name(), typeid(T).name(), p);
        throw error(p, "Wrong type of symbol found.  Got %s expected %s. (%p)",
                    typeid(*p->entity<Symbol>()).name(), typeid(T).name(), p);
        //abort(); // FIXME Error Message
      }
    } else if (p->is_a("fluid")) {
      assert_num_args(p, 1);
      const FluidBinding * b = lookup_symbol<FluidBinding>(p->arg(0), ns, start, stop, strategy, gather);
      return lookup_symbol<T>(SymbolKey(b->rebind, ns), p->arg(0)->str(), start, stop, strategy, gather, cmp); // fixme: should I use NoOpGather here?
    } else if (p->is_a("`")) {
      const InnerNS * ns = lookup_inner_ns(p, start);
      return lookup_symbol<T>(p->arg(0), ns, start, stop, strategy, gather, cmp);
    } else if (p->is_a("::")) {
      //printf("w/outer %s %s\n", ~p->to_string(), ~p->sample_w_loc());
      const Module * m = lookup_symbol<Module>(p->arg(0), OUTER_NS, start, stop, strategy);
      //printf("W/OUTER %s %p %p\n", ~p->to_string(), m, m->syms);
      unsigned last = p->num_args() - 1;
      //for (unsigned i = 1; i < last; ++i) {
      //  m = lookup_symbol<Module>(p->arg(1), OUTER_NS, m->syms, NULL, StripMarks);
      //}
      return lookup_symbol<T>(p->arg(last), ns, m->syms.front, m->syms.back, StripMarks, gather, cmp);
    } else {
      //p->print(); printf("?\n");
      return NULL;
    }
  }

  //
  //
  //
  template <typename T>
  const T * find_overloaded_symbol(const Tuple * to_find, const Syntax * id,
                                   const Symbol * sym, Environ & env) 
  {
    if (!sym)
      sym = env.symbols.find<Symbol>(id);
    if (const T * s = dynamic_cast<const T *>(sym))
      return s;
    const OverloadedSymbol * cur = dynamic_cast<const OverloadedSymbol *>(sym);
    if (!cur)
      throw unknown_error(id);
    Vector<const Symbol *> syms;
    while (cur) {
      //IOUT.printf("FOS: %s %p\n", ~cur->name(), cur->sym);
      if (to_find) {
        const Tuple * have = cur->sym->overloadable();
        assert(have);
        if (to_find->parms.size() != have->parms.size()) goto next;
        for (unsigned i = 0; i != to_find->parms.size(); ++i)
          if (to_find->parms[i].type->root != have->parms[i].type->root) goto next;
      }
      syms.push_back(cur->sym);
    next:
      cur = cur->next;
    }
    if (syms.size() == 0)
      return NULL;
    if (syms.size() > 1)
      throw error(id, "Multiple matches for type with parms %s", ~to_find->to_string());
    const T * s = dynamic_cast<const T *>(syms.front());
    if (!s) 
      throw unknown_error(id);
    return s;
  }


  template <typename T>
  const T * lookup_overloaded_symbol(const Tuple * parms, const Syntax * id,
                                     const Symbol * sym, Environ & env) 
  {
    if (!sym)
      sym = env.symbols.lookup<Symbol>(id);
    const T * s = find_overloaded_symbol<T>(parms, id, sym, env);
    if (!s) 
      throw error(id, "Cannot find a match for type with parms %s", ~parms->to_string());
    return s;
  }

  //
  //
  //

  template <typename T> 
  inline T * SymbolTable::lookup(const Syntax * p, const InnerNS * ns, Strategy ms) const {
    return lookup_symbol<T>(p, ns, front, NULL, ms);
  }

  template <typename T>
  static inline
  T * find_symbol(const Syntax * p, const InnerNS * ns,
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
  inline T * SymbolTable::find(const Syntax * p, const InnerNS * ns, Strategy ms) const {
    return find_symbol<T>(p, ns, front, NULL, ms);
  }

  template <typename T> 
  inline T * SymbolTable::find_this_scope(const Syntax * p, const InnerNS * ns) const {
    return find_symbol<Symbol>(p, ns, front, back, ThisScope);
  }

  inline bool SymbolTable::exists(const Syntax * p, const InnerNS * ns) const {
    return find_symbol<Symbol>(p, ns, front, back);
  }

  inline bool SymbolTable::exists_this_scope(const Syntax * p, const InnerNS * ns) const {
    return find_symbol<Symbol>(p, ns, front, back, ThisScope);
  }

  inline void TopLevelSymbolTable::add_defn(Stmt * stmt) {
    //IOUT.printf("ADD DEFN %s: %p %p\n", ~stmt->desc(), this, stmt);
    //IOUT.printf("add defn in %p %p\n", first, last);
    for (Stmt * cur = first; cur; cur = cur->next) {
      assert(cur != stmt);
    }
    if (last) {
      last->next = stmt;
      last = stmt;
    } else {
      first = last = stmt;
    }
    //IOUT.printf("add defn res %p %p\n", first, last);
  }

  inline void TopLevelSymbolTable::move_defn(Stmt * stmt) {
    //IOUT.printf("MOVE DEFN %s: %p %p\n", ~stmt->desc(), this, stmt);
    Stmt * prev = NULL;
    Stmt * cur = first;
    while (cur && cur != stmt)
      prev = cur, cur = cur->next;
    if (cur) {
      // cur == stmt
      prev->next = cur->next;
      cur->next = NULL;
    }
    add_defn(stmt);
  }

  inline void Environ::add_defn(Stmt * defn) {
    //IOUT.printf("E:ADD DEFN %s: %p %p\n", ~defn->desc(), this, defn);
    assert(top_level_symbols);
    top_level_symbols->add_defn(defn);
  }
  
  inline void Environ::move_defn(Stmt * defn) {
    assert(top_level_symbols);
    top_level_symbols->move_defn(defn);
  }

  Exp * to_ref(Exp *, Environ &);
  Exp * from_ref(Exp *, Environ &);
  Exp * make_temp(Exp *, Environ &);

  void include_file(String file_name, Environ & env);
  void import_file(String file_name, Environ & env);

  void init_ct_var(const char * n, void * * ptr, Environ & env);

}

#endif
