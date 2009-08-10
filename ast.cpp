#include <assert.h>
#include <stdio.h>

#include <cxxabi.h>

#include <algorithm>

#include "ast.hpp"
#include "parse.hpp"
#include "parse_op.hpp"
#include "parse_decl.hpp"
#include "expand.hpp"

#include "syntax_gather.hpp"

#include "ct_value-impl.hpp"
#include "hash-t.hpp"

// from peg.hpp
const Syntax * parse_str(String what, SourceStr str, const Replacements * repls = 0);

// each AST node pushes the result on the top of the stack
//   unless the type is void

#define SYN new Syntax

namespace ast {

  CompileWriter::CompileWriter(TargetLang tl) 
    : target_lang(tl), in_fun(), indent_level(0), deps(), syntax_gather() 
  {
    if (tl == ZLE)
      syntax_gather = new SyntaxGather;
  }

  void compile(CompileWriter & o, const SymbolKey & key, const InnerNS * default_ns)
  {
    assert(!key.marks);
    if (key.ns && key.ns != default_ns) {
      assert(o.target_lang == CompileWriter::ZLE);
      o << "(` ";
      key.SymbolName::to_string(o);
      o << ' ' << key.ns->name() << ")";
    } else {
      o << key.name;
    }
  }

  void compile(CompileWriter & o, const SymbolNode * n) {
    AST * decl = NULL;
    if (n->alias()) {
      const TopLevelSymbol * tl = dynamic_cast<const TopLevelSymbol *>(n->value);
      if (!tl) goto unknown;
      SymbolKey uniq_key = tl->uniq_name();
      uniq_key.ns = tl->tl_namespace();
      o << indent << "(alias " << n->key << " " << uniq_key << ")\n";
    } else if ((decl = dynamic_cast<AST *>(n->value))) {
      o << decl;
      //o << "\n";
    } else {
    unknown:
      o << indent << "#? " << n->key << " " 
        << abi::__cxa_demangle(typeid(*n->value).name(), NULL, NULL, NULL) 
        << "\n";
    }
  }

  struct EmptyStmt : public StmtLeaf {
    EmptyStmt() {}
    const char * what() const {return "empty";}
    void compile(CompileWriter & f) {
      // do absolutely nothing
    }
  };

  static EmptyStmt EMPTY_STMT_OBJ;
  Stmt * const EMPTY_STMT = &EMPTY_STMT_OBJ;

  static const Syntax * ID = new Syntax("id");

  //
  //
  //

  static const Syntax * expand_id(const Syntax * p) {
    const Syntax * res = try_id(p);
    if (!res) throw error(p, "Expected identifier");
    return res;
  }
  
  //
  //
  //

  struct EStmt : public Stmt {
    EStmt() {}
    const char * what() const {return "estmt";}
    EStmt(Exp * e) : exp(e) {}
    //AST * part(unsigned i) {return exp;}
    Exp * exp;
    void finalize(FinalizeEnviron & env) {
      exp->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      exp->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << indent << exp << "\n";
    }
  };

  inline EStmt * Exp::as_stmt() {
    if (!this) return NULL; // FIXME: You should know why
    return new EStmt(this);
  }

  //
  //
  //

  struct BasicVar;
  
  template <Position pos> struct PositionTypeInfo {typedef Stmt t;};
  template <> struct PositionTypeInfo<ExpPos> {typedef Exp t;};
  template <> struct PositionTypeInfo<FieldPos> {typedef BasicVar t;};

  template <Position pos, Pass pass = AllPasses>
  struct Parse;

  template <Position pos>
  struct Parse<pos,AllPasses> {
    Environ & env;
    Parse(Environ & e) : env(e) {}
    const Syntax * partly_expand(const Syntax * p) const {
      return ::partly_expand(p, pos, env);
    }
    typedef typename PositionTypeInfo<pos>::t Ret;
    Ret * finish_parse(const Syntax * p) const;
    Ret * operator() (const Syntax * p) const {
      p = partly_expand(p);
      return finish_parse(p);
    }
  };

  template <Position pos>
  struct Parse<pos,FirstPass> {
    Environ & env;
    Collect & collect;
    Parse(Environ & e, Collect & c) : env(e), collect(c) {}
    const Syntax * partly_expand(const Syntax * p) const {
      return ::partly_expand(p, pos, env);
    }
    Stmt * finish_parse(const Syntax * p) const;
    Stmt * operator() (const Syntax * p) const {
      p = partly_expand(p);
      return finish_parse(p);
    }
  };

  // needed here otherwise gcc gets confuses
  template <>
  Stmt * Parse<TopLevel>::finish_parse(const Syntax * p) const;
  template <>
  Stmt * Parse<TopLevel,FirstPass>::finish_parse(const Syntax * p) const;
  template <>
  Stmt * Parse<StmtDeclPos>::finish_parse(const Syntax * p) const;

  template <typename C, typename P>
  void add_ast_nodes(Parts::const_iterator i, Parts::const_iterator end, 
                     C & container, const P & prs) {
    for (; i != end; ++i) {
      const Syntax * p = prs.partly_expand(*i);
      if (p->is_a("@")) {
        add_ast_nodes(p->args_begin(), p->args_end(), container, prs);
      } else {
        typename C::value_type ast = prs.finish_parse(p);
        container.push_back(ast);
      }
    }
  }

  Stmt * add_stmts(Parts::const_iterator i, Parts::const_iterator end, Environ & env)
  {
    Stmt * a = NULL;
    Parse<StmtDeclPos> prs(env);
    for (; i != end; ++i) {
      const Syntax * p = prs.partly_expand(*i);
      if (p->is_a("@")) {
        a = add_stmts(p->args_begin(), p->args_end(), env);
      } else {
        a = prs.finish_parse(p);
        prs.env.add_stmt(a);
      }
    }
    return a;
  }

  //
  //
  //

#if 0
  void AST::print() {
    printf(" (%s", what().c_str());
    for (int i = 0; i != parse_->num_args(); ++i) {
      part(i)->print();
    }
    printf(")");
  }
#endif

  //
  //
  //

  void Symbol::add_to_env(const SymbolKey & k, Environ & env, bool shadow_ok) {
    SymbolNode * n = env.symbols.add(k, this);
    assert(!key);
    key = &n->key;
    if (env.special()) return;
    make_unique(env.symbols.front);
  }

  void TopLevelSymbol::add_to_env(const SymbolKey & k, Environ & env, bool shadow_ok) {
    //assert(!env.symbols.exists_this_scope(k));
    if (!shadow_ok && env.symbols.exists_this_scope(k)) {
      fprintf(stderr, "TLS SHADOW %s\n", ~k.to_string());
    }
    SymbolNode * local = env.symbols.add(k, this);
    assert(!key);
    key = &local->key;
    if (env.special()) return;
    local->set_flags(SymbolNode::ALIAS);
    if (num == NPOS)
      assign_uniq_num<TopLevelSymbol>(this, *env.top_level_symbols->front);
    //printf(">>%d %s %u %u %s\n", env.special(), ~k.to_string(), num, NPOS, typeid(*this).name());
    SymbolKey uniq_key = uniq_name();
    uniq_key.ns = tl_namespace();
    SymbolNode * tl = find_symbol_node(uniq_key, *env.top_level_symbols->front);
    //printf("1> %s %s %s\n", ~k.to_string(), ~uniq_key.to_string(),  typeid(*this).name());
    if (tl) {
      if (tl == local) {
        //printf("TLS DUP %s\n", ~uniq_name());
        goto finish;
      } else if (static_cast<const Symbol *>(this) == tl->value) {
        fprintf(stderr, "tls mismatch %s\n", ~uniq_key);
      } else {
        fprintf(stderr, "TLS MISMATCH %s\n", ~uniq_key);
      }
      abort();
      //return;
    }
    tl = env.top_level_symbols->add(uniq_key, this);
  finish:
    tl->unset_flags(SymbolNode::ALIAS);
    //printf("2> %s %s %s %d\n", ~name, ~uniq_name(), typeid(*this).name(), order_num);
  }
  
  //
  //
  //

  struct Var;

  struct TempInsrPointWrapper {
    TempInsrPointWrapper(Environ & e, TempInsrPoint::Where w = TempInsrPoint::ExtendedExp) 
      : ip(w), env(e.new_extended_exp(&ip)) {}
    TempInsrPoint ip;
    Environ env;
    Exp * finish(Exp * exp);     // created EBlock;
    Var * finish(Environ & env); // add temp to env
  };

  void finish(Var * v, TempInsrPointWrapper & wrap,
              TempInsrPointWrapper & wrap2, Environ & oenv);


  //
  //
  //

  static Exp * mk_assign(Exp *, Exp *, Environ & env);
  static Exp * mk_init(Exp *, Exp *, bool need_res, Environ & env);
  static Exp * mk_init(const Var *, Exp *, bool need_res, Environ & env);
  static Exp * mk_id(const VarSymbol *, Environ & env);
  static Exp * mk_literal(int val, Environ & env);


  //
  //
  //

  struct NoOp : public Stmt {
    NoOp() {}
    const char * what() const {return "noop";}
    NoOp * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(0);
      return this;
    }
    void compile_prep(CompileEnviron &) {}
    void finalize(FinalizeEnviron &) {}
    void compile(CompileWriter & f) {
      f << indent << "(noop)\n";
    }
  };

//   struct Terminal : public FakeAST {
//     Terminal(const Syntax * p) : FakeAST(p->what(), p) {}
//     Terminal * parse_self(const Syntax * p, Environ & env) {abort();}
//   };

//   struct Generic : public FakeAST {
//     Vector<AST *> parts;
//     Generic(const Syntax * p) : FakeAST(p->what(), p) {
//       for (int i = 0; i != p->num_args(); ++i)
//         abort();
//         //parts.push_back(new Terminal(p->arg(i)));
//     }
//     Generic(const Syntax * p, const Vector<AST *> & pts) 
//       : FakeAST(p->what(), p), parts(pts) {}
//     //AST * part(unsigned i) {return parts[i];}
//     Generic * parse_self(const Syntax*, Environ&) {abort();}
//   };

  struct Label : public Stmt, public Symbol {
    Label() {}
    const char * what() const {return "label";}
    void compile_prep(CompileEnviron &) {}
    void finalize(FinalizeEnviron &) {}
    void compile(CompileWriter & o) {
      o << adj_indent(-2) << indent << "(label " << uniq_name() << ")\n";
    }
  };

  struct NormalLabel : public Label {
    mutable unsigned num;
    NormalLabel() : num() {}
    void uniq_name(OStream & o) const {
      o.printf("%s$$%u", ~name(), num);
    }
    void add_to_env(const SymbolKey & k, Environ &, bool shadow_ok);
    void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const {
      assign_uniq_num<NormalLabel>(this, self->next, stop);
    }
  };

  void NormalLabel::add_to_env(const SymbolKey & k, Environ & env, bool shadow_ok) {
    SymbolNode * n = env.fun_labels.add(k, this);
    key = &n->key;
    make_unique(*env.fun_labels.front);
  }

  struct LocalLabel : public Label {
    mutable unsigned num;
    LocalLabel() : num() {}
    using Label::uniq_name;
    void uniq_name(OStream & o) const {
      o.printf("%s$%u", ~name(), num);
    }
    void make_unique(SymbolNode * self, SymbolNode * stop) const {
      assign_uniq_num<LocalLabel>(this, self->next, stop);
    }
  };

  Stmt * parse_label(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    SymbolKey n = expand_binding(p->arg(0), LABEL_NS, env);
    Label * label = env.symbols.find<Label>(n);
    if (!label) {
      label = new NormalLabel;
      env.add(SymbolKey(n, LABEL_NS), label);
    }
    label->syn = p;
    return label;
  }

  struct LocalLabelDecl : public Stmt {
    LocalLabelDecl() {}
    const char * what() const {return "local_label";}
    LocalLabel * label;
    LocalLabelDecl * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(1);
      SymbolKey n = expand_binding(p->arg(0), LABEL_NS, env);
      label = new LocalLabel;
      env.add(n, label);
      return this;
    }
    void finalize(FinalizeEnviron & env) {}
    void compile_prep(CompileEnviron & env) {}
    void compile(CompileWriter & o) {
      o << indent << "(local_label " << label->uniq_name() << ")\n";
    }
  };

  struct Case : public Stmt {
    Case() {}
    const char * what() const {return "case";}
    Exp * exp; 
    Case * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      if (p->num_args() == 1) {
        exp = parse_exp(p->arg(0), env);
      } else /* default */ {
        exp = NULL;
      }
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      if (exp)
        exp->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      // nothing to do
    }
    void compile(CompileWriter & o) {
      if (exp)
        o << adj_indent(-2) << indent << "(case " << exp << ")\n";
      else
        o << adj_indent(-2) << indent << "(case)\n";
    }
  };

  struct Goto : public Stmt {
    Goto() {}
    const char * what() const {return "goto";}
    const Syntax * label_s;
    SymbolName label;
    const Label * sym;
    Goto * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(1);
      label_s = expand_id(p->arg(0));
      label = *label_s;
      sym = env.symbols.find<Label>(SymbolKey(label, LABEL_NS));
      return this;
    }
    // FIXME, move into compile ...
    //void resolve(Environ & env) {
    //  if (!env.labels->exists(label))
    //    throw error(parse_->arg(0)->arg(0), "Unknown label %s", ~label);
    //  type = env.void_type();
    //}
    void finalize(FinalizeEnviron & env) {
      if (!sym)
        sym = lookup_symbol<Label>(SymbolKey(label, LABEL_NS), 
                                  label_s->str(), env.fun_symbols);
    }
    void compile_prep(CompileEnviron & env) {}
    void compile(CompileWriter & o) {
      o << indent << "(goto " << sym << ")\n";
    }
  };
  
  //AST * Literal::part(unsigned i) {return new Terminal(parse_->arg(0));}
  
  Literal * Literal::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(1,2);
    type = env.types.inst(p->num_args() > 1 ? *p->arg(1) : String("int"));
    ct_value_ = new_literal_ct_value(p->arg(0), type, env);
    return this;
  }
  //void Literal::eval(ExecEnviron & env) {
    //env.ret<int>(this) = value;
  //}
  void Literal::compile(CompileWriter & f) {
    ct_value_->compile(f, NULL);
  }
  Exp * mk_literal(int val, Environ & env) {
    Literal * l = new Literal;
    l->type = env.types.inst("int");
    l->ct_value_ = new CT_Value<int>(val);
    return l;
  }

  FloatC * FloatC::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(1,2);
    type = env.types.inst(p->num_args() > 1 ? *p->arg(1) : String("double"));
    ct_value_ = new_float_ct_value(p->arg(0), type, env);
    return this;
  }

  void FloatC::compile(CompileWriter & f) {
    ct_value_->compile(f, NULL);
  }

  StringC * StringC::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(1,2);
    orig = *p->arg(0);
    //printf("StringC: %s\n", ~p->to_string());
    type = env.types.inst(".ptr", env.types.ct_const(env.types.inst("char")));
    type = env.types.ct_const(type);
    ct_value_ = &ct_nval;
    return this;
  }
  void StringC::compile(CompileWriter & f) {
    f << "(s \"" << orig << "\")";
  }
  
  CharC * CharC::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(1, 2);
    orig = *p->arg(0);
    type = env.types.inst("char");
    type = env.types.ct_const(type);
    ct_value_ = &ct_nval;
    return this;
  }
  void CharC::compile(CompileWriter & f) {
    f << "(c \"" << orig << "\")"; // FIXME: Not right
  }

  struct Id : public ExpLeaf {
    Id() {}
    Id(const VarSymbol * s) : sym(s) {}
    const char * what() const {return "id";}
    //AST * part(unsigned i) {return new Terminal(parse_->arg(0));}
    const VarSymbol * sym;
    Id * construct(Environ & env) {
      const TopLevelVarDecl * tl = sym->top_level();
      if (tl && env.deps)
        env.deps->insert(tl);
      if (sym->ct_value)
        ct_value_ = sym->ct_value;
      type = sym->type;
      lvalue = sym->lvalue;
      return this;
    }
    Id * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(1);
      sym = env.symbols.lookup<VarSymbol>(p->arg(0));
      return construct(env);
    }
    void compile(CompileWriter & f) {
      f << sym;
    }
  };

  static Exp * mk_id(const VarSymbol * s, Environ & env) {
    Id * id = new Id(s);
    return id->construct(env);
  }

  struct If : public Stmt {
    If() {}
    const char * what() const {return "if";}
    //AST * part(unsigned i) 
    //  {return i == 0 ? exp : i == 1 ? if_true : i == 2 ? if_false : 0;}
    Exp * exp;
    AST * if_true;
    AST * if_false;
    If * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(2,3);
      TempInsrPointWrapper wrap(env);
      exp = parse_exp(p->arg(0), env);
      exp = wrap.finish(exp);
      if_true = parse_stmt(p->arg(1), env);
      if (p->num_args() == 3) {
        if_false = parse_stmt(p->arg(2), env);
      } else {
        if_false = NULL;
      }
      exp = exp->resolve_to(env.bool_type(), env);
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      exp->finalize(env);
      if_true->finalize(env);
      if (if_false)
        if_false->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      exp->compile_prep(env);
      if_true->compile_prep(env);
      if (if_false)
        if_false->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << indent << "(if " << exp << "\n";
      f << adj_indent(2) << if_true;
      //f << indent << "else\n";
      if (if_false)
        f << adj_indent(2) << if_false;
      f << indent << ")\n";
    }
  };

  void EIf::finalize(FinalizeEnviron & env) {
    exp->finalize(env);
    if_true->finalize(env);
    if_false->finalize(env);
  }

  void EIf::compile_prep(CompileEnviron & env) {
    exp->compile_prep(env);
    if_true->compile_prep(env);
    if_false->compile_prep(env);
  }
  
  void EIf::compile(CompileWriter & f) {
    f << "(eif " << exp << " " << if_true << " " << if_false << ")";
  }

  EIf * EIf::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(3);
    exp = parse_exp(p->arg(0), env);
    exp = exp->resolve_to(env.bool_type(), env);
    if_true = parse_exp(p->arg(1), env);
    if_false = parse_exp(p->arg(2), env);
    if_false = if_false->resolve_to(if_true->type, env);
    type = if_true->type;
    ct_value_ = eif_ct_value(this);
    return this;
  }

  struct Switch : public Stmt {
    Switch() {}
    const char * what() const {return ".switch";}
    Exp * exp;
    AST * body;
    //AST * part(unsigned i) {return i == 0 ? exp : i == 1 ? body : 0;}
    Switch * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(2);
      exp = parse_exp(p->arg(0), env);
      exp = exp->resolve_to(env.bool_type(), env);  
      body = parse_stmt(p->arg(1), env);
      return this;
    }
    //void resolve(Environ & env) {
    //  type = env.void_type();
    //  resolve_to(env, exp, env.bool_type());      
    //  resolve_to_void(env, body);
    //}
    void finalize(FinalizeEnviron & env) {
      exp->finalize(env);
      body->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      exp->compile_prep(env);
      body->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << indent << "(.switch " << exp << "\n";
      f << adj_indent(2) << body;
      f << indent << ")\n";
    }
  };

  //
  //
  //

  struct OtherVar : public BasicVar {
    OtherVar() : num() {}
    OtherVar(const Type * t, bool mangle) : BasicVar(t), num(mangle ? NPOS : 0) {}
    mutable unsigned num; // 0 to avoid renaming, NPOS needs uniq num
    void uniq_name(OStream & o) const {
      if (num == 0)
        o << name();
      else
        o.printf("%s$%u", ~name(), num);
    }
    void make_unique(SymbolNode * self, SymbolNode * stop) const {
      if (num == NPOS)
        assign_uniq_num<OtherVar>(this, self->next, stop);
    }
  };

  static OtherVar * new_other_var(SymbolName n, const Type * t) {
    bool mangle = n.marks;
    return new OtherVar(t, mangle);
  }

  // this marks the point where cleanup is needed
  struct CleanupFlag : public ExpLeaf {
    CleanupFlag(struct Var * v) : var(v) {}
    struct Var * var;
    virtual const char * what() const {return "need-cleanup";}
    void compile(CompileWriter & cw);
  };

  struct Cleanup : public Stmt {
    Cleanup() : cleanup_flag(), code() {}
    virtual const char * what() const {return "cleanup";}
    CleanupFlag * cleanup_flag; // NULL if not used
    Stmt * code;
    void compile_prep(CompileEnviron & env) {code->compile_prep(env);}
    void finalize(FinalizeEnviron & env) {code->finalize(env);}
    void compile(CompileWriter & cw);
    Cleanup * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(1);
      code = parse_stmt(p->arg(0), env);
      return this;
    }
  };

  Exp * mk_copy_constructor(Exp * lhs, Exp * rhs, Environ & env) {
    return parse_exp(SYN(SYN("member"), 
                         SYN(lhs),
                         SYN(SYN("call"), SYN(ID, SYN("_copy_constructor")), 
                             SYN(SYN("."), SYN(rhs)))),
                     env);
  }

  struct Var : virtual public VarDecl {
    Var() : init(), constructor(), cleanup() {}
    const char * what() const {return "var";}
    //AST * part(unsigned i) {return new Terminal(parse_->arg(0));}
    Exp * init;
    Stmt * constructor;
    Cleanup * cleanup;
    void finish_parse(Environ & env) {
      if (syn->num_args() > 2) {
        //env.add(name, sym, SecondPass);
        TempInsrPointWrapper wrap(env);
        init = parse_exp(syn->arg(2), wrap.env);
        TempInsrPointWrapper wrap2(env, top_level() ? TempInsrPoint::TopLevelVar : TempInsrPoint::Var);
        init = init->resolve_to(type, wrap2.env);
        finish(this, wrap, wrap2, env);
        //if (sym->type->is_ref && !init->lvalue) {
        //  temp_exp = init;
        //  SymbolKey name = expand_binding(name_p, env);
        //  
        //  temp_sym = new_var_symbol(...);
        //  init = addrof(temp_sym);
        //}
        if (storage_class == SC_STATIC && type->read_only && init->ct_value_) {
          ct_value = init->ct_value_;
        }
      }
      if (const UserType * ut = dynamic_cast<const UserType *>(type)) {
        if (!init) constructor = make_constructor(ut, env)->as_stmt();
        add_cleanup(ut, env);
      }
    }
    Exp * make_constructor(const UserType * ut, Environ & env) {
      if (ut && find_symbol<Symbol>("_constructor", ut->module->syms)) {
        return parse_exp(SYN(SYN("member"), 
                              SYN(mk_id(this, env)),
                              SYN(SYN("call"), SYN(ID, SYN("_constructor")), SYN(SYN(".")))),
                          env);
      } else {
        return NULL;
      }
    }
    Exp * try_copy_constructor(Exp * rhs, Environ & env) {
      const UserType * ut = dynamic_cast<const UserType *>(type->unqualified);
      if (ut && find_symbol<Symbol>("_copy_constructor", ut->module->syms)) {
        return mk_copy_constructor(mk_id(this, env), rhs, env);
      } else {
        return NULL;
      }
    }

    void add_cleanup(const UserType * ut, Environ & env) {
      if (ut && find_symbol<Symbol>("_destructor", ut->module->syms)) {
        cleanup = new Cleanup;
        cleanup->code = parse_stmt(SYN(SYN("member"), 
                                       SYN(mk_id(this, env)),
                                       SYN(SYN("call"), SYN(ID, SYN("_destructor")), SYN(SYN(".")))),
                                   env);
      }
    }

    virtual void fix_up_init(Environ & env) {
      if (!init) return;
      Exp * copy_c = try_copy_constructor(init, env);
      if (copy_c) {
        constructor = copy_c->as_stmt();
        init = NULL;
      }
    }
    
    void finalize(FinalizeEnviron & env) {
      if (init)
        init->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      if (init)
        init->compile_prep(env);
    }
    void compile(CompileWriter & f, Phase phase) const {
      if (cleanup && cleanup->cleanup_flag) {
        f << indent << "(var "<< uniq_name() << "$nc (bool)";
        if (phase != Forward) f << " 0";
        f << ")\n";
      }
      f << indent;
      f << "(var";
      f << ' ' << uniq_name();
      f << " " << type;
      write_storage_class(f);
      if (init && phase != Forward) {
        if (init->ct_value_) {
          f << " ";
          init->ct_value_->compile(f, init);
        } else {
          f << " " << init;
        }
      }
      f << ")\n";        
    }
  };

  void CleanupFlag::compile(CompileWriter & cw) {
    cw << "(assign " << var->uniq_name() << "$nc 1)";
  }

  void Cleanup::compile(CompileWriter & cw) {
    cw << indent << "(cleanup ";
    if (cleanup_flag) {
      cw << "(if " << cleanup_flag->var->uniq_name() << "$nc\n";
      cw << adj_indent(2) << code;
      cw << indent << "))\n";
    } else {
      cw << "\n";
      cw << adj_indent(2) << code;
      cw << indent << ")\n";
    }
  }

  struct AutoVar : public Var {
    AutoVar() : num() {}
    mutable unsigned num;
    void compile(CompileWriter & f, Phase phase) const {
      assert(phase == Normal);
      Var::compile(f, phase);
      if (constructor)
        f << constructor;
      if (cleanup)
        f << cleanup;
    }
    void uniq_name(OStream & o) const {
      o.printf("%s$%u", ~name(), num);
    }
    void make_unique(SymbolNode * self, SymbolNode * stop) const {
      assign_uniq_num<AutoVar>(this, self->next, stop);
    }
  };

  static unsigned last_temp_num = 0;

  struct AutoTemp : public AutoVar {
    AutoTemp(const Type * t, LValue lv) {type = t; lvalue = lv;}
    void make_unique(SymbolNode *, SymbolNode *) const {
      num = last_temp_num++;
    }
  };

  struct TopLevelVar : public Var, public TopLevelVarDecl {
    TopLevelVar() {lvalue = LV_TOPLEVEL;}
    void fix_up_init(Environ & env) {
      Var::fix_up_init(env);
      if (init && !init->ct_value_) {
        constructor = mk_init(this, init, false, env)->as_stmt();
        init = NULL;
      } 
    }
    void finish_parse(Environ & env) {
      Var::finish_parse(env);
      env.move_defn(this);
    }
  };

  struct TopLevelTemp : public TopLevelVar {
    TopLevelTemp(const Type * t) {type = t; num = NPOS;}
  };

  static StorageClass get_storage_class(const Syntax * p) {
    if (p->flag("auto")) return SC_AUTO;
    if (p->flag("static")) return SC_STATIC;
    if (p->flag("extern")) return SC_EXTERN;
    if (p->flag("register")) return SC_REGISTER;
    return SC_NONE;
  }

  static void make_static_if_marked(StorageClass & storage_class, const SymbolKey & name) {
    if ((storage_class == SC_NONE || storage_class == SC_EXTERN) && name.marks)
      storage_class = SC_STATIC;
  }
  
  static Stmt * parse_var_forward(const Syntax * p, Environ & env, Collect & collect) {
    assert_num_args(p, 2,3);
    const Syntax * name_p = p->arg(0);
    SymbolKey name = expand_binding(name_p, env);
    StorageClass storage_class = get_storage_class(p);
    Var * var;
    Stmt * res;
    bool fresh = true;
    if (env.scope == LEXICAL && (storage_class == SC_NONE ||
                                 storage_class == SC_AUTO ||
                                 storage_class == SC_REGISTER)) 
    {
      AutoVar * v = new AutoVar;
      var = v;
      res = v;
    } else {
      make_static_if_marked(storage_class, name);
      fresh = !env.symbols.exists_this_scope(name);
      TopLevelVar * v = fresh ? new TopLevelVar : env.symbols.find<TopLevelVar>(name);
      bool mangle = env.scope == LEXICAL || name.marks || env.where || storage_class == SC_STATIC;
      v->num = mangle ? NPOS : 0;
      v->where = env.where;
      v->deps_closed = true;
      var = v;
      res = empty_stmt();
    }
    var->syn = p;
    var->name_p = name_p;
    var->type = parse_type(p->arg(1), env);
    var->storage_class = storage_class;
    if (storage_class != SC_EXTERN && var->type->size() == NPOS)
      throw error(name_p, "Size not known");
    if (fresh)
      env.add(name, var);
    collect.push_back(var); // fixme, not always the case
    return res;
  }

  static Stmt * parse_var(const Syntax * p, Environ & env) {
    Collect collect;
    Stmt * res = parse_var_forward(p, env, collect);
    for (Collect::iterator i = collect.begin(), e = collect.end(); i != e; ++i) {
      (*i)->finish_parse(env);
    }
    return res;
  }

  static BasicVar * parse_field_var(const Syntax * p, Environ & env) {
    assert_num_args(p,2);
    const Syntax * name_p = p->arg(0);
    SymbolKey name = expand_binding(name_p, env);
    const Type * type = parse_type(p->arg(1), env);
    OtherVar * var = new_other_var(name, type);
    var->name_p = name_p;
    if (var->type->size() == NPOS)
      throw error(name_p, "Size not known");
    env.add(name, var);
    return var;
  }
  
  //
  //
  //

  template <typename T> 
  struct BlockBase : public T {
    using T::syn;
    BlockBase() {}
    static const bool as_exp = T::ast_type == Exp::ast_type;
    //AST * part(unsigned i) {return stmts[i];}
    //SymbolTable symbols; // not valid until done parsing block
    Stmt * stmts;
    inline void set_type_if_exp(Stmt * t);
    T * parse_self(const Syntax * p, Environ & env0) {
      syn = p;
      Environ env = env0.new_scope();
      stmts = NULL;
      env.ip = &stmts;
      Stmt * last = add_stmts(p->args_begin(), p->args_end(), env);
      if (stmts == NULL) // must test against stmts as last will may
                         // be an empty_stmt
        stmts = last = empty_stmt();
      //symbols = env.symbols;
      set_type_if_exp(last);
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      for (Stmt * cur = stmts; cur; cur = cur->next) {
        cur->finalize(env);
      }
    }
    void compile_prep(CompileEnviron & env) {
      for (Stmt * cur = stmts; cur; cur = cur->next) { 
       cur->compile_prep(env);
      }
    }
    void compile(CompileWriter & f) {
      if (as_exp)
        f << "(eblock\n";
      else
        f << indent << "(block\n";
      for (Stmt * cur = stmts; cur; cur = cur->next) {
        f << adj_indent(2) << cur;
      }
      if (as_exp)
        f << indent << ")";
      else
        f << indent << ")\n";
    }
  };

  template <>
  inline void BlockBase<Stmt>::set_type_if_exp(Stmt *) {}

  template <>
  inline void BlockBase<Exp>::set_type_if_exp(Stmt * stmt) {
    EStmt * estmt = dynamic_cast<EStmt *>(stmt);
    if (!estmt) return;
    type = estmt->exp->type;
    lvalue = estmt->exp->lvalue;
  }

  struct Block : public BlockBase<Stmt> {
    Block() {}
    const char * what() const {return "block";}
  };

  struct EBlock : public BlockBase<Exp> {
    EBlock() {}
    const char * what() const {return "eblock";}
    void set_type(const Type * t, LValue lv) {
      type = t;
      lvalue = lv;
    }
    void set_type(const Exp * e) {
      set_type(e->type, e->lvalue);
    }
    void set_type(const VarSymbol * s) {
      set_type(s->type, s->lvalue);
    }
  };

  struct Seq : public Exp {
    Vector<Exp *> exps;
    const char * what() const {return "seq";}
    void construct(Environ & env) {
      type = exps.back()->type;
      lvalue = exps.back()->lvalue;
    }
    Seq * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      for (Parts::const_iterator i = p->args_begin(), e = p->args_end();
           i != e; ++i)
        exps.push_back(parse_exp(*i, env));
      construct(env);
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      for (Vector<Exp *>::iterator i = exps.begin(), e = exps.end();
           i != e; ++i)
        (*i)->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      for (Vector<Exp *>::iterator i = exps.begin(), e = exps.end();
           i != e; ++i)
        (*i)->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << "(seq";
      for (Vector<Exp *>::iterator i = exps.begin(), e = exps.end();
           i != e; ++i)
        f << ' ' << *i;
      f << ")";
    }
  };

  void check_type(Exp * exp, TypeCategory * cat) {
    if (!exp->type->is(cat)) 
      throw error(exp->syn, "Expected %s type", ~cat->name);
  }
  
  //
  // UnOp
  //

  UnOp * UnOp::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(1);
    exp = parse_exp(p->arg(0), env);
    return construct(env);
  }

  void UnOp::make_ct_value() {}

  void UnOp::finalize(FinalizeEnviron & env) {
    exp->finalize(env);
  }

  void UnOp::compile_prep(CompileEnviron & env) {
    exp->compile_prep(env);
  }

  void UnOp::compile(CompileWriter & f) {
    String w = what();
    if (f.target_lang == CompileWriter::ZLS) {
      if (w == "addrof_ref") w = "addrof"; // HACK
      if (w == "deref_ref") w = "deref"; // HACK
      f << "(" << w << " " << exp << ")";
    } else if (f.target_lang == CompileWriter::ZLE) {
      if (w == "addrof_ref" || w == "deref_ref") 
        f << exp;
      else
      f << "(" << w << " " << exp << ")";
    }
  }

  const Type * resolve_unop(Environ & env, TypeCategory * cat, Exp * & exp) {
    if (!exp->type->is(cat))
      abort();
    exp = exp->to_effective(env);
    return exp->type;
  }

  struct SimpleUnOp : public UnOp {
    SimpleUnOp(String name, String op0, TypeCategory * c) 
      : UnOp(name, op0), category(c) {}
    TypeCategory * category;
    void resolve(Environ & env) {
      type = resolve_unop(env, category, exp);
    }
  };

  struct UPlus : public SimpleUnOp {
    UPlus() : SimpleUnOp("uplus", "+", NUMERIC_C) {}
    void make_ct_value() {
      ct_value_ = exp->ct_value_;
    }
  };

  struct Neg : public SimpleUnOp {
    Neg() : SimpleUnOp("neg", "-", NUMERIC_C) {}
    void make_ct_value() {
      ct_value_ = neg_ct_value(this);
    }
  };

  struct Compliment : public SimpleUnOp {
    Compliment() : SimpleUnOp("compliment", "~", INT_C) {}
    template <typename T>
    struct F : public std::unary_function<T,T> {
      T operator()(T x) {return ~x;}
    };
    void make_ct_value() {
      ct_value_ = compliment_ct_value(this);
    }
  };

  struct Not : public UnOp {
    Not() : UnOp("not", "!") {}
    void resolve(Environ & env) {
      // FIXME: Do I need to do more?
      exp = exp->to_effective(env);
      type = env.types.inst("int");
    }
  };

  struct AddrOf : public UnOp {
    AddrOf() : UnOp("addrof", "&") {}
    void resolve(Environ & env) {
      if (!exp->lvalue) {
        throw error(exp->syn, "Can not be used as lvalue");
      }
      exp = exp->to_effective(env);
      // FIXME: add check for register qualifier
      TypeSymbol * t = env.types.find(".ptr");
      Vector<TypeParm> p;
      p.push_back(TypeParm(exp->type));
      type = t->inst(p);
    }
    void make_ct_value() {
      if (!exp->ct_value_) {
        if (exp->lvalue == LV_TOPLEVEL) 
          ct_value_ = &ct_nval;
      } else if (exp->ct_value_->nval()) {
        ct_value_ = &ct_nval;
      } else {
        CT_LValue val = exp->ct_value_direct<CT_LValue>();
        ct_value_ = new CT_Value<CT_Ptr>(val.addr);
      }
    }
  };

  struct AddrOfRef : public UnOp {
    AddrOfRef() : UnOp("addrof_ref", "&") {}
    void resolve(Environ & env) {
      if (!exp->lvalue) {
        throw error(exp->syn, "Can not be used as lvalue");
      }
      TypeSymbol * t = env.types.find(".ref");
      Vector<TypeParm> p;
      p.push_back(TypeParm(exp->type));
      type = t->inst(p);
    }
    void make_ct_value() {
      if (!exp->ct_value_) {
        if (exp->lvalue == LV_TOPLEVEL) 
          ct_value_ = &ct_nval;
      } else if (exp->ct_value_->nval()) {
        ct_value_ = &ct_nval;
      } else {
        CT_LValue val = exp->ct_value_direct<CT_LValue>();
        ct_value_ = new CT_Value<CT_Ptr>(val.addr);
      }
    }
  };

  struct DeRef : public UnOp {
    DeRef() : UnOp("deref", "*") {}
    void resolve(Environ & env) {
      check_type(exp, POINTER_C);
      exp = exp->to_effective(env);
      const PointerLike * t = dynamic_cast<const PointerLike *>(exp->type->unqualified);
      type = t->subtype;
      lvalue = LV_NORMAL;
    }
    void make_ct_value() {
      if (!exp->ct_value_) return;
      if (exp->ct_value_->nval()) {
        ct_value_ = &ct_nval;
      } else {
        CT_Ptr val = exp->ct_value_direct<CT_Ptr>();
        ct_value_ = new CT_Value<CT_LValue>(CT_LValue(val));
      }
    }
  };

  struct DeRefRef : public UnOp {
    DeRefRef() : UnOp("deref_ref", "*") {}
    void resolve(Environ & env) {
      const Reference * t = dynamic_cast<const Reference *>(exp->type->unqualified);
      type = t->subtype;
      lvalue = LV_NORMAL;
    }
    void make_ct_value() {
      if (!exp->ct_value_) return;
      if (exp->ct_value_->nval()) {
        ct_value_ = &ct_nval;
      } else {
        CT_Ptr val = exp->ct_value_direct<CT_Ptr>();
        ct_value_ = new CT_Value<CT_LValue>(CT_LValue(val));
      }
    }
  };

  Exp * parse_addrof(const Syntax * p, Environ & env) {
    AddrOf * addrof = new AddrOf;
    addrof->parse_self(p, env);
    DeRef * deref = dynamic_cast<DeRef *>(addrof->exp);
    if (deref) return deref->exp;
    return addrof;
  }

  Exp * parse_deref(const Syntax * p, Environ & env) {
    DeRef * deref = new DeRef;
    deref->parse_self(p, env);
    AddrOf * addrof = dynamic_cast<AddrOf *>(deref->exp);
    if (addrof) return addrof->exp;
    return deref;
  }

  Exp * to_ref(Exp * exp, Environ & env) {
    DeRefRef * deref = dynamic_cast<DeRefRef *>(exp);
    if (deref) return deref->exp;
    AddrOfRef * res = new AddrOfRef();
    res->syn = exp->syn;
    res->exp = exp;
    return res->construct(env);
  }

  Exp * from_ref(Exp * exp, Environ & env) {
    AddrOfRef * addrof = dynamic_cast<AddrOfRef *>(exp);
    if (addrof) return addrof->exp;
    DeRefRef * res = new DeRefRef();
    res->syn = exp->syn;
    res->exp = exp;
    return res->construct(env);
  }

  //
  // BinOp
  //

  BinOp * BinOp::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(2);
    lhs = parse_exp(p->arg(0), env);
    rhs = parse_exp(p->arg(1), env);
    return construct(env);
  }
  void BinOp::make_ct_value() {}
  void BinOp::finalize(FinalizeEnviron & env) {
    lhs->finalize(env);
    rhs->finalize(env);
  }
  void BinOp::compile_prep(CompileEnviron & env) {
    lhs->compile_prep(env);
    rhs->compile_prep(env);
  }
  void BinOp::compile(CompileWriter & f) {
    f << "(" << what() << " " << lhs << " " << rhs << ")";
  }
  
  struct MemberAccess : public Exp {
    MemberAccess() {}
    const char * what() const {return "member";}
    Exp * exp;
    const VarSymbol * sym;
    MemberAccess * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(2);
      exp = parse_exp(p->arg(0), env);
      exp = exp->to_effective(env);
      //printf("::"); p->arg(1)->print(); printf("\n");
      SymbolName id = *expand_id(p->arg(1));
      const StructUnion * t = dynamic_cast<const StructUnion *>(exp->type->unqualified);
      if (!t) throw error(p->arg(0), "Expected struct or union type");
      if (!t->defined) throw error(p->arg(1), "Invalid use of incomplete type");
      sym = t->env.symbols.find<VarSymbol>(id, StripMarks);
      if (!sym)
        throw error(p->arg(1), "\"%s\" is not a member of \"%s\"", 
                    ~id.to_string(), ~t->to_string());
      type = sym->type;
      lvalue = exp->lvalue;
      if (exp->ct_value_) {
        if (exp->ct_value_->nval()) {
          ct_value_ = &ct_nval;
        } else {
          CT_Ptr p = exp->ct_value_direct<CT_LValue>().addr;
          Vector<Member>::const_iterator i = t->members.begin(), end = t->members.end();
          while (i != end && i->sym != sym)
            ++i;
          assert(i != end);
          p.val += i->offset;
          ct_value_ = new CT_Value<CT_LValue>(CT_LValue(p));
        }
      }
      return this;
    };
    void finalize(FinalizeEnviron & env) {
      exp->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      exp->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << "(member " << exp << " " << sym->uniq_name() << ")";
    }
  };

  const Type * resolve_binop(Environ & env, TypeCategory * cat, Exp *& lhs, Exp *& rhs) {
    check_type(lhs, cat);
    check_type(rhs, cat);
    const Type * t = env.type_relation->unify(0, lhs, rhs, env);
    return t;
  }

  const Type * p_subtype(const Type * t) {
    if (const PointerLike * p = dynamic_cast<const PointerLike *>(t))
      return p->subtype;
    return VOID_T;
    //abort();
  }

  enum PointerBinOp {P_MINUS, P_COMP};
  void resolve_pointer_binop(PointerBinOp op, Environ & env, Exp *& lhs, Exp *& rhs) {
    check_type(lhs, POINTER_C);
    check_type(rhs, POINTER_C);
    const Type * lhs_subtype = p_subtype(lhs->type->effective->unqualified)->unqualified;
    const Type * rhs_subtype = p_subtype(rhs->type->effective->unqualified)->unqualified;
    if (op == P_MINUS) {
      if (lhs_subtype == rhs_subtype) goto ok;
    } else if (op == P_COMP) {
      if (lhs_subtype == rhs_subtype
          || lhs->type->is_null || lhs->type->is_null 
          || dynamic_cast<const Void *>(lhs_subtype) 
          || dynamic_cast<const Void *>(rhs_subtype))
        goto ok;
    } else {
      goto fail;
    }
  ok:
    lhs = lhs->to_effective(env);
    rhs = rhs->to_effective(env);
    return;
  fail:
    abort();
    //throw error(rhs->syn, "Incompatible pointer types");
  }
  
  const Type * resolve_additive(Environ & env, Exp *& lhs, Exp *& rhs) {
    check_type(lhs, SCALAR_C);
    check_type(rhs, SCALAR_C);
    if (lhs->type->is(NUMERIC_C) && rhs->type->is(NUMERIC_C)) {
      return resolve_binop(env, NUMERIC_C, lhs, rhs);
    } else if (lhs->type->is(POINTER_C)) {
      check_type(rhs, INT_C);
      lhs = lhs->to_effective(env);
      rhs = rhs->to_effective(env);
      return lhs->type;
    } else if (lhs->type->is(INT_C)) {
      check_type(rhs, POINTER_C);
      lhs = lhs->to_effective(env);
      rhs = rhs->to_effective(env);
      return rhs->type;
    } else {
      abort(); // this should't happen
    }
  }

  struct Assign : public BinOp {
    Assign() : BinOp("assign", "=") {}
    Assign(Exp * l, Exp * r) : BinOp("assign", "=") {lhs = l; rhs = r;}
    void resolve(Environ & env) {
      //printf("RESOLVE ASSIGN:: %p %s\n", lhs, ~syn->to_string());
      //printf("RESOLVE ASSIGN lhs:: %s\n", ~lhs->syn->to_string());
      //printf("RESOLVE ASSIGN lhs:: %s\n", ~lhs->syn->sample_w_loc(60));
      env.type_relation->resolve_assign(lhs, rhs, env);
      type = lhs->type;
      lvalue = lhs->lvalue;
    }
  };

  static Exp * try_user_assign(Exp * lhs, Exp * rhs, Environ & env) {
    const UserType * ut = dynamic_cast<const UserType *>(lhs->type->unqualified);
    if (ut && find_symbol<Symbol>("_assign", ut->module->syms)) {
      return parse_exp(SYN(SYN("member"), 
                           SYN(lhs),
                           SYN(SYN("call"), SYN(ID, SYN("_assign")), 
                               SYN(SYN("."), SYN(rhs)))),
                       env);
    } else {
      return NULL;
    }
  }

  static Exp * mk_assign(Exp * lhs, Exp * rhs, Environ & env) {
    env.type_relation->resolve_assign(lhs, rhs, env);
    Exp * assign = try_user_assign(lhs, rhs, env);
    if (assign) return assign;
    Assign * exp = new Assign(lhs, rhs);
    return exp->construct(env);
  }

  static Exp * parse_assign(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    Exp * lhs = parse_exp(p->arg(0), env);
    Exp * rhs = parse_exp(p->arg(1), env);
    Exp * assign = mk_assign(lhs, rhs, env);
    assign->syn = p;
    return assign;
  }

  struct InitAssign : public BinOp {
    InitAssign() : BinOp("assign", "=") {}
    InitAssign(Exp * l, Exp * r) : BinOp("assign", "=") {lhs = l; rhs = r;}
    void resolve(Environ & env) {
      type = lhs->type;
      rhs->resolve_to(lhs->type, env);
      lvalue = lhs->lvalue;
    }
  };

  Exp * try_copy_constructor(Exp * lhs, Exp * rhs, Environ & env) {
    const UserType * ut = dynamic_cast<const UserType *>(lhs->type->unqualified);
    if (ut && find_symbol<Symbol>("_copy_constructor", ut->module->syms)) {
      return mk_copy_constructor(lhs, rhs, env);
    } else {
      return NULL;
    }
  }

  static Exp * mk_init(Exp * l, Exp * r, bool need_res, Environ & env) {
    r = r->resolve_to(l->type, env);
    Exp * copy_c = try_copy_constructor(l, r, env);
    if (copy_c) {
      if (need_res) {
        Seq * seq = new Seq();
        seq->exps.push_back(copy_c);
        seq->exps.push_back(l);
        seq->construct(env);
        return seq;
      } else {
        return copy_c;
      }
    } else {
      InitAssign * exp = new InitAssign(l, r);
      return exp->construct(env);
    }
  }


  static Exp * mk_init(const Var * v, Exp * r, bool need_res, Environ & env) {
    return mk_init(mk_id(v, env), r, need_res, env);
  }

  static Exp * parse_init_assign(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    Exp * lhs = parse_exp(p->arg(0), env);
    Exp * rhs = parse_exp(p->arg(1), env);
    Exp * assign = mk_init(lhs, rhs, true, env);
    assign->syn = p;
    return assign;
  }

  struct CompoundAssign : public BinOp {
    CompoundAssign(String name, String op0, BinOp *bop, Environ & env) 
      : BinOp(name, op0), assign(new Assign), binop(bop) 
    {
      syn = binop->syn;
      lhs = binop->lhs;
      rhs = binop->rhs;
      assign->syn = syn;
      assign->lhs = lhs;
      assign->rhs = binop;
      assign->resolve(env);
      type = assign->type;
    }
    Assign * assign;
    BinOp * binop;
    CompoundAssign * parse_self(const Syntax * p, Environ & env) {abort();}
    void resolve(Environ & env) {abort();}
    void compile(CompileWriter & f) {
      f << "(c-assign " << binop->what() << " " << lhs << " " << rhs << ")";
    }
  };

  struct SimpleBinOp : public BinOp {
    SimpleBinOp(String name, String op0, TypeCategory * c) 
      : BinOp(name, op0), category(c) {}
    TypeCategory * category;
    void resolve(Environ & env) {
      type = resolve_binop(env, category, lhs, rhs);
    }
  };

  struct Plus : public BinOp {
    Plus() : BinOp("plus", "+") {}
    void resolve(Environ & env) {
      type = resolve_additive(env, lhs, rhs);
    }
    void make_ct_value();
  };

  void Plus::make_ct_value() {
    if (lhs->type->is(NUMERIC_C) && rhs->type->is(NUMERIC_C))
      ct_value_ = plus_ct_value(this);
    else if (lhs->type->is(POINTER_C))
      ct_value_ = ptr_plus_ct_value(this);
    else if (rhs->type->is(POINTER_C))
      ct_value_ = plus_ptr_ct_value(this);
    else
      abort();
  }

  struct Minus : public BinOp {
    Minus() : BinOp("minus", "-") {}
    void resolve(Environ & env) {
      try {
        type = resolve_additive(env, lhs, rhs);
      } catch(...) {
        if (lhs->type->is(POINTER_C) || rhs->type->is(POINTER_C)) {
          resolve_pointer_binop(P_MINUS, env, lhs, rhs);
          type = env.types.inst("int");
        }
        else
          throw;
      }
    }
    void make_ct_value() {
      if (lhs->type->is(NUMERIC_C) && rhs->type->is(NUMERIC_C))
        ct_value_ = minus_ct_value(this);
    }
  };

  struct Times : public SimpleBinOp {
    Times() : SimpleBinOp("times", "*", NUMERIC_C) {}
    void make_ct_value() {
      ct_value_ = times_ct_value(this);
    }
  };

  struct Div : public SimpleBinOp {
    Div() : SimpleBinOp("div", "/", NUMERIC_C) {}
    void make_ct_value() {
      ct_value_ = div_ct_value(this);
    }
  };

  struct Mod : public SimpleBinOp {
    Mod() : SimpleBinOp("mid", "%", INT_C) {}
    void make_ct_value() {
      ct_value_ = mod_ct_value(this);
    }
  };

  struct BAnd : public SimpleBinOp { 
    BAnd() : SimpleBinOp("band", "&", INT_C) {}
    void make_ct_value() {
      ct_value_ = band_ct_value(this);
    }
  };

  struct BOr : public SimpleBinOp {
    BOr() : SimpleBinOp("bor", "|", INT_C) {}
    void make_ct_value() {
      ct_value_ = bor_ct_value(this);
    }
  };

  struct XOr : public SimpleBinOp {
    XOr() : SimpleBinOp("xor", "^", INT_C) {}
    void make_ct_value() {
      ct_value_ = xor_ct_value(this);
    }
  };

  struct BShift : public BinOp {
    BShift(String n, String op) : BinOp(n, op) {}
    void resolve(Environ & env) {
      // FIXME: Resolve sementans are slightly diffrent from normal
      // binops...
      type = resolve_binop(env, INT_C, lhs, rhs);
    }
  };

  struct LeftShift : public BShift {
    LeftShift() : BShift("lshift", "<<") {}
    template <typename T>
    void make_ct_value() {
      ct_value_ = leftshift_ct_value(this);
    }
  };

  struct RightShift : public BShift {
    RightShift() : BShift("rshift", ">>") {}
    void make_ct_value() {
      ct_value_ = rightshift_ct_value(this);
    }
  };

  struct CompOp : public BinOp {
    CompOp(String n, String op) : BinOp(n, op) {}
    void resolve(Environ & env) {
      check_type(lhs, SCALAR_C);
      check_type(rhs, SCALAR_C);
      if (lhs->type->is(NUMERIC_C)) {
        resolve_binop(env, NUMERIC_C, lhs, rhs);
      } else if (lhs->type->is(POINTER_C)) {
        resolve_pointer_binop(P_COMP, env, lhs, rhs);
      } else {
        abort(); // This should't happen
      }
      //type = env.types.inst("<bool>");
      type = env.types.inst("int");
    }
  };

  struct Eq : public CompOp {
    Eq() : CompOp("eq", "==") {}
    void make_ct_value() {
      ct_value_ = eq_ct_value(this);
    }
  };

  struct Ne : public CompOp {
    Ne() : CompOp("ne", "!=") {}
    void make_ct_value() {
      ct_value_ = ne_ct_value(this);
    }
  };

  struct Lt : public CompOp {
    Lt() : CompOp("lt", "<") {}
    void make_ct_value() {
      ct_value_ = lt_ct_value(this);
    }
  };

  struct Gt : public CompOp {
    Gt() : CompOp("gt", ">") {}
    void make_ct_value() {
      ct_value_ = gt_ct_value(this);
    }
  };

  struct Le : public CompOp {
    Le() : CompOp("le", "<=") {}
    void make_ct_value() {
      ct_value_ = le_ct_value(this);
    }
  };

  struct Ge : public CompOp {
    Ge() : CompOp("ge", ">=") {}
    void make_ct_value() {
      ct_value_ = ge_ct_value(this);
    }
  };

  struct PostIncDec : public Exp {
    PostIncDec(const char * name, String op0) : what_(name), op(op0) {}
    const char * what_;
    const char * what() const {return what_;}
    AST * part(unsigned i) {return exp;}
    Exp * exp;
    String op;
    PostIncDec * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(1);
      exp = parse_exp(p->arg(0), env);
      exp = exp->to_effective(env);
      type = exp->type;
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      exp->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      exp->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << "(" << what_ << " " << exp << ")";
    }
  };

  struct PostInc : public PostIncDec {
    PostInc() : PostIncDec("postinc", "++") {}
  };

  struct PostDec : public PostIncDec {
    PostDec() : PostIncDec("postdec", "--") {}
  };

  struct InitList : public Exp {
    InitList() {}
    const char * what() const {return "init";}
    AST * part(unsigned i) {return parts[i];}
    Vector<Exp *> parts;
    InitList * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      unsigned num_args = p->num_args();
      parts.reserve(num_args);
      add_ast_nodes(p->args_begin(), p->args_end(), parts, Parse<ExpPos>(env));
      type = VOID_T;
      ct_value_ = &ct_nval;
      for (unsigned i = 0; i != parts.size(); ++i) {
        if (!parts[i]->ct_value_) ct_value_ = NULL;
      }
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      for (unsigned i = 0; i != parts.size(); ++i)
        parts[i]->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      for (unsigned i = 0; i != parts.size(); ++i)
        parts[i]->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << "(.\n";
      for (unsigned i = 0; i != parts.size(); ++i)
        f << indent << "  " << adj_indent(2) << parts[i] << "\n";
      f << indent << ")";
    }
    virtual Exp * resolve_to(const Type * type, Environ & env, TypeRelation::CastType rule) {
      // FIXME: resolve individual components of list
      return this;
    }
  };

  //
  //
  //

  Exp * TempInsrPointWrapper::finish(Exp * exp) {
    if (!ip.stmts) return exp;
    // now wrap the expression in an EBlock
    EBlock * b = new EBlock();
    b->stmts = ip.stmts;
    Stmt * cur;
    for (cur = ip.stmts; cur; cur = cur->next)
      env.add(SymbolKey("tmp"), dynamic_cast<Symbol *>(cur), true);
    ip.add(new EStmt(exp)); // and thus add to b->stmts list
    b->set_type(exp);
    ip.reset();
    return b;
  }

  Var * TempInsrPointWrapper::finish(Environ & oenv) {
    if (env.temp_ip != &ip) return NULL;
    if (!ip.stmts) return NULL;
    Stmt * cur = ip.stmts;
    assert(!cur->next); // there should only be one
    oenv.add(SymbolKey("tmp"), dynamic_cast<Symbol *>(cur), true);
    if (ip.where == TempInsrPoint::TopLevelVar) {
      oenv.add_defn(cur);
    } else {
      oenv.add_stmt(cur);
    }
    ip.reset();
    return dynamic_cast<Var *>(cur);
  }


  static Exp * wrap_w_cleanup_flag(Var * temp, Exp * res, bool need_res, Environ & env) {
    Seq * seq = new Seq();
    seq->exps.push_back(res);
    seq->exps.push_back(temp->cleanup->cleanup_flag);
    if (need_res)
      seq->exps.push_back(mk_id(temp,env));
    seq->construct(env);
    return seq;
  }

  void finish(Var * v, TempInsrPointWrapper & wrap,
              TempInsrPointWrapper & wrap2, Environ & oenv) 
  {
    Var * t = wrap2.finish(oenv);
    if (t) v = t;
    if (wrap.ip.stmts) {
      Exp * exp = mk_init(v, v->init, false, oenv);
      if (v->cleanup && v->cleanup->cleanup_flag)
        exp = wrap_w_cleanup_flag(v, exp, false, oenv);
      exp = wrap.finish(exp);
      v->init = NULL;
      v->constructor = exp->as_stmt();
    } else {
      v->fix_up_init(oenv);
    }
  }

  static Var * start_temp(const Type * type, Environ & env) {
    Var * temp = NULL;
    if (env.temp_ip->where == TempInsrPoint::TopLevelVar)
      temp = new TopLevelTemp(type);
    else if (env.temp_ip->where == TempInsrPoint::Var)
      temp = new AutoTemp(type, LV_NORMAL);
    else
      temp = new AutoTemp(type, LV_EEXP);
    env.temp_ip->add(temp);
    temp->add_cleanup(dynamic_cast<const UserType *>(type->unqualified), env);
    if (temp->cleanup)
      temp->cleanup->cleanup_flag = new CleanupFlag(temp);
    return temp;
  }

  Exp * make_temp(Exp * exp, Environ & env) {
    Var * temp = start_temp(exp->type, env);
    if (env.temp_ip->where == TempInsrPoint::ExtendedExp) {
      Exp * res = mk_init(temp, exp, true, env);
      if (temp->cleanup) {
        res = wrap_w_cleanup_flag(temp, res, true, env);
      }
      return res;
    } else {
      temp->init = exp;
      return mk_id(temp, env);
    }
  }

  Exp * make_temp(const Type * type, Environ & env) {
    Var * temp = start_temp(type, env);
    Exp * constructor = temp->make_constructor(dynamic_cast<const UserType *>(type), env);
    if (env.temp_ip->where == TempInsrPoint::ExtendedExp) {
      Seq * seq = new Seq();
      if (constructor)
        seq->exps.push_back(constructor);
      else if (type->is(SCALAR_C))
        seq->exps.push_back(mk_init(temp, mk_literal(0, env), false, env));
      if (temp->cleanup)
        seq->exps.push_back(temp->cleanup->cleanup_flag);
      seq->exps.push_back(mk_id(temp,env));
      seq->construct(env);
      return seq;
    } else {
      temp->constructor = constructor->as_stmt();
      return mk_id(temp, env);
    }
  }

  Exp * parse_anon(const Syntax * p, Environ & env) {
    Type * t = env.types.inst(p->arg(0));
    return make_temp(t, env);
  }

  //
  //
  //

  void parse_stmts_raw(SourceStr str, Environ & env) {
    Parse<TopLevel> prs(env);
    while (!str.empty()) {
      parse_parse::Res r = parse_parse::parse(str);
      const Syntax * p = prs.partly_expand(r.parse);
      if (p->is_a("@")) {
        parse_stmts(p, env);
      } else {
        prs.finish_parse(p);
      }
      str.begin = r.end;
    }
  }

  void parse_stmts(const Syntax * parse, Environ & env) {
    Parse<TopLevel> prs(env);
    for (unsigned i = 0; i < parse->num_args(); ++i) {
      const Syntax * p = prs.partly_expand(parse->arg(i));
      if (p->is_a("@")) {
        parse_stmts(p, env);
      } else {
        prs.finish_parse(p);
      }
    }
  }

  static void pre_parse_stmts(const Syntax * parse, Environ & env) {
    SyntaxEnum * els = partly_expand_list(parse, TopLevel, env);
    while (const Syntax * p = els->next()) {
      pre_parse_decl(p, env);
    }
  }

  static void parse_stmts_first_pass(const Syntax * parse, Environ & env, Collect & collect) {
    Parse<TopLevel,FirstPass> prs(env, collect);
    for (unsigned i = 0; i < parse->num_args(); ++i) {
      const Syntax * p = prs.partly_expand(parse->arg(i));
      if (p->is_a("@")) {
        parse_stmts_first_pass(p, env, collect);
      } else {
        prs.finish_parse(p);
      }
    }
  }

  static void parse_stmts_two_pass(const Syntax * parse, Environ & env) {
    Collect collect;
    parse_stmts_first_pass(parse, env, collect);
    for (Collect::iterator i = collect.begin(), e = collect.end(); i != e; ++i) {
      (*i)->finish_parse(env);
    }
  }

  AST * parse_top(const Syntax * p, Environ & env) {
    assert(p->is_a("top")); // FIXME Error
    parse_stmts(p, env);
    return empty_stmt();
  }

  //
  //
  //

  void Module::compile(CompileWriter & f, Phase phase) const {
    assert(f.target_lang = CompileWriter::ZLE);
    if (phase == Forward) {
      f << "(module " << uniq_name() << ")\n";
    } else {
      f << "(module " << uniq_name() << "\n";
      Vector<SymbolNode *> syms2;
      for (SymbolNode * cur = syms; cur; cur = cur->next) {
        if (!(cur->key.marks || cur->should_skip()))
          syms2.push_back(cur);
      }
      for (Vector<SymbolNode *>::const_reverse_iterator 
             i = syms2.rbegin(), e = syms2.rend();
           i != e; ++i)
      {
        f << adj_indent(2) << *i;
      }
        //} else if (const TopLevelSymbol * tl = dynamic_cast<const TopLevelSymbol *>(cur->value)) {
        //  SymbolKey uniq_key = tl->uniq_name();
        //  uniq_key.ns = tl->tl_namespace();
        //  f << "  " << "(alias " << cur->key << " " << uniq_key << ")\n";
        //} else {
        //  f << "  #?" << cur->key << "\n";
        //}
      f << ")\n";
    }
  }

  Stmt * parse_module(const Syntax * p, Environ & env0, bool pre_parse = false) {
    assert_num_args(p, 1, 2);
    SymbolName n = *p->arg(0);
    Module * m = NULL;
    if (env0.symbols.exists_this_scope(SymbolKey(n, OUTER_NS))) {
      m = env0.symbols.lookup<Module>(p->arg(0), OUTER_NS);
    } else {
      m = new Module();
      m->where = env0.where;
      m->num = env0.where ? NPOS : 0;
      env0.add(SymbolKey(n, OUTER_NS), m);
    }
    if (p->num_args() > 1) {
      assert_num_args(p, 2);
      Environ env = env0.new_scope();
      env.scope = TOPLEVEL;
      env.where = m;

      Collect collect;
      if (pre_parse) {
        pre_parse_stmts(p->arg(1), env);
      } else {
        parse_stmts_first_pass(p->arg(1), env, collect);
      }


      //printf("%s EXPORTS:\n", ~n.to_string());

      //for (unsigned i = 0; i != m->exports.size(); ++i) {
      //  const Syntax * p = m->exports[i];
      //  for (unsigned i = 0, sz = p->num_args(); i != sz; ++i) {
      //    //SymbolName n = *to_export->part(i);
      //    SymbolKey k = expand_binding(p->arg(i), env);
      //    printf("  %s\n", ~k.to_string());
      //    //m->syms = new SymbolNode(expand_binding(p->arg(i), env), 
      //    //                         env.symbols.lookup<Symbol>(p->arg(i)), m->syms);
      //  }
      //}
      
      //printf("%s SYMBOLS:\n", ~n.to_string());

      SymbolList l;
      for (SymbolNode * s = env.symbols.front; s != env.symbols.back; s = s->next) {
        //printf("  %s\n", ~s->key.to_string());
        l.push_back(*s);
      }
      m->syms = l.first;

      //if (env.symbols.front != env.symbols.back) {
      //  SymbolNode * s = env.symbols.front;
      //  SymbolNode * back = new SymbolNode(s->key, s->value);
      //  m->syms = back;
      //  s = s->next;
      //  for (; s != env.symbols.back; s = s->next) {
      //    back->next = new SymbolNode(s->key, s->value);
      //    back = back->next;
      //  }
      //}

      //printf("MODULE %s\n", ~n);
      
      //printf("INTERNAL\n");
      //for (SymbolNode * c = env.symbols.front; c != env.symbols.back; c = c->next)
      //  printf("  %s %p %s %s\n", ~c->key.to_string(), 
      //         c->value,
      //         c->value ? ~c->value->name : "", 
      //         c->value ? ~c->value->uniq_name() : "");
      
      //printf("EXPORTED\n");
      //for (SymbolNode * c = m->syms; c; c = c->next)
      //  printf("  %s %p %s %s\n", ~c->key.to_string(), 
      //         c->value,
      //        c->value ? ~c->value->name : "", 
      //         c->value ? ~c->value->uniq_name() : "");
      
      for (Collect::iterator i = collect.begin(), e = collect.end(); i != e; ++i) {
        (*i)->finish_parse(env);
      }
      env.add_defn(m);
    }
    return empty_stmt();
  }

  Stmt * pre_parse_module(const Syntax * p, Environ & env) {
    return parse_module(p, env, true);
  }

  struct GatherMarks {
    Vector<const Mark *> marks;
    void stripped_mark(const Mark * m) {marks.push_back(m);}
  };

  Stmt * parse_add_prop(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    Module * m = dynamic_cast<Module *>(env.where);
    m->add_prop(*p->arg(0), p->arg(1));
    return empty_stmt();
  }

  Stmt * parse_export(const Syntax * p, Environ & env) {
    //Module * m = dynamic_cast<Module *>(env.where);
    //m->exports.push_back(flatten(p));
    return empty_stmt();
  }

  struct Import : public Symbol, public Declaration {
    const Module * from;
    Import(const Module * f) : from(f) {}
    const char * what() const {return "import";}
    void compile(CompileWriter & f, Phase phase) const {
      assert(f.target_lang = CompileWriter::ZLE);
      f << indent << "(" << "import " << from->uniq_name() << ")\n";
    }
    const InnerNS * tl_namespace() const {return SPECIAL_NS;}
  };

  void import_module(const Module * m, Environ & env, const GatherMarks & gather, bool same_scope = false) {
    SymbolList l;
    for (SymbolNode * cur = m->syms; cur; cur = cur->next) {
      // now add marks back in reverse order
      SymbolKey k = cur->key;
      for (Vector<const Mark *>::const_reverse_iterator 
             i = gather.marks.rbegin(), e = gather.marks.rend();
           i != e; ++i)
        k.marks = mark(k.marks, *i);
      SymbolNode * r = l.push_back(k, cur->value);
      r->flags = cur->flags;
      if (same_scope) 
        r->set_flags(SymbolNode::ALIAS | SymbolNode::IMPORTED);
      else
        r->set_flags(SymbolNode::ALIAS | SymbolNode::IMPORTED | SymbolNode::DIFF_SCOPE);
    }
    env.symbols.splice(l.first, l.last);
    env.add(SymbolKey("", SPECIAL_NS), new Import(m));
  }

  Stmt * parse_import(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    GatherMarks gather;
    const Module * m = lookup_symbol<Module>(p->arg(0), OUTER_NS, env.symbols.front, NULL, 
                                             NormalStrategy, gather);
    import_module(m, env, gather);
    return empty_stmt();
  }

  extern "C" Syntax * module_imports(const Syntax * p, Environ * env) {
    assert_num_args(p, 1);
    Syntax * res = new Syntax;
    SymbolName n = *p->arg(0);
    GatherMarks gather;
    const Module * m = lookup_symbol<Module>(p->arg(0), OUTER_NS, env->symbols.front, NULL, 
                                             NormalStrategy, gather);
    for (SymbolNode * cur = m->syms; cur; cur = cur->next) {
      // now add marks back in reverse order;
      SymbolKey k = cur->key;
      for (Vector<const Mark *>::reverse_iterator 
             i = gather.marks.rbegin(), e = gather.marks.rend();
           i != e; ++i)
        k.marks = mark(k.marks, *i);
      res->add_part(new Syntax(k)); // FIXME not quite right
    }
    return res;
  }

  struct InnerNSDecl : public Declaration, public InnerNS {
    const char * what() const {return "inner_ns";}
    InnerNSDecl() {}
    void finalize(FinalizeEnviron &) {};
    void compile_prep(CompileEnviron &) {};
    void compile(CompileWriter & f, Phase phase) const {
      assert(f.target_lang = CompileWriter::ZLE);
      if (phase == Forward || phase == Normal) {
        f << indent << "(" << "make_inner_ns " << uniq_name() << ")\n";
      }
    }
  };

  Stmt * parse_make_inner_ns(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    SymbolName n = *p->arg(0);
    InnerNS * ns = new InnerNSDecl;
    env.add(SymbolKey(n, INNER_NS), ns);
    return empty_stmt();
  }

  //
  //
  //

  //
  //
  //

  void UserType::compile(CompileWriter & f, Phase phase) const {
    if (f.target_lang != CompileWriter::ZLE)
      return;
    if (phase == Forward) {
      f << "(declare_user_type " << uniq_name() << ")\n";
    } else {
      f << "(make_user_type " << uniq_name() << " " << type;
      if (parent) 
        f << " :(subtype " << parent << ")";
      f << ")\n";
    }
  }

  Stmt * parse_declare_user_type(const Syntax * p, Environ & env) {
    SymbolName name = *p->arg(0);
    UserType * t = dynamic_cast<UserType *>(env.types.inst(SymbolKey(name)));
    if (!(env.symbols.exists_this_scope(SymbolKey(name)) && (t && !t->defined /* FIXME: hack */))) {
      //printf("DECLARE: ADDING SYM %s\n", ~name.to_string());
      UserType * s = new UserType;
      s->category = new TypeCategory(name.name, USER_C);
      add_simple_type(env.types, SymbolKey(name), s, env.where);
    } else {
      //printf("DECLARE: SYM ALREADY EXISTS: %s\n", ~name);
      if (name == "_VTable") {
        const Symbol * s = env.symbols.find<Symbol>(SymbolKey(name));
        //abort();
      }
    }
    return empty_stmt();
  }

  Stmt * parse_make_user_type(const Syntax * p, Environ & env) {
    assert_num_args(p, 1, 2);
    SymbolName name = *p->arg(0);
    UserType * s;
    if (env.symbols.exists_this_scope(SymbolKey(name))) {
      Type * t0 = env.types.inst(SymbolKey(name));
      s = dynamic_cast<UserType *>(t0);
      //sym = s->type_symbol;
    } else {
      s = new UserType;
      s->category = new TypeCategory(name.name, USER_C);
      add_simple_type(env.types, SymbolKey(name), s, env.where);
    }
    if (p->num_args() > 1) {
      s->type = parse_type(p->arg(1), env);
    } else {
      s->type = env.types.inst(SymbolKey(name, TAG_NS));
      //parse_stmt_decl(new Syntax(new Syntax("talias"), p->arg(0), new Syntax(s->type)), env);
    }
    s->module = env.symbols.lookup<Module>(p->arg(0), OUTER_NS);
    assert(s->num == s->module->num);
    s->defined = true;
    s->finalize();
    return empty_stmt();
  }

  Stmt * parse_user_type(const Syntax * p, Environ & env) {
    assert_num_args(p, 1, 3);
    SymbolName name = *p->arg(0);
    //printf("PARSING USER TYPE %s\n", ~name);
    if (!env.symbols.exists_this_scope(SymbolKey(name))) {
      //printf("ADDING SYM %s\n", ~name);
      UserType * s = new UserType;
      s->category = new TypeCategory(name.name, USER_C);
      add_simple_type(env.types, SymbolKey(name), s, env.where);
    } else {
      //printf("SYM ALREADY EXISTS: %s\n", ~name);
      //if (name == "_VTable") {
      //  const Symbol * s = env.symbols.find<Symbol>(SymbolKey(name));
      //  //abort();
      //}
    }
    return parse_module(p, env);
  }

  Stmt * parse_finalize_user_type(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    Module * m = dynamic_cast<Module *>(env.where);
    //printf("FINALIZE USER TYPE %s\n", ~m->name.to_string());
    Type * t0 = env.types.inst(SymbolKey(m->name()));
    UserType * s = dynamic_cast<UserType *>(t0);
    s->module = m;
    s->type = parse_type(p->arg(0), env);
    s->defined = true;
    s->finalize();
    assert(s->num == s->module->num);
    return empty_stmt();
  }

  struct UserCast : public Symbol, public Declaration {
    const char * what() const {return "user-cast";}
    const Type * from;
    const Type * to;
    const Symbol * cast_macro;
    void compile(CompileWriter & cw, Phase) const {
      cw << indent << "(user_cast " << KeyDefNS(key, CAST_NS) << " " 
         << from << " " << to << " " << cast_macro->key << ")\n";
    }
  };

  struct UserCastCompare {
    const Type * from;
    const Type * to;
    UserCastCompare(const Type * f, const Type * t)
      : from(f), to(t) {}
    bool operator() (SymbolKey, const Symbol * s) {
      const UserCast * ut = dynamic_cast<const UserCast *>(s);
      if (!ut) return false;
      return ut->from == from && ut->to == to;
    }
  };

  Stmt * parse_make_subtype(const Syntax * p, Environ & env) {
    //printf(">>%s\n", ~p->to_string());
    assert_num_args(p, 2, 3);
    const Syntax * parent = p->arg(0);
    const Syntax * up_cast = p->arg(1);

    // FIXME: Add check that we are in a user_type
    Module * m = dynamic_cast<Module *>(env.where);
    UserType * parent_t = dynamic_cast<UserType *>(env.types.inst(parent));
    UserType * child_t  = dynamic_cast<UserType *>(env.types.inst(SymbolKey(m->name())));
    UserCast * user_cast = new UserCast;
    user_cast->from = child_t;
    user_cast->to = parent_t;
    user_cast->cast_macro = env.symbols.lookup<Symbol>(up_cast);
    assert(!child_t->parent);
    child_t->parent = parent_t;
    child_t->category = new TypeCategory(child_t->name(), parent_t->category);
    env.add(SymbolKey("up_cast", CAST_NS), user_cast);
    m->syms = new SymbolNode(SymbolKey("up_cast", CAST_NS), user_cast, m->syms);
    return empty_stmt();
  }

  static const Syntax * THIS = new Syntax("this");

  Exp * parse_member_access(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    Exp * exp = parse_exp(p->arg(0), env);
    exp = exp->to_effective(env);
    Syntax * ptr_exp = new Syntax(new Syntax("addrof"), new Syntax(exp));
    if (dynamic_cast<const StructUnion *>(exp->type->unqualified)) {
      const Syntax * np = new Syntax(p->str(), p->part(0), new Syntax(exp), p->arg(1));
      return (new MemberAccess)->parse_self(np, env);
    } else if (const UserType * t = dynamic_cast<const UserType *>(exp->type->unqualified)) {
      // FIXME: Am I calling partly_expand in the right scope here, or correctly at all
      const Syntax * arg1 = partly_expand(p->arg(1), OtherPos, env, EXPAND_NO_MACRO_CALL);
      const Syntax * call;
      if (arg1->is_a("call")) {
        assert_num_args(arg1, 2);
        const Syntax * n = expand_id(arg1->arg(0));
        Syntax * a = new Syntax(*arg1->arg(1));
        a->add_flag(new Syntax(THIS, ptr_exp));
        const Symbol * sym = lookup_symbol<Symbol>(n, DEFAULT_NS, t->module->syms, NULL, StripMarks);
        call = new Syntax(p->str(), arg1->part(0), new Syntax(ID, new Syntax(n, sym)), a);
      } else {
        const Syntax * n = expand_id(arg1);
        const Symbol * sym = lookup_symbol<Symbol>(n, DEFAULT_NS, t->module->syms, NULL, StripMarks);
        Syntax * c = new Syntax(p->str(), ID, new Syntax(n, sym));
        c->add_flag(new Syntax(THIS, ptr_exp));
        call = c;
      }
      //printf("member: %s\n", ~call->to_string());
      return parse_exp(call, env);
    } else {
      abort();
    }
  }

  const Type * change_unqualified(const Type * from, const Type * to) {
    return to;
  }

  Exp * parse_imember_access(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    Exp * exp = parse_exp(p->arg(0), env);
    exp = exp->to_effective(env);
    //printf("::"); p->arg(1)->print(); printf("\n");
    SymbolName id = *expand_id(p->arg(1));
    const UserType * t = dynamic_cast<const UserType *>(exp->type->unqualified);
    if (!t) throw error(p->arg(0), "Expected user type but got ??");
    if (!t->defined) throw error(p->arg(1), "Invalid use of incomplete type");
    exp->type = change_unqualified(exp->type, t->type);
    Syntax * res = new Syntax(new Syntax("member"),
                              new Syntax(exp),
                              p->arg(1));
    return (new MemberAccess)->parse_self(res, env);
  };

  Stmt * parse_memberdecl(const Syntax * p, Environ & env0) {
    //printf("parse_memberdecl\n%s\n", ~p->to_string());
    assert_num_args(p, 2);
    GatherMarks gather;
    const Module * m = lookup_symbol<Module>(p->arg(0), OUTER_NS, env0.symbols.front, NULL, 
                                             NormalStrategy, gather);
    const Syntax * d = p->arg(1);
    const Symbol * s = lookup_symbol<Symbol>(d->arg(0), DEFAULT_NS, m->syms, NULL, StripMarks);
    Environ env = env0.new_scope();
    import_module(m, env, gather, true);
    const Symbol * pm = find_symbol<Symbol>("_parse_memberdecl", m->syms);
    
    parse_top_level(d, env);
    return empty_stmt();
  }

  //
  //
  //

  Exp * cast_up(Exp * exp, const Type * type, Environ & env) {
    exp = exp->to_effective(env);
    const PointerLike * from_ptr = dynamic_cast<const PointerLike *>(exp->type->unqualified);
    const QualifiedType * from_qualified = dynamic_cast<const QualifiedType *>(from_ptr->subtype->root);
    const UserType * from_base = dynamic_cast<const UserType *>(from_ptr->subtype->unqualified);
    if (from_base == type) return exp;
    //printf("CAST UP: %s -> %s\n", ~from_base->to_string(), ~type->to_string());
    //const Type * to_base = from_base->parent;
    const Type * to_qualified = from_qualified
      ? env.types.inst(".qualified", TypeParm(from_qualified->qualifiers), TypeParm(from_base->parent))
      : from_base->parent;
    assert(to_qualified); // FIXME: Maybe Error Message
    const Type * to_ptr = env.types.inst(".ptr", to_qualified);
    NoOpGather gather;
    //UserCastCompare cmp(from_base, from_base->parent);
    const UserCast * cast = lookup_symbol<UserCast>(SymbolKey("up_cast", CAST_NS), exp->syn->str(), 
                                                    from_base->module->syms);//, NULL, NormalStrategy, gather, cmp);
    const Syntax * p = new Syntax(new Syntax("call"), 
                                  new Syntax(new Syntax("id"), new Syntax(cast->cast_macro)),
                                  new Syntax(new Syntax("."), new Syntax(exp)));
    Exp * res = parse_exp(p, env);
    res = res->resolve_to(to_ptr, env);
    return cast_up(res, type, env);
  }

  Exp * cast_down(Exp * exp, const Type * type, Environ & env) {
    abort();
    //if (exp->type == type)
    //  return exp;
    //const UserType * ?? = dynamic_cast<const UserType *>(type);
    //const Type * ???_unqualified = ??->parent;
    //call cast macro;
    //compile to ast;
    //return it;
  }

  Exp * subtype_cast(Exp * exp, const UserType * type, Environ & env) {
    const Type * have = exp->type->effective->unqualified;
    const Type * need = type->unqualified;
    if (have->is(need->category))
      return cast_up(exp, type, env);
    else if (need->is(have->category))
      return cast_down(exp, type, env);
    else
      throw error(exp->syn, "Invalid cast from \"%s\" to \"%s\"",
                  ~have->to_string(), ~need->to_string());
  }

  void Cast::finalize(FinalizeEnviron & env) {
    exp->finalize(env);
  }

  void Cast::compile_prep(CompileEnviron & env) {
    exp->compile_prep(env);
  }
  void Cast::compile(CompileWriter & f) {
    f << "(cast " << type << " " << exp << ")";
  }

  Exp * parse_cast(const Syntax * p, Environ & env, TypeRelation::CastType ctype) {
    assert_num_args(p, 2);
    const Syntax * t = p->arg(0);
    if (t->is_a("(type)"))
      t = t->arg(0);
    Type * type = parse_type(t, env);
    Exp * exp = parse_exp(p->arg(1), env);
    Exp * res = env.type_relation->resolve_to(exp, type, env, ctype);
    if (dynamic_cast<Cast *>(res))
      res->syn = p;
    return res;
  }

  //
  //
  //

 
  //
  //
  //


#if 0
  AST * Fun::part(unsigned i) {
    if (i == 0) {
      return new Terminal(syn->arg(0));
    } else if (i == 1) {
      return new Generic(syn->arg(1));
    } else {
      return body;
    }
  }
#endif

  Fun * parse_fun_forward(const Syntax * p, Environ & env, Collect & collect) {
    assert_num_args(p,3,4);

    //printf("FUN:: %s\n", ~p->to_string());

    SymbolKey name = expand_binding(p->arg(0), env);
    
    bool previous_declared = env.symbols.exists_this_scope(name);
    Fun * f = NULL;
    if (previous_declared) {
      f = env.symbols.find<Fun>(name);
      if (p->num_args() > 3)
        f->parse_forward_i(p, env, collect);
    } else {
      f = new Fun;
      f->storage_class = get_storage_class(p);
      make_static_if_marked(f->storage_class, name);
      bool mangle = name.marks || env.where || f->storage_class == SC_STATIC;
      f->num = mangle ? NPOS : 0;
      f->where = env.where;
      env.add(name, f);
      f->parse_forward_i(p, env, collect);
    }
    return f;
  }

  Fun * parse_fun(const Syntax * p, Environ & env) {
    Collect collect;
    Fun * f = parse_fun_forward(p, env, collect);
    if (!collect.empty())
      f->finish_parse(env);
    return f;
  }
  
  AST * Fun::parse_forward_i(const Syntax * p, Environ & env0, Collect & collect) {
    syn = p;

    //printf("PARSING FLAGS OF %s\n", ~p->to_string());
    inline_ = false;
    if (p->flag("inline")) inline_ = true;
    ct_callback = false;
    if (p->flag("__ct_callback")) ct_callback = true;
    for_ct_ = ct_callback;
    if (p->flag("__for_ct")) for_ct_ = true;
    deps_closed = ct_callback;
    static_constructor = false;
    if (p->flag("__static_constructor")) static_constructor = true;
    if (p->flag("__constructor__")) static_constructor = true;

    if (p->flag("__need_snapshot"))
      env_ss = *env0.top_level_environ;

    // FIXME: is this necessary/correct for a new frame to be created
    //        to expand/parse the _paramaters_.  Of cource is needed for
    //        the body that that is done in parse_body.
    Environ env = env0.new_frame();
    env.where = this;
    env.deps = &deps_;
    env.for_ct = &for_ct_;

    parms = expand_fun_parms(p->arg(1), env);

    ret_type = parse_type(p->arg(2), env);
    type = env.function_sym()->inst(env.types, this);

    body = 0;
    if (p->num_args() > 3) {
      collect.push_back(this);
    } else {
      symbols = env.symbols;
    }

    //sym->value = this;

    return this;
  }

  void Fun::finish_parse(Environ & env0) {
    assert(syn->num_args() > 3);

    Environ env = env0.new_frame();
    env.where = this;
    env.deps = &deps_;
    env.for_ct = &for_ct_;
    env.frame->return_type = ret_type;

    for (Tuple::Parms::const_iterator i = parms->parms.begin(), e = parms->parms.end();
         i != e; ++i)
    {
      SymbolName n = i->name;
      BasicVar * sym = new_other_var(n, i->type);
      i->sym = sym;
      env.add(n, sym);
      //env.symbols.add(n, sym);
    }

    body = dynamic_cast<Block *>(parse_stmt(syn->arg(3), env));
    assert(body); // FiXME

    // fix up order_num so the function body will come after any top
    // level symbols which where created in the local env
    env.add_defn(this);
    symbols = env.symbols;

    FinalizeEnviron fenv;
    fenv.fun_symbols = symbols.front;
    body->finalize(fenv);

    //printf("FUN DEPS: %s %d\n", ~name, deps_.size());
    //for (Deps::iterator i = deps_.begin(), e = deps_.end(); i != e; ++i)
    //  printf("  %s\n", ~(*i)->name);
    //printf("---\n");
  }

  void Fun::finalize(FinalizeEnviron & env0) {
    //if (body) {
    //  FinalizeEnviron env = env0;
    //  env.fun_symbols = symbols.front;
    //  body->finalize(env);
    //}
  }

  void Fun::compile_prep(CompileEnviron & env) {
    if (env.for_macro_sep_c && (env_ss || is_macro))
      env.for_macro_sep_c->macro_funs.push_back(this);
    if (body) {
      body->compile_prep(env);
    }
  }

  void Fun::compile(CompileWriter & f, Phase phase) const {
    if (!body && phase == Body)
      return;
    if (env_ss && phase != Forward) {
      f << "(var " << uniq_name() << '$' << "env_ss" << " (.ptr (struct EnvironSnapshot)))\n";
    }
    f << "(fun " << uniq_name();
    f << " " << parms;
    f << " " << ret_type;
    write_storage_class(f);
    if (inline_)
        f << " :inline";
    if (static_constructor)
      f << " :__constructor__";
    if (body && phase != Forward) {
      f.in_fun = this;
      f << " " << body;
      f.in_fun = NULL;
    }
    f << ")\n";
  }
  
  struct Return : public Stmt {
    Exp * exp;
    Return() {}
    const char * what() const {return "return";}
    Return * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(1);
      exp = parse_exp(p->arg(0), env);
      exp = exp->resolve_to(env.frame->return_type, env);
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      exp->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      exp->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << indent << "(return " << exp << ")\n";
    }
  };

  struct Call : public Exp {
    Call() {} 
    const char * what() const {return "call";}
    //AST * part(unsigned i) {return i == 0 ? lhs : new Generic(syn->arg(1), parms);}
    Exp * lhs;
    Vector<Exp *> parms;
    Call * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(2);
      lhs = parse_exp(p->arg(0), env);
      p = p->arg(1);
      add_ast_nodes(p->args_begin(), p->args_end(), parms, Parse<ExpPos>(env));
      const Function * ftype = dynamic_cast<const Function *>(lhs->type);
      if (!ftype) {
        if (const Pointer * t = dynamic_cast<const Pointer *>(lhs->type))
          ftype = dynamic_cast<const Function *>(t->subtype);
      }
      if (!ftype)
        throw error (lhs->syn, "Expected function type");
      type = ftype->ret;
      lvalue = type->addressable ? LV_NORMAL : LV_FALSE;
      if (!ftype->parms->vararg && parms.size() != ftype->parms->parms.size()) 
        throw error(syn->arg(1), 
                    "Wrong number of parameters, expected %u but got %u when calling %s",
                    ftype->parms->parms.size(), parms.size(), "??");
      else if (ftype->parms->vararg && parms.size() < ftype->parms->parms.size())
        throw error(syn->arg(1),
                    "Not enough parameters, expected at least %u but got %u when calling %s",
                    ftype->parms->parms.size(), parms.size(), "??");
      const int typed_parms = ftype->parms->parms.size();
      const int num_parms = parms.size();
      int i = 0;
      for (;i != typed_parms; ++i) {
        parms[i] = parms[i]->resolve_to(ftype->parms->parms[i].type, env);
      }
      for (;i != num_parms; ++i) {
        parms[i] = parms[i]->def_arg_prom(env);
      }
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      lhs->finalize(env);
      const int num_parms = parms.size();
      for (int i = 0; i != num_parms; ++i)
        parms[i]->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      lhs->compile_prep(env);
      const int num_parms = parms.size();
      for (int i = 0; i != num_parms; ++i)
        parms[i]->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << "(call " << lhs << " (.";
      int i = 0;
      while (i != parms.size()) {
        f << " ";  
        parms[i]->compile(f);
        ++i;
      }
      f << "))";
    }
  };

  //
  //
  //
  
  Stmt * parse_type_alias(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    SymbolKey n = expand_binding(p->arg(0), DEFAULT_NS, env);
    Type * of = parse_type(p->arg(1), env);
    TypeAlias * decl = new TypeAlias(of);
    decl->syn = p;
    SimpleType * talias = add_simple_type(env.types, n, decl, env.where);
    return empty_stmt();
  }

  void TypeAlias::compile(CompileWriter & f, Phase phase) const {
    if (phase == Body) return;
    f << indent << "(talias " << uniq_name() << " " << of << ")\n";
  }

  Stmt * parse_struct_union(StructUnion::Which which, const Syntax * p, Environ & env0) {
    //assert(p->is_a(what()));
    const Syntax * name = p->arg(0);
    StructUnion * decl;
    if (env0.symbols.exists_this_scope(name, TAG_NS)) {
      Type * t0 = env0.types.inst(name, TAG_NS);
      decl = dynamic_cast<StructUnion *>(t0);
    } else {
      SymbolKey n = expand_binding(name, TAG_NS, env0);
      if (which == Struct::STRUCT) decl = new Struct;
      else                         decl = new Union;
      // fixme: add_simple_type calls finalize() which it probably
      // should't do since we do explicitly latter
      add_simple_type(env0.types, n, decl, env0.where);
    }
    decl->syn = p;
    decl->env = env0.new_scope();
    decl->env.scope = OTHER;
    if (p->num_args() > 1) {
      decl->have_body = true;
      if (p->what().name[0] == '.') {
        for (unsigned i = 1; i != p->num_args(); ++i) {
          const Syntax * q = p->arg(i);
          assert(q);
          const Syntax * name_p = q->part(1);
          assert(name_p);
          SymbolKey name = expand_binding(name_p, decl->env);
          const Type * type = parse_type(q->part(0), decl->env);
          OtherVar * v = new_other_var(name, type);
          v->name_p = name_p;
          decl->env.add(name, v);
          decl->members.push_back(v);
        }
      } else {
        const Syntax * q = p->arg(1);
        add_ast_nodes(q->parts_begin(), q->parts_end(), decl->members, Parse<FieldPos>(decl->env));
      }
      env0.add_defn(decl);
    } else {
      decl->have_body = false;
    }
    //StringBuf type_name;
    //type_name << "struct " << what();
    //if (s->members.empty())
    //  fprintf(stderr, "Warning: %s\n", error(p, "Empty Struct Currently Unsupported")->message().c_str());
    decl->SimpleType::finalize();
    return empty_stmt();
  }

  Stmt * parse_struct(const Syntax * p, Environ & env) {
    return parse_struct_union(Struct::STRUCT, p, env);
  }

  Stmt * parse_union(const Syntax * p, Environ & env) {
    return parse_struct_union(Union::UNION, p, env);
  }

  void StructUnion::compile(CompileWriter & f, Phase phase) const {
    if (!have_body && phase == Declaration::Body) return;
    f << indent << "(." << what() << " " << uniq_name();
    if (have_body && phase != Forward) {
      f << "\n";
      for (int i = 0; i != members.size(); ++i) {
        BasicVar * v = members[i].sym;
        f << adj_indent(2) << indent;
        f << "(" << v->type << " " << v->uniq_name() << ")\n";
      }
    }
    f << ")\n";
  }

  void Struct::finalize_hook() {
    size_ = 0;
    align_ = 0;
    for (unsigned i = 0; i != members.size(); ++i) {
      members[i].offset = size_;
      const Type * t = members[i].sym->type;
      if (t->storage_align() > align_) align_ = t->storage_align();
      unsigned align_offset = size_ % t->storage_align();
      if (align_offset != 0) size_ += t->storage_align() - align_offset;
      size_ += t->storage_size();
    }
    defined = true;
  }

  void Union::finalize_hook() {
    size_ = 0;
    align_ = 0;
    for (unsigned i = 0; i != members.size(); ++i) {
      members[i].offset = 0;
      const Type * t = members[i].sym->type;
      if (t->storage_align() > align_) align_ = t->storage_align();
      if (t->storage_size() > size_) size_ = t->storage_size();
    }
    defined = true;
  }


  Stmt * parse_enum(const Syntax * p, Environ & env) {
    SymbolName name = *p->arg(0);
    Enum * decl;
    if (env.symbols.exists_this_scope(SymbolKey(name, TAG_NS))) {
      decl = dynamic_cast<Enum *>(env.types.inst(SymbolKey(name, TAG_NS)));
    } else {
      decl = new Enum;
      decl->exact_type = env.types.inst("int")->exact_type;
      // fixme: add_simple_type calls finalize() which it probably
      // should't do since we do explicitly latter
      add_simple_type(env.types, SymbolKey(name, TAG_NS), decl, env.where);
    }
    decl->syn = p;
    decl->body = NULL;
    if (p->num_args() > 1) {
      Vector<TypeParm> q_parms;
      q_parms.push_back(TypeParm(QualifiedType::CT_CONST));
      q_parms.push_back(TypeParm(decl));
      const Type * t = env.types.find(".qualified")->inst(q_parms);
      int val = 0;
      const Syntax * arg1;
      unsigned i;
      if (p->what().name[0] == '.') {
        arg1 = decl->body = p;
        i = 1;
      } else {
        arg1 = decl->body = p->arg(1);
        i = 0;
      }
      decl->members.reserve(arg1->num_args() - i);
      for (; i != arg1->num_args(); ++i) {
        const Syntax * arg = arg1->arg(i);
        if (arg->num_parts() > 1) {
          Exp * e = parse_exp(arg->part(1), env);
          e = e->resolve_to(env.types.inst("int"), env);
          val = e->ct_value<target_int>();
        }
        SymbolName n = *arg->part(0);
        Enum::Member mem(arg1, new_other_var(n, t), val);
        VarSymbol * sym = mem.sym;
        val++;
        decl->members.push_back(mem);
        sym->ct_value = &decl->members.back().ct_value;
        env.add_internal(n, sym);
      }
      env.add_defn(decl);
    }
    decl->Int::finalize();
    return empty_stmt();
  }

  void Enum::compile(CompileWriter & f, Phase phase) const {
    if (!body && phase == Body) return;
    f << indent << "(.enum " << uniq_name();
    if (body && phase != Forward) {
      f << "\n";
      for (int i = 0; i != members.size(); ++i)
        f << adj_indent(2) << indent 
          << " (" << members[i].sym << " " << members[i].ct_value.val << ")\n";
    }
    f << ")\n";
  }

  void Enum::finalize_hook() {
    defined = true;
  }

  //
  //
  //

  struct SizeOf : public ExpLeaf {
    SizeOf() {}
    const char * what() const {return "sizeof";}
    const Type * sizeof_type;
    SizeOf * parse_self(const Syntax * p, Environ & env);
    void finalize(FinalizeEnviron &) {}
    void compile(CompileWriter & f) {
      f << "(n " << sizeof_type->size() << " (size_t))";
    }
  };
  
  SizeOf * SizeOf::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(1);
    if (p->arg(0)->is_a("(type)")) {
      sizeof_type = parse_type(p->arg(0)->arg(0), env);
    } else {
      Exp * exp = parse_exp(p->arg(0), env);
      sizeof_type = parse_type(new Syntax(new Syntax(".typeof"), new Syntax(exp)), env);
    }
    type = env.types.ct_const(env.types.inst(".size"));
    ct_value_ = new CT_Value<target_size_t>(sizeof_type->size());
    return this;
  }

  //

  struct SyntaxC : public ExpLeaf {
    SyntaxC() {}
    const char * what() const {return "syntax";}
    static Vector<const Syntax *> keep_me;
    const Syntax * syn_p;
    unsigned syn_num;
    SyntaxC * parse_self(const Syntax * p, Environ & env);
    void compile_prep(CompileEnviron & env);
    void compile(CompileWriter & f);
  };

  const Syntax * parse_syntax_c(const Syntax * p) {
    assert_num_args(p, 1);
    const Syntax * syn;
    String what = p->part(0)->what().name;
    if (what == "syntax") {
      syn = p->part(1);
      //fprintf(stdout, "SYN %s<<\n", ~syn->to_string());
    } else if (what == "raw_syntax") {
      using namespace parse_parse;
      Res r = parse(p->part(1)->str());
      syn = r.parse;
      //fprintf(stdout, "RSYN %s<<\n", ~syn->to_string());
    } else {
      abort();
    }
    ChangeSrc<SyntaxSourceInfo> cs(syn);
    syn = new Syntax(cs, *syn);
    SyntaxC::keep_me.push_back(syn);
    return syn;
  }

  SyntaxC * SyntaxC::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(1);
    syn_p = parse_syntax_c(p);
    syn_num = (unsigned)-1;
    *env.for_ct = true;
    type = env.types.inst(".ptr", env.types.inst("UnmarkedSyntax"));
    type = env.types.ct_const(type);
    return this;
  }
  void SyntaxC::compile_prep(CompileEnviron & env) {
    if (env.for_macro_sep_c) {
      syn_num = env.for_macro_sep_c->syntaxes.size();
        env.for_macro_sep_c->syntaxes.push_back(this);
    }
  }
  void SyntaxC::compile(CompileWriter & f) {
    if (f.for_macro_sep_c) {
      f.printf("(member (deref (plus _syntaxes %d)) syn)", syn_num);
    } else if (f.for_compile_time()) 
      f.printf("(cast (.ptr (struct UnmarkedSyntax)) %p)", syn_p); 
    else
      f.printf("(cast (.ptr (struct UnmarkedSyntax)) 0)");
  }

  Vector<const Syntax *> SyntaxC::keep_me;

  struct EnvironSnapshot : public ExpLeaf {
    EnvironSnapshot() {}
    const char * what() const {return "environ_snapshot";}
    SymbolNode * env_ss;
    EnvironSnapshot * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(0);
      env_ss = *env.top_level_environ;
      type = env.types.inst(".ptr", env.types.inst("EnvironSnapshot"));
      type = env.types.ct_const(type);
      *env.for_ct = true;
      return this;
    }
    void compile(CompileWriter & f) {
      if (f.in_fun && f.in_fun->env_ss) 
        f.printf("(id %s$env_ss)", ~f.in_fun->uniq_name());
      else if (f.for_compile_time())
        f.printf("(cast (.ptr (struct EnvironSnapshot)) (n %p (unsigned-long)))", env_ss); 
      else 
        f.printf("(cast (.ptr (struct EnvironSnapshot)) (n 0 (unsigned-long)))");
    }
  };

  //

  AST * parse_top(const Syntax * p) {
    Environ env(TOPLEVEL);
    return parse_top(p, env);
  }

  Stmt * try_just_decl(const Syntax * p, Environ & env);
  Stmt * try_decl_first_pass(const Syntax * p, Environ & env, Collect & collect);
  Stmt * try_just_stmt(const Syntax * p, Environ & env);
  Exp * try_just_exp(const Syntax * p, Environ & env);
  EStmt * try_exp_stmt(const Syntax * p, Environ & env);

  //template <typename T>
  //T * try_ast(const Syntax * p, Environ & env) {
  //  if (p->have_entity()) {
  //    if (T * ast = p->entity<T>()) {
  //      return ast;
  //    } else if (Error * err = p->entity<Error>()) {
  //      throw err;
  //    } else {
  //     abort(); // FIXME Error message
  //    }
  //  }
  //  return 0;
  //}

  template <typename T>
  T * try_ast(const Syntax * p, Environ & env);

  template <>
  Exp * try_ast<Exp>(const Syntax * p, Environ & env) {
    if (p->have_entity()) {
      if (Exp * ast = p->entity<Exp>()) {
        return ast;
      } else if (Error * err = p->entity<Error>()) {
        throw err;
      } else {
        abort(); // FIXME Error message
      }
    }
    return 0;
  }

  template <>
  Stmt * try_ast<Stmt>(const Syntax * p, Environ & env) {
    if (p->have_entity()) {
      if (Stmt * ast = p->entity<Stmt>()) {
        return ast;
      } else if (Exp * ast = p->entity<Exp>()) {
        return ast->as_stmt();
      } else if (Error * err = p->entity<Error>()) {
        throw err;
      } else {
        abort(); // FIXME Error message
      }
    }
    return 0;
  }

  template <>
  Stmt * Parse<TopLevel>::finish_parse(const Syntax * p) const {
    Stmt * res;
    //printf("Parsing top level:\n  %s\n", ~p->to_string());
    res = try_ast<Stmt>(p, env);
    if (res) return res;
    res = try_just_decl(p, env);
    if (res) return res;
    throw error (p, "Unsupported primative at top level:: %s", ~p->what());
    //throw error (p, "Expected top level expression.");
  }

  Stmt * parse_top_level(const Syntax * p, Environ & env) {
    return Parse<TopLevel>(env)(p);
  }

  template <>
  Stmt * Parse<TopLevel,FirstPass>::finish_parse(const Syntax * p) const {
    Stmt * res;
    //printf("Parsing top level fp:\n  %s\n", ~p->to_string());
    res = try_ast<Stmt>(p, env);
    if (res) return res;
    res = try_decl_first_pass(p, env, collect);
    if (res) return res;
    throw error (p, "Unsupported primative at top level: %s", ~p->what());
    //throw error (p, "Expected top level expression.");
  }
    
  Stmt * parse_top_level_first_pass(const Syntax * p, Environ & env, Collect & collect) {
    return Parse<TopLevel,FirstPass>(env,collect)(p);
  }

  template <>
  BasicVar * Parse<FieldPos>::finish_parse(const Syntax * p) const {
    Stmt * res;
    // FIXME
    //res = try_ast<Stmt>(p, env);
    //if (res) return res;
    //res = try_decl(p, env);
    String what = p->what().name;
    if (what == "var") return parse_field_var(p, env);
    throw error (p, "Unsupported primitive inside a struct or union: %s", ~p->what());
    //throw error (p, "Expected struct or union member.");
  }

  const Syntax * pre_parse_decl(const Syntax * p, Environ & env) {
    String what = p->what().name;
    //printf("PRE PARSING %s\n", ~p->to_string());
    if (what == "struct")  parse_struct(p, env);
    if (what == ".struct") parse_struct(p, env);
    if (what == "union")   parse_union(p, env);
    if (what == ".union")  parse_union(p, env);
    if (what == "enum")    parse_enum(p, env);
    if (what == ".enum")   parse_enum(p, env);
    if (what == "talias")  parse_type_alias(p, env);
    if (what == "module")         pre_parse_module(p, env);
    if (what == "make_user_type") parse_make_user_type(p, env);
    if (what == "user_type")          parse_user_type(p, env);
    if (what == "finalize_user_type") parse_finalize_user_type(p, env);
    if (what == "make_subtype") parse_make_subtype(p, env);
    if (what == "declare_user_type") parse_declare_user_type(p, env);
    return p;
  }

  template <>
  Stmt * Parse<StmtPos>::finish_parse(const Syntax * p) const {
    Stmt * res;
    res = try_ast<Stmt>(p, env);
    if (res) return res;
    res = try_just_stmt(p, env);
    if (res) return res;
    res = try_exp_stmt(p, env);
    if (res) return res;
    //throw error (p, "Unsupported primative at statement position: %s", ~p->name);
    throw error (p, "Expected statement in: %s.", ~p->to_string());
  }

  Stmt * parse_stmt(const Syntax * p, Environ & env) {
    return Parse<StmtPos>(env)(p);
  }

  template <>
  Stmt * Parse<StmtDeclPos>::finish_parse(const Syntax * p) const {
    Stmt * res;
    res = try_ast<Stmt>(p, env);
    if (res) return res;
    res = try_just_decl(p, env);
    if (res) return res;
    res = try_just_stmt(p, env);
    if (res) return res;
    res = try_exp_stmt(p, env);
    if (res) return res;
    //throw error (p, "Unsupported primative at statement position: %s", ~p->name);
    p->print(); printf("\n");
    throw error (p, "Expected statement or declaration.");
  }

  Stmt * parse_stmt_decl(const Syntax * p, Environ & env) {
    return Parse<StmtDeclPos>(env)(p);
  }

  Stmt * parse_stmts(Parts::const_iterator i, Parts::const_iterator end) {
    return NULL;
  }

  template<> 
  Exp * Parse<ExpPos>::finish_parse(const Syntax * p) const {
    Exp * res;
    res = try_ast<Exp>(p, env);
    if (res) return res;
    res = try_just_exp(p, env);
    if (res) return res;
    //abort();
    throw error (p, "Unsupported primative at expression position: %s", ~p->what().name);
    //throw error (p, "Expected expression.");
  }

  Exp * parse_exp(const Syntax * p, Environ & env) {
    return Parse<ExpPos>(env)(p);
  }

  Stmt * try_decl_common(const Syntax * p, Environ & env) {
    String what = p->what().name;
    if (what == "struct")  return parse_struct(p, env);    
    if (what == ".struct")  return parse_struct(p, env);
    if (what == "union")   return parse_union(p, env);
    if (what == ".union")   return parse_union(p, env);
    if (what == "enum")    return parse_enum(p, env);
    if (what == ".enum")    return parse_enum(p, env);
    if (what == "talias")  return parse_type_alias(p, env);
    if (what == "local_label") return (new LocalLabelDecl)->parse_self(p, env);
    if (what == "macro")   return parse_map(p, env);
    if (what == "smacro")  return parse_map(p, env);
    if (what == "make_macro")         return parse_macro(p, env);
    if (what == "make_syntax_macro")  return parse_macro(p, env);
    if (what == "fluid_binding") return parse_fluid_binding(p, env);
    if (what == "module")        return parse_module(p, env);
    if (what == "import")        return parse_import(p, env);
    if (what == "make_inner_ns") return parse_make_inner_ns(p, env);
    if (what == "make_user_type") return parse_make_user_type(p, env);
    if (what == "user_type")          return parse_user_type(p, env);
    if (what == "finalize_user_type") return parse_finalize_user_type(p, env);
    if (what == "make_subtype") return parse_make_subtype(p, env);
    if (what == "declare_user_type") return parse_declare_user_type(p, env);
    if (what == "export")  return parse_export(p, env);
    if (what == "add_prop")  return parse_add_prop(p, env);
    if (what == "memberdecl") return parse_memberdecl(p, env);
    return 0;
  }

  Stmt * try_decl_first_pass(const Syntax * p, Environ & env, Collect & collect) {
    String what = p->what().name;
    if (what == "var")     return parse_var_forward(p, env, collect);
    if (what == "fun" )    return parse_fun_forward(p, env, collect), empty_stmt();
    return try_decl_common(p, env);
  }

  Stmt * try_just_decl(const Syntax * p, Environ & env) {
    String what = p->what().name;
    if (what == "var")     return parse_var(p, env);
    if (what == "fun" )    return parse_fun(p, env), empty_stmt();
    return try_decl_common(p, env);
  }

  Stmt * try_just_stmt(const Syntax * p, Environ & env) {
    String what = p->what().name;
    if (what == "goto")    return (new Goto)->parse_self(p, env);
    if (what == "label")   return parse_label(p, env);
    if (what == "case")    return (new Case)->parse_self(p, env);
    if (what == "if")      return (new If)->parse_self(p, env);
    if (what == ".switch") return (new Switch)->parse_self(p, env);
    if (what == "block")   return (new Block)->parse_self(p, env);
    if (what == "noop")    return (new NoOp)->parse_self(p, env);
    if (what == "return")  return (new Return)->parse_self(p, env);
    if (what == "cleanup") return (new Cleanup)->parse_self(p, env);
    return 0;
  }

  Exp * try_just_exp(const Syntax * p, Environ & env) {
    String what = p->what().name;
    if (what == "id")      return (new Id)->parse_self(p, env);
    if (what == "n")       return (new Literal)->parse_self(p, env);
    if (what == "f")       return (new FloatC)->parse_self(p, env);
    if (what == "c")       return (new CharC)->parse_self(p, env);
    if (what == "s")       return (new StringC)->parse_self(p, env);
    if (what == "eif")     return (new EIf)->parse_self(p, env);
    if (what == "assign")      return parse_assign(p, env);
    if (what == "init-assign") return parse_init_assign(p, env);
    if (what == "plus")    return (new Plus)->parse_self(p, env);
    if (what == "minus")   return (new Minus)->parse_self(p, env);
    if (what == "lshift")  return (new LeftShift)->parse_self(p, env);
    if (what == "rshift")  return (new RightShift)->parse_self(p, env);
    if (what == "times")   return (new Times)->parse_self(p, env);
    if (what == "div")     return (new Div)->parse_self(p, env);
    if (what == "mod")     return (new Mod)->parse_self(p, env);
    if (what == "bor")     return (new BOr)->parse_self(p, env);
    if (what == "xor")     return (new XOr)->parse_self(p, env);
    if (what == "band")    return (new BAnd)->parse_self(p, env);
    if (what == "postinc") return (new PostInc)->parse_self(p, env);
    if (what == "postdec") return (new PostDec)->parse_self(p, env);
    if (what == "neg")     return (new Neg)->parse_self(p, env);
    if (what == "eq")      return (new Eq)->parse_self(p, env);
    if (what == "ne")      return (new Ne)->parse_self(p, env);
    if (what == "lt")      return (new Lt)->parse_self(p, env);
    if (what == "gt")      return (new Gt)->parse_self(p, env);
    if (what == "le")      return (new Le)->parse_self(p, env);
    if (what == "ge")      return (new Ge)->parse_self(p, env);
    if (what == "not")     return (new Not)->parse_self(p, env);
    if (what == "bnot")    return (new Compliment)->parse_self(p, env);
    if (what == "addrof")  return parse_addrof(p, env);
    if (what == "deref")   return parse_deref(p, env);
    if (what == "member")  return parse_member_access(p, env);
    if (what == "imember") return parse_imember_access(p, env);
    if (what == "call")    return (new Call)->parse_self(p, env);
    if (what == "anon")    return parse_anon(p, env);
    if (what == "seq")     return (new Seq)->parse_self(p, env);
    if (what == "eblock")  return (new EBlock)->parse_self(p, env);
    if (what == "sizeof")  return (new SizeOf)->parse_self(p, env);
    if (what == "cast")    return parse_cast(p, env, TypeRelation::Explicit);
    if (what == "icast")   return parse_cast(p, env, TypeRelation::Implicit);
    if (what == ".")       return (new InitList)->parse_self(p, env);
    //if (what == "empty")   return (new Empty)->parse_self(p, env);
    if (what == "syntax")           return (new SyntaxC)->parse_self(p, env);
    if (what == "raw_syntax")       return (new SyntaxC)->parse_self(p, env);
    if (what == "environ_snapshot") return (new EnvironSnapshot)->parse_self(p, env);
    if (what == "c-assign") {
      AST * ast = try_just_exp(new Syntax(p->str(), p->arg(0), p->arg(1), p->arg(2)), env);
      if (!ast) return 0;
      BinOp * binop = dynamic_cast<BinOp *>(ast);
      StringBuf op;
      op << binop->op << "=";
      return new CompoundAssign(what, op.freeze(), binop, env);
    }
    return 0;
  }

  EStmt * try_exp_stmt(const Syntax * p, Environ & env) {
    TempInsrPointWrapper wrap(env);
    Exp * exp = try_just_exp(p, wrap.env);
    if (exp) {
      exp = wrap.finish(exp);
      return new EStmt(exp);
    } else {
      return NULL;
    }
  }

  //
  // VarDeclaration methods
  //

  void VarDeclaration::write_storage_class_c(CompileWriter & f) const {
    StorageClass sc = storage_class;
    if (f.for_compile_time())
      if (const TopLevelVarDecl * tl = top_level()) {
        if (tl->ct_ptr)
          sc = SC_EXTERN;
        else if (sc == SC_STATIC)
          sc = SC_NONE;
      }
    switch (sc) {
    case SC_AUTO: 
        f << "auto "; break;
    case SC_STATIC: 
      f << "static "; break;
    case SC_EXTERN: 
      f << "extern "; break;
    case SC_REGISTER: 
      f << "register "; break;
    default:
      break;
    }
  }

  void VarDeclaration::write_storage_class(CompileWriter & f) const {
    StorageClass sc = storage_class;
    if (f.for_compile_time())
      if (const TopLevelVarDecl * tl = top_level()) {
        if (tl->ct_ptr)
          sc = SC_EXTERN;
        else if (sc == SC_STATIC)
          sc = SC_NONE;
      }
    switch (sc) {
    case SC_AUTO: 
        f << " :auto"; break;
    case SC_STATIC: 
      f << " :static"; break;
    case SC_EXTERN: 
      f << " :extern"; break;
    case SC_REGISTER: 
      f << " :register"; break;
    default:
      break;
    }
  }
  
  void TopLevelVarDecl::calc_deps_closure() const {  
    deps_closed = true;
    for (unsigned i = 0, sz = deps_.size(); i < sz; ++i) {
      const TopLevelVarDecl * d = deps_[i];
      if (!d->deps_closed) d->calc_deps_closure();
      deps_.merge(d->deps_);
      if (!d->deps_closed) deps_closed = false;
      if (d->for_ct_) for_ct_ = true;
    }
  }

  //
  // __compile__
  //

  void escape(OStream & out, SourceStr str) {
    for (const char * i = str.begin; i != str.end; ++i) {
      switch (*i) {
      case '\a': out.put("\\a"); break;
      case '\b': out.put("\\b"); break;
      case '\f': out.put("\\f"); break;
      case '\n': out.put("\\n"); break;
      case '\t': out.put("\\t"); break;
      case '\v': out.put("\\v"); break;
      case '\"': out.put("\\\""); break;
      case '\x00' - '\x1f': out.printf("\\x%.2x", *i); break;
      default: out.put(*i);
      }
    }
  }
  
  static void sep(CompileWriter & cw, const char * what) {
    cw << "\n"
       << "#\n"
       << "# " << what << "\n"
       << "#\n\n";
  }

  void compile(TopLevelSymbolTable * tls, CompileWriter & cw) {

    SymbolNode * syms = *tls->front;
    Stmt * defns = tls->first;

    typedef const TopLevelVarDecl * VarP;
    typedef const TypeDeclaration * TypeP;
    typedef const Module * ModuleP;
    typedef SymbolNode * OtherP;
    typedef Vector<VarP> Vars;
    typedef Vector<TypeP> Types;
    typedef Vector<ModuleP> Modules;
    typedef Vector<OtherP> Others;
    typedef Vars::const_iterator VarsItr;
    typedef Types::const_iterator TypesItr;
    typedef Modules::const_iterator ModulesItr;
    typedef Others::const_iterator OthersItr;

    Vars vars;
    Vars var_defns;
    Types types;
    Types type_defns;;
    Modules modules;
    Others others;
    //Others other_defns;

    for (SymbolNode * cur = syms; cur; cur = cur->next) {
      if (cur->should_skip())
        continue;

      const Symbol * sym = cur->value;

      //printf("?? %s\n", ~cur->key.to_string());
      if (cur->alias()) {
        if (cw.target_lang != CompileWriter::ZLE)
          others.push_back(cur);
        continue;
      }
      VarP var = dynamic_cast<VarP>(sym);
      if (var) {
        if (cw.for_compile_time()) {
          if (cw.deps->have(var)) {
            vars.push_back(var);
          }
        } else if (cw.for_macro_sep_c || !var->for_ct()) {
          vars.push_back(var);
        }
        continue;
      }
      TypeP type = dynamic_cast<TypeP>(sym);
      if (type) {
        types.push_back(type);
        continue;
      }
      if (cw.target_lang != CompileWriter::ZLE)
        continue;
      ModuleP module = dynamic_cast<ModuleP>(sym);
      if (module) {
        modules.push_back(module);
      }
      others.push_back(cur);
    }

    std::reverse(vars.begin(), vars.end());
    std::reverse(types.begin(), types.end());
    std::reverse(modules.begin(), modules.end());
    std::reverse(others.begin(), others.end());

    for (Stmt * cur = defns; cur; cur = cur->next) {
      VarP var = dynamic_cast<VarP>(cur);
      if (var) {
        if (cw.for_compile_time()) {
          if (cw.deps->have(var)) {
            var_defns.push_back(var);
          }
        } else if (cw.for_macro_sep_c || !var->for_ct()) {
          var_defns.push_back(var);
        }
        continue;
      }
      TypeP type = dynamic_cast<TypeP>(cur);
      if (type) {
        type_defns.push_back(type);
        continue;
      }
      if (cw.target_lang != CompileWriter::ZLE)
        continue;
      //other_defns.push_back(cur);
    }

  
    Vector<AST *> init, cleanup;
    const TopLevelVarSymbol * tl = NULL;

    if (cw.target_lang == CompileWriter::ZLE) {
      sep(cw, "module decls");
      for (ModulesItr i = modules.begin(), e = modules.end(); i != e; ++i) {
        (*i)->compile(cw, Declaration::Forward);
      }
    }

    sep(cw, "type decls");

    for (TypesItr i = types.begin(), e = types.end(); i != e; ++i) {
      (*i)->compile(cw, Declaration::Forward);
    }

    sep(cw, "type definitions");

    for (TypesItr i = type_defns.begin(), e = type_defns.end(); i != e; ++i) {
      (*i)->compile(cw, Declaration::Body);
    }

    if (cw.for_macro_sep_c) {
      
      sep(cw, "macro sep. c. stuff");

      for (VarsItr i = vars.begin(), e = vars.end(); i != e; ++i) {
        const_cast<TopLevelVarDecl *>(*i)->compile_prep(cw); // evil I know...
      }
      
      unsigned macro_funs_size = cw.for_macro_sep_c->macro_funs.size();
      cw << "(var _macro_funs_size (unsigned) " << macro_funs_size << ")\n";
      if (macro_funs_size  > 0 ) {
        cw << "(var _macro_funs (.array (.ptr (char :const)) " << macro_funs_size << ") (.\n";
        for (Vector<Fun *>::const_iterator i = cw.for_macro_sep_c->macro_funs.begin(), 
               e = cw.for_macro_sep_c->macro_funs.end(); i != e; ++i)
        {
          cw << "  (s \"" << ~(*i)->uniq_name() <<  "\")\n";
        }
        cw << "))\n";
      }

      unsigned syntaxes_size = cw.for_macro_sep_c->syntaxes.size();
      if (syntaxes_size > 0) {
        cw << "(var _syntaxes_size (unsigned) " << syntaxes_size << ")\n";
        cw << "(.struct _syntaxes ((.ptr (char :const)) str) ((.ptr (struct UnmarkedSyntax)) syn))\n";
        cw << "(var _syntaxes (.array (struct _syntaxes) " << syntaxes_size << ") (.\n";
        for (Vector<SyntaxC *>::const_iterator i = cw.for_macro_sep_c->syntaxes.begin(), 
               e = cw.for_macro_sep_c->syntaxes.end(); i != e; ++i)
        {
          cw << "  (. (s \"";
          escape(cw, (*i)->syn->str());
          cw << "\") 0)\n";
        }
        cw << "))\n\n";
      }
    }

    sep(cw, "decls");

    for (VarsItr i = vars.begin(), e = vars.end(); i != e; ++i) {
      (*i)->compile(cw, Declaration::Forward);
      //if (const Fun * d = dynamic_cast<const Fun *>(*i)) {
      //  d->compile(cw, Declaration::Forward);
      //}
    }

    sep(cw, "definitions");

    for (VarsItr i = var_defns.begin(), e = var_defns.end(); i != e; ++i) {
      //printf("COMPILE %s %d\n", ~(*i)->uniq_name(), (*i)->order_num);
      (*i)->compile(cw, Declaration::Body);
    }

    sep(cw, "special");

    for (VarsItr i = var_defns.begin(), e = var_defns.end(); i != e; ++i) {
      if (const TopLevelVar * s = dynamic_cast<const TopLevelVar *>(*i)) {
        if (s->constructor) init.push_back(s->constructor);
        if (s->cleanup) cleanup.push_back(s->cleanup);
      }
    }

    if (!init.empty()) {
      cw << "(fun init$s (.) (void) :static :__constructor__ (block\n";
      for (Vector<AST *>::const_iterator i = init.begin(), e = init.end(); i != e; ++i) {
        cw << adj_indent(2) << *i;
      }
      cw << "))\n";
    }
    
    if (!cleanup.empty()) {
      cw << "(fun cleanup$s (.) (void) :static :__destructor__ (block\n";
      for (Vector<AST *>::const_iterator i = cleanup.begin(), e = cleanup.end(); i != e; ++i) {
        cw << adj_indent(2) << *i;
      }
      cw << "))\n";
    }

    if (cw.target_lang == CompileWriter::ZLE) {
      sep(cw, "others");
      StringBuf buf;
      OStream * stream = cw.stream;
      cw.stream = &buf;
      for (OthersItr i = others.begin(), e = others.end(); i != e; ++i) {
        cw << *i;
      }
      cw.stream = stream;
      cw << "(syntax_data\n";
      cw.printf("  (marks %u)\n", cw.syntax_gather->mark_map.num);
      cw.printf("  (repl_tables %u\n", cw.syntax_gather->repl_table_map.to_print.size());
      for (Vector<String>::const_iterator 
             i = cw.syntax_gather->repl_table_map.to_print.begin(), 
             e = cw.syntax_gather->repl_table_map.to_print.end();
           i != e; ++i) 
        cw.printf("    %s\n", ~*i);
      cw << ")\n";
      cw << buf.freeze();
    }

    sep(cw, "done");
  }

  
  //
  //
  //
}

extern "C" namespace macro_abi {

  using namespace ast;

  typedef const ::Syntax Syntax;
  typedef Syntax UnmarkedSyntax;
  Syntax * replace(const UnmarkedSyntax * p, void * match, Mark * mark);

  int symbol_exists(Syntax * sym, Syntax * where, Mark * mark, Environ * env) {
    printf("symbol_exists %s in %s\n", ~sym->to_string(), ~where->to_string());
    if (mark)
      sym = macro_abi::replace(sym, NULL, mark);
    if (where) {
      const Type * type = NULL;
      try {
        type = parse_type(where, *env);
      } catch (...) {}
      if (!type) {
        try {
          Exp * exp = parse_exp(where, *env);
          type = exp->type;
        } catch (...) {
          return false;
        }
      }
      SymbolNode * syms = NULL;
      if (const StructUnion * t = dynamic_cast<const StructUnion *>(type->unqualified))
        syms = t->env.symbols.front;
      else if (const UserType * t = dynamic_cast<const UserType *>(type->unqualified))
        syms = t->module->syms;
      else
        return false;
      if (find_symbol<Symbol>(sym, DEFAULT_NS, syms, NULL, StripMarks))
        return true;
      else
        return false;
    } else {
      return env->symbols.exists(sym);
    }
  }
}
