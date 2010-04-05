#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

#include <cxxabi.h>

#include <algorithm>

#include "ast.hpp"
#include "parse.hpp"
#include "parse_op.hpp"
#include "parse_decl.hpp"
#include "expand.hpp"
#include "peg.hpp"

#include "syntax_gather.hpp"

#include "parse_common.hpp"

#include "ct_value-impl.hpp"
#include "hash-t.hpp"

//#define NO_ELIDE

// each AST node pushes the result on the top of the stack
//   unless the type is void

namespace ast {

  struct Var;

  struct ExpContext {
    static Var * VOID_CONTEXT_MARKER;
    ExpContext(Var * v = NULL) : var_(v) {}
    bool void_context() {return var_ == VOID_CONTEXT_MARKER;}
    Var * res_loc() {return void_context() ? NULL : var_;}
  private:
    Var * var_; // where to put the result, may be NULL
  };
  
  ExpContext VOID_CONTEXT(ExpContext::VOID_CONTEXT_MARKER);
  
  static Exp * parse_exp(const Syntax * p, Environ & env, ExpContext c);
  static Exp * just_parse_exp(const Syntax * p, Environ & env, ExpContext c);

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
      for (const InnerNS * cur = key.ns; cur; cur = cur->next)
        o << ' ' << cur->tag->name();
      o << ")";
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

  struct NoOp : public Exp {
    NoOp() {assert(VOID_T); type = VOID_T;}
    const char * what() const {return "noop";}
    NoOp * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(0);
      return this;
    }
    void compile_prep(CompileEnviron &) {}
    void finalize(FinalizeEnviron &) {}
    void compile(CompileWriter & f) {
      f << "(noop)";
    }
  };

  static inline Exp * noop() {
    static NoOp NO_OP;
    return &NO_OP;
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

  static const Syntax * ID = new_syntax("id");
  static const Syntax * expand_id(const Syntax * p);

  //
  //
  //

  struct EStmt : public Stmt {
    EStmt() {}
    const char * what() const {return "estmt";}
    EStmt(Exp * e) : exp(e) {assert(e);}
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

  inline Exp * Stmt::as_exp(Environ & env) {
    env.exp_ip->add(this);
    return noop();
  }

  inline void InsrPoint::add(Exp * to_add) {
    if (to_add == noop()) return;
    add(to_add->as_stmt());
  }

  //
  //
  //

  struct BasicVar;
  
  template <Position pos> struct PositionTypeInfo {typedef Stmt t;};
  template <> struct PositionTypeInfo<ExpPos> {typedef Exp t;};
  template <> struct PositionTypeInfo<FieldPos> {typedef BasicVar t;};

  template <Position pos>
  struct Parse {
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

  // needed here otherwise gcc gets confuses
  template <>
  Stmt * Parse<TopLevel>::finish_parse(const Syntax * p) const;
  template <>
  Stmt * Parse<StmtDeclPos>::finish_parse(const Syntax * p) const;

  template <typename C, typename P>
  void add_ast_nodes(parts_iterator i, parts_iterator end, 
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

  void add_stmts(parts_iterator i, parts_iterator end, Environ & env)
  {
    Parse<StmtDeclPos> prs(env);
    for (; i != end; ++i) {
      const Syntax * p = prs.partly_expand(*i);
      if (p->is_a("@")) {
        add_stmts(p->args_begin(), p->args_end(), env);
      } else {
        Stmt * cur = prs.finish_parse(p);
        prs.env.add_stmt(cur);
      }
    }
  }
  
  struct AddAllButLast {
    Parse<StmtDeclPos> prs;
    const Syntax * last;
    AddAllButLast(Environ & env) : prs(env), last() {}
    void add(const Syntax * cur) {
      if (last) {
        Stmt * stmt = prs.finish_parse(last);
        prs.env.add_stmt(stmt);
      } 
      last = cur;
    }
    void add(parts_iterator i, parts_iterator end) {
      for (; i != end; ++i) {
        const Syntax * p = prs.partly_expand(*i);
        if (p->is_a("@"))
          add(p->args_begin(), p->args_end());
        else
          add(p);
      }
    }
    void flush() {
      add(NULL);
    }
  };
  
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

  struct CollectFinishAddEnvSymbol : public CollectAction {
    Symbol * sym;
    CollectFinishAddEnvSymbol(Symbol * s, Environ & e) 
      : CollectAction(e), sym(s)  {}
    void doit_hook() {
      sym->make_unique(env->symbols.front);
    }
  };

  static SymbolNode * handle_overloaded_symbol(Symbol * sym, const SymbolKey & k, Environ & env, bool shadow_ok) {
    if (sym->overloadable()) {
      //IOUT.printf("FOUND ONE %s (%s) %p\n", ~k.to_string(), env.where ? ~env.where->name() : "", sym);
      const Symbol * prev0 = env.symbols.find_this_scope<Symbol>(k);
      const OverloadedSymbol * prev = dynamic_cast<const OverloadedSymbol *>(prev0);
      if (prev0 && !prev) {
        throw unknown_error(0); // FIXME: If shadow_ok than we need to keep looking...
      }
      //if (prev)
      //  IOUT.printf("... AND A PREV %p %p\n", prev, prev0);
      OverloadedSymbol * over = dynamic_cast<VarSymbol *>(sym) 
        ? new OverloadedVar(sym, prev)
        : new OverloadedSymbol(sym, prev);
      return env.add(k, over, shadow_ok);
    } else {
      //IOUT.printf("not one %s (%s) %p\n", ~k.to_string(), env.where ? ~env.where->name() : "", sym);
      return NULL;
    }
  }

  SymbolNode * Symbol::add_to_env(const SymbolKey & k, Environ & env, bool shadow_ok) {
    SymbolNode * overloaded = handle_overloaded_symbol(this, k, env, shadow_ok);
    SymbolNode * n = overloaded 
      ? overloaded 
      : env.symbols.add(env.where, k, this);
    assert(!key);
    key = &n->key;
    if (overloaded) {
      // nothing to do
    } else if (env.special()) {
      if (env.collect) 
          env.collect->first_pass.add(new CollectFinishAddEnvSymbol(this, env));
    } else {
      make_unique(env.symbols.front);
    }
    return n;
  }

  struct CollectFinishAddEnvTopLevel : public CollectAction {
    TopLevelSymbol * sym;
    SymbolNode * local;
    CollectFinishAddEnvTopLevel(TopLevelSymbol * s, SymbolNode * l, Environ & e) 
      : CollectAction(e), sym(s), local(l) {}
    void doit_hook() {
      sym->finish_add_to_env(local, *env);
    }
  };

  SymbolNode * TopLevelSymbol::add_to_env(const SymbolKey & k, Environ & env, bool shadow_ok) {
    //assert(!env.symbols.exists_this_scope(k));
    // FIXME: Have more precise check
    if (!shadow_ok && !overloadable() && env.symbols.exists_this_scope(k)) {
      fprintf(stderr, "TLS SHADOW %s\n", ~k.to_string());
      //abort();
    }
    SymbolNode * overloaded = handle_overloaded_symbol(this, k, env, shadow_ok);
    SymbolNode * local = overloaded 
      ? overloaded 
      : env.symbols.add(env.where, k, this);
    assert(!key);
    key = &local->key;
    if (env.where) {
      where = env.where;
      if (!env.where->named_outer())
        num = NPOS;
    }
    if (k.marks || k.ns != tl_namespace())
      num = NPOS;
    //printf("ADDED %s %s %s %p %p\n", ~name(), ~k.to_string(), ~key->to_string(), local, local->value);
    if (env.special()) {
      if (env.collect)
        env.collect->first_pass.add(new CollectFinishAddEnvTopLevel(this, local, env));
    } else {
      finish_add_to_env(local, env);
    }
    return local;
  }

  void TopLevelSymbol::finish_add_to_env(SymbolNode * local, Environ & env) {
    local->set_flags(SymbolNode::ALIAS);
    if (num == NPOS)
      assign_uniq_num<TopLevelSymbol>(this, *env.top_level_symbols->front);
    //printf(">>%d %s %u %u %s\n", env.special(), ~k.to_string(), num, NPOS, typeid(*this).name());
    String old_uniq_name_ = uniq_name_;
    uniq_name_ = String();
    SymbolKey uniq_key = uniq_name();
    if (old_uniq_name_.defined() && old_uniq_name_ != uniq_name_) 
      fprintf(stderr, "Warning: uniq_name changed from %s to %s\n", ~old_uniq_name_, ~uniq_name_);
    //printf("FINISH_ADD_TO_ENV %p: %s\n", env.where, ~uniq_key.name);
    uniq_key.ns = tl_namespace();
    SymbolNode * tl = find_symbol_node(uniq_key, *env.top_level_symbols->front);
    //printf("1> %s %s %s\n", ~k.to_string(), ~uniq_key.to_string(),  typeid(*this).name());
    if (tl) {
      if (tl == local) {
        //printf("TLS DUP %s\n", ~uniq_name());
        if (dynamic_cast<const OverloadedSymbol *>(tl->value)) {
          // insert it after the overloaded symbol
          local->next = new SymbolNode(env.where, uniq_key, this, 0, local->next);
        }
        goto finish;
      } else if (static_cast<const Symbol *>(this) == tl->value) {
        fprintf(stderr, "tls mismatch %s\n", ~uniq_key);
      } else {
        fprintf(stderr, "TLS MISMATCH %s\n", ~uniq_key);
      }
      abort();
      //goto finish;
      //return;
    }
    tl = env.top_level_symbols->add(env.where, uniq_key, this);
  finish:
    tl->unset_flags(SymbolNode::ALIAS);
    //printf("2> %s %s %s %d\n", ~name, ~uniq_name(), typeid(*this).name(), order_num);
  }
  
  //
  //
  //

  struct Var;

  struct ExpInsrPointWrapperBase {
    ExpInsrPointWrapperBase(Environ & e, ExpInsrPoint::Where w = ExpInsrPoint::ExtendedExp, bool force_new_scope = false) 
      : ip(&stmts, w), stmts(NULL), env(e.new_extended_exp(&ip, force_new_scope)) {}
    ExpInsrPoint ip;
    Stmt * stmts; 
    Environ env;
    void reset() {ip = &stmts; stmts = NULL;}
  };

  struct ExpInsrPointWrapper : public ExpInsrPointWrapperBase {
    ExpInsrPointWrapper(Environ & e, bool force_new_scope = false) 
      : ExpInsrPointWrapperBase(e, ExpInsrPoint::ExtendedExp, force_new_scope) {}
    Stmt * finish();
    Stmt * finish(Exp * exp); // if necessary wrap exp in a block
                              // otherwise return estmt
    Stmt * finish(Stmt * final); 
  };

  struct RefInsrPointWrapper : public ExpInsrPointWrapperBase {
    RefInsrPointWrapper(Environ & e, ExpInsrPoint::Where w) : ExpInsrPointWrapperBase(e, w) {}
    Var * finish(Environ & env); // add temp to env
  };

  struct BranchInsrPointWrapper {
    BranchInsrPointWrapper(Environ & e)     
      : ip(&stmts), stmts(NULL), env(e.new_exp_branch(&ip)) {}
    InsrPoint ip;
    Stmt * stmts;
    Environ env;
    Stmt * finish(Var * v, Exp * e, Environ & env);
    void reset() {ip = &stmts; stmts = NULL;}
  };

  void finish(Var * v, ExpInsrPointWrapper & wrap,
              RefInsrPointWrapper & wrap2, Environ & oenv);

  //
  //
  //

  void check_type(Exp * exp, TypeCategory * cat) {
    if (!exp->type->is(cat))
      throw error(exp->syn, "Expected %s type", ~cat->name);
  }

  //
  //
  //

  static Exp * mk_assign(Exp *, Exp *, Environ & env);
  static Exp * mk_init(Exp *, Exp *, Environ & env);
  static Exp * mk_init(const Var *, Exp *, Environ & env);
  static Exp * mk_id(const VarSymbol *, Environ & env);
  static Exp * mk_literal(int val, Environ & env);
  static Var * start_temp(const Type * type, Environ & env);

  //
  //
  //


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
    bool uniq_name(OStream & o) const {
      o.printf("%s$$%u", ~name(), num);
      return num != NPOS;
    }
    SymbolNode * add_to_env(const SymbolKey & k, Environ &, bool shadow_ok);
    void make_unique(SymbolNode * self, SymbolNode * stop = NULL) const {
      assign_uniq_num<NormalLabel>(this, self->next, stop);
    }
  };

  SymbolNode * NormalLabel::add_to_env(const SymbolKey & k, Environ & env, bool shadow_ok) {
    SymbolNode * n = env.fun_labels.add(env.where, k, this);
    key = &n->key;
    make_unique(*env.fun_labels.front);
    return n;
  }

  struct LocalLabel : public Label {
    mutable unsigned num;
    LocalLabel() : num() {}
    using Label::uniq_name;
    bool uniq_name(OStream & o) const {
      o.printf("%s$%u", ~name(), num);
      return num != NPOS;
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
    StringBuf res;
    parse_common::unescape(orig, orig.end(), res, '\'');
    if (res.size() == 1) 
      ct_value_ = new CT_Value<CT_Type<char>::type>(res[0]);
    else
      ct_value_ = &ct_nval;
    return this;
  }
  void CharC::compile(CompileWriter & f) {
    f << "(c \"" << orig << "\")"; // FIXME: Not right
  }

  Id * Id::construct(Environ & env) {
    next = sym->ids; 
    sym->ids = this;
    const TopLevelVarDecl * tl = sym->top_level();
    if (tl && env.deps)
      env.deps->insert(tl);
    if (sym->ct_value)
      ct_value_ = sym->ct_value;
    type = sym->type;
    lvalue = sym->lvalue;
      return this;
  }

  Id * Id::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(1);
    sym = env.symbols.lookup<VarSymbol>(p->arg(0));
    return construct(env);
  }

  void Id::compile(CompileWriter & f) {
    f << sym;
  }

  static const Syntax * expand_id(const Syntax * p) {
    if (p->have_entity()) {
      if (p->entity<Symbol>()) return p;
      Id * id = dynamic_cast<Id *>(p->entity<Exp>());
      if (id) return SYN(id->sym);
    }
    const Syntax * res = try_id(p);
    if (!res) abort(); //throw error(p, "Expected identifier");
    return res;
  }

  Exp * BasicVar::as_lvalue(Environ & env) const {
    Id * id = new Id(this);
    return id->construct(env);
  }

  //
  // Blocks
  //

  struct Block : public Stmt {
    Block() {}
    const char * what() const {return "block";}
    //AST * part(unsigned i) {return stmts[i];}
    Stmt * stmts;
    Stmt * parse_self(const Syntax * p, Environ & env0) {
      syn = p;
      Environ env = env0.new_scope();
      stmts = NULL;
      InsrPoint ip = &stmts;
      if (!env.exp_ip)
        env.stmt_ip = &ip;
      else
        assert(env.exp_ip == env.stmt_ip);
      add_stmts(p->args_begin(), p->args_end(), env);
      if (stmts == NULL) // must test against stmts as last will may
                         // be an empty_stmt
        stmts = empty_stmt();
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
      f << indent << "(block\n";
      for (Stmt * cur = stmts; cur; cur = cur->next)
        f << adj_indent(2) << cur;
      f << indent << ")\n";
    }
  };
  
  //
  //
  // 

  struct CollectParseDef : public CollectAction {
    Declaration * decl;
    CollectParseDef(Declaration * d, Environ & e) 
      : CollectAction(e), decl(d) {}
    void doit_hook() {
      //IOUT.printf(">>%p %p\n", this, decl);
      decl->finish_parse(*env);
    }
  };

  //
  // Var Declararions
  //

  struct OtherVar : public BasicVar {
    OtherVar() : num() {}
    OtherVar(const Type * t, bool mangle) : BasicVar(t), num(mangle ? NPOS : 0) {}
    mutable unsigned num; // 0 to avoid renaming, NPOS needs uniq num
    bool uniq_name(OStream & o) const {
      if (num == 0)
        o << name();
      else
        o.printf("%s$%u", ~name(), num);
      return num != NPOS;
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

  //struct InitVar : public Stmt {
  //  const Id * var;
  //  const Stmt * code;
  //  void compile_prep(CompileEnviron & env) {code->compile_prep(env);}
  //  void finalize(FinalizeEnviron & env) {code->finalize(env);}
  //  void compile(CompileWriter & cw);
  //};

  struct Trivial : public Symbol {
    Trivial(const Type * t) : parms(dynamic_cast<const Tuple *>(t)) {}
    const Tuple * parms;
    const Tuple * overloadable() const {return parms;}
  };

  struct NoParmsCmp {
    NoParmsCmp() {}
    bool operator() (const Symbol * sym) {
      const Tuple * parms = sym->overloadable();
      // if we have the symbol but its not overloadable assume it
      // takes no parameters
      if (!parms) return true;
      if (parms->num_parms() == 0) return true;
      return false;
    }
  };

  template <typename Cmp>
  bool have_special(const UserType * ut, const char * what, Cmp & cmp) {
    const Symbol * res = find_symbol<Symbol>(what, 
                                             ut->module->syms.front, ut->module->syms.back,
                                             ThisScope);
    if (!res) return false;
    if (const OverloadedSymbol * cur = dynamic_cast<const OverloadedSymbol *>(res)) {
      while (cur && !cmp(cur->sym))
        cur = cur->next;
      if (!cur) return false;
      res = cur->sym;
    } else {
      if (!cmp(res))
        return false;
    }
    if (dynamic_cast<const Trivial *>(res)) return false;
    return true;
  }

  bool have_default_constructor(const UserType * ut) {
    NoParmsCmp cmp;
    return have_special(ut, "_constructor", cmp);
  }

  struct RefCmp {
    const Type * t;
    RefCmp(const Type * t0) : t(t0) {}
    bool operator() (const Symbol * sym) {
      const Tuple * parms = sym->overloadable();
      if (!parms) return false;
      if (parms->num_parms() != 1) return false;
      TypeParm parm = parms->parm(0);
      if (!parm.is_type()) return false;
      const Reference * ref = dynamic_cast<const Reference *>(parm.as_type->root);
      if (!ref) return false;
      if (ref->subtype->unqualified != t) return false;
      return true;
    }
  };

  bool have_copy_constructor(const UserType * ut) {
    RefCmp cmp(ut);
    return have_special(ut, "_constructor", cmp);
  }

  bool have_assign(const UserType * ut) {
    RefCmp cmp(ut);
    return have_special(ut, "_assign", cmp);
  }

  struct AlwaysTrueCmp {
    bool operator() (const Symbol *) {return true;}
  };

  bool have_destructor(const UserType * ut) {
    AlwaysTrueCmp cmp;
    return have_special(ut, "_destructor", cmp);
  }

  Stmt * mk_constructor(Exp * exp, Environ & env) {
    ExpInsrPointWrapper wrap(env);
    Exp * call = parse_exp(SYN(SYN("member"), 
                               SYN(exp),
                               SYN(SYN("call"), SYN(ID, SYN("_constructor")), SYN(SYN(".")))),
                           wrap.env);

    return wrap.finish(call);
  }

  Stmt * mk_copy_constructor(Exp * lhs, Exp * rhs, Environ & env) {
    ExpInsrPointWrapper wrap(env);
    Exp * call =  parse_exp(SYN(SYN("member"), 
                                SYN(lhs),
                                SYN(SYN("call"), SYN(ID, SYN("_constructor")), 
                                    SYN(SYN("."), SYN(rhs)))),
                            wrap.env);
    return wrap.finish(call);
  }

  Exp * mk_destructor(Exp * exp, Environ & env) {
    return parse_exp(SYN(SYN("member"), 
                         SYN(exp),
                         SYN(SYN("call"), SYN(ID, SYN("_destructor")), SYN(SYN(".")))),
                     env);
  }

  const Symbol * resolve_call(const Syntax * name, const Vector<Exp *> & parms, Environ & env);

  struct Var : virtual public VarDecl {
    Var() : init(), constructor(), cleanup() {}
    const char * what() const {return "var";}
    //AST * part(unsigned i) {return new Terminal(parse_->arg(0));}
    Exp * init;
    Stmt * constructor;
    Cleanup * cleanup;
    virtual SourceStr id_str() const {return syn ? syn->arg(0)->str() : SourceStr();}
    virtual void add_to_sub_env(Environ & env) {assert (!env.temp_ip);}
    void parse_init(parts_iterator i, parts_iterator e, Environ & env) {
      ExpInsrPointWrapper wrap(env);
      const Syntax * init_syn = *i++;
      if (init_syn->eq(".")) {
        if (i == e) throw unknown_error(init_syn);
        init_syn = *i++;
        if (init_syn->is_a("()")) init_syn = reparse("SPLIT", init_syn->inner());
        const UserType * user_type = dynamic_cast<const UserType *>(type->unqualified->root);
        printf("CON: %s\n", ~init_syn->to_string());
        if (user_type) {
          // find constructor to use
          Vector<Exp *> parms;
          add_ast_nodes(init_syn->args_begin(), init_syn->args_end(), parms, Parse<ExpPos>(wrap.env));
          if (parms.size() == 1 && parms[0]->type->unqualified == type->unqualified) {
            // We will be calling a copy constructor, but go via the
            // path of an init value to give us an opportunity to
            // elide the copy constructor if possible
            init_syn = SYN(parms[0]);
            goto with_init;
          } 
          const Symbol * sym = resolve_call(SYN(id_str(),
                                                SYN("::"), 
                                                SYN<Symbol>(user_type->module), 
                                                SYN("_constructor", id_str())), 
                                            parms, wrap.env); 
          if (const Trivial * trivial = dynamic_cast<const Trivial *>(sym)) {
            assert(parms.size() <= 1);
            if (parms.size() == 0) {
              throw unknown_error(init_syn);
            } else if (parms.size() == 1) {
              init_syn = SYN(parms[0]);
            }
          } else {
            SyntaxBuilder synb;
            synb.add_part(SYN("."));
            for (Vector<ast::Exp *>::const_iterator i = parms.begin(), e = parms.end();
                 i != e; ++i)
              synb.add_part(SYN(*i));
            synb.set_flags(init_syn->flags_begin(), init_syn->flags_end());
            Syntax * parms_s = synb.build();
            printf("NOW PARSING CONSTRUCTOR %s\n", ~parms_s->to_string());
            Exp * call = parse_exp(SYN(SYN("member"), 
                                       SYN(mk_id(this, env)),
                                       SYN(SYN("call"), SYN(sym), parms_s)),
                                   env);
            constructor = wrap.finish(call);
            return;
          }
        } else {
          if (init_syn->num_args() == 0) {
            init_syn = SYN("0");
          } else if (init_syn->num_args() == 1) {
            init_syn = init_syn->arg(0);
          } else {
            throw unknown_error(init_syn);
          }
        }
      } else if (init_syn->eq("=")) {
        if (i == e) throw unknown_error(init_syn);
        init_syn = *i++;
      }
    with_init:
      //env.add(name, sym, SecondPass);
      init = parse_exp(init_syn, wrap.env, ExpContext(this));
      if (init == noop()) init = NULL;
      RefInsrPointWrapper wrap2(env, top_level() ? ExpInsrPoint::TopLevelVar : ExpInsrPoint::Var);
      if (init) init = init->resolve_to(type, wrap2.env);
      finish(this, wrap, wrap2, env);
    }
    void construct(parts_iterator i, parts_iterator e, Environ & env) {
      if (i < e) parse_init(i,e,env);
      const UserType * ut;
      if (!cleanup && (ut = dynamic_cast<const UserType *>(type))) {
        if (!init && !constructor) constructor = try_constructor(ut, env);
        add_cleanup(ut, env);
      }
    }
    Stmt * finish_parse(Environ & env) {
      //fprintf(stderr, "SIZE OF VAR = %u\n", sizeof(Var));
      //printf("FINISH PARSE %s\n", ~syn->to_string());
      construct(syn->args_begin() + 2, syn->args_end(), env);
      if (storage_class == SC_STATIC && type->read_only && init && init->ct_value_) {
        ct_value = init->ct_value_;
      }
      return this;
    }
    Stmt * try_constructor(const UserType * ut, Environ & env) {
      if (ut && have_default_constructor(ut)) {
        return mk_constructor(mk_id(this, env), env);
      } else {
        return NULL;
      }
    }
    Stmt * try_copy_constructor(Exp * rhs, Environ & env) {
      const UserType * ut = dynamic_cast<const UserType *>(type->unqualified);
      if (ut && have_copy_constructor(ut)) {
        return mk_copy_constructor(mk_id(this, env), rhs, env);
      } else {
        return NULL;
      }
    }

    void add_cleanup(const UserType * ut, Environ & env) {
      if (ut && have_destructor(ut)) {
        cleanup = new Cleanup;
        ExpInsrPointWrapper wrap(env, true);
        Exp * call = mk_destructor(mk_id(this, wrap.env), wrap.env);
        cleanup->code = wrap.finish(call);
      }
    }

    virtual void fix_up_init(Environ & env) {
      if (!init) return;
      Stmt * copy_c = try_copy_constructor(init, env);
      if (copy_c) {
        constructor = copy_c;
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
    if (var)
      cw << "(assign " << var->uniq_name() << "$nc 1)";
    else
      cw << "(noop)";
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

  //void InitVar::compile(CompileWriter & cw) {
  //  cw << code;
  //  if (var->sym->clean && var->sym->cleanup_flag)
  //}

  struct AutoVar : public Var {
    AutoVar() : num(), shadow(false) {}
    AutoVar(const Type * t) : num(), shadow(false) {type = t;}
    mutable unsigned num;
    bool shadow;
    void compile(CompileWriter & f, Phase phase) const {
      assert(phase == Normal);
      Var::compile(f, phase);
      if (constructor)
        f << constructor;
      if (cleanup)
        f << cleanup;
    }
    bool uniq_name(OStream & o) const {
      o.printf("%s$%u", ~name(), num);
      return true;
    }
    void make_unique(SymbolNode * self, SymbolNode * stop) const {
      assign_uniq_num<AutoVar>(this, self->next, stop);
    }
  };

  static AutoVar * new_auto_var(SymbolName n, const Type * t) {
    return new AutoVar(t);
  }


  static unsigned last_temp_num = 0;

  struct TempBase : public AutoVar {
    bool uniq_name(OStream & o) const {
      o.printf("%s$t%u", ~name(), num);
      return true;
    }
    void make_unique(SymbolNode *, SymbolNode *) const {
      num = last_temp_num++;
    }
  };

  // Not really considered a temp, since it has the same scope of a
  // normal variable, thus is_temp() is still false.  However, since
  // it doesn't get added to the normal env. it still needs special
  // naming thus it inherits from TempBase rather than AutoTemp
  struct AutoTemp : public TempBase {
    AutoTemp(const Type * t) {type = t; num = NPOS;}
  };

  struct EExpTemp : public TempBase {
    EExpTemp() {lvalue = LV_EEXP;}
    bool is_temp() const {return true;}
  };

  struct AnonTemp : public EExpTemp {
    AnonTemp(const Type * t) {type = t;}
  };

  struct NamedTemp : public EExpTemp {
    void add_to_sub_env(Environ & env) {
      env.temp_ip->add(this);
    }
    void fix_up_init(Environ & env) {
      Var::fix_up_init(env);
      if (init && !init->ct_value_) {
        constructor = mk_init(this, init, env)->as_stmt();
        init = NULL;
      } 
    }
    Stmt * finish_parse(Environ & env);
  };

  Stmt * NamedTemp::finish_parse(Environ & env) {
    Var::finish_parse(env);
    if (cleanup && !cleanup->cleanup_flag) {
      cleanup->cleanup_flag = new CleanupFlag(this);
      if (constructor) {
        Block * b = dynamic_cast<Block *>(constructor);
        if (!b) {
          b = new Block;
          b->stmts = constructor;
        } 
        Stmt * * ip;
        ip = &b->stmts->next;
        while (*ip != NULL)
          ip = &b->stmts->next;
        *ip = cleanup->cleanup_flag->as_stmt();
        constructor = b;
      } else {
        constructor = cleanup->cleanup_flag->as_stmt();
      }
    }
    if (constructor) {
      Stmt * ret = constructor;
      constructor = NULL;
      return ret;
    } else {
      return empty_stmt();
    }
  }

  struct TopLevelVar : public Var, public TopLevelVarDecl {
    TopLevelVar() {lvalue = LV_TOPLEVEL;}
    void fix_up_init(Environ & env) {
      Var::fix_up_init(env);
      if (init && !init->ct_value_) {
        constructor = mk_init(this, init, env)->as_stmt();
        init = NULL;
      } 
    }
    Stmt * finish_parse(Environ & env0) {
      // Need to create a new fake scope to capture any dependencies
      // when initializing the var
      Environ env = env0.new_scope();
      env.where = this;
      env.deps = &deps_;
      env.for_ct = &for_ct_;
      Var::finish_parse(env);
      env.move_defn(this);
      return empty_stmt();
    }
  };

  // Like AutoTemp, not really a temp
  struct TopLevelTemp : public TopLevelVar {
    TopLevelTemp(const Type * t) {type = t; num = NPOS;}
  };

  struct VarLike : public Var {
    Exp * lvalue_exp;
    VarLike(Exp * lv, const Type * t) : lvalue_exp(lv) {type = t;}
    Exp * as_lvalue(Environ & env) const {return lvalue_exp;}
    void compile_lvalue(CompileWriter & o) const {o << lvalue_exp;}
    void add_to_sub_env(Environ & env) {
      if (env.temp_ip) env.temp_ip->add(this);
    }
    void compile(CompileWriter & f, Phase phase) const {
      assert(!cleanup && !init && !constructor);
    }
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
  
  static Stmt * parse_var(const Syntax * p, Environ & env) {
    assert_num_args(p,2,4);
    Collect * collect = env.collect;
    const Syntax * name_p = p->arg(0);
    SymbolKey name = expand_binding(name_p, env);
    StorageClass storage_class = get_storage_class(p);
    bool shadow = p->flag("__shadow");
    //bool shadow = true;
    if (shadow && collect) throw unknown_error(p);
    Var * var;
    Stmt * res;
    bool fresh = true;
    if (env.scope >= LEXICAL && (storage_class == SC_NONE ||
                                 storage_class == SC_AUTO ||
                                 storage_class == SC_REGISTER)) 
    {
      AutoVar * v = env.scope == LEXICAL ? new AutoVar : new NamedTemp;
      var = v;
      res = v;
    } else {
      make_static_if_marked(storage_class, name);
      fresh = !env.symbols.exists_this_scope(name);
      TopLevelVar * v = fresh ? new TopLevelVar : env.symbols.find<TopLevelVar>(name);
      //v->num = env.scope >= LEXICAL || storage_class == SC_STATIC ? NPOS : 0;
      //v->num = storage_class == SC_STATIC ? NPOS : 0;
      v->deps_closed = false;
      var = v;
      res = empty_stmt();
    }
    if (env.interface && env.scope == TOPLEVEL) {
      if (storage_class == SC_STATIC)
        return empty_stmt();
      storage_class = SC_EXTERN;
    }
    var->syn = p;
    var->name_p = name_p;
    var->type = parse_type(p->arg(1), env);
    var->storage_class = storage_class;
    if (storage_class != SC_EXTERN && var->type->size() == NPOS)
      throw error(name_p, "Size not known");
    var->add_to_sub_env(env);
    if (fresh && !shadow)
      env.add(name, var);
    if (env.parse_def() && !collect)
      res = var->finish_parse(env);
    else if (collect) 
      collect->second_pass.add(new CollectParseDef(var, env));
    if (shadow)
      env.add(name, var);
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

  Var EMPTY_VAR;
  Var * ExpContext::VOID_CONTEXT_MARKER = &EMPTY_VAR;

  //
  // EBlock and Sequences
  //

  static Exp * parse_eblock_helper(const Syntax * p, Environ & oenv, ExpContext c) {
    Environ env = oenv.new_scope();
    env.scope = EXTENDED_EXP;
    env.stmt_ip = env.exp_ip;
    AddAllButLast wk(env);
    wk.add(p->args_begin(), p->args_end());
    if (!wk.last) return noop();
    if (c.void_context()) {
      wk.flush();
      return noop();
    } else {
      return just_parse_exp(wk.last, env, c);
    }
  };

  static Exp * parse_seq_helper(const Syntax * p, Environ & env, ExpContext c) {
    parts_iterator i = p->args_begin(), e = p->args_end();
    assert(i != e);
    while (true) {
      const Syntax * p0 = *i++;
      if (i == e) {
        return parse_exp(p0, env, c);
      } else {
        env.exp_ip->add(parse_exp(p0, env, VOID_CONTEXT));
      }
    }
  }

  static Exp * seq_wrap(Exp * res, Environ & env, ExpContext c) {
    return res;
  /*
    if (!res) return NULL; // final value already put in c.ret_loc
    if (c.void_context()) {
      env.exp_ip->add(res);
      return noop();
    } else {
      Var * v = start_temp(res->type, env);
      env.exp_ip->add(mk_init(v, res, env));
      if (v->cleanup)
        env.exp_ip->add(v->cleanup->cleanup_flag);
      return mk_id(v, env);
    }
  */
  }

  Exp * parse_eblock(const Syntax * p, Environ & env, ExpContext c) {
    Exp * res = parse_eblock_helper(p, env, c);
    return seq_wrap(res, env, c);
  }

  Exp * parse_seq(const Syntax * p, Environ & env, ExpContext c) {
    Exp * res = parse_seq_helper(p, env, c);
    return seq_wrap(res, env, c);
  }

  //
  // Conditionals
  //

  struct If : public Stmt {
    If() {}
    If(const Syntax * p, Exp * e, Stmt * et, Stmt * ef)
      : exp(e), if_true(et), if_false(ef) {syn = p;}
    const char * what() const {return "if";}
    //AST * part(unsigned i) 
    //  {return i == 0 ? exp : i == 1 ? if_true : i == 2 ? if_false : 0;}
    Exp * exp;
    Stmt * if_true;
    Stmt * if_false;
    If * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(2,3);
      //ExpInsrPointWrapper wrap(env);
      exp = parse_exp(p->arg(0), env);
      //exp = wrap.finish(exp);
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

  EIf * EIf::construct(Environ & env) {
    type = if_true->type;
    ct_value_ = eif_ct_value(this);
    return this;
  }

  Exp * parse_eif(const Syntax * p, Environ & env) {
    assert_num_args(p, 3);
    Exp * exp = parse_exp(p->arg(0), env);
    exp = exp->resolve_to(env.bool_type(), env);
    BranchInsrPointWrapper wrap1(env);
    Exp * if_true = parse_exp(p->arg(1), wrap1.env);
    BranchInsrPointWrapper wrap2(env);
    Exp * if_false = parse_exp(p->arg(2), wrap2.env);
    if_false = if_false->resolve_to(if_true->type, wrap2.env);
    if (wrap1.stmts || wrap2.stmts) {
      Var * v = start_temp(if_true->type, env);
      Stmt * s1 = wrap1.finish(v, if_true, env);
      Stmt * s2 = wrap2.finish(v, if_true, env);
      Stmt * s = new If(p, exp, s1, s2);
      env.exp_ip->add(s);
      return mk_id(v, env);
    } else {
      return (new EIf(p, exp, if_true, if_false))->construct(env);
    }
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
    Compliment() : SimpleUnOp("bnot", "~", INT_C) {}
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
    AddrOf(Exp * e) : UnOp("addrof", "&") {exp = e;}
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
      exp = exp->to_effective(env);
      check_type(exp, POINTER_C);
      const PointerLike * t = dynamic_cast<const PointerLike *>(exp->type->unqualified);
      if (!t) throw error(exp->syn, "Internal Error: Expected PointerLink");
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

  struct OverloadExtraCmp {
    const Type * t;
    OverloadExtraCmp(const Type * t0) : t(t0) {}
    bool operator() (const SymbolNode * n) {
      const Symbol * sym = n->value;
      const Fun * fun = dynamic_cast<const Fun *>(sym);
      if (!fun) return false;
      return fun->parms->parms[0].type->effective->unqualified == t;
    }
  };

  Exp * BinOp::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(2);
    lhs = parse_exp(p->arg(0), env);
    rhs = parse_exp(p->arg(1), env);
    if (lhs->type->effective->unqualified == rhs->type->effective->unqualified
        && lhs->type->effective->is(USER_C)) 
    {
      NoOpGather gather;
      OverloadExtraCmp cmp(lhs->type->effective->unqualified);
      // FIXME: Need to preserve marks somehow
      Fun * fun = find_symbol<Fun>(SymbolKey(what_,OPERATOR_NS), 
                                   env.symbols.front, NULL, NormalStrategy, gather, cmp);
      if (fun)
        return parse_exp(SYN(SYN("call"), 
                             SYN(mk_id(fun, env)),
                             SYN(SYN("."), SYN(lhs), SYN(rhs))), env);
    }
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
      const StructUnion * t = dynamic_cast<const StructUnion *>(exp->type->unqualified);
      if (!t) throw error(p->arg(0), "Expected struct or union type"); 
      if (!t->defined) throw error(p->arg(1), "Invalid use of incomplete type");
      const Syntax * id = expand_id(p->arg(1));
      sym = t->env.symbols.find<VarSymbol>(id, DEFAULT_NS, StripMarks);
      if (!sym)
        throw error(p->arg(1), "\"%s\" is not a member of \"%s\"", 
                    ~id->to_string(), ~t->to_string());
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
    if (ut && have_assign(ut)) {
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

  Stmt * try_copy_constructor(Exp * lhs, Exp * rhs, Environ & env) {
    const UserType * ut = dynamic_cast<const UserType *>(lhs->type->unqualified);
    if (ut && have_copy_constructor(ut)) {
      return mk_copy_constructor(lhs, rhs, env);
    } else {
      return NULL;
    }
  }

  static Exp * mk_init(Exp * l, Exp * r, Environ & env) {
    r = r->resolve_to(l->type, env);
    Stmt * copy_c = try_copy_constructor(l, r, env);
    if (copy_c) {
      return copy_c->as_exp(env);
    } else if (const Array * a = dynamic_cast<const Array *>(l->type)) {
      return parse_exp(SYN(SYN("call"), 
                           SYN("memcpy"), 
                           SYN(SYN("."), 
                               SYN(l), 
                               SYN(r), 
                               SYN(SYN("sizeof"), SYN(l)))),
                       env);
    } else {
      InitAssign * exp = new InitAssign(l, r);
      return exp->construct(env);
    }
  }

  static Exp * mk_init(const Var * v, Exp * r, Environ & env) {
    return mk_init(mk_id(v, env), r, env);
  }

  static Exp * parse_init_assign(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    Exp * lhs = parse_exp(p->arg(0), env);
    Exp * rhs = parse_exp(p->arg(1), env);
    Exp * assign = mk_init(lhs, rhs, env);
    assign->syn = p;
    return assign;
  }

  Stmt * try_constructor(Exp * exp, Environ & env) {
    const UserType * ut = dynamic_cast<const UserType *>(exp->type->unqualified);
    if (ut && have_default_constructor(ut)) {
      return mk_constructor(exp, env);
    } else {
      return NULL;
   }
  }

  Exp * try_destructor(Exp * exp, Environ & env) {
    const UserType * ut = dynamic_cast<const UserType *>(exp->type->unqualified);
    if (ut && have_destructor(ut)) {
      return mk_destructor(exp, env);
    } else {
      return NULL;
    }
  }

  static Exp * parse_construct(const Syntax * p, Environ & env) {
    assert_num_args(p, 2, 4);
    Exp * exp = parse_exp(p->arg(0), env);
    Type * type = parse_type(p->arg(1), env);
    VarLike * var = new VarLike(exp, type);
    var->add_to_sub_env(env);
    var->construct(p->args_begin() + 2, p->args_end(), env);
    var->cleanup = NULL;
    if (var->init) {
      Exp * res = mk_init(exp, var->init, env);
      var->init = NULL;
      return res;
    } else if (var->constructor) {
      Exp * res = var->constructor->as_exp(env);
      var->constructor = NULL;
      return res;
    } else {
      return noop();
    }
  }

  static Exp * parse_destroy(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    Exp * exp = parse_exp(p->arg(0), env);
    Exp * d = try_destructor(exp, env);
    if (d) return d;
    else return (new AddrOf(exp))->construct(env);
  }

  struct CompoundAssign : public BinOp {
    CompoundAssign(String name, String op0, Exp * lhs0, BinOp *bop, Environ & env) 
      : BinOp(name, op0), assign(new Assign), binop(bop) 
    {
      syn = binop->syn;
      rhs = binop->rhs;
      assign->syn = syn;
      assign->lhs = lhs0;
      assign->rhs = binop;
      assign->resolve(env);
      lhs = assign->lhs;
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
    Mod() : SimpleBinOp("mod", "%", INT_C) {}
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
    const char * what() const {return ".";}
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

  Stmt * ExpInsrPointWrapper::finish() {
    if (!stmts) return NULL;
    Block * b = new Block();
    b->stmts = stmts;
    reset();
    return b;
  }

  Stmt * ExpInsrPointWrapper::finish(Exp * exp) {
    if (!stmts) return exp->as_stmt();
    ip.add(new EStmt(exp)); 
    Block * b = new Block();
    b->stmts = stmts;
    reset();
    return b;
  }

  Stmt * ExpInsrPointWrapper::finish(Stmt * final) {
    if (!stmts) return final;
    ip.add(final); 
    Block * b = new Block();
    b->stmts = stmts;
    reset();
    return b;
  }

  Var * RefInsrPointWrapper::finish(Environ & oenv) {
    if (!stmts) return NULL;
    assert(env.temp_ip == &ip);
    Stmt * cur = stmts;
    assert(!cur->next); // there should only be one
    if (ip.where == ExpInsrPoint::TopLevelVar) {
      oenv.add_defn(cur);
    } else {
      oenv.add_stmt(cur);
    }
    reset();
    return dynamic_cast<Var *>(cur);
  }

  Stmt * BranchInsrPointWrapper::finish(Var * v, Exp * e, Environ & env) {
    ip.add(mk_init(v, e, env)->as_stmt());
    if (v->cleanup)
      ip.add(v->cleanup->cleanup_flag);
    Block * b = new Block();
    b->stmts = stmts;
    reset();
    return b;
  }

  static bool try_to_elide(Var * v, Stmt * * stmts, Environ & oenv) {
#ifndef NO_ELIDE
    Id * id;
    if (v->init &&
        (id = dynamic_cast<Id *>(v->init)) &&
        id->sym->is_temp() &&
        id->type->unqualified == v->type->unqualified)
    {
      // get the temp as both a Var and a Stmt
      const Var * tmp = dynamic_cast<const Var *>(id->sym);
      const Stmt * tmp_s = tmp; 
      // we need to find the temp in the stmt list, if v is also a
      // temp start with it, otherwise use the passed in stmt list
      Stmt * * tmp_ip = stmts;
      //if (v->is_temp()) tmp_ip = &v->next;
      if (oenv.temp_ip && !*tmp_ip) tmp_ip = &v->next;
      for (; *tmp_ip && *tmp_ip != tmp_s; tmp_ip = &(*tmp_ip)->next) {}
      // if we failed to find the temp this means that "v" is a temp
      // which was defined after the temp to eliminate, thus bail 
      if (!*tmp_ip) return false;
      // now remove the temp
      *tmp_ip = tmp->next;
      // we should not have any cleanup code yet on "v" if we 
      assert(!v->is_temp() || !v->cleanup);
      // if the temp had any attached init., detach it and do the
      // init. in the place where the temp was defined
      if (tmp->init) {
        Stmt * init = mk_init(tmp, tmp->init, oenv)->as_stmt();
        assert(!(tmp->cleanup && tmp->cleanup->cleanup_flag));
        init->next = *tmp_ip;
        *tmp_ip = init;
      } else if (tmp->constructor) {
        assert(!(tmp->cleanup && tmp->cleanup->cleanup_flag));
        assert(tmp->constructor->next == NULL);
        tmp->constructor->next = *tmp_ip;
        *tmp_ip = tmp->constructor;
      }
      // change all ids to point v
      for (Id * cur = id->sym->ids; cur;) {
        cur->sym = v;
        Id * next = cur->next;
        cur->next = v->ids;
        v->ids = cur;
        cur = next;
      }
      // fix up cleanup_flag to point to v
      if (tmp->cleanup && tmp->cleanup->cleanup_flag)
        tmp->cleanup->cleanup_flag->var = v;
      // ...
      v->init = NULL;
      // make the cleanup for "v" the same as the temp, not always
      // necessary, but never hurts
      v->cleanup = tmp->cleanup;
      return true;
    } 
#endif
    return false;
  }

  void finish(Var * v, ExpInsrPointWrapper & wrap,
              RefInsrPointWrapper & wrap2, Environ & oenv) 
  {
    Var * t = wrap2.finish(oenv);
    if (t) v = t;
    bool elided = try_to_elide(v, &wrap.stmts, oenv);
    if (wrap.stmts) {
      if (v->init) { // if we elided a copy than this may have
                     // already been done
        wrap.ip.add(mk_init(v, v->init, oenv));
        if (v->cleanup && v->cleanup->cleanup_flag)
          wrap.ip.add(v->cleanup->cleanup_flag);
        v->init = NULL;
      }
      v->constructor = wrap.finish();
    } else if (elided) { 
      // Prevent the default constructor from being called by marking
      // the constructor as being there even though it is a noop.
      // This can happen if "v" is already a temp and the init. code
      // was added to the existing extended environment, hence
      // wrap.stmts will be empty.
      v->constructor = empty_stmt();
    } else {
      v->fix_up_init(oenv);
    }
  }

  static Var * start_temp(const Type * type, Environ & env) {
    Var * temp = NULL;
    if (env.temp_ip->where == ExpInsrPoint::TopLevelVar)
      temp = new TopLevelTemp(type);
    else if (env.temp_ip->where == ExpInsrPoint::Var)
      temp = new AutoTemp(type);
    else
      temp = new AnonTemp(type);
    env.add(SymbolKey("tmp"), temp, true);
    env.temp_ip->add(temp);
    temp->add_cleanup(dynamic_cast<const UserType *>(type->unqualified), env);
    if (temp->cleanup)
      temp->cleanup->cleanup_flag = new CleanupFlag(temp);
    return temp;
  }

  Exp * make_temp(Exp * exp, Environ & env) {
    Var * temp = start_temp(exp->type, env);
    if (env.temp_ip->where == ExpInsrPoint::ExtendedExp) {
      env.exp_ip->add(mk_init(temp, exp, env));
      if (temp->cleanup)
        env.exp_ip->add(temp->cleanup->cleanup_flag);
      return mk_id(temp, env);
    } else {
      temp->init = exp;
      return mk_id(temp, env);
    }
  }

  Exp * make_temp(const Type * type, Environ & env, ExpContext c) {
    bool use_res_loc = false;
    Var * temp = NULL;
#ifndef NO_ELIDE
    temp = c.res_loc();
    if (temp && temp->type->unqualified != type->unqualified) temp = NULL;
    if (temp) use_res_loc = true;
#endif
    if (!temp) temp = start_temp(type, env);
    Stmt * constructor = temp->try_constructor(dynamic_cast<const UserType *>(type), env);
    if (env.temp_ip->where == ExpInsrPoint::ExtendedExp) {
      assert(!temp->cleanup || !use_res_loc);
      if (constructor)
        env.exp_ip->add(constructor);
      else if (type->is(SCALAR_C))
        env.exp_ip->add(mk_init(temp, mk_literal(0, env), env));
      if (temp->cleanup)
        env.exp_ip->add(temp->cleanup->cleanup_flag);
    } else {
      temp->constructor = constructor;
    }
    if (use_res_loc)
      return noop();
    else
      return mk_id(temp, env);
  }

  Exp * parse_anon(const Syntax * p, Environ & env, ExpContext c) {
    Type * t = env.types.inst(p->arg(0));
    return make_temp(t, env, c);
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

  static void parse_stmts(parts_iterator i, parts_iterator end, Environ & env);

  inline void parse_stmts(const Syntax * p, Environ & env) {
    parse_stmts(p->args_begin(), p->args_end(), env);
  }

  static void parse_stmts(parts_iterator i, parts_iterator end, Environ & env) {
    Parse<TopLevel> prs(env);
    for (; i != end; ++i) {
      const Syntax * p = prs.partly_expand(*i);
      if (p->is_a("@")) {
        parse_stmts(p, env);
      } else {
        prs.finish_parse(p);
      }
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
      for (SymbolNode * cur = syms.front; cur != syms.back; cur = cur->next) {
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

  struct DummyExp : public ExpLeaf {
    DummyExp(const Type * t) {type = t;}
    const char * what() const {return "<dummy>";}
    void compile(CompileWriter&) {abort();}
  };

  void handle_special_methods(UserType * user_type, Environ & env) {
    //printf("HANDLE SPECIAL METHODS\n");

    const Type * parms_empty = 
      env.types.inst(".");
    const Type * ref_type = 
      env.types.inst(".ref", env.types.inst(".qualified", TypeParm(QualifiedType::CONST), TypeParm(user_type)));
    const Type * parms_self = 
      env.types.inst(".", TypeParm(ref_type));

    if (!have_default_constructor(user_type))
      env.add("_constructor", new Trivial(parms_empty));

    if (!have_copy_constructor(user_type))
      env.add("_constructor", new Trivial(parms_self));

    if (!have_assign(user_type))
      env.add("_assign", new Trivial(parms_self));

    if (!have_destructor(user_type))
      env.add("_destructor", new Trivial(parms_empty));
  }

  // "new" is for lack of a better name as it doesn't necessary create
  // a new instance
  Module * new_module(const Syntax * p, Environ & env) {
    SymbolKey n = SymbolKey(*p, OUTER_NS);
    Module * m = env.symbols.find_this_scope<Module>(n);
    if (!m) {
      m = new Module();
      env.add(n, m);
    }
    return m;
  }

  struct ModuleBuilderBase { // shared with zl abi
    Module * module;
    UserType * user_type;
    Environ * lenv;
    ModuleBuilderBase(const Syntax * p, Environ & e, UserType * ut = NULL)
      : module(ut ? ut->module : new_module(p, e)),
        user_type(ut),
        lenv(new Environ(e.new_open_scope())) {}
  };
  
  struct ModuleBuilder : public ModuleBuilderBase, public DeclHandle {
    // this should be head allocated unless the module will be
    // finalized in the same scope
    ModuleBuilder(const Syntax * p, Environ & e, UserType * ut = NULL) 
      : ModuleBuilderBase(p, e, ut),
        outer_env(&e),
        name(p),
        prs(*lenv),
        do_finalize(true)
      {
        assert(module);
        assert(lenv);
        assert(lenv == &prs.env);
        lenv->scope = TOPLEVEL;
        lenv->where = module;
        lenv->collect = &collect;
        module->syms = lenv->symbols;
        //module->collect = &collect;
      }
    Environ * outer_env;
    Syntax * name;
    Collect collect;
    Parse<TopLevel> prs;
    bool do_finalize;
    const SourceStr & source_str() const {return name->str();} // FIXME: This isn't quite right
    void add_syntax(const Syntax * p) {
      //printf("QQ: ADD SYNTAX %p (%s): %s\n", this, ~name->to_string(), ~p->to_string());
      p = prs.partly_expand(p);
      if (p->is_a("@")) {
        add_syntax(p->args_begin(), p->args_end());
      } else {
        Stmt * cur = prs.finish_parse(p);
        lenv->add_stmt(cur);
      }
    }
    void add_syntax(parts_iterator i, parts_iterator end) {
      for (; i != end; ++i)
        add_syntax(*i);
    }
    struct Finalize : public CollectAction {
      ModuleBuilder * builder;
      Finalize(ModuleBuilder * b, Environ & e)
        : CollectAction(e), builder(b) {}
      void doit_hook() {
        builder->finalize(*env);
      }
    };
    void finalize(Environ & env) {
      //IOUT.printf("QQ: FINALIZE ON %p (%s)\n", this, ~name->to_string());
      if (env.special()) {
        //IOUT.printf("QQ: ENV SPECIAL ON %p (%s)\n", this, ~name->to_string());
        if (env.collect)
          env.collect->first_pass.add(new Finalize(this, env));
        return; // FIXME: I don't think this is quite right
      } else {
        //IOUT.printf("QQ: ALL GOOD ON %p (%s)\n", this, ~name->to_string());
      }
      for (CollectPart::iterator 
             i = collect.first_pass.begin(), 
             e = collect.first_pass.end(); 
           i != e; ++i) 
      {
        //IOUT.printf("QQ: FIRST PASS %p (%s) %p\n", this, ~name->to_string(), *i);
        //INCREASE_INDENT(IOUT);
        (*i)->doit(env);
      }
      //IOUT.printf("QQ: ADDING DEFN %p (%s)\n", this, ~name->to_string());
      env.add_defn(module);
      if (env.parse_def() && !env.collect) 
        finalize_second_pass(env);
      else if (env.collect) {
        env.collect->second_pass.add(new FinalizeSecondPass(this, env));
      }
    }
    struct FinalizeSecondPass : public CollectAction {
      ModuleBuilder * builder;
      FinalizeSecondPass(ModuleBuilder * b, Environ & e)
        : CollectAction(e), builder(b) {}
      void doit_hook() {
        builder->finalize_second_pass(*env);
      }
    };
    void finalize_second_pass(Environ & env) {
      if (user_type)
        handle_special_methods(user_type, *lenv);
      //printf("DUMPING %s\n", ~name->to_string());
      //module->syms.dump_this_scope();
      //IOUT.printf("START\n");
      for (CollectPart::iterator 
             i = collect.second_pass.begin(), 
             e = collect.second_pass.end(); 
           i != e; ++i)
      {
        //IOUT.printf("QQ: SECOND PASS %p (%s) %p\n", this, ~name->to_string(), *i);
        //INCREASE_INDENT(IOUT);
        (*i)->doit(env);
      }
      //IOUT.printf("DONE\n");
    }
    void parse_body(const Syntax * p) {
      add_syntax(p->args_begin(), p->args_end());
      finalize(*outer_env);
    }
    void desc(OStream & o) {
      if (user_type)
        user_type->desc(o);
      else
        module->desc(o);
      if (!do_finalize)
        o << " (forward)";
    }
    Stmt * complete(Environ & env) {
      SymbolName n = *name;
      //IOUT.printf("QQ: COMPLETE ON %p (%s)\n", this, ~name->to_string());
      module->where = env.where;
      Module * module_in_env = env.symbols.find_this_scope<Module>(SymbolKey(n, OUTER_NS));
      if (module_in_env) {
        if(module != module_in_env) throw unknown_error(0);
      } else {
        module->key = NULL;
        env.add(SymbolKey(n, OUTER_NS), module);
      }
      if (user_type) {
        UserType * ut_in_env = env.symbols.find_this_scope<UserType>(SymbolKey(n));
        if (ut_in_env) {
          assert(user_type == ut_in_env);
        } else {
          user_type->key = NULL;
          add_simple_type(env.types, SymbolKey(n), user_type, env.where);
        }
      }
      if (do_finalize)
        finalize(env);
      //IOUT.printf("QQ: COMPLETE DONE %p (%s)\n", this, ~name->to_string());
      return empty_stmt();
    }
  private:
    ModuleBuilder();
    ModuleBuilder(const ModuleBuilder &);
    void operator=(const ModuleBuilder &);
  };

  Stmt * parse_module(const Syntax * p, Environ & env) {
    assert_num_args(p, 1, 2);
    ModuleBuilder * m = new ModuleBuilder(p->arg(0), env);
    if (p->num_args() > 1)
      m->parse_body(p->arg(1));
    return empty_stmt();
  }

  struct GatherMarks {
    Vector<const Mark *> marks;
    void stripped_mark(const Mark * m) {marks.push_back(m);}
  };

  Stmt * parse_add_prop(const Syntax * p, Environ & env) {
    assert_num_args(p, 2, 3);
    if (p->num_args() == 2) {
      Module * m = dynamic_cast<Module *>(env.where);
      m->add_prop(*p->arg(0), p->arg(1));
    } else {
      Symbol * sym = env.symbols.find<Symbol>(p->arg(0));
      sym->add_prop(*p->arg(1), p->arg(2));
    }
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

  struct AlwaysTrueFilter {
    bool operator() (SymbolNode *) const {return true;}
  };

  struct OnlyThisFilter {
    SymbolKey key;
    OnlyThisFilter(const SymbolKey & k) : key(k) {}
    bool operator() (SymbolNode * n) const {
      return n->key == key;
    }
  };

  template <typename Filter>
  void import_module(const Module * m, Environ & env, const GatherMarks & gather, 
                     Filter & filter, bool same_scope) 
  {
    SymbolList l;
    for (SymbolNode * cur = m->syms.front; cur != m->syms.back; cur = cur->next) {
      if (!filter(cur)) continue;
      SymbolKey k = cur->key;
      // now add marks back in reverse order
      for (Vector<const Mark *>::const_reverse_iterator 
             i = gather.marks.rbegin(), e = gather.marks.rend();
           i != e; ++i)
        k.marks = mark(k.marks, *i);
      SymbolNode * res = l.push_back(cur->scope, k, cur->value);
      res->flags = cur->flags;
      if (same_scope) {
        res->set_flags(SymbolNode::ALIAS | SymbolNode::IMPORTED);
        if (!res->diff_scope())
          res->scope = env.where;
      } else {
        res->set_flags(SymbolNode::ALIAS | SymbolNode::IMPORTED | SymbolNode::DIFF_SCOPE);
      }
    }
    env.symbols.splice(l.first, l.last);
    env.add(SymbolKey("", SPECIAL_NS), new Import(m));
  }

  Stmt * parse_bring_to_this_scope(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    Symbol * sym = lookup_symbol<Symbol>(p->arg(0), DEFAULT_NS, env.symbols.front);
    env.symbols.add(env.where, *sym->key, sym, SymbolNode::ALIAS);
    return empty_stmt();
  }

  void import_module(const Module * m, Environ & env, const GatherMarks & gather, bool same_scope = false) 
  {
    AlwaysTrueFilter filter;
    import_module(m, env, gather, filter, same_scope);
  }


  Stmt * parse_import(const Syntax * p, Environ & env) {
    //assert_num_args(p, 1);
    GatherMarks gather;
    bool same_scope = p->flag("same_scope");
    const Module * m = lookup_symbol<Module>(p->arg(0), OUTER_NS, env.symbols.front, NULL, 
                                             NormalStrategy, gather);
    if (p->num_args() == 1) {
      //printf("IMPORTING EVERTHING FROM %s\n", ~m->name());
      import_module(m, env, gather, same_scope);
    } else {
      //printf("IMPORTING SOMETHINGS FROM %s (same_scope = %d)\n", ~m->name(), same_scope);
      OnlyThisFilter filter(*p->arg(1));
      import_module(m, env, gather, filter, same_scope);
    }
    return empty_stmt();
  }

  extern "C" Syntax * module_imports(const Syntax * p, Environ * env) {
    assert_num_args(p, 1);
    SyntaxBuilder res;
    SymbolName n = *p->arg(0);
    GatherMarks gather;
    const Module * m = lookup_symbol<Module>(p->arg(0), OUTER_NS, env->symbols.front, NULL, 
                                             NormalStrategy, gather);
    for (SymbolNode * cur = m->syms.front; cur != m->syms.back; cur = cur->next) {
      // now add marks back in reverse order;
      SymbolKey k = cur->key;
      for (Vector<const Mark *>::reverse_iterator 
             i = gather.marks.rbegin(), e = gather.marks.rend();
           i != e; ++i)
        k.marks = mark(k.marks, *i);
      res.add_part(SYN(k)); // FIXME not quite right
    }
    return res.build();
  }

  struct InnerNSDecl : public Declaration, public InnerNS::Tag {
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
    InnerNS::Tag * ns = new InnerNSDecl;
    env.add(SymbolKey(n, INNER_NS), ns);
    return empty_stmt();
  }

  //
  //
  //

  Stmt * parse_extern(const Syntax * p, Environ & env) {
    assert_num_args(p, 1, NPOS);
    const Syntax * val = p->arg(0);
    if (!val->simple() && val->is_a("s")) val = val->arg(0);
    bool mangle_save = env.mangle;
    if (val->eq("C")) env.mangle = false;
    else throw error(p->arg(0), "Unknown extern type \"%s\"\n", ~val->to_string());
    parse_stmts(p->args_begin() + 1, p->args_end(), env);
    env.mangle = mangle_save;
    return empty_stmt();
  }

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

  // "new" is for lack of a better name as it doesn't necessary create
  // a new instance
  UserType * new_user_type(const Syntax * p, Environ & env) {
    SymbolName name = *p;
    //printf("PARSING USER TYPE %s\n", ~name);
    UserType * s = env.symbols.find_this_scope<UserType>(SymbolKey(name));
    if (!s) {
      //printf("ADDING USER TYPE SYM %s (where = %s)\n", ~name, env.where ? ~env.where->full_name() : "<nil>");
      s = new UserType;
      s->category = new TypeCategory(name.name, USER_C);
      s->module = new_module(p, env);
      add_simple_type(env.types, SymbolKey(name), s, env.where);
    }
    return s;
  }

  void finalize_user_type(UserType * s, Environ & env) {
    assert(s->num == s->module->num);
    s->defined = true;
    s->finalize();
    Symbol * vs = env.symbols.find_this_scope<Symbol>("_sizeof");
    Var * v = dynamic_cast<Var *>(vs);
    if (v) {
      v->init = parse_exp(SYN(SYN("sizeof"), SYN(SYN(".type"), SYN(static_cast<Type *>(s)))), env);
      s->lt_sizeof_ = mk_id(v, env);
    }
  }

  struct UserTypeBuilder : public ModuleBuilder {
    UserTypeBuilder(const Syntax * p, Environ & e)
      : ModuleBuilder(p, e, new_user_type(p, e)) {}
    UserTypeBuilder(const Syntax * p, Environ & e, UserType * u)
      : ModuleBuilder(p, e, u) {}
  };

  Stmt * parse_declare_user_type(const Syntax * p, Environ & env, DeclHandle ** handle) {
    UserType * ut = new_user_type(p->arg(0), env);
    if (handle) {
      UserTypeBuilder * utb = new UserTypeBuilder(p->arg(0), env, ut);
      utb->do_finalize = false;
      *handle = utb;
    }
    return empty_stmt();
  }

  Stmt * parse_make_user_type(const Syntax * p, Environ & env) {
    assert_num_args(p, 1, 2);
    SymbolName name = *p->arg(0);
    UserType * s = new_user_type(p->arg(0), env);
    if (p->num_args() > 1) {
      s->type = parse_type(p->arg(1), env);
    } else {
      s->type = env.types.inst(SymbolKey(name, TAG_NS));
      //parse_stmt_decl(SYN(SYN("talias"), p->arg(0), SYN(s->type)), env);
    }
    finalize_user_type(s, env);
    return empty_stmt();
  }

  Stmt * parse_user_type(const Syntax * p, Environ & env) {
    //assert_num_args(p, 1, 3);
    assert_num_args(p, 1, 2);
    UserTypeBuilder * m = new UserTypeBuilder(p->arg(0), env);
    if (p->num_args() > 1)
      m->parse_body(p->arg(1));
    return empty_stmt();
  }

  Stmt * parse_finalize_user_type(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    Module * m = dynamic_cast<Module *>(env.where);
    //printf("FINALIZE USER TYPE %s\n", ~m->name.to_string());
    Type * t0 = env.types.inst(SymbolName(*m->key)); 
    //                        ^^ need to kill namespace but preserve marks
    UserType * s = dynamic_cast<UserType *>(t0);
    s->type = parse_type(p->arg(0), env);
    finalize_user_type(s, env);
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

  static void add_cast(Module * m, const Syntax * cast, const char * what, 
                       UserType * from, UserType * to, Environ & env) {
    UserCast * user_cast = new UserCast;
    user_cast->from = from;
    user_cast->to = to;
    user_cast->cast_macro = env.symbols.lookup<Symbol>(cast);
    env.add(SymbolKey(what, CAST_NS), user_cast);
    m->syms.front->next = new SymbolNode(m, SymbolKey(what, CAST_NS), user_cast, m->syms.front->next); // FIMXE: What is this doing?
  }

  Stmt * parse_make_subtype(const Syntax * p, Environ & env) {
    //printf(">>%s\n", ~p->to_string());
    assert_num_args(p, 2, 3);
    const Syntax * parent = p->arg(0);
    const Syntax * up_cast = p->arg(1);
    const Syntax * down_cast = p->num_args() > 2 ? p->arg(2) : NULL;

    // FIXME: Add check that we are in a user_type
    Module * m = dynamic_cast<Module *>(env.where);
    UserType * parent_t = dynamic_cast<UserType *>(env.types.inst(parent));
    UserType * child_t  = dynamic_cast<UserType *>(env.types.inst(SymbolKey(m->name())));
    assert(!child_t->parent);
    child_t->parent = parent_t;
    child_t->category = new TypeCategory(child_t->name(), parent_t->category);

    add_cast(m, up_cast, "up_cast", parent_t, child_t, env);
    if (down_cast)
      add_cast(m, down_cast, "down_cast", child_t, parent_t, env);
      
    return empty_stmt();
  }

  static const Syntax * THIS = SYN("this");

  Exp * parse_member_access(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    Exp * exp = parse_exp(p->arg(0), env);
    exp = exp->to_effective(env);
    Syntax * ptr_exp = SYN(SYN("addrof"), SYN(exp));
    if (dynamic_cast<const StructUnion *>(exp->type->unqualified)) {
      const Syntax * np = SYN(p->str(), p->part(0), SYN(exp), p->arg(1));
      return (new MemberAccess)->parse_self(np, env);
    } else if (const UserType * t = dynamic_cast<const UserType *>(exp->type->unqualified)) {
      // FIXME: Am I calling partly_expand in the right scope here, or correctly at all
      const Syntax * arg1 = partly_expand(p->arg(1), OtherPos, env, EXPAND_NO_MACRO_CALL);
      const Syntax * call;
      if (arg1->is_a("call")) {
        assert_num_args(arg1, 2);
        const Syntax * n = expand_id(arg1->arg(0));
        const Syntax * a = SYN(*arg1->arg(1), PARTS(), FLAGS(SYN(THIS, ptr_exp)));
        const Symbol * sym;
        //if (n->is_a("::")) // FIXME: This is hack, and not quite right
        //  sym = lookup_symbol<Symbol>(n, DEFAULT_NS, env.symbols.front, NULL, StripMarks);
        //else
        //  sym = lookup_symbol<Symbol>(n, DEFAULT_NS, t->module->syms, NULL, StripMarks);
        if (!n->is_a("::"))
          n = SYN(SYN("::"), SYN<Symbol>(t->module), n);
        //call = SYN(p->str(), arg1->part(0), SYN(ID, SYN(n->str(), sym)), a);
        call = SYN(p->str(), arg1->part(0), n, a);
      } else {
        const Syntax * n = expand_id(arg1);
        const Symbol * sym = lookup_symbol<Symbol>(n, DEFAULT_NS, t->module->syms.front, t->module->syms.back, StripMarks);
        Syntax * c = new_syntax(p->str(), PARTS(ID, SYN(n->str(), sym)), FLAGS(SYN(THIS, ptr_exp)));
        //SYN(p->str(), ID, SYN(n, sym));
        //c->add_flag(SYN(THIS, ptr_exp));
        call = c;
      }
      //printf("member: %s\n", ~call->to_string());
      return parse_exp(call, env);
    } else {
      throw error(p->arg(0), "Expected struct or user type, got %s.",
                  abi::__cxa_demangle(typeid(*exp->type->unqualified).name(), NULL, NULL, NULL)); 
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
    const UserType * t = dynamic_cast<const UserType *>(exp->type->unqualified);
    assert(t);
    if (!t) throw error(p->arg(0), "Expected user type but got ??");
    if (!t->defined) throw error(p->arg(1), "Invalid use of incomplete type");
    // FIXME: Should't change the type of an exp as it can cause
    //   problems when p->arg(0) is an entity
    exp->type = change_unqualified(exp->type, t->type); 
    Syntax * res = SYN(SYN("member"),
                              SYN(exp),
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
    const Symbol * s = lookup_symbol<Symbol>(d->arg(0), DEFAULT_NS, m->syms.front, m->syms.back, StripMarks);
    Environ env = env0.new_scope();
    import_module(m, env, gather, true);
    const Symbol * pm = m->find_symbol<Symbol>("_parse_memberdecl");
    
    parse_top_level(d, env);
    return empty_stmt();
  }

  struct ImportedFile : public Symbol {
    bool included;
    ImportedFile(bool inc = false) : included(inc) {}
  };

  String get_file_id(String file_name) {
    struct stat stat_d;
    int res = stat(~file_name, &stat_d);
    if (res) {
      perror(~file_name);
      abort();
    }
    StringBuf buf;
    buf.printf("fid:%lld:%lu", stat_d.st_dev, stat_d.st_ino);
    return buf.freeze();
  }

  void include_file(String file_name, Environ & env) {
    String file_id = get_file_id(file_name);
    ImportedFile * imf = env.symbols.find<ImportedFile>(file_id);
    if (imf) {
      if (imf->included) {
        fprintf(stderr, "Error: Trying to include file which is already imported: %s\n", ~file_name);
        abort();
      } else {
        printf("SKIPPING (already included): %s\n", ~file_name);
        return;
      }
    }
    clock_t start = clock();
    printf("INCLUDING: %s\n", ~file_name);
    SourceFile * code = new_source_file(file_name);
    parse_stmts(parse_str("SLIST", SourceStr(code, code->begin(), code->end())), env);
    clock_t parse_done = clock();
    clock_t load_done = clock();
    printf("Include Time (%s): %f  (parse: %f)\n", ~file_name,
           (load_done - start)/1000000.0, (parse_done - start)/1000000.0);
    imf = new ImportedFile(true);
    env.add(file_id, imf);
  }

  Stmt * parse_include_file(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    String file_name = *p->arg(0);
    file_name = add_dir_if_needed(file_name, p->str().source);
    include_file(file_name, env);
    return empty_stmt();
  }

  void import_file(String file_name, Environ & env) {
    String file_id = get_file_id(file_name);
    ImportedFile * imf = env.symbols.find<ImportedFile>(file_id);
    if (imf) {
      printf("SKIPPING (already imported): %s\n", ~file_name);
      return;
    }
    clock_t start = clock();
    printf("IMPORTING: %s\n", ~file_name);
    SourceFile * code = new_source_file(file_name);
    bool env_interface_orig = env.interface;
    try {
      env.interface = true;
      assert(!env.collect);
      parse_stmts(parse_str("SLIST", SourceStr(code, code->begin(), code->end())), env);
    } catch (...) {
      env.interface = env_interface_orig;
      throw;
    }
    clock_t parse_done = clock();
    env.interface = env_interface_orig;
    const char * dot = strrchr(~file_name, '.');
    StringBuf buf;
    if (!dot)
      buf.append(file_name);
    else
      buf.append(~file_name, dot);
    buf.append("-fct.so");
    String lib_fn = buf.freeze();
    int res = access(~lib_fn, F_OK);
    if (res == 0) {
      load_macro_lib(~lib_fn, env);
    }
    clock_t load_done = clock();
    printf("Import Time (%s): %f  (parse: %f)\n", ~file_name, 
           (load_done - start)/1000000.0, (parse_done - start)/1000000.0);
    imf = new ImportedFile();
    env.add(file_id, imf);
  }

  Stmt * parse_import_file(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    String file_name = *p->arg(0);
    file_name = add_dir_if_needed(file_name, p->str().source);
    import_file(file_name, env);
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
    const UserCast * cast = lookup_symbol<UserCast>(SymbolKey("up_cast", CAST_NS), exp->source_str(), 
                                                    from_base->module->syms.front, 
                                                    from_base->module->syms.back);//, NULL, NormalStrategy, gather, cmp);
    const Syntax * p = SYN(SYN("call"), 
                                  SYN(SYN("id"), SYN(cast->cast_macro)),
                                  SYN(SYN("."), SYN(exp)));
    Exp * res = parse_exp(p, env);
    res = res->resolve_to(to_ptr, env);
    return cast_up(res, type, env);
  }

  Exp * cast_down(Exp * exp, const UserType * type, Environ & env) {
    const PointerLike * from_ptr = dynamic_cast<const PointerLike *>(exp->type->unqualified);
    const QualifiedType * from_qualified = dynamic_cast<const QualifiedType *>(from_ptr->subtype->root);
    const Type * from_base = from_ptr->subtype->unqualified;
    if (from_base == type) return exp;
    const Type * parent = type->parent;
    assert(parent); 
    const Type * to_qualified = from_qualified
      ? env.types.inst(".qualified", TypeParm(from_qualified->qualifiers), TypeParm(type))
      : type;
    const Type * to_ptr = env.types.inst(".ptr", to_qualified);
    NoOpGather gather;
    const UserCast * cast = lookup_symbol<UserCast>(SymbolKey("down_cast", CAST_NS), exp->source_str(), 
                                                    type->module->syms.front, type->module->syms.back);
    Exp * subexp = exp;
    if (parent != from_base) {
      const UserType * t = dynamic_cast<const UserType *>(parent);
      subexp = cast_down(exp, t, env);
    }
    const Syntax * p = SYN(SYN("call"), 
                                  SYN(SYN("id"), SYN(cast->cast_macro)),
                                  SYN(SYN("."), SYN(subexp)));
    Exp * res = parse_exp(p, env);
    res = res->resolve_to(to_ptr, env);
    return res;
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
    if (t->is_a("<>")) {
      t = reparse("TOKENS", t->inner());
      t = parse_decl_->parse_type(t, env);
    } else if (t->is_a(".type")) {
      t = t->arg(0);
    }
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

  Stmt * parse_fun_forward(const Syntax * p, Environ & env, Collect * collect) {
    assert_num_args(p,3,4);

    //printf("FUN:: %s\n", ~p->to_string());

    SymbolKey name;
    bool is_op = false;
    if (p->arg(0)->is_a("operator")) {
      name = *p->arg(0)->arg(0);
      if (name.name == "==")
        name.name = "eq";
      else if (name.name == "!=")
        name.name = "ne";
      else
        throw error(p, "Overloading of \"%s\" not supported yet.\n", ~name.name);
      name.ns = OPERATOR_NS;
      is_op = true;
    } else {
      name = expand_binding(p->arg(0), env);
    }
    
    Fun * f = NULL;
    if (!is_op) {
      Symbol * s = find_symbol<Symbol>(name, env.symbols.front, env.symbols.back, ThisScope);
      //if (s)
      //  printf("found somthing: %s %s %p %s\n", ~p->arg(0)->to_string(), ~name, s, ~s->name());
      //if (s)
      //  assert(name.name == s->name());
      if (s) {
        const Tuple * parms = expand_fun_parms(p->arg(1), env);
        f = const_cast<Fun *>(find_overloaded_symbol<Fun>(parms, p->arg(0), s, env));
      }
      //if (s)
      //  printf("found somthing: %s %s %p %s\n", ~p->arg(0)->to_string(), ~name, s, ~s->name());
      //if (s)
      //  assert(name.name == s->name());
    }
    bool previous_declared = f;
    if (previous_declared) {
      if (p->num_args() > 3)
        f->parse_forward_i(p, env, collect);
    } else {
      f = new Fun;
      if (is_op) f->overload = false; // FIXME: Hack
      f->storage_class = get_storage_class(p);
      make_static_if_marked(f->storage_class, name);
      if (env.interface && f->storage_class == SC_STATIC)
        return empty_stmt();
      //f->num = f->storage_class == SC_STATIC ? NPOS : 0;
      f->parse_forward_i(p, env, collect);
      env.add(name, f, is_op ? true : false);
      if (is_op) {
        if (f->parms->num_parms() != 2)
          throw error(p, "Expected two paramaters for operator overloading.");
        if (f->parms->parms[0].type != f->parms->parms[1].type)
          throw error(p, "Paramater types for operator overloading must match.");
        if (!f->parms->parms[0].type->is(USER_C))
          throw error(p, "Paramater types for operator overloading must be user type.");
      }
    }
    return f;
  }

  Stmt * parse_fun(const Syntax * p, Environ & env) {
    if (env.parse_def() && !env.collect) {
      Collect collect;
      Stmt * f = parse_fun_forward(p, env, &collect);
      if (!collect.second_pass.empty()) {
        // FIXME: Make sure it is a CollectParseDef
        collect.second_pass[0]->doit(env);
      }
      return f;
    } else {
      return parse_fun_forward(p, env, env.collect);
    }
  }

  bool Fun::uniq_name(OStream & o) const {
    if (key->ns == OPERATOR_NS) {
      o << "op$";
      TopLevelVarDecl::uniq_name(o);
      o << "$";
      const UserType * ut = dynamic_cast<const UserType *>(parms->parms[0].type->effective->unqualified);
      ut->uniq_name(o);
    } else {
      StringBuf buf;
      TopLevelVarDecl::uniq_name(buf);
      if (overload && mangle) {
        for (unsigned i = 0; i != parms->parms.size(); ++i) {
          mangle_print_inst->to_string(*parms->parms[i].type, buf);
        }
      }
      o << buf.freeze();
    }
    return num != NPOS;
  }
  
  AST * Fun::parse_forward_i(const Syntax * p, Environ & env0, Collect * collect) {
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

    mangle = !ct_callback && env0.mangle;

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
    if (!env.interface && p->num_args() > 3) {
      if (collect) {
        //IOUT.printf("Adding to collect for %p %s\n", this, ~p->to_string());
        collect->second_pass.add(new CollectParseDef(this, env0));
      }
    } else {
      symbols = env.symbols;
    }

    //sym->value = this;

    return this;
  }

  Stmt * Fun::finish_parse(Environ & env0) {
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
      BasicVar * sym = new_auto_var(n, i->type);
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
    return empty_stmt();
  }

  void Fun::finalize(FinalizeEnviron & env0) {
    //if (body) {
    //  FinalizeEnviron env = env0;
    //  env.fun_symbols = symbols.front;
    //  body->finalize(env);
    //}
  }

  void Fun::compile_prep(CompileEnviron & env) {
    if (env.for_macro_sep_c && (env_ss || is_macro) && body)
      env.for_macro_sep_c->macro_funs.push_back(this);
    if (body) {
      body->compile_prep(env);
    }
  }

  void Fun::compile(CompileWriter & f, Phase phase) const {
    if (!body && phase == Body)
      return;
    //f << indent << "# " << full_name() << "\n";
    if (env_ss && phase != Forward) {
      f << "(var " << uniq_name() << '$' << "env_ss" << " (.ptr (struct EnvironSnapshot)))\n";
    }
    f << "(fun " << uniq_name();
    f << " " << parms;
    f << " " << ret_type;
    write_storage_class(f);
    //if (inline_) FIXME: Inline not support by zls yet
    //    f << " :inline";
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
    Stmt * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(0,1);
      if (p->num_args() == 1) {
        ExpInsrPointWrapper wrap(env);
        exp = parse_exp(p->arg(0), wrap.env);
        exp = exp->resolve_to(env.frame->return_type, wrap.env);
        return wrap.finish(this);
      } else {
        exp = NULL;
        return this;
      }
    }
    void finalize(FinalizeEnviron & env) {
      if (exp)
        exp->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      if (exp)
        exp->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      if (exp) 
        f << indent << "(return " << exp << ")\n";
      else
        f << indent << "(return)\n";
    }
  };

  static void resolve_fun_parms(const Vector<Exp *> & parms, Vector<Exp *> * res, const Tuple * fun_parms, 
                                Environ & env, Syntax * syn = NULL) {
    if (!fun_parms->vararg && parms.size() != fun_parms->parms.size()) 
      throw error(syn, 
                  "Wrong number of parameters, expected %u but got %u when calling %s",
                  fun_parms->parms.size(), parms.size(), "??");
    else if (fun_parms->vararg && parms.size() < fun_parms->parms.size())
      throw error(syn,
                  "Not enough parameters, expected at least %u but got %u when calling %s",
                  fun_parms->parms.size(), parms.size(), "??");
    const int typed_parms = fun_parms->parms.size();
    const int num_parms = parms.size();
    int i = 0;
    for (;i != typed_parms; ++i) {
      Exp * r = env.type_relation->resolve_to(parms[i], fun_parms->parms[i].type, env, 
                                              TypeRelation::Implicit, 
                                              res ? TypeRelation::CheckOnlyFalse : TypeRelation::CheckOnly);
      if (res)
        (*res)[i] = r;
    }
    if (res) 
      for (;i != num_parms; ++i) {
        (*res)[i] = parms[i]->def_arg_prom(env);
      }
  }

  struct Call : public Exp {
    Call() {} 
    const char * what() const {return "call";}
    //AST * part(unsigned i) {return i == 0 ? lhs : new Generic(syn->arg(1), parms);}
    Exp * lhs;
    Vector<Exp *> parms;
    Call * construct(Exp * lhs0, Vector<Exp *> & parms0 /* will steal */, Environ & env) {
      lhs = lhs0;
      parms.swap(parms0);
      const Function * ftype = dynamic_cast<const Function *>(lhs->type);
      if (!ftype) {
        if (const Pointer * t = dynamic_cast<const Pointer *>(lhs->type))
          ftype = dynamic_cast<const Function *>(t->subtype);
      }
      if (!ftype)
        throw error (lhs->syn, "Expected function type");
      type = ftype->ret;
      lvalue = type->addressable ? LV_NORMAL : LV_FALSE;
      resolve_fun_parms(parms, &parms, ftype->parms, env, syn->arg(1));
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
  Exp * mk_call(Syntax * p, Exp * lhs0, Vector<Exp *> & parms0 /* will steal */, Environ & env) {
    Call * call = new Call;
    call->syn = p;
    call->construct(lhs0, parms0, env);
    return call;
  }

  struct GatherExtraCmp {
    SymbolNode::Scope scope;
    Vector<const Symbol *> syms;
    GatherExtraCmp() : scope() {}
    bool operator() (const SymbolNode * n) {
      //printf("?? %s %p\n", ~n->key.to_string(), n->scope);
      if (!n->value->overloadable()) return true;
      if (!scope) scope = n->scope;
      if (n->scope == scope)
        syms.push_back(n->value);
      return false;
    }
  };

  const Symbol * resolve_call(const Syntax * name, const Vector<Exp *> & parms, Environ & env) {
    const Symbol * sym = env.symbols.lookup<Symbol>(name);
    if (const OverloadedSymbol * cur = dynamic_cast<const OverloadedSymbol *>(sym)) {
      Vector<const Symbol *> syms;
      for (; cur; cur = cur->next)
      {
        try {
          if (std::find(syms.begin(), syms.end(), cur->sym) != syms.end()) continue;
          resolve_fun_parms(parms, NULL, cur->sym->overloadable(), env, NULL);
          syms.push_back(cur->sym);
        } catch (...) {}
      }
      if (syms.size() == 0)
        throw error(name, "No match for call to %s", ~name->to_string());
      if (syms.size() > 1)
        throw error(name, "Multiple matches for call to %s", ~name->to_string());
      //assert(syms.size() == 1); // FIXME: Error Message
      sym = syms.front();
    }
    return sym;
  }

  Exp * parse_call(const Syntax * p, Environ & env) {
    //return (new Call)->parse_self_(p, env);
    //printf("CALL %s\n", ~p->to_string());
    assert_num_args(p, 2);
    Syntax * lhs = partly_expand(p->arg(0), ExpPos, env);
    Vector<Exp *> parms;
    if (lhs->is_a("id")) {
      add_ast_nodes(p->arg(1)->args_begin(), p->arg(1)->args_end(), parms, Parse<ExpPos>(env));
      const Symbol * sym = resolve_call(lhs->arg(0), parms, env);
      if (const Fun * fun = dynamic_cast<const Fun *>(sym)) {
        return mk_call(p, mk_id(fun, env), parms, env);
      } else if (const Syntax * res = expand_macro(p, sym, parms, env)) {
        return parse_exp(res, env);
      } else {
        abort();
      }
    } else {
      Exp * lhs_e = just_parse_exp(lhs, env, ExpContext()); // must be done here
      add_ast_nodes(p->arg(1)->args_begin(), p->arg(1)->args_end(), parms, Parse<ExpPos>(env));
      return mk_call(p, lhs_e, parms, env);
    }
  }
  
  //
  //
  //
  
  Stmt * parse_type_alias(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    SymbolKey n = expand_binding(p->arg(0), DEFAULT_NS, env);
    Type * of = parse_type(p->arg(1), env);
    if (env.symbols.exists_this_scope(n)) {
      Type * old = env.types.inst(n);
      if (of->root != old->root)
        throw error(p, "Conflicting typedefs.");
    } else {
      TypeAlias * decl = new TypeAlias(of);
      decl->syn = p;
      SimpleType * talias = add_simple_type(env.types, n, decl, env.where);
    }
    return empty_stmt();
  }

  void TypeAlias::compile(CompileWriter & f, Phase phase) const {
    if (phase == Body) return;
    f << indent << "(talias " << uniq_name() << " " << of << ")\n";
  }

  void ForwardTypeDecl::compile(CompileWriter & f, Phase phase) const {
    if (phase == Body) return;
    if (strcmp(what_, "user_type")) {
      f << "(declare_user_type " << uniq_name() << ")\n";
    } else {
      abort();
    }
  }

  struct CollectAddDefn : public CollectAction {
    Declaration * decl;
    CollectAddDefn(Declaration * d, Environ & e)
      : CollectAction(e), decl(d) {}
    void doit_hook() {
      //IOUT.printf("qq: collect add defn: %p %s \n", static_cast<Stmt *>(decl), ~decl->desc());
      env->add_defn(decl);
    }
  };

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
      if (!env0.special())
        env0.add_defn(decl);
      else if (env0.collect)
        env0.collect->first_pass.add(new CollectAddDefn(decl, env0));
    } else {
      decl->have_body = false;
    }
    //StringBuf type_name;
    //type_name << "struct " << what();
    //if (s->members.empty())
    //  fprintf(stderr, "Warning: %s\n", error(p, "Empty Struct Currently Unsupported")->message().c_str());
    if (p->flag("bit-field")) {
      printf("WARNING: Bit Fields not supported in %s\n", ~name->to_string());
      decl->bit_field = true;
    }
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
    f << indent << "# " << full_name() << "\n";
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
      const Type * t = members[i].sym->type;
      if (t->storage_align() > align_) align_ = t->storage_align();
      //printf("?? %s: %u %u\n", ~desc(), size_, t->storage_align());
      unsigned align_offset = size_ % t->storage_align();
      if (align_offset != 0) size_ += t->storage_align() - align_offset;
      members[i].offset = size_;
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
          val = parse_ct_value(arg->part(1), env);
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
    Exp * lt_sizeof;
    SizeOf * parse_self(const Syntax * p, Environ & env);
    void finalize(FinalizeEnviron &) {}
    void compile(CompileWriter & f) {
      f << "(n " << sizeof_type->size() << " (size_t))";
    }
  };
  
  SizeOf * SizeOf::parse_self(const Syntax * p, Environ & env) {
    syn = p;
    assert_num_args(1);
    if (p->arg(0)->is_a(".type")) {
      sizeof_type = parse_type(p->arg(0)->arg(0), env);
    } else {
      Exp * exp = parse_exp(p->arg(0), env);
      sizeof_type = parse_type(SYN(SYN(".typeof"), SYN(exp)), env);
    }
    lt_sizeof = sizeof_type->lt_sizeof();
    type = env.types.ct_const(env.types.inst(".size"));
    if (lt_sizeof)
      lt_sizeof = lt_sizeof->resolve_to(type, env);
    else
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
    syn = SYN(cs, *syn);
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
  //
  //

  struct GccBuiltIn : public Exp {
    const char * what_;
    unsigned num_parms;
    GccBuiltIn(const char * wt, unsigned n, const Type * t) 
      : what_(wt), num_parms(n) {type = t;}
    const char * what() const {return what_;}
    Vector<Exp *> parms;
    GccBuiltIn * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(num_parms);
      add_ast_nodes(p->args_begin(), p->args_end(), parms, Parse<ExpPos>(env));
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      for (int i = 0; i != num_parms; ++i)
        parms[i]->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      for (int i = 0; i != num_parms; ++i)
        parms[i]->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << "(call " << what_ << " (.";
      int i = 0;
      while (i != num_parms) {
        f << " " << parms[i];
        ++i;
      }
      f << "))";
    }
  };

  struct VaArg : public Exp {
    const char * what() const {return "__builtin_va_arg";}
    Exp * va;
    VaArg * parse_self(const Syntax * p, Environ & env) {
      syn = p;
      assert_num_args(2);
      va = parse_exp(p->arg(0), env);
      const Syntax * type_s = p->arg(1);
      if (type_s->is_a("parm")) {
        type_s = reparse("TOKENS", type_s->outer());
        type_s = parse_decl_->parse_type(type_s, env);
        if (!type_s) throw error(p->arg(1), "Expected type.");
      }
      type = parse_type(type_s, env);
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      va->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      va->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << "(call __builtin_va_arg (.";
      f << " " << va;
      f << " " << type;
      f << "))";
    }
  };

  //

  AST * parse_top(const Syntax * p) {
    Environ env(TOPLEVEL);
    return parse_top(p, env);
  }

  Stmt * try_just_decl(const Syntax * p, Environ & env, DeclHandle ** = NULL);
  Stmt * try_just_stmt(const Syntax * p, Environ & env);
  Exp * try_just_exp(const Syntax * p, Environ & env, ExpContext c);
  Stmt * try_exp_stmt(const Syntax * p, Environ & env);

  void try_error(const Syntax * p, Environ & env) {
    if (Error * err = p->entity<Error>())
      throw err;
  }

  template <>
  Stmt * Parse<TopLevel>::finish_parse(const Syntax * p) const {
    Stmt * res;
    //printf("Parsing top level:\n  %s\n", ~p->to_string());
    try_error(p, env);
    res = try_just_decl(p, env);
    if (res) return res;
    throw error (p, "Unsupported primative at top level:: %s", ~p->what());
    //throw error (p, "Expected top level expression.");
  }

  Stmt * parse_top_level(const Syntax * p, Environ & env) {
    return Parse<TopLevel>(env)(p);
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
    //fprintf(stderr, "PRE PARSING %s\n", ~p->to_string());
    assert(env.special());
    DeclHandle * handle = NULL;
    try_just_decl(p, env, &handle);
    if (handle)
      return SYN(p->str(), handle);
    else
      return p;
  }

  template <>
  Stmt * Parse<StmtPos>::finish_parse(const Syntax * p) const {
    Stmt * res;
    try_error(p, env);
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
    try_error(p, env);
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

  Stmt * parse_stmts(parts_iterator i, parts_iterator end) {
    return NULL;
  }

  static Exp * just_parse_exp(const Syntax * p, Environ & env, ExpContext c) {
    Exp * res;
    try_error(p, env);
    res = try_just_exp(p, env, c); 
    if (res) return res;
    //abort();
    throw error (p, "Unsupported primative at expression position: %s", ~p->what().name);
    //throw error (p, "Expected expression.");
  }

  static Exp * parse_exp(const Syntax * p, Environ & env, ExpContext c) {
    p = partly_expand(p, ExpPos, env);
    return just_parse_exp(p, env, c);
  }


  Exp * parse_exp(const Syntax * p, Environ & env) {
    return parse_exp(p, env, ExpContext());
  }

  Exp * parse_exp_for_type(const Syntax * p, Environ & env) {
    ExpInsrPointWrapper wrap(env);
    return parse_exp(p, wrap.env, ExpContext());
  }
  
  template<> 
  Exp * Parse<ExpPos>::finish_parse(const Syntax * p) const {
    return just_parse_exp(p, env, ExpContext());
  }

  Stmt * try_just_decl(const Syntax * p, Environ & env, DeclHandle ** handle) {
    if (DeclHandle * decl = p->entity<DeclHandle>()) {
      if (handle) {
        assert(env.special() && !env.collect);
        *handle = decl;
        // still need to call complete to add the object to the env.
        return decl->complete(env);
      } else {
        return decl->complete(env);
      }
    }
    String what = p->what().name;
    if (what == "var")     return parse_var(p, env);
    if (what == "fun" )    return parse_fun(p, env), empty_stmt();
    if (what == "struct")  return parse_struct(p, env);    
    if (what == ".struct")  return parse_struct(p, env);
    if (what == "union")   return parse_union(p, env);
    if (what == ".union")   return parse_union(p, env);
    if (what == "enum")    return parse_enum(p, env);
    if (what == ".enum")    return parse_enum(p, env);
    if (what == "talias")  return parse_type_alias(p, env);
    if (what == "local_label") return (new LocalLabelDecl)->parse_self(p, env);
    if (what == "macro")   return parse_map(p, env);
    //if (what == "smacro")  return parse_map(p, env);
    if (what == "make_macro")         return parse_macro(p, env);
    //if (what == "make_syntax_macro")  return parse_macro(p, env);
    if (what == "fluid_binding") return parse_fluid_binding(p, env);
    if (what == "module")        return parse_module(p, env);
    if (what == "import")        return parse_import(p, env);
    if (what == "bring_to_this_scope") return parse_bring_to_this_scope(p, env);
    if (what == "make_inner_ns") return parse_make_inner_ns(p, env);
    if (what == "make_user_type") return parse_make_user_type(p, env);
    if (what == "user_type")          return parse_user_type(p, env);
    if (what == "finalize_user_type") return parse_finalize_user_type(p, env);
    if (what == "make_subtype") return parse_make_subtype(p, env);
    if (what == "declare_user_type") return parse_declare_user_type(p, env, handle);
    if (what == "export")  return parse_export(p, env);
    if (what == "add_prop")  return parse_add_prop(p, env);
    if (what == "memberdecl") return parse_memberdecl(p, env);
    if (what == "include_file") return parse_include_file(p, env);
    if (what == "import_file") return parse_import_file(p, env);
    if (what == "extern") return parse_extern(p, env);
    return 0;
  }

  Stmt * try_just_stmt(const Syntax * p, Environ & env) {
    if (Stmt * stmt = p->entity<Stmt>()) return stmt;
    String what = p->what().name;
    if (what == "goto")    return (new Goto)->parse_self(p, env);
    if (what == "label")   return parse_label(p, env);
    if (what == "case")    return (new Case)->parse_self(p, env);
    if (what == "if")      return (new If)->parse_self(p, env);
    if (what == ".switch") return (new Switch)->parse_self(p, env);
    if (what == "block")   return (new Block)->parse_self(p, env);
    if (what == "return")  return (new Return)->parse_self(p, env);
    if (what == "cleanup") return (new Cleanup)->parse_self(p, env);
    return 0;
  }

  Exp * try_just_exp(const Syntax * p, Environ & env, ExpContext c) {
    if (Exp * exp = p->entity<Exp>()) return exp;
    String what = p->what().name;
    if (what == "id")      return (new Id)->parse_self(p, env);
    if (what == "n")       return (new Literal)->parse_self(p, env);
    if (what == "f")       return (new FloatC)->parse_self(p, env);
    if (what == "c")       return (new CharC)->parse_self(p, env);
    if (what == "s")       return (new StringC)->parse_self(p, env);
    if (what == "eif")     return parse_eif(p, env);
    if (what == "assign")      return parse_assign(p, env);
    if (what == "init-assign") return parse_init_assign(p, env);
    if (what == "construct")   return parse_construct(p, env);
    if (what == "destroy")     return parse_destroy(p, env);
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
    if (what == "call")    return parse_call(p, env);
    if (what == "anon")    return parse_anon(p, env, c);
    if (what == "seq")     return parse_seq(p, env, c);
    if (what == "eblock")  return parse_eblock(p, env, c);
    if (what == "sizeof")  return (new SizeOf)->parse_self(p, env);
    if (what == "cast")    return parse_cast(p, env, TypeRelation::Explicit);
    if (what == "icast")   return parse_cast(p, env, TypeRelation::Implicit);
    if (what == "implicit_cast")    return parse_cast(p, env, TypeRelation::Implicit);
    if (what == "reinterpret_cast") return parse_cast(p, env, TypeRelation::Reinterpret);
    if (what == ".")       return (new InitList)->parse_self(p, env);
    if (what == "noop")    return (new NoOp)->parse_self(p, env);
    //if (what == "empty")   return (new Empty)->parse_self(p, env);
    if (what == "syntax")           return (new SyntaxC)->parse_self(p, env);
    if (what == "raw_syntax")       return (new SyntaxC)->parse_self(p, env);
    if (what == "environ_snapshot") return (new EnvironSnapshot)->parse_self(p, env);
    if (what == "__builtin_va_start") 
      return (new GccBuiltIn("__builtin_va_start", 2, VOID_T))->parse_self(p, env);
    if (what == "__builtin_va_end") 
      return (new GccBuiltIn("__builtin_va_end", 1, VOID_T))->parse_self(p, env);
    if (what == "__builtin_va_arg")
      return (new VaArg)->parse_self(p, env);
    if (what == "__builtin_va_copy") 
      return (new GccBuiltIn("__builtin_va_copy", 2, VOID_T))->parse_self(p, env);
    if (what == "c-assign") {
      Exp * lhs = parse_exp(p->arg(1), env);
      Exp * rhs = try_just_exp(SYN(p->str(), p->arg(0), SYN(p->arg(1)->str(), lhs), p->arg(2)), env, c);
      if (!rhs) return 0;
      BinOp * binop = dynamic_cast<BinOp *>(rhs);
      StringBuf op;
      op << binop->op << "=";
      return new CompoundAssign(what, op.freeze(), lhs, binop, env);
    }
    return 0;
  }

  Stmt * try_exp_stmt(const Syntax * p, Environ & env) {
    ExpInsrPointWrapper wrap(env);
    Exp * exp = try_just_exp(p, wrap.env, VOID_CONTEXT);
    if (exp) {
      return wrap.finish(exp);
    } else {
      return NULL;
    }
  }

  //
  //
  //

  target_int parse_ct_value(const Syntax * syn, Environ & env) { 
   ExpInsrPointWrapper wrap(env);
    Exp * e = parse_exp(syn, wrap.env);
    if (wrap.stmts)
      throw error(syn, "Invalid Compile Time Expression");
    e = e->resolve_to(env.types.inst("int"), env);
    return e->ct_value<target_int>();
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
    Types type_defns;
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
      //printf("DEFN %p\n", cur);
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
      cw << "(var _macro_funs_size (unsigned) :(__visibility__ (s protected)) " << macro_funs_size << ")\n";
      if (macro_funs_size  > 0 ) {
        cw << "(var _macro_funs (.array (.ptr (char :const)) " << macro_funs_size << ") :(__visibility__ (s protected)) (.\n";
        for (Vector<Fun *>::const_iterator i = cw.for_macro_sep_c->macro_funs.begin(), 
               e = cw.for_macro_sep_c->macro_funs.end(); i != e; ++i)
        {
          cw << "  (s \"" << ~(*i)->uniq_name() <<  "\")\n";
        }
        cw << "))\n";
      }

      unsigned syntaxes_size = cw.for_macro_sep_c->syntaxes.size();
      if (syntaxes_size > 0) {
        cw << "(var _syntaxes_size (unsigned) :(__visibility__ (s protected)) " << syntaxes_size << ")\n";
        cw << "(.struct _syntaxes ((.ptr (char :const)) str) ((.ptr (struct UnmarkedSyntax)) syn))\n";
        cw << "(var _syntaxes (.array (struct _syntaxes) " << syntaxes_size << ") :(__visibility__ (s protected)) (.\n";
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

  Environ * temp_environ(const Environ * env0) {
    Environ * env = new Environ(env0->new_scope());
    env->top_level_symbols = NULL;
    return env;
  }

  Environ * new_scope(const Environ * env0, const Syntax * where) {
    Environ * env = new Environ(env0->new_scope());
    if (where) {
      TopLevelSymbol * sym = env->symbols.lookup<TopLevelSymbol>(where);
      env->where = sym;
    }
    return env;
  }

  typedef const ::Syntax Syntax;
  typedef Syntax UnmarkedSyntax;
  Syntax * replace(const UnmarkedSyntax * p, void * match, Mark * mark);

  int symbol_exists(Syntax * sym, Syntax * where, Mark * mark, Environ * env) {
    //printf("symbol_exists %s in %s\n", ~sym->to_string(), ~where->to_string());
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
      SymbolNode * front = NULL, * back = NULL;
      if (const StructUnion * t = dynamic_cast<const StructUnion *>(type->unqualified))
        front = t->env.symbols.front, back = NULL;
      else if (const UserType * t = dynamic_cast<const UserType *>(type->unqualified))
        front = t->module->syms.front, back = t->module->syms.back;
      else
        return false;
      if (find_symbol<Symbol>(sym, DEFAULT_NS, front, back, StripMarks))
        return true;
      else
        return false;
    } else {
      return env->symbols.exists(sym);
    }
  }

  int user_type_have_default_constructor(const UserType * ut) {
    bool res = ast::have_default_constructor(ut);
    //printf("HAVE DEFAULT CONSTRUCTOR FOR %s = %d\n", ~ut->full_name(), res);
    return res;
  }

  int user_type_have_copy_constructor(const UserType * ut) {
    return ast::have_copy_constructor(ut);
  }

  int user_type_have_assign(const UserType * ut) {
    return ast::have_assign(ut);
  }
  
  int user_type_have_destructor(const UserType * ut) {
    return ast::have_destructor(ut);
  }

  ModuleBuilderBase * new_module_builder(const Syntax * name, Environ * env) {
    return new ModuleBuilder(name, *env);
  }

  void module_builder_add(ModuleBuilderBase * b0, const Syntax * p) {
    ModuleBuilder * b = static_cast<ModuleBuilder *>(b0);
    //printf("ADDING to %p: %s\n", static_cast<TopLevelSymbol *>(b->module), ~p->to_string());
    b->add_syntax(p);
  }

  const Syntax * module_builder_to_syntax(ModuleBuilderBase * b) {
    return SYN(static_cast<ModuleBuilder *>(b));
  }

  ModuleBuilderBase * new_user_type_builder(Syntax * name, Environ * env) {
    //printf("CREATING NEW USER TYPE BUILDER for \"%s\" in %s env\n", ~name->to_string(), env->special() ? "SPECIAL" : "NORMAL");
    return new UserTypeBuilder(name, *env);
  }

}
