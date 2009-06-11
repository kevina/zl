#include <assert.h>
#include <stdio.h>

#include "ast.hpp"

#include "parse.hpp"
#include "parse_op.hpp"
#include "parse_decl.hpp"
#include "expand.hpp"

#include "ct_value-impl.hpp"
#include "hash-t.hpp"

// from peg.hpp
const Syntax * parse_str(String what, SourceStr str, const Replacements * repls = 0);

// each AST node pushes the result on the top of the stack
//   unless the type is void

#define SYN new Syntax

namespace ast {

  static const Syntax * ID = new Syntax("id");

  //
  //
  //

  struct ASTList : public FakeAST {
    ASTList() : FakeAST("astlist") {}
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      return this;
    }
  };

  template <typename C>
  void add_ast_nodes(Parts::const_iterator i, Parts::const_iterator end, 
                     C & container, AST * (*f)(const Syntax *, Environ &), Environ & env) {
    for (; i != end; ++i) {
      AST * n = f(*i, env);
      if (ASTList * list = dynamic_cast<ASTList *>(n)) {
        add_ast_nodes(list->parse_->args_begin(), list->parse_->args_end(), container, f, env);
      } else {
        container.push_back(n);
      }
    }
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

  VarSymbol * new_var_symbol(SymbolName n, Scope s, 
                             const VarDeclaration * d, TopLevelSymbol * w) 
  {
    if (s == OTHER) {
      bool mangle = n.marks;
      return new OtherVarSymbol(n.name, mangle);
    } else if (s == LEXICAL && (!d || 
                                d->storage_class == VarDeclaration::NONE || 
                                d->storage_class == VarDeclaration::AUTO || 
                                d->storage_class == VarDeclaration::REGISTER)) 
    {
      return new LexicalVarSymbol(n.name);
    } else {
      assert(d);
      bool mangle = s == LEXICAL || n.marks || w || d->storage_class == VarDeclaration::STATIC;
      return new TopLevelVarSymbol(n.name,d,mangle,w);
    }
  }
  
  //
  //
  //

  void Symbol::add_to_env(const SymbolKey & k, Environ & env, Pass pass) const {
    if (pass == AllPasses || pass == FirstPass) {
      env.symbols.add(k, this);
      make_unique(env.symbols.front);
    }
  }

  void TopLevelSymbol::add_to_local_env(const SymbolKey & k, Environ & env) const {
    env.symbols.add(k, this);
  }
  
  void TopLevelSymbol::add_to_top_level_env(const SymbolKey & k, Environ & env) const {
    if (env.temporary()) return;
    // If the symbol already exists in the table, remove it and insert
    // it in the end.  This will happen if a function was declared
    // before it was defined.  Since order matters it's where the
    // function is defined that is important.  Fixme, it not
    // really necessary to do this every time...
    Vector<const TopLevelSymbol *>::iterator 
      i = env.top_level_symbols->begin(),
      e = env.top_level_symbols->end();
    while (i != e) {
      if (*i == this) break;
      ++i;
    }
    if (i != e) {
      env.top_level_symbols->erase(i);
      env.top_level_symbols->push_back(this);
    } else {
      env.top_level_symbols->push_back(this);
      if (num == NPOS)
        assign_uniq_num<TopLevelSymbol>(*env.top_level_symbols);
    }
  }
  
  void TopLevelSymbol::add_to_env(const SymbolKey & k, Environ & env, Pass pass) const {
    if (pass == AllPasses || pass == FirstPass)
      add_to_local_env(k, env);
    if (pass == AllPasses || pass == SecondPass)
      add_to_top_level_env(k, env);
  }

  void NormalLabelSymbol::add_to_env(const SymbolKey & k, Environ & env, Pass) const {
    env.fun_labels.add(k, this);
    make_unique(*env.fun_labels.front);
  }

  //
  //
  //

  struct NoOp : public ASTLeaf {
    NoOp() : ASTLeaf("noop") {}
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(0);
      type = env.void_type();
      return this;
    }
    void compile(CompileWriter & f) {
      f << indent << "noop();\n";
    }
  };

  struct Terminal : public FakeAST {
    Terminal(const Syntax * p) : FakeAST(p->what(), p) {}
    AST * parse_self(const Syntax * p, Environ & env) {abort();}
  };

  struct Generic : public FakeAST {
    Vector<AST *> parts;
    Generic(const Syntax * p) : FakeAST(p->what(), p) {
      for (int i = 0; i != p->num_args(); ++i)
        abort();
        //parts.push_back(new Terminal(p->arg(i)));
    }
    Generic(const Syntax * p, const Vector<AST *> & pts) 
      : FakeAST(p->what(), p), parts(pts) {}
    //AST * part(unsigned i) {return parts[i];}
    AST * parse_self(const Syntax*, Environ&) {abort();}
  };

  struct LStmt : public AST {
    LStmt() : AST("lstmt") {}
    const LabelSymbol * label;
    AST * stmt;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(2);
      SymbolKey n = expand_binding(p->arg(0), LABEL_NS, env);
      label = env.symbols.find<LabelSymbol>(n);
      if (!label) {
        label = new NormalLabelSymbol(n.name);
        env.add(SymbolKey(n, LABEL_NS), label);
      }
      stmt = parse_stmt_decl(p->arg(1), env);
      type = stmt->type;
      return this;
    }
    void compile_prep(CompileEnviron & env) {
      stmt->compile_prep(env);
    }
    void compile(CompileWriter & o) {
      // note: noop() is needed because gcc won't let us put a label
      // before a declaration
      o << adj_indent(-2) << indent << label << ": noop();\n";
      o << stmt;
    }
    void finalize(FinalizeEnviron & env) {
      stmt->finalize(env);
    }
  };

  struct LCStmt : public AST {
    LCStmt() : AST("lcstmt") {}
    AST * exp; 
    AST * stmt;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(2);
      if (p->arg(0)->is_a("case")) {
        exp = parse_exp(p->arg(0)->arg(0), env);
      } else /* default */ {
        exp = NULL;
      }
      stmt = parse_stmt(p->arg(1), env);
      type = stmt->type;
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      if (exp)
        exp->finalize(env);
      stmt->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      stmt->compile_prep(env);
    }
    void compile(CompileWriter & o) {
      if (exp)
        o << adj_indent(-2) << indent << "case " << exp << ":\n";
      else
        o << adj_indent(-2) << indent << "default:\n";
      o << stmt;
    }
  };

  struct Goto : public AST {
    Goto() : AST("goto") {}
    SymbolName label;
    const LabelSymbol * sym;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(1);
      assert(p->arg(0)->is_a("id"));
      label = *p->arg(0)->arg(0);
      sym = env.symbols.find<LabelSymbol>(SymbolKey(label, LABEL_NS));
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
        sym = lookup_symbol<LabelSymbol>(SymbolKey(label, LABEL_NS), 
                                         parse_->arg(0)->arg(0)->str(), env.fun_symbols);
    }
    void compile_prep(CompileEnviron & env) {}
    void compile(CompileWriter & o) {
      o << indent 
        << "goto " << sym << ";\n";
    }
  };
  
  struct LocalLabelDecl : public ASTLeaf {
    LocalLabelDecl() : ASTLeaf("local_label") {}
    const LabelSymbol * label;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(1);
      SymbolKey n = expand_binding(p->arg(0), LABEL_NS, env);
      label = new LocalLabelSymbol(n.name);
      env.add(n, label);
      type = env.void_type();
      return this;
    }
    void compile(CompileWriter & o) {
      o << indent << "__label__ " << label << ";\n";
    }
  };

  //AST * Literal::part(unsigned i) {return new Terminal(parse_->arg(0));}
  
  AST * Literal::parse_self(const Syntax * p, Environ & env) {
    parse_ = p;
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

  AST * FloatC::parse_self(const Syntax * p, Environ & env) {
    parse_ = p;
    assert_num_args(1,2);
    type = env.types.inst(p->num_args() > 1 ? *p->arg(1) : String("double"));
    ct_value_ = new_float_ct_value(p->arg(0), type, env);
    return this;
  }

  void FloatC::compile(CompileWriter & f) {
    ct_value_->compile(f, NULL);
  }

  AST * StringC::parse_self(const Syntax * p, Environ & env) {
    parse_ = p;
    assert_num_args(1,2);
    orig = *p->arg(0);
    type = env.types.inst(".pointer", env.types.ct_const(env.types.inst("char")));
    type = env.types.ct_const(type);
    return this;
  }
  void StringC::compile(CompileWriter & f) {
    f << orig;
  }
  
  AST * CharC::parse_self(const Syntax * p, Environ & env) {
    parse_ = p;
    assert_num_args(1, 2);
    orig = *p->arg(0);
    type = env.types.inst("char");
    type = env.types.ct_const(type);
    return this;
  }
  void CharC::compile(CompileWriter & f) {
    f << orig;
  }

  struct Id : public ASTLeaf {
    Id() : ASTLeaf("id") {}
    //AST * part(unsigned i) {return new Terminal(parse_->arg(0));}
    const VarSymbol * sym;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(1);
      sym = env.symbols.lookup<VarSymbol>(p->arg(0));
      const TopLevelVarSymbol * tl = NULL;
      if (env.deps && (tl = dynamic_cast<const TopLevelVarSymbol *>(sym)))
        env.deps->insert(tl);
      if (sym->ct_value)
        ct_value_ = sym->ct_value;
      type = sym->type;
      lvalue = dynamic_cast<const TopLevelVarSymbol *>(sym) ? 2 : 1;
      return this;
    }
    void compile(CompileWriter & f) {
      f << sym;
    }
  };

  struct If : public AST {
    If() : AST("if") {}
    //AST * part(unsigned i) 
    //  {return i == 0 ? exp : i == 1 ? if_true : i == 2 ? if_false : 0;}
    AST * exp;
    AST * if_true;
    AST * if_false;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(2,3);
      exp = parse_exp(p->arg(0), env);
      if_true = parse_stmt(p->arg(1), env);
      if (p->num_args() == 3) {
        if_false = parse_stmt(p->arg(2), env);
      } else {
        if_false = new NoOp;
      }
      type = env.void_type();
      exp = exp->resolve_to(env.bool_type(), env);
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      exp->finalize(env);
      if_true->finalize(env);
      if_false->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      exp->compile_prep(env);
      if_true->compile_prep(env);
      if_false->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << indent << "if (" << exp << ")\n";
      f << adj_indent(2) << if_true;
      f << indent << "else\n";
      f << adj_indent(2) << if_false;
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
    f << "(" << exp << " ? " << if_true << " : " << if_false << ")";
  }

  AST * EIf::parse_self(const Syntax * p, Environ & env) {
    parse_ = p;
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

  struct Switch : public AST {
    Switch() : AST("switch") {}
    AST * exp;
    AST * body;
    //AST * part(unsigned i) {return i == 0 ? exp : i == 1 ? body : 0;}
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(2);
      exp = parse_exp(p->arg(0), env);
      exp = exp->resolve_to(env.bool_type(), env);  
      body = parse_stmt(p->arg(1), env);
      type = env.void_type();
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
      f << indent << "switch (" << exp << ")\n";
      f << adj_indent(2) << body;
    }
  };


  struct Loop : public AST {
    Loop() : AST("loop") {}
    //AST * part(unsigned i) 
    //  {return body;}
    AST * body;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(1);
      body = parse_stmt(p->arg(0), env);
      type = env.void_type();
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      body->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      body->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << indent << "for (;;)\n";
      f << body;
    }
  };
  
  struct Break : public ASTLeaf {
    Break() : ASTLeaf("break") {}
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(0);
      type = env.void_type();
      return this;
    }
    void compile(CompileWriter & f) {
      f << indent << "break;\n";
    }
  };

  struct Var : public VarDeclaration {
    Var() : VarDeclaration("var"), init(), constructor() {}
    //AST * part(unsigned i) {return new Terminal(parse_->arg(0));}
    SymbolKey name;
    const Syntax * name_p;
    AST * init;
    AST * constructor;
    AST * parse_forward(const Syntax * p, Environ & env, Collect & collect) {
      parse_ = p;
      assert_num_args(2,3);
      name_p = p->arg(0);
      SymbolKey name = expand_binding(name_p, env);
      parse_flags(p);
      sym = new_var_symbol(name, env.scope, this, env.where);
      sym->type = parse_type(p->arg(1), env);
      if (storage_class != EXTERN && sym->type->size() == NPOS)
        throw error(name_p, "Size not known");
      env.add(name, sym);
      collect.push_back(this); // fixme, not always the case
      deps_closed = true;
      type = env.void_type();
      if (dynamic_cast<TopLevelVarSymbol *>(sym))
        return new Empty();
      else
        return this;
    }
    void finish_parse(Environ & env) {
      if (parse_->num_args() > 2) {
        //env.add(name, sym, SecondPass);
        init = parse_exp(parse_->arg(2), env);
        //if (sym->type->is_ref && !init->lvalue) {
        //  temp_exp = init;
        //  SymbolKey name = expand_binding(name_p, env);
        //  
        //  temp_sym = new_var_symbol(...);
        //  init = addrof(temp_sym);
        //}
        init = init->resolve_to(sym->type, env);
        if (storage_class == STATIC && sym->type->read_only && init->ct_value_) {
          sym->ct_value = init->ct_value_;
        }
      }
      if (const UserType * ut = dynamic_cast<const UserType *>(sym->type)) {
        if (find_symbol<Symbol>("_constructor", ut->module->syms)) {
          constructor = parse_stmt(SYN(SYN("member"), 
                                       SYN(ID, SYN(name_p, sym)),
                                       SYN(SYN("call"), SYN(ID, SYN("_constructor")), SYN(SYN("list")))),
                                   env);
        }
      }
      if (TopLevelVarSymbol * tlsym = dynamic_cast<TopLevelVarSymbol *>(sym)) {
        if (init && !init->ct_value_) {
          tlsym->init = parse_stmt(SYN(SYN("assign"), SYN(ID, SYN(sym)), SYN(init)), env);
          init = NULL;
        } else if (constructor) {
          tlsym->init = constructor;
          constructor = NULL;
        }
      }
    }
    AST * parse_self_as_member(const Syntax * p, Environ & env) {
      Collect collect;
      AST * res = parse_forward(p, env, collect);
      //assert(collect.empty());
      return res;
    }
    AST * parse_self(const Syntax * p, Environ & env) {
      Collect collect;
      AST * res = parse_forward(p, env, collect);
      if (!collect.empty())
        finish_parse(env);
      return res;
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
      f << indent;
      write_flags(f);
      StringBuf buf;
      //if (temp_sym) {
      //  c_print_inst->declaration(temp_sym->uniq_name(), *temp_sym->type, buf);
      //  f << " = " << temp_exp << ";\n";
      //}
      c_print_inst->declaration(sym->uniq_name(), *sym->type, buf);
      f << buf.freeze();
      if (init && phase != Forward) {
        if (init->ct_value_) {
          f << " = ";
          init->ct_value_->compile(f, init);
        } else {
          f << " = " << init;
        }
      }
      f << ";\n";
      if (constructor)
        f << constructor;
    }
  };

  struct EStmt : public AST {
    EStmt() : AST("estmt") {}
    EStmt(AST * e) : AST("estmt"), exp(e) {type = exp->type;}
    //AST * part(unsigned i) {return exp;}
    AST * exp;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(1);
      exp = parse_exp(p->arg(0), env);
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
      f << indent << exp << ";\n";
    }
  };

  struct BlockBase : public AST {
    BlockBase(String name, bool ae) : AST(name), as_exp(ae) {}
    bool as_exp;
    //AST * part(unsigned i) {return stmts[i];}
    SymbolTable symbols; // not valid until done parsing block
    Vector<AST *> stmts;
    AST * parse_self(const Syntax * p, Environ & env0) {
      parse_ = p;
      Environ env = env0.new_scope();
      if (p->num_args() > 0) {
        add_ast_nodes(p->args_begin(), p->args_end(), stmts, parse_stmt_decl, env);
      } else {
        stmts.push_back(new NoOp);
      }
      symbols = env.symbols;
      if (as_exp) 
        type = stmts.back()->type;
      else
        type = env.void_type();

      return this;
    }
    void finalize(FinalizeEnviron & env) {
      for (int i = 0; i != stmts.size(); ++i) {
        stmts[i]->finalize(env);
      }
    }
    void compile_prep(CompileEnviron & env) {
      for (int i = 0; i != stmts.size(); ++i) {
        stmts[i]->compile_prep(env);
      }
    }
    void compile(CompileWriter & f) {
      if (as_exp)
        f << "({\n";
      else
        f << indent << "{\n";
      for (int i = 0; i != stmts.size(); ++i) {
        f << adj_indent(2) << stmts[i];
      }
      if (as_exp)
        f << indent << "})";
      else
        f << indent << "}\n";
    }
  };

  struct Block : public BlockBase {
    Block() : BlockBase("block", false) {}
  };

  struct EBlock : public BlockBase {
    EBlock() : BlockBase("eblock", true) {}
  };

  struct Print : public AST {
    Print() : AST("print") {}
    //AST * part(unsigned i) {return exp;}
    AST * exp;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(1);
      exp = parse_exp(p->arg(0), env);
      exp = exp->resolve_to(env.types.inst("int"), env);
      type = env.void_type();
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      exp->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      exp->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << indent << "printf(\"%d\\n\", " << exp << ");\n";
    }
  };

  void check_type(AST * exp, TypeCategory * cat) {
    if (!exp->type->is(cat)) 
      throw error(exp->parse_, "Expected %s type", ~cat->name);
  }
  
  //
  // UnOp
  //

  AST * UnOp::parse_self(const Syntax * p, Environ & env) {
    parse_ = p;
    assert_num_args(1);
    exp = parse_exp(p->arg(0), env);
    resolve(env);
    make_ct_value();
    return this;
  }

  void UnOp::make_ct_value() {}

  void UnOp::finalize(FinalizeEnviron & env) {
    exp->finalize(env);
  }

  void UnOp::compile_prep(CompileEnviron & env) {
    exp->compile_prep(env);
  }

  void UnOp::compile(CompileWriter & f) {
    f << "(" << op << exp << ")";
  }

  const Type * resolve_unop(Environ & env, TypeCategory * cat, AST * & exp) {
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
        throw error(exp->parse_, "Can not be used as lvalue");
      }
      exp = exp->to_effective(env);
      // FIXME: add check for register qualifier
      const TypeSymbol * t = env.types.find(".pointer");
      Vector<TypeParm> p;
      p.push_back(TypeParm(exp->type));
      type = t->inst(p);
    }
    void make_ct_value() {
      if (!exp->ct_value_) {
        if (exp->lvalue > 1) 
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
        throw error(exp->parse_, "Can not be used as lvalue");
      }
      // FIXME: add check for register qualifier
      const TypeSymbol * t = env.types.find(".reference");
      Vector<TypeParm> p;
      p.push_back(TypeParm(exp->type));
      type = t->inst(p);
    }
  };

  struct DeRef : public UnOp {
    DeRef() : UnOp("deref", "*") {}
    void resolve(Environ & env) {
      check_type(exp, POINTER_C);
      exp = exp->to_effective(env);
      const PointerLike * t = dynamic_cast<const PointerLike *>(exp->type->unqualified);
      type = t->subtype;
      lvalue = true;
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
      lvalue = true;
    }
  };

  AST * parse_addrof(const Syntax * p, Environ & env) {
    AddrOf * addrof = new AddrOf;
    addrof->parse_self(p, env);
    DeRef * deref = dynamic_cast<DeRef *>(addrof->exp);
    if (deref) return deref->exp;
    return addrof;
  }

  AST * parse_deref(const Syntax * p, Environ & env) {
    DeRef * deref = new DeRef;
    deref->parse_self(p, env);
    AddrOf * addrof = dynamic_cast<AddrOf *>(deref->exp);
    if (addrof) return addrof->exp;
    return deref;
  }

  AST * to_ref(AST * exp, Environ & env) {
    DeRefRef * deref = dynamic_cast<DeRefRef *>(exp);
    if (deref) return deref->exp;
    AddrOfRef * res = new AddrOfRef();
    res->parse_ = exp->parse_;
    res->exp = exp;
    res->resolve(env);
    return res;
  }

  AST * from_ref(AST * exp, Environ & env) {
    AddrOfRef * addrof = dynamic_cast<AddrOfRef *>(exp);
    if (addrof) return addrof->exp;
    DeRefRef * res = new DeRefRef();
    res->parse_ = exp->parse_;
    res->exp = exp;
    res->resolve(env);
    return res;
  }

  //
  // BinOp
  //

  AST * BinOp::parse_self(const Syntax * p, Environ & env) {
    parse_ = p;
    assert_num_args(2);
    lhs = parse_exp(p->arg(0), env);
    rhs = parse_exp(p->arg(1), env);
    resolve(env);
    make_ct_value();
    return this;
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
    f << "(" << lhs << " " << op << " " << rhs << ")";
  }
  
  struct MemberAccess : public AST {
    MemberAccess() : AST("member") {}
    AST * part(unsigned i) {abort();}
    AST * exp;
    const VarSymbol * sym;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(2);
      exp = parse_exp(p->arg(0), env);
      exp = exp->to_effective(env);
      //printf("::"); p->arg(1)->print(); printf("\n");
      if (!p->arg(1)->is_a("id")) throw error(p->arg(1), "Expected identifier");
      SymbolName id = *p->arg(1)->arg(0);
      const StructUnionT * t = dynamic_cast<const StructUnionT *>(exp->type->unqualified);
      if (!t) throw error(p->arg(0), "Expected struct or union type");
      if (!t->defined) throw error(p->arg(1), "Invalid use of incomplete type");
      sym = t->env->symbols.find<VarSymbol>(id, StripMarks);
      if (!sym)
        throw error(p->arg(1), "\"%s\" is not a member of \"%s\"", 
                    ~id.to_string(), ~t->to_string());
      type = sym->type;
      lvalue = true;
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
      f << exp << "." << sym->uniq_name();
    }
  };

  const Type * resolve_binop(Environ & env, TypeCategory * cat, AST *& lhs, AST *& rhs) {
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
  void resolve_pointer_binop(PointerBinOp op, Environ & env, AST *& lhs, AST *& rhs) {
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
    //throw error(rhs->parse_, "Incompatible pointer types");
  }
  
  const Type * resolve_additive(Environ & env, AST *& lhs, AST *& rhs) {
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
    void resolve(Environ & env) {
      //printf("RESOLVE ASSIGN:: %p %s\n", lhs, ~parse_->to_string());
      //printf("RESOLVE ASSIGN lhs:: %s\n", ~lhs->parse_->to_string());
      //printf("RESOLVE ASSIGN lhs:: %s\n", ~lhs->parse_->sample_w_loc(60));
      env.type_relation->resolve_assign(lhs, rhs, env);
      type = lhs->type;
    }
  };

  struct CompoundAssign : public BinOp {
    CompoundAssign(String name, String op0, BinOp *bop, Environ & env) 
      : BinOp(name, op0), assign(new Assign), binop(bop) 
    {
      parse_ = binop->parse_;
      lhs = binop->lhs;
      rhs = binop->rhs;
      assign->parse_ = parse_;
      assign->lhs = lhs;
      assign->rhs = binop;
      assign->resolve(env);
      type = assign->type;
    }
    Assign * assign;
    BinOp * binop;
    AST * parse_self(const Syntax * p, Environ & env) {abort();}
    void resolve(Environ & env) {abort();}
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

  struct PostIncDec : public AST {
    PostIncDec(String name, String op0) : AST(name), op(op0) {}
    AST * part(unsigned i) {return exp;}
    AST * exp;
    String op;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
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
      f << "(" << exp << " " << op << ")";
    }
  };

  struct PostInc : public PostIncDec {
    PostInc() : PostIncDec("postinc", "++") {}
  };

  struct PostDec : public PostIncDec {
    PostDec() : PostIncDec("postdec", "--") {}
  };

  struct InitList : public AST {
    InitList() : AST("list") {}
    AST * part(unsigned i) {return parts[i];}
    Vector<AST *> parts;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      unsigned num_args = p->num_args();
      parts.reserve(num_args);
      add_ast_nodes(p->args_begin(), p->args_end(), parts, parse_exp, env);
      type = VOID_T;
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
      f << "{";
      for (unsigned i = 0; i != parts.size(); ++i)
        f << parts[i] << ",";
      f << "}";
    }
    virtual AST * resolve_to(const Type * type, Environ & env, TypeRelation::CastType rule) {
      // FIXME: resolve individual components of list
      return this;
    }
  };

  //
  //
  //

  void parse_stmts(const Syntax * parse, Environ & env) {
    for (unsigned i = 0; i < parse->num_args(); ++i) {
      const Syntax * p = parse->arg(i);
      AST * a = parse_top_level(p, env);
      if (dynamic_cast<ASTList *>(a)) {
        parse_stmts(a->parse_, env);
      } else {
        FinalizeEnviron fenv;
        a->finalize(fenv);
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
    for (unsigned i = 0; i < parse->num_args(); ++i) {
      const Syntax * p = parse->arg(i);
      AST * a = parse_top_level_first_pass(p, env, collect);
      if (dynamic_cast<ASTList *>(a)) {
        parse_stmts_first_pass(a->parse_, env, collect);
      } else {
        FinalizeEnviron fenv;
        a->finalize(fenv);
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
    return new Empty();
  }

  //
  //
  //

  AST * parse_module(const Syntax * p, Environ & env0, bool pre_parse = false) {
    assert_num_args(p, 1, 2);
    SymbolName n = *p->arg(0);
    Module * m = NULL;
    if (env0.symbols.exists_this_scope(SymbolKey(n, OUTER_NS))) {
      m = const_cast<Module *>(env0.symbols.lookup<Module>(p->arg(0), OUTER_NS));
    } else {
      m = new Module();
      m->name = n.name;
      m->where = env0.where;
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
    }
    return new Empty();
  }

  AST * pre_parse_module(const Syntax * p, Environ & env) {
    return parse_module(p, env, true);
  }

  struct GatherMarks {
    Vector<const Mark *> marks;
    void stripped_mark(const Mark * m) {marks.push_back(m);}
  };

  AST * parse_add_prop(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    Module * m = dynamic_cast<Module *>(env.where);
    m->add_prop(*p->arg(0), p->arg(1));
    return new Empty();
  }

  AST * parse_export(const Syntax * p, Environ & env) {
    //Module * m = dynamic_cast<Module *>(env.where);
    //m->exports.push_back(flatten(p));
    return new Empty();
  }

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
      if (same_scope) 
        r->imported = cur->imported;
      else
        r->imported = true;
    }
    env.symbols.splice(l.first, l.last);
  }

  AST * parse_import(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    GatherMarks gather;
    const Module * m = lookup_symbol<Module>(p->arg(0), OUTER_NS, env.symbols.front, NULL, 
                                             NormalStrategy, gather);
    import_module(m, env, gather);
    return new Empty();
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

  AST * parse_make_inner_ns(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    SymbolName n = *p->arg(0);
    const InnerNS * ns = new InnerNS(n.name);
    env.add(SymbolKey(n, INNER_NS), ns);
    return new Empty();
  }

  AST * parse_declare_user_type(const Syntax * p, Environ & env) {
    SymbolName name = *p->arg(0);
    UserType * t = dynamic_cast<UserType *>(env.types.inst(SymbolKey(name)));
    if (!(env.symbols.exists_this_scope(SymbolKey(name)) && (t && !t->defined /* FIXME: hack */))) {
      //printf("DECLARE: ADDING SYM %s\n", ~name.to_string());
      UserType * s = new UserType;
      s->category = new TypeCategory(name.name, USER_C);
      add_simple_type(env.types, SymbolKey(name), s);
    } else {
      //printf("DECLARE: SYM ALREADY EXISTS: %s\n", ~name);
      if (name == "_VTable") {
        const Symbol * s = env.symbols.find<Symbol>(SymbolKey(name));
        //abort();
      }
    }
    return new Empty();
  }

  AST * parse_make_user_type(const Syntax * p, Environ & env) {
    assert_num_args(p, 1, 2);
    SymbolName name = *p->arg(0);
    UserType * s;
    if (env.symbols.exists_this_scope(SymbolKey(name))) {
      const Type * t0 = env.types.inst(SymbolKey(name));
      s = const_cast<UserType *>(dynamic_cast<const UserType *>(t0));
      //sym = s->type_symbol;
    } else {
      s = new UserType;
      s->category = new TypeCategory(name.name, USER_C);
      /*sym = */env.types.add_name(SymbolKey(name), new UserTypeSymbol(s));
    }
    if (p->num_args() > 1) {
      s->type = parse_type(p->arg(1), env);
    } else {
      s->type = env.types.inst(SymbolKey(name, TAG_NS));
      //parse_stmt_decl(new Syntax(new Syntax("talias"), p->arg(0), new Syntax(s->type)), env);
    }
    s->module = env.symbols.lookup<Module>(p->arg(0), OUTER_NS);
    s->defined = true;
    s->finalize();
    return new Empty();
  }

  AST * parse_user_type(const Syntax * p, Environ & env) {
    assert_num_args(p, 1, 3);
    SymbolName name = *p->arg(0);
    //printf("PARSING USER TYPE %s\n", ~name);
    if (!env.symbols.exists_this_scope(SymbolKey(name))) {
      //printf("ADDING SYM %s\n", ~name);
      UserType * s = new UserType;
      s->category = new TypeCategory(name.name, USER_C);
      env.types.add_name(SymbolKey(name), new UserTypeSymbol(s));
    } else {
      //printf("SYM ALREADY EXISTS: %s\n", ~name);
      if (name == "_VTable") {
        const Symbol * s = env.symbols.find<Symbol>(SymbolKey(name));
        //abort();
      }
    }
    return parse_module(p, env);
  }

  AST * parse_finalize_user_type(const Syntax * p, Environ & env) {
    assert_num_args(p, 1);
    Module * m = dynamic_cast<Module *>(env.where);
    //printf("FINALIZE USER TYPE %s\n", ~m->name.to_string());
    const Type * t0 = env.types.inst(SymbolKey(m->name));
    UserType * s = const_cast<UserType *>(dynamic_cast<const UserType *>(t0));
    s->module = m;
    s->type = parse_type(p->arg(0), env);
    s->defined = true;
    s->finalize();
    return new Empty();
  }

  struct UserCast : public Symbol {
    const Type * from;
    const Type * to;
    const Symbol * cast_macro;
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

  AST * parse_make_subtype(const Syntax * p, Environ & env) {
    //printf(">>%s\n", ~p->to_string());
    assert_num_args(p, 2, 3);
    const Syntax * parent = p->arg(0);
    const Syntax * up_cast = p->arg(1);

    // FIXME: Add check that we are in a user_type
    Module * m = dynamic_cast<Module *>(env.where);
    UserType * parent_t = const_cast<UserType *>
      (dynamic_cast<const UserType *>(env.types.inst(parent)));
    UserType * child_t  = const_cast<UserType *>
      (dynamic_cast<const UserType *>(env.types.inst(SymbolKey(m->name))));
    UserCast * user_cast = new UserCast;
    user_cast->from = child_t;
    user_cast->to = parent_t;
    user_cast->cast_macro = env.symbols.lookup<Symbol>(up_cast);
    assert(!child_t->parent);
    child_t->parent = parent_t;
    child_t->category = new TypeCategory(child_t->what(), parent_t->category);
    env.symbols.add(SymbolKey("up_cast", CAST_NS), user_cast);
    m->syms = new SymbolNode(SymbolKey("up_cast", CAST_NS), user_cast, m->syms);
    return new Empty();
  }

  static const Syntax * THIS = new Syntax("this");

  AST * parse_member_access(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    AST * exp = parse_exp(p->arg(0), env);
    exp = exp->to_effective(env);
    Syntax * ptr_exp = new Syntax(new Syntax("addrof"), new Syntax(exp));
    if (dynamic_cast<const StructUnionT *>(exp->type->unqualified)) {
      const Syntax * np = new Syntax(p->str(), p->part(0), new Syntax(exp), p->arg(1));
      return (new MemberAccess)->parse_self(np, env);
    } else if (const UserType * t = dynamic_cast<const UserType *>(exp->type->unqualified)) {
      // FIXME: Am I calling partly_expand in the right scope here, or correctly at all
      const Syntax * arg1 = partly_expand(p->arg(1), OtherPos, env, EXPAND_NO_MACRO_CALL);
      const Syntax * call;
      if (arg1->is_a("call")) {
        assert_num_args(arg1, 2);
        const Syntax * n = arg1->arg(0);
        assert(n && n->is_a("id")); // FIXME Error Message
        Syntax * a = new Syntax(*arg1->arg(1));
        a->add_flag(new Syntax(THIS, ptr_exp));
        const Symbol * sym = lookup_symbol<Symbol>(n->arg(0), DEFAULT_NS, t->module->syms, NULL, StripMarks);
        call = new Syntax(arg1->part(0), new Syntax(ID, new Syntax(n->arg(0), sym)), a);
      } else {
        const Syntax * n = arg1;
        assert(n && n->is_a("id")); // FIXME Error Message
        const Symbol * sym = lookup_symbol<Symbol>(n->arg(0), DEFAULT_NS, t->module->syms, NULL, StripMarks);
        Syntax * c = new Syntax(ID, new Syntax(n->arg(0), sym));
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

  AST * parse_imember_access(const Syntax * p, Environ & env) {
    assert_num_args(p, 2);
    AST * exp = parse_exp(p->arg(0), env);
    exp = exp->to_effective(env);
    //printf("::"); p->arg(1)->print(); printf("\n");
    if (!p->arg(1)->is_a("id")) throw error(p->arg(1), "Expected Identifier");
    SymbolName id = *p->arg(1)->arg(0);
    const UserType * t = dynamic_cast<const UserType *>(exp->type->unqualified);
    if (!t) throw error(p->arg(0), "Expected user type but got ??");
    if (!t->defined) throw error(p->arg(1), "Invalid use of incomplete type");
    exp->type = change_unqualified(exp->type, t->type);
    Syntax * res = new Syntax(new Syntax("member"),
                              new Syntax(exp),
                              p->arg(1));
    return (new MemberAccess)->parse_self(res, env);
  };

  AST * parse_memberdecl(const Syntax * p, Environ & env0) {
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
    return new Empty();
  }

  //
  //
  //

  AST * cast_up(AST * exp, const Type * type, Environ & env) {
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
    const Type * to_ptr = env.types.inst(".pointer", to_qualified);
    NoOpGather gather;
    //UserCastCompare cmp(from_base, from_base->parent);
    const UserCast * cast = lookup_symbol<UserCast>(SymbolKey("up_cast", CAST_NS), exp->parse_->str(), 
                                                    from_base->module->syms);//, NULL, NormalStrategy, gather, cmp);
    const Syntax * p = new Syntax(new Syntax("call"), 
                                  new Syntax(new Syntax("id"), new Syntax(cast->cast_macro)),
                                  new Syntax(new Syntax("list"), new Syntax(exp)));
    AST * res = parse_exp(p, env);
    res = res->resolve_to(to_ptr, env);
    return cast_up(res, type, env);
  }

  AST * cast_down(AST * exp, const Type * type, Environ & env) {
    abort();
    //if (exp->type == type)
    //  return exp;
    //const UserType * ?? = dynamic_cast<const UserType *>(type);
    //const Type * ???_unqualified = ??->parent;
    //call cast macro;
    //compile to ast;
    //return it;
  }

  AST * subtype_cast(AST * exp, const UserType * type, Environ & env) {
    const Type * have = exp->type->effective->unqualified;
    const Type * need = type->unqualified;
    if (have->is(need->category))
      return cast_up(exp, type, env);
    else if (need->is(have->category))
      return cast_down(exp, type, env);
    else
      throw error(exp->parse_, "Invalid cast from \"%s\" to \"%s\"",
                  ~have->to_string(), ~need->to_string());
  }

  void Cast::finalize(FinalizeEnviron & env) {
    exp->finalize(env);
  }

  void Cast::compile_prep(CompileEnviron & env) {
    exp->compile_prep(env);
  }
  void Cast::compile(CompileWriter & f) {
    StringBuf buf;
    c_print_inst->to_string(*type, buf);
    f << "((" << buf.freeze() << ")" << exp << ")";
  };

  AST * parse_cast(const Syntax * p, Environ & env, TypeRelation::CastType ctype) {
    assert_num_args(p, 2);
    const Syntax * t = p->arg(0);
    if (t->is_a("(type)"))
      t = t->arg(0);
    Type * type = parse_type(t, env);
    AST * exp = parse_exp(p->arg(1), env);
    AST * res = env.type_relation->resolve_to(exp, type, env, ctype);
    if (dynamic_cast<Cast *>(res))
      res->parse_ = p;
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
      return new Terminal(parse_->arg(0));
    } else if (i == 1) {
      return new Generic(parse_->arg(1));
    } else {
      return body;
    }
  }
#endif

  Fun * parse_fun_forward(const Syntax * p, Environ & env, Collect & collect) {
    assert_num_args(p,3,4);

    //printf("%s\n", ~p->to_string());

    SymbolKey name = expand_binding(p->arg(0), env);
    
    bool previous_declared = env.symbols.exists_this_scope(name);
    VarSymbol * sym;
    TopLevelSymbol * tlsym = NULL;
    Fun * f = NULL;
    if (previous_declared) {
      sym = const_cast<VarSymbol *>(env.symbols.find<VarSymbol>(name));
      tlsym = dynamic_cast<TopLevelSymbol *>(sym);
      f = const_cast<Fun *>(dynamic_cast<const Fun *>(tlsym->decl));
      if (f->body) goto foo; // FIXME: This is a hack, to allow
                             // functions to shadow an imported
                             // function.
      if (p->num_args() > 3)
        f->parse_forward_i(p, env, collect);
    } else {
    foo:
      f = new Fun;
      f->name = name;
      f->sym = sym = new_var_symbol(name, env.scope, f, env.where);
      tlsym = dynamic_cast<TopLevelSymbol *>(sym);
      tlsym->add_to_local_env(name, env);
      f->parse_forward_i(p, env, collect);
    }
    return f;
  }

  AST * parse_fun(const Syntax * p, Environ & env) {
    Collect collect;
    Fun * f = parse_fun_forward(p, env, collect);
    if (!collect.empty())
      f->finish_parse(env);
    return f;
  }
  
  AST * Fun::parse_forward_i(const Syntax * p, Environ & env0, Collect & collect) {
    parse_ = p;

    TopLevelSymbol * tlsym = dynamic_cast<TopLevelSymbol *>(sym);    

    parse_flags(p);

    if (p->flag("__need_snapshot"))
      env_ss = *env0.top_level_environ;

    // FIXME: is this necessary/correct for a new frame to be created
    //        to expand/parse the _paramaters_.  Of cource is needed for
    //        the body that that is done in parse_body.
    Environ env = env0.new_frame();
    env.where = tlsym;
    env.deps = &deps_;
    env.for_ct = &for_ct_;

    parms = expand_fun_parms(p->arg(1), env);

    ret_type = parse_type(p->arg(2), env);
    type = ret_type;
    sym->type = env.function_sym()->inst(env.types, this);

    body = 0;
    if (p->num_args() > 3) {
      collect.push_back(this);
    } else {
      tlsym->add_to_top_level_env(name, env0);
      symbols = env.symbols;
    }

    //sym->value = this;

    return this;
  }

  void Fun::finish_parse(Environ & env0) {
    assert(parse_->num_args() > 3);

    TopLevelSymbol * tlsym = dynamic_cast<TopLevelSymbol *>(sym);

    Environ env = env0.new_frame();
    env.where = tlsym;
    env.deps = &deps_;
    env.for_ct = &for_ct_;
    env.frame->return_type = ret_type;

    for (Tuple::Parms::const_iterator i = parms->parms.begin(), e = parms->parms.end();
         i != e; ++i)
    {
      SymbolName n = i->name;
      VarSymbol * sym = new_var_symbol(n);
      i->sym = sym;
      sym->type = i->type;
      env.add(n, sym);
      //env.symbols.add(n, sym);
    }

    body = dynamic_cast<Block *>(parse_stmt(parse_->arg(3), env));
    assert(body); // FiXME

    //printf("FUN DEPS: %s %d\n", ~name, deps_.size());
    //for (Deps::iterator i = deps_.begin(), e = deps_.end(); i != e; ++i)
    //  printf("  %s\n", ~(*i)->name);
    //printf("---\n");

    tlsym->add_to_top_level_env(name, env0);
    symbols = env.symbols;
  }

  void Fun::finalize(FinalizeEnviron & env0) {
    if (body) {
      FinalizeEnviron env = env0;
      env.fun_symbols = symbols.front;
      body->finalize(env);
    }
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
      f << "struct EnvironSnapshot * " << sym->uniq_name() << '$' << "env_ss" << ";\n";
    }
    write_flags(f);
    StringBuf buf;
    c_print_inst->declaration(sym->uniq_name(), *sym->type, buf);
    f << buf.freeze();
    if (body && phase != Forward) {
      f.in_fun = this;
      f << "\n" << body;
      f.in_fun = NULL;
    } else {
      if (static_constructor)
        f << " __attribute__((constructor))";
      f << ";\n";
    }
  }
  
  struct Return : public AST {
    AST * what;
    Return() : AST("return") {}
    AST * part(unsigned i) {return what;}
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(1);
      what = parse_exp(p->arg(0), env);
      what = what->resolve_to(env.frame->return_type, env);
      type = env.void_type();
      return this;
    }
    void finalize(FinalizeEnviron & env) {
      what->finalize(env);
    }
    void compile_prep(CompileEnviron & env) {
      what->compile_prep(env);
    }
    void compile(CompileWriter & f) {
      f << indent << "return " << what << ";\n";
    }
  };

  struct Call : public AST {
    Call() : AST("call") {} 
    //AST * part(unsigned i) {return i == 0 ? lhs : new Generic(parse_->arg(1), parms);}
    AST * lhs;
    Vector<AST *> parms;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(2);
      lhs = parse_exp(p->arg(0), env);
      p = p->arg(1);
      add_ast_nodes(p->args_begin(), p->args_end(), parms, parse_exp, env);
      const Function * ftype = dynamic_cast<const Function *>(lhs->type);
      if (!ftype) {
        if (const Pointer * t = dynamic_cast<const Pointer *>(lhs->type))
          ftype = dynamic_cast<const Function *>(t->subtype);
      }
      if (!ftype)
        throw error (lhs->parse_, "Expected function type");
      type = ftype->ret;
      lvalue = type->addressable;
      if (!ftype->parms->vararg && parms.size() != ftype->parms->parms.size()) 
        throw error(parse_->arg(1), 
                    "Wrong number of parameters, expected %u but got %u when calling %s",
                    ftype->parms->parms.size(), parms.size(), ~ftype->what());
      else if (ftype->parms->vararg && parms.size() < ftype->parms->parms.size())
        throw error(parse_->arg(1),
                    "Not enough parameters, expected at least %u but got %u when calling %s",
                    ftype->parms->parms.size(), parms.size(), ~ftype->what());
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
      f << lhs << "(";
      int i = 0;
      if (i != parms.size()) while (true) {
        parms[i]->compile(f);
        ++i;
        if (i == parms.size()) break;
        f.printf(", ");
      }
      f << ")";
    }
  };

  struct TypeDeclaration : public Declaration {
    TypeDeclaration(String n) : Declaration(n) {}
  };

  struct TypeAlias : public TypeDeclaration {
    TypeAlias() : TypeDeclaration("talias") {}
    TypeSymbol * name_sym;
    const Type * type;
    AST * part(unsigned i) {abort();}
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(2);
      SymbolKey n = expand_binding(p->arg(0), DEFAULT_NS, env);
      type = parse_type(p->arg(1), env);
      name_sym = add_simple_type(env.types, n, new AliasT(type), this, env.where);
      return new Empty();
    }
    void finalize(FinalizeEnviron &) {}
    void compile_prep(CompileEnviron &) {}
    void compile(CompileWriter & f, Phase phase) const {
      if (phase == Body) return;
      f << indent << "typedef ";
      StringBuf buf;
      c_print_inst->declaration(name_sym->uniq_name(), *type, buf);
      f << buf.freeze();
      f << ";\n";
    }
  };

  struct StructUnion : public TypeDeclaration {
    enum Which {STRUCT, UNION} which;
    struct Body : public FakeAST {
      Body(Which w) : FakeAST(w == STRUCT ? "struct_body" : "union_body") {}
      AST * part(unsigned i) {return members[i];}
      Vector<AST *> members;
      AST * parse_self(const Syntax * p, Environ & env) {
        add_ast_nodes(p->parts_begin(), p->parts_end(), members, parse_member, env);
        return this;
      }
    };
    StructUnion(Which w) : TypeDeclaration(w == STRUCT ? "struct" : "union"), which(w), env(OTHER)  {}
    AST * part(unsigned i) {return 0; /* FIXME */}
    const TypeSymbol * sym;
    Body * body;
    Environ env;
    AST * parse_self(const Syntax * p, Environ & env0) {
      parse_ = p;
      assert(p->is_a(what()));
      const Syntax * name = p->arg(0);
      env = env0.new_scope();
      env.scope = OTHER;
      if (p->num_args() > 1) {
        body = new Body(which);
        body->parse_self(p->arg(1), env);
      } else {
        body = NULL;
      }
      StructUnionT * s;
      if (env0.symbols.exists_this_scope(name, TAG_NS)) {
        const Type * t0 = env0.types.inst(name, TAG_NS);
        s = const_cast<StructUnionT *>(dynamic_cast<const StructUnionT *>(t0));
        sym = s->type_symbol;
      } else {
        SymbolKey n = expand_binding(name, TAG_NS, env);
        if (which == STRUCT) s = new StructT(n);
        else                 s = new UnionT(n);
        sym = add_simple_type(env0.types, n, s, this, env.where);
      }
      if (body)
        for (unsigned i = 0; i != body->members.size(); ++i) {
          Var * v = dynamic_cast<Var *>(body->members[i]);
          assert(v);
          s->members.push_back(Member(v->sym));
        }
      //StringBuf type_name;
      //type_name << "struct " << what();
      sym->decl = this;
      s->env = &env;
      //if (s->members.empty())
      //  fprintf(stderr, "Warning: %s\n", error(p, "Empty Struct Currently Unsupported")->message().c_str());
      s->finalize();
      return new Empty();
    }
    void finalize(FinalizeEnviron & e) {
      if (body)
        for (int i = 0; i != body->members.size(); ++i) {
          body->members[i]->finalize(e);
        }
    }
    void compile_prep(CompileEnviron & e) {
      if (body)
        for (int i = 0; i != body->members.size(); ++i) {
          body->members[i]->compile_prep(e);
        }
    }
    void compile(CompileWriter & f, Phase phase) const {
      if (!body && phase == Declaration::Body) return;
      f << indent << what() << " " << sym;
      if (body && phase != Forward) {
        f << " {\n";
        for (int i = 0; i != body->members.size(); ++i) {
          f << adj_indent(2) << body->members[i];
        }
        f << indent << "}";
      } 
      f << ";\n";
    }
  };

  struct Struct : public StructUnion {
    Struct() : StructUnion(STRUCT) {}
  };

  struct Union : public StructUnion {
    Union() : StructUnion(UNION) {}
  };

  struct Enum : public TypeDeclaration {
    Enum() : TypeDeclaration("enum") {}
    const TypeSymbol * sym;
    const Syntax * body;
    struct Member {
      const Syntax * parse;
      VarSymbol * sym;
      CT_Value<target_int> ct_value;
      Member(const Syntax * p, VarSymbol * sym, int v) : parse(p), sym(sym), ct_value(v) {}
    };
    Vector<Member> members;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      SymbolName name = *p->arg(0);
      EnumT * t0;
      if (env.symbols.exists_this_scope(SymbolKey(name, TAG_NS))) {
        t0 = (const_cast<EnumT *>(dynamic_cast<const EnumT *>
                                  (env.types.inst(SymbolKey(name, TAG_NS)))));
        sym = t0->type_symbol;
      } else {
        t0 = new EnumT(name.name);
        t0->exact_type = env.types.inst("int")->exact_type;
        sym = add_simple_type(env.types, SymbolKey(name, TAG_NS), t0, this, env.where);
      }
      body = NULL;
      if (p->num_args() > 1) {
        Vector<TypeParm> q_parms;
        q_parms.push_back(TypeParm(QualifiedType::CT_CONST));
        q_parms.push_back(TypeParm(t0));
        const Type * t = env.types.find(".qualified")->inst(q_parms);
        int val = 0;
        const Syntax * arg1 = body = p->arg(1);
        members.reserve(arg1->num_args());
        for (unsigned i = 0; i != arg1->num_args(); ++i) {
          const Syntax * arg = arg1->arg(i);
          if (arg->num_parts() > 1) {
            AST * e = parse_exp(arg->part(1), env);
            e = e->resolve_to(env.types.inst("int"), env);
            val = e->ct_value<target_int>();
          }
          SymbolName n = *arg->part(0);
          Member mem(arg1, new_var_symbol(n), val);
          VarSymbol * sym = mem.sym;
          val++;
          sym->type = t;
          members.push_back(mem);
          sym->ct_value = &members.back().ct_value;
          sym->add_to_env(n, env);
        }
      }
      sym->decl = this;
      t0->finalize();
      return new Empty();
    }
    void finalize(FinalizeEnviron &) {}
    void compile_prep(CompileEnviron &) {}
    void compile(CompileWriter & f, Phase phase) const {
      if (!body && phase == Body) return;
      f << indent << what() << " " << sym;
      if (body && phase != Forward) {
        f << "{\n";
        for (int i = 0; i != members.size(); ++i) {
          f << adj_indent(2) << indent << members[i].sym << " = " << members[i].ct_value.val;
          if (i == members.size())
            f << "\n";
          else
            f << ",\n";
        }
        f << indent << "}";
      }
      f << ";\n";
    }
  };

  struct SizeOf : public ASTLeaf {
    SizeOf() : ASTLeaf("sizeof") {}
    const Type * sizeof_type;
    AST * parse_self(const Syntax * p, Environ & env);
    void finalize(FinalizeEnviron &) {}
    void compile(CompileWriter & f) {
      f << sizeof_type->size();
    }
  };
  
  AST * SizeOf::parse_self(const Syntax * p, Environ & env) {
    parse_ = p;
    assert_num_args(1);
    if (p->arg(0)->is_a("(type)")) {
      sizeof_type = parse_type(p->arg(0)->arg(0), env);
    } else {
      AST * exp = parse_exp(p->arg(0), env);
      sizeof_type = parse_type(new Syntax(new Syntax(".typeof"), new Syntax(exp)), env);
    }
    type = env.types.ct_const(env.types.inst(".size"));
    ct_value_ = new CT_Value<target_size_t>(sizeof_type->size());
    return this;
  }

  //

  struct SyntaxC : public ASTLeaf {
    SyntaxC() : ASTLeaf("syntax") {}
    static Vector<const Syntax *> keep_me;
    const Syntax * syn;
    unsigned syn_num;
    AST * parse_self(const Syntax * p, Environ & env);
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

  AST * SyntaxC::parse_self(const Syntax * p, Environ & env) {
    parse_ = p;
    assert_num_args(1);
    syn = parse_syntax_c(p);
    syn_num = (unsigned)-1;
    *env.for_ct = true;
    type = env.types.inst(".pointer", env.types.inst("UnmarkedSyntax"));
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
      f.printf("_syntaxes[%d].syn", syn_num);
    } else if (f.for_compile_time()) 
      f.printf("(struct UnmarkedSyntax *)%p", syn); 
    else
      f.printf("(struct UnmarkedSyntax *)0");
  }

  Vector<const Syntax *> SyntaxC::keep_me;

  struct EnvironSnapshot : public ASTLeaf {
    EnvironSnapshot() : ASTLeaf("environ_snapshot") {}
    SymbolNode * env_ss;
    AST * parse_self(const Syntax * p, Environ & env) {
      parse_ = p;
      assert_num_args(0);
      env_ss = *env.top_level_environ;
      type = env.types.inst(".pointer", env.types.inst("EnvironSnapshot"));
      type = env.types.ct_const(type);
      *env.for_ct = true;
      return this;
    }
    void compile(CompileWriter & f) {
      if (f.in_fun && f.in_fun->env_ss) 
        f.printf("%s$env_ss", ~f.in_fun->sym->uniq_name());
      else if (f.for_compile_time())
        f.printf("(struct EnvironSnapshot *)%p", env_ss); 
      else 
        f.printf("(struct EnvironSnapshot *)0");
    }
  };

  //

  AST * parse_top(const Syntax * p) {
    Environ env(TOPLEVEL);
    return parse_top(p, env);
  }

  AST * try_ast(const Syntax * p, Environ & env);
  AST * try_decl(const Syntax * p, Environ & env);
  AST * try_decl_first_pass(const Syntax * p, Environ & env, Collect & collect);
  AST * try_stmt(const Syntax * p, Environ & env);
  AST * try_exp(const Syntax * p, Environ & env);

  AST * parse_top_level(const Syntax * p, Environ & env) {
    AST * res;
    p = partly_expand(p, TopLevel, env);
    //printf("Parsing top level:\n  %s\n", ~p->to_string());
    res = try_ast(p, env);
    if (res) return res;
    res = try_decl(p, env);
    if (res) return res;
    throw error (p, "Unsupported primative at top level:: %s", ~p->what());
    //throw error (p, "Expected top level expression.");
  }
    
  AST * parse_top_level_first_pass(const Syntax * p, Environ & env, Collect & collect) {
    AST * res;
    p = partly_expand(p, TopLevel, env);
    //printf("Parsing top level fp:\n  %s\n", ~p->to_string());
    res = try_ast(p, env);
    if (res) return res;
    res = try_decl_first_pass(p, env, collect);
    if (res) return res;
    throw error (p, "Unsupported primative at top level: %s", ~p->what());
    //throw error (p, "Expected top level expression.");
  }

  AST * parse_member(const Syntax * p, Environ & env) {
    AST * res;
    p = partly_expand(p, FieldPos, env);
    res = try_ast(p, env);
    if (res) return res;
    //printf("Parsing member:\n  %s\n", ~p->to_string());
    //res = try_decl(p, env);
    String what = p->what().name;
    if (what == "@")   return (new ASTList)->parse_self(p, env);
    if (what == "var") return (new Var)->parse_self_as_member(p, env);
    //if (res) return res;
    throw error (p, "Unsupported primitive inside a struct or union: %s", ~p->what());
    //throw error (p, "Expected struct or union member.");
  }

  const Syntax * pre_parse_decl(const Syntax * p, Environ & env) {
    String what = p->what().name;
    //printf("PRE PARSING %s\n", ~p->to_string());
    if (what == "struct")  (new Struct)->parse_self(p, env);
    if (what == "union")   (new Union)->parse_self(p, env);
    if (what == "enum")    (new Enum)->parse_self(p, env);
    if (what == "talias")  (new TypeAlias)->parse_self(p, env);
    if (what == "module")         pre_parse_module(p, env);
    if (what == "make_user_type") parse_make_user_type(p, env);
    if (what == "user_type")          parse_user_type(p, env);
    if (what == "finalize_user_type") parse_finalize_user_type(p, env);
    if (what == "make_subtype") parse_make_subtype(p, env);
    if (what == "declare_user_type") parse_declare_user_type(p, env);
    return p;
  }

  AST * parse_stmt(const Syntax * p, Environ & env) {
    AST * res;
    //printf("1>%s\n", ~p->to_string());
    p = partly_expand(p, StmtPos, env);
    //printf("2>%s\n", ~p->to_string());
    res = try_ast(p, env);
    if (res) return res;
    //printf("Parsing stmt:\n  %s\n", ~p->to_string());
    res = try_stmt(p, env);
    if (res) return res;
    res = try_exp(p, env);
    if (res) return new EStmt(res);
    //throw error (p, "Unsupported primative at statement position: %s", ~p->name);
    throw error (p, "Expected statement in: %s.", ~p->to_string());
  }

  AST * parse_stmt_decl(const Syntax * p, Environ & env) {
    AST * res;
    p = partly_expand(p, StmtDeclPos, env);
    res = try_ast(p, env);
    if (res) return res;
    //printf("Parsing stmt decl:\n  %s\n", ~p->to_string());
    res = try_decl(p, env);
    if (res) return res;
    res = try_stmt(p, env);
    if (res) return res;
    res = try_exp(p, env);
    if (res) return new EStmt(res);
    //throw error (p, "Unsupported primative at statement position: %s", ~p->name);
    p->print(); printf("\n");
    throw error (p, "Expected statement or declaration.");
  }

  AST * parse_exp(const Syntax * p, Environ & env) {
    AST * res;
    p = partly_expand(p, ExpPos, env);
    res = try_ast(p, env);
    if (res) return res;
    //printf("parsing expression: %s\n", ~p->to_string());
    res = try_exp(p, env);
    if (res) return res;
    //abort();
    throw error (p, "Unsupported primative at expression position: %s", ~p->what());
    //throw error (p, "Expected expression.");
  }

  AST * try_ast(const Syntax * p, Environ & env) {
    if (p->entity()) {
      AST * ast = dynamic_cast<AST *>(p->entity());
      if (ast) return ast;
      Error * err = dynamic_cast<Error *>(p->entity());
      if (err) throw err;
      abort(); // FIXME Error message
    }
    return 0;
  }

  AST * try_decl_common(const Syntax * p, Environ & env) {
    String what = p->what().name;
    if (what == "@")       return (new ASTList)->parse_self(p, env);
    if (what == "struct")  return (new Struct)->parse_self(p, env);
    if (what == "union")   return (new Union)->parse_self(p, env);
    if (what == "enum")    return (new Enum)->parse_self(p, env);
    if (what == "talias")  return (new TypeAlias)->parse_self(p, env);
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

  AST * try_decl_first_pass(const Syntax * p, Environ & env, Collect & collect) {
    String what = p->what().name;
    if (what == "var")     return (new Var)->parse_forward(p, env, collect);
    if (what == "fun" )    return parse_fun_forward(p, env, collect);
    return try_decl_common(p, env);
  }

  AST * try_decl(const Syntax * p, Environ & env) {
    String what = p->what().name;
    if (what == "var")     return (new Var)->parse_self(p, env);
    if (what == "fun" )    return parse_fun(p, env);
    return try_decl_common(p, env);
  }

  AST * try_stmt(const Syntax * p, Environ & env) {
    String what = p->what().name;
    if (what == "goto")    return (new Goto)->parse_self(p, env);
    if (what == "lstmt")   return (new LStmt)->parse_self(p, env);
    if (what == "lcstmt")  return (new LCStmt)->parse_self(p, env);
    if (what == "if")      return (new If)->parse_self(p, env);
    if (what == "loop")    return (new Loop)->parse_self(p, env);
    if (what == "switch")  return (new Switch)->parse_self(p, env);
    if (what == "break")   return (new Break)->parse_self(p, env);
    if (what == "block")   return (new Block)->parse_self(p, env);
    if (what == "print")   return (new Print)->parse_self(p, env);
    if (what == "noop")    return (new NoOp)->parse_self(p, env);
    if (what == "return")  return (new Return)->parse_self(p, env);
    return 0;
  }

  AST * try_exp(const Syntax * p, Environ & env) {
    String what = p->what().name;
    if (what == "@")       return (new ASTList)->parse_self(p, env);
    if (what == "id")      return (new Id)->parse_self(p, env);
    if (what == "literal") return (new Literal)->parse_self(p, env);
    if (what == "float")   return (new FloatC)->parse_self(p, env);
    if (what == "char")    return (new CharC)->parse_self(p, env);
    if (what == "string")  return (new StringC)->parse_self(p, env);
    if (what == "eif")     return (new EIf)->parse_self(p, env);
    if (what == "assign")  return (new Assign)->parse_self(p, env);
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
    if (what == "complmnt")return (new Compliment)->parse_self(p, env);
    if (what == "addrof")  return parse_addrof(p, env);
    if (what == "deref")   return parse_deref(p, env);
    if (what == "member")  return parse_member_access(p, env);
    if (what == "imember") return parse_imember_access(p, env);
    if (what == "call")    return (new Call)->parse_self(p, env);
    if (what == "eblock")  return (new EBlock)->parse_self(p, env);
    if (what == "sizeof")  return (new SizeOf)->parse_self(p, env);
    if (what == "cast")    return parse_cast(p, env, TypeRelation::Explicit);
    if (what == "icast")   return parse_cast(p, env, TypeRelation::Implicit);
    if (what == "list")    return (new InitList)->parse_self(p, env);
    if (what == "empty")   return (new Empty)->parse_self(p, env);
    if (what == "syntax")           return (new SyntaxC)->parse_self(p, env);
    if (what == "raw_syntax")       return (new SyntaxC)->parse_self(p, env);
    if (what == "environ_snapshot") return (new EnvironSnapshot)->parse_self(p, env);
    if (strcmp(what + what.size()-3, "_eq") == 0) {
      // This is a bit of a hack to handle op_eq cases (ie += -=, etc)
      StringBuf buf(what, what.size()-3);
      AST * ast = try_exp(new Syntax(p->str(), new Syntax(buf.freeze()), p->arg(0), p->arg(1)), env);
      if (!ast) return 0;
      BinOp * binop = dynamic_cast<BinOp *>(ast);
      StringBuf op;
      op << binop->op << "=";
      return new CompoundAssign(what, op.freeze(), binop, env);
    }
    return 0;
  }

  //
  // VarDeclaration methods
  //

  void VarDeclaration::parse_flags(const Syntax * p) {
    storage_class = NONE;
    //printf("PARSING FLAGS OF %s\n", ~p->to_string());
    if (p->flag("auto")) storage_class = AUTO;
    else if (p->flag("static")) storage_class = STATIC;
    else if (p->flag("extern")) storage_class = EXTERN;
    else if (p->flag("register")) storage_class = REGISTER;
    inline_ = false;
    if (p->flag("inline")) inline_ = true;
    ct_callback = false;
    if (p->flag("__ct_callback")) ct_callback = true;
    for_ct_ = ct_callback;
    if (p->flag("__for_ct")) for_ct_ = true;
    deps_closed = ct_callback;
    static_constructor = false;
    if (p->flag("__static_constructor")) static_constructor = true;
  }
  
  void VarDeclaration::write_flags(CompileWriter & f) const {
    StorageClass sc = storage_class;
    if (f.for_compile_time() && dynamic_cast<TopLevelVarSymbol *>(sym)) {
      if (sym->ct_ptr)
        sc = EXTERN;
      else if (sc == STATIC)
        sc = NONE;
    }
    switch (sc) {
    case AUTO: 
        f << "auto "; break;
    case STATIC: 
      f << "static "; break;
    case EXTERN: 
      f << "extern "; break;
    case REGISTER: 
      f << "register "; break;
    default:
      break;
    }
    if (inline_)
        f << "inline ";
  }
  
  void VarDeclaration::calc_deps_closure() const {  
    deps_closed = true;
    for (unsigned i = 0, sz = deps_.size(); i < sz; ++i) {
      const VarDeclaration * d = deps_[i]->decl;
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

  void compile(const Vector<const TopLevelSymbol *> & syms, CompileWriter & cw) {

    static const char * prelude = 
      "static inline void noop() {}\n"
      "\n";
    cw << prelude;

    Vector<const TopLevelSymbol *>::const_iterator i, e = syms.end();
    Vector<AST *> init, cleanup;
    const TopLevelVarSymbol * tl = NULL;

    cw << "/* type decls */\n";

    for (i = syms.begin(); i != e; ++i) {
      if (const TypeDeclaration * d = dynamic_cast<const TypeDeclaration *>((*i)->decl))
        d->compile(cw, Declaration::Forward);
    }

    cw << "/* type definitions */\n";

    for (i = syms.begin(); i != e; ++i) {
      if (const TypeDeclaration * d = dynamic_cast<const TypeDeclaration *>((*i)->decl))
        d->compile(cw, Declaration::Body);
    }

    if (cw.for_macro_sep_c) {

      cw << "/* macro sep. c. stuff */\n";

      for (i = syms.begin(); i != e; ++i) {
        if (const VarDeclaration * d = dynamic_cast<const VarDeclaration *>((*i)->decl)) {
          const_cast<VarDeclaration *>(d)->compile_prep(cw); // evil I know...
        }
      }
      
      unsigned macro_funs_size = cw.for_macro_sep_c->macro_funs.size();
      cw << "unsigned _macro_funs_size = " << macro_funs_size << ";\n";
      if (macro_funs_size  > 0 ) {
        cw << "const char * _macro_funs[" << macro_funs_size << "] = {\n";
        for (Vector<Fun *>::const_iterator i = cw.for_macro_sep_c->macro_funs.begin(), 
               e = cw.for_macro_sep_c->macro_funs.end(); i != e; ++i)
        {
          cw.printf("  \"%s\"%s\n", ~(*i)->sym->uniq_name(), i + 1 != e ? "," : "");
        }
        cw << "};\n";
      }

      unsigned syntaxes_size = cw.for_macro_sep_c->syntaxes.size();
      if (syntaxes_size > 0) {
        cw << "unsigned _syntaxes_size = " << syntaxes_size << ";\n";
        cw << "struct {const char * str; struct UnmarkedSyntax * syn;} ";
        cw << "_syntaxes[" << syntaxes_size << "] = {\n";
        for (Vector<SyntaxC *>::const_iterator i = cw.for_macro_sep_c->syntaxes.begin(), 
               e = cw.for_macro_sep_c->syntaxes.end(); i != e; ++i)
        {
          cw << "  {\"";
          escape(cw, (*i)->parse_->str());
          cw << "\", 0}";
          if (i + 1 != e)
            cw << ",\n";
          else
            cw << "\n";
        }
        cw << "};\n\n";
      }
    }

    cw << "/* function decls */\n";

    for (i = syms.begin(); i != e; ++i) {
      if (const Fun * d = dynamic_cast<const Fun *>((*i)->decl)) {
        if (cw.for_compile_time()) {
          tl = dynamic_cast<const TopLevelVarSymbol *>(d->sym);
          if (cw.deps->have(tl)) {
            d->compile(cw, Declaration::Forward);
          }
        } else if (cw.for_macro_sep_c || !d->for_ct()) {
          d->compile(cw, Declaration::Forward);
        }
      }
    }

    cw << "/* definitions */\n";

    for (i = syms.begin(); i != e; ++i) {
      if (const VarDeclaration * d = dynamic_cast<const VarDeclaration *>((*i)->decl)) {
        if (cw.for_compile_time()) {
          tl = dynamic_cast<const TopLevelVarSymbol *>(d->sym);
          if (cw.deps->have(tl) && !d->sym->ct_ptr) {
            d->compile(cw, Declaration::Body);
          }
        } else if (cw.for_macro_sep_c || !d->for_ct()) {
          d->compile(cw, Declaration::Body);
        }
      }
    }

    cw << "/* special */\n";

    for (i = syms.begin(); i != e; ++i) {
      if (const TopLevelVarSymbol * s = dynamic_cast<const TopLevelVarSymbol *>(*i)) {
        if (s->init) init.push_back(s->init);
        if (s->cleanup) cleanup.push_back(s->cleanup);
      }
    }

    if (!init.empty()) {
      cw << "__attribute__((constructor)) static void init$s() {\n";
      for (Vector<AST *>::const_iterator i = init.begin(), e = init.end(); i != e; ++i) {
        cw << adj_indent(2) << *i;
      }
      cw << "}\n";
    }
    
    if (!cleanup.empty()) {
      cw << "__attribute__((destructor)) static void cleanup$s() {\n";
      for (Vector<AST *>::const_reverse_iterator i = cleanup.rbegin(), e = cleanup.rend(); i != e; ++i) {
        cw << adj_indent(2) << *i;
      }
      cw << "}\n";
    }

    cw << "/* done */\n";
  }
  
  //
  //
  //
  

}

