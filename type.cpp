#include <typeinfo>

#include "type.hpp"
#include "parse.hpp"
#include "ast.hpp"

namespace ast {

  static TypeCategory ANY_C_OBJ("any", 0);
  static TypeCategory SCALAR_C_OBJ("scalar", ANY_C);
  //static TypeCategory BOOL_C_OBJ("bool", SCALAR_C);
  static TypeCategory NUMERIC_C_OBJ("numeric", SCALAR_C);
  static TypeCategory INT_C_OBJ("integer", NUMERIC_C);
  static TypeCategory SIGNED_C_OBJ("signed integer", INT_C);
  static TypeCategory UNSIGNED_C_OBJ("unsigned integer", INT_C);
  static TypeCategory FLOAT_C_OBJ("floating point", NUMERIC_C);
  static TypeCategory POINTER_C_OBJ("pointer", SCALAR_C);
  static TypeCategory ARRAY_C_OBJ("array", POINTER_C); // FIXME: Is this right?
  static TypeCategory FUN_C_OBJ("function", ANY_C); // FIXME: Is this right?
  static TypeCategory UNKNOWN_C_OBJ("?", ANY_C);
  static TypeCategory ZERO_C_OBJ("zero", INT_C, POINTER_C);
  static TypeCategory USER_C_OBJ("user", ANY_C);

  TypeCategory * const ANY_C = &ANY_C_OBJ;
  TypeCategory * const SCALAR_C = &SCALAR_C_OBJ;
  //TypeCategory * const BOOL_C = &BOOL_C_OBJ;
  TypeCategory * const NUMERIC_C = &NUMERIC_C_OBJ;
  TypeCategory * const INT_C = &INT_C_OBJ;
  TypeCategory * const SIGNED_C = &SIGNED_C_OBJ;
  TypeCategory * const UNSIGNED_C = &UNSIGNED_C_OBJ;
  TypeCategory * const FLOAT_C = &FLOAT_C_OBJ;
  TypeCategory * const POINTER_C = &POINTER_C_OBJ;
  TypeCategory * const ARRAY_C = &ARRAY_C_OBJ;
  TypeCategory * const FUN_C = &FUN_C_OBJ;
  TypeCategory * const UNKNOWN_C = &UNKNOWN_C_OBJ;
  TypeCategory * const ZERO_C = &ZERO_C_OBJ;
  TypeCategory * const USER_C = &USER_C_OBJ;
  
  Type * VOID_T = 0;

  const char * type_name(const Type * t) {
    return typeid(*t).name();
  }

  Type * TypeSymbolTable::inst(SymbolKey n, Vector<TypeParm> & p) {
    TypeSymbol * s = find(n);
    if (!s) return NULL;
    return s->inst(p);
  }

  Type * TypeSymbolTable::inst(const Syntax * n, const InnerNS * ns, Vector<TypeParm> & p) {
    TypeSymbol * s = env->symbols.find<TypeSymbol>(n, ns);
    if (!s) return NULL;
    return s->inst(p);
  }

  class GenericPrintInst : public PrintInst { 
  public:
    enum Mode {ZLS_MODE, ZL_MODE} mode;
    bool kill_const;
    GenericPrintInst(Mode m = ZL_MODE) : mode(m), kill_const(false) {}
    void to_string(const TypeInst &, StringBuf & buf) const;
    void declaration(String var, const TypeInst &, StringBuf & buf) const;
  private:
    void to_string0(const TypeInst &, StringBuf & buf, bool keep_const = true) const;
  };

  class CPrintInst : public PrintInst { 
  public:
    enum Mode {C_MODE, ZL_MODE} mode;
    CPrintInst(Mode m = C_MODE) : mode(m) {}
    void to_string(const TypeInst &, StringBuf & buf) const;
    void declaration(String var, const TypeInst & t, StringBuf & buf, bool parentheses) const;
    void declaration(String var, const TypeInst & t, StringBuf & buf) const {
      declaration(var, t, buf, false);
    }
  };

  class ZLPrintInst : public CPrintInst { 
  public:
    ZLPrintInst() : CPrintInst(ZL_MODE) {}
  };

  class ZLSPrintInst : public GenericPrintInst { 
  public:
    ZLSPrintInst() : GenericPrintInst(ZLS_MODE) {}
  };

  class ZLSKillConstPrintInst : public ZLSPrintInst { 
  public:
    ZLSKillConstPrintInst() : ZLSPrintInst() {kill_const = true;}
  };

  class ZLEPrintInst : public GenericPrintInst { 
  public:
    ZLEPrintInst() : GenericPrintInst(ZL_MODE) {}
  };

  class ManglePrintInst : public PrintInst { 
  public:
    void to_string(const TypeInst &, StringBuf & buf) const;
    void declaration(String var, const TypeInst & t, StringBuf & buf) const;
  };

  PrintInst const * const generic_print_inst = new GenericPrintInst();
  PrintInst const * const c_print_inst = new CPrintInst();
  PrintInst const * const zl_print_inst = new ZLPrintInst();
  PrintInst const * const zls_print_inst = new ZLSPrintInst();
  PrintInst const * const zls_kill_const_print_inst = new ZLSKillConstPrintInst();
  PrintInst const * const zle_print_inst = new ZLEPrintInst();
  PrintInst const * const mangle_print_inst = new ManglePrintInst();
  
  void TypeParm::to_string(const PrintInst & pi, StringBuf & buf) const {
    switch (what) {
    case NONE:
      buf << "<none>";
      break;
    case TYPE:
    case TUPLE:
      pi.to_string(*as_type, buf);
      break;
    case INT:
      buf.printf("%d", as_int);
      break;
    case EXP:
      buf.printf("<EXP>");
      break;
    case DOTS:
      buf.printf("...");
      break;
    case UNKNOWN:
      abort();
      break;
    }
  }

  bool operator==(const TypeInst & lhs, const Vector<TypeParm> & rhs) {
    unsigned lhs_sz = lhs.num_parms(), rhs_sz = rhs.size();
    if (lhs_sz != rhs_sz) return false;
    for (unsigned i = 0; i != lhs_sz; ++i) {
      if (lhs.parm(i) != rhs[i]) return false;
    }
    return true;
  }

  bool operator==(const TypeInst & lhs, const TypeInst & rhs) {
    if (lhs.type_symbol != rhs.type_symbol) return false;
    unsigned lhs_sz = lhs.num_parms(), rhs_sz = rhs.num_parms();
    if (lhs_sz != rhs_sz) return false;
    for (unsigned i = 0; i != lhs_sz; ++i) {
      if (lhs.parm(i) != rhs.parm(i)) return false;
    }
    return true;
  }

  void GenericPrintInst::to_string0(const TypeInst & type0, StringBuf & buf, bool keep_const) const {
    bool zls_mode = mode == ZLS_MODE;
    const TypeInst * type = &type0;
    for (;;) { // loop while something changed
      if (const ZeroT * t = dynamic_cast<const ZeroT *>(type)) {
        type = t->of;
      } else if (const WrapperTypeInst * t = dynamic_cast<const WrapperTypeInst *>(type)) {
        type = t->of;
      } else if (const UserType * t = zls_mode ? dynamic_cast<const UserType *>(type) : NULL) {
        type = t->type;
      } else {
        break;
      }
    }
    unsigned sz = type->num_parms();
    if (sz == 0) {
      if (const char * tag = type->tag()) {
        buf << tag << " ";
      }
      buf << type->type_symbol->uniq_name();
    } else if (const Tuple * t = dynamic_cast<const Tuple *>(type)) {
      buf << ".";
      for (unsigned i = 0; i < t->parms.size();) {
        buf << " (";
        to_string(*t->parms[i].type, buf);
        buf << " ";
        buf << (t->parms[i].sym ? t->parms[i].sym->uniq_name() : t->parms[i].name.name);
        buf << ")";
        ++i;
      }
      if (t->vararg) {
	buf << " ...";
      }
    } else if (const QualifiedType * t = dynamic_cast<const QualifiedType *>(type)) {
      to_string0(*t->subtype, buf);
      if (t->qualifiers & QualifiedType::CONST && keep_const)  buf += " :const";
      if (t->qualifiers & QualifiedType::VOLATILE)             buf += " :volatile";
      if (t->qualifiers & QualifiedType::RESTRICT)             buf += " :restrict";
    } else if (const Reference * t = zls_mode ? dynamic_cast<const Reference *>(type) : NULL) {
      buf += ".ptr ";
      to_string(*t->subtype, buf);
    } else {
      buf += type->type_symbol->name();
      for (unsigned i = 0;;) {
        buf += " ";
        type->parm(i).to_string(*this, buf);
        ++i;
        if (i == sz) break;
      }
    }
  }

  void GenericPrintInst::to_string(const TypeInst & type, StringBuf & buf) const {
    buf << "(";
    to_string0(type, buf, !kill_const);
    buf << ")";
  }

  void GenericPrintInst::declaration(String var, const TypeInst & type, StringBuf & buf) const {
    buf << "(var " << var << " ";
    to_string(type, buf);
    buf << ")";
  }

  void ManglePrintInst::to_string(const TypeInst & type0, StringBuf & buf) const {
    // $s: "struct X"
    // $e: "enum X"
    // $c: "class X" (unused)
    // $u: "union X"
    // $b: <built in type with spaces replaced with _> (
    // $_: <type>
    // $C: "const X"
    // $v: "volatile X"
    // $r: "restrict R"
    // $P: "* x" 
    // $R: "& x"
    const TypeInst * type = type0.root;
    for (;;) { // loop while something changed
      if (const ZeroT * t = dynamic_cast<const ZeroT *>(type)) {
        type = t->of;
      } else if (const WrapperTypeInst * t = dynamic_cast<const WrapperTypeInst *>(type)) {
        type = t->of;
      } else {
        break;
      }
    }
    unsigned sz = type->num_parms();
    if (sz == 0) {
      if (const char * tag = type->tag()) {
        buf << "$" << tag[0]; // $s, $e, $c, or $u
        buf << type->type_symbol->uniq_name();
      } else {
        StringBuf tmp;
        tmp << type->type_symbol->uniq_name();
        bool dash = false;
        for (unsigned i = 0; i != tmp.size(); ++i) {
          if (tmp[i] == '-') {tmp[i] = '_'; dash = true;}
        }
        buf << (dash ? "$b" : "$_") << tmp.freeze();
      }
    } else if (const QualifiedType * t = dynamic_cast<const QualifiedType *>(type)) {
      if (t->qualifiers & QualifiedType::CONST)    buf += "$C";
      if (t->qualifiers & QualifiedType::VOLATILE) buf += "$v";
      if (t->qualifiers & QualifiedType::RESTRICT) buf += "$r";
      to_string(*t->subtype, buf);
    } else if (const PointerLike * t = dynamic_cast<const PointerLike *>(type)) {
      buf += "$P";
      to_string(*t->subtype, buf);
    } else if (const Reference * t = dynamic_cast<const Reference *>(type)) {
      buf += "$R";
      to_string(*t->subtype, buf);
    } else {
      buf += "$U";
    }
  }

  void ManglePrintInst::declaration(String var, const TypeInst & type, StringBuf & buf) const {
    abort();
  }

  void CPrintInst::to_string(const TypeInst & type, StringBuf & buf) const {
    declaration("", type, buf);
  }

  void CPrintInst::declaration(String var, const TypeInst & type0, StringBuf & buf, bool parentheses) const {
    const Type * type = &type0;
    String qualifiers;
    for (;;) { // loop while something changed
      if (const ZeroT * t = dynamic_cast<const ZeroT *>(type)) {
        type = t->of;
      } else if (const QualifiedType * t = dynamic_cast<const QualifiedType *>(type)) {
        StringBuf lbuf = qualifiers;
        if (t->qualifiers & QualifiedType::CONST)    lbuf += " const";
        if (t->qualifiers & QualifiedType::VOLATILE) lbuf += " volatile";
        if (t->qualifiers & QualifiedType::RESTRICT) lbuf += " restrict";
        qualifiers = lbuf.freeze();
        type = t->subtype;
      } else if (const WrapperTypeInst * t = dynamic_cast<const WrapperTypeInst *>(type)) {
        type = t->of;
      } else if (const UserType * t = dynamic_cast<const UserType *>(mode == C_MODE ? type : NULL)) {
        type = t->type;
      } else {
        break;
      }
    }
    if (const Tuple * t = dynamic_cast<const Tuple *>(type)) {
      buf << "(";
      for (unsigned i = 0; i < t->parms.size();) {
        declaration(t->parms[i].sym ? t->parms[i].sym->uniq_name() : t->parms[i].name.name, 
                    *t->parms[i].type, buf);
        ++i;
        if (i == t->parms.size()) break;
        buf << ", ";
      }
      if (t->vararg) {
	buf << ", ...";
      }
      buf << ")";
    } else if (const Pointer * t = dynamic_cast<const Pointer *>(type)) {
      //const Type * rec_subtype = t->subtype;
      //while (const Pointer * t2 = dynamic_cast<const Pointer *>(rec_subtype))
      //  rec_subtype = t2->subtype;
      StringBuf lbuf;
      lbuf << "*" << qualifiers;
      if (!var.empty())
        lbuf << " " << var;
      declaration(lbuf.freeze(), *t->subtype, buf, true);
    } else if (const Reference * t = dynamic_cast<const Reference *>(type)) {
      StringBuf lbuf;
      lbuf << (mode == C_MODE ? "*" : "&") << qualifiers;
      if (!var.empty())
        lbuf << " " << var;
      declaration(lbuf.freeze(), *t->subtype, buf, true);
    } else if (const Array * t = dynamic_cast<const Array *>(type)) {
      assert(qualifiers.empty());
      StringBuf lbuf;
      if (parentheses) lbuf << "(" << var << ")";
      else             lbuf << var;
      lbuf.printf("[%d]", t->length);
      declaration(lbuf.freeze(), *t->subtype, buf);
    } else if (const Function * t = dynamic_cast<const Function *>(type)) {
      assert(qualifiers.empty());
      StringBuf lbuf;
      if (parentheses) lbuf << "(" << var << ")";
      else             lbuf << var;
      declaration("", *t->parms, lbuf);
      declaration(lbuf.freeze(), *t->ret, buf);
    } else if (type->num_parms() == 0) {
      if (const char * tag = type->tag()) {
        buf << tag << " ";
      }
      unsigned i = buf.size();
      buf << type->type_symbol->uniq_name();
      unsigned e = buf.size();
      for (; i != e; ++i)
        if (buf[i] == '-') buf[i] = ' ';
      buf << qualifiers;
      if (!var.empty())
	buf << " " << var;
    } else {
      abort();
    }
  }

  //Type * TypeOfSymbol::inst(Vector<TypeParm> & d) {
  //  assert(d.size() == 1);
  //  assert(d[0].what == TypeParm::EXP);
  //  TypeOf * t = new TypeOf(d[0].as_exp);
  //  t->finalize();
  //  return t;
  //}

  inline TypeOf::TypeOf(Exp * a) 
    : WrapperTypeInst(a->type->effective, a->syn), of_ast(a) {}

  void parse_type_parms(parts_iterator start, parts_iterator end, 
                        const TypeSymbol * t, Vector<TypeParm> & parms, 
                        Environ & env, bool in_tuple) 
  {
    unsigned sz = end - start;
    parms.reserve(sz);
    for (int i = 0; i != sz; ++i) {
      const Syntax * p0 = start[i];
      SymbolKey n;
      if (in_tuple && !p0->part(0)->simple()) { // HACK!
	if (p0->num_parts() > 1)
	  n = expand_binding(p0->part(1), env);
	p0 = p0->part(0);
      }
      switch(t->parmt(i)) {
      case TypeParm::TYPE: {
	if (*p0 == "...") {
	  parms.push_back(TypeParm::dots());
	} else {
	  Type * t0 = parse_type(p0, env);
	  parms.push_back(TypeParm(t0, n));
	}
        break;
      }
      case TypeParm::INT: {
        parms.push_back(TypeParm(parse_ct_value(p0, env)));
        break;
      }
      case TypeParm::TUPLE: {
        Type * t0 = parse_type(p0, env);
        Tuple * tt = dynamic_cast<Tuple *>(t0);
        if (!tt)
          throw error(p0, "Expected Tuple Type");
        parms.push_back(TypeParm(TypeParm::TUPLE, t0));
        break;
      }
      case TypeParm::EXP: {
        Exp * exp = parse_exp(p0, env); // FIXME: Do I really need to parse here....
        parms.push_back(TypeParm(exp));
        break;
      }
      case TypeParm::NONE: {
        abort();
        //throw error(p0, "Two Many Type Paramaters");
      }
      case TypeParm::DOTS: {
	abort(); // special case, should not happen
      }
      case TypeParm::UNKNOWN: {
        if (p0->is_a(".type")) {
          parms.push_back(TypeParm(parse_type(p0->arg(0), env), n));
        } else {
          parms.push_back(TypeParm(parse_ct_value(p0, env)));
        }
      }}
    }
  }


  Type * parse_type(const Syntax * p, Environ & env) {
    if (p->have_entity()) {
      Type * type = p->entity<Type>();
      assert(type);
      return type;
    }
    TypeSymbolTable types = env.types;
    p = limited_expand(p, env);
    p = flatten(p); // FIXME: This is an overkill...
    if (p->is_a(".type")) p = p->arg(0);
    unsigned sz = p->num_args();
    const InnerNS * ns = DEFAULT_NS;
    //printf("TYPE: %s %s\n", ~p->what(), ~p->to_string());
    const Syntax * name = p->part(0);
    name = expand_id(name, env); // FIXME: Is this the right place
    String name_str = name->what().name;
    String tag;
    if (name_str == "struct" || name_str == "union" || name_str == "enum") {
      tag = name_str;
      assert(sz == 1);
      // FIXME: Add check for wrong type of tag
      ns = TAG_NS;
      name = p->arg(0);
      StringBuf buf;
      buf << tag << " " << name_str;
      name_str = buf.freeze();
      --sz;
    } else if (name_str == ".typeof") {
      Exp * ast = parse_exp_for_type(p->arg(0), env);
      Type * t = new TypeOf(ast);
      t->finalize();
      return t;
    } else if (name_str == ".tprop") {
      const Syntax * tp = p->arg(0);
      const Type * t;
      if (tp->is_a(".type")) {
        t = parse_type(tp->arg(0), env);
      } else {
        Exp * exp = parse_exp_for_type(tp, env);
        // Exp may be a pointer to a overloaded symbol, so we need to
        // resolve the type.  This will not work for a true overloaded
        // symbol, though.
        exp = exp->resolve_to(NULL, env); 
        t = exp->type->effective;
      }
      t = t->tprop(p->arg(1), env);
      Type * res = new WrapperTypeInst(t, p);
      res->finalize();
      return res;
    }
    TypeSymbol * t = types.find(name, ns);
    if (!t) {
      if (tag.empty()) {
        throw error(p, "Unknown type: %s", ~name->to_string());
      } else {
        SimpleType * ti;
        SymbolKey n = expand_binding(name, TAG_NS, env);
        if (tag == "struct")     ti = new Struct;
        else if (tag == "union") ti = new Union;
        else if (tag == "enum")  ti = new Enum;
        else abort();
        add_simple_type(types, n, ti, env.where);
        t = types.find(n);
        assert(t);
      }
    }
    bool in_tuple = name_str == ".";
    //if (t->required_parms() == 0 && sz != 0 && !in_tuple /* HACK! */)
    //  throw error(p, "Type \"%s\" is not a paramatized type.", ~name_str);
    if (t->parmt(0) == TypeParm::NONE && sz != 0)
      throw error(p, "Type \"%s\" is not a paramatized type.", ~name_str);
    if (t->required_parms() > sz) 
      throw error(p, "Not enough paramaters for \"%s\" type, %d required, but got %d",
                  ~name_str, t->required_parms(), sz);
    Vector<TypeParm> parms;
    if (sz > 0) // needed because sz != num_args() due to tagged types
      parse_type_parms(p->args_begin(), p->args_end(), t, parms, env, in_tuple);
    Type * inst_type = t->inst(parms);
    unsigned qualifiers = 0;
    if (p->flag("const"))    qualifiers |= QualifiedType::CONST;
    if (p->flag("volatile")) qualifiers |= QualifiedType::VOLATILE;
    if (p->flag("restrict")) qualifiers |= QualifiedType::RESTRICT;
    if (qualifiers) {
      Vector<TypeParm> q_parms;
      q_parms.push_back(TypeParm(inst_type));
      q_parms.push_back(TypeParm(qualifiers));
      return types.find(".qualified")->inst(q_parms);
    } else {
      return inst_type;
    }
  }

  const Type * PointerLike::tprop(const Syntax * p, Environ & env) const {
    if (p->is_a("inner"))
      return subtype;
    else
      throw error(p, "Unsupported type property \"%s\" for pointer type", ~p->what());
  }
  
  Function * FunctionSymbol::inst(TypeSymbolTable types, Fun * f) {
    Vector<TypeParm> p;
    p.clear();
    p.push_back(TypeParm(TypeParm::TUPLE, f->parms));
    p.push_back(TypeParm(f->ret_type));
    return static_cast<Function *>(inst(p));
  }

  const Type * Function::tprop(const Syntax * p, Environ & env) const {
    if (p->is_a("ret")) {
      return ret;
    } else if (p->is_a("parm")) {
      assert_num_args(p, 1);
      target_int i = parse_ct_value(p->arg(0), env);
      return parms->parm(i).as_type;
    } else {
      throw error(p, "Unsupported type property \"%s\" for function type", ~p->what());
    }
  }
  

#if 0
  Function::Function(Fun * f) 
    : ParmTypeInst(POINTER_SIZE)
  {
    ret = f->type;
    Tuple * ps = new Tuple();
    parms = ps;
    for (int i = 0; i != f->parms.size(); ++i) {
      ps->parts.push_back(f->parms[i].type);
    }
  }
#endif

  const Type * TypeRelation::unify(int rule, Exp * & x, Exp * & y, Environ & env) {
    const Type * t = unify(rule, x->type, y->type, env);
    //printf("UNIFY %d to \"\%s\"\n", x->parse_->str().source->get_pos(x->parse_->str().begin).line, ~t->to_string());
    //if (t != xt) {x = new Cast(x, t);}
    //if (t != yt) {y = new Cast(y, t);}
    x = x->resolve_to(t, env);
    y = y->resolve_to(t, env);
    return t;
  }

  class C_TypeRelation : public TypeRelation {
  public:
    TypeConv resolve_to(Exp * exp, const Type * type, Environ & env, CastType rule, CheckOnlyType) const;
    Exp * to_effective(Exp * exp, Environ & env) const;
    Exp * def_arg_prom(Exp * exp, Environ & env) const;
    void resolve_assign(Exp * & lhs, Exp * & rhs, Environ & env) const;
    const Type * unify(int rule, const Type *, const Type *, Environ & env) const;
  };

  static const Int * INT;
  static const Int * UINT;

  static const Int * int_promotion(const Int * x) {
    // 6.3.1.1: Integer promotions
    assert(x->min != x->max);
    //printf("%lld <= %lld && %llu <= %llu %p %p\n", INT->min, x->min, x->max, INT->max, INT, x);
    if (INT->min <= x->min && x->max <= INT->max) return INT;
    //printf("%lld <= %lld && %llu <= %llu %p %p\n", UINT->min, x->min, x->max, UINT->max, UINT, x);
    if (UINT->min <= x->min && x->max <= UINT->max) return UINT;
    return x;
  }

#define TC(conv, exp) TypeConv(TypeConv::conv, check_only ? NULL : (exp))
  TypeConv C_TypeRelation::resolve_to(Exp * orig_exp, const Type * type, Environ & env, 
                                      CastType rule, CheckOnlyType check_only) const 
  {
    static int i = -1;
    ++i;

    // NOTE: type may be NULL.  This is a bit of a hack, which should
    // be removed once I implement the "auto" type.

    if (!orig_exp->type) {
      // OK so we have an overloaded symbol, type is expected to be a
      // pointer to a fun or NULL, and orig_exp is an id
      Id * id = dynamic_cast<Id *>(orig_exp);
      const Fun * fun;
      if (type) {
        const Pointer * ptr = dynamic_cast<const Pointer *>(type->root);
        const Function * fun_t = dynamic_cast<const Function *>(ptr->subtype);
        fun = lookup_overloaded_symbol<Fun>(fun_t->parms, orig_exp->syn, id->sym, &env);
      } else {
        fun = lookup_overloaded_symbol<Fun>(NULL, orig_exp->syn, id->sym, &env);
      }
      return TC(Other, mk_id(fun, env));
    }

    if (!type) return TC(Other, orig_exp);

    const Type * have = orig_exp->type->unqualified;
    const Type * have_qualified = orig_exp->type->root;
    const Type * need = type->unqualified;
    // FIXME: Should't need exp below if check_only...
    //Exp * exp = check_only ? NULL : orig_exp;
    Exp * exp = orig_exp;
    
    const Reference * h_r = dynamic_cast<const Reference *>(have);
    const Reference * n_r = dynamic_cast<const Reference *>(need);
    if (h_r) {
      if (!check_only)
        exp = from_ref(exp, env);
      //have = exp->type->unqualified;
      //have_qualified = exp->type->root;
      have = h_r->subtype->unqualified;
      have_qualified = h_r->subtype->root;
    }
    if (n_r) {
      unsigned conv = TypeConv::ExactMatch;
      const UserType * hu, * nu;
      if (have != n_r->subtype->unqualified) {
        if ((hu = dynamic_cast<const UserType *>(have)) &&
            (nu = dynamic_cast<const UserType *>(n_r->subtype->unqualified))) 
        {
          if (have_qualified->read_only && !n_r->subtype->read_only)
            throw error(orig_exp->syn, "Conversion from \"%s\" to \"%s\" disregards const qualifier <1>\n", 
                        ~have->to_string(), ~need->to_string());
          else if (hu->is(nu->category))
            return TC(PointerConv, ptr_to_ref(cast_up(addrof(exp, env), nu, env), env));
          else if (nu->is(hu->category) && rule == Explicit || rule == Static)
            return TC(Other, ptr_to_ref(cast_down(addrof(exp, env), nu, env), env));
          else
            goto fail;
        } else if (n_r->subtype->read_only) {
          //goto fail;
          TypeConv res = resolve_to(exp, n_r->subtype->unqualified, env, rule, check_only);
          conv = res.conv;
          exp = res.exp;
          //have = exp->type->unqualified;
          //have_qualified = exp->type->root;
          have = n_r->subtype->unqualified;
          have_qualified = n_r->subtype->root;
        } else {
          goto fail;
        }
      }
      //if (!check_only)
      //  printf("888> %s vs. %s\n", 
      //         ~exp->type->unqualified->to_string(), 
      //         ~n_r->subtype->unqualified->to_string());
      if (have_qualified->read_only && !n_r->subtype->read_only)
        throw error(orig_exp->syn, "Conversion from \"%s\" to \"%s\" disregards const qualifier <2>\n", 
                    ~orig_exp->type->to_string(), ~type->to_string());
      if (check_only)
        return TypeConv(conv, NULL);
      if (!exp->lvalue
          || (env.temp_ip && env.temp_ip->where >= ExpInsrPoint::Var && exp->lvalue < LV_NORMAL))
        exp = make_temp(exp, env);
      if (exp->type->unqualified != n_r->subtype->unqualified)
        // need a cast here since the pointer types are incompatible to the backend
        return TypeConv(conv, new Cast(to_ref(exp, env), type));
      else
        return TypeConv(conv, to_ref(exp, env));
    }

    if (have == need) return TC(ExactMatch, exp);
    //printf("NO MATCH %p:%s %p:%s\n", 
    //       have, ~have->to_string(),
    //       need, ~need->to_string());
    //printf("%i RESOLVE_TO %s (%s) (%s) %d\n", 
    //       i, orig_exp->syn ? ~orig_exp->syn->to_string() : "?", 
    //       ~orig_exp->type->to_string(),
    //       ~type->to_string(),
    //       rule);

    // FIXME: Is this right?
    if (rule == Reinterpret /* || rule == Explicit*/) // FIXME: This isn't always legal
      return TC(Other, new Cast(exp, type));

    if (have->is(INT_C)) {
      if (need->is(INT_C)) {
        const Int * have_i = dynamic_cast<const Int *>(have);
        const Int * need_i = dynamic_cast<const Int *>(need);
        const Int * prom = int_promotion(have_i);
        if (prom == need_i)
          return TC(IntProm, new Cast(exp, type));
        else
          return TC(IntConv, new Cast(exp, type));
      } else if (need->is(FLOAT_C)) {
        return TypeConv(TypeConv::FloatInt, new Cast(exp, type));
      }
    } else if (have->is(FLOAT_C)) {
      if (need->is(FLOAT_C)) {
        const Float * have_f = dynamic_cast<const Float *>(have);
        const Float * need_f = dynamic_cast<const Float *>(need);
        if (have_f->size_ <= need_f->size_) //FIXME: Should use precision, but thats currently not set correctly
          return TC(FloatProm, new Cast(exp, type));
        else
          return TC(FloatConv, new Cast(exp, type));
      } else if (need->is(INT_C)) {
        return TC(FloatInt, new Cast(exp, type));
      }
    }

#if 0
    // FIXME: This is not really C...
    if (have->is(USER_C) && need->is(USER_C)) {
      if (have->is(need->category))
        return new Cast(exp, type);
      else
        goto fail;
    }
#endif

    // deal with pointer to int case
    // fixme, need to seperate out boolean case, and issue a warning otherwise
    if (have->is(POINTER_C) && need->is(INT_C))
      return TC(BoolConv, new Cast(exp, type));

    // deal with pointer types
    {
      const Pointer * n_p = dynamic_cast<const Pointer *>(need);
      const Array   * n_a = dynamic_cast<const Array *>(need); // FIXME: Hack
      const Type * n_subtype = n_p ? n_p->subtype : n_a ? n_a->subtype : 0;
      if (!n_subtype) goto fail;
      // FIXME: I should be able to say have->is_null, fix up handling
      //        of ZeroT to make this possible.
      if (orig_exp->type->is_null) return TC(PointerConv, new Cast(exp, type));
      // FIXME: This probably isn't right
      if (dynamic_cast<const Function *>(have)) {
        if (rule == Implicit)
          return TC(PointerConv, exp);
        else
          return TC(PointerConv, new Cast(exp, type));
      }
      const Pointer * h_p = dynamic_cast<const Pointer *>(have);
      const Array   * h_a = dynamic_cast<const Array *>(have);
      const Type * h_subtype = h_p ? h_p->subtype : h_a ? h_a->subtype : 0;
      if (!h_subtype) goto fail;
      if (dynamic_cast<const Function *>(h_subtype->unqualified) // FIXME: Hack
          && dynamic_cast<const Function *>(n_subtype->unqualified))
        return TC(PointerConv, new Cast(exp, type));
      const UserType * hu, * nu;
      if (h_subtype->unqualified == n_subtype->unqualified ||
          dynamic_cast<const Void *>(h_subtype->unqualified) ||
          dynamic_cast<const Void *>(n_subtype->unqualified) /* this one is C only */)  
      {
        if (rule == Explicit) {
          return TC(PointerConv, new Cast(exp, type));
        } else if (!h_subtype->read_only || n_subtype->read_only) {
          if (h_subtype->unqualified == n_subtype->unqualified)
            return TC(ExactMatch, exp);
          else
            return TC(PointerConv, exp);
        } else {
          throw error(orig_exp->syn, "Conversion from \"%s\" to \"%s\" disregards const qualifier <3>\n", 
                      ~have->to_string(), ~need->to_string());
        }
      } else if ((hu = dynamic_cast<const UserType *>(h_subtype->unqualified)) &&
                 (nu = dynamic_cast<const UserType *>(n_subtype->unqualified)))
      { // this is C++ only
        //printf("XYZ %d>%s %s | %s %s\n", i,
        //       ~h_subtype->to_string(), ~n_subtype->to_string(),
        //       ~hu->to_string(), ~nu->to_string());
        if (h_subtype->read_only && !n_subtype->read_only)
          throw error(orig_exp->syn, "Conversion from \"%s\" to \"%s\" disregards const qualifier <4>\n", 
                      ~have->to_string(), ~need->to_string());
        else if (h_subtype->is(n_subtype->category))
          return TC(PointerConv, cast_up(exp, nu, env));
        else if (n_subtype->is(h_subtype->category) && rule == Explicit || rule == Static)
          return TC(Other, cast_down(exp, nu, env));
        else
          goto fail;
      } else {
        goto fail;
      }
    }
    //abort();

  fail:
    if (rule == Explicit) // FIXME: This isn't always legal
      return TC(Other,new Cast(exp, type));

    throw error(orig_exp->syn, "%d Mismatch Types expected \"%s\" but got \"%s\"", i,
                ~type->to_string(), ~orig_exp->type->to_string());
  }

  Exp * C_TypeRelation::to_effective(Exp * exp, Environ & env) const {
    const Reference * ref = dynamic_cast<const Reference *>(exp->type->unqualified);
    if (ref)
      return from_ref(exp, env);
    else
      return exp;
  }

  Exp * C_TypeRelation::def_arg_prom(Exp * exp, Environ & env) const {
    // FIXME: Need to do more ...
    const Reference * ref = dynamic_cast<const Reference *>(exp->type->unqualified);
    if (ref)
      return from_ref(exp, env);
    else
      return exp;
  }

  void C_TypeRelation::resolve_assign(Exp * & lhs, Exp * & rhs, Environ & env) const {
    if (!lhs->lvalue)
      throw error(lhs->syn, "Can not be used as lvalue in assignment");
    //throw error(syn->arg(1), "Can not be used as lvalue");
    //if (lhs->type->read_only) 
    //  throw error (lhs->syn, "Assignment to read-only location");
    const Reference * lhs_r = dynamic_cast<const Reference *>(lhs->type->unqualified);
    if (lhs_r) {
      lhs = from_ref(lhs, env);
    }
    try {
      rhs = rhs->resolve_to(lhs->type, env);
    } catch (Error * err) {
      StringBuf buf;
      buf = err->msg;
      buf.printf(" in assignment of %s.", ~lhs->syn->sample_w_loc());
      err->msg = buf.freeze();
      throw err;
    }
  }

  const Type * C_TypeRelation::unify(int rule, const Type * x0, const Type * y0, Environ & env) const {
    // FIXME: The Fancy case needs a lot of work, it should conform to
    // C++ handling of the conditional op., 5.16
    
    // 6.3.1.8: Usual arithmetic conversions
    const Type * x = x0->effective->unqualified;
    const Type * y = y0->effective->unqualified;
    if (rule == Default /* HACK! */) {
    {
      const Float * x0 = dynamic_cast<const Float *>(x);
      const Float * y0 = dynamic_cast<const Float *>(y); 
      if (x0 && y0) return x0->precision > y0->precision ? x0 : y0;
      if (x0) return x0;
      if (y0) return y0;
    } {
      const Int * x0 = dynamic_cast<const Int *>(x);
      const Int * y0 = dynamic_cast<const Int *>(x);
      x0 = int_promotion(x0);
      y0 = int_promotion(y0);
      assert(x0 && y0);
      if (x0->signed_ == y0->signed_) return x0->rank > y0->rank ? x0 : y0;
      const Int * u = x0->signed_ == Int::UNSIGNED ? x0 : y0;
      const Int * s = x0->signed_ == Int::SIGNED   ? x0 : y0;
      assert (u != s);
      if (u->rank >= s->rank) return u;
      if (s->min <= u->min && s->max >= u->max) return s;
      if (rule != Fancy)
        abort(); // FIXME: From standard: Otherwise, both operands are
                 // converted to the unsigned integer type corresponding
                 // to the type of the operand with signed integer type.

    }}
    assert(rule == Fancy);
    // deal with pointer types
    {
      const Pointer * x_p = dynamic_cast<const Pointer *>(x);
      const Pointer * y_p = dynamic_cast<const Pointer *>(y);
      if (x_p && y_p) {
        if (x_p->subtype->root == y_p->subtype->root)
          return x0;
        if (x_p->subtype->unqualified != y_p->subtype->unqualified)
          return x0;
        unsigned qualifiers = 0;
        const QualifiedType * x_q = dynamic_cast<const QualifiedType *>(x_p->subtype->root);
        const QualifiedType * y_q = dynamic_cast<const QualifiedType *>(y_p->subtype->root);
        if (x_q && ! y_q) return x0;
        if (y_q && ! x_q) return y0;
        if (x_q) qualifiers |= x_q->qualifiers;
        if (y_q) qualifiers |= y_q->qualifiers;
        if (qualifiers) {
          env.types.inst(".ptr", 
                         env.types.inst(".qualified", 
                                        TypeParm(x_p->subtype->unqualified),
                                        TypeParm(qualifiers)));
        } else 
          return x0;
      }
    }
    return x0;
  }

  TypeRelation * new_c_type_relation() {
    return new C_TypeRelation();
  }

  const Int * add_c_int(TypeSymbolTable types, String n);

  void create_c_types(TypeSymbolTable types) {
    add_internal_type(types, ".int8", new_signed_int(1));
    add_internal_type(types, ".uint8", new_unsigned_int(1));
    add_internal_type(types, ".int16", new_signed_int(2));
    add_internal_type(types, ".uint16", new_unsigned_int(2));
    add_internal_type(types, ".int32", new_signed_int(4));
    add_internal_type(types, ".uint32", new_unsigned_int(4));
    add_internal_type(types, ".int64", new_signed_int(8));
    add_internal_type(types, ".uint64", new_unsigned_int(8));

    add_c_int(types, "signed-char");
    add_c_int(types, "short");
    INT = add_c_int(types, "int");
    add_c_int(types, "long");
    add_c_int(types, "long-long");

    //types.add("signed short", types.find("short"));
    //types.add("signed int", types.find("int"));
    //types.add("signed long", types.find("long"));
    //types.add("signed long long", types.find("long long"));

    add_c_int(types, "unsigned-char");
    add_c_int(types, "unsigned-short");
    UINT = add_c_int(types, "unsigned-int");
    add_c_int(types, "unsigned-long");
    add_c_int(types, "unsigned-long-long");

    types.add_alias("unsigned", types.find("unsigned-int"));

    types.add_alias("wchar_t", types.find("long"));

    add_c_int(types, "char");

    VOID_T = new Void();
    add_internal_type(types, "void", static_cast<Void *>(VOID_T));

    add_internal_type(types, "float", new Float(Float::SINGLE));
    add_internal_type(types, "double", new Float(Float::DOUBLE));
    add_internal_type(types, "long-double", new Float(Float::LONG));

    //add_internal_type(types, "<bool>", new AliasT(types.inst("int")));

    

    types.add_alias("<void>", types.find("void"));

    types.add_alias(".size", types.find("unsigned-long"));
    types.add_alias(".ptrdiff", types.find("long"));

    types.add_internal(".ptr", new PointerSymbol);
    types.add_internal(".ref", new ReferenceSymbol);
    //types.add_name("<const>", new ConstSymbol);
    types.add_internal(".array", new ArraySymbol);
    types.add_internal(".fun", new FunctionSymbol);
    types.add_internal(".", new TupleSymbol);
    types.add_internal(".qualified", new QualifiedTypeSymbol);
    types.add_internal(".zero", new ZeroTypeSymbol);
    //types.add_internal(".typeof", new TypeOfSymbol);
    add_internal_type(types, "__builtin_va_list", new Void());
    //add_internal_type(types, "Match", new Void());
    //add_internal_type(types, "Syntax", new Void());
    //add_internal_type(types, "Mark", new Void());
    //add_internal_type(types, "Context", new Void());
  }

  Type * TypeSymbolTable::ct_const(const Type * t) {
    return inst(".qualified", TypeParm(t), TypeParm(QualifiedType::CONST));
  }

}

namespace ast {

  struct IntMap {
    String c_type;
    String exact_type;
  };

  const IntMap int_map[] = {
    {"char", ".int8"},
    {"signed-char", ".int8"},
    {"unsigned-char", ".uint8"},
    {"short", ".int16"},
    {"unsigned-short", ".uint16"},
    {"int", ".int32"},
    {"unsigned-int", ".uint32"},
    {"long", ".int32"},
    {"unsigned-long", ".uint32"},
    {"long-long", ".int64"},
    {"unsigned-long-long", ".uint64"}
  };
  const IntMap * int_map_end = int_map + sizeof(int_map)/sizeof(IntMap);
  
  String c_type_to_exact(String t) {
    for (const IntMap * i = int_map; i != int_map_end; ++i) {
      if (i->c_type == t) return i->exact_type;
    }
    abort();
  }
  
  const Int * add_c_int(TypeSymbolTable types, String n) {
    Int * i = new Int(static_cast<const Int *>(types.inst(c_type_to_exact(n))));
    add_internal_type(types, n, i);
    return i;
  }

}

extern "C" namespace macro_abi {
  using namespace ast;
  typedef Function FunType;

  const Type * symbol_to_type(const Symbol * sym) {
    abort(); // this doesn't necessary make sense due to the fact that
             // a type is not really a symbol, we are only pretending
             // it is for the macro API
  }

  const Symbol * type_to_symbol(const Type * type) {
    return type->type_symbol;
  }

  const Type * type_root(const Type * type) {
    type = type->root;
    for (;;) { // loop while something changed
      if (const ZeroT * t = dynamic_cast<const ZeroT *>(type)) {
        type = t->of;
      } else if (const WrapperTypeInst * t = dynamic_cast<const WrapperTypeInst *>(type)) {
        type = t->of;
      } else {
        break;
      }
    }
    return type;
  }

  const Type * type_subtype(const Type * type) {
    if (type->num_parms() == 0) return NULL;
    TypeParm p = type->parm(0);
    if (!p.is_type()) return NULL;
    return p.as_type;
  }
  
  int type_qualifiers(const Type * type) {
    const QualifiedType * t = dynamic_cast<const QualifiedType *>(type);
    if (!t) return 0;
    return t->qualifiers;
  }

  const char * type_tag(const Type * type) {
    return type->tag();
  }

  const bool type_is_scalar(const Type * type) {
    return type->is(SCALAR_C);
  }
  
  const bool type_is_qualified(const Type * type) {
    return dynamic_cast<const QualifiedType *>(type);
  }

  const bool type_is_pointer(const Type * type) {
    return dynamic_cast<const Pointer *>(type);
  }

  const bool type_is_reference(const Type * type) {
    return dynamic_cast<const Reference *>(type);
  }

  const bool type_is_array(const Type * type) {
    return dynamic_cast<const Array *>(type);
  }

  const bool type_is_fun(const Type * type) {
    return dynamic_cast<const Function *>(type);
  }

  const Type * fun_type_ret_type(const FunType * type) {
    return type->ret;
  }

  unsigned fun_type_num_parms(const FunType * type) {
    return type->parms->num_parms();
  }

  const Type * fun_type_parm_type(const FunType * type, unsigned num) {
    return type->parms->parm(num).is_type() ? type->parms->parm(num).as_type : NULL;
  }

}
