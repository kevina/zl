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
    const TypeSymbol * s = find(n);
    if (!s) return NULL;
    return s->inst(p);
  }

  Type * TypeSymbolTable::inst(const Syntax * n, const InnerNS * ns, Vector<TypeParm> & p) {
    const TypeSymbol * s = env->symbols.find<TypeSymbol>(n, ns);
    if (!s) return NULL;
    return s->inst(p);
  }

  void TypeSymbolTable::add_name(const SymbolKey & k, TypeSymbol * t) {
    t->name = k.name;
    env->add(k, t);
  }

  PrintInst const * const generic_print_inst = new GenericPrintInst();
  PrintInst const * const c_print_inst = new CPrintInst();
  PrintInst const * const zl_print_inst = new ZLPrintInst();
  PrintInst const * const zls_print_inst = new ZLSPrintInst();
  
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

  void GenericPrintInst::to_string0(const TypeInst & type0, StringBuf & buf) const {
    bool zls_mode = mode == ZLS_MODE;
    const TypeInst * type = &type0;
    for (;;) { // loop while something changed
      if (const ZeroT * t = dynamic_cast<const ZeroT *>(type)) {
        type = t->of;
      } else if (const TypeOf * t = dynamic_cast<const TypeOf *>(type)) {
        type = t->of;
      } else if (const UserType * t = zls_mode ? dynamic_cast<const UserType *>(type) : NULL) {
        type = t->type;
      } else {
        break;
      }
    }
    unsigned sz = type->num_parms();
    if (sz == 0) {
      if (const TaggedType * t = dynamic_cast<const TaggedType *>(type)) {
        buf << t->what << " ";
      }
      type->type_symbol->uniq_name(buf);
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
      if (t->qualifiers & QualifiedType::CONST)    buf += " :const";
      if (t->qualifiers & QualifiedType::VOLATILE) buf += " :volatile";
      if (t->qualifiers & QualifiedType::RESTRICT) buf += " :restrict";
    } else if (const Reference * t = zls_mode ? dynamic_cast<const Reference *>(type) : NULL) {
      buf += ".ptr ";
      to_string(*t->subtype, buf);
    } else {
      buf += type->type_symbol->name;
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
    to_string0(type, buf);
    buf << ")";
  }

  void GenericPrintInst::declaration(String var, const TypeInst & type, StringBuf & buf) const {
    buf << "(var " << var << " ";
    to_string(type, buf);
    buf << ")";
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
      } else if (const TypeOf * t = dynamic_cast<const TypeOf *>(type)) {
        type = t->of;
      } else if (const UserType * t = dynamic_cast<const UserType *>(type)) {
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
      if (const TaggedType * t = dynamic_cast<const TaggedType *>(type)) {
        buf << t->what << " ";
      }
      unsigned i = buf.size();
      type->type_symbol->uniq_name(buf);
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

  Type * TypeOfSymbol::inst(Vector<TypeParm> & d) const {
    assert(d.size() == 1);
    assert(d[0].what == TypeParm::EXP);
    TypeOf * t = new TypeOf(d[0].as_exp);
    t->finalize();
    return t;
  }

  inline TypeOf::TypeOf(Exp * a) 
    : SimpleTypeInst(a->type->effective), of_ast(a), of(a->type->effective) {type_symbol = of->type_symbol;}

  Type * parse_type(const Syntax * p, Environ & env) {
    if (p->entity()) {
      Type * type = dynamic_cast<Type *>(p->entity());
      assert(type);
      return type;
    }
    TypeSymbolTable types = env.types;
    unsigned sz = p->num_args();
    const InnerNS * ns = DEFAULT_NS;
    //printf("TYPE: %s %s\n", ~p->what(), ~p->to_string());
    const Syntax * name = p->part(0);
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
      Exp * ast = parse_exp(p->arg(0), env);
      Type * t = new TypeOf(ast);
      t->finalize();
      return t;
    }
    const TypeSymbol * t = types.find(name, ns);
    if (!t) {
      if (tag.empty()) {
        throw error(p, "Unknown type: %s", ~name_str);
      } else {
        SimpleTypeInst * ti;
        SymbolKey n = expand_binding(name, TAG_NS, env);
        if (tag == "struct")     ti = new StructT(n);
        else if (tag == "union") ti = new UnionT(n);
        else if (tag == "enum")  ti = new EnumT(n);
        else abort();
        add_simple_type(types, n, ti, NULL, env.where);
        t = types.find(n);
        assert(t);
      }
    }
    if (t->required_parms() == 0 && sz != 0 && name_str != "." /* HACK! */)
      throw error(p, "Type \"%s\" is not a paramatized type.", ~name_str);
    if (t->required_parms() > sz) 
      throw error(p, "Not enough paramaters for \"%s\" type, %d required, but got %d",
                  ~name_str, t->required_parms(), sz);
    Vector<TypeParm> parms;
    parms.reserve(sz);
    for (int i = 0; i != sz; ++i) {
      const Syntax * p0 = p->arg(i);
      SymbolKey n;
      if (name_str == "." && !p0->part(0)->simple()) { // HACK!
	if (p0->num_parts() > 1)
	  n = expand_binding(p0->part(1), env);
	p0 = p0->part(0);
      }
      switch(t->parm(i)) {
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
        Exp * exp = parse_exp(p0, env);
        exp = exp->resolve_to(types.inst("int"), env);
        parms.push_back(TypeParm(exp->ct_value<int>()));
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
        throw error(p0, "Two Many Type Paramaters");
      }
      case TypeParm::DOTS: {
	abort(); // special case, should not happen
      }}
    }
    Type * inst_type = t->inst(parms);
    unsigned qualifiers = 0;
    if (p->flag("const"))    qualifiers |= QualifiedType::CONST;
    if (p->flag("volatile")) qualifiers |= QualifiedType::VOLATILE;
    if (p->flag("restrict")) qualifiers |= QualifiedType::RESTRICT;
    if (qualifiers) {
      Vector<TypeParm> q_parms;
      q_parms.push_back(TypeParm(qualifiers));
      q_parms.push_back(TypeParm(inst_type));
      return types.find(".qualified")->inst(q_parms);
    } else {
      return inst_type;
    }
  }
  
  Function * FunctionSymbol::inst(TypeSymbolTable types, Fun * f) const {
    Vector<TypeParm> p;
    p.clear();
    p.push_back(TypeParm(TypeParm::TUPLE, f->parms));
    p.push_back(TypeParm(f->type));
    return static_cast<Function *>(inst(p));
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
    const Type * t = unify(rule, x->type, y->type);
    //printf("UNIFY %d to \"\%s\"\n", x->parse_->str().source->get_pos(x->parse_->str().begin).line, ~t->to_string());
    //if (t != xt) {x = new Cast(x, t);}
    //if (t != yt) {y = new Cast(y, t);}
    x = x->resolve_to(t, env);
    y = y->resolve_to(t, env);
    return t;
  }

  class C_TypeRelation : public TypeRelation {
  public:
    Exp * resolve_to(Exp * exp, const Type * type, Environ & env, CastType rule) const;
    Exp * to_effective(Exp * exp, Environ & env) const;
    Exp * def_arg_prom(Exp * exp, Environ & env) const;
    void resolve_assign(Exp * & lhs, Exp * & rhs, Environ & env) const;
    const Type * unify(int rule, const Type *, const Type *) const;
  };

  Exp * C_TypeRelation::resolve_to(Exp * orig_exp, const Type * type, Environ & env, CastType rule) const {
    static int i = -1;
    ++i;
    Exp * exp = orig_exp;
    const Type * have = exp->type->unqualified;
    const Type * need = type->unqualified;
    
    const Reference * h_r = dynamic_cast<const Reference *>(have);
    const Reference * n_r = dynamic_cast<const Reference *>(need);
    if (h_r) {
      exp = from_ref(exp, env);
      have = exp->type->unqualified;
    }
    if (n_r) {
      if (!exp->lvalue) 
        throw error(orig_exp->parse_, 
                    "Can not take a reference of temporary (yet) in conversion from \"%s\" to \"%s\"",
                    ~orig_exp->type->to_string(), ~type->to_string());
      if (have != n_r->subtype->unqualified) 
        goto fail;
      if (exp->type->read_only && !n_r->subtype->read_only)
        throw error(orig_exp->parse_, "Conversion from \"%s\" to \"%s\" disregards const qualifier\n", 
                    ~orig_exp->type->to_string(), ~type->to_string());
      return to_ref(exp, env);
    }

    if (have == need) return exp;
    // FIXME: Is this right?

    if (rule == Explicit) // FIXME: This isn't always legal
      return new Cast(exp, type);

    if (have->is(NUMERIC_C) && need->is(NUMERIC_C))
      return new Cast(exp, type); 

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
      return new Cast(exp, type); 

    // deal with pointer types
    {
      const Pointer * n_p = dynamic_cast<const Pointer *>(need);
      const Type * n_subtype = n_p ? n_p->subtype : 0;
      if (!n_subtype) goto fail;
      if (exp->type->is_null) return exp;
      // FIXME: This probably isn't right
      if (dynamic_cast<const Function *>(have)) {
        return exp;
      }
      const Pointer * h_p = dynamic_cast<const Pointer *>(have);
      const Array   * h_a = dynamic_cast<const Array *>(have);
      const Type * h_subtype = h_p ? h_p->subtype : h_a ? h_a->subtype : 0;
      if (!h_subtype) goto fail;
      if (dynamic_cast<const Function *>(h_subtype->unqualified) // FIXME: Hack
          && dynamic_cast<const Function *>(n_subtype->unqualified))
        return new Cast(exp, type);
      if (h_subtype->unqualified == n_subtype->unqualified ||
          dynamic_cast<const Void *>(h_subtype->unqualified) ||
          dynamic_cast<const Void *>(n_subtype->unqualified) /* this one is C only */)  
      {
        if (h_subtype->read_only && !n_subtype->read_only)
          throw error(orig_exp->parse_, "Conversion from \"%s\" to \"%s\" disregards const qualifier\n", 
                      ~have->to_string(), ~need->to_string());
        else
          return exp;
      } else if (dynamic_cast<const UserType *>(h_subtype->unqualified) &&
                 dynamic_cast<const UserType *>(n_subtype->unqualified) &&
                 h_subtype->is(n_subtype->category)) 
      { // this is C++ only
        if (h_subtype->read_only && !n_subtype->read_only)
          throw error(orig_exp->parse_, "Conversion from \"%s\" to \"%s\" disregards const qualifier\n", 
                      ~have->to_string(), ~need->to_string());
        else 
          return cast_up(exp, n_subtype, env);
      } else {
        goto fail;
      }
    }
    //abort();

  fail:
    //abort();
    throw error(orig_exp->parse_, "%d Mismatch Types expected \"%s\" but got \"%s\"", i,
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
      throw error(lhs->parse_, "Can not be used as lvalue");
    //throw error(parse_->arg(1), "Can not be used as lvalue");
    if (lhs->type->read_only) 
      throw error (lhs->parse_, "Assignment to read-only location");
    const Reference * lhs_r = dynamic_cast<const Reference *>(lhs->type->unqualified);
    if (lhs_r) {
      lhs = from_ref(lhs, env);
    }
    try {
      rhs = rhs->resolve_to(lhs->type, env);
    } catch (Error * err) {
      StringBuf buf;
      buf = err->msg;
      buf.printf(" in assignment of %s.", ~lhs->parse_->sample_w_loc());
      err->msg = buf.freeze();
      throw err;
    }
  }

  const Type * C_TypeRelation::unify(int rule, const Type * x, const Type * y) const {
    // 3.6.1.8: Usual arithmetic conversions
    x = x->effective->unqualified;
    y = y->effective->unqualified;
    if (x == y) return x;
    {
      const Float * x0 = dynamic_cast<const Float *>(x);
      const Float * y0 = dynamic_cast<const Float *>(y); 
      if (x0 && y0) return x0->precision > y0->precision ? x0 : y0;
      if (x0) return x0;
      if (y0) return y0;
    } {
      const Int * x0 = dynamic_cast<const Int *>(x);
      const Int * y0 = dynamic_cast<const Int *>(x);
      assert(x0 && y0);
      if (x0->signed_ == y0->signed_) return x0->rank > y0->rank ? x0 : y0;
      const Int * u = x0->signed_ == Int::UNSIGNED ? x0 : y0;
      const Int * s = x0->signed_ == Int::SIGNED   ? x0 : y0;
      assert (u != s);
      if (u->rank >= s->rank) return u;
      if (s->min <= u->min && s->max >= u->max) return s;
      abort(); // FIXME: From standard: Otherwise, both operands are
               // converted to the unsigned integer type corresponding
               // to the type of the operand with signed integer type.

    }
  }

  TypeRelation * new_c_type_relation() {
    return new C_TypeRelation();
  }

  void add_c_int(TypeSymbolTable types, String n);

  void create_c_types(TypeSymbolTable types) {
    add_simple_type(types, ".int8", new_signed_int(1));
    add_simple_type(types, ".uint8", new_unsigned_int(1));
    add_simple_type(types, ".int16", new_signed_int(2));
    add_simple_type(types, ".uint16", new_unsigned_int(2));
    add_simple_type(types, ".int32", new_signed_int(4));
    add_simple_type(types, ".uint32", new_unsigned_int(4));
    add_simple_type(types, ".int64", new_signed_int(8));
    add_simple_type(types, ".uint64", new_unsigned_int(8));

    add_c_int(types, "signed-char");
    add_c_int(types, "short");
    add_c_int(types, "int");
    add_c_int(types, "long");
    add_c_int(types, "long-long");

    //types.add("signed short", types.find("short"));
    //types.add("signed int", types.find("int"));
    //types.add("signed long", types.find("long"));
    //types.add("signed long long", types.find("long long"));

    add_c_int(types, "unsigned-char");
    add_c_int(types, "unsigned-short");
    add_c_int(types, "unsigned-int");
    add_c_int(types, "unsigned-long");
    add_c_int(types, "unsigned-long-long");

    types.add("unsigned", types.find("unsigned-int"));

    add_c_int(types, "char");

    VOID_T = new Void();
    add_simple_type(types, "void", static_cast<SimpleTypeInst *>(VOID_T));

    add_simple_type(types, "float", new Float(Float::SINGLE));
    add_simple_type(types, "double", new Float(Float::DOUBLE));
    add_simple_type(types, "long-double", new Float(Float::LONG));

    add_simple_type(types, "<bool>", new AliasT(types.inst("int")));

    types.add("<void>", types.find("void"));

    types.add(".size", types.find("unsigned-long"));
    types.add(".ptrdiff", types.find("long"));

    types.add_name(".ptr", new PointerSymbol);
    types.add_name(".ref", new ReferenceSymbol);
    //types.add_name("<const>", new ConstSymbol);
    types.add_name(".array", new ArraySymbol);
    types.add_name(".fun", new FunctionSymbol);
    types.add_name(".", new TupleSymbol);
    types.add_name(".qualified", new QualifiedTypeSymbol);
    types.add_name(".zero", new ZeroTypeSymbol);
    types.add_name(".typeof", new TypeOfSymbol);
    add_simple_type(types, "__builtin_va_list", new Void());
    //add_simple_type(types, "Match", new Void());
    //add_simple_type(types, "Syntax", new Void());
    //add_simple_type(types, "Mark", new Void());
    //add_simple_type(types, "Context", new Void());
  }

  Type * TypeSymbolTable::ct_const(const Type * t) {
    return inst(".qualified", TypeParm(QualifiedType::CT_CONST), TypeParm(t));
  }

  void StructT::finalize_hook() {
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

  void UnionT::finalize_hook() {
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

  void EnumT::finalize_hook() {
    defined = true;
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
  
  void add_c_int(TypeSymbolTable types, String n) {
    add_simple_type(types, n, new Int(static_cast<const Int *>(types.inst(c_type_to_exact(n)))));
  }

}

