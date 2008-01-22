#ifndef TYPE__HPP
#define TYPE__HPP

#include <typeinfo>
#include <stdint.h>

#include "util.hpp"
#include "symbol_table.hpp"
#include "string_buf.hpp"

struct Parse;

namespace AST {

  // NOTE: In order to avoid expensive comparasions between types each
  // type object must only be allocated once, this way an cheap pointer
  // comparison can be used

  class TypeSymbol;
  class TypeInst;
  typedef TypeInst Type;
  class Tuple;

  struct AST;

  //struct ExpectedType {
  //  Vector<TypeCategory *> expected;
  //  Type * got;
  //};

  struct TypeCategory {
    String name;
    TypeCategory * parent;
    TypeCategory(String n, TypeCategory * p) : name(n), parent(p) {}
  };

  extern TypeCategory * const ANY_C;
  extern TypeCategory * const SCALAR_C;
  extern TypeCategory * const NUMERIC_C;
  extern TypeCategory * const INT_C;
  extern TypeCategory * const SIGNED_C;
  extern TypeCategory * const UNSIGNED_C;
  extern TypeCategory * const FLOAT_C;
  extern TypeCategory * const POINTER_C;
  extern TypeCategory * const ARRAY_C;
  extern TypeCategory * const FUN_C;
  extern TypeCategory * const UNKNOWN_C;

  struct TypeParm : public gc {
    enum What {NONE, TYPE, INT, TUPLE, EXP};
    What what;
    union {
      const Type * as_type;
      int          as_int;
      AST        * as_exp;
    };
    String name;
    bool is_type() {return what == TYPE || what == TUPLE;}
    explicit TypeParm() : what(NONE) {}
    explicit TypeParm(const Type * t, String n = String()) : what(TYPE), as_type(t), name(n) {}
    explicit TypeParm(What w, const Type * t, String n = String()) : what(w), as_type(t), name(n) {}
    explicit TypeParm(int i, String n = String()) : what(INT), as_int(i), name(n) {}
    explicit TypeParm(AST * exp, String n = String()) : what(EXP), as_exp(exp), name(n) {}
    void to_string(StringBuf & buf) const;
  };

  struct TypeSymbolTable : public SymbolTable<const TypeSymbol *> {
    typedef SymbolTable<const TypeSymbol *> Base;
    TypeSymbolTable(TypeSymbolTable * p = 0) : Base(p) {}
    const TypeSymbol * lookup(SymbolKey k) {
      if (!Base::exists(k)) return 0;
      return Base::lookup(k);
    }
    Type * inst(String n, Vector<TypeParm> &);
    Type * inst(String n) {
      Vector<TypeParm> dummy;
      return inst(n, dummy);
    }
    Type * inst(String n, const Type * t) {
      Vector<TypeParm> parms;
      parms.push_back(TypeParm(t));
      return inst(n, parms);
    }
    void add_name(SymbolKey k, TypeSymbol * t);
  };

  static inline bool operator==(TypeParm lhs, TypeParm rhs) {
    if (lhs.what != rhs.what) return false;
    switch (lhs.what) {
    case TypeParm::NONE:
      return true;
    case TypeParm::TYPE:
    case TypeParm::TUPLE:
      return lhs.as_type == rhs.as_type && lhs.name == rhs.name;
    case TypeParm::INT:
      return lhs.as_int == rhs.as_int;
    case TypeParm::EXP:
      return lhs.as_exp == rhs.as_exp;
    }
    abort(); // this should't happen
  }
  
  static inline bool operator!=(TypeParm lhs, TypeParm rhs) {
    return !(lhs == rhs);
  }

  class PrintInst : public gc_cleanup {
  public:
    virtual void to_string(const TypeInst &, StringBuf & buf) const = 0;
    // declaration is needed to handle C types correctly, perhaps this is not 
    // the best place for it
    virtual void declaration(String var, const TypeInst &, StringBuf & buf) const = 0;
    virtual ~PrintInst() {}
  };

  class GenericPrintInst : public PrintInst { 
  public:
    void to_string(const TypeInst &, StringBuf & buf) const;
    void declaration(String var, const TypeInst &, StringBuf & buf) const;
  };

  class CPrintInst : public PrintInst { 
  public:
    void to_string(const TypeInst &, StringBuf & buf) const;
    void declaration(String var, const TypeInst & t, StringBuf & buf, bool parentheses) const;
    void declaration(String var, const TypeInst & t, StringBuf & buf) const {
      declaration(var, t, buf, false);
    }
  };

  extern PrintInst const * const generic_print_inst;
  extern PrintInst const * const c_print_inst;

  struct AST;

  class TypeRelation : public gc_cleanup {
  public:
    virtual AST * resolve_to(AST * exp, const Type * type) const = 0;
    virtual const Type * unify(int rule, const Type *, const Type *) const = 0;
    const Type * unify(int rule, AST * &, AST * &);
    virtual AST * cast(int rule, AST * exp, const Type * type) const = 0;
    //virtual const Type * promote(AST * exp) const = 0;
    virtual ~TypeRelation() {}
  };

  class TypeInst;
  
  class TypeSymbol : public gc_cleanup {
  public:
    Parse * parse;
    String name;
    const PrintInst * print_inst;
    TypeSymbol() : parse(), print_inst(c_print_inst) {}
    virtual Type * inst(Vector<TypeParm> & d) const = 0;
    //virtual Type * inst(const Parse *) const = 0;
    virtual unsigned required_parms() const = 0;
    virtual TypeParm::What parm(unsigned i) const = 0; // return TypeParm::NONE if off end
    virtual ~TypeSymbol() {}
  };

  static inline bool category_in(const TypeCategory * x, const TypeCategory * y) {
    if (x == y) return true;
    if (x->parent == 0) return false;
    return category_in(x->parent, y);
  }

  class TypeInst : public gc_cleanup {
  public:
    TypeCategory * category;
    const TypeSymbol * type_symbol;
    unsigned size;
    unsigned align;
    bool addressable;
    bool read_only;
    bool is_null;
    TypeInst(TypeCategory * c = UNKNOWN_C, unsigned sz = NPOS, unsigned a = NPOS)
      : category(c), size(sz), align(a == NPOS ? sz : a), 
        addressable(false), read_only(false), is_null(false) {}
    TypeInst(const TypeInst * p) 
      : category(p->category), size(p->size), align(p->align), 
        addressable(p->addressable), read_only(p->read_only), is_null(p->is_null) {}
    void to_string(StringBuf & buf) const {type_symbol->print_inst->to_string(*this, buf);}
    String to_string() const {StringBuf buf; to_string(buf); return buf.freeze();}
    bool is(const TypeCategory * other) const {
      return category_in(category, other);
    }
    const TypeInst * root; // for typedef, the root type, recursively resolved
    const TypeInst * unqualified; // unqualified version of root, _not_ recursively resolved
    void finalize() // should be called just before it added
                            // to the type symbol cache (or whatever)
      {finalize_hook(); root = find_root(); unqualified = root->find_unqualified();}
    virtual unsigned num_parms() const = 0;
    virtual TypeParm parm(unsigned i) const = 0;
    virtual ~TypeInst() {}
  protected:
    virtual void finalize_hook() {} 
    virtual const TypeInst * find_root() {return this;}
    virtual const TypeInst * find_unqualified() const {return this;}
  };

  bool operator==(const TypeInst & lhs, const Vector<TypeParm> & rhs);
  static inline bool operator!=(const TypeInst & lhs, const Vector<TypeParm> & rhs) {
    return !(lhs == rhs);
  }

  bool operator==(const TypeInst & lhs, const TypeInst & rhs);
  static inline bool operator!=(const TypeInst & lhs, const TypeInst & rhs) {
    return !(lhs == rhs);
  }

  //
  //
  //

  class SimpleTypeInst : public TypeInst {
  public:
    SimpleTypeInst(TypeCategory * c = UNKNOWN_C, unsigned sz = NPOS, unsigned a = NPOS) 
      : TypeInst(c, sz, a) {}
    SimpleTypeInst(const Type * p) : TypeInst(p) {}
    unsigned num_parms() const {return 0;}
    TypeParm parm(unsigned i) const {abort();}
  };

  class SimpleTypeSymbol : public TypeSymbol {
  public:
    SimpleTypeSymbol(SimpleTypeInst * t) : type(t) {
      t->type_symbol = this;
      t->finalize();
    }
    SimpleTypeInst * type;
    SimpleTypeInst * inst(Vector<TypeParm> & d) const {
      assert(d.empty());
      return type;
    }
    unsigned required_parms() const {return 0;}
    TypeParm::What parm(unsigned i) const {return TypeParm::NONE;}
  };

  static inline void add_simple_type(TypeSymbolTable * sym, SymbolKey name, SimpleTypeInst * type) {
    SimpleTypeSymbol * t = new SimpleTypeSymbol(type);
    sym->add_name(name, t);
  }

  //
  //
  //

  class ParmTypeInst /* Parametrized Type Instance */ : public TypeInst {
  public:
    ParmTypeInst(TypeCategory * c = UNKNOWN_C, unsigned sz = NPOS, unsigned a = NPOS) 
      : TypeInst(c, sz, a) {}
    ParmTypeInst(const Type * p) : TypeInst(p) {}
    virtual unsigned num_parms() const = 0;
    virtual TypeParm parm(unsigned i) const = 0;
    const Type * find_root() {
      Vector<TypeParm> parms;
      unsigned sz = num_parms();
      for (unsigned i = 0; i != sz; ++i) {
        TypeParm p = parm(i);
        if (p.is_type())
          parms.push_back(TypeParm(p.what, p.as_type->root));
        else
          parms.push_back(p);
      }
      return type_symbol->inst(parms);
    }
  };

  class InstCache {
  public:
    Vector<ParmTypeInst *> d;
    ParmTypeInst * find(Vector<TypeParm> & to_find) const {
      Vector<ParmTypeInst *>::const_iterator i = d.begin(), e = d.end();
      for (; i != e; ++i) {
        if (**i == to_find) return *i;
      }
      return 0;
    }
    void insert(ParmTypeInst * to_insert) {
      d.push_back(to_insert);
    }
  };

  class ParmTypeSymbol : public TypeSymbol {
  public:
    mutable InstCache inst_cache;
    ParmTypeInst * inst(Vector<TypeParm> & d) const {
      ParmTypeInst * r = inst_cache.find(d);
      if (r) return r;
      ParmTypeInst * res = inst_p(d);
      res->type_symbol = this;
      inst_cache.insert(res); // FIXME: hack
      res->finalize();
      return res;
    }
    virtual ParmTypeInst * inst_p(Vector<TypeParm> &) const = 0;
    virtual unsigned required_parms() const = 0;
    virtual TypeParm::What parm(unsigned i) const = 0; // return TypeParm::NONE if off end
  };

  //
  //
  //

  class ZeroT : public ParmTypeInst {
  public:
    ZeroT(const Type * st) : ParmTypeInst(st), of(st) {
      if (of->is(INT_C)) is_null = true;
      printf("@@@ZERO T@@@ %p %d\n", this, is_null);
    }
    const Type * of;
    unsigned num_parms() const {return 1;}
    TypeParm parm(unsigned) const {return TypeParm(of);}
  protected:
    const Type * find_root() {
      return of->root; 
    }
  };

  class ZeroTypeSymbol : public ParmTypeSymbol {
  public:
    ParmTypeInst * inst_p(Vector<TypeParm> & p) const {
      assert(p.size() == 1);
      assert(p[0].what == TypeParm::TYPE);
      return new ZeroT(p[0].as_type);
    }
    unsigned required_parms() const {return 1;}
    TypeParm::What parm(unsigned i) const {
      if (i == 0) return TypeParm::TYPE;
      else return TypeParm::NONE;
    }
  };

  //
  //
  //

  class Tuple : public ParmTypeInst {
  public:
    struct Parm {
      const Type * type;
      String name;
      Parm() {}
      Parm(const Type * t, String n) : type(t), name(n) {}
    };
    typedef Vector<Parm> Parms;
    Parms parms;
    virtual unsigned num_parms() const {return parms.size();}
    virtual TypeParm parm(unsigned i) const {return TypeParm(parms[i].type, parms[i].name);}
  };

  class TupleSymbol : public ParmTypeSymbol {
  public:
    unsigned required_parms() const {return 0;}
    virtual TypeParm::What parm(unsigned i) const {return TypeParm::TYPE;}
    virtual ParmTypeInst * inst_p(Vector<TypeParm> & p) const {
      Tuple * r = new Tuple();
      for (int i = 0; i != p.size(); ++i) {
        assert(p[i].what == TypeParm::TYPE);
        r->parms.push_back(Tuple::Parm(p[i].as_type, p[i].name));
      }
      return r;
    }
  };

  //
  //
  //

  struct ParseEnviron;
  Type * parse_type(TypeSymbolTable * types, const Parse * p, ParseEnviron & env);

  //
  //
  //

  class Void : public SimpleTypeInst {
  public:
    Void() : SimpleTypeInst(0, 1) {}
  };

  //
  //
  //

  //template <typename T>
  //static inline T max(T x, T y) {
  //  return x > y ? x : y;
  //}

  //
  //
  //

  class Int : public SimpleTypeInst {
  public:
    enum Overflow {UNDEFINED, MODULE, SATURATED, EXCEPTION};
    enum Signed {UNSIGNED = 0, SIGNED = 1};
    Int(int64_t mn, uint64_t mx, Overflow o, unsigned sz)
      : SimpleTypeInst(INT_C, sz), min(mn), max(mx), signed_(mn < 0 ? SIGNED : UNSIGNED), overflow(o) {}
    int64_t  min;
    uint64_t max;
    Signed   signed_;
    Overflow overflow;
    int      rank;
    //unsigned calc_size() {
    //  if (min < 0) return ceill(log2l(max)/8);
    //  else return ceill((max(log2l(llabs(min)), log2l(max)) + 1)/8);
    //}
  };
  
  static inline Int * new_signed_int(unsigned sz) {
    uint64_t sz0 = sz;
    return new Int(-(1<<(sz0*8-1)), (1<<(sz0*8-1))-1, Int::UNDEFINED, sz);
  }
  
  static inline Int * new_unsigned_int(unsigned sz) {
    uint64_t sz0 = sz;
    return new Int(0, (1<<(sz0*8))-1, Int::MODULE, sz);
  }

  class Float : public SimpleTypeInst {
  public:
    enum Precision {SINGLE, DOUBLE, LONG} precision;
    Float(Precision p) : SimpleTypeInst(FLOAT_C, p == SINGLE ? 4 : DOUBLE ? 8 : 16) {}
  };

  //
  //
  //

  static const unsigned POINTER_SIZE = sizeof(void *);

  //
  //
  //

  class PointerLike : public ParmTypeInst {
  public:
    PointerLike(TypeCategory * c, const Type * st, 
                unsigned sz = NPOS, unsigned a = NPOS)
      : ParmTypeInst(c, sz, a), subtype(st) {}
    const Type * subtype;
  };

  class Pointer : public PointerLike {
  public:
    Pointer(const Type * t) : PointerLike(POINTER_C, t, POINTER_SIZE) {}
    unsigned num_parms() const {return 1;}
    TypeParm parm(unsigned) const {return TypeParm(subtype);}
  };

  class PointerSymbol : public ParmTypeSymbol {
  public:
    ParmTypeInst * inst_p(Vector<TypeParm> & p) const {
      assert(p.size() == 1);
      assert(p[0].what == TypeParm::TYPE);
      return new Pointer(p[0].as_type);
    }
    unsigned required_parms() const {return 1;}
    TypeParm::What parm(unsigned i) const {
      if (i == 0) return TypeParm::TYPE;
      else return TypeParm::NONE;
    }
  };


  //
  // QualifiedType is a type with one or more qualifiers which restrict there use
  //   when used as an lvalue (const), or control optimizations when the variable is
  //   used.
  //

  class QualifiedType : public ParmTypeInst {
  public:
    enum {CONST = 1, VOLATILE = 2, RESTRICT = 4};
    const Type * subtype;
    unsigned qualifiers; // BIT FIELD
  public:
    QualifiedType(unsigned q, const Type * t) 
      : ParmTypeInst(t->category, t->size, t->align),
        subtype(t), qualifiers(q) {if (q & CONST) read_only = true;}
    unsigned num_parms() const {return 2;}
    TypeParm parm(unsigned i) const {return i == 0 ? TypeParm(qualifiers) : TypeParm(subtype);}
    const Type * find_unqualified() const {return subtype;}
  };

  class QualifiedTypeSymbol : public ParmTypeSymbol {
  public:
    ParmTypeInst * inst_p(Vector<TypeParm> & p) const {
      assert(p.size() == 2);
      assert(p[0].what == TypeParm::INT);
      assert(p[1].what == TypeParm::TYPE);
      unsigned qualifiers = p[0].as_int;
      const Type * subtype = p[1].as_type;
      if (const QualifiedType * t = dynamic_cast<const QualifiedType *>(subtype)) {
        qualifiers |= t->qualifiers;
        subtype = t->subtype;
      }
      return new QualifiedType(qualifiers, subtype);
    }
    unsigned required_parms() const {return 2;}
    TypeParm::What parm(unsigned i) const {
      if (i == 0) return TypeParm::INT;
      if (i == 1) return TypeParm::TYPE;
      else return TypeParm::NONE;
    }
  };

  //
  //
  //
  
  class Array : public PointerLike {
  public:
    Array(const Type * t, unsigned sz) : PointerLike(ARRAY_C, t, t->size * sz, t->align), length(sz) {}
    unsigned length;
    unsigned num_parms() const {return 2;}
    TypeParm parm(unsigned i) const {return i == 0 ? TypeParm(subtype) : TypeParm(length);}
  };

  class ArraySymbol : public ParmTypeSymbol {
  public:
    ParmTypeInst * inst_p(Vector<TypeParm> & p) const {
      assert(p.size() == 2);
      assert(p[0].what == TypeParm::TYPE);
      assert(p[1].what == TypeParm::INT);
      return new Array(p[0].as_type, p[1].as_int);
    }
    unsigned required_parms() const {return 2;}
    TypeParm::What parm(unsigned i) const {
      switch (i) {
      case 0: return TypeParm::TYPE;
      case 1: return TypeParm::INT;
      default: return TypeParm::NONE;
      }
    }
  };

  //
  //
  //

  struct Member {
    String name;
    const Type * subtype;
    Member(String n, const Type * s) : name(n), subtype(s) {}
  };

  class StructUnionT : public SimpleTypeInst {
  public:
    String what;
    String name;
    Vector<Member> members;
    StructUnionT(String w) : what(w) {}
    struct ParseEnviron * env;
  };

  class StructT : public StructUnionT {
  public:
    StructT() : StructUnionT("struct") {}
    void finalize_hook() {} // FIXME: Implement
  };

  class UnionT : public StructUnionT {
  public:
    UnionT() : StructUnionT("union") {}
    void finalize_hook() {} // FIXME: Implement
  };
  
  //
  //
  //

  class AliasT : public SimpleTypeInst {
  public:
    AliasT(const Type * st) : SimpleTypeInst(st), of(st) {}
    const Type * of;
  protected:
    const Type * find_root() {
      return of->root; 
    }
  };

  //
  //
  //

  struct AST;

  class TypeOfSymbol : public TypeSymbol {
  public:
    TypeOfSymbol() {}
    Type * inst(Vector<TypeParm> & d) const;
    unsigned required_parms() const {return 1;}
    TypeParm::What parm(unsigned i) const {return i == 0 ? TypeParm::EXP : TypeParm::NONE;}
  };

  class TypeOf : public SimpleTypeInst {
  public:
    inline TypeOf(AST * a);
    AST * of_ast;
    const Type * of;
  protected:
    const Type * find_root() {
      return of->root; 
    }
  };

  //
  //
  //

  class Fun;

  class FunctionPtr : public ParmTypeInst {
  public:
    FunctionPtr(const Tuple * p, const Type * r)
      : ParmTypeInst(FUN_C), parms(p), ret(r) {}
    const Tuple * parms;
    const Type * ret;
    unsigned num_parms() const {return 2;}
    TypeParm parm(unsigned i) const 
      {return i == 0 ? TypeParm(TypeParm::TUPLE, parms) : TypeParm(ret);}
//     void to_string_(StringBuf & buf) const {
//       unsigned sz = parms.size();
//       if (sz > 0) {
//         buf += "(";
//         int i = 0;
//         for (;;) {
//           parms[i]->to_string_(buf);
//           ++i;
//           if (i == sz) break;
//           buf += ", ";
//         }
//         buf += ")";
//       } else {
//         buf += "()";
//       }
//       buf += " -> ";
//       ret->to_string_(buf);
//     }
  };

  class FunctionPtrSymbol : public ParmTypeSymbol {
  public:
    virtual unsigned required_parms() const {return 2;}
    virtual TypeParm::What parm(unsigned i) const {
      switch (i) {
      case 0: return TypeParm::TUPLE; // parms
      case 1: return TypeParm::TYPE;
      default: return TypeParm::NONE;
      }
    }
    ParmTypeInst * inst_p(Vector<TypeParm> & p) const {
      assert(p.size() == 2);
      assert(p[0].what == TypeParm::TUPLE);
      assert(p[0].as_type);
      assert(p[1].what == TypeParm::TYPE);
      return new FunctionPtr(static_cast<const Tuple *>(p[0].as_type), p[1].as_type);
    }
    using ParmTypeSymbol::inst;
    FunctionPtr * inst(TypeSymbolTable * types, Fun * f) const;
  };


  TypeRelation * new_c_type_relation();
  void create_c_types(TypeSymbolTable * types);

}



/*

  pointer: % *
  const:   % const
  array:   %1 [%2]
*/

#endif
