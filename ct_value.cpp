#include "ct_value-impl.hpp"

using namespace ast;

namespace {

  struct CT_Value_Map {
    String type;
    CT_Value_Base * ct_value;
  };

  template <template <typename> class W> 
  const CT_Value_Base * int_op_ct_value(const Type * t) {
    static CT_Value_Map map[] = {
      {".uint8", new W<uint8_t>},
      {".uint16", new W<uint16_t>},
      {".uint32", new W<uint32_t>},
      {".uint64", new W<uint64_t>},
      {".int8", new W<int8_t>},
      {".int16", new W<int16_t>},
      {".int32", new W<int32_t>},
      {".int64", new W<int64_t>}      
    };
    static CT_Value_Map * map_end = map + sizeof(map)/sizeof(CT_Value_Map);
    String n = t->ct_type_name();
    for (const CT_Value_Map * i = map; i != map_end; ++i) {
      if (i->type == n) return i->ct_value;
    }
    return NULL;
  }

  template <template <typename> class W, template <typename> class F>
  struct OCTV_Proxy {
    template <typename T>
    struct Type : public W<F<T> > {};
  };
 
  template <template <typename> class W> 
  const CT_Value_Base * op_ct_value(const Type * t) {
    static CT_Value_Map map[] = {
      {"float", new W<float>},
      {"double", new W<double>},
      {"long double", new W<long double>},
    };
    static CT_Value_Map * map_end = map + sizeof(map)/sizeof(CT_Value_Map);
    String n = t->ct_type_name();
    for (const CT_Value_Map * i = map; i != map_end; ++i) {
      if (i->type == n) return i->ct_value;
    }
    return int_op_ct_value<W>(t);
  }

  template <template <typename> class W, template <typename> class F> 
  inline const CT_Value_Base * int_op_ct_value(const Type * t) {
    return int_op_ct_value<OCTV_Proxy<W,F>::template Type>(t);
  }

  template <template <typename> class W, template <typename> class F> 
  inline const CT_Value_Base * op_ct_value(const Type * t) {
    return op_ct_value<OCTV_Proxy<W,F>::template Type>(t);
  }

}

namespace ast {

  CT_Value_Base * new_literal_ct_value(const Syntax * vp, const Type * & t, Environ & env) {
    const Int * it = dynamic_cast<const Int *>(t->unqualified);
    // FIXME: Need to promote type as indicated in the standard if
    //   the specified type is too small for the literal
    // FIXME: Do I need to make into ct_const type
    if (it->min == 0) {
      const char * s = ~*vp;
      char * e = (char *)s;
      unsigned long long value = strtoull(s, &e, 0);
      if (*e) throw error(vp, "Expected Integer");
      if (value == 0) t = env.types.inst(".zero", t);
      String n = it->ct_type_name();
      if (n == ".uint8")
        return new Literal_Value<uint8_t>(value);
      else if (n == ".uint16")
        return new Literal_Value<uint16_t>(value);
      else if (n == ".uint32")
        return new Literal_Value<uint32_t>(value);
      else if (n == ".uint64")
        return new Literal_Value<uint64_t>(value);
      else
        abort();
    } else {
      const char * s = ~*vp;
      char * e = (char *)s;
      long long value = strtoll(s, &e, 0);
      if (*e) throw error(vp, "Expected Integer");
      if (value == 0) t = env.types.inst(".zero", t);
      String n = it->ct_type_name();
      if (n == ".int8")
        return new Literal_Value<int8_t>(value);
      else if (n == ".int16")
        return new Literal_Value<int16_t>(value);
      else if (n == ".int32")
        return new Literal_Value<int32_t>(value);
      else if (n == ".int64")
        return new Literal_Value<int64_t>(value);
      else
        abort();
    }
  }

  CT_Value_Base * new_float_ct_value(const Syntax * vp, const Type * & t, Environ & env) {
    // FIXME: Do I need to make into ct_const type
    String n = t->ct_type_name();
    const char * s = ~*vp;
    char * e = (char *)s;
    long double value = strtold(s, &e);
    if (*e) throw error(vp, "Expected Number");
    if (n == "float")
      return new Literal_Value<float>(value);
    else if (n == "double")
      return new Literal_Value<double>(value);
    else if (n == "long double")
      return new Literal_Value<long double>(value);
    else
      abort();
  }

  template <typename T> 
  struct EIf_CT_Value : public CT_Value<T> {
    T value(const AST * a) const {
      const EIf * eif = dynamic_cast<const EIf *>(a);
      if (eif->exp->ct_value<target_bool>())
        return eif->if_true->ct_value<T>();
      else
        return eif->if_false->ct_value<T>();
    }
  };

  const CT_Value_Base * eif_ct_value(const Type * t) {
    return op_ct_value<EIf_CT_Value>(t);
  }

  template <typename F> 
  struct UnOp_CT_Value : public CT_Value<typename F::result_type> {
    typename F::result_type value(const AST * a) const {
      const UnOp * b = dynamic_cast<const UnOp *>(a);
      typename F::argument_type x = b->exp->ct_value<typename F::argument_type>();
      return F()(x);
    }
  };

  const CT_Value_Base * neg_ct_value(const Type * type) {
    return op_ct_value<UnOp_CT_Value, std::negate>(type);
  }

  template <typename T>
  struct Compliment_F : public std::unary_function<T,T> {
      T operator()(T x) {return ~x;}
  };

  const CT_Value_Base * compliment_ct_value(const Type * type) {
    return int_op_ct_value<UnOp_CT_Value, Compliment_F>(type);
  }

  template <typename T>
  struct Not_F : public std::unary_function<T,T> {
      T operator()(T x) {return !x;}
  };

  const CT_Value_Base * not_ct_value(const Type * type) {
    return op_ct_value<UnOp_CT_Value, Not_F>(type);
  }

  template <typename F> 
  struct BinOp_CT_Value : public CT_Value<typename F::result_type> {
    typename F::result_type value(const AST * a) const {
      const BinOp * b = dynamic_cast<const BinOp *>(a);
      typename F::first_argument_type x = b->lhs->ct_value<typename F::first_argument_type>();
      typename F::second_argument_type y = b->rhs->ct_value<typename F::second_argument_type>();
      return F()(x, y);
    }
  };

  template <typename F> 
  struct Comp_CT_Value : public CT_Value<target_bool> {
    int value(const AST * a) const {
      const BinOp * b = dynamic_cast<const BinOp *>(a);
      typename F::first_argument_type x = b->lhs->ct_value<typename F::first_argument_type>();
      typename F::second_argument_type y = b->rhs->ct_value<typename F::second_argument_type>();
      return F()(x, y);
    }
  };
  
  template <typename T> 
  struct Ptr_Plus_CT_Value : public CT_Value<CT_Ptr> {
    CT_Ptr value(const AST * a) const {
      const BinOp * plus = dynamic_cast<const BinOp *>(a);
      CT_Ptr   lhs = plus->lhs->ct_value<CT_Ptr>();
      T        sz  = dynamic_cast<const PointerLike *>(plus->lhs->type)->subtype->size();
      T        rhs = plus->rhs->ct_value<T>();
      return CT_Ptr(lhs.val + sz*rhs);
    }
  };

  template <typename T> 
  struct Plus_Ptr_CT_Value : public CT_Value<CT_Ptr> {
    CT_Ptr value(const AST * a) const {
      const BinOp * plus = dynamic_cast<const BinOp *>(a);
      T        lhs = plus->lhs->ct_value<T>();
      T        sz  = plus->rhs->type->size();
      CT_Ptr   rhs = plus->rhs->ct_value<CT_Ptr>();
      return CT_Ptr(sz*lhs + rhs.val);
    }
  };

  const CT_Value_Base * plus_ct_value(const Type * type) {
    return op_ct_value<BinOp_CT_Value, std::plus>(type);
  }

  const CT_Value_Base * ptr_plus_ct_value(const Type * type) {
    return int_op_ct_value<Ptr_Plus_CT_Value>(type);
  }

  const CT_Value_Base * plus_ptr_ct_value(const Type * type) {
    return int_op_ct_value<Plus_Ptr_CT_Value>(type);
  }

  const CT_Value_Base * minus_ct_value(const Type * type) {
    return op_ct_value<BinOp_CT_Value, std::minus>(type);
  }
  
  const CT_Value_Base * times_ct_value(const Type * type) {
    return op_ct_value<BinOp_CT_Value, std::multiplies>(type);
  }

  const CT_Value_Base * div_ct_value(const Type * type) {
    return op_ct_value<BinOp_CT_Value, std::divides>(type);
  }

  const CT_Value_Base * mod_ct_value(const Type * type) {
    return int_op_ct_value<BinOp_CT_Value, std::modulus>(type);
  }

  template <typename T>
  struct BAnd_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x & y;}
  };
  const CT_Value_Base * band_ct_value(const Type * type) {
    return int_op_ct_value<BinOp_CT_Value, BAnd_F>(type);
  }

  template <typename T>
  struct BOr_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x | y;}
  };
  const CT_Value_Base * bor_ct_value(const Type * type) {
    return int_op_ct_value<BinOp_CT_Value, BOr_F>(type);
  }

  template <typename T>
  struct XOr_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x | y;}
  };
  const CT_Value_Base * xor_ct_value(const Type * type) {
    return int_op_ct_value<BinOp_CT_Value, XOr_F>(type);
  }

  template <typename T>
  struct LeftShift_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x | y;}
  };
  const CT_Value_Base * leftshift_ct_value(const Type * type) {
    return int_op_ct_value<BinOp_CT_Value, LeftShift_F>(type);
  }

  template <typename T>
  struct RightShift_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x | y;}
  };
  const CT_Value_Base * rightshift_ct_value(const Type * type) {
    return int_op_ct_value<BinOp_CT_Value, RightShift_F>(type);
  }

  const CT_Value_Base * eq_ct_value(const Type * type) {
    return op_ct_value<Comp_CT_Value, std::equal_to>(type);
  }

  const CT_Value_Base * ne_ct_value(const Type * type) {
    return op_ct_value<Comp_CT_Value, std::not_equal_to>(type);
  }

  const CT_Value_Base * lt_ct_value(const Type * type) {
    return op_ct_value<Comp_CT_Value, std::less>(type);
  }

  const CT_Value_Base * gt_ct_value(const Type * type) {
    return op_ct_value<Comp_CT_Value, std::greater>(type);
  }

  const CT_Value_Base * le_ct_value(const Type * type) {
    return op_ct_value<Comp_CT_Value, std::less_equal>(type);
  }

  const CT_Value_Base * ge_ct_value(const Type * type) {
    return op_ct_value<Comp_CT_Value, std::greater_equal>(type);
  }

}

namespace {

  template <typename To>
  struct Cast_CT_Value_Base : public CT_Value<To> {
    virtual To value_direct(const AST * t) const = 0;
  };

  template <typename From, typename To> 
  struct Cast_CT_Value_Direct : public Cast_CT_Value_Base<To> {
    To value_direct(const AST * exp) const {
      return static_cast<To>(exp->ct_value<From>());
    }
  };

  template <typename From, typename To>
  struct Cast_CT_Value : public  Cast_CT_Value_Direct<From, To> {
    To value(const AST * t) const {
      const Cast * c = dynamic_cast<const Cast *>(t);
      return Cast_CT_Value_Direct<From,To>::value_direct(c->exp);
    }
  };

  template <typename T>
  struct Cast_CT_Value<T,T> : public Cast_CT_Value_Base<T> {
    T value_direct(const AST * exp) const {
      // this makes no sense and could lead to infinite recursion
      abort();
    }
    T value(const AST * t) const {
      const Cast * c = dynamic_cast<const Cast *>(t);
      return c->exp->ct_value<T>();
    }
  };

  template <typename To>
  struct Cast_CT_Value_Direct<CT_Ptr,To> : public Cast_CT_Value_Base<To> {
    To value_direct(const AST * exp) const {
      return static_cast<To>(exp->ct_value<CT_Ptr>().val);
    }
  };

  template <typename From>
  struct Cast_CT_Value_Direct<From,CT_Ptr> : public Cast_CT_Value_Base<CT_Ptr> {
    CT_Ptr value_direct(const AST * exp) const {
      return CT_Ptr(static_cast<size_t>(exp->ct_value<From>()));
    }
  };

  template <typename To>
  struct Cast_CT_Value_Direct<CT_LValue,To> : public Cast_CT_Value_Base<To> {
    To value_direct(const AST * exp) const {
      abort();
    }
  };

  template <typename From>
  struct Cast_CT_Value_Direct<From,CT_LValue> : public Cast_CT_Value_Base<CT_LValue> {
    CT_LValue value_direct(const AST * exp) const {
      abort();
    }
  };

  template <>
  struct Cast_CT_Value_Direct<CT_LValue,CT_Ptr> : public Cast_CT_Value_Base<CT_Ptr> {
    CT_Ptr value_direct(const AST * exp) const {
      // it can only happen id exp is an array type
      return exp->ct_value<CT_LValue>().addr;
    }
  };

  template <>
  struct Cast_CT_Value_Direct<CT_Ptr,CT_LValue> : public Cast_CT_Value_Base<CT_LValue> {
    CT_LValue value_direct(const AST * exp) const {
      abort();
    }
  };

  struct Cast_CT_Value_Inner_Map {
    String to;
    const CT_Value_Base * cast;
  };

  template <typename From>
  struct Cast_CT_Value_Group {
    static Cast_CT_Value_Inner_Map map[];
    static Cast_CT_Value_Inner_Map * map_end;
  };

  template <typename From>
  Cast_CT_Value_Inner_Map Cast_CT_Value_Group<From>::map[] = {
    {".uint8", new Cast_CT_Value<From, uint8_t>},
    {".uint16", new Cast_CT_Value<From, uint16_t>},
    {".uint32", new Cast_CT_Value<From, uint32_t>},
    {".uint64", new Cast_CT_Value<From, uint64_t>},
    {".int8", new Cast_CT_Value<From, int8_t>},
    {".int16", new Cast_CT_Value<From, int16_t>},
    {".int32", new Cast_CT_Value<From, int32_t>},
    {".int64", new Cast_CT_Value<From, int64_t>},
    {"float", new Cast_CT_Value<From, float>},
    {"double", new Cast_CT_Value<From, double>},
    {"long double", new Cast_CT_Value<From, long double>},
    {".pointer", new Cast_CT_Value<From, CT_Ptr>},
    {".lvalue", new Cast_CT_Value<From, CT_LValue>}
  };
  template <typename From>
  Cast_CT_Value_Inner_Map * Cast_CT_Value_Group<From>::map_end = 
    map + sizeof(Cast_CT_Value_Group<From>::map)/sizeof(Cast_CT_Value_Inner_Map);

  struct Cast_CT_Value_Map {
    String from;
    Cast_CT_Value_Inner_Map * map;
    Cast_CT_Value_Inner_Map * map_end;
    Cast_CT_Value_Map(String f, Cast_CT_Value_Inner_Map * m, Cast_CT_Value_Inner_Map * e)
      : from(f), map(m), map_end(e) {}
  };

  template <typename T>
  static inline Cast_CT_Value_Map make_cast_ct_value_map(String n) {
    return Cast_CT_Value_Map(n, Cast_CT_Value_Group<T>::map, Cast_CT_Value_Group<T>::map_end);
  }

  Cast_CT_Value_Map cast_ct_value_map[] = {
    make_cast_ct_value_map<uint8_t>(".uint8"), 
    make_cast_ct_value_map<uint16_t>(".uint16"), 
    make_cast_ct_value_map<uint32_t>(".uint32"), 
    make_cast_ct_value_map<uint64_t>(".uint64"), 
    make_cast_ct_value_map<int8_t>(".int8"), 
    make_cast_ct_value_map<int16_t>(".int16"), 
    make_cast_ct_value_map<int32_t>(".int32"), 
    make_cast_ct_value_map<int64_t>(".int64"), 
    make_cast_ct_value_map<float>("float"),
    make_cast_ct_value_map<double>("double"),
    make_cast_ct_value_map<long double>("long double"),
    make_cast_ct_value_map<CT_Ptr>(".pointer"),
    make_cast_ct_value_map<CT_LValue>(".lvalue")
  };
  Cast_CT_Value_Map * cast_ct_value_map_end = 
    cast_ct_value_map + sizeof(cast_ct_value_map)/sizeof(Cast_CT_Value_Map);

}

namespace ast {

  static const CT_Value_Base * cast_ct_value(String from, String to) {
    for (const Cast_CT_Value_Map * i = cast_ct_value_map; i != cast_ct_value_map_end; ++i) {
      if (i->from == from) {
        for (const Cast_CT_Value_Inner_Map * j = i->map; j != i->map_end; ++j) {
          if (j->to == to)
            return j->cast;
        }
        return NULL;
      }
    }
    return NULL;
  }

  const CT_Value_Base * cast_ct_value(const Type * f, const Type * t) {
    String from = f->ct_type_name();
    String to   = t->ct_type_name();
    return cast_ct_value(from, to);
  }

  template <typename To> 
  const Cast_CT_Value_Base<To> * cast_ct_value(String from) {
    String to   = CT_Type<To>::name;
    return dynamic_cast<const Cast_CT_Value_Base<To> *>(cast_ct_value(from, to));
  }

  template <typename T> 
  T AST::real_ct_value() const {
    if (!ct_value_) throw error(parse_, "\"%s\" can not be used in constant-expression", ~what_);
    const CT_Value<T> * ctv = dynamic_cast<const CT_Value<T> *>(ct_value_);
    if (ctv)
      return ctv->value(this);
    const Cast_CT_Value_Base<T> * cast_ctv = cast_ct_value<T>(ct_value_->type_name());
    if (cast_ctv)
      return cast_ctv->value_direct(this);
    abort();
  }
  template uint8_t AST::real_ct_value<uint8_t>() const;
  template uint16_t AST::real_ct_value<uint16_t>() const;
  template uint32_t AST::real_ct_value<uint32_t>() const;
  template uint64_t AST::real_ct_value<uint64_t>() const;
  template int8_t AST::real_ct_value<int8_t>() const;
  template int16_t AST::real_ct_value<int16_t>() const;
  template int32_t AST::real_ct_value<int32_t>() const;
  template int64_t AST::real_ct_value<int64_t>() const;
  template float AST::real_ct_value<float>() const;
  template double AST::real_ct_value<double>() const;
  template long double AST::real_ct_value<long double>() const;

}
