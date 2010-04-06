#include "ct_value-impl.hpp"

using namespace ast;

namespace ast {
  const CT_Value<CT_NVal> ct_nval;
}

namespace {
  
  template <typename Exp_T>
  struct DummyGetValue {
    static const CT_Value_Base * get_value(const Exp_T *) {return NULL;}
  };

  template <typename Exp_T>
  struct CT_Value_Map {
    String type;
    const CT_Value_Base * (*get_value)(const Exp_T *);
  };

  template <template <typename> class W>
  const CT_Value_Base * (*int_op_ct_value(const Type * t))(const typename W<void>::Exp_T *) {
    typedef CT_Value_Map<typename W<void>::Exp_T> CT_VM;
    static CT_VM map[] = {
      {".uint8", W<uint8_t>::get_value},
      {".uint16", W<uint16_t>::get_value},
      {".uint32", W<uint32_t>::get_value},
      {".uint64", W<uint64_t>::get_value},
      {".int8", W<int8_t>::get_value},
      {".int16", W<int16_t>::get_value},
      {".int32", W<int32_t>::get_value},
      {".int64", W<int64_t>::get_value}      
    };
    static CT_VM * map_end = map + sizeof(map)/sizeof(CT_VM);
    String n = t->ct_type_name();
    for (CT_VM * i = map; i != map_end; ++i) {
      if (i->type == n) return i->get_value;
    }
    return DummyGetValue<typename W<void>::Exp_T>::get_value;
  }

  template <template <typename> class W, template <typename> class F>
  struct OCTV_Proxy {
    template <typename T>
    struct Type : public W<F<T> > {};
  };
 
  template <template <typename> class W> 
  const CT_Value_Base * (*op_ct_value(const Type * t))(const typename W<void>::Exp_T *) {
    typedef CT_Value_Map<typename W<void>::Exp_T> CT_VM;
    static CT_VM map[] = {
      {"float", W<float>::get_value},
      {"double", W<double>::get_value},
      {"long double", W<long double>::get_value},
    };
    static CT_VM * map_end = map + sizeof(map)/sizeof(CT_VM);
    String n = t->ct_type_name();
    for (const CT_VM * i = map; i != map_end; ++i) {
      if (i->type == n) return i->get_value;
    }
    return int_op_ct_value<W>(t);
  }

  template <template <typename> class W, template <typename> class F> 
  inline const CT_Value_Base * (*int_op_ct_value(const Type * t))(const typename W<void>::Exp_T *) {
    return int_op_ct_value<OCTV_Proxy<W,F>::template Type>(t);
  }

  template <template <typename> class W, template <typename> class F> 
  inline const CT_Value_Base * (*op_ct_value(const Type * t))(const typename W<void>::Exp_T *) {
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
      if (*e) throw error(vp, "Expected Unsigned Integer, got \"%s\"", s);
      if (value == 0) t = env.types.inst(".zero", t);
      String n = it->ct_type_name();
      if (n == ".uint8")
        return new CT_Value<uint8_t>(value);
      else if (n == ".uint16")
        return new CT_Value<uint16_t>(value);
      else if (n == ".uint32")
        return new CT_Value<uint32_t>(value);
      else if (n == ".uint64")
        return new CT_Value<uint64_t>(value);
      else
        abort();
    } else {
      const char * s = ~vp->what().name;
      if (strcmp(s, "0))") == 0) abort();
      char * e = (char *)s;
      long long value = strtoll(s, &e, 0);
      if (*e) throw error(vp, "Expected Integer, got \"%s\"", s);
      if (value == 0) t = env.types.inst(".zero", t);
      String n = it->ct_type_name();
      if (n == ".int8")
        return new CT_Value<int8_t>(value);
      else if (n == ".int16")
        return new CT_Value<int16_t>(value);
      else if (n == ".int32")
        return new CT_Value<int32_t>(value);
      else if (n == ".int64")
        return new CT_Value<int64_t>(value);
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
      return new CT_Value<float>(value);
    else if (n == "double")
      return new CT_Value<double>(value);
    else if (n == "long double")
      return new CT_Value<long double>(value);
    else
      abort();
  }

  const CT_Value_Base * eif_ct_value(const EIf * eif) {
    if (!eif->exp->ct_value_ || eif->exp->ct_value_->nval())
      return NULL;
    if (eif->exp->ct_value_direct<target_bool>())
      return eif->if_true->ct_value_;
    else
      return eif->if_false->ct_value_;
  }

  template <typename F> 
  struct UnOp_GetValue {
    typedef UnOp Exp_T;
    static const CT_Value_Base * get_value(const UnOp * u) {
      if (!u->exp->ct_value_) return NULL;
      if (u->exp->ct_value_->nval()) return &ct_nval;
      typename F::argument_type x = u->exp->ct_value_direct<typename F::argument_type>();
      return new CT_Value<typename F::result_type>(F()(x));
    }
  };

  const CT_Value_Base * neg_ct_value(const UnOp * exp) {
    return op_ct_value<UnOp_GetValue, std::negate>(exp->type)(exp);
  }

  template <typename T>
  struct Compliment_F : public std::unary_function<T,T> {
      T operator()(T x) {return ~x;}
  };

  const CT_Value_Base * compliment_ct_value(const UnOp * exp) {
    return int_op_ct_value<UnOp_GetValue, Compliment_F>(exp->type)(exp);
  }

  template <typename T>
  struct Not_F : public std::unary_function<T,T> {
      T operator()(T x) {return !x;}
  };

  const CT_Value_Base * not_ct_value(const UnOp * exp) {
    return op_ct_value<UnOp_GetValue, Not_F>(exp->type)(exp);
  }

  template <typename F> 
  struct BinOp_GetValue {
    typedef BinOp Exp_T;
    static const CT_Value_Base * get_value(const BinOp * b) {
      if (!b->lhs->ct_value_ || !b->rhs->ct_value_) return NULL;
      if (b->lhs->ct_value_->nval() || b->rhs->ct_value_->nval()) return NULL;
      typename F::first_argument_type x = b->lhs->ct_value_direct<typename F::first_argument_type>();
      typename F::second_argument_type y = b->rhs->ct_value_direct<typename F::second_argument_type>();
      return new CT_Value<typename F::result_type>(F()(x, y));
    }
  };

  template <typename F> 
  struct Comp_GetValue {
    typedef BinOp Exp_T;
    static const CT_Value_Base * get_value(const BinOp * b) {
      if (!b->lhs->ct_value_ || !b->rhs->ct_value_) return NULL;
      if (b->lhs->ct_value_->nval() || b->rhs->ct_value_->nval()) return NULL;
      typename F::first_argument_type x = b->lhs->ct_value_direct<typename F::first_argument_type>();
      typename F::second_argument_type y = b->rhs->ct_value_direct<typename F::second_argument_type>();
      return new CT_Value<target_bool>(F()(x, y));
    }
  };
  
  template <typename T> 
  struct Ptr_Plus_GetValue {
    typedef BinOp Exp_T;
    static const CT_Value_Base * get_value(const BinOp * plus) {
      if (!plus->lhs->ct_value_ || !plus->rhs->ct_value_) return NULL;
      if (plus->lhs->ct_value_->nval() || plus->rhs->ct_value_->nval()) return &ct_nval;
      CT_Ptr   lhs = plus->lhs->ct_value_direct<CT_Ptr>();
      T        sz  = dynamic_cast<const PointerLike *>(plus->lhs->type)->subtype->size();
      T        rhs = plus->rhs->ct_value_direct<T>();
      return new CT_Value<CT_Ptr>(CT_Ptr(lhs.val + sz*rhs));
    }
  };

  template <typename T> 
  struct Plus_Ptr_GetValue {
    typedef BinOp Exp_T;
    static const CT_Value_Base * get_value(const BinOp * plus) {
      if (!plus->lhs->ct_value_ || !plus->rhs->ct_value_) return NULL;
      if (plus->lhs->ct_value_->nval() || plus->rhs->ct_value_->nval()) return &ct_nval;
      T        lhs = plus->lhs->ct_value_direct<T>();
      T        sz  = plus->rhs->type->size();
      CT_Ptr   rhs = plus->rhs->ct_value_direct<CT_Ptr>();
      return new CT_Value<CT_Ptr>(CT_Ptr(sz*lhs + rhs.val));
    }
  };

  const CT_Value_Base * plus_ct_value(const BinOp * exp) {
    return op_ct_value<BinOp_GetValue, std::plus>(exp->type)(exp);
  }

  const CT_Value_Base * ptr_plus_ct_value(const BinOp * exp) {
    return int_op_ct_value<Ptr_Plus_GetValue>(exp->rhs->type)(exp);
  }

  const CT_Value_Base * plus_ptr_ct_value(const BinOp * exp) {
    return int_op_ct_value<Plus_Ptr_GetValue>(exp->lhs->type)(exp);
  }

  const CT_Value_Base * minus_ct_value(const BinOp * exp) {
    return op_ct_value<BinOp_GetValue, std::minus>(exp->type)(exp);
  }
  
  const CT_Value_Base * times_ct_value(const BinOp * exp) {
    return op_ct_value<BinOp_GetValue, std::multiplies>(exp->type)(exp);
  }

  const CT_Value_Base * div_ct_value(const BinOp * exp) {
    return op_ct_value<BinOp_GetValue, std::divides>(exp->type)(exp);
  }

  const CT_Value_Base * mod_ct_value(const BinOp * exp) {
    return int_op_ct_value<BinOp_GetValue, std::modulus>(exp->type)(exp);
  }

  template <typename T>
  struct BAnd_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x & y;}
  };
  const CT_Value_Base * band_ct_value(const BinOp * exp) {
    return int_op_ct_value<BinOp_GetValue, BAnd_F>(exp->type)(exp);
  }

  template <typename T>
  struct BOr_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x | y;}
  };
  const CT_Value_Base * bor_ct_value(const BinOp * exp) {
    return int_op_ct_value<BinOp_GetValue, BOr_F>(exp->type)(exp);
  }

  template <typename T>
  struct XOr_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x | y;}
  };
  const CT_Value_Base * xor_ct_value(const BinOp * exp) {
    return int_op_ct_value<BinOp_GetValue, XOr_F>(exp->type)(exp);
  }

  template <typename T>
  struct LeftShift_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x << y;}
  };
  const CT_Value_Base * leftshift_ct_value(const BinOp * exp) {
    return int_op_ct_value<BinOp_GetValue, LeftShift_F>(exp->type)(exp);
  }

  template <typename T>
  struct RightShift_F : public std::binary_function<T,T,T> {
    T operator()(T x, T y) {return x >> y;}
  };
  const CT_Value_Base * rightshift_ct_value(const BinOp * exp) {
    return int_op_ct_value<BinOp_GetValue, RightShift_F>(exp->type)(exp);
  }

  const CT_Value_Base * eq_ct_value(const BinOp * exp) {
    return op_ct_value<Comp_GetValue, std::equal_to>(exp->lhs->type)(exp);
  }

  const CT_Value_Base * ne_ct_value(const BinOp * exp) {
    return op_ct_value<Comp_GetValue, std::not_equal_to>(exp->lhs->type)(exp);
  }

  const CT_Value_Base * lt_ct_value(const BinOp * exp) {
    return op_ct_value<Comp_GetValue, std::less>(exp->lhs->type)(exp);
  }

  const CT_Value_Base * gt_ct_value(const BinOp * exp) {
    return op_ct_value<Comp_GetValue, std::greater>(exp->lhs->type)(exp);
  }

  const CT_Value_Base * le_ct_value(const BinOp * exp) {
    return op_ct_value<Comp_GetValue, std::less_equal>(exp->lhs->type)(exp);
  }

  const CT_Value_Base * ge_ct_value(const BinOp * exp) {
    return op_ct_value<Comp_GetValue, std::greater_equal>(exp->lhs->type)(exp);
  }

  }

namespace {

  template <typename From, typename To> 
  struct Cast_GetValue_Base {
    static const CT_Value_Base * get_value(const Exp * exp) {
      if (!exp->ct_value_) return NULL;
      if (exp->ct_value_->nval()) return NULL;
      return new CT_Value<To>(static_cast<To>(exp->ct_value_direct<From>()));
    }
  };

  template <typename From, typename To> 
  struct Cast_GetValue : public Cast_GetValue_Base<From,To> {};

  template <typename T> 
  struct Cast_GetValue<T,T> {
    static const CT_Value_Base * get_value(const Exp * exp) {
      return exp->ct_value_;
    }
  };

  template <typename To>
  struct Cast_GetValue_Base<CT_Ptr,To> {
    static const CT_Value_Base * get_value(const Exp * exp) {
      if (!exp->ct_value_) return NULL;
      return new CT_Value<To>(static_cast<To>(exp->ct_value_direct<CT_Ptr>().val));
    }
  };

  template <typename From>
  struct Cast_GetValue_Base<From,CT_Ptr> {
    static const CT_Value_Base * get_value(const Exp * exp) {
      if (!exp->ct_value_) return NULL;
      return new CT_Value<CT_Ptr>(CT_Ptr(static_cast<size_t>(exp->ct_value_direct<From>())));
    }
  };

  template <typename To>
  struct Cast_GetValue_Base<CT_LValue,To> {
    static const CT_Value_Base * get_value(const Exp * exp) {
      abort();
    }
  };

  template <typename From>
  struct Cast_GetValue_Base<From,CT_LValue> {
    static const CT_Value_Base * get_value(const Exp * exp) {
      abort();
    }
  };

  template <>
  struct Cast_GetValue_Base<CT_LValue,CT_Ptr> {
    static const CT_Value_Base * get_value(const Exp * exp) {
      if (!exp->ct_value_) return NULL;
      // it can only happen id exp is an array type
      return new CT_Value<CT_Ptr>(exp->ct_value_direct<CT_LValue>().addr);
    }
  };

  template <>
  struct Cast_GetValue_Base<CT_Ptr,CT_LValue> {
    static const CT_Value_Base * get_value(const Exp * exp) {
      abort();
    }
  };

  struct Cast_GetValue_Inner_Map {
    String to;
    const CT_Value_Base * (*get_value)(const Exp *);
  };

  template <typename From>
  struct Cast_GetValue_Group {
    static Cast_GetValue_Inner_Map map[];
    static Cast_GetValue_Inner_Map * map_end;
  };

  template <typename From>
  Cast_GetValue_Inner_Map Cast_GetValue_Group<From>::map[] = {
    {".uint8", Cast_GetValue<From, uint8_t>::get_value},
    {".uint16", Cast_GetValue<From, uint16_t>::get_value},
    {".uint32", Cast_GetValue<From, uint32_t>::get_value},
    {".uint64", Cast_GetValue<From, uint64_t>::get_value},
    {".int8", Cast_GetValue<From, int8_t>::get_value},
    {".int16", Cast_GetValue<From, int16_t>::get_value},
    {".int32", Cast_GetValue<From, int32_t>::get_value},
    {".int64", Cast_GetValue<From, int64_t>::get_value},
    {"float", Cast_GetValue<From, float>::get_value},
    {"double", Cast_GetValue<From, double>::get_value},
    {"long double", Cast_GetValue<From, long double>::get_value},
    {".ptr", Cast_GetValue<From, CT_Ptr>::get_value},
    {".lvalue", Cast_GetValue<From, CT_LValue>::get_value}
  };
  template <typename From>
  Cast_GetValue_Inner_Map * Cast_GetValue_Group<From>::map_end = 
    map + sizeof(Cast_GetValue_Group<From>::map)/sizeof(Cast_GetValue_Inner_Map);

  struct Cast_GetValue_Map {
    String from;
    Cast_GetValue_Inner_Map * map;
    Cast_GetValue_Inner_Map * map_end;
    Cast_GetValue_Map(String f, Cast_GetValue_Inner_Map * m, Cast_GetValue_Inner_Map * e)
      : from(f), map(m), map_end(e) {}
  };

  template <typename T>
  static inline Cast_GetValue_Map make_cast_get_value_map(String n) {
    return Cast_GetValue_Map(n, Cast_GetValue_Group<T>::map, Cast_GetValue_Group<T>::map_end);
  }

  Cast_GetValue_Map cast_get_value_map[] = {
    make_cast_get_value_map<uint8_t>(".uint8"), 
    make_cast_get_value_map<uint16_t>(".uint16"), 
    make_cast_get_value_map<uint32_t>(".uint32"), 
    make_cast_get_value_map<uint64_t>(".uint64"), 
    make_cast_get_value_map<int8_t>(".int8"), 
    make_cast_get_value_map<int16_t>(".int16"), 
    make_cast_get_value_map<int32_t>(".int32"), 
    make_cast_get_value_map<int64_t>(".int64"), 
    make_cast_get_value_map<float>("float"),
    make_cast_get_value_map<double>("double"),
    make_cast_get_value_map<long double>("long double"),
    make_cast_get_value_map<CT_Ptr>(".ptr"),
    make_cast_get_value_map<CT_LValue>(".lvalue")
  };
  Cast_GetValue_Map * cast_get_value_map_end = 
    cast_get_value_map + sizeof(cast_get_value_map)/sizeof(Cast_GetValue_Map);

}

namespace ast {

  static const CT_Value_Base * (*cast_get_value(String from, String to))(const Exp *) {
    //printf("cast_get_value:: %s %s\n", ~from, ~to);
    for (const Cast_GetValue_Map * i = cast_get_value_map; i != cast_get_value_map_end; ++i) {
      if (i->from == from) {
        for (const Cast_GetValue_Inner_Map * j = i->map; j != i->map_end; ++j) {
          if (j->to == to) {
            return j->get_value;
          }
        }
        return DummyGetValue<Exp>::get_value;
      }
    }
    return DummyGetValue<Exp>::get_value;
  }

  const CT_Value_Base * cast_ct_value(const Exp * f, const Type * t) {
    String from = f->type->ct_type_name();
    String to   = t->ct_type_name();
    return cast_get_value(from, to)(f);
  }

  template <typename T> 
  T Exp::real_ct_value() const {
    if (!ct_value_) throw error(syn, "\"%s\" can not be used in constant-expression this way", ~syn->to_string());
    const CT_Value<T> * ctv = dynamic_cast<const CT_Value<T> *>(ct_value_);
    if (ctv)
      return ctv->val;
    const CT_Value_Base * cast_ctv = cast_get_value(ct_value_->type_name(), CT_Type<T>::name)(this);
    if (!cast_ctv) {
      throw error(syn, "\"%s\" can not be used in constant-expression this way <1>", ~syn->to_string());
    }
    ctv = dynamic_cast<const CT_Value<T> *>(cast_ctv);
    if (ctv)
      return ctv->val;
    throw error(syn, "\"%s\" can not be used in constant-expression this way <2>", ~syn->to_string());
    //abort();
  }
  template uint8_t Exp::real_ct_value<uint8_t>() const;
  template uint16_t Exp::real_ct_value<uint16_t>() const;
  template uint32_t Exp::real_ct_value<uint32_t>() const;
  template uint64_t Exp::real_ct_value<uint64_t>() const;
  template int8_t Exp::real_ct_value<int8_t>() const;
  template int16_t Exp::real_ct_value<int16_t>() const;
  template int32_t Exp::real_ct_value<int32_t>() const;
  template int64_t Exp::real_ct_value<int64_t>() const;
  template float Exp::real_ct_value<float>() const;
  template double Exp::real_ct_value<double>() const;
  template long double Exp::real_ct_value<long double>() const;

}

namespace ast {

  bool CT_Value<CT_NVal>::nval() const {
    return true;
  }

  void CT_Value<CT_NVal>::compile_c(CompileWriter & cw, Exp * exp) const {
    abort();
  }

  void CT_Value<CT_NVal>::compile(CompileWriter & cw, Exp * exp) const {
    exp->compile(cw);
  }

  const char * CT_Value<CT_NVal>::type_name() const {
    return "";
  }

}

namespace ast {

  //template<typename T> const char * const CT_Type_Base<T>::name = "";
  template<> const char * const CT_Type_Base<int8_t>::name = ".int8";
  template<> const char * const CT_Type_Base<uint8_t>::name = ".uint8";
  template<> const char * const CT_Type_Base<int16_t>::name = ".int16";
  template<> const char * const CT_Type_Base<uint16_t>::name = ".uint16";
  template<> const char * const CT_Type_Base<int32_t>::name = ".int32";
  template<> const char * const CT_Type_Base<uint32_t>::name = ".uint32";
  template<> const char * const CT_Type_Base<int64_t>::name = ".int64";
  template<> const char * const CT_Type_Base<uint64_t>::name = ".uint64";
  template<> const char * const CT_Type_Base<float>::name = "float";
  template<> const char * const CT_Type_Base<double>::name = "double";
  template<> const char * const CT_Type_Base<long double>::name = "long double";
  template<> const char * const CT_Type_Base<CT_Ptr>::name = ".ptr";
  template<> const char * const CT_Type_Base<CT_LValue>::name = ".lvalue";

}

namespace ast {

  template <typename T>
  void CT_Value<T>::compile_c(CompileWriter & o, Exp *) const {
    abort();
  }

  template void CT_Value<CT_LValue>::compile_c(CompileWriter & o, Exp *) const;

  template <>
  void CT_Value<signed char>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%d", val);
  }

  template <>
  void CT_Value<unsigned char>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%uu", val);
  }

  template <>
  void CT_Value<short>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%d", val);
  }

  template <>
  void CT_Value<unsigned short>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%uu", val);
  }

  template <>
  void CT_Value<int>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%d", val);
  }

  template <>
  void CT_Value<unsigned>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%uu", val);
  }

  template <>
  void CT_Value<long>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%ldl", val);
  }

  template <>
  void CT_Value<unsigned long>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%luul", val);
  }

  template <>
  void CT_Value<long long>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%lldll", val);
  }

  template <>
  void CT_Value<unsigned long long>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%lluull", val);
  }

  template <>
  void CT_Value<float>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%af", val);
  }

  template <>
  void CT_Value<double>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%a", val);
  }

  template <>
  void CT_Value<long double>::compile_c(CompileWriter & o, Exp *) const {
    o.printf("%Lal", val);
  }

  //
  //
  //

  template <typename T>
  void CT_Value<T>::compile(CompileWriter & o, Exp *) const {
    abort();
  }

  template void CT_Value<CT_LValue>::compile(CompileWriter & o, Exp *) const;

  template <>
  void CT_Value<signed char>::compile(CompileWriter & o, Exp *) const {
    //o.printf("(literal %d (signed-char)", val);
    o.printf("%d", val);
  }

  template <>
  void CT_Value<unsigned char>::compile(CompileWriter & o, Exp *) const {
    //o.printf("(literal %u (unsigned-char))", val);
    o.printf("%u", val);
  }

  template <>
  void CT_Value<short>::compile(CompileWriter & o, Exp *) const {
    //o.printf("(literal %d (short))", val);
    o.printf("%d", val);
  }

  template <>
  void CT_Value<unsigned short>::compile(CompileWriter & o, Exp *) const {
    //o.printf("(literal %u (unsigned-short))", val);
    o.printf("%u", val);
  }

  template <>
  void CT_Value<int>::compile(CompileWriter & o, Exp *) const {
    //o.printf("(literal %d (int))", val);
    o.printf("%d", val);
  }

  template <>
  void CT_Value<unsigned>::compile(CompileWriter & o, Exp *) const {
    if (val <= INT_MAX)
      o.printf("%u", val);
    else
      o.printf("(n %u (unsigned))", val);
  }

  template <>
  void CT_Value<long>::compile(CompileWriter & o, Exp *) const {
    o.printf("(n %ld (long))", val);
  }

  template <>
  void CT_Value<unsigned long>::compile(CompileWriter & o, Exp *) const {
    o.printf("(n %lu (unsigned-long))", val);
  }

  template <>
  void CT_Value<long long>::compile(CompileWriter & o, Exp *) const {
    o.printf("(n %lld (long-long))", val);
  }

  template <>
  void CT_Value<unsigned long long>::compile(CompileWriter & o, Exp *) const {
    o.printf("(n %llu (unsigned-long-long))", val);
  }

  template <>
  void CT_Value<float>::compile(CompileWriter & o, Exp *) const {
    o.printf("(f %a (float))", val);
  }

  template <>
  void CT_Value<double>::compile(CompileWriter & o, Exp *) const {
    o.printf("(f %a (double))", val);
  }

  template <>
  void CT_Value<long double>::compile(CompileWriter & o, Exp *) const {
    o.printf("(f %La (long-double))", val);
  }

  //
  //
  //

}
