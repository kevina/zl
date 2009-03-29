#ifndef CT_VALUE__HPP
#define CT_VALUE__HPP

#include "util.hpp"

#include <limits>

namespace ast {

  struct AST;

  struct CT_Ptr {
    size_t val;
    CT_Ptr(size_t v) : val(v) {}
  };

  struct CT_LValue {
    CT_Ptr addr;
    CT_LValue(CT_Ptr a) : addr(a) {}
  };

  struct CT_Value_Base : public gc {
    virtual void to_string(const AST *, OStream &) const = 0;
    virtual const char * type_name() const = 0;
    virtual ~CT_Value_Base() {}
  };

  template <typename T> struct CT_Value : public CT_Value_Base {
    void to_string(const AST *, OStream & o) const;
    virtual T value(const AST *) const = 0;
    const char * type_name() const;
  };

  template <typename T>
  struct CT_Type_Base {typedef T type; static const char * const name;};
  template<typename T> const char * const CT_Type_Base<T>::name = NULL;

  template <typename T, bool is_int, bool is_signed, size_t size>
  struct CT_Type_ByProp : public CT_Type_Base<T> {};

  template <typename T> struct CT_Type_ByProp<T, true, true, 1> : public CT_Type_Base<int8_t> {};
  template <typename T> struct CT_Type_ByProp<T, true, false, 1> : public CT_Type_Base<uint8_t> {};
  template <typename T> struct CT_Type_ByProp<T, true, true, 2> : public CT_Type_Base<int16_t> {};
  template <typename T> struct CT_Type_ByProp<T, true, false, 2> : public CT_Type_Base<uint16_t> {};
  template <typename T> struct CT_Type_ByProp<T, true, true, 4> : public CT_Type_Base<int32_t> {};
  template <typename T> struct CT_Type_ByProp<T, true, false, 4> : public CT_Type_Base<uint32_t> {};
  template <typename T> struct CT_Type_ByProp<T, true, true, 8> : public CT_Type_Base<int64_t> {};
  template <typename T> struct CT_Type_ByProp<T, true, false, 8> : public CT_Type_Base<uint64_t> {};

  template <typename T> struct CT_Type
    : public CT_Type_ByProp<T,
                            std::numeric_limits<T>::is_integer,
                            std::numeric_limits<T>::is_signed,
                            sizeof(T)> {};

  template <typename T> struct CT_Type<T *> : public CT_Type_Base<CT_Ptr> {};
                                                
  template <typename T>
  const char * CT_Value<T>::type_name() const {
    return CT_Type<T>::name;
  }

}
#endif
