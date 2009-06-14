#ifndef CT_VALUE__HPP
#define CT_VALUE__HPP

#include "util.hpp"

#include <limits>

namespace ast {

  struct AST;
  struct Symbol;
  struct CompileWriter;

  struct CT_NVal {};

  struct CT_Ptr {
    //static const size_t NVAL = (size_t)-1;
    size_t val;
    //CT_Ptr() : val(NVAL) {}
    CT_Ptr(size_t v) : val(v) {}
  };

  struct CT_LValue {
    CT_Ptr addr;
    //CT_LValue() : addr() {}
    CT_LValue(CT_Ptr a) : addr(a) {}
  };

  struct CT_Value_Base : public gc {
    CT_Value_Base() {}
    // nval() return true if the precise value is not known at compile time
    // but can be determined once the executable is compiled, primary used
    // when taking the address of a global variable.  Also used for int lists.
    virtual bool nval() const {return false;}
    // compile(...) will attempt to compile the compile time value, if
    // nval() is true than compile the exp passed in since it doesn't
    // have a precise value at compile time
    virtual void compile_c(CompileWriter &, AST * exp) const = 0;
    virtual void compile(CompileWriter &, AST * exp) const = 0;
    virtual const char * type_name() const = 0;
    virtual ~CT_Value_Base() {}
  };

  template <typename T> struct CT_Value : public CT_Value_Base {
    T val;
    CT_Value(const T & v) : val(v) {}
    void compile_c(CompileWriter &, AST *) const;
    void compile(CompileWriter &, AST *) const;
    const char * type_name() const;
  };

  template <> struct CT_Value<CT_NVal> : public CT_Value_Base {
    bool nval() const;
    void to_string(OStream & o) const;
    void compile_c(CompileWriter &, AST *) const;
    void compile(CompileWriter &, AST *) const;
    const char * type_name() const;
  };

  extern const CT_Value<CT_NVal> ct_nval;

  template <typename T>
  struct CT_Type_Base {typedef T type; static const char * const name;};

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
