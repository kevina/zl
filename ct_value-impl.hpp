#ifndef CT_VALUE_IMPL__HPP
#define CT_VALUE_IMPL__HPP

#include "ct_value.hpp"
#include "ast.hpp"

namespace ast {

  CT_Value_Base * new_literal_ct_value(const Syntax * vp, const Type * & t, Environ & env);
  CT_Value_Base * new_float_ct_value(const Syntax * vp, const Type * & t, Environ & env);
  
  const CT_Value_Base * eif_ct_value(const EIf *);

  const CT_Value_Base * neg_ct_value(const UnOp * t);
  const CT_Value_Base * compliment_ct_value(const UnOp * t);
  const CT_Value_Base * not_ct_value(const UnOp * t);

  const CT_Value_Base * plus_ct_value(const BinOp * type);
  const CT_Value_Base * plus_ptr_ct_value(const BinOp * type);  
  const CT_Value_Base * ptr_plus_ct_value(const BinOp * type);
  const CT_Value_Base * minus_ct_value(const BinOp * type);
  const CT_Value_Base * times_ct_value(const BinOp * type);
  const CT_Value_Base * div_ct_value(const BinOp * type);
  const CT_Value_Base * mod_ct_value(const BinOp * type);
  const CT_Value_Base * band_ct_value(const BinOp * type);
  const CT_Value_Base * bor_ct_value(const BinOp * type);
  const CT_Value_Base * xor_ct_value(const BinOp * type);
  const CT_Value_Base * leftshift_ct_value(const BinOp * type);
  const CT_Value_Base * rightshift_ct_value(const BinOp * type);
  const CT_Value_Base * eq_ct_value(const BinOp * type);
  const CT_Value_Base * ne_ct_value(const BinOp * type);
  const CT_Value_Base * lt_ct_value(const BinOp * type);
  const CT_Value_Base * gt_ct_value(const BinOp * type);
  const CT_Value_Base * le_ct_value(const BinOp * type);
  const CT_Value_Base * ge_ct_value(const BinOp * type);


}

#endif
