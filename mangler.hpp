#ifndef MANGLER__HPP
#define MANGLER__HPP

#ifdef __zl
#define SYMBOL Symbol
#else
namespace ast {class Symbol;}
#define SYMBOL const ast::Symbol
#endif

struct StringObj;

typedef StringObj * (*MangleFun)(SYMBOL *);

struct Mangler {
  const char * abi_name;
  MangleFun mangler;
};

#endif
