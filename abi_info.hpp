#ifndef ABIINFO__HPP
#define ABIINFO__HPP

#ifdef __zl
#define SYMBOL Symbol
#define ENVIRON Environ
#define SYNTAX Syntax
#else
namespace syntax_ns {struct SyntaxBase; typedef const SyntaxBase Syntax;}
namespace ast {class Symbol; struct Environ;}
#define SYMBOL const ast::Symbol
#define ENVIRON const ast::Environ
#define SYNTAX syntax_ns::Syntax
#endif

typedef struct StringObj StringObj;

typedef StringObj * (*MangleFun)(SYMBOL *);
typedef SYNTAX * (*MacroLikeFun)(SYNTAX *, ENVIRON *);

typedef struct AbiInfo {
  const char * abi_name;
  MangleFun mangler;
  MacroLikeFun parse_class;
} AbiInfo;

extern "C" AbiInfo * environ_get_abi_info(ENVIRON * env);

#endif
