#ifndef ABIINFO__HPP
#define ABIINFO__HPP

#ifdef __zl
include_file "macro_api.zlh";
#define SYMBOL Symbol
#define ENVIRON Environ
#define SYNTAX Syntax
#define MODULE Module
#else
namespace syntax_ns {struct SyntaxBase; typedef const SyntaxBase Syntax;}
namespace ast {class Symbol; struct Environ; struct Module;}
#define SYMBOL const ast::Symbol
#define ENVIRON const ast::Environ
#define SYNTAX syntax_ns::Syntax
#define MODULE const ast::Module
#endif

typedef struct StringObj StringObj;

typedef StringObj * (*MangleFun)(SYMBOL *);
typedef SYNTAX * (*MacroLikeFun)(SYNTAX *, ENVIRON *);

typedef struct AbiInfo {
  const char * abi_name;
  MangleFun mangler;
  MacroLikeFun parse_class;
  const char * module_name; // if present module will be lookup up in
                            // the env.
  MODULE * module;
} AbiInfo;

extern "C" AbiInfo * environ_get_abi_info(ENVIRON * env);

#undef MODULE
#undef SYNTAX
#undef ENVIRON
#undef SYMBOL

#endif
