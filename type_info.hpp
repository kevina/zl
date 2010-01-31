#ifndef TYPE_INFO__HPP
#define TYPE_INFO__HPP

struct Error;

namespace syntax_ns {
  struct SyntaxBuilderBase;
}

namespace ast {
  struct Symbol;
  struct SymbolKeyEntity;
  struct AST;
  struct Exp;
  struct Stmt;
  struct TypeInst;
}

template <typename T> struct TypeInfo;
template <> struct TypeInfo<Error> {
  typedef Error type; 
  static const unsigned id = 0x1FF;};
template <> struct TypeInfo<ast::Symbol> {
  typedef ast::Symbol type; 
  static const unsigned id = 0x2FF;};
template <> struct TypeInfo<ast::SymbolKeyEntity> {
  typedef ast::SymbolKeyEntity type; 
  static const unsigned id = 0x3FF;};
//template <> struct TypeInfo<ast::AST> {
//  typedef ast::AST type; 
//  static const unsigned id = 0x400;};
template <> struct TypeInfo<ast::Exp> {
  typedef ast::Exp type; 
  static const unsigned id = 0x401;};
template <> struct TypeInfo<ast::Stmt> {
  typedef ast::Stmt type; 
  static const unsigned id = 0x402;};
template <> struct TypeInfo<ast::TypeInst> {
  typedef ast::TypeInst type; 
  static const unsigned id = 0x5FF;};
template <> struct TypeInfo<syntax_ns::SyntaxBuilderBase> {
  typedef syntax_ns::SyntaxBuilderBase type; 
  static const unsigned id = 0x6FF;};

#endif
