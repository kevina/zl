#include "symbol_table.hpp"
#include "type.hpp"
#include "gc.hpp"

#ifndef ENVIRON_HPP
#define ENVIRON_HPP

namespace ast {

  struct AST;
  struct Stmt;

  //class TypeSymbol;
  //class TypeInst;
  //typedef TypeInst Type;
  //struct Environ;
  //class TypeRelation;

  //enum Scope {STATIC, STACK};

  enum Scope {OTHER, TOPLEVEL, LEXICAL};
  
  struct Frame : public gc {
    const TypeInst * return_type;
  };

  struct TopLevelVarDecl;
  typedef TopLevelVarDecl TopLevelVarSymbol;

  struct Deps : public Vector<const TopLevelVarSymbol *> {
    void insert(const TopLevelVarSymbol * sym) {
      for (iterator i = begin(), e = end(); i != e; ++i)
        if (*i == sym) return;
      push_back(sym);
    }
    void merge(const Deps & other) {
      for (const_iterator i = other.begin(), e = other.end(); i != e; ++i)
        insert(*i);
    }
    bool have(const TopLevelVarSymbol * sym) {
      for (const_iterator i = begin(), e = end(); i != e; ++i)
        if (*i == sym) return true;
      return false;
    }
  };

  struct InsrPoint {
    InsrPoint() : ptr() {}
    Stmt * * ptr;
    void clear() {
      ptr = NULL;
    }
    inline void add(Stmt *); // defined in ast.hpp
  };

  struct Environ : public gc {
    TypeRelation * type_relation;
    bool special() const {return !top_level_symbols.front;}
    const Symbol * find_tls(const char * to_find) const {
      if (!top_level_symbols.front)
        return NULL;
      return top_level_symbols.find<Symbol>(to_find);
    }
    OpenSymbolTable top_level_symbols;
    SymbolTable symbols;
    TypeSymbolTable types;
    OpenSymbolTable fun_labels;
    Scope scope;
    Frame * frame;
    TopLevelSymbol * where;
    SymbolNode * const * top_level_environ;
    Deps * deps;
    bool * for_ct; // set if this function uses a ct primitive such as syntax
    InsrPoint ip;
    Type * void_type() {return types.inst("<void>");}
    //Type * bool_type() {return types.inst("<bool>");}
    Type * bool_type() {return types.inst("int");}
    FunctionSymbol * function_sym() 
      {return static_cast<FunctionSymbol *>(types.find(".fun"));}
    Environ(Scope s = TOPLEVEL) 
      : types(this), scope(s), where(),
        top_level_environ(&symbols.front), 
        deps(), for_ct()
      {
        if (s == TOPLEVEL) {
          top_level_symbols.front = &symbols.front;
          type_relation = new_c_type_relation(); // FIXME HACK
          create_c_types(types); // FIXME Another HACK
          add_inner_nss(*this);
        }
        frame = new Frame();
      }
    Environ(const Environ & other) 
      : type_relation(other.type_relation), 
        top_level_symbols(other.top_level_symbols), 
        symbols(other.symbols),
        types(this), fun_labels(other.fun_labels), 
        scope(other.scope), frame(other.frame), 
        where(other.where),
        top_level_environ(other.top_level_environ == &other.symbols.front ? &symbols.front :  other.top_level_environ),
        deps(other.deps), for_ct(other.for_ct), ip(other.ip) {}
    Environ new_scope() {
      Environ env = *this;
      env.ip.clear();
      env.symbols = symbols.new_scope();
      return env;
    }

    void add_stmt(Stmt * stmt) {
      ip.add(stmt);
    }

    void add(const SymbolKey & k, Symbol * sym) {
      sym->add_to_env(k, *this);
    }

    void add_internal(const SymbolKey & k, Symbol * sym) {
      SymbolNode * n = symbols.add(k, sym, SymbolNode::INTERNAL);
      sym->key = &n->key;
    }

    void add_alias(const SymbolKey & k, Symbol * sym) {
      symbols.add(k, sym, SymbolNode::ALIAS);
    }

    Environ new_frame() {
      Environ env = *this;
      env.ip.clear();
      env.scope = LEXICAL;
      env.symbols = symbols.new_scope(env.fun_labels);
      env.frame = new Frame();
      env.top_level_environ = &symbols.front;
      env.for_ct = NULL;
      return env;
    }
  private:
  };
  
  inline TypeSymbol * TypeSymbolTable::find(const SymbolKey & k) {
    return env->symbols.find<TypeSymbol>(k);
  }

  inline TypeSymbol * TypeSymbolTable::find(const Syntax * p, const InnerNS * ns) {
    return env->symbols.find<TypeSymbol>(p, ns);
  }

  inline void TypeSymbolTable::add(const SymbolKey & k, TypeSymbol * t) {
    env->add(k, t);
  }

  inline void TypeSymbolTable::add_internal(const SymbolKey & k, TypeSymbol * t) {
    env->add_internal(k, t);
  }

  inline void TypeSymbolTable::add_alias(const SymbolKey & k, TypeSymbol * t) {
    env->add_alias(k, t);
  }
}

#endif
