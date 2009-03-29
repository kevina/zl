#include "symbol_table.hpp"
#include "type.hpp"
#include "gc.hpp"

#ifndef ENVIRON_HPP
#define ENVIRON_HPP

namespace ast {

  struct AST;
  struct VarDeclaration;

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

  struct TopLevelVarSymbol;

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

  struct InitCleanup {
    AST * init;
    AST * cleanup;
    InitCleanup(AST * i = NULL, AST * c = NULL)
      : init(i), cleanup(c) {}
  };

  struct Environ : public gc {
    TypeRelation * type_relation;
    Vector<const TopLevelSymbol *> * top_level_symbols;
    bool temporary() const {return !top_level_symbols;}
    const TopLevelSymbol * find_tls(const char * to_find) const {
      if (!top_level_symbols)
        return NULL;
      Vector<const TopLevelSymbol *>::const_iterator 
        i = top_level_symbols->begin(),
        e = top_level_symbols->end();
      for (; i != e; ++i) {
        if ((*i)->uniq_name() == to_find) return *i;
      }
      return NULL;
    }
    SymbolTable symbols;
    TypeSymbolTable types;
    OpenSymbolTable fun_labels;
    Scope scope;
    Frame * frame;
    TopLevelSymbol * where;
    SymbolNode * const * top_level_environ;
    Deps * deps;
    bool * for_ct; // set if this function uses a ct primitive such as syntax
    Type * void_type() {return types.inst("<void>");}
    //Type * bool_type() {return types.inst("<bool>");}
    Type * bool_type() {return types.inst("int");}
    const FunctionSymbol * function_sym() 
      {return static_cast<const FunctionSymbol *>(types.find(".fun"));}
    Environ(Scope s = TOPLEVEL) 
      : types(this), scope(s), where(),
        top_level_environ(&symbols.front), 
        deps(), for_ct()
      {
        top_level_symbols = new Vector<const TopLevelSymbol *>();
        type_relation = new_c_type_relation(); // FIXME HACK
        create_c_types(types); // FIXME Another HACK
        add_inner_nss(symbols);
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
        deps(other.deps), for_ct(other.for_ct) {}
    Environ new_scope() {
      Environ env = *this;
      env.symbols = symbols.new_scope();
      return env;
    }

    void add(const SymbolKey & k, const Symbol * sym, Pass pass = AllPasses) {
      sym->add_to_env(k, *this, pass);
    }

    Environ new_frame() {
      Environ env = *this;
      env.scope = LEXICAL;
      env.symbols = symbols.new_scope(env.fun_labels);
      env.frame = new Frame();
      env.top_level_environ = &symbols.front;
      env.for_ct = NULL;
      return env;
    }
  private:
  };
  
  inline const TypeSymbol * TypeSymbolTable::find(const SymbolKey & k) {
    return env->symbols.find<TypeSymbol>(k);
  }

  inline const TypeSymbol * TypeSymbolTable::find(const Syntax * p, const InnerNS * ns) {
    return env->symbols.find<TypeSymbol>(p, ns);
  }

  inline void TypeSymbolTable::add(const SymbolKey & k, const TypeSymbol * t) {
    env->add(k, t);
  }

}

#endif
