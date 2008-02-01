#include "gc.hpp"
#include "hash-t.hpp" //FIXME
#include "parse.hpp"
#include "fstream.hpp"
#include "symbol_table.hpp"
#include "type.hpp"

#include <map>

#undef NPOS

namespace AST {

  struct AST;
  enum Scope {STATIC, STACK};

  struct VarLoc : public gc {
    Scope scope;
    unsigned offset; // offset in local block for interpreter
  };

  struct Symbol : public VarLoc {
    String name;
    SourceStr str;
    const Type * type;
    void * value; // for compile time constants
    Symbol(String n) : name(n) {}
  };

  typedef SymbolTable<const Parse *> ParmsSymbolTable;

  enum LabelType {NormalLabel = 0, LocalLabel = 1};

  typedef SymbolTable<Symbol *>     VarSymbolTable;
  typedef SymbolTable<String>       LabelSymbolTable;
  // note: value of label symbol table is mapping to unique c label


  struct Frame : public gc {
    const TypeInst * return_type;
#if 0
    unsigned cur_frame_size;
    unsigned max_frame_size;
    unsigned frame_size_last_var; // safety measure
    Frame() : return_type(), cur_frame_size(), max_frame_size(), frame_size_last_var() {}
    unsigned alloc_tmp(const Type * t) {
      unsigned diff = cur_frame_size % t->align;
      if (diff != 0) cur_frame_size += t->align - diff;
      unsigned loc = cur_frame_size;
      cur_frame_size += t->size;
      if (cur_frame_size > max_frame_size) 
        max_frame_size = cur_frame_size;
      return loc;
    }
    unsigned alloc_var(const Type * t) {
      unsigned loc = alloc_tmp(t);
      frame_size_last_var = cur_frame_size;
      return loc;
    }
    //unsigned reserve(const Type * t) {
    //  unsigned loc = alloc_tmp(t);
    //  pop_tmp(t);
    //  return loc;
    //}
    void pop_tmp(const Type * t) {
      //cur_frame_size -= t->size;
      //assert(cur_frame_size >= frame_size_last_var);
    }
    void pop_to(unsigned sz) {
      //cur_frame_size = sz;
      //assert(cur_frame_size >= frame_size_last_var);
    }
#endif
  };

  struct ParseEnviron : public gc {
    LabelSymbolTable * labels;
    TypeRelation * type_relation;
    TypeSymbolTable * types;
    VarSymbolTable  * vars;
    Frame * frame;
    Type * void_type() {return types->inst("<void>");}
    Type * bool_type() {return types->inst("<bool>");}
    const FunctionPtrSymbol * function_sym() 
      {return static_cast<const FunctionPtrSymbol *>(types->lookup(".fun"));}
    ParseEnviron() {
      type_relation = new_c_type_relation(); // FIXME HACK
      types = new TypeSymbolTable;
      create_c_types(types); // FIXME Another HACK
      vars  = new VarSymbolTable;
      frame = new Frame();
      labels = 0;
    }
    ParseEnviron new_scope() {
      ParseEnviron env = *this;
      env.types = new TypeSymbolTable(env.types);
      env.vars  = new VarSymbolTable(env.vars);
      return env;
    }
    ParseEnviron new_frame() {
      ParseEnviron env = *this;
      env.types = new TypeSymbolTable(env.types);
      env.vars  = new VarSymbolTable(env.vars);
      env.frame = new Frame();
      return env;
    }
  };

  typedef ParseEnviron ResolveEnviron;


#if 0
  struct Scope {
    TypeSymbolTable * types;
    VarSymbolTable  * vars;
    ResolveEnviron() 
      : types(), vars() {}
    ResolveEnviron(TypeSymbolTable * t, 
                   VarSymbolTable  * v)
      : types(t), vars(v) {}
  };
#endif

  struct PrepEvalEnviron : public gc {
#if 0
    SymbolTable<const Type *> * types;
    SymbolTable<Symbol *> * sym;
    Frame * frame;
    ResolveEnviron() {
      frame = new Frame();
    }
    ResolveEnviron new_frame(const Scope & scp) {
      ResolveEnviron env = *this;
      env       = scp;
      env.frame = new Frame();
      return env;
    }
#endif
  };

  class AST;

  struct ExecEnviron : public gc {
    union {
      struct {
        char * static_ptr;
        char * frame_ptr;
      };
      char * * var_ptrs[2];
    };
    unsigned * cur_frame_size;
    char * stack_ptr;
    char * end_of_stack;
    template <typename T> T & static_var(unsigned i)
      {return *reinterpret_cast<T *>(static_ptr + i);}
    template <typename T> T & local_var(unsigned i)
      {return *reinterpret_cast<T *>(frame_ptr + i);}
    template <typename T> T & var(const VarLoc & l)
      {return *reinterpret_cast<T *>(var_ptrs[l.scope][l.offset]);}
    template <typename T> inline T & ret(const AST *);
    void * local_var(unsigned i)
      {return static_cast<void *>(static_ptr + i);}
    void * var(const VarLoc & l) 
      {return static_cast<void *>(var_ptrs[l.scope][l.offset]);}
    inline void * ret(const AST * exp);
    //template <typename T> T * push_tmp(const Type * t) {
    //  T * p = reinterpret_cast<T *>(stack_ptr);
    //  stack_ptr += t->size;
    //  assert(stack_ptr <= end_of_stack);
    //}
    //template <typename T> T * pop_tmp(const Type * t) {
    //  T * p = reinterpret_cast<T *>(stack_ptr);
    //  stack_ptr -= t->size;
    //}
    //void pop_to(void * p) {
    //  stack_ptr = static_cast<char *>(p);
    //}
    //ExecEnviron new_frame(unsigned offset, unsigned sz) {
    //  ExecEnviron ret;
    //  ret.static_ptr = static_ptr;
    //  ret.frame_ptr = stack_ptr - offset;
    //  ret.stack_ptr = stack_ptr + sz - offset;
    //  ret.end_of_stack = end_of_stack;
    //  assert(ret.stack_ptr <= end_of_stack);
    //  return ret;
    //}
    ExecEnviron new_frame(unsigned start_at) {
      ExecEnviron ret;
      ret.static_ptr = static_ptr;
      ret.stack_ptr = ret.frame_ptr = stack_ptr + start_at;
      ret.end_of_stack = end_of_stack;
      assert(ret.stack_ptr <= end_of_stack);
      return ret;
    }
    void alloc(unsigned i) {
      stack_ptr += i;
      assert(stack_ptr <= end_of_stack);
    }
  };

  struct CompileEnviron {};

  class CompileWriter : public FStream {
  public:
    hash_set<String> label_names;
    LabelSymbolTable * label_map;
    unsigned indent_level;
    CompileWriter() : indent_level(0) {}
    void indent() {
      for (int i = 0; i != indent_level; ++i)
        *this << ' ';
    }
  };

  static inline void copy_val(void * lhs, const void * rhs, const Type * t) {
    memcpy(lhs, rhs, t->size());
  }


  struct BreakException {};
  struct ReturnException {};

  struct AST : public gc_cleanup {
    String name_;
    String name() {return name_;}
    virtual AST * part(unsigned i) {return 0;}
    const Parse * parse_;
    const Type * type;
    bool lvalue;
    unsigned return_offset; // for rhs values: NPOS if not LHS
    VarLoc addr;            // for lhs values
    AST(String n, const Parse * p = 0) : name_(n), parse_(p), type(), lvalue(false), return_offset(NPOS), addr() {}
    void assert_num_args(int p) {
      if (parse_->num_args() != p) 
        //abort();
        throw error(parse_, "%s: Wrong Number of Arguments", ~name_);
    };
    void assert_num_args(int min, int max) {
      if (parse_->num_args() < min || parse_->num_args() > max) 
        throw error(0, parse_->str(), "%s: Wrong Number of Arguments", ~name_);
    };
    virtual AST * parse_self(const Parse * p, ParseEnviron &) = 0;
      // ^^ returns itself, to allow chaining ie 
      //      new Foo(p)->parse(env);
    virtual void prep_eval(PrepEvalEnviron &) {abort();}
    virtual void eval(ExecEnviron &) {abort();}
    virtual void compile(CompileWriter &, CompileEnviron &) = 0;
    virtual ~AST() {}
    void print();
  };

  template <typename T>
  T & ExecEnviron::ret(const AST * exp) {
    return local_var<T>(exp->return_offset);
  }
  
  void * ExecEnviron::ret(const AST * exp) {
    return local_var(exp->return_offset);
  }

  //
  //
  //

  static inline 
  CompileWriter & operator<< (CompileWriter & o, const char * str) {
    static_cast<FStream &>(o) << str;
    return o;
  }

  static inline 
  CompileWriter & operator<< (CompileWriter & o, AST * v) {
    printf("COMPILE %s\n", ~v->name());
    CompileEnviron dummy;
    v->compile(o, dummy);
    return o;
  }

  struct adj_indent {
    int adj;
    CompileWriter * o;
    explicit adj_indent(int a) : adj(a) {}
  };

  static inline 
  adj_indent operator<< (CompileWriter & o, adj_indent i) {
    i.o = &o;
    return i;
  }

  template <typename T>
  static inline 
  CompileWriter & operator<< (adj_indent i, T v) {
    i.o->indent_level += i.adj;
    *i.o << v;
    i.o->indent_level -= i.adj;
    return *i.o;
  }

  struct indent_ {};
  static const indent_ indent = indent_();

  static inline 
  CompileWriter & operator<< (CompileWriter & o, indent_) {
    o.indent();
    return o;
  }

  //
  //
  //

  struct Cast : public AST {
    Cast(String s) : AST(s) {}
    AST * exp;
    void compile(CompileWriter&, CompileEnviron&);
  };

  struct ImplicitCast : public Cast {
    ImplicitCast(AST * e, const Type * t) 
      : Cast("<cast>") {exp = e; type = t;}
    AST * parse_self(const Parse * p, ParseEnviron & env0) {abort();}
  };

  struct ExplicitCast : public Cast {
    ExplicitCast() : Cast("cast") {}
    AST * parse_self(const Parse * p, ParseEnviron & env0);
  };

  //
  //
  //

  /*
  struct Parm : public gc {
    String name;
    const Type * type;
    const Parse * type_parse;
    Parm(String n, const Parse * t) : name(n), type(), type_parse(t) {}
  };
  */
 
  struct Block;

  struct Declaration : public AST {
    Declaration(String n) : AST(n) {}
    enum StorageClass {NONE, AUTO, STATIC, EXTERN, REGISTER};
    StorageClass storage_class;
    bool inline_;
    void parse_flags(const Parse * p) {
      storage_class = NONE;
      if (p->flag("auto")) storage_class = AUTO;
      else if (p->flag("static")) storage_class = STATIC;
      else if (p->flag("extern")) storage_class = EXTERN;
      else if (p->flag("register")) storage_class = REGISTER;
      inline_ = false;
      if (p->flag("inline")) inline_ = true;
    }
    void write_flags(CompileWriter & f) const {
      switch (storage_class) {
      case AUTO: 
        f << "auto "; break;
      case STATIC: 
        f << "static "; break;
      case EXTERN: 
        f << "extern "; break;
      case REGISTER: 
        f << "register "; break;
      default:
        break;
      }
      if (inline_)
        f << "inline ";
    }
  };

 struct Fun : public Declaration {
    Fun() : Declaration("fun") {}
    AST * part(unsigned i);
    String name;
    Symbol * sym;
    const Tuple * parms;
    const Type * ret_type;
    Block * body;
    LabelSymbolTable * labels;
    unsigned frame_offset;
    unsigned frame_size;
    AST * parse_self(const Parse * p, ParseEnviron & env0);
    void resolve(ResolveEnviron & env0);
    void eval(ExecEnviron & env);
    void compile(CompileWriter & f, CompileEnviron & env);
  };

  struct Literal : public AST {
    Literal() : AST("literal") {}
    AST * part(unsigned i);
    long long value;
    AST * parse_self(const Parse * p, ParseEnviron &);
    void resolve(ResolveEnviron & env);
    void eval(ExecEnviron & env);
    void compile(CompileWriter & f, CompileEnviron &);
  };

  struct FloatC : public AST {
    FloatC() : AST("float") {}
    //AST * part(unsigned i);
    long double value;
    AST * parse_self(const Parse * p, ParseEnviron &);
    void compile(CompileWriter & f, CompileEnviron &);
  };

  struct StringC : public AST {
    StringC() : AST("string") {}
    //AST * part(unsigned i);
    String orig;
    String value; // unused at the moment
    AST * parse_self(const Parse * p, ParseEnviron &);
    void compile(CompileWriter & f, CompileEnviron &);
  };

  struct CharC : public AST {
    CharC() : AST("char") {}
    //AST * part(unsigned i);
    String orig;
    char value; // unused at the moment
    AST * parse_self(const Parse * p, ParseEnviron &);
    void compile(CompileWriter & f, CompileEnviron &);
  };

  //
  //
  //

  int ct_value(const Parse * p, ParseEnviron &);

  AST * parse_top(const Parse * p);
  AST * parse_exp(const Parse * p, ParseEnviron & env);

}
