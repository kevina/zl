#include <deque>

#include "peg.hpp"
#include "parse_decl.hpp"
#include "string_buf.hpp"
#include "expand.hpp"
#include "ast.hpp"
#include "type.hpp"

// the ";" is just noise at this point and is NOT expected to be included

/*
  -- declarations
  (var ID TYPE INIT) flags: 
  (talias alias TYPE)
  (fun ID PARMS RET BODY]) flags:
  (struct ID [BODY]) 
  (union ID [BODY])
  -- types live in a diffrent namespace, can be wrapped my (type ...)
  -- any type have any of the qualifies as flags
  (.ptr TYPE)
  (.array TYPE EXP)
  (.fun PARMS RETURN_TYPE)
  (struct ID)
  (union ID)
  (<typename>)
*/

// TO FIX
void ignore() {}

//static const Syntax * const TYPE = new Syntax("type");

static unsigned uniq_num = 0;

enum Mode {IdRequired, IdNotRequired, StopAtParen};

struct DeclWorking {
  DeclWorking(SyntaxBuilder &); // FIXME

  const Syntax * gen_sym() {
    StringBuf buf;
    buf.printf("_s_%d_", uniq_num++);
    return SYN(buf.freeze());
  }

  SyntaxBuilder & type_scope;

  bool parse_first_part(parts_iterator & i, 
                        parts_iterator end, 
                        Environ & env, 
                        bool top_level = false); 
                         // not really top_level, but can't think of
                         // better name
  const Syntax * parse_outer_type_info(const Syntax * & id, 
                                      parts_iterator & i, 
                                      parts_iterator end,
                                      const Syntax * t,
                                      Environ & env,
                                      Mode = IdRequired);
  const Syntax * parse_init_exp(parts_iterator & i, 
                               parts_iterator end);

  typedef Vector<const Syntax *> Attributes;
  Attributes attributes;
  bool try_attributes(const Syntax * p) {
    if (*p == "__for_ct" || *p == "__ct_callback" || *p == "__static_constructor" || *p == "__need_snapshot" || *p == "__shadow") {
      attributes.push_back(p);
      return true;
    } else {
      return false;
    }
  }

  const Syntax * storage_class;
  enum What {VAR, TYPEDEF} what;

  bool try_storage_class(const Syntax * p) {
    if (*p == "typedef") {
      what = TYPEDEF;
    } else if (*p == "extern" || *p == "static" ||
               *p == "auto"  || *p == "register") {
      what = VAR;
    } else {
      return false;
    }
    if (storage_class) ignore();
    storage_class = p;
    return true;
  }

  SourceStr inner_type_str;
  enum Sign {NO_SIGN, UNSIGNED, SIGNED} sign;
  bool try_sign(const Syntax * p) {
    if      (*p == "unsigned") set_sign(UNSIGNED);
    else if (*p == "signed")   set_sign(SIGNED);
    else return false;
    inner_type_str.adj(p->str());
    return true;
  }
  void set_sign(Sign v) {
    if (sign && sign != v) ignore();
    else sign = v;
  }
  enum Size {NO_SIZE, SHORT, LONG, LONG_LONG} size;
  bool try_size(const Syntax * p) {
    if (*p == "short") {
      if (size != NO_SIZE) ignore();
      size = SHORT;
    } else if (*p == "long") {
      if      (size == NO_SIZE) size = LONG; 
      else if (size == LONG)    size = LONG_LONG;
      else ignore();
    } else {
      return false;
    }
    inner_type_str.adj(p->str());
    return true;
  }
  enum BaseType {NO_BT, VOID, CHAR, INT, FLOAT, DOUBLE} base_type;
  bool try_base_type(const Syntax * p) {
    if      (*p == "void")      set_base_type(VOID);
    else if (*p == "char")      set_base_type(CHAR);
    else if (*p == "int")       set_base_type(INT);
    else if (*p == "float")     set_base_type(FLOAT);
    else if (*p == "double")    set_base_type(DOUBLE);
    else return false;
    inner_type_str.adj(p->str());
    return true;
  }
  void set_base_type(BaseType v) {
    if (base_type && base_type != v) ignore();
    if (type_symbol) ignore();
    else base_type = v;
  }

  SyntaxBuilder qualifiers;
  bool try_qualifier(const Syntax * p, SyntaxBuilder & qualifiers) {
    if (p->eq("const", "restrict", "volatile"))
      qualifiers.add_flag(p);
    else if (p->eq("__const__", "__const"))
      qualifiers.add_flag(SYN("const"));
    else if (p->eq("__restrict__", "__restrict"))
      qualifiers.add_flag(SYN("restrict"));
    else if (p->eq("__volatile__", "__volatile"))
      qualifiers.add_flag(SYN("volatile"));
    else return false;
    return true;
  }
  bool try_qualifier(const Syntax * p) {return try_qualifier(p, qualifiers);}

  const Syntax * inline_;
  const Syntax * virtual_;
  bool pure_virtual;
  bool try_function_specifier(const Syntax * p) {
    if      (p->eq("inline", "__inline", "__inline__")) inline_ = p;
    else if (*p == "virtual") virtual_ = p;
    else return false;
    return true;
  }

  bool try_explicit_type(const Syntax * p, Environ & env) {
    // doesn't make sense to have two types, so if we already have a
    // type, fail
    if (base_type || type_symbol || inner_type) return false;
    if (p->is_a(".type")) {
      inner_type = p->arg(0);
      return true;
    } else {
      return false;
    }
  }

  const Syntax * type_symbol_p;
  const ast::TypeSymbol * type_symbol;
  bool try_type_name(const Syntax * p, Environ & env) {
    // doesn't make sense to have two types, so if we already have a
    // type, fail
    if (base_type || type_symbol || inner_type) return false;
    // Handle template types
    if (p->is_a("tid")) {
      printf("tid>%s\n", ~p->to_string());
      //const ast::TypeSymbol * ts = env.symbols.find<ast::TypeSymbol>(p->arg(0));
      if (true) {
        SyntaxBuilder res(p->arg(0));
        if (p->arg(1)->is_a("<>")) {
          expand_template_parms(p->arg(1), res, env);
        } else {
          res.add_parts(p->args_begin() + 1, p->args_end());
        }
        inner_type = res.build(p->str());
        printf("TID>%s\n", ~inner_type->to_string());
        return true;
      } else {
        return false;
      }
    } else {
      const ast::TypeSymbol * ts = env.symbols.find<ast::TypeSymbol>(p);
      if (ts) {
        type_symbol_p = p;
        type_symbol = ts;
        return true;
      } else {
        return false;
      }
    }
  }
  bool try_typeof(const Syntax * p, Environ &) {
    if (*p == ".typeof") {
      inner_type = SYN(p->part(0), p->arg(0));
      return true;
    } else {
      return false;
    }
  }
  Syntax * handle_embedded_decl(const Syntax * p, Environ & env, bool by_itself) {
    //xprintf("input>%s\n", ~p->to_string());
    p = limited_expand(p, env);
    //printf("INPUT>%s\n", ~p->to_string());
    if (p->is_a("<@")) {
      //printf("LIVE ONE\n");
      unsigned i;
      for (i = 0; i != p->num_args()-1; ++i) {
        type_scope.add_part(p->arg(0));
      }
      //printf("%d: %s\n", i, ~p->arg(i)->to_string());
      if (by_itself) {
        inner_type = p->arg(i);
        return NULL;
      } else {
        return p->arg(i);
      }
    } else {
      //printf("NOT\n");
      return p;
    }
  }
  bool try_struct_union(const Syntax * p, Environ &, bool by_itself = false);
  bool try_enum(const Syntax * p, Environ &, bool by_itself = false);
  const Syntax * parse_struct_union_body(const Syntax * p, Environ &, bool & bit_field);
  bool dots;
  Syntax * inner_type;
  Syntax * field_size;
  void make_inner_type(const Syntax * orig);
  const Syntax * try_pointers(parts_iterator & i, 
                              parts_iterator end,
                              const Syntax * t);
  const Syntax * try_reference(parts_iterator & i, 
                               parts_iterator end,
                               const Syntax * t);
  const Syntax * try_arrays(parts_iterator & i, 
                            parts_iterator end,
                           const Syntax * t);
  
  const Syntax * make_declaration(const Syntax * id, const Syntax * t, 
                                  const Syntax * init1 = NULL, const Syntax * init2 = NULL);
  const Syntax * make_function(const Syntax * id, const Syntax * t, const Syntax * body);
  const Syntax * parse_fun_parms(const Syntax * parms, Environ &);
  const Syntax * make_function_type(const Syntax *, const Syntax *, Environ & env);
};

class ParseDeclImpl : public ParseDecl {
public:
  ParseDeclImpl() {}
  const Syntax * parse_decl(const Syntax * p, Environ &, bool field_pos);
  const Syntax * parse_type(parts_iterator & i, parts_iterator e, Environ &, bool for_new = false);
  const Syntax * parse_type(const Syntax * p, Environ &);
  const Syntax * parse_fun_parms(const Syntax*, Environ &);
  void init() {}
  ~ParseDeclImpl() {}
};

ParseDecl * parse_decl_ = new ParseDeclImpl();

DeclWorking::DeclWorking(SyntaxBuilder & p)
  : type_scope(p), storage_class(NULL), what(VAR), 
    sign(NO_SIGN), size(NO_SIZE), base_type(NO_BT),
    inline_(NULL), virtual_(NULL), pure_virtual(false), 
    type_symbol(NULL), dots(false), inner_type(NULL),
    field_size(NULL) {}

//
// The real code....
//

const Syntax * ParseDeclImpl::parse_decl(const Syntax * p, Environ & env, bool field_pos)
{
  SyntaxBuilder res;

  //printf(">IN>%s\n", ~p->to_string());

  // make a local (shallow) copy of the args since we might need to
  // modify them
  Vector<const Syntax *> args(p->args_begin(), p->args_end());

  parts_iterator i = args.pbegin();
  parts_iterator end = args.pend();

  DeclWorking w(res);

  bool r = w.parse_first_part(i, end, env, true);

  //if (i != end)
  //  printf("<>%d %s\n", i - p->args_begin(), ~(*i)->to_string());

  if (i != end && r && (*i)->is_a("()")) 
    try {
      const Syntax * paran = reparse("TOKENS", (*i)->inner());
      const Syntax * parms = w.parse_fun_parms(paran, env);
      if (env.scope >= ast::LEXICAL) return NULL;
      const_cast<const Syntax * &>(*i) = parms;
      w.inner_type = SYN(SYN("void"));
      --i;
    } catch (...) {}

  if (i != end && (*i)->eq("~")) {

     if (env.scope >= ast::LEXICAL) return NULL;
     w.inner_type = SYN(SYN("void"));

   } else if (!r) {

    //printf("NO!\n");
    return NULL;

  } else {

    w.make_inner_type(p);

  }

  while (i != end) {

    //printf(">1>%s\n", ~(*i)->to_string());

    const Syntax * id;
    const Syntax * t = w.parse_outer_type_info(id, i, end, w.inner_type, env, 
                                               field_pos ? IdNotRequired : IdRequired);

    if (!id) {
      id = SYN(".");
    }

    //if (i != end)
    //  printf(">2>%s\n", ~(*i)->to_string());

    if (i != end && (*i)->eq(":")) {
      w.field_size = SYN("0");
      ++i;
      while (i != end && (*i)->ne("{}")) ++i;
    }

    const Syntax * decl;
    if (i != end && (*i)->eq("=")) {
      ++i;
      const Syntax * init = w.parse_init_exp(i, end);
      decl = w.make_declaration(id, t, init);
    } else if (i != end && (*i)->is_a("()")) {
      // we are calling a constructor
      const Syntax * init = *i;
      ++i;
      decl = w.make_declaration(id, t, SYN("."), init);
    } else if (i != end && (*i)->is_a("{}")) {
      const Syntax * body = *i;
      ++i;
      decl = w.make_function(id, t, body);
    } else {
      decl = w.make_declaration(id, t);
    }

    res.add_part(decl);

    if (i == end) break;
    //printf(">3>%s\n", ~(*i)->to_string());

    if (!(*i)->eq(",")) {
      throw error(*i, "Expected \",\" got \"%s\".<1>", ~(*i)->to_string());
    }
    ++i;
    
  }

  for (mutable_parts_iterator i = res.parts_begin(), e = res.parts_end(); i != e; ++i) {
    const Syntax * p = *i;
    const Syntax * n = p->arg(0);
    if (n->is_a("::")) {
      const Syntax * c = n->arg(0);
      const Syntax * n2 = n->arg(1);
      SemiMutableSyntax * p2 = p->clone();
      p2->arg(0) = n2;
      *i = SYN(SYN("memberdecl"), c, p2);
    }
  }

  Syntax * ret;
  if (res.num_parts() == 1)
    ret = res.part(0);
  else
    ret = SYN(SYN("@"), PARTS(res.parts_begin(), res.parts_end()));

  //printf(">OUT>%s\n", ~ret->to_string());
  
  return ret;
}

const Syntax * ParseDeclImpl::parse_type(parts_iterator & i, 
                                         parts_iterator end, 
                                         Environ & env,
                                         bool for_new) 
{
  if (i == end) return NULL;
  const Syntax * p = *i;
  SyntaxBuilder dummy;
  DeclWorking w(dummy);
  const Syntax * id = NULL;
  bool r = w.parse_first_part(i, end, env);
  if (!r) return NULL;
  w.make_inner_type(p);
  const Syntax * t = w.parse_outer_type_info(id, i, end, w.inner_type, env, 
                                             for_new ? StopAtParen : IdNotRequired);
  if (id) return NULL;
  //assert(dummy.empty()); // FIXME: Error Message
  return t;
}

const Syntax * ParseDeclImpl::parse_type(const Syntax * p, Environ & env) {
  parts_iterator i = p->args_begin();
  parts_iterator end = p->args_end();
  const Syntax * t = parse_type(i, end, env);
  if (i != end) return NULL;
  return t;
}

const Syntax * ParseDeclImpl::parse_fun_parms(const Syntax * p, Environ & env) {
  SyntaxBuilder dummy;
  DeclWorking w(dummy);
  const Syntax * t = w.parse_fun_parms(p, env);
  assert(dummy.empty()); // FIXME: Error Message
  return t;
}

bool DeclWorking::parse_first_part(parts_iterator & i, 
                                   parts_iterator end,
                                   Environ & env, 
                                   bool top_level)
{
  parts_iterator begin = i;
  bool by_itself = top_level && i + 1 == end;
  if (i != end && (*i)->eq("...")) {
    dots = true;
    inner_type = SYN("...", (*i)->str());
    ++i;
  } else while (i != end) {
    const Syntax * cur = *i;
    cur = handle_embedded_decl(cur, env, by_itself);
    if (!cur) {
      ++i;
    } else if (const Syntax * p = try_id(cur)) {
      const Syntax * p = cur;
      bool any = 
        try_attributes(p) ||
        try_storage_class(p) ||
        try_sign(p) ||
        try_size(p) ||
        try_base_type(p) ||
        try_qualifier(p) ||
        try_function_specifier(p) ||
        try_type_name(p, env);
      if (!any) break;
      ++i;
    } else if (try_type_name(cur, env)) {
      //fixme: this seams like a hack...
      ++i;
    } else if (try_explicit_type(cur, env)) {
      ++i;
    } else if (try_typeof(cur, env)) {
      ++i;
    } else if (try_struct_union(cur, env, by_itself)) {
      ++i;
    } else if (try_enum(cur, env, by_itself)) {
      ++i;
    } else {
      break;
    }
  }
  if (i == begin) return false;
  else return true;
}

bool DeclWorking::try_struct_union(const Syntax * p, Environ & env, bool by_itself) {
  const Syntax * name = NULL;
  const Syntax * body = NULL;
  if (p->is_a("struct") || p->is_a("union")) {
    unsigned i = 0;
    if (p->num_args() == 0) abort(); //throw error(p->str().source, p->str().end, 
                            //            "Expected indentifer or \"{\" after \"%s\".",
                            //            ~p->what());
    name = p->arg(i);
    ++i;
    if (i == p->num_args()) goto finish;
    if (p->arg(i)->is_a("{...}")) {
      body = p->arg(i);
      ++i;
    }
    if (i != p->num_args()) abort(); // internal error, should't happen
  finish:
    if (name->what().empty())
      name = gen_sym();
    inner_type = SYN(p->part(0), name);
    if (body || by_itself) {
      const Syntax * n = name;
      if (n->simple())
        n = SYN(SYN("`"), n, SYN("tag"));
      SyntaxBuilder struct_union(p->part(0));
      struct_union.add_part(n);
      struct_union.set_flags(p);
      if (body) {
         bool bit_field = false;
         const Syntax * b = parse_struct_union_body(body, env, bit_field);
         struct_union.add_part(b);
         if (bit_field) {
           struct_union.add_flag(SYN("bit-field"));
         }
      }
      //if (body)
      //  struct_union.add_part(body);
      type_scope.add_part(struct_union.build());
    }
    return true;
  } else {
    return false;
  }
}

// FIXME: Eventually Eliminate
const Syntax * DeclWorking::parse_struct_union_body(const Syntax * p0, Environ & env, bool & bit_field)
{
  SyntaxBuilder res(SYN("{...}"));
  unsigned anon_num = 0;

  std::deque<const Syntax *> args(p0->args_begin(), p0->args_end());

  while (!args.empty()) {

    const Syntax * p = args.front();
    args.pop_front();
    if (p->is_a("@{}"))
      p = reparse("STMTS", p->inner(), &env);
    if (p->is_a("@")) {
      args.insert(args.begin(), p->args_begin(), p->args_end());
      continue;
    }

    DeclWorking w(type_scope);

    if (p->is_a("stmt")) {

        parts_iterator i = p->args_begin();
        parts_iterator end = p->args_end();
        
        {
          bool r = w.parse_first_part(i, end, env);
          if (!r) throw error(p, "Expected declaration in \"%s\".", ~p->to_string());
        }
        
        w.make_inner_type(p);
        
        while (i != end) {

          //printf(">?1>%s\n", ~(*i)->to_string());
          
          const Syntax * id = NULL;
          const Syntax * t = w.parse_outer_type_info(id, i, end, w.inner_type, env, IdNotRequired);

          if (!id) {
            char buf[8];
            snprintf(buf, 8, "$anon%d", anon_num++);
            id = SYN(buf);
          }

          //if (id)
          //  printf(">id>%s\n", ~id->to_string());
          
          //if (i != end)
          //  printf(">?2>%s\n", ~(*i)->to_string());

          //const Syntax * decl = w.make_declaration(id, t);
          // FIXME: Duplicate code from parse_decl, also...
          const Syntax * decl;

          if (i != end && (*i)->eq(":")) {
            bit_field = true;
            ++i;
            if (i != end) {/*printf("123>%s\n", ~(*i)->to_string());*/ ++i;}
          }
            
          if (i != end && (*i)->eq("=")) {
            ++i;
            const Syntax * init = w.parse_init_exp(i, end);
            decl = w.make_declaration(id, t, init);
          } else if (i != end && (*i)->is_a("{}")) {
            const Syntax * body = *i;
            ++i;
            decl = w.make_function(id, t, body);
          } else {
            decl = w.make_declaration(id, t);
          }
          
          res.add_part(decl);
          
          if (i == end) break;
          
          if (!(*i)->eq(","))
            throw error(*i, "Expected \",\"<2> got %s.", ~(*i)->to_string());
          ++i;
        }
    } else {
      res.add_part(p);
    }
  }
  return res.build();
}

bool DeclWorking::try_enum(const Syntax * p, Environ & env, bool by_itself) {
  const Syntax * name = NULL;
  const Syntax * body = NULL;
  if (p->is_a("enum")) {
    unsigned i = 0;
    if (p->num_args() == 0) throw error(p->str().source, p->str().end, 
                                        "Expected indentifer or \"{\" after \"%s\".",
                                        ~p->what());
    name = p->arg(i);
    ++i;
    if (i == p->num_args()) goto finish;
    if (p->arg(i)->is_a("{,}")) {
      body = p->arg(i);
      ++i;
    }
    if (i != p->num_args()) abort(); // internal error, should't happen
  finish:
    if (name->what().empty())
      name = gen_sym();
    inner_type = SYN(p->part(0), name);
    if (body || by_itself) {
      SyntaxBuilder enum_(p->part(0));
      enum_.add_part(name);
      if (body)
        enum_.add_part(body);
      type_scope.add_part(enum_.build());
    }
    return true;
  } else {
    return false;
  }
}

void DeclWorking::make_inner_type(const Syntax * orig) {
  if (!inner_type) {
    StringBuf t;
    switch (base_type) {
    case NO_BT:
      if (type_symbol) {
        if (size != NO_SIZE) ignore();
        if (sign != NO_SIGN) ignore();
        break;
      } else if (size || sign) {
        goto int_case;
      } else {
        throw error(orig, "No type name specified in declaration.");
      }
    case VOID:
      if (size != NO_SIZE) ignore();
      if (sign != NO_SIGN) ignore();
      t = "void";
      break;
    case CHAR:
      if (size != NO_SIZE) ignore();
      switch (sign) {
      case NO_SIGN:
        t = "char";
        break;
      case UNSIGNED:
        t = "unsigned-char";
        break;
      case SIGNED:
        t = "signed-char";
        break;
      }
      break;
    int_case:
    case INT:
      switch (size) {
      case NO_SIZE:
        t = "int";
        break;
      case SHORT:     
        t = "short"; 
        break;
      case LONG:
        t = "long";
        break;
      case LONG_LONG:
        t = "long-long";
        break;
      }
      switch (sign) {
      case NO_SIGN:
      case SIGNED:
        break;
      case UNSIGNED:
        t.prepend("unsigned-");
        break;
      }
      break;
    case FLOAT:
      if (size != NO_SIZE) ignore();
      if (sign != NO_SIGN) ignore();
      t = "float";
      break;
    case DOUBLE:
      if (sign != NO_SIGN) ignore();
      switch (size) {
      case NO_SIZE:
        t = "double";
        break;
      case LONG:
        t = "long-double";
        break;
      default:
        ignore();
        t = "double";
        break;
      }
      break;
    }
    //inner_type->add_part(type_symbol ? SYN(type_symbol_p, type_symbol) : SYN(t.freeze()));
    // Don't use the resolved symbol the final parse might bind it to
    // a different symbol.  Otherwise "class X {X foo() {...}};" won't
    // work as expected.
    inner_type = SYN(type_symbol ? type_symbol_p : SYN(t.freeze()));
  } else {
    // stuct, union or template
    // nothing to do
  }
  if (!qualifiers.empty()) {
    inner_type = SYN(inner_type->str(),
                     PARTS(inner_type->parts_begin(), inner_type->parts_end()),
                     FLAGS(qualifiers.flags_begin(), qualifiers.flags_end()));
    //inner_type->set_flags(qualifiers);
  }
}

// returns a type and sets id
const Syntax * DeclWorking::parse_outer_type_info(const Syntax * & id, 
                                                 parts_iterator & i, 
                                                 parts_iterator end,
                                                 const Syntax * t,
                                                 Environ & env,
                                                 Mode mode) 
{
  assert(t);
  parts_iterator prev;
  do {
    prev = i;
    t = try_pointers(i, end, t);
    t = try_reference(i, end, t);
  } while (i != prev);

  const Syntax * outer = NULL;

  bool stop = false;
  const Syntax * p;
  if (i == end || (*i)->eq(",", "=", ":")) {
    stop = true;
    goto def;
  } else if ((p = handle_w_tilda(i, end, env))) {
    id = p;
  } else if ((p = handle_operator_fun_id(i, end, env))) {
    id = p;
  } else if ((p = try_id(*i))) {
    id = p;
    ++i;
  } else if (mode != StopAtParen && (*i)->is_a("()")) {
    outer = reparse("TOKENS", (*i)->inner());
    ++i;
  } else
  def: 
    if (mode == IdRequired)
      throw error(*i, "Expected identifier or \"(\".");

  if (stop || i == end) {
    // do nothing
  } else if (mode != StopAtParen && (*i)->is_a("()")) {
    try {
      t = make_function_type(t, reparse("TOKENS", (*i)->inner()), env);
      ++i;
    } catch (Error * err) {
      printf("note: %s\n", ~err->message());
    }
  } else if ((*i)->is_a(".") || (*i)->is_a("(...)")) {
    t = make_function_type(t, *i, env);
    ++i;
  } else {
    // we axre an array of type t
    t = try_arrays(i, end, t);
  }

  if (i != end && (*i)->eq("throw")) {
    // ignore throw spec. for now
    ++i;
    if (i == end || !(*i)->is_a("()")) 
      throw error(*i, "Expected \"(\"");
    ++i;
  } 
  
  if (outer) {
    parts_iterator j = outer->args_begin();
    t = parse_outer_type_info(id, j, outer->args_end(), t, env, mode);
    if (j != outer->args_end()) throw error(*j, "Expected \")\".");
    return t;
  } else {
    return t;
  }
}

const Syntax * DeclWorking::parse_fun_parms(const Syntax * parms,
                                            Environ & env)
{
  if (parms->is_a(".")) return parms;
  SyntaxBuilder ps(SYN("."));
  //printf("MAKE FUNCTION TYPE: %s %s\n", ~ret->to_string(), ~parms->to_string());
  parts_iterator i = parms->args_begin();
  parts_iterator end = parms->args_end();
  if (i != end) for (;;) {
    parts_iterator begin = i;
    DeclWorking w(type_scope);
    const Syntax * id = NULL;
    bool r = w.parse_first_part(i, end, env, false);
    if (!r) throw error(*i, "Expected type or \"...\".");
    if (w.dots) {
      ps.add_part(w.inner_type); // FIXME: Preserve source info..
    } else {
      w.make_inner_type(parms);
      const Syntax * t = w.parse_outer_type_info(id, i, end, w.inner_type, env, IdNotRequired);
      SyntaxBuilder p;
      assert(t);
      p.add_part(t);
      if (id)
	p.add_part(id);
      ps.add_part(p.build());
      //ps->add_part(t);
    } 
    if (i == end) break;
    if (!(*i)->eq(",")) throw error(*i, "Expected \",\" got \"%s\".", ~(*i)->to_string());
    ++i;
  }
  const Syntax * res = ps.build();
  if (res->num_args() == 1
      && res->arg(0)->num_parts() == 1
      && res->arg(0)->part(0)->num_parts() == 1
      && res->arg(0)->part(0)->part(0)->eq("void")) 
    return SYN(SYN("."));
  else
    return res;
}

const Syntax * DeclWorking::make_function_type(const Syntax * ret,
                                               const Syntax * parms,
                                               Environ & env)
{
  return SYN(SYN(".fun"), parse_fun_parms(parms, env), ret);
}

const Syntax * DeclWorking::parse_init_exp(parts_iterator & i, 
                                            parts_iterator end)
{
  parts_iterator begin = i;
  while (i != end && !(*i)->eq(","))
    ++i;
  return SYN(SYN("exp"), PARTS(begin, i));
}

const Syntax * DeclWorking::try_pointers(parts_iterator & i, 
                                         parts_iterator end,
                                         const Syntax * t) 
{
  while (i != end && (*i)->eq("*")) {
    ++i;
    SyntaxBuilder res(SYN(".ptr"));
    res.add_part(t);
    while (i != end && try_qualifier((*i), res))
      ++i;
    t = res.build();
  }
  return t;
}

const Syntax * DeclWorking::try_reference(parts_iterator & i, 
                                          parts_iterator end,
                                          const Syntax * t) 
{
  if (i != end && (*i)->eq("&")) {
    ++i;
    return SYN(SYN(".ref"), t);
  } else {
    return t;
  }
}

const Syntax * DeclWorking::try_arrays(parts_iterator & i, 
                                      parts_iterator end,
                                      const Syntax * t) 
{
  Vector<const Syntax *> stack;
  while (i != end && (*i)->is_a("[]")) {
    stack.push_back(reparse("ARRAY_SIZE", (*i)->inner()));
    ++i;
  }
  Vector<const Syntax *>::const_reverse_iterator j = stack.rbegin();
  Vector<const Syntax *>::const_reverse_iterator e = stack.rend();
  while (j != e) {
    t = SYN(SYN(".array"), t, *j);
    ++j;
  }
  return t;
}

const Syntax *  DeclWorking::make_declaration(const Syntax * id, const Syntax * t, 
                                              const Syntax * init1, const Syntax * init2)
{
  if (what == VAR) {
    if (t->is_a(".fun")) {
      return make_function(id, t, NULL);
    } else {
      SyntaxBuilder res(SYN("var"));
      res.add_part(id);
      assert(t);
      res.add_part(t);
      if (init1)
        res.add_part(init1);
      if (init2)
        res.add_part(init2);
      if (storage_class)
        res.add_flag(storage_class);
      if (field_size)
        res.add_flag(SYN(SYN("field_size"), field_size));
      for (Attributes::const_iterator i = attributes.begin(), e =  attributes.end();
           i != e; ++i)
        res.add_flag(*i);
      return res.build();
    }
  } else if (what == TYPEDEF) {
    return SYN(SYN("talias"), id, t);
  } else {
    abort();
  }
}

const Syntax * DeclWorking::make_function(const Syntax * id, const Syntax * t, const Syntax * body)
{
  if (what == VAR) {
    SyntaxBuilder res(SYN("fun"));
    if (inline_)
      res.add_flag(inline_);
    if (virtual_)
      res.add_flag(virtual_);
    if (storage_class)
      res.add_flag(storage_class);
    for (Attributes::const_iterator i = attributes.begin(), e =  attributes.end();
         i != e; ++i)
      res.add_flag(*i);
    res.add_part(id);
    res.add_part(t->arg(0));
    res.add_part(t->arg(1));
    if (body)
      res.add_part(body);
    return res.build();
  } else {
    abort();
  }
}
