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
  (.pointer TYPE)
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

struct DeclWorking {
  DeclWorking(Parts &); // FIXME

  const Syntax * gen_sym() {
    StringBuf buf;
    buf.printf("_s_%d_", uniq_num++);
    return new Syntax(buf.freeze());
  }

  Parts & type_scope;

  bool parse_first_part(Parts::const_iterator & i, 
                        Parts::const_iterator end, 
                        Environ & env, 
                        bool top_level = false); 
                         // not really top_level, but can't think of
                         // better name
  const Syntax * parse_outer_type_info(const Syntax * & id, 
                                      Parts::const_iterator & i, 
                                      Parts::const_iterator end,
                                      const Syntax * t,
                                      Environ & env,
                                      bool id_required = true);
  const Syntax * parse_init_exp(Parts::const_iterator & i, 
                               Parts::const_iterator end);

  typedef Vector<const Syntax *> Attributes;
  Attributes attributes;
  bool try_attributes(const Syntax * p) {
    if (*p == "__for_ct" || *p == "__ct_callback") {
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
    if (!type_name.empty()) ignore();
    else base_type = v;
  }

  Flags qualifiers;
  bool try_qualifier(const Syntax * p, Flags & qualifiers) {
    if (*p == "const" || *p == "restrict" || *p == "volatile") 
      qualifiers.insert(p);
    else return false;
    return true;
  }
  bool try_qualifier(const Syntax * p) {return try_qualifier(p, qualifiers);}

  const Syntax * inline_;
  const Syntax * virtual_;
  bool pure_virtual;
  bool try_function_specifier(const Syntax * p) {
    if      (*p == "inline") inline_ = p;
    else if (*p == "virtual") virtual_ = p;
    else return false;
    return true;
  }


  SymbolName type_name;
  bool try_type_name(const Syntax * p, Environ & env) {
    SymbolName n = p->what_;
    if (env.symbols.find<ast::TypeSymbol>(n)) {
      if (base_type) ignore();
      type_name = n;
      return true;
    } else {
      return false;
    }
  }
  bool try_typeof(const Syntax * p, Environ &) {
    if (*p == ".typeof") {
      inner_type = new Syntax(p->part(0), p->arg(0));
      return true;
    } else {
      return false;
    }
  }
  bool try_struct_union(const Syntax * p, Environ &, bool by_itself = false);
  bool try_enum(const Syntax * p, Environ &, bool by_itself = false);
  const Syntax * parse_struct_union_body(const Syntax * p, Environ &);
  bool dots;
  Syntax * inner_type;
  void make_inner_type(const Syntax * orig);
  const Syntax * try_pointers(Parts::const_iterator & i, 
                             Parts::const_iterator end,
                             const Syntax * t);
  const Syntax * try_arrays(Parts::const_iterator & i, 
                           Parts::const_iterator end,
                           const Syntax * t);

  const Syntax * make_declaration(const Syntax * id, const Syntax * t, const Syntax * init = NULL);
  const Syntax * make_function(const Syntax * id, const Syntax * t, const Syntax * body);
  const Syntax * make_function_type(const Syntax *, const Syntax *, Environ & env);
};

class ParseDeclImpl : public ParseDecl {
public:
  ParseDeclImpl() {}
  const Syntax * parse_decl(const Syntax * p, Environ &);
  const Syntax * parse_type(const Syntax * p, Environ &);
  
  void init() {}
  ~ParseDeclImpl() {}
};

ParseDecl * parse_decl_ = new ParseDeclImpl();

DeclWorking::DeclWorking(Parts & p)
  : type_scope(p), storage_class(NULL), what(VAR),
    sign(NO_SIGN), size(NO_SIZE), base_type(NO_BT),
    inline_(NULL), virtual_(NULL), pure_virtual(false),
    dots(false), inner_type(NULL) {}

//
// The real code....
//

const Syntax * ParseDeclImpl::parse_decl(const Syntax * p, Environ & env)
{
  Syntax * res = new Syntax(new Syntax("slist"));

  Parts::const_iterator i = p->args_begin();
  Parts::const_iterator end = p->args_end();

  DeclWorking w(res->d->parts);

  {
    bool r = w.parse_first_part(i, end, env, true);
    if (!r) return NULL;
  }

  w.make_inner_type(p);

  while (i != end) {

    const Syntax * id;
    const Syntax * t = w.parse_outer_type_info(id, i, end, w.inner_type, env);

    const Syntax * decl;
    if (i != end && (*i)->is_a("sym", "=")) {
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
    
    res->add_part(decl);

    if (i == end) break;

    if (!(*i)->is_a("sym", ","))
      throw error(*i, "Expected \",\".");
    ++i;
    
  }
  return res;
}

const Syntax * ParseDeclImpl::parse_type(const Syntax * p, Environ & env) {
  Parts dummy;
  Parts::const_iterator i = p->args_begin();
  Parts::const_iterator end = p->args_end();
  DeclWorking w(dummy);
  const Syntax * id = NULL;
  bool r = w.parse_first_part(i, end, env);
  if (!r) return NULL;
  w.make_inner_type(p);
  const Syntax * t = w.parse_outer_type_info(id, i, end, w.inner_type, env, false);
  if (i != end || id) return NULL;
  assert(dummy.empty()); // FIXME: Error Message
  return t;
}

bool DeclWorking::parse_first_part(Parts::const_iterator & i, 
                                   Parts::const_iterator end,
                                   Environ & env, 
                                   bool top_level)
{
  Parts::const_iterator begin = i;
  bool by_itself = top_level && i + 1 == end;
  if (i != end && (*i)->is_a("sym", "...")) {
    dots = true;
    inner_type = new Syntax("...", (*i)->str());
    ++i;
  } else while (i != end) {
    const Syntax * cur = *i;
    if (cur->is_a("id")) {
      const Syntax * p = cur->arg(0);
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
  if (p->is_a("struct") || p->is_a("union") || p->is_a("class")) {
    unsigned i = 0;
    if (p->num_args() == 0) throw error(p->str().source, p->str().end, 
                                        "Expected indentifer or \"{\" after \"%s\".",
                                        ~p->what());
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
    inner_type = new Syntax(p->part(0), name);
    if (body || by_itself) {
      Syntax * struct_union = new Syntax(p->part(0));
      struct_union->add_part(name);
      struct_union->set_flags(p);
      if (body)
        struct_union->add_part(parse_struct_union_body(body, env));
      type_scope.push_back(struct_union);
    }
    return true;
  } else {
    return false;
  }
}

const Syntax * DeclWorking::parse_struct_union_body(const Syntax * p0, Environ & env)
{
  Syntax * res = new Syntax();
  for (unsigned h = 0; h != p0->num_args(); ++h) {

    const Syntax * p = p0->arg(h);

    DeclWorking w(type_scope);

    Parts::const_iterator i = p->args_begin();
    Parts::const_iterator end = p->args_end();
    
    {
      bool r = w.parse_first_part(i, end, env);
      if (!r) throw error(p, "Expected declaration.");
    }
    
    w.make_inner_type(p);
    
    while (i != end) {

      const Syntax * id;
      const Syntax * t = w.parse_outer_type_info(id, i, end, w.inner_type, env);
      
      //const Syntax * decl = w.make_declaration(id, t);
      // FIXME: Duplicate code from parse_decl, also...
      const Syntax * decl;
      if (i != end && (*i)->is_a("sym", "=")) {
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
      
      res->add_part(decl);
      
      if (i == end) break;
      
      if (!(*i)->is_a("sym", ","))
        throw error(*i, "Expected \",\".");
      ++i;
    }
  }
  return res;
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
    inner_type = new Syntax(p->part(0), name);
    if (body || by_itself) {
      Syntax * enum_ = new Syntax(p->part(0));
      enum_->add_part(name);
      if (body)
        enum_->add_part(body);
      type_scope.push_back(enum_);
    }
    return true;
  } else {
    return false;
  }
}

void DeclWorking::make_inner_type(const Syntax * orig) {
  if (!inner_type) {
    inner_type = new Syntax();
    SymbolName tn;
    StringBuf t;
    switch (base_type) {
    case NO_BT:
      if (!type_name.empty()) {
        if (size != NO_SIZE) ignore();
        if (sign != NO_SIGN) ignore();
        tn = type_name;
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
        t = "unsigned char";
        break;
      case SIGNED:
        t = "signed char";
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
        t = "long long";
        break;
      }
      switch (sign) {
      case NO_SIGN:
      case SIGNED:
        break;
      case UNSIGNED:
        t.prepend("unsigned ");
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
        t = "long double";
        break;
      default:
        ignore();
        t = "double";
        break;
      }
      break;
    }
    if (!t.empty()) tn = t.freeze();
    assert(!tn.empty());
    inner_type->add_part(new Syntax(tn));
  } else {
    // stuct or union
    // nothing to do
  }
  inner_type->set_flags(qualifiers);
}

// returns a type and sets id
const Syntax * DeclWorking::parse_outer_type_info(const Syntax * & id, 
                                                 Parts::const_iterator & i, 
                                                 Parts::const_iterator end,
                                                 const Syntax * t,
                                                 Environ & env,
                                                 bool id_required) 
{
  assert(t);
  t = try_pointers(i, end, t);
  if (i == end) return t;

  const Syntax * outer = NULL;
  
  if ((*i)->is_a("id")) {
    id = (*i)->arg(0);
    ++i;
  } else if ((*i)->is_a("()")) {
    outer = reparse("TOKENS", (*i)->arg(0));
    ++i;
  } else if (id_required) {
    throw error(*i, "Expected identifier or \"(\".");
  }

  if (i == end) {
    // do nothing
  } else if ((*i)->is_a("()")) {
    t = make_function_type(t, reparse("TOKENS", (*i)->arg(0)), env);
    ++i;
  } else {
    // we are an array of type t
    t = try_arrays(i, end, t);
  }
  
  if (outer) {
    Parts::const_iterator j = outer->args_begin();
    t = parse_outer_type_info(id, j, outer->args_end(), t, env, id_required);
    if (j != outer->d->parts.end()) throw error(*j, "Expected \")\".");
    return t;
  } else {
    return t;
  }
}

const Syntax * DeclWorking::make_function_type(const Syntax * ret,
                                              const Syntax * parms,
                                              Environ & env)
{
  Syntax * ps = new Syntax(new Syntax(".tuple"));
  Parts::const_iterator i = parms->args_begin();
  Parts::const_iterator end = parms->args_end();
  if (i != end) for (;;) {
    Parts::const_iterator begin = i;
    DeclWorking w(type_scope);
    const Syntax * id = NULL;
    bool r = w.parse_first_part(i, end, env, false);
    if (!r) throw error(*i, "Expected type or \"...\".");
    if (w.dots) {
      ps->add_part(w.inner_type); // FIXME: Preserve source info..
    } else {
      w.make_inner_type(parms);
      const Syntax * t = w.parse_outer_type_info(id, i, end, w.inner_type, env, false);
      Syntax * p = new Syntax();
      assert(t);
      p->add_part(t);
      if (id)
	p->add_part(id);
      ps->add_part(p);
      //ps->add_part(t);
    } 
    if (i == end) break;
    if (!(*i)->is_a("sym", ",")) throw error(*i, "Expected \",\".");
    ++i;
  }
  return new Syntax(new Syntax(".fun"), ps, ret);
}

const Syntax * DeclWorking::parse_init_exp(Parts::const_iterator & i, 
                                            Parts::const_iterator end)
{
  Parts::const_iterator begin = i;
  while (i != end && !(*i)->is_a("sym", ","))
    ++i;
  Syntax * p = new Syntax(new Syntax("exp"));
  p->d->parts.insert(p->d->parts.end(), begin, i);
  return p;
}

const Syntax * DeclWorking::try_pointers(Parts::const_iterator & i, 
                                        Parts::const_iterator end,
                                        const Syntax * t) 
{
  while (i != end && (*i)->is_a("sym", "*")) {
    ++i;
    Syntax * new_t = new Syntax(new Syntax(".pointer"), t);
    while (i != end && 
           (*i)->is_a("id") && try_qualifier((*i)->arg(0), new_t->d->flags))
      ++i;
    t = new_t;
  }
  return t;
}


const Syntax * DeclWorking::try_arrays(Parts::const_iterator & i, 
                                      Parts::const_iterator end,
                                      const Syntax * t) 
{
  Vector<const Syntax *> stack;
  while (i != end && (*i)->is_a("[]")) {
    stack.push_back(reparse("ARRAY_SIZE", (*i)->arg(0)));
    ++i;
  }
  Vector<const Syntax *>::const_reverse_iterator j = stack.rbegin();
  Vector<const Syntax *>::const_reverse_iterator e = stack.rend();
  while (j != e) {
    t = new Syntax(new Syntax(".array"), t, *j);
    ++j;
  }
  return t;
}

const Syntax *  DeclWorking::make_declaration(const Syntax * id, const Syntax * t, const Syntax * init)
{
  if (what == VAR) {
    if (t->is_a(".fun")) {
      return make_function(id, t, NULL);
    } else {
      Syntax * p = new Syntax(new Syntax("var"));
      p->add_part(id);
      assert(t);
      p->add_part(t);
      if (init)
        p->add_part(init);
      if (storage_class)
        p->add_flag(storage_class);
      for (Attributes::const_iterator i = attributes.begin(), e =  attributes.end();
           i != e; ++i)
        p->add_flag(*i);
      return p;
    }
  } else if (what == TYPEDEF) {
    Syntax * p = new Syntax(new Syntax("talias"));
    p->add_part(id);
    p->add_part(t);
    return p;
  } else {
    abort();
  }
}

const Syntax * DeclWorking::make_function(const Syntax * id, const Syntax * t, const Syntax * body)
{
  if (what == VAR) {
    Syntax * p = new Syntax(new Syntax("fun"));
    if (inline_)
      p->add_flag(inline_);
    if (virtual_)
      p->add_flag(virtual_);
    if (storage_class)
      p->add_flag(storage_class);
    for (Attributes::const_iterator i = attributes.begin(), e =  attributes.end();
         i != e; ++i)
      p->add_flag(*i);
    p->add_part(id);
    p->add_part(t->arg(0));
    p->add_part(t->arg(1));
    if (body)
      p->add_part(body);
    return p;
  } else {
    abort();
  }
}
