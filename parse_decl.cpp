#include "peg.hpp"
#include "parse_decl.hpp"
#include "string_buf.hpp"
#include "expand.hpp"

// the ";" is just noise at this point and is NOT expected to be included

/*
  -- declarations
  (var ID TYPE INIT), flags: 
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

//static const Parse * const TYPE = new Parse("type");

static unsigned uniq_num = 0;

struct DeclWorking {
  DeclWorking(Parts &); // FIXME

  const Parse * gen_sym() {
    StringBuf buf;
    buf.printf("_s_%d_", uniq_num++);
    return new Parse(buf.freeze());
  }

  Parts & type_scope;

  bool parse_first_part(Parts::const_iterator & i, 
                        Parts::const_iterator end, 
                        ExpandEnviron & env);
  const Parse * parse_outer_type_info(const Parse * & id, 
                                      Parts::const_iterator & i, 
                                      Parts::const_iterator end,
                                      const Parse * t,
                                      ExpandEnviron & env,
                                      bool id_required = true);
  const Parse * parse_init_exp(Parts::const_iterator & i, 
                               Parts::const_iterator end);

  const Parse * storage_class;
  enum What {VAR, TYPEDEF} what;

  bool try_storage_class(const Parse * p) {
    if (p->name == "typedef") {
      what = TYPEDEF;
    } else if (p->name == "extern" || p->name == "static" ||
               p->name == "auto"   || p->name == "register") {
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
  bool try_sign(const Parse * p) {
    if      (p->name == "unsigned") set_sign(UNSIGNED);
    else if (p->name == "signed")   set_sign(SIGNED);
    else return false;
    inner_type_str.adj(p->str());
    return true;
  }
  void set_sign(Sign v) {
    if (sign && sign != v) ignore();
    else sign = v;
  }
  enum Size {NO_SIZE, SHORT, LONG, LONG_LONG} size;
  bool try_size(const Parse * p) {
    if (p->name == "short") {
      if (size != NO_SIZE) ignore();
      size = SHORT;
    } else if (p->name == "long") {
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
  bool try_base_type(const Parse * p) {
    if      (p->name == "void")      set_base_type(VOID);
    else if (p->name == "char")      set_base_type(CHAR);
    else if (p->name == "int")       set_base_type(INT);
    else if (p->name == "float")     set_base_type(FLOAT);
    else if (p->name == "double")    set_base_type(DOUBLE);
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
  bool try_qualifier(const Parse * p, Flags & qualifiers) {
    if (p->name == "const" || p->name == "restrict" || p->name == "volatile") 
      qualifiers.insert(p);
    else return false;
    qualifiers.print();
    return true;
  }
  bool try_qualifier(const Parse * p) {return try_qualifier(p, qualifiers);}

  const Parse * inline_;
  bool try_function_specifier(const Parse * p) {
    if (p->name == "inline") inline_ = p;
    else return false;
    return true;
  }
  String type_name;
  bool try_type_name(const Parse * p, ExpandEnviron & env) {
    String name = p->name;
    if (env.symbols->exists(name) && env.symbols->lookup(name) == TypeSym) {
      if (base_type) ignore();
      type_name = name;
      return true;
    } else {
      return false;
    }
  }
  bool try_typeof(const Parse * p, ExpandEnviron &) {
    if (p->name == ".typeof") {
      inner_type = new Parse(p->part(0), p->arg(0));
      return true;
    } else {
      return false;
    }
  }
  bool try_struct_union(const Parse * p, ExpandEnviron &);
  const Parse * parse_struct_union_body(const Parse * p, ExpandEnviron &);
  Parse * inner_type;
  void make_inner_type(const Parse * orig);
  const Parse * try_pointers(Parts::const_iterator & i, 
                             Parts::const_iterator end,
                             const Parse * t);
  const Parse * try_arrays(Parts::const_iterator & i, 
                           Parts::const_iterator end,
                           const Parse * t);

  const Parse * make_declaration(const Parse * id, const Parse * t, const Parse * init = NULL);
  const Parse * make_function(const Parse * id, const Parse * t, const Parse * body);
  const Parse * make_function_type(const Parse *, const Parse *, ExpandEnviron & env);
};

class ParseDeclImpl : public ParseDecl {
public:
  ParseDeclImpl() {}
  const Parse * parse(const Parse * p, ExpandEnviron &);
  
  void init() {}
  ~ParseDeclImpl() {}
};

ParseDecl * parse_decl_ = new ParseDeclImpl();

DeclWorking::DeclWorking(Parts & p)
  : type_scope(p), storage_class(NULL), what(VAR),
    sign(NO_SIGN), size(NO_SIZE), base_type(NO_BT),
    inline_(NULL), inner_type(NULL) {}

//
// The real code....
//

const Parse * ParseDeclImpl::parse(const Parse * p, struct ExpandEnviron & env)
{
  Parse * res = new Parse(new Parse("slist"));

  Parts::const_iterator i = p->args_begin();
  Parts::const_iterator end = p->args_end();

  DeclWorking w(res->d->parts);

  {
    bool r = w.parse_first_part(i, end, env);
    if (!r) return NULL;
  }

  w.make_inner_type(p);

  while (i != end) {

    const Parse * id;
    const Parse * t = w.parse_outer_type_info(id, i, end, w.inner_type, env);

    const Parse * decl;
    if (i != end && (*i)->is_a("sym", "=")) {
      ++i;
      const Parse * init = w.parse_init_exp(i, end);
      decl = w.make_declaration(id, t, init);
    } else if (i != end && (*i)->is_a("{}")) {
      const Parse * body = *i;
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

bool DeclWorking::parse_first_part(Parts::const_iterator & i, 
                                   Parts::const_iterator end,
                                   ExpandEnviron & env) 
{
  Parts::const_iterator begin = i;
  const Parse * cur;
  while (i != end) {
    cur = *i;
    if (cur->name == "id") {
      const Parse * p = cur->arg(0);
      bool any = 
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
    } else if (try_struct_union(cur, env)) {
      ++i;
    } else {
      break;
    }
  }
  if (i == begin) return false;
  else return true;
}

bool DeclWorking::try_struct_union(const Parse * p, ExpandEnviron & env) {
  const Parse * name = NULL;
  const Parse * body = NULL;
  if (p->name == "struct" || p->name == "union") {
    unsigned i = 0;
    if (p->num_args() == 0) throw error(p->str().source, p->str().end, 
                                        "Expected indentifer or \"{\" after \"%s\".",
                                        ~p->name);
    name = p->arg(i);
    ++i;
    if (i == p->num_args()) goto finish;
    if (p->arg(i)->is_a("{...}")) {
      body = p->arg(i);
      ++i;
    }
    if (i != p->num_args()) abort(); // internal error, should't happen
  finish:
    if (name->name.empty())
      name = gen_sym();
    inner_type = new Parse(p->part(0), name);
    if (body) {
      Parse * struct_union = new Parse(p->part(0));
      struct_union->add_part(name);
      struct_union->add_part(parse_struct_union_body(body, env));
      type_scope.push_back(struct_union);
    }
    return true;
  } else {
    return false;
  }
}
const Parse * DeclWorking::parse_struct_union_body(const Parse * p0, ExpandEnviron & env)
{
  Parse * res = new Parse();
  for (unsigned h = 0; h != p0->num_args(); ++h) {

    const Parse * p = p0->arg(h);

    DeclWorking w(type_scope);

    Parts::const_iterator i = p->args_begin();
    Parts::const_iterator end = p->args_end();
    
    {
      bool r = w.parse_first_part(i, end, env);
      if (!r) throw error(p, "Expected declaration.");
    }
    
    w.make_inner_type(p);
    
    while (i != end) {

      const Parse * id;
      const Parse * t = w.parse_outer_type_info(id, i, end, w.inner_type, env);
      
      const Parse * decl = w.make_declaration(id, t);
      
      res->add_part(decl);
      
      if (i == end) break;
      
      if (!(*i)->is_a("sym", ","))
        throw error(*i, "Expected \",\".");
      ++i;
    }
  }
  return res;
}


void DeclWorking::make_inner_type(const Parse * orig) {
  if (!inner_type) {
    inner_type = new Parse();
    StringBuf t;
    switch (base_type) {
    case NO_BT:
      if (!type_name.empty()) {
        if (size != NO_SIZE) ignore();
        if (sign != NO_SIGN) ignore();
        t = type_name;
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
        break;
      case SHORT:     
        t = "short "; 
        break;
      case LONG:
        t = "long ";
        break;
      case LONG_LONG:
        t = "long long ";
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
      t += "int";
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
    assert(!t.empty());
    inner_type->add_part(new Parse(t.freeze()));
  } else {
    // stuct or union
    // nothing to do
  }
  inner_type->set_flags(qualifiers);
}

// returns a type and sets id
const Parse * DeclWorking::parse_outer_type_info(const Parse * & id, 
                                                 Parts::const_iterator & i, 
                                                 Parts::const_iterator end,
                                                 const Parse * t,
                                                 ExpandEnviron & env,
                                                 bool id_required) 
{
  assert(t);
  if (i == end) return t;
  t = try_pointers(i, end, t);

  const Parse * outer = NULL;
  
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
    printf("***************************\n");
    (*i)->print();
    printf("\n");
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

const Parse * DeclWorking::make_function_type(const Parse * ret,
                                              const Parse * parms,
                                              ExpandEnviron & env)
{
  Parse * ps = new Parse(new Parse(".tuple"));
  Parts::const_iterator i = parms->args_begin();
  Parts::const_iterator end = parms->args_end();
  if (i != end) for (;;) {
    Parts::const_iterator begin = i;
    DeclWorking w(type_scope);
    const Parse * id = NULL;
    bool r = w.parse_first_part(i, end, env);
    if (!r) throw error(*i, "Expected type.");
    w.make_inner_type(parms);
    const Parse * t = w.parse_outer_type_info(id, i, end, w.inner_type, env, false);
    Parse * p = new Parse();
    assert(t);
    p->add_part(t);
    if (id)
      p->add_part(id);
    ps->add_part(p);
    //ps->add_part(t);
    if (i == end) break;
    if (!(*i)->is_a("sym", ",")) throw error(*i, "Expected \",\".");
    ++i;
  }
  return new Parse(new Parse(".fun"), ps, ret);
}

const Parse * DeclWorking::parse_init_exp(Parts::const_iterator & i, 
                                            Parts::const_iterator end)
{
  Parts::const_iterator begin = i;
  while (i != end && !(*i)->is_a("sym", ","))
    ++i;
  Parse * p = new Parse(new Parse("exp"));
  p->d->parts.insert(p->d->parts.end(), begin, i);
  return p;
}

const Parse * DeclWorking::try_pointers(Parts::const_iterator & i, 
                                        Parts::const_iterator end,
                                        const Parse * t) 
{
  while (i != end && (*i)->is_a("sym", "*")) {
    ++i;
    Parse * new_t = new Parse(new Parse(".pointer"), t);
    while (i != end && 
           (*i)->is_a("id") && try_qualifier((*i)->arg(0), new_t->d->flags))
      ++i;
    t = new_t;
  }
  return t;
}


const Parse * DeclWorking::try_arrays(Parts::const_iterator & i, 
                                      Parts::const_iterator end,
                                      const Parse * t) 
{
  Vector<const Parse *> stack;
  while (i != end && (*i)->is_a("[]")) {
    stack.push_back((*i));
    ++i;
  }
  Vector<const Parse *>::const_reverse_iterator j = stack.rbegin();
  Vector<const Parse *>::const_reverse_iterator e = stack.rend();
  while (j != e) {
    t = new Parse(new Parse(".array"), t, *j);
    ++j;
  }
  return t;
}

const Parse *  DeclWorking::make_declaration(const Parse * id, const Parse * t, const Parse * init)
{
  if (what == VAR) {
    if (t->is_a(".fun")) {
      return make_function(id, t, NULL);
    } else {
      Parse * p = new Parse(new Parse("var"));
      p->add_part(id);
      assert(t);
      p->add_part(t);
      if (init)
        p->add_part(init);
      if (storage_class)
        p->add_flag(storage_class);
      return p;
    }
  } else if (what == TYPEDEF) {
    Parse * p = new Parse(new Parse("talias"));
    p->add_part(id);
    p->add_part(t);
    return p;
  } else {
    abort();
  }
}

const Parse * DeclWorking::make_function(const Parse * id, const Parse * t, const Parse * body)
{
  if (what == VAR) {
    Parse * p = new Parse(new Parse("fun"));
    if (inline_)
      p->add_flag(inline_);
    if (storage_class)
      p->add_flag(storage_class);
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

