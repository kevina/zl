#include <set>

#include <stdio.h>
#include <dlfcn.h>

#include "peg.hpp"
#include "expand.hpp"
#include "ast.hpp"
#include "parse_op.hpp"
#include "parse_decl.hpp"

// 
//   EXP
//   DECL
//   STMT

//   TYPE


// (top DECL+)

// DECL:
//   (var ID TYPE EXP)
//   (talias alias TYPE)
//   (fun ID RET PARMS [BODY])
//   (struct ID [BODY])
//   (union ID [BODY])


//
// Annon
//

struct MacroSymbol;

struct ReparseAnnon : public Annon {
  const Syntax * str;
  String what;
  ReparseAnnon(const Syntax * s, String w)
    : str(s), what(w) {}
  void to_string(OStream & o) const;
};

struct ExpandAnnon : public Annon {
  const MacroSymbol * macro;
  const Syntax * call_site;
  ExpandAnnon(const MacroSymbol * m, const Syntax * c) 
    : macro(m), call_site(c) {}
  void to_string(OStream & o) const;
};

struct ReplaceAnnon : public Annon {
  const Syntax * call_site_mid;
  const Syntax * repl;
  ReplaceAnnon(const Syntax * m, const Syntax * r)
    : call_site_mid(m), repl(r) {}
  void to_string(OStream & o) const;
};

//
//
//

using namespace ast;

void assert_pos(const Syntax * p, Position have, unsigned need);

void compile_for_ct(Deps & deps, Environ & env);

// see prelude.zlh

// the slightly diffrent C++ version
typedef ReplTable::Table Match;
typedef Marks Context;
typedef Syntax SyntaxList;
typedef Syntax UnmarkedSyntax;
struct SyntaxEnum;
extern "C" Mark * new_mark_f(SymbolNode *);
extern "C" const Syntax * syntax_flag(const Syntax *, UnmarkedSyntax *);
extern "C" SyntaxList * new_syntax_list();
extern "C" int syntax_list_empty(const SyntaxList *);
extern "C" void syntax_list_append(SyntaxList *, const Syntax *);
extern "C" const Syntax * syntax_enum_next(SyntaxEnum *);
extern "C" Match * match(Match * m, const UnmarkedSyntax * pattern, const Syntax * with);
extern "C" Match * match_args(Match * m, const UnmarkedSyntax * pattern, const Syntax * with);
extern "C" Match * match_local(Match *, ...);
extern "C" const Syntax * match_var(Match *, UnmarkedSyntax *);
extern "C" SyntaxEnum * match_varl(Match *, UnmarkedSyntax *);
extern "C" const Syntax * replace(const UnmarkedSyntax * p, Match * match, Mark * mark);
extern "C" const Context * get_context(const Syntax * p);
extern "C" const Syntax * replace_context(const Syntax * p, const Context * context);
extern "C" const Syntax * partly_expand(const Syntax *, Position pos, Environ *);
extern "C" const Syntax * reparse(const Syntax *, const char *, Environ *);
extern "C" const SyntaxEnum * partly_expand_list(SyntaxEnum *, Position pos, Environ *);
extern "C" const UnmarkedSyntax * string_to_syntax(const char *);
extern "C" const char * syntax_to_string(const UnmarkedSyntax *);
extern "C" void dump_syntax(const UnmarkedSyntax * s);
typedef UserType UserTypeInfo;
typedef Module ModuleInfo;
extern "C" const UserTypeInfo * user_type_info(const Syntax *, Environ *);
extern "C" const ModuleInfo * user_type_module(const UserTypeInfo *);
extern "C" const ModuleInfo * module_info(const Syntax *, Environ *);
extern "C" SyntaxEnum * module_symbols(const ModuleInfo *);
extern "C" bool module_have_symbol(const ModuleInfo *, const Syntax *);

String gen_sym() {
  static unsigned uniq_num = 0;
  StringBuf buf;
  buf.printf("_m_%d_", uniq_num++);
  return buf.freeze();
}

const Syntax * replace(const Syntax * p, ReplTable * r);

struct MacroSymbol : public Symbol {
  const Syntax * def;
  virtual const Syntax * expand(const Syntax *, Environ & env) const = 0;
  const Syntax * expand(const Syntax * s, const Syntax * p, Environ & env) const {
    return new Syntax(new ExpandAnnon(this, s), expand(p, env));
  }
};

struct Map : public MacroSymbol {
  const SourceFile * entity;
  const Syntax * parse;
  const Syntax * parms;
  const Syntax * free;
  const Syntax * repl;
  const SymbolNode * env;
  Map * parse_self(const Syntax * p, Environ & e) {
    //printf("PARSING MAP %s\n%s\n", ~p->arg(0)->what(), ~p->to_string());
    env = e.symbols.front;
    entity = p->str().source;
    def = parse = p;
    assert_num_args(p, 4);
    name = expand_binding(p->arg(0), e);
    parms = p->arg(1);
    free = p->arg(2);
    repl = p->arg(3);
    repl = change_src(repl->str(), repl);
    return this;
  }
  // FIXME: Figure out what this does and document it
  const Syntax * change_src(SourceStr outer_str, const Syntax * orig) {
    Syntax * res = new Syntax(orig->what(), orig->str());
    res->repl = orig->repl;
    SourceStr orig_str = orig->str();
    if (orig_str.source == outer_str.source && 
        outer_str.begin <= orig_str.begin && orig_str.end <= outer_str.end)
      res->str_.source = entity;
    if (orig->d) {
      res->d = new Syntax::D;
      res->d->parts.reserve(orig->d->parts.size());
      for (Parts::const_iterator i = orig->d->parts.begin(), e = orig->d->parts.end();
           i != e;
           ++i)
        res->d->parts.push_back(change_src(outer_str, *i));
      for (Flags::const_iterator i = orig->d->flags.begin(), e = orig->d->flags.end();
           i != e;
           ++i)
        res->d->flags.insert(change_src(outer_str, *i));
    }
    return res;
  }
  const Syntax * expand(const Syntax * p, Environ &) const {
    Match * m = match_args(NULL, parms, p);
    m = match(m, free, replace_context(free, get_context(p)));
    const Syntax * res = replace(repl, m, new Mark(env));
    return res;
  }
};

struct Macro : public MacroSymbol {
  const Syntax * parse;
  const VarSymbol * fun;
  typedef const Syntax * (*MacroCall)(const Syntax *, Environ * env);
  Macro * parse_self(const Syntax * p, Environ & e) {
    //printf("PARSING MAP %s\n", ~p->arg(0)->name);
    //p->print();
    //printf("\n");
    parse = p;
    assert_num_args(p, 1, 2);
    name = expand_binding(p->arg(0), e);
    fun = e.symbols.lookup<VarSymbol>(p->num_args() == 1 ? p->arg(0) : p->arg(1));
    def = static_cast<const TopLevelVarSymbol *>(fun)->decl->parse_;
    return this;
  }
  const Syntax * expand(const Syntax * p, Environ & env) const {
    if (!fun->ct_ptr) {
      Deps deps;
      deps.push_back(static_cast<const TopLevelVarSymbol *>(fun));
      compile_for_ct(deps, env);
      assert(fun->ct_ptr);
    }
    const Syntax * res = ((MacroCall)fun->ct_ptr)(p, &env);
    //printf("MACRO EXP RES\n");
    //res->print();
    //printf("\n");
    return res;
  }
};

//
// Annon Definations
//

void ReparseAnnon::to_string(OStream & o) const {
  o << "in ";
  str->str().sample_w_loc(o);
  o << " parsed as " << what;
}

void ExpandAnnon::to_string(OStream & o) const {
  o << "in call ";
  call_site->str().sample_w_loc(o);
  o << " of macro " << macro->name;
  if (macro->def)
    macro->def->str().pos_str(" at ", o, "");
}

void ReplaceAnnon::to_string(OStream & o) const {
  o << "when replacing ";
  call_site_mid->str().sample_w_loc(o);
  o << "  with ";
}

//
//
//

Mark * new_mark_f(SymbolNode * e) {
  return new Mark(e);
}

const Syntax * syntax_flag(const Syntax * s, UnmarkedSyntax * n) {
  return s->flag(*n);
}

SyntaxList * new_syntax_list() {
  return new Syntax(new Syntax("@"));
}

int syntax_list_empty(const SyntaxList * p) {
  return p->num_args() == 0;
}

void syntax_list_append(SyntaxList * l, const Syntax * p) {
  l->add_part(p);
}

struct SyntaxEnum {
  virtual const Syntax * next() = 0;
  virtual ~SyntaxEnum() {}
};

struct SimpleSyntaxEnum : public SyntaxEnum {
  Parts::const_iterator i;
  Parts::const_iterator end;
  SimpleSyntaxEnum(Parts::const_iterator i0, Parts::const_iterator e0)
    : i(i0), end(e0) {}
  const Syntax * next() {
    if (i == end) return NULL;
    const Syntax * res = *i;
    ++i;
    return res;
  }
};

const Syntax * syntax_enum_next(SyntaxEnum * e) {
  return e->next();
}

//
//
//

void add_match_var(Match * m, const SymbolName & n, const Syntax * repl) {
  if (n == "_") return; // "_" is a special variable meaning: don't care
  m->push_back(Match::value_type(n, repl));
}

bool match_list(Match * m, 
                Parts::const_iterator p_i, Parts::const_iterator p_end,
                Parts::const_iterator r_i, Parts::const_iterator r_end);

// if match_prep sets p == 0 then there is nothing more to do
bool match_prep(Match * m, const Syntax * & p, const Syntax * & repl) {
  //printf("match_parm <<: %s %s\n", ~p->to_string(), repl ? ~repl->to_string() : "<null>");
  if (p->num_args() > 0) {
    if (p->is_a("pattern")) {
      p = p->arg(0);
      assert(p->what() == repl->what());
      bool res = match_list(m, p->args_begin(), p->args_end(), repl->args_begin(), repl->args_end());
      p = 0;
      return res;
    } else if (p->is_a("reparse")) {
      if (!repl) {
        if (p->num_args() > 1)
          repl = p->arg(1);
      }
      p = p->arg(0);
    } else {
      p->print();
      //printf("\n");
      abort();
    }
  }
  return true;
}

bool match_parm(Match * m, const Syntax * p, const Syntax * repl) {
  //printf("match_parm <<: %s %s\n", ~p->to_string(), repl ? ~repl->to_string() : "<null>");
  bool cont = match_prep(m, p, repl);
  if (!cont) return true;
  if (repl) {
    add_match_var(m, *p, repl);
    return true;
  } else {
    return false;
  }
}

bool match_list(Match * m, 
                Parts::const_iterator p_i, Parts::const_iterator p_end,
                Parts::const_iterator r_i, Parts::const_iterator r_end)
{
  bool now_optional = false;
  while (p_i != p_end) {
    const Syntax * p = *p_i;
    const Syntax * r = r_i < r_end ? *r_i : NULL;
    bool ok = match_prep(m, p, r);
    if (!ok) return false;
    if (p) {
      SymbolName v = *p;
      if (v == "@") {
        now_optional = true;
        ++p_i; 
        continue;
      }
      if (v.name[0] == '@') {
        v.name = v.name.c_str() + 1;
        r = new Syntax(new Syntax("@"), r_i, r_end);
      }
      if (r) {
        add_match_var(m, v, r);
      } else {
        if (!now_optional) return false;
      }
    }
    ++p_i;
    ++r_i;
  }
  return true;
}

Match * match(Match * orig_m, const Syntax * pattern, const Syntax * with, unsigned shift) {
  Match * m = new Match();
  if (pattern->is_a("()"))
    pattern = reparse("MATCH_LIST", pattern->arg(0));
  //printf("MATCH\n");
  //pattern->print(); printf("\n");
  //with->print(); printf("\n");
  //printf("---\n");
  pattern = pattern->ensure_branch();
  with    = with->ensure_branch();
  const Flags & flags = pattern->d->flags;
  bool ok = match_list(m, pattern->parts_begin(), pattern->parts_end(), 
                        with->parts_begin() + shift, with->parts_end());
  if (!ok) return NULL;
  for (Flags::const_iterator i = flags.begin(), e = flags.end(); i != e; ++i) {
    const Syntax * w = with->flag((*i)->what());
    match_parm(m, (*i)->arg(0), w ? w->arg(0) : NULL);
  }
  if (orig_m) 
    m->insert(m->end(), orig_m->begin(), orig_m->end());
  return m;
}

Match * match(Match * m, const Syntax * pattern, const Syntax * with) {
  return match(m, pattern, with, 0);
}

Match * match_args(Match * m, const Syntax * pattern, const Syntax * with) {
  return match(m, pattern, with, 1);
}

const Syntax * match_var(Match * m, UnmarkedSyntax * n) {
  Match::const_iterator i = m->begin(), e = m->end();
  for (; i != e; ++i)
    if (i->first == *n) return i->second;
  return NULL;
}

SyntaxEnum * match_varl(Match * m, UnmarkedSyntax * n) {
  const Syntax * p = match_var(m, n);
  if (p) {
    assert(p->is_a("@"));
    return new SimpleSyntaxEnum(p->args_begin(), p->args_end());
  } else {
    return NULL;
  }
}

Match * match_local(Match * orig_m, ...) {
  Match * m = new Match;
  va_list parms;
  const Syntax * p;
  char buf[8];
  unsigned i = 1;
  va_start(parms, orig_m);
  while ((p = va_arg(parms, const Syntax *))) {
    snprintf(buf, 8, "$%d", i);
    ++i;
    add_match_var(m, buf, p);
  }
  va_end(parms);
  if (orig_m) 
    m->insert(m->end(), orig_m->begin(), orig_m->end());
  return m;
}

const Syntax * replace(const Syntax * p, Match * match, Mark * mark) {
  ReplTable * rparms = new ReplTable;
  if (match)
    rparms->table = *match;
  rparms->mark = mark;
  const Syntax * res;
  if (p->is_a("{}")) {
    res = reparse("STMTS", p->arg(0), rparms);
    if (res->num_args() == 1)
      res = res->arg(0);
  } else {
    res = replace(p, rparms);
  }
  res->str_ = p->str();
  return res;
}

const Syntax * reparse(String what, const Syntax * p, ReplTable * r) {
  //printf("REPARSE %s AS %s\n", ~p->to_string(), ~what);
  p = new Syntax(new ReparseAnnon(p, what), p);
  const Replacements * repls = combine_repl(p->repl, r);
  const Syntax * res;  
  try {
    res = parse_str(what, p->str(), repls);
  } catch (Error * err) {
    //Annon * annon = new ReparseAnnon(p, what);
    //annon->prev = p->str().annon; // FIXME: Is this right
    //err->annon = annon;
    //puts(~err->message());
    throw err;
  }
  //res = new Syntax(new ReparseAnnon(p, what), res);
  //printf("PARSED STRING %s\n", ~res->to_string());
  if (repls) {
    for (Replacements::const_iterator i = repls->begin(), e = repls->end(); 
         i != e; ++i) 
    {
      //printf("REPLACE %i\n", i - repls->begin());
      //res->print();
      //printf("\n");
      res = replace(res, *i);
      //printf("replace %i\n", i - repls->begin());
      //res->print();
      //printf("\n");
    }
  }
  //printf("REPARSE %s RES: %s\n", ~p->to_string(), ~res->to_string());
  return res;
}

const Syntax * reparse(const Syntax * s, const char * what, Environ * env) {
  return reparse(what, s);
}


const Syntax * replace(const Syntax * p, ReplTable * r) {
  // FIXME: Do I need to handle the case where the entity is a symbol name?
  static unsigned seq=0;
  unsigned seql = seq++;
  //printf("REPLACE %d: %s\n", seql, ~p->to_string());
  if (p->simple()) {
    //return p;
    //printf("MARK %s\n", ~p->what());
    return new Syntax(p, r->mark);
  } else if (p->is_a("mid") && r->have(*p->arg(0))) {
    const Syntax * p0 = r->lookup(*p->arg(0));
    p0 = new Syntax(new ReplaceAnnon(p, p0), p0);
    if (p->num_args() > 1) {
      String what = p->arg(1)->as_symbol_name().name;
      if (what == "TOKEN" || what == "EXP" || what == "STMT")
        what = "PARM";
      if (p0->simple() && !p0->str().empty()) {
        // if p0 has marks, they must be preserved
        p0 = replace_context(reparse(what, p0), get_context(p0));
      } else if (p0->is_a("parm")) {
        p0 = reparse(what, p0->arg(0));
      }
    }
    //printf("REPLACE RES %d: %s\n", seql, ~p0->to_string());
    return p0;
  } else if (p->is_a("string") || p->is_a("char") || p->is_a("literal") || p->is_a("float") || p->is_a("sym")) {
    return p;
  } else if (p->is_a("{}") || p->is_a("()") || p->is_a("[]") || p->is_a("parm")) {
    // raw tokens
    assert(p->num_args() == 1);
    assert(p->arg(0)->simple());
    assert(p->repl == p->arg(0)->repl);
    Syntax * res = new Syntax(p->str(), new Syntax(p->part(0), r->mark));
    res->repl = combine_repl(p->repl, r);
    Syntax * r0 = new Syntax(String(p->arg(0)->str()), p->arg(0)->str());
    r0->repl = res->repl;
    res->add_part(r0);
    //printf("REPLACE RES %d: %s\n", seql, ~res->to_string());
    return res;
  } else {
    Syntax * res = new Syntax(p->str());
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      //const Syntax * q = (i == 0 && p->part(0)->simple()) ? p->part(0) : replace(p->part(i), r); // HACK
      const Syntax * q = replace(p->part(i), r);
      if (!q->simple() && q->is_a("@")) {
        for (unsigned j = 0; j != q->num_args(); ++j)
          res->add_part(q->arg(j));
      } else {
        res->add_part(q);
      }
    }
    //printf("REPLACE RES %d: %s\n", seql, ~res->to_string());
    return res;
  }
}

const Context * get_context(const Syntax * p) {
  return p->what().marks;
}

const Syntax * replace_context(const Syntax * p, const Context * context) {
  if (p->simple()) {
    return new Syntax(p, context);
  } else if (p->is_a("string") || p->is_a("char") || p->is_a("literal") || p->is_a("float") || p->is_a("sym")) {
    return p;
  } else if (p->is_a("{}") || p->is_a("()") || p->is_a("[]") || p->is_a("parm")) {
    // raw tokens
    fprintf(stderr, "Unhandled Case\n");
    abort();
  } else {
    Syntax * res = new Syntax(p->str());
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      res->add_part(replace_context(p->part(i), context));
    }
    return res;
  }
}

//
//
//

extern "C" const UnmarkedSyntax * string_to_syntax(const char * str) {
  return new Syntax(str);
}

extern "C" const char * syntax_to_string(const UnmarkedSyntax * s) {
  return ~s->to_string();
}

extern "C" void dump_syntax(const UnmarkedSyntax * s) {
  s->print();
  printf("\n");
}


// three type of macros, two namespaces
// syntax macros in there own name space
//   (m ...) or (m)
// function macros
//   (call m (list ...))
// identifier macros
//   m
// function and identifier macros in the same namespace
// NOTE: may also want macros for tags...

//static SymbolTable syntax_maps;

/*
struct BuildIn {
  const char * name;
  unsigned pos;
  unsigned min;
  unsigned max;
  Position parms[4];
}
*/

void read_macro(const Syntax * p, Environ & env) {
  parse_top_level(p, env);
}

const Syntax * ID = new Syntax("id");
const Syntax * ESTMT = new Syntax("estmt");

// should't override following primatives
//   "exp" "stmt" "estmt" and TOKENS
// FIXME: need to add check

const Syntax * handle_paran(const Syntax * p, Environ & env) {
  try {
    const Syntax * exp = reparse("PARAN_EXP", p);
    const Syntax * type = parse_decl_->parse_type(exp, env);
    if (type) return new Syntax(p->str(), new Syntax("(type)"), type);
    // Since the raw string might need to be reparsed we can't use an
    // exp here.  Unfortunately this will likely mean duplicate work.
    // Avoiding that will take more thought
    else return p;
  } catch (...) {
    return p;
  }
}

const Syntax * e_parse_exp(const Syntax * p, Environ & env) {
  Syntax * tmp = new Syntax(p->part(0));
  for (unsigned i = 0; i != p->num_args(); ++i) {
    const Syntax * t = p->arg(i);
    if (t->is_a("()")) t = handle_paran(t, env);
    tmp->add_part(t);
  }
  const Syntax * res = parse_exp_->parse(tmp);
  // FIXME: if really an expression than "list" needs to become the
  //        comma operator
  return res;
}

const Syntax * partly_expand(const Syntax * p, Position pos, Environ & env, unsigned flags) {
  if (p->entity()) return p;
  SymbolName what = p->what().name;
  //printf("\n>expand>%s//\n", ~what);
  //p->print();
  // printf("\n////\n");
  if (p->simple()) {
    fprintf(stderr, "partly_expand can't be simple: %s\n", ~p->to_string());
    //abort(); // FIXME: Error Message
    return p;
  } else if (what == "{}") {
    if (pos == ExpPos)
      return partly_expand(reparse("INIT", p->arg(0)), pos, env, flags);
    else
      return partly_expand(reparse("BLOCK", p), pos, env, flags);
  } else if (what == "()") {
    return partly_expand(reparse("PARAN_EXP", p), pos, env, flags);
  } else if (what == "[]") {
    return partly_expand(reparse("EXP", p->arg(0)), pos, env, flags);
  } else if (what == "parm") {
    //abort();
    if (pos & ExpPos)
      return partly_expand(reparse("EXP", p), pos, env, flags);
    else
      return partly_expand(reparse("STMT", p), pos, env, flags);
  } else if (env.symbols.exists(SymbolKey(what, SYNTAX_NS))) { // syntax macros
    p = env.symbols.lookup<MacroSymbol>(SymbolKey(what, SYNTAX_NS), p->str())->expand(p, p, env);
    return partly_expand(p, pos, env, flags);
  } else if (what == "id" && !(flags & EXPAND_NO_ID_MACRO_CALL)) { 
    // NOTE: ID macros can have flags, since there is no parameter list
    //       they are passed in as flags as (id <id> :(flag1 val1))
    assert_num_args(p, 1);
    const Syntax * n = p;
    // FIXME: This needs to use lookup(Syntax *), but that throws an
    // error need find(Syntax *)
    const MacroSymbol * m = env.symbols.find<MacroSymbol>(n->arg(0));
    if (m) { // id macro
      Syntax * a = new Syntax(new Syntax("list"));
      a->set_flags(p);
      p = m->expand(p, a, env);
      return partly_expand(p, pos, env, flags);
    }
  } else if (what == "call") { 
    assert_num_args(p, 2);
    const Syntax * n = partly_expand(p->arg(0), OtherPos, env, flags | EXPAND_NO_ID_MACRO_CALL);
    const Syntax * a = p->arg(1);
    if (a->is_a("()")) a = reparse("SPLIT", p->arg(1)->arg(0));
    if (n && n->is_a("id")) {
      // FIXME: This needs to use lookup(Syntax *), but that throws an
      // error need find(Syntax *)
      if (!(flags & EXPAND_NO_FUN_MACRO_CALL)) {
        const MacroSymbol * m = env.symbols.find<MacroSymbol>(n->arg(0));
        if (m) { // function macros
          //  (call (id fun) (list parm1 parm2 ...))?
          p = m->expand(p, a, env);
          return partly_expand(p, pos, env, flags);
        } else if (*n->arg(0) == "environ_snapshot") {
          if (a->num_args() > 0)
            throw error(a, "environ_snapshot does not take any paramaters");
          return new Syntax(n->arg(0));
        }
      }
    }
    Syntax * res = new Syntax(p->str(), p->part(0));
    res->add_part(p->arg(0));
    res->add_part(a);
    p = res;
  } else if (what == "stmt") {
    assert_pos(p, pos, TopLevel|FieldPos|StmtPos|StmtDeclPos);
    const Syntax * res = parse_decl_->parse_decl(p, env);
    if (!res)
      res = e_parse_exp(p, env);
    return partly_expand(res, pos, env, flags);
  } else if (what == "exp" || what == "init") {
    assert_pos(p, pos, ExpPos);
    p = e_parse_exp(p, env);
    return partly_expand(p, pos, env, flags);
  }
  // we should have a primitive
  return p;
}

const Syntax * partly_expand(const Syntax * p, Position pos, Environ * env) {
  return partly_expand(p, pos, *env);
}

struct PartlyExpandSyntaxEnum : public SyntaxEnum {
  typedef Vector<SyntaxEnum *> VD;
  VD stack;
  SyntaxEnum * top;
  Position pos;
  Environ * env;
  PartlyExpandSyntaxEnum(SyntaxEnum * e0, Position p0, Environ * env0)
    : top(e0), pos(p0), env(env0) {}
  const Syntax * next() {
  try_again:
    const Syntax * res = top->next();
    if (!res) {
      if (stack.empty()) {
        return NULL;
      } else {
        top = stack.back();
        stack.pop_back();
        goto try_again;
      }
    }
    res = partly_expand(res, pos, *env);
    if (res->is_a("@")) {
      stack.push_back(top);
      top = new SimpleSyntaxEnum(res->args_begin(), res->args_end());
      goto try_again;
    }
    return res;
  }
};

const SyntaxEnum * partly_expand_list(SyntaxEnum * l, Position pos, Environ * env) {
  return new PartlyExpandSyntaxEnum(l, pos, env);
}

SymbolKey expand_binding(const Syntax * p, const InnerNS * ns, Environ & env) {
  //printf("EB::"); p->print(); printf("\n");
  if (p->simple()) {
    return SymbolKey(*p, ns);
  } else if (p->is_a("fluid")) {
    assert_num_args(p, 1);
    const FluidBinding * b = env.symbols.lookup<FluidBinding>(p->arg(0), ns);
    return SymbolKey(b->rebind, ns);
  } else if (p->is_a("w/inner")) {
    assert_num_args(p, 2);
    const InnerNS * ns = env.symbols.lookup<InnerNS>(p->arg(1), INNER_NS);
    return expand_binding(p->arg(0), ns, env);
  } else if (p->is_a("w/outer")) {
    throw error(p, "Can not use outer namespaces in binding form");
  } else if (const SymbolKeyEntity * s = dynamic_cast<const SymbolKeyEntity *>(p->entity())) {
    return s->name;
  } else {
    printf("Unsupported Binding Form: %s\n", ~p->to_string());
    abort();
  }
}

AST * parse_map(const Syntax * p, Environ & env) {
  //printf("MAP>>%s\n", ~p->to_string());
  Map * m = new Map;
  m->parse_self(p, env);
  if (p->is_a("smap"))
    env.add(SymbolKey(m->name, SYNTAX_NS), m);
  else
    env.add(m->name, m);
  return new Empty();
}

AST * parse_macro(const Syntax * p, Environ & env) {
  Macro * m = new Macro;
  m->parse_self(p, env);
  if (p->is_a("syntax_macro"))
    env.add(SymbolKey(m->name, SYNTAX_NS), m);
  else
    env.add(m->name, m);
  return new Empty();
}

void assert_pos(const Syntax * p, Position have, unsigned need) {
  //if (!(have & need)) {
  //  p->print(); printf("\n");
  //  abort();
  //}
  // FIXME Better Error Message
  //if (!(have & need)) 
  //  throw error(p, "syntax error");
}

void assert_num_args(const Syntax * p, unsigned num) {
  if (p->num_args() != num)
    throw error(p, "Expected %d arguments but got %d for \"%s\"", num, p->num_args(), ~p->what());
}

void assert_num_args(const Syntax * p, unsigned min, unsigned max) {
  if (p->num_args() < min)
    throw error(p, "Expected at least %d arguments but got %d for \"%s\"", min, p->num_args(), ~p->what());
  if (p->num_args() > max)
    throw error(p->arg(max), "Too many arguments for \"%s\"", ~p->what());
}

Tuple * expand_fun_parms(const Syntax * parse, Environ & env) {
  //printf("FUN_PARMS: %s\n", ~parse->to_string());
  Syntax * res = new Syntax(parse->part(0));
  for (unsigned i = 0; i != parse->num_args(); ++i) {
    const Syntax * p = parse->arg(i);
    if (!p->is_a("...")) {
      Syntax * r = new Syntax(parse_type(p->part(0), env));
      if (p->num_parts() == 2) 
	r->add_part(p->part(1));
      res->add_part(r);
    } else {
      res->add_part(p);
    }
  }
  Type * type = parse_type(res, env);
  Tuple * tuple = dynamic_cast<Tuple *>(type);
  assert(tuple); // FIXME: Error Message?
  return tuple;
}



void compile_for_ct(Deps & deps, Environ & env) {

  static unsigned cntr = 0;
  
  for (unsigned i = 0, sz = deps.size(); i != sz; ++i) {
    deps.merge(deps[i]->decl->deps());
  }

  //printf("COMPILE FOR CT DEPS: %d\n", deps.size());
  //for (Deps::iterator i = deps.begin(), e = deps.end(); i != e; ++i)
  //  printf("  %s\n", ~(*i)->name);
  //printf("---\n");
  
  printf("COMPILE FOR CT: zlfct%03d\n", cntr);
  StringBuf buf;
  buf.printf("./zlfct%03d.c", cntr);
  String source = buf.freeze();
  buf.printf("./zlfct%03d.so", cntr);
  String lib = buf.freeze();
  buf.printf("gcc -g -fexceptions -shared -fpic -o zlfct%03d.so zlfct%03d.c", cntr, cntr);
  String cmd = buf.freeze();
  cntr++;
  
  CompileWriter cw;
  cw.open(source, "w");
  cw.deps = &deps;
  compile(*env.top_level_symbols, cw);
  cw.close();
  
  system(cmd);
 
  void * lh = dlopen(lib, RTLD_NOW | RTLD_GLOBAL);
  if (!lh) {
    fprintf(stderr, "ERROR: %s\n", dlerror());
    abort();
  }
  
  for (Deps::const_iterator i = deps.begin(), e = deps.end(); i != e; ++i) {
    if (!(*i)->ct_ptr) { // FIXME: I need a better test
      //printf(">>%s\n", ~(*i)->uniq_name());
      void * p = dlsym(lh, (*i)->uniq_name());
      //assert(p);
      (*i)->ct_ptr = p;
    }
  }
}


AST * parse_fluid_binding(const Syntax * p, Environ & env) {
  assert_num_args(p, 1);
  SymbolKey n = expand_binding(p->arg(0), DEFAULT_NS, env);
  FluidBinding * b = new FluidBinding(n.name, mark(n, new Mark(NULL)));
  env.add(n, b);
  return new Empty();
}


//
// 
//

const UserTypeInfo * user_type_info(const Syntax * s, Environ * env) {
  return dynamic_cast<const UserType *>(env->types.inst(s));
}

const ModuleInfo * user_type_module(const UserTypeInfo * t) {
  return t->module;
}

const ModuleInfo * module_info(const Syntax *, Environ * env) {
  abort();
}

struct ModuleSymbolsEnum : public SyntaxEnum {
  SymbolNode * cur;
  Syntax * next() {
    if (!cur) return NULL;
    Syntax * res = new Syntax(new SymbolKeyEntity(cur->key));
    cur = cur->next;
    return res;
  }
  ModuleSymbolsEnum(SymbolNode * c) : cur(c) {}
};

SyntaxEnum * module_symbols(const ModuleInfo * m) {
  return new ModuleSymbolsEnum(m->syms);
}

bool module_have_symbol(const ModuleInfo * m, const Syntax * s) {
  return find_symbol<Symbol>(s, DEFAULT_NS, m->syms);
}


