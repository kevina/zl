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

using namespace ast;

void assert_pos(const Syntax * p, Position have, unsigned need);
void assert_num_args(const Syntax * p, unsigned num);
void assert_num_args(const Syntax * p, unsigned min, unsigned max);

void compile_for_ct(Deps & deps, Environ & env);

// the "C" version of the callback functions

char MACRO_PRELUDE_STR[] = 
  "typedef typedef struct _IO_FILE FILE;\n"
  "int printf (const char *, ...);"
  "typedef struct Match Match;\n"
  "typedef struct Syntax Syntax;\n"
  "typedef struct Mark Mark;\n"
  "typedef struct Context Context;\n"
  "typedef struct Environ Environ;\n"
  "typedef struct EnvironSnapshot EnvironSnapshot;\n"
  "__ct_callback Match * match(Match *, Syntax * pattern, Syntax * with);\n"
  "__ct_callback Match * match_args(Match *, Syntax * pattern, Syntax * with);\n"
  "__ct_callback Mark * new_mark_f(EnvironSnapshot *);\n"
  "map new_mark() {new_mark_f(environ_snapshot());}\n"
  "map new_empty_mark() {new_mark_f(0);}\n"
  "__ct_callback Syntax * replace(Syntax *, Match *, Mark *);\n"
  "__ct_callback Context * get_context(Syntax *);\n"
  "__ct_callback Syntax * replace_context(Syntax *, Context *);\n";
const char * MACRO_PRELUDE = MACRO_PRELUDE_STR;
const char * MACRO_PRELUDE_END = MACRO_PRELUDE_STR + sizeof(MACRO_PRELUDE_STR) - 1;

// the slightly diffrent C++ version
typedef ReplTable::Table Match;
typedef Marks Context;
extern "C" Match * match(Match * m, const Syntax * pattern, const Syntax * with);
extern "C" Match * match_args(Match * m, const Syntax * pattern, const Syntax * with);
extern "C" Mark * new_mark_f(SymbolNode *);
extern "C" const Syntax * replace(const Syntax * p, Match * match, Mark * mark);
extern "C" const Context * get_context(const Syntax * p);
extern "C" const Syntax * replace_context(const Syntax * p, const Context * context);

String gen_sym() {
  static unsigned uniq_num = 0;
  StringBuf buf;
  buf.printf("_m_%d_", uniq_num++);
  return buf.freeze();
}

const Syntax * replace(const Syntax * p, ReplTable * r);

struct MacroSymbol : public Symbol {
  virtual const Syntax * expand(const Syntax *, Environ & env) const = 0;
};

struct Map : public MacroSymbol {
  SourceEntity entity;
  const Syntax * parse;
  const Syntax * parms;
  const Syntax * free;
  const Syntax * repl;
  const SymbolNode * env;
  Map * parse_self(const Syntax * p, Environ & e) {
    //printf("PARSING MAP %s\n", ~p->arg(0)->name);
    //p->print();
    //printf("\n");
    env = e.symbols.front;
    entity = SourceEntity(p);
    parse = p;
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
      res->str_.source = &entity;
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
    return replace(repl, m, new Mark(env));
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

void match_parm(Match * m, const Syntax * p, const Syntax * repl) {
  if (p->num_args() > 0) {
    if (p->is_a("pattern")) {
      p = p->arg(0);
      assert(p->what() == repl->what());
      for (int j = 0; j != p->num_args(); ++j) {
        m->push_back(Match::value_type(*p->arg(j), repl->arg(j)));
      }
    } else if (p->is_a("reparse")) {
      if (!repl) {
        if (p->num_args() > 1)
          repl = p->arg(1);
        else
          abort();
      }
      p = p->arg(0);
    } else {
      p->print();
      printf("\n");
      abort();
    }
  }
  m->push_back(Match::value_type(*p, repl));
}

Match * match(Match * orig_m, const Syntax * pattern, const Syntax * with, unsigned shift) {
  Match * m = new Match();
  if (pattern->is_a("()"))
    pattern = reparse("MATCH_LIST", pattern->arg(0));
  //printf("MATCH\n");
  //pattern->print(); printf("\n");
  //with->print(); printf("\n");
  //printf("---\n");
  if (pattern->simple()) {
    if (pattern->what().defined())
      match_parm(m, pattern, with->part(shift));
  } else {
    const Parts & parts = pattern->d->parts;
    const Flags & flags = pattern->d->flags;
    for (unsigned i = 0, sz = parts.size(); i < sz; ++i) {
      unsigned ii = i + shift;
      match_parm(m, parts[i], with->num_parts() > ii ? with->part(ii) : NULL);
    }
    for (Flags::const_iterator i = flags.begin(), e = flags.end(); i != e; ++i) {
      const Syntax * w = with->flag((*i)->what());
      match_parm(m, (*i)->arg(0), w ? w->arg(0) : NULL);
    }
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

Mark * new_mark_f(SymbolNode * e) {
  return new Mark(e);
}

const Syntax * replace(const Syntax * p, Match * match, Mark * mark) {
  ReplTable * rparms = new ReplTable;
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
  //printf("REPARSE AS %s\n", ~what);
  const Replacements * repls = combine_repl(p->repl, r);
  const Syntax * res = parse_str(what, p->str(), repls);
  //printf("PARSED STRING\n");
  //res->print();
  //if (repls) repls->print();
  //printf("\n");
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
  return res;
}

const Syntax * replace(const Syntax * p, ReplTable * r) {
  if (p->simple()) {
    //return p;
    //printf("MARK %s %p\n", ~p->what(), r->mark);
    return new Syntax(p, r->mark);
  } else if (p->is_a("mid") && r->have(*p->arg(0))) {
    const Syntax * p0 = r->lookup(*p->arg(0));
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
    return res;
  } else {
    Syntax * res = new Syntax(p->str());
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      //const Syntax * q = (i == 0 && p->part(0)->simple()) ? p->part(0) : replace(p->part(i), r); // HACK
      const Syntax * q = replace(p->part(i), r);
      if (q->is_a("...")) {
        for (unsigned j = 0; j != q->num_args(); ++j)
          res->add_part(q->arg(j));
      } else {
        res->add_part(q);
      }
    }
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
  Syntax * tmp = new Syntax("exp");
  for (unsigned i = 0; i != p->num_args(); ++i) {
    const Syntax * t = p->arg(i);
    if (t->is_a("()")) t = handle_paran(t, env);
    tmp->add_part(t);
  }
  const Syntax * res = parse_exp_->parse(tmp);
  return res;
}

const Syntax * partly_expand(const Syntax * p, Position pos, Environ & env) {
  if (p->entity()) return p;
  SymbolName what = p->what().name;
  //printf("\n>expand>%s//\n", ~what);
  //p->print();
  //printf("\n////\n");
  if (p->simple()) {
    abort(); // FIXME: Error Message
  } else if (what == "{}") {
    return partly_expand(reparse("BLOCK", p), pos, env);
  } else if (what == "()") {
    return partly_expand(reparse("PARAN_EXP", p), pos, env);
  } else if (what == "[]") {
    return partly_expand(reparse("EXP", p->arg(0)), pos, env);
  } else if (what == "parm") {
    return partly_expand(reparse("EXP", p), pos, env);
  } else if (env.symbols.exists(SymbolKey(what, SYNTAX_NS))) { // syntax macros
    p = env.symbols.lookup<MacroSymbol>(SymbolKey(what, SYNTAX_NS), p->str())->expand(p, env);
    return partly_expand(p, pos, env);
  } else if (what == "call") { 
    assert_num_args(p, 2);
    const Syntax * n = partly_expand(p->arg(0), OtherPos, env);
    const Syntax * a = p->arg(1);
    if (a->is_a("()")) a = reparse("SPLIT", p->arg(1)->arg(0));
    if (n && n->is_a("id")) {
      const MacroSymbol * m = NULL;
      SymbolName sn = *n->arg(0);
      m = env.symbols.find<MacroSymbol>(sn);
      if (m) { // function macros
        //  (call (id fun) (list parm1 parm2 ...))?
        p = m->expand(a, env);
        return partly_expand(p, pos, env);
      } else if (sn == "environ_snapshot") {
        if (a->num_args() > 0)
          throw error(a, "%s does not take any paramaters", ~sn.name);
        return new Syntax(n->arg(0));
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
    return partly_expand(res, pos, env);
  } else if (what == "exp") {
    assert_pos(p, pos, ExpPos);
    p = e_parse_exp(p, env);
    return partly_expand(p, pos, env);
  }
  // we should have a primitive
  return p;
}

SymbolName expand_binding(const Syntax * p, unsigned ns, Environ & env) {
  if (p->simple()) {
    return *p;
  } else if (p->is_a("fluid")) {
    assert_num_args(p, 1);
    const FluidBinding * b = env.symbols.lookup<FluidBinding>(p->arg(0), ns);
    return b->rebind;
  } else {
    p->print();
    printf("\n");
    abort();
  }
}

AST * parse_map(const Syntax * p, Environ & env) {
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
  
  printf("COMPILE FOR CT: zlfct%03d\n", cntr);
  StringBuf buf;
  buf.printf("./zlfct%03d.c", cntr);
  String source = buf.freeze();
  buf.printf("./zlfct%03d.so", cntr);
  String lib = buf.freeze();
  buf.printf("gcc -g -shared -fpic -o zlfct%03d.so zlfct%03d.c", cntr, cntr);
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


AST * parse_fluid_binding(unsigned ns, const Syntax * p, Environ & env) {
  assert_num_args(p, 1);
  SymbolName n = *p->arg(0);
  FluidBinding * b = new FluidBinding(n.name, mark(n, new Mark(NULL)));
  env.add(SymbolKey(n, ns), b);
  return new Empty();
}


