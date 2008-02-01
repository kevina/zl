#include <set>

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

using namespace AST;

enum Position {NoPos = 0, OtherPos = 1, TopLevel = 2, FieldPos = 4, StmtPos = 8, ExpPos = 16};

void assert_pos(const Parse * p, Position have, unsigned need);
void assert_num_args(const Parse * p, unsigned num);
void assert_num_args(const Parse * p, unsigned min, unsigned max);

String gen_sym() {
  static unsigned uniq_num = 0;
  StringBuf buf;
  buf.printf("_m_%d_", uniq_num++);
  return buf.freeze();
}

const Parse * replace(const Parse * p, ReplTable * r);

struct Map {
  String name;
  SourceEntity entity;
  const Parse * parse;
  const Parse * parms;
  const Parse * repl;
  Map * parse_self(const Parse * p) {
    printf("PARSING MAP %s\n", ~p->arg(0)->name);
    p->print();
    entity = SourceEntity(p);
    parse = p;
    assert_num_args(p, 3);
    name = p->arg(0)->name;
    parms = p->arg(1);
    repl = p->arg(2);
    repl = change_src(repl->str(), repl);
    return this;
  }
  const Parse * change_src(SourceStr outer_str, const Parse * orig) {
    Parse * res = new Parse(orig->name, orig->str_);
    res->repl = orig->repl;
    SourceStr orig_str = orig->str();
    if (orig_str.source == outer_str.source && 
        outer_str.begin <= orig_str.begin && orig_str.end <= outer_str.end)
      res->str_.source = &entity;
    if (orig->d) {
      res->d = new Parse::D;
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
  const Parse * expand(const Parse * p, Position, ExpandEnviron &, unsigned shift = 0) const {
    printf(">>EXPAND MAP %s\n", ~name);
    ReplTable * rparms = new ReplTable;
    int i = 0;
    for (; i != parms->num_parts(); ++i) {
      if (parms->part(i)->num_args() > 0) {
        const Parse * sp = p->arg(i + shift);
        assert(parms->part(i)->name == sp->name);
        const Parse * mp = parms->part(i);
        for (int j = 0; j != mp->num_args(); ++j) {
          rparms->insert(mp->arg(j)->name, sp->arg(j));
        }
      } else if (parms->part(i)->name == "...") {
        Parse * p2 = new Parse(new Parse("..."));
        for (; i != p->num_args(); ++i) {
          p2->add_part(p->arg(i + shift));
        }
        rparms->insert("...", p2);
        break;
      }
      rparms->insert(parms->part(i)->name, p->arg(i + shift));
    }
    printf(">>TO EXPAND %s\n", ~name);
    repl->print();
    printf("\n");
    const Parse * res;
    if (repl->name == "{}") {
      res = reparse("STMTS", repl->arg(0), rparms);
      if (res->num_args() == 1)
	res = res->arg(0);
    } else {
      res = replace(repl, rparms);
    }
    res->str_ = p->str();
    printf(">>>EXPANDED %s\n", ~name);
    res->print();
    printf("\n");
    return res;
  }
};

const Parse * reparse(String what, const Parse * p, ReplTable * r) {
  Replacements * repls = combine_repl(p->repl, r);
  const Parse * res = parse_str(what, p->str(), repls);
  printf("PARSED STRING\n");
  res->print();
  if (repls) repls->print();
  printf("\n");
  if (repls) {
    for (Replacements::const_iterator i = repls->begin(), e = repls->end(); 
         i != e; ++i) 
    {
      printf("REPLACE %i\n", i - repls->begin());
      res->print();
      printf("\n");
      res = replace(res, *i);
      printf("replace %i\n", i - repls->begin());
      res->print();
      printf("\n");
    }
  }
  return res;
}

const Parse * replace(const Parse * p, ReplTable * r) {
  if (p->simple()) {
    if (p->name.size() > 2 && p->name[0] == '`' && p->name[1] == '`') {
      // strip one `
      return new Parse(~p->name + 1, p->str_, p->str_.begin + 1, p->str_.end);
    } else if (p->name.size() > 1 && p->name[0] == '`') {
      if (!r->have(p->name))
        r->insert(p->name, new Parse(gen_sym(), p->str_));
      return r->lookup(p->name);
    } else if (r->have(p->name)) {
      return r->lookup(p->name);
    } else {
      return p;
    }
  } else if (p->name == "id" && p->arg(0)->name[0] != '`' && r->have(p->arg(0)->name)) {
    return r->lookup(p->arg(0)->name); 
  } else if (p->name == "mid") {
    const Parse * p0 = r->lookup(p->arg(0)->name);
    if (p0) {
      if (p0->name == "parm") {
        String what = p->arg(1)->name;
        if (what == "TOKEN" || what == "EXP" || what == "STMT")
          what = "PARM";
        p0 = reparse(what, p0->arg(0));
      }
      return p0;
    } else {
      return p;
    }
  } else if (p->name == "string" || p->name == "char" || p->name == "literal" || p->name == "float" || p->name == "sym") {
    return p;
  } else if (p->name == "{}" || p->name == "()" || p->name == "[]" || p->name == "parm") {
    // raw tokens
    assert(p->num_args() == 1);
    assert(p->arg(0)->simple());
    assert(p->repl == p->arg(0)->repl);
    Parse * res = new Parse(p->str(), p->part(0));
    res->repl = combine_repl(p->repl, r);
    Parse * r0 = new Parse(String(p->arg(0)->str()), p->arg(0)->str());
    r0->repl = res->repl;
    res->add_part(r0);
    return res;
  } else {
    Parse * res = new Parse(p->str());
    for (unsigned i = 0; i != p->num_parts(); ++i) {
      const Parse * q = replace(p->part(i), r);
      if (q->name == "...") {
        for (unsigned j = 0; j != q->num_args(); ++j)
          res->add_part(q->arg(j));
      } else {
        res->add_part(q);
      }
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

static SymbolTable<const Map *> syntax_maps;
static SymbolTable<const Map *> maps;

/*
struct BuildIn {
  const char * name;
  unsigned pos;
  unsigned min;
  unsigned max;
  Position parms[4];
}
*/

const Parse * expand_args(const Parse * p, Position pos, ExpandEnviron & env);
const Parse * expand_parts(const Parse * p, Position pos, ExpandEnviron & env);
const Parse * expand_fun_parms(const Parse * parse, ExpandEnviron & env);
const Parse * expand_enum_body(const Parse * parse, ExpandEnviron & env);
const Parse * expand_call_parms(const Parse * parse, ExpandEnviron & env);
const Parse * expand(const Parse * p, Position pos, ExpandEnviron & env);
const Parse * expand_type(const Parse * p, ExpandEnviron & env);


const Parse * expand_top(const Parse * p) {
  ExpandEnviron env;
  assert(p->name == "top"); // FIXME Error
  return expand_args(p, TopLevel, env);
}

const Parse * read_macro(const Parse * p) {
  ExpandEnviron env;
  return expand(p, TopLevel, env);
}

const Parse * ID = new Parse("id");
const Parse * ESTMT = new Parse("estmt");

// should't override following primatives
//   "exp" "stmt" "estmt" and TOKENS
// FIXME: need to add check

const Parse * expand(const Parse * p, Position pos, ExpandEnviron & env) {
  String name = p->name;
  printf("\n>expand>%s//\n", ~name);
  p->print();
  printf("\n////\n");
  if (p->simple()) {
    abort(); // FIXME: Error Message
  } else if (name == "{}") {
    printf("RAW BRACE\n");
    p->print();
    printf("\n--------\n");
    return expand(reparse("BLOCK", p), pos, env);
  } else if (name == "()") {
    printf("RAW PARAN\n");
    p->print();
    printf("\n--------\n");
    return expand(reparse("PARAN_EXP", p), pos, env);
  } else if (name == "[]") {
    printf("RAW BRACK\n");
    p->print();
    printf("\n--------\n");
    return expand(reparse("EXP", p->arg(0)), pos, env);
  } else if (syntax_maps.exists(name)) { // syntax macros
    p = syntax_maps.lookup(p->name)->expand(p, pos, env);
    return expand(p, pos, env);
  } else if (name == "id") {
    //name = p->arg(0)->name;
    //if (maps.exists(name) && !env.symbols->exists(name)) { // identifier macros
    //  p = maps.lookup(name)->expand(p, pos, env);
    //  return expand(p, pos, env);
    //} else {
    return p;
    //}
  } else if (name == "sym" || name == "string" || name == "char" || name == "literal" || name == "float") { // simple tokens
    return p;
  } else if (name == "call") { 
    assert_num_args(p, 2);
    const Parse * n = expand(p->arg(0), OtherPos, env);
    // the parameters are just a list of tokens at this point,
    // we need to turn them into a proper list
    const Parse * a = reparse("SPLIT", p->arg(1)->arg(0));
    if (n->name == "id" && maps.exists(n->arg(0)->name) && !env.symbols->exists(n->arg(0)->name)) { // function macros
      //  (call (id fun) (list parm1 parm2 ...))?
      p = maps.lookup(n->arg(0)->name)->expand(a, pos, env);
      return expand(p, pos, env);
    } else {
      Parse * res = new Parse(p->str(), p->part(0));
      res->add_part(n);
      res->add_part(expand_call_parms(a, env));
      return res;
    }
  } else if (name == "stmt") {
    assert_pos(p, pos, TopLevel|FieldPos|StmtPos);
    const Parse * res = parse_decl_->parse_decl(p, env);
    if (!res)
      res = parse_exp_->parse(p);
    return expand(res, pos, env);
  } else if (name == "exp") {
    assert_pos(p, pos, ExpPos);
    p = parse_exp_->parse(p);
    return expand(p, pos, env);
  } else if (name == "slist") {
    return expand_args(p, pos, env);
  } else if (name == "block") {
    assert_pos(p, pos, StmtPos);
    ExpandEnviron new_env = env.new_scope();
    return expand_args(p, StmtPos, new_env);
  } else if (name == "eblock") {
    assert_pos(p, pos, StmtPos | ExpPos);
    ExpandEnviron new_env = env.new_scope();
    return expand_args(p, StmtPos, new_env);
  } else if (name == "if") {
    assert_pos(p, pos, StmtPos);
    assert_num_args(p, 2, 3);
    Parse * res = new Parse(p->str(), p->part(0));
    res->add_part(expand(p->arg(0), ExpPos, env));
    res->add_part(expand(p->arg(1), StmtPos, env));
    if (p->num_args() == 3) 
      res->add_part(expand(p->arg(2), StmtPos, env));
    return res;
  } else if (name == "switch") {
    assert_pos(p, pos, StmtPos);
    assert_num_args(p, 2);
    Parse * res = new Parse(p->str(), p->part(0));
    res->add_part(expand(p->arg(0), ExpPos, env));
    res->add_part(expand(p->arg(1), StmtPos, env));
    return res;
  } else if (name == "loop") {
    assert_pos(p, pos, StmtPos);
    assert_num_args(p, 1);
    return new Parse(p->str(), p->part(0), expand(p->arg(0), StmtPos, env));
  } else if (name == "smap" || name == "map") {
    assert_pos(p, pos, TopLevel);
    Map * m = new Map;
    m->parse_self(p);
    if (name == "smap") 
      syntax_maps.add(m->name, m);
    else
      maps.add(m->name, m);
    return p;
  } else if (name == "lcstmt" || name == "lstmt") {
    assert_pos(p, pos, StmtPos);
    assert_num_args(p, 2, 2);
    Parse * res = new Parse(p->str(), p->part(0));
    res->add_part(expand(p->arg(0), OtherPos, env));
    res->add_part(expand(p->arg(1), StmtPos, env));
    return res;
  } else if (name == "label" || name == "local") {
    return p; // don't expand label name
  } else if (name == "var") {
    assert_pos(p, pos, TopLevel|FieldPos|StmtPos);
    assert_num_args(p, 2, 3);
    env.symbols->add(p->arg(0)->name, VarSym);
    Parse * res = new Parse(p->str(), p->part(0), p->arg(0));
    res->set_flags(p);
    res->add_part(expand_type(p->arg(1), env));
    if (p->num_args() > 2)
      res->add_part(expand(p->arg(2), ExpPos, env));
    return res;
  } else if (name == "talias") {
    assert_pos(p, pos, TopLevel|FieldPos|StmtPos);
    assert_num_args(p, 2);
    env.symbols->root->add(p->arg(0)->name, TypeSym);
    return new Parse(p->str(), p->part(0), p->arg(0), expand_type(p->arg(1), env));
  } else if (name == "fun") {
    assert_pos(p, pos, TopLevel);
    assert_num_args(p, 3, 4);
    env.symbols->root->add(p->arg(0)->name, VarSym);
    const Parse * parms = expand_fun_parms(p->arg(1), env);
    const Parse * ret = expand_type(p->arg(2), env);
    const Parse * body = 0;
    if (p->num_args() > 3) {
      ExpandEnviron new_env = env.new_scope();
      for (unsigned i = 0; i != parms->num_args(); ++i) {
        const Parse * parm = parms->arg(i);
        if (parm->num_parts() == 2) 
          new_env.symbols->add(parm->part(1)->name, VarSym);
      }
      body = expand(p->arg(3), StmtPos, new_env);
    }
    Parse * res = new Parse(p->part(0), p->arg(0), parms, ret);
    res->set_flags(p);
    if (body) res->add_part(body);
    return res;
  } else if (name == "struct" || name == "union") { 
    assert_pos(p, pos, TopLevel|FieldPos|StmtPos);
    assert_num_args(p, 2);
    Parse * res = new Parse(p->str(), p->part(0), p->arg(0));
    res->set_flags(p);
    res->add_part(expand_parts(p->arg(1), FieldPos, env));
    return res;
  } else if (name == "enum") {
    assert_pos(p, pos, TopLevel|FieldPos|StmtPos);
    assert_num_args(p, 2);
    Parse * res = new Parse(p->str(), p->part(0), p->arg(0));
    res->set_flags(p);
    res->add_part(expand_enum_body(p->arg(1), env));
    return res;
  } else if (name == "sizeof") {
    assert_pos(p, pos, ExpPos);
    const Parse * res = parse_decl_->parse_type(p->arg(0), env);
    if (res) {
      res = expand_type(res, env);
      res = new Parse(p->part(0), res);
    } else {
      res = parse_exp_->parse(p->arg(0));
      res = expand(res, pos, env);
      res = new Parse(p->part(0), new Parse(new Parse("typeof"), res));
    }
    return res;
  } else {
    printf(">OTHER>%s\n", ~name);
    return expand_args(p, ExpPos, env);
  }
}

const Parse * expand_type(const Parse * p, ExpandEnviron & env) {
  if (p->name == ".typeof") {
    return new Parse(p->str(), p->part(0), expand(p->arg(0), ExpPos, env));
  } else if (p->name == ".array") {
    return new Parse(p->str(), p->part(0), expand_type(p->arg(0), env), expand(p->arg(1), ExpPos, env));
  } else {
    Parse * res = new Parse(p->str(), p->part(0));
    res->set_flags(p);
    for (unsigned i = 0; i != p->num_args(); ++i) {
      res->add_part(expand_type(p->arg(i), env));
    }
    return res;
  }
}

void assert_pos(const Parse * p, Position have, unsigned need) {
  // FIXME Better Error Message
  //if (!(have & need)) 
  //  throw error(p, "syntax error");
}

void assert_num_args(const Parse * p, unsigned num) {
  if (p->num_args() != num)
    throw error(p, "Expected %d arguments but got %d for \"%s\"", num, p->num_args(), ~p->name);
}

void assert_num_args(const Parse * p, unsigned min, unsigned max) {
  if (p->num_args() < min)
    throw error(p, "Expected at least %d arguments but got %d for \"%s\"", min, p->num_args(), ~p->name);
  if (p->num_args() > max)
    throw error(p->arg(max), "Too many arguments for \"%s\"", ~p->name);
}

const Parse * expand_args(const Parse * p, Position pos, ExpandEnviron & env) {
  static unsigned num = 0;
  unsigned n = num++;
  printf(">>> %d EXPAND PARMS\n", n);
  p->print();
  printf("\n<<<\n");
  
  Parse * res = new Parse(p->str(), p->part(0));
  res->set_flags(p);
  for (unsigned i = 0; i != p->num_args(); ++i) {
    printf("e?? %d\n", i);
    res->add_part(expand(p->arg(i), pos, env));
  }
  printf("*** %d EXPAND PARMS\n", n);
  res->print();
  printf("\n---\n");
  return res;
}

const Parse * expand_parts(const Parse * p, Position pos, ExpandEnviron & env) {
  static unsigned num = 0;
  unsigned n = num++;
  printf(">>> %d EXPAND PARMS\n", n);
  p->print();
  printf("\n<<<\n");
  
  Parse * res = new Parse(p->str());
  res->set_flags(p);
  for (unsigned i = 0; i != p->num_parts(); ++i) {
    printf("e?? %d\n", i);
    res->add_part(expand(p->part(i), pos, env));
  }
  printf("*** %d EXPAND PARMS\n", n);
  res->print();
  printf("\n---\n");
  return res;
}

const Parse * expand_fun_parms(const Parse * parse, ExpandEnviron & env) {
  Parse * res = new Parse(parse->part(0));
  for (unsigned i = 0; i != parse->num_args(); ++i) {
    const Parse * p = parse->arg(i);
    if (p->name != "...") {
      Parse * r = new Parse(expand_type(p->part(0), env));
      if (p->num_parts() == 2) 
	r->add_part(p->part(1));
      res->add_part(r);
    } else {
      res->add_part(p);
    }
  }
  return res;
}

const Parse * expand_enum_body(const Parse * parse, ExpandEnviron & env) {
  Parse * res = new Parse(parse->part(0));
  for (unsigned i = 0; i != parse->num_args(); ++i) {
    const Parse * p = parse->arg(i);
    if (p->num_parts() == 1) {
      res->add_part(p);
    } else {
      res->add_part(new Parse(p->part(0), expand(p->part(1), ExpPos, env)));
    }
  }
  return res;
}

const Parse * expand_call_parms(const Parse * p, ExpandEnviron & env) {
  if (p->name == "list")
    return expand_args(p, ExpPos, env);
  assert(p->name == "(,)");
  static unsigned num = 0;
  unsigned n = num++;
  printf(">>> %d EXPAND CALL PARMS\n", n);
  p->print();
  printf("\n<<<\n");
  
  Parse * res = new Parse(p->str(), new Parse("list"));
  res->set_flags(p);
  printf(">>NUM ARGS %d\n", p->num_args());
  for (unsigned i = 0; i != p->num_args(); ++i) {
    p->arg(i)->print();
    const Parse * q = reparse("EXP", p->arg(i));
    res->add_part(expand(q, ExpPos, env));
  }
  printf("\n*** %d EXPAND CALL PARMS\n", n);
  res->print();
  printf("\n---\n");
  return res;
}

