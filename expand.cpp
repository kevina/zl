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

using namespace ast;

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

struct Map : public Symbol {
  String name;
  SourceEntity entity;
  const Parse * parse;
  const Parse * parms;
  const Parse * repl;
  Map * parse_self(const Parse * p) {
    //printf("PARSING MAP %s\n", ~p->arg(0)->name);
    //p->print();
    //printf("\n");
    entity = SourceEntity(p);
    parse = p;
    assert_num_args(p, 3);
    name = *p->arg(0);
    parms = p->arg(1);
    repl = p->arg(2);
    repl = change_src(repl->str(), repl);
    return this;
  }
  const Parse * change_src(SourceStr outer_str, const Parse * orig) {
    Parse * res = new Parse(orig->what(), orig->str());
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
  const Parse * expand(const Parse * p, Position, Environ &, unsigned shift = 0) const {
    //printf(">>EXPAND MAP %s\n", ~name);
    ReplTable * rparms = new ReplTable;
    int i = 0;
    for (; i != parms->num_parts(); ++i) {
      const Parse * mp = parms->part(i);
      if (mp->num_args() > 0) {
        const Parse * sp = p->arg(i + shift);
        assert(mp->what() == sp->what());
        for (int j = 0; j != mp->num_args(); ++j) {
          rparms->insert(*mp->arg(j), sp->arg(j));
        }
      } else if (mp->is_a("...")) {
        Parse * p2 = new Parse(new Parse("..."));
        for (; i != p->num_args(); ++i) {
          p2->add_part(p->arg(i + shift));
        }
        rparms->insert("...", p2);
        break;
      }
      rparms->insert(mp->what(), p->arg(i + shift));
    }
    //printf(">>TO EXPAND %s\n", ~name);
    //repl->print();
    //printf("\n");
    const Parse * res;
    if (repl->is_a("{}")) {
      res = reparse("STMTS", repl->arg(0), rparms);
      if (res->num_args() == 1)
	res = res->arg(0);
    } else {
      res = replace(repl, rparms);
    }
    res->str_ = p->str();
    //printf(">>>EXPANDED %s\n", ~name);
    //res->print();
    //printf("\n");
    return res;
  }
};

const Parse * reparse(String what, const Parse * p, ReplTable * r) {
  const Replacements * repls = combine_repl(p->repl, r);
  const Parse * res = parse_str(what, p->str(), repls);
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

const Parse * replace(const Parse * p, ReplTable * r) {
  if (p->simple()) {
    String s = *p;
    if (s.size() > 2 && s[0] == '`' && s[1] == '`') {
      // strip one `
      return new Parse(~s + 1, p->str_, p->str_.begin + 1, p->str_.end);
    } else if (s.size() > 1 && s[0] == '`') {
      if (!r->have(s))
        r->insert(s, new Parse(gen_sym(), p->str_));
      return r->lookup(s);
    } else if (r->have(s)) {
      return r->lookup(s);
    } else {
      return p;
    }
  } else if (p->is_a("id") && ((String)*p->arg(0))[0] != '`' && r->have(*p->arg(0))) {
    return r->lookup(*p->arg(0)); 
  } else if (p->is_a("mid")) {
    const Parse * p0 = r->lookup(*p->arg(0));
    if (p0) {
      if (p0->is_a("parm")) {
        String what = *p->arg(1);
        if (what == "TOKEN" || what == "EXP" || what == "STMT")
          what = "PARM";
        p0 = reparse(what, p0->arg(0));
      }
      return p0;
    } else {
      return p;
    }
  } else if (p->is_a("string") || p->is_a("char") || p->is_a("literal") || p->is_a("float") || p->is_a("sym")) {
    return p;
  } else if (p->is_a("{}") || p->is_a("()") || p->is_a("[]") || p->is_a("parm")) {
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

// three type of macros, two namespaces
// syntax macros in there own name space
//   (m ...) or (m)
// function macros
//   (call m (list ...))
// identifier macros
//   m
// function and identifier macros in the same namespace
// NOTE: may also want macros for tags...

static SymbolTable syntax_maps;

/*
struct BuildIn {
  const char * name;
  unsigned pos;
  unsigned min;
  unsigned max;
  Position parms[4];
}
*/

const Parse * expand_call_parms(const Parse * parse, Environ & env);

AST * expand_top(const Parse * p) {
  Environ env;
  assert(p->is_a("top")); // FIXME Error
  return (new Top())->parse_self(p, env);
}

void read_macro(const Parse * p) {
  Environ env;
  expand(p, TopLevel, env);
}

const Parse * ID = new Parse("id");
const Parse * ESTMT = new Parse("estmt");

// should't override following primatives
//   "exp" "stmt" "estmt" and TOKENS
// FIXME: need to add check

const Parse * handle_paran(const Parse * p, Environ & env) {
  try {
    const Parse * exp = reparse("PARAN_EXP", p);
    const Parse * type = parse_decl_->parse_type(exp, env);
    if (type) return new Parse(p->str(), new Parse("(type)"), type);
    // Since the raw string might need to be reparsed we can't use an
    // exp here.  Unfortunately this will likely mean duplicate work.
    // Avoiding that will take more thought
    else return p;
  } catch (...) {
    return p;
  }
}

const Parse * e_parse_exp(const Parse * p, Environ & env) {
  Parse * tmp = new Parse("exp");
  for (unsigned i = 0; i != p->num_args(); ++i) {
    const Parse * t = p->arg(i);
    if (t->is_a("()")) t = handle_paran(t, env);
    tmp->add_part(t);
  }
  const Parse * res = parse_exp_->parse(tmp);
  return res;
}

AST * expand(const Parse * p, Position pos, Environ & env) {
  if (p->entity()) {
    AST * ast = dynamic_cast<AST *>(p->entity());
    assert(ast); // FIXME Error message
    return ast;
  }
  String what = p->what();
  //printf("\n>expand>%s//\n", ~what);
  //p->print();
  //printf("\n////\n");
  if (p->simple()) {
    abort(); // FIXME: Error Message
  } else if (what == "{}") {
    return expand(reparse("BLOCK", p), pos, env);
  } else if (what == "()") {
    return expand(reparse("PARAN_EXP", p), pos, env);
  } else if (what == "[]") {
    return expand(reparse("EXP", p->arg(0)), pos, env);
  } else if (syntax_maps.exists(what)) { // syntax macros
    p = syntax_maps.find<Map>(what)->expand(p, pos, env);
    return expand(p, pos, env);
  } else if (what == "call") { 
    assert_num_args(p, 2);
    const Parse * n = p->arg(0); // FIXME: Need to "partly expand"
    // The parms might already be parsed as something else,
    // the parameters are just a list of tokens at this point,
    // we need to turn them into a proper list
    const Parse * a = reparse("SPLIT", p->arg(1)->arg(0));
    const Map * map = NULL;
    if (n && n->is_a("id"))
        map = env.symbols.find<Map>(*n->arg(0));
    if (map) { // function macros
      //  (call (id fun) (list parm1 parm2 ...))?
      p = map->expand(a, pos, env);
      return expand(p, pos, env);
    } else {
      Parse * res = new Parse(p->str(), p->part(0));
      res->add_part(p->arg(0));
      res->add_part(expand_call_parms(a, env));
      return parse_exp(res, env);
    }
  } else if (what == "smap" || what == "map") {
    assert_pos(p, pos, TopLevel);
    Map * m = new Map;
    m->parse_self(p);
    if (what == "smap") 
      syntax_maps.add(m->name, m);
    else
      env.symbols.add(m->name, m);
    return new Empty();
  } else if (what == "stmt") {
    assert_pos(p, pos, TopLevel|FieldPos|StmtPos|StmtDeclPos);
    const Parse * res = parse_decl_->parse_decl(p, env);
    if (!res)
      res = e_parse_exp(p, env);
    return expand(res, pos, env);
  } else if (what == "exp") {
    assert_pos(p, pos, ExpPos);
    p = e_parse_exp(p, env);
    return expand(p, pos, env);
  } else {
    // we should have a primitive
    switch (pos) {
    case TopLevel:
      return parse_top_level(p, env);
    case FieldPos:
      return parse_member(p, env);
    case StmtDeclPos:
      return parse_stmt_decl(p, env);
    case StmtPos:
      return parse_stmt(p, env);
    case ExpPos:
      return parse_exp(p, env);
    default:
      abort();
    }
  }
}

Type * expand_type(const Parse * p, Environ & env) {
  if (p->entity()) {
    Type * type = dynamic_cast<Type *>(p->entity());
    assert(type);
    return type;
  }
  return parse_type(p, env);
}

void assert_pos(const Parse * p, Position have, unsigned need) {
  // FIXME Better Error Message
  //if (!(have & need)) 
  //  throw error(p, "syntax error");
}

void assert_num_args(const Parse * p, unsigned num) {
  if (p->num_args() != num)
    throw error(p, "Expected %d arguments but got %d for \"%s\"", num, p->num_args(), ~p->what());
}

void assert_num_args(const Parse * p, unsigned min, unsigned max) {
  if (p->num_args() < min)
    throw error(p, "Expected at least %d arguments but got %d for \"%s\"", min, p->num_args(), ~p->what());
  if (p->num_args() > max)
    throw error(p->arg(max), "Too many arguments for \"%s\"", ~p->what());
}

const Parse * expand_args(const Parse * p, Position pos, Environ & env, Parse * res) {
  res->set_flags(p);
  for (unsigned i = 0; i != p->num_args(); ++i) {
    // FIXME need to partly expand it first
    if (p->arg(i)->is_a("list")) {
      expand_args(p->arg(i), pos, env, res);
    } else {
      res->add_part(new Parse(expand(p->arg(i), pos, env))); // FIXME: maybe partial expand only?
    }
  }
  return res;
}

Tuple * expand_fun_parms(const Parse * parse, Environ & env) {
  Parse * res = new Parse(parse->part(0));
  for (unsigned i = 0; i != parse->num_args(); ++i) {
    const Parse * p = parse->arg(i);
    if (!p->is_a("...")) {
      Parse * r = new Parse(expand_type(p->part(0), env));
      if (p->num_parts() == 2) 
	r->add_part(p->part(1));
      res->add_part(r);
    } else {
      res->add_part(p);
    }
  }
  Type * type = expand_type(res, env);
  Tuple * tuple = dynamic_cast<Tuple *>(type);
  assert(tuple); // FIXME: Error Message?
  return tuple;
}

const Parse * expand_call_parms(const Parse * p, Environ & env) {
  if (p->is_a("list"))
    return expand_args(p, ExpPos, env, new Parse(p->str(), p->part(0)));
  assert(p->is_a("(,)"));
  
  Parse * res = new Parse(p->str(), new Parse("list"));
  res->set_flags(p);
  for (unsigned i = 0; i != p->num_args(); ++i) {
    const Parse * q = reparse("EXP", p->arg(i));
    res->add_part(new Parse(expand(q, ExpPos, env)));
  }
  return res;
}

