#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "config.h"

#include "parse.hpp"
#include "parse_op.hpp"
#include "hash-t.hpp"

// http://en.wikipedia.org/wiki/Common_operator_notation
//   http://en.wikipedia.org/w/index.php?title=Common_operator_notation&oldid=147904781

using std::pair;

enum Assoc {None, Left, Right, List};
enum Which {Category, Symbol};

struct OpKey : public gc {
  String what;
  Which which;
  OpKey() {}
  // FIXME: operators should proabaly be hygienic
  OpKey(const SymbolName & wt, Which wc)
    : what(wt.name), which(wc) {}
};

static inline bool operator== (const OpKey & x, const OpKey & y) {
  return x.what == y.what && x.which == y.which;
}

//static inline bool operator< (const OpKey & x, const OpKey & y) {
//  if (x.category < y.category) return true;
//  if (x.category > y.category) return false;
//  return x.symbol < y.symbol;
//}

template <> struct hash<OpKey> {
    inline unsigned long operator() (const OpKey & v) const {
      return HashString<String>()(v.what);
    }
};

struct MatchOp : public OpKey {
  bool capture_op_itself() const {return which == Category;}
  void parse_match_op(const Syntax * p) {
    if (p->is_a("category")) {
      what = *p->arg(0);
      which = Category;
    } else {
      what = *p;
      which = Symbol;
    }
  }
};

struct OpCommon : public MatchOp {
  enum Type {Other = 1, Prefix = 2, Bin = 4, Postfix = 8, Special = 16} type;
  typedef int Types;
  String name;
  const Syntax * parse;
  bool capture_op_itself() const {return which == Category;}
  virtual void parse_self(const Syntax * p) {
    if (p->is_a("bin"))
      type = Bin;
    else if (p->is_a("prefix"))
      type = Prefix;
    else if (p->is_a("postfix"))
      type = Postfix;
    else if (p->is_a("exp"))
      type = Other;
    else if (p->is_a("special"))
      type = Special;
    else
      abort();
    parse = p->arg(0);           ;
    name = parse->what();
    parse_match_op(p->arg(1));
  }
  virtual ~OpCommon() {}
};

struct Op : public OpCommon {
  int level;
  Assoc assoc;
};

struct SpecialOp : public OpCommon {
  Vector<MatchOp> rest;
  virtual void parse_self(const Syntax * p) {
    OpCommon::parse_self(p);
    assert(type == Special);
    rest.resize(p->num_args() - 2);
    for (unsigned i = 2; i < p->num_args(); ++i) {
      rest[i-2].parse_match_op(p->arg(i));
    }
  }
  const Syntax * match(Parts::const_iterator & i0, Parts::const_iterator end) const {
    Parts::const_iterator i = i0;
    const Syntax * fp = *i;
    ++i;
    Parts working;
    Vector<MatchOp>::const_iterator j = rest.begin(), e = rest.end();
    for (; j != e && i != end; ++j, ++i) {
      const Syntax * p = *i;
      if (p->simple()) {
        if (j->which == Symbol && p->eq(j->what)) {}
        else return NULL;
      } else {
        if (j->which == Category && p->is_a(j->what))
          working.push_back(p);
        else
          return NULL;
      }
    }
    if (j != e) return NULL;
    --i; // backup on
    SourceStr str = fp->str();
    str.adj((*i)->str());
    SyntaxBuilder res;
    res.add_part(SYN(name));
    res.add_parts(working.begin(), working.end());
    i0 = i;
    return res.build(str);
  }
};

struct Ops {
  typedef hash_multimap<OpKey, OpCommon *> Lookup;
  Lookup lookup_;
  void parse_self(const Syntax * p) {
    for (int i = 0; i != p->num_args(); ++i) {
      parse_group(p->arg(i), i);
    }
  }
  void parse_group(const Syntax * p, unsigned level) {
    Assoc assoc;
    if (p->is_a("none"))
      assoc = None;
    else if (p->is_a("left"))
      assoc = Left;
    else if (p->is_a("right"))
      assoc = Right;
    else if (p->is_a("list"))
      assoc = List;
    else
      abort();
    for (int i = 0; i != p->num_args(); ++i) {
      if (p->arg(i)->is_a("special")) {
        SpecialOp * op = new SpecialOp;
        op->parse_self(p->arg(i));
        lookup_.insert(pair<OpKey,OpCommon *>(*op,op));      
      } else {
        Op * op = new Op;
        op->parse_self(p->arg(i));
        op->level = level;
        op->assoc = assoc;
        lookup_.insert(pair<OpKey,OpCommon *>(*op,op));      
      }
    }
  }
  Op::Types i_lookup_types(Op::Types res, const OpKey & k, const Syntax * p) const {
      pair<Lookup::const_iterator,Lookup::const_iterator> is
       = lookup_.equal_range(k);
     for( ; is.first != is.second; ++is.first) {
       res |= is.first->second->type;
     }
     return res;
  }

  Op::Types lookup_types(const Syntax * p) const {
    Op::Types res = 0;
    if (p->simple())
      res = i_lookup_types(res, OpKey(p->what(), Symbol), p);
    else 
      res = i_lookup_types(res, OpKey(p->what(), Category), p);
    if (res == 0) 
      res = Op::Other;
    return res;
  }

  Op generic;

  Ops() : generic() {generic.type = Op::Other; generic.level = 0;}

  const Op * i_lookup(const OpKey & k, int type, const Syntax * p) const {
    pair<Lookup::const_iterator,Lookup::const_iterator> is
      = lookup_.equal_range(k);
    for( ; is.first != is.second; ++is.first)
      if (is.first->second->type == type)
        return static_cast<const Op *>(is.first->second);
    return 0;
  }

  const Op * lookup(const Syntax * p, int type) const {
    const Op * op;
    if (p->simple())
      op = i_lookup(OpKey(p->what(), Symbol), type, p);
    else
      op = i_lookup(OpKey(p->what(), Category), type, p);
    if (op == 0)
      op = &generic;
    return op;
  }

  const Syntax * i_try_special(const OpKey & k, Parts::const_iterator & i, Parts::const_iterator e) {
    pair<Lookup::const_iterator,Lookup::const_iterator> is
      = lookup_.equal_range(k);
     for( ; is.first != is.second; ++is.first) {
       if (is.first->second->type == Op::Special) {
         const SpecialOp * op = static_cast<const SpecialOp *>(is.first->second);
         const Syntax * res = op->match(i, e);
         if (res) return res;
       }
     }
     return NULL;
  }
  
  const Syntax * try_special(const Syntax * p, Parts::const_iterator & i, Parts::const_iterator e) {
    const Syntax * res = NULL;
    if (p->simple())
      res = i_try_special(OpKey(p->what(), Symbol), i, e);
    else
      res = i_try_special(OpKey(p->what(), Category), i, e);
    return res;
  }
};

class ParseExpImpl : public ParseExp {
  Ops ops;
  struct OpInfo {
    const Op * op;
    const Syntax * parse;
    OpInfo(const Op * o, const Syntax * p)
      : op(o), parse(p) {}
  };
  Vector<const Syntax *> val_s;
  Vector<OpInfo>         opr_s;
  String list_is;
public:

  void init() {
    SourceFile * code = new_source_file(SOURCE_PREFIX "ops.in");
    //const char * s = code.begin();
    parse_parse::Res r = parse_parse::parse(code);
    //printf(">.>%.*s<.<\n", r.end-s, s);
    //r.parse->print();
    //printf("\n");
    ops.parse_self(r.parse);
  }
  
  const Syntax * parse(const Syntax * p, const char * l_i) {
    //printf("ParseExpImpl::parse: %s %s\n", ~p->sample_w_loc(), ~p->to_string());
    opr_s.clear();
    val_s.clear();
    list_is = l_i;
    Op::Types prev = 0;
    try {
      for (Parts::const_iterator i = p->args_begin(), e = p->args_end(); i != e; ++i) {
        const Syntax * pop = *i; // parsed op
        Op::Types cur = ops.lookup_types(pop);
        if (cur & Op::Special) {
          const Syntax * res = ops.try_special(pop, i, e);
          if (res) {
            pop = res;
            cur = ops.lookup_types(pop);
          } else {
            cur &= ~Op::Special;
          }
        } 
        assert(!(cur & Op::Special));
        if (prev == 0 || prev & (Op::Prefix | Op::Bin)) {
          cur &= (Op::Prefix | Op::Other);
          if (cur == 0) 
            throw error(pop, "Expected an operand or a prefix operator.");
        } else {
          cur &= (Op::Bin | Op::Postfix);
          if (cur == 0) 
            throw error(pop, "Expected a binary or postfix operator.");
        }
        //if (cur == (Op::Prefix | Op::Other)) {
        //  if (i + 1 == sz || (ops.lookup_types(p->arg(i+1)) & (Op::Postfix | Op::Bin))) {
        //    cur &= (Op::Postfix | Op::Other);
        //    if (cur == 0)
        //      throw error(pop, "Expected an operand or a postfix operator.");
        //  } else {
        //    cur &= (Op::Bin | Op::Prefix);
        //    if (cur == 0)
        //      throw error(pop, "Expected an binary or prefix operator.");
        //  }
        //}
        const Op * op = ops.lookup(pop, cur);
        
        if (op->type == Op::Other) {
          val_s.push_back(pop);
        } else {
          while (!opr_s.empty() && 
                 (opr_s.back().op->level < op->level || 
                  (opr_s.back().op->level == op->level && op->assoc == Left)))
            reduce();
          if (!opr_s.empty() && opr_s.back().op->level == op->level && op->assoc == None)
            throw error(pop, "\"%s\" is non-associative.", 
                        op->what.c_str());
          opr_s.push_back(OpInfo(op, pop)); 
        }
        prev = cur;
      }
      while (!opr_s.empty())
        reduce();
      if (val_s.size() == 0)
        throw error(p, "Empty expression.");
      if (val_s.size() > 1)
        throw error(val_s[val_s.size()-2], "Extra operand(s).");
      assert(val_s.size() == 1);
      // Don't do this for now, cases weird problems
      //return new Syntax(p->str(), *val_s.front());
      return val_s.front();
    } catch (Error * err) {
      printf("?? %s %s\n", ~p->sample_w_loc(), ~p->to_string());
      //abort();
      err->source = new ParseSourceInfo(p->str(), "<op exp>");
      throw err;
    }
  }
  
  void reduce() {
    const OpInfo & opi = opr_s.back();
    const Op * op = opi.op;
    SyntaxBuilder res;
    if (op->name == "<list>")
      res.add_part(SYN(list_is));
    else
      res.add_part(op->parse);
    SourceStr str = opi.parse->str(); // FIXME: Is this right
    if (op->assoc == List) {
      if (val_s.size() < 2)
        throw error(opi.parse, "\"%s\" operator needs 2 operands.",
                    op->what.c_str());
      int num = 1;
      while (!opr_s.empty() && opr_s.back().op->level == op->level) {
        ++num; 
        opr_s.pop_back();
      }
      // FIXME: This isn't right when the parts of the list are not
      // from the same file
      str.begin = val_s[val_s.size() - num]->str().begin;
      str.end = val_s.back()->str().end;
      for (int i = val_s.size() - num; i != val_s.size(); ++i)
        res.add_part(val_s[i]);
      val_s.resize(val_s.size() - num);
    } else if (op->type == Op::Bin) {
      if (val_s.size() < 2)
        throw error(opi.parse, "\"%s\" operator needs 2 operands.",
                    op->what.c_str());
      const Syntax * p2 = val_s.back(); val_s.pop_back();
      const Syntax * p1 = val_s.back(); val_s.pop_back();
      str = opi.parse->str();
      if (p1->str().source->block() == str.source->block())
        str.begin = p1->str().begin;
      if (p2->str().source->block() == str.source->block())
        str.end   = p2->str().end;
      res.add_part(p1);
      if (op->capture_op_itself()) res.add_part(opi.parse);
      res.add_part(p2);
      opr_s.pop_back();
    } else {
      if (val_s.size() < 1)
        throw error(opi.parse, "\"%s\" operator needs an operand.",
                    op->what.c_str());
      const Syntax * p1 = val_s.back(); val_s.pop_back();
      if (op->type == Op::Prefix) {
        str = opi.parse->str();
        if (p1->str().source->block() == str.source->block())
          str.end = p1->str().end;
        if (op->capture_op_itself())
          res.add_part(opi.parse);
        res.add_part(p1);
      } else if (op->type == Op::Postfix) {
        str = opi.parse->str();
        if (p1->str().source->block() == str.source->block())
          str.begin = p1->str().begin;
        res.add_part(p1);
        if (op->capture_op_itself())
          res.add_part(opi.parse);
      } 
      opr_s.pop_back();
    }
    val_s.push_back(res.build(str));
  }
};

ParseExp * new_parse_exp() {
  return new ParseExpImpl();
}

ParseExpImpl parse_exp_obj;
ParseExp * parse_exp_ = &parse_exp_obj;

