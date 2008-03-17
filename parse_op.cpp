#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "parse.hpp"
#include "parse_op.hpp"
#include "hash-t.hpp"

// http://en.wikipedia.org/wiki/Common_operator_notation
//   http://en.wikipedia.org/w/index.php?title=Common_operator_notation&oldid=147904781

using std::pair;

enum Assoc {None, Left, Right, List};

struct OpKey : public gc {
  String category;
  String symbol;
  OpKey() {}
  // FIXME: operators should proabaly be hygienic
  OpKey(const SymbolName & c)
    : category(c.name) {}
  OpKey(const SymbolName & c, const SymbolName & s)
    : category(c.name), symbol(s.name) {}
};

static inline bool operator== (const OpKey & x, const OpKey & y) {
  return x.category == y.category && x.symbol == y.symbol;
}

static inline bool operator< (const OpKey & x, const OpKey & y) {
  if (x.category < y.category) return true;
  if (x.category > y.category) return false;
  return x.symbol < y.symbol;
}

template <> struct hash<OpKey> {
    inline unsigned long operator() (const OpKey & v) const {
      return HashString<String>()(v.category)
        + HashString<String>()(v.symbol);
    }
};

struct MatchOp : public OpKey {
  bool capture_op_itself() const {return symbol.empty();}
  void parse_match_op(const Parse * p) {
    if (p->is_a("category")) {
      category = *p->arg(0);
    } else {
      category = p->what();
      symbol = *p->arg(0);
    }
  }
};

struct OpCommon : public MatchOp {
  enum Type {Other = 1, Prefix = 2, Bin = 4, Postfix = 8, Special = 16} type;
  typedef int Types;
  String name;
  const Parse * parse;
  bool capture_op_itself() const {return symbol.empty();}
  virtual void parse_self(const Parse * p) {
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
  virtual void parse_self(const Parse * p) {
    OpCommon::parse_self(p);
    assert(type == Special);
    rest.resize(p->num_args() - 2);
    for (unsigned i = 2; i < p->num_args(); ++i) {
      rest[i-2].parse_match_op(p->arg(i));
    }
  }
  const Parse * match(Parts::const_iterator & i0, Parts::const_iterator end) const {
    Parts::const_iterator i = i0;
    const Parse * fp = *i;
    ++i;
    Parts working;
    Vector<MatchOp>::const_iterator j = rest.begin(), e = rest.end();
    for (; j != e && i != end; ++j, ++i) {
      const Parse * p = *i;
      if (p->is_a(j->category)) {
        if (j->symbol.empty()) {
          working.push_back(p);
        } else if (*p->arg(0) == ~j->symbol) {
          return NULL;
        }
      } else {
        return NULL;
      }
    }
    if (j != e) return NULL;
    --i; // backup on
    SourceStr str = fp->str();
    str.adj((*i)->str());
    Parse * res = new Parse(str, new Parse(name));
    res->add_parts(working.begin(), working.end());
    i0 = i;
    return res;
  }
};

struct Ops : public gc_cleanup {
  typedef hash_multimap<OpKey, OpCommon *> Lookup;
  Lookup lookup_;
  void parse_self(const Parse * p) {
    for (int i = 0; i != p->num_args(); ++i) {
      parse_group(p->arg(i), i);
    }
  }
  void parse_group(const Parse * p, unsigned level) {
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
  Op::Types i_lookup_types(Op::Types res, const OpKey & k, const Parse * p) const {
      pair<Lookup::const_iterator,Lookup::const_iterator> is
       = lookup_.equal_range(k);
     for( ; is.first != is.second; ++is.first) {
       res |= is.first->second->type;
     }
     return res;
  }

  Op::Types lookup_types(const Parse * p) const {
    Op::Types res = 0;
    if (p->num_args() > 0)
      res = i_lookup_types(res, OpKey(p->what(), p->arg(0)->what()), p);
    if (res == 0)
      res = i_lookup_types(res, OpKey(p->what()), p);
    if (res == 0) 
      res = Op::Other;
    return res;
  }

  Op generic;

  Ops() : generic() {generic.type = Op::Other; generic.level = 0;}

  const Op * i_lookup(const OpKey & k, int type, const Parse * p) const {
    pair<Lookup::const_iterator,Lookup::const_iterator> is
      = lookup_.equal_range(k);
    for( ; is.first != is.second; ++is.first)
      if (is.first->second->type == type)
        return static_cast<const Op *>(is.first->second);
    return 0;
  }

  const Op * lookup(const Parse * p, int type) const {
    const Op * op = 0;
    if (p->num_args() > 0)
      op = i_lookup(OpKey(p->what(), p->arg(0)->what()), type, p);
    if (op == 0)
      op = i_lookup(OpKey(p->what()), type, p);
    if (op == 0)
      op = &generic;
    return op;
  }

  const Parse * i_try_special(const OpKey & k, Parts::const_iterator & i, Parts::const_iterator e) {
    pair<Lookup::const_iterator,Lookup::const_iterator> is
      = lookup_.equal_range(k);
     for( ; is.first != is.second; ++is.first) {
       if (is.first->second->type == Op::Special) {
         const SpecialOp * op = static_cast<const SpecialOp *>(is.first->second);
         const Parse * res = op->match(i, e);
         if (res) return res;
       }
     }
     return NULL;
  }
  
  const Parse * try_special(const Parse * p, Parts::const_iterator & i, Parts::const_iterator e) {
    const Parse * res = NULL;
    if (p->num_args() > 0)
      res = i_try_special(OpKey(p->what(), p->arg(0)->what()), i, e);
    if (res == 0)
      res = i_try_special(OpKey(p->what()), i, e);
    return res;
  }
};

class ParseExpImpl : public ParseExp {
  Ops ops;
  struct OpInfo {
    const Op * op;
    const Parse * parse;
    OpInfo(const Op * o, const Parse * p)
      : op(o), parse(p) {}
  };
  Vector<const Parse *> val_s;
  Vector<OpInfo>        opr_s;
public:

  void init() {
    SourceFile * code = new_source_file("ops.in");
    //const char * s = code.begin();
    parse_parse::Res r = parse_parse::parse(code->entity());
    //printf(">.>%.*s<.<\n", r.end-s, s);
    //r.parse->print();
    //printf("\n");
    ops.parse_self(r.parse);
  }
  
  const Parse * parse(const Parse * p) {
    opr_s.clear();
    val_s.clear();
    Op::Types prev = 0;
    for (Parts::const_iterator i = p->args_begin(), e = p->args_end(); i != e; ++i) {
      const Parse * pop = *i; // parsed op
      Op::Types cur = ops.lookup_types(pop);
      if (cur & Op::Special) {
        const Parse * res = ops.try_special(pop, i, e);
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
                      op->symbol.c_str());
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
    return val_s.front();
  }

  void reduce() {
    const OpInfo & opi = opr_s.back();
    const Op * op = opi.op;
    Parse * parse = new Parse(op->parse);
    parse->str_ = opi.parse->str(); // FIXME: Is this right
    if (op->assoc == List) {
      if (val_s.size() < 2)
        throw error(opi.parse, "\"%s\" operator needs 2 operands.",
                    op->symbol.c_str());
      int num = 1;
      while (!opr_s.empty() && opr_s.back().op->level == op->level) {
        ++num; 
        opr_s.pop_back();
      }
      // FIXME: This isn't right when the parts of the list are not
      // from the same file
      parse->str_.begin = val_s[val_s.size() - num]->str().begin;
      parse->str_.end = val_s.back()->str().end;
      for (int i = val_s.size() - num; i != val_s.size(); ++i)
        parse->add_part(val_s[i]);
      val_s.resize(val_s.size() - num);
    } else if (op->type == Op::Bin) {
      if (val_s.size() < 2)
        throw error(opi.parse, "\"%s\" operator needs 2 operands.",
                    op->symbol.c_str());
      const Parse * p2 = val_s.back(); val_s.pop_back();
      const Parse * p1 = val_s.back(); val_s.pop_back();
      parse->str_ = opi.parse->str();
      if (p1->str().source == parse->str_.source)
        parse->str_.begin = p1->str().begin;
      if (p2->str().source == parse->str_.source)
        parse->str_.end   = p2->str().end;
      parse->add_part(p1);
      if (op->capture_op_itself()) parse->add_part(opi.parse);
      parse->add_part(p2);
      opr_s.pop_back();
    } else {
      if (val_s.size() < 1)
        throw error(opi.parse, "\"%s\" operator needs an operand.",
                    op->symbol.c_str());
      const Parse * p1 = val_s.back(); val_s.pop_back();
      if (op->type == Op::Prefix) {
        parse->str_ = opi.parse->str();
        if (p1->str().source == parse->str_.source)
          parse->str_.end   = p1->str().end;
        if (op->capture_op_itself())
          parse->add_part(opi.parse);
        parse->add_part(p1);
      } else if (op->type == Op::Postfix) {
        parse->str_ = opi.parse->str();
        if (p1->str().source == parse->str_.source)
          parse->str_.begin = p1->str().begin;
        parse->add_part(p1);
        if (op->capture_op_itself())
          parse->add_part(opi.parse);
      } 
      opr_s.pop_back();
    }
    val_s.push_back(parse);
  }
};

ParseExp * new_parse_exp() {
  return new ParseExpImpl();
}

ParseExpImpl parse_exp_obj;
ParseExp * parse_exp_ = &parse_exp_obj;

