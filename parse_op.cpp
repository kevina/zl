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
  OpKey(const String & c)
    : category(c) {}
  OpKey(const String & c, const String & s)
    : category(c), symbol(s) {}
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

struct Op : public OpKey {
  enum Type {Other = 1, Prefix = 2, Bin = 4, Postfix = 8} type;
  typedef int Types;
  int level;
  Assoc assoc;
  String name;
  const Parse * parse;
  bool capture_op_itself() const {return symbol.empty();}
  void parse_self(const Parse * p) {
    if (p->name == "bin")
      type = Bin;
    else if (p->name == "prefix")
      type = Prefix;
    else if (p->name == "postfix")
      type = Postfix;
    else if (p->name == "exp")
      type = Other;
    else
      abort();
    parse = p->arg(0);
    name = parse->name;
    if (p->arg(1)->name == "category") {
      category = p->arg(1)->arg(0)->name;
    } else {
      category = p->arg(1)->name;
      symbol = p->arg(1)->arg(0)->name;
    }
  }
};

struct Group {
  Assoc assoc;
  Vector<Op> ops;
  void parse_self(const Parse * p) {
    if (p->name == "none")
      assoc = None;
    else if (p->name == "left")
      assoc = Left;
    else if (p->name == "right")
      assoc = Right;
    else if (p->name == "list")
      assoc = List;
    else
      abort();
    ops.resize(p->num_args());
    for (int i = 0; i != p->num_args(); ++i) {
      ops[i].parse_self(p->arg(i));
    }
  }
};

struct Ops : public gc_cleanup {
  typedef hash_multimap<OpKey, Op *> Lookup;
  Lookup lookup_;
  Vector<Group> groups;
  void parse_self(const Parse * p) {
    groups.resize(p->num_args());
    for (int i = 0; i != p->num_args(); ++i) {
      groups[i].parse_self(p->arg(i));
      Vector<Op> & ops = groups[i].ops;
      for (int j = 0; j != ops.size(); ++j) {
        ops[j].level = i;
        ops[j].assoc = groups[i].assoc;
        pair<Lookup::iterator,bool> res = lookup_.insert(pair<OpKey,Op *>(ops[j],&ops[j]));
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
      res = i_lookup_types(res, OpKey(p->name, p->arg(0)->name), p);
    if (res == 0)
      res = i_lookup_types(res, OpKey(p->name), p);
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
        return is.first->second;
    return 0;
  }

  const Op * lookup(const Parse * p, int type) const {
    const Op * op = 0;
    if (p->num_args() > 0)
      op = i_lookup(OpKey(p->name, p->arg(0)->name), type, p);
    if (op == 0)
      op = i_lookup(OpKey(p->name), type, p);
    if (op == 0)
      op = &generic;
    return op;
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
    unsigned sz = p->num_args();
    Op::Types prev = 0;
    for (int i = 0; i != sz; ++i) {
      const Parse * pop = p->arg(i); // parsed op
      printf("$$%d %s\n", i, ~p->arg(i)->name);
      Op::Types cur = ops.lookup_types(pop);
      if (prev == 0 || prev & (Op::Prefix | Op::Bin)) {
        cur &= (Op::Prefix | Op::Other);
        if (cur == 0) 
          throw error(pop, "Expected an operand or a prefix operator.");
      } else {
        cur &= (Op::Bin | Op::Postfix);
        if (cur == 0)
          throw error(pop, "Expected a binary or postfix operator.");
      } 
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
    val_s.front()->print();
    return val_s.front();
  }

  const Parse * parse_list(const Parse * p) {
    String list_group = ops.groups.back().ops.front().name;
    if (p->num_args() == 0) return new Parse(list_group);
    const Parse * res = parse(p);
    if (list_group == res->name) {
      return res;
    } else {
      return new Parse(new Parse(list_group), res);
    }
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
        printf(">PREFIX>> %.*s\n",  opi.parse->str().end - opi.parse->str().begin, opi.parse->str().begin);
        parse->str_ = opi.parse->str();
        if (p1->str().source == parse->str_.source)
          parse->str_.end   = p1->str().end;
        if (op->capture_op_itself())
          parse->add_part(opi.parse);
        parse->add_part(p1);
        parse->print();
        printf("\n>prefix>> %.*s\n",  parse->str().end - parse->str().begin, parse->str().begin);
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

