#include "symbol_table.hpp"
#include "syntax_gather.hpp"
#include "environ.hpp"

namespace ast {
  
  unsigned InnerNS::Tag::last_order_num = 0;

  InnerNS::Tag DEFAULT_NS_OBJ;
  InnerNS::Tag TAG_NS_OBJ;
  InnerNS::Tag LABEL_NS_OBJ;
  InnerNS::Tag SYNTAX_NS_OBJ;
  InnerNS::Tag OUTER_NS_OBJ;
  InnerNS::Tag INNER_NS_OBJ;
  InnerNS::Tag CAST_NS_OBJ;
  InnerNS::Tag SPECIAL_NS_OBJ;
  InnerNS::Tag OPERATOR_NS_OBJ;
  InnerNS::Tag INTERNAL_NS_OBJ;

  const InnerNS * const DEFAULT_NS = &DEFAULT_NS_OBJ;
  const InnerNS * const TAG_NS = &TAG_NS_OBJ;
  const InnerNS * const LABEL_NS = &LABEL_NS_OBJ;
  const InnerNS * const SYNTAX_NS = &SYNTAX_NS_OBJ;
  const InnerNS * const OUTER_NS = &OUTER_NS_OBJ;
  const InnerNS * const INNER_NS = &INNER_NS_OBJ;
  const InnerNS * const CAST_NS = &CAST_NS_OBJ;
  const InnerNS * const SPECIAL_NS = &SPECIAL_NS_OBJ;
  const InnerNS * const OPERATOR_NS = &OPERATOR_NS_OBJ;
  const InnerNS * const INTERNAL_NS = &INTERNAL_NS_OBJ;

  void add_inner_nss(Environ & env) {
    env.add_internal(SymbolKey("default", INNER_NS), &DEFAULT_NS_OBJ);
    env.add_internal(SymbolKey("tag", INNER_NS), &TAG_NS_OBJ);
    env.add_internal(SymbolKey("label", INNER_NS), &LABEL_NS_OBJ);
    env.add_internal(SymbolKey("syntax", INNER_NS), &SYNTAX_NS_OBJ);
    env.add_internal(SymbolKey("outer", INNER_NS), &OUTER_NS_OBJ);
    env.add_internal(SymbolKey("inner", INNER_NS), &INNER_NS_OBJ);
    env.add_internal(SymbolKey("cast", INNER_NS), &CAST_NS_OBJ);
    env.add_internal(SymbolKey("special", INNER_NS), &SPECIAL_NS_OBJ);
    env.add_internal(SymbolKey("operator", INNER_NS), &OPERATOR_NS_OBJ);
    env.add_internal(SymbolKey("internal", INNER_NS), &INTERNAL_NS_OBJ);
  }

  void marks_ignored(String name) {
    fprintf(stderr, "WARNING: IGNORING MARKS ON \"%s\"\n", ~name);
    abort();
  }

  unsigned Mark::last_id = 0;

  void Marks::to_string(OStream & o, SyntaxGather * g) const {
    Vector<const Mark *> mks;
    for (const Marks * cur = this; cur; cur = cur->prev) {
      mks.push_back(cur->mark);
    }
    while (!mks.empty()) {
      const Mark * m = mks.back();
      mks.pop_back();
      if (g) {
        o.printf("'%u", g->mark_map.insert(m));
      } else {
        o.printf("'%u", m->id);
      }
    }
  }

  void SymbolName::to_string(OStream & o, SyntaxGather * g) const {
    o << name;
    if (marks)
      marks->to_string(o, g);
  }

  void SymbolKey::to_string(OStream & o, const InnerNS * def_ns) const {
    SymbolName::to_string(o);
    for (const InnerNS * cur = ns; cur && cur != def_ns; cur = cur->next) {
      if (cur->tag != def_ns)
        o << "`" << cur->tag->name();
    }
  }

  void TopLevelSymbol::make_unique(SymbolNode * self, SymbolNode * stop) const {
    abort();
    //if (num == NPOS)
    //  assign_uniq_num<TopLevelSymbol>(self, stop);
  }

  void SymbolTableBase::dump() const {
    printf("=== BEGIN SYMBOL TABLE ===\n");
    //printf("%p %p <> %p %p\n", front, back, &front, ip.front);
    for (SymbolNode * c = front; c; c = c->next) {
      if (c == back)
        printf("--- end current scope ---\n");
      printf("  %s %p %s %s\n", ~c->key.to_string(), 
             c->value,
             c->value ? ~c->value->name() : "", 
             c->value ? ~c->value->uniq_name() : "");
    }
    printf("^^^ END SYMBOL TABLE ^^^\n");
  }

  void SymbolTableBase::dump_this_scope() const {
    for (SymbolNode * c = front; c != back; c = c->next) {
      printf("  %s %p %s %s", ~c->key.to_string(), 
             c->value,
             c->value ? ~c->value->name() : "", 
             c->value ? ~c->value->uniq_name() : "");
      unsigned flags = c->flags;
      if (flags == 0) printf(" NO_FLAGS");
      if (flags & SymbolNode::ALIAS) printf(" ALIAS");
      if (flags & SymbolNode::IMPORTED) printf(" IMPORTED");
      if (flags & SymbolNode::DIFF_SCOPE) printf(" DIFF_SCOPE");
      if (flags & SymbolNode::INTERNAL) printf(" INTERVAL");
      printf("\n");
    }
  }

  void Props::add_prop(SymbolName n, const Syntax * s) {
    props = new PropNode(n, s, props);
  }

  const Syntax * Props::get_prop(SymbolName n) const {
    for (PropNode * cur = props; cur; cur = cur->next) {
      if (n == cur->name) return cur->value;
    }
    if (n.marks) {
      n.marks = n.marks->prev;
      return get_prop(n);
    } else {
      return NULL;
    }
  }

  void TopLevelSymbol::assign_uniq_num(SymbolNode * cur) const {
    ast::assign_uniq_num<TopLevelSymbol>(this, cur);
  }

  template <>
  void assign_uniq_num<TopLevelSymbol>(const TopLevelSymbol * sym, SymbolNode * cur, SymbolNode * stop) {
    const TopLevelSymbol * t = NULL;
    // we need to compare the actual symbol name, since it may be
    // aliases as a different name
    String name = sym->name();
    const InnerNS * ns = sym->tl_namespace();
    for (; cur != stop; cur = cur->next) {
      if (cur->value && cur->value->name() == name && 
          (t = dynamic_cast<const TopLevelSymbol *>(cur->value)) && 
          t != sym && t->num != 0 && t->tl_namespace() == ns) 
        break;
      t = NULL;
    }
    unsigned num = 1;
    assert(!t || t->num != NPOS);
    if (t) num = t->num + 1;
    assign_uniq_num(num, sym);
  }


}
