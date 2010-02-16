#include "symbol_table.hpp"
#include "syntax_gather.hpp"
#include "environ.hpp"

namespace ast {

  InnerNS DEFAULT_NS_OBJ;
  InnerNS TAG_NS_OBJ;
  InnerNS LABEL_NS_OBJ;
  InnerNS SYNTAX_NS_OBJ;
  InnerNS OUTER_NS_OBJ;
  InnerNS INNER_NS_OBJ;
  InnerNS CAST_NS_OBJ;
  InnerNS SPECIAL_NS_OBJ;
  InnerNS OPERATOR_NS_OBJ;

  InnerNS * const DEFAULT_NS = &DEFAULT_NS_OBJ;
  InnerNS * const TAG_NS = &TAG_NS_OBJ;
  InnerNS * const LABEL_NS = &LABEL_NS_OBJ;
  InnerNS * const SYNTAX_NS = &SYNTAX_NS_OBJ;
  InnerNS * const OUTER_NS = &OUTER_NS_OBJ;
  InnerNS * const INNER_NS = &INNER_NS_OBJ;
  InnerNS * const CAST_NS = &CAST_NS_OBJ;
  InnerNS * const SPECIAL_NS = &SPECIAL_NS_OBJ;
  InnerNS * const OPERATOR_NS = &OPERATOR_NS_OBJ;

  void add_inner_nss(Environ & env) {
    env.add_internal(SymbolKey("default", INNER_NS), DEFAULT_NS);
    env.add_internal(SymbolKey("tag", INNER_NS), TAG_NS);
    env.add_internal(SymbolKey("label", INNER_NS), LABEL_NS);
    env.add_internal(SymbolKey("syntax", INNER_NS), SYNTAX_NS);
    env.add_internal(SymbolKey("outer", INNER_NS), OUTER_NS);
    env.add_internal(SymbolKey("inner", INNER_NS), INNER_NS);
    env.add_internal(SymbolKey("cast", INNER_NS), CAST_NS);
    env.add_internal(SymbolKey("special", INNER_NS), SPECIAL_NS);
    env.add_internal(SymbolKey("operator", OPERATOR_NS), OPERATOR_NS);
  }

  void marks_ignored(String name) {
    fprintf(stderr, "WARNING: IGNORING MARKS ON \"%s\"\n", ~name);
    //abort();
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
    if (ns != def_ns)
      o << "`" << ns->name();
  }

  void TopLevelSymbol::make_unique(SymbolNode * self, SymbolNode * stop) const {
    abort();
    //if (num == NPOS)
    //  assign_uniq_num<TopLevelSymbol>(self, stop);
  }

  void SymbolTable::dump() const {
    printf("=== BEGIN SYMBOL TABLE ===\n");
    printf("%p %p <> %p %p\n", front, back, &front, ip.front);
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

  void SymbolTable::dump_this_scope() const {
    for (SymbolNode * c = front; c != back; c = c->next)
      printf("  %s %p %s %s\n", ~c->key.to_string(), 
             c->value,
             c->value ? ~c->value->name() : "", 
             c->value ? ~c->value->uniq_name() : "");
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
