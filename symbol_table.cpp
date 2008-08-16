#include "symbol_table.hpp"

namespace ast {

  InnerNS DEFAULT_NS_OBJ("default");
  InnerNS TAG_NS_OBJ("tag");
  InnerNS LABEL_NS_OBJ("label");
  InnerNS SYNTAX_NS_OBJ("syntax");
  InnerNS OUTER_NS_OBJ("outer");
  InnerNS INNER_NS_OBJ("inner");
  InnerNS CAST_NS_OBJ("cast");

  const InnerNS * const DEFAULT_NS = &DEFAULT_NS_OBJ;
  const InnerNS * const TAG_NS = &TAG_NS_OBJ;
  const InnerNS * const LABEL_NS = &LABEL_NS_OBJ;
  const InnerNS * const SYNTAX_NS = &SYNTAX_NS_OBJ;
  const InnerNS * const OUTER_NS = &OUTER_NS_OBJ;
  const InnerNS * const INNER_NS = &INNER_NS_OBJ;
  const InnerNS * const CAST_NS = &CAST_NS_OBJ;

  void add_inner_nss(SymbolTable & syms) {
    syms.add(SymbolKey("default", INNER_NS), DEFAULT_NS);
    syms.add(SymbolKey("tag", INNER_NS), TAG_NS);
    syms.add(SymbolKey("label", INNER_NS), LABEL_NS);
    syms.add(SymbolKey("syntax", INNER_NS), SYNTAX_NS);
    syms.add(SymbolKey("outer", INNER_NS), OUTER_NS);
    syms.add(SymbolKey("inner", INNER_NS), INNER_NS);
    syms.add(SymbolKey("cast", INNER_NS), CAST_NS);
  }

  void marks_ignored(String name) {
    fprintf(stderr, "WARNING: IGNORING MARKS ON \"%s\"\n", ~name);
    //abort();
  }

  unsigned Mark::last_id = 0;

  void Marks::to_string(OStream & o) const {
    Vector<const Mark *> mks;
    for (const Marks * cur = this; cur; cur = cur->prev) {
      mks.push_back(cur->mark);
    }
    while (!mks.empty()) {
      const Mark * m = mks.back();
      mks.pop_back();
      o.printf("'%d", m->id);
    }
  }

  void SymbolName::to_string(OStream & o) const {
    o << name;
    if (marks)
      marks->to_string(o);
  }

  void SymbolKey::to_string(OStream & o) const {
    SymbolName::to_string(o);
    if (ns)
      o << "`" << ns->name;
  }

  void TopLevelSymbol::make_unique(SymbolNode * self, SymbolNode * stop) const {
    if (num == NPOS)
      assign_uniq_num<TopLevelSymbol>(self, stop);
  }

  void LexicalSymbol::make_unique(SymbolNode * self, SymbolNode * stop) const {
    assign_uniq_num<LexicalSymbol>(self, stop);
  }

  void OtherSymbol::make_unique(SymbolNode * self, SymbolNode * stop) const {
    if (num == NPOS)
      assign_uniq_num<OtherSymbol>(self, stop);
  }

  void SymbolTable::dump_this_scope() {
    for (SymbolNode * c = front; c != back; c = c->next)
      printf("  %s %p %s %s\n", ~c->key.to_string(), 
             c->value,
             c->value ? ~c->value->name : "", 
             c->value ? ~c->value->uniq_name() : "");
  }

}
