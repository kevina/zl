#include "symbol_table.hpp"

namespace ast {

  void marks_ignored(String name) {
    printf("WARNING: IGNORING MARKS ON \"%s\"\n", ~name);
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

}
