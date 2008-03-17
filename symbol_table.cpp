#include "symbol_table.hpp"

namespace ast {

  void marks_ignored(String name) {
    printf("WARNING: IGNORING MARKS ON \"%s\"\n", ~name);
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

  void SymbolTable::rename(bool if_marked) {
    Vector<SymbolNode *> nodes;
    for (SymbolNode * cur = front; cur != back; cur = cur->next) {
      if (!cur->value) continue;
      nodes.push_back(cur);
    }
    while (!nodes.empty()) {
      SymbolNode * cur = nodes.back();
      nodes.pop_back();
      if (if_marked && !cur->key.marks) continue;
      SymbolNode * p = cur->next;
      for (; p; p = p->next) {
        if (p != cur && p->key.name == cur->key.name) break;
      }
      unsigned num = 1;
      if (p) num = p->value->num + 1;
      cur->value->num = num;
    }
  }

}
