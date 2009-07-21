#ifndef SYNTAX_GATHER__HPP
#define SYNTAX_GATHER__HPP

#include "hash.hpp"

namespace ast {
  struct Mark;
}
struct ReplTable;

struct SyntaxGather {

  struct MarkMap {
    unsigned num;
    hash_map<const void *, unsigned> map_;
    MarkMap() : num(0) {}
    unsigned insert(const ast::Mark * obj) {
      unsigned & res = map_.insert(obj, NPOS).first->second;
      if (res == NPOS)
        res = num++;
      return res;
    }
  };
  
  struct ReplTableMap {
    Vector<String> to_print;
    hash_map<const void *, unsigned> map_;
    std::pair<unsigned, bool> insert(const ReplTable * obj) {
      unsigned & res = map_.insert(obj, NPOS).first->second;
      if (res == NPOS) {
        res = to_print.alloc();
        return std::pair<unsigned, bool>(res, false);
      } else {
        return std::pair<unsigned, bool>(res, true);
      }
    }
    void set_str(unsigned num, String str) {
      to_print[num] = str;
    }
  };

  MarkMap      mark_map;
  ReplTableMap repl_table_map;
};

#endif // SYNTAX_GATHER
