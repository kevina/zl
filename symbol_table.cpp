#include "symbol_table.hpp"
#include "syntax_gather.hpp"
#include "environ.hpp"

namespace ast {

  Overloadable Overloadable::AS_ID(Overloadable::SELF);

  void Overloadable::to_string(StringBuf & buf) const {
    if (data == &AS_ID) {
      buf << "as id";
    } else if (const Tuple * t = as_tuple()) {
      buf << "with parms: ";
      t->to_string(buf);
    } else {
      buf << "<nil>";
    }
  }

  
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
  InnerNS::Tag HIDDEN_NS_OBJ;
  InnerNS::Tag MACRO_EXPORT_NS_OBJ;

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
  const InnerNS * const HIDDEN_NS = &HIDDEN_NS_OBJ;
  const InnerNS * const MACRO_EXPORT_NS = &MACRO_EXPORT_NS_OBJ;

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
    env.add_internal(SymbolKey("hidden", INNER_NS), &HIDDEN_NS_OBJ);
    env.add_internal(SymbolKey("macro_export", MACRO_EXPORT_NS), &MACRO_EXPORT_NS_OBJ);
  }

  void marks_ignored(String name) {
    fprintf(stderr, "WARNING: IGNORING MARKS ON \"%s\"\n", ~name);
    //abort();
  }

  unsigned Mark::last_id = 0;

  const Marks * add_mark(const Marks * ms, const Mark * m) {

    // check if cached
    for (Mark::Cache::iterator i = m->cache.begin(), e = m->cache.end(); i != e; ++i) 
      if (i->first == ms) return i->second;

    // create new mark
    unsigned num_marks = ms ? ms->num_marks + 1 : 1;
    Marks * nms = (Marks *)GC_MALLOC(sizeof(Marks) + sizeof(void *)*num_marks);
    nms->num_marks = num_marks;
    nms->prev = ms;
    unsigned i = 0;
    if (ms) {
      for (;i != ms->num_marks; ++i)
        nms->marks[i] = ms->marks[i];
    }
    nms->marks[i] = m;
    nms->normalized = add_mark_normalized(ms, m, nms);

    // return result
    m->cache.push_back(Mark::CacheNode(ms, nms));
    return nms;
  }

  const NMarks * add_mark_normalized(const Marks * ms, const Mark * m, 
                                     const Marks * nms /* new mark, not normalized */) 
  {
    // create normalized mark
    const Marks * n0 = ms ? ms->normalized : NULL;
    unsigned base_id = m->base_mark->id;
    const Marks * normalized = NULL;
    if (n0 && !(n0->back()->id < base_id)) {
      fprintf(stderr, "OUT OF ORDER!\n");
      // out of order, rebuild mark with new mark in correct location
      unsigned i = 0;
      for (;i < n0->num_marks && n0->marks[i]->id < base_id; ++i) 
        normalized = add_mark(normalized,  n0->marks[i]);
      assert(i != n0->num_marks);
      if (n0->marks[i] != m->base_mark)
        normalized = add_mark(normalized, m->base_mark);
      for (; i < n0->num_marks; ++i)
        normalized = add_mark(normalized,  n0->marks[i]);
    } else if (!nms || m != m->base_mark) {
      // in order but need new mark
      normalized = add_mark(n0, m->base_mark);
    } else {
      normalized = nms;
    }
    return static_cast<const NMarks *>(normalized);
  }

  const Marks * merge_marks(const NMarks * orig, 
                            const NMarks * to_prune,
                            const NMarks * to_add)
  {
    Marks::iterator o = NULL, oe = NULL;
    if (orig) o = orig->begin(), oe = orig->end();
    Marks::iterator p = NULL, pe = NULL;
    if (to_prune) p = to_prune->begin(), pe = to_prune->end();
    Marks::iterator a = NULL, ae = NULL;
    if (to_add) a = to_add->begin(), ae = to_add->end();
    const Marks * nms = 0;
    for (;;) {
      // advance o past any marks in p
      while (o != oe && p != pe && *o == *p)
        ++o, ++p;
      if (o == oe && a == ae) break;
      const Mark * m;
      if      (a == ae)              m = *o++;
      else if (o == oe)              m = *a++;
      else if ((*o)->id == (*a)->id) {++o; ++a; continue;}
      else if ((*o)->id < (*a)->id)  m = *o++;
      else                           m = *a++;
      nms = add_mark(nms, m);
    }
    if (nms)
      assert(nms == nms->normalized);
    return nms;
  }

  const Marks * fix_up_marks(const Marks * orig,
                             const Mark * fixup) 
  {
    if (!orig->pop()) return fixup->export_to;

    fprintf(stderr, "NON SIMPLE CASE\n");
    for (Mark::Cache::iterator 
           i = fixup->fixup_cache.begin(), e = fixup->fixup_cache.end(); i != e; ++i) 
      if (i->first == orig) return i->second;
    
    const Marks * nms = merge_marks(normalize(orig->pop()), fixup->also_allow, fixup->export_to);

    fixup->fixup_cache.push_back(Mark::CacheNode(orig, nms));
    
    return nms;
  }

  void Marks::to_string(OStream & o, SyntaxGather * g) const {
    for (unsigned i = 0; i != num_marks; ++i) {
      const Mark * m = marks[i];
      if (g) {
        o.printf("'%u", g->mark_map.insert(m));
      } else {
        o.printf("'%u", m->id);
      }
    }
  }

  void Mark::dump(OStream & o) const {
    o << id;
    if (base_mark != this) o << "(" << base_mark->id << ")";
    if (export_tl) {
      o << "[export_tl";
      if (export_to) {o << "=>";  export_to->to_string(o, NULL);}
      if (also_allow) {o << "-";  also_allow->to_string(o, NULL);}
      o << "]";
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

  void dump_symbol_node(const SymbolNode * c, bool verbose) {
    printf("  %s %p %s %s", ~c->key.to_string(), 
           c->value,
           c->value ? ~c->value->name() : "", 
           c->value ? ~c->value->uniq_name() : "");
    if (verbose) {
      unsigned flags = c->flags;
      if (flags == 0) printf(" NO_FLAGS");
      if (flags & SymbolNode::ALIAS) printf(" ALIAS");
      if (flags & SymbolNode::IMPORTED) printf(" IMPORTED");
      if (flags & SymbolNode::DIFF_SCOPE) printf(" DIFF_SCOPE");
      if (flags & SymbolNode::INTERNAL) printf(" INTERNAL");
    }
    printf("\n");
  }

  void dump_symbols(const SymbolNode * start, const SymbolNode * back, 
                    const SymbolNode * stop) 
  {
    printf("=== BEGIN SYMBOL TABLE ===\n");
    bool verbose = true;
    //printf("%p %p <> %p %p\n", front, back, &front, ip.front);
    for (const SymbolNode * c = start; c != stop; c = c->next) {
      if (c == back) {
        printf("--- end current scope ---\n");
        verbose = false;
      }
      dump_symbol_node(c, verbose);
    }
    printf("^^^ END SYMBOL TABLE ^^^\n");
  }

  void SymbolTableBase::dump() const {
    dump_symbols(front, back, NULL);
  }

  void SymbolTableBase::dump_this_scope() const {
    dump_symbols(front, back, back);
  }

  void Props::add_prop(SymbolName n, const Syntax * s) {
    props = new PropNode(n, s, props);
  }

  const Syntax * Props::get_prop(SymbolName n) const {
    for (PropNode * cur = props; cur; cur = cur->next) {
      if (n == cur->name) return cur->value;
    }
    if (n.marks) {
      n.marks = n.marks->pop();
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
    unsigned prev_num = 0;
    String name = sym->name();
    const InnerNS * ns = sym->tl_namespace();
    for (; cur != stop; cur = cur->next) {
      if (!cur->alias() && cur->value && 
          cur->value->name() == name && 
          (t = dynamic_cast<const TopLevelSymbol *>(cur->value)) && 
          t != sym && t->num != NPOS && t->num > prev_num &&
          t->tl_namespace() == ns) 
        prev_num = t->num;
      t = NULL;
    }
    unsigned num = prev_num + 1;
    assign_uniq_num(num, sym);
  }

}

extern "C" namespace macro_abi {
  using namespace ast;

  unsigned symbol_num(const Symbol * sym) {
    if (const TopLevelSymbol * tl_sym = dynamic_cast<const TopLevelSymbol *>(sym)) {
      return tl_sym->num;
    } else {
      return 0;
    }
  }

  Symbol * symbol_where(const Symbol * sym) {
    const TopLevelSymbol * tl_sym = dynamic_cast<const TopLevelSymbol *>(sym);
    if (!tl_sym) return NULL;
    return tl_sym->where;
  }

  bool symbol_asm_hidden(const Symbol * sym) {
    const TopLevelSymbol * tl_sym = dynamic_cast<const TopLevelSymbol *>(sym);
    if (!tl_sym) return false;
    return tl_sym->asm_hidden;
  }

  const char * symbol_name(const Symbol * sym) {
    return sym->name();
  }

  const char * symbol_uniq_name(const Symbol * sym) {
    return sym->uniq_name();
  }

  Syntax * symbol_get_prop(const Symbol * sym, const Syntax * prop) {
    if (prop->is_a("id")) prop = prop->arg(0);
    return sym->get_prop(*prop);
  }

}

