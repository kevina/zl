#ifndef SYNTAX_T_HPP
#define SYNTAX_T_HPP

#include "syntax.hpp"

SourceStr get_inner_src(const SourceStr & base, bool first, 
                        parts_iterator i, parts_iterator e);

// Try to find a reasonable span for the syntax which if possible is
// not from syntax generated from a macro
template <typename F>
SourceStr find_qualifying_span(const Syntax * syn, const F & filter_f) {
  // If the outer span qualifies just use it
  SourceStr str = syn->str();
  if (str.defined() && filter_f(str))
    return str;
  // If we don't have any parts to work with, give up
  if (!syn->have_parts())
    return SourceStr();
  // That didn't work, maybe the inner span will be better
  str = syn->get_inner_src();
  if (str.defined() && filter_f(str))
    return str;
  // If we only have one (or fewer parts) give up
  if (syn->num_parts() <= 1)
    return syn->str();
  // That didn't work, eliminate any parts which doesn't qualify and
  // try again
  Vector<Syntax *> parts;
  parts.reserve(syn->num_parts());
  bool used_first = false;
  parts_iterator b = syn->parts_begin(), e = syn->parts_end();
  for (parts_iterator i = b; i != e; ++i) {
    str = find_qualifying_span(*i, filter_f);
    if (str.defined() && filter_f(str)) {
      if (i == b) used_first = true;
      parts.push_back(*i);
    }
  }
  str = get_inner_src(SourceStr(), used_first, parts.pbegin(), parts.pend());
  if (str.defined())
    return str;
  // Nothing works, give up
  return SourceStr();
}

struct NotFromMacro {
  bool operator() (const SourceStr & str) const {
    return str.source && !str.source->block.backtrace;
  }
};

struct ExpandedFromHere {
  const BacktraceInfo * bi;
  ExpandedFromHere(const BacktraceInfo * bi) : bi(bi) {}
  bool operator() (const SourceStr & str) const {
    return str.source && str.source->block.backtrace == bi;
  }
};

#endif
