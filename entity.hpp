#ifndef ENTITY__HPP
#define ENTITY__HPP

#include <stdlib.h>

#include "util.hpp"
#include "gc.hpp"
#include "ostream.hpp"

struct Parse;
struct SourceStr;

class Entity : public gc_cleanup
{
public:
  virtual void print(OStream & o) const {abort();}

  //virtual const SourceStr & source_str() const {abort();}
  //virtual void change_source_str(const SourceStr &) const {abort();}
  // ^^ const is a lie!!!

  // these should really be considerd utility function, which is why
  // they are not virtual, even though they often shadow similar
  // functions in the Parse class
  virtual ~Entity() {}
};



#endif
