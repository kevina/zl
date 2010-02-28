// This file is part of The New Aspell
// Copyright (C) 2001-2003 by Kevin Atkinson under the GNU LGPL license
// version 2.0 or 2.1.  You should have received a copy of the LGPL
// license along with this library if you did not you can find
// it at http://www.gnu.org/.

#ifndef ASPELL_VECTOR__HPP
#define ASPELL_VECTOR__HPP

#include <vector>
#include "gc.hpp"

//namespace aspell
//{
  template <typename T>
  class Vector : public std::vector<T, gc_allocator<T> > //, public gc_cleanup
  {
  public:

    // Bring type definitions from the std::vector class to aspell::Vector
    typedef T value_type;
    typedef std::vector<T, gc_allocator<T> > Base;
    typedef typename Base::size_type size_type;
    typedef typename Base::difference_type difference_type;
    typedef typename Base::reference reference;
    typedef typename Base::const_reference const_reference;
    typedef typename Base::iterator iterator;
    typedef typename Base::const_iterator const_iterator;
    typedef typename Base::reverse_iterator reverse_iterator;
    typedef typename Base::const_reverse_iterator const_reverse_iterator;

    Vector() {}
    Vector(unsigned int s) : Base(s) {}
    Vector(unsigned int s, const T & val) : Base(s, val) {}

    template <typename I>
    Vector(I start, I stop) : Base(start, stop) {}

    void append(T t) {
      this->push_back(t);
    }
    void append(const T * begin, unsigned int size) {
      insert(this->end(), begin, begin+size);
    }
    void append(const T * begin, const T * end) {
      insert(this->end(), begin, end);
    }
    int alloc(int s = 1) {
      int pos = this->size();
      this->resize(pos + s);
      return pos;
    }
    T * data() {return &*this->begin();}
    T * data(int pos) {return &*this->begin() + pos;}
    T * data_end() {return &*this->end();}

    T * pbegin() {return &*this->begin();}
    T * pend()   {return &*this->end();}

    const T * pbegin() const {return &*this->begin();}
    const T * pend()   const {return &*this->end();}

    void pop_front() {this->erase(this->begin());}
    void push_front(const T & v) {this->insert(this->begin(), v);}
  };
//}

#endif
