// Copyright (c) 2001
// Kevin Atkinson
//
// Permission to use, copy, modify, distribute and sell this software
// and its documentation for any purpose is hereby granted without fee,
// provided that the above copyright notice appear in all copies and
// that both that copyright notice and this permission notice appear
// in supporting documentation.  Silicon Graphics makes no
// representations about the suitability of this software for any
// purpose.  It is provided "as is" without express or implied warranty.

// prime list taken from SGI STL with the following copyright

/*
 * Copyright (c) 1996-1998
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */

#ifndef autil__hash_t_hh
#define autil__hash_t_hh

#include <cstdlib>
#include <cstdio>
#include <new>

#include "hash.hpp"
#include "block_slist-t.hpp"

//namespace acommon {
  
  static const unsigned int primes[] =
  {
    53,         97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457, 
    static_cast<unsigned int>(-1)
  };

  template <class P>
  typename HashTable<P>::PrimeIndex HashTable<P>::next_largest(Size s) 
  {
    PrimeIndex i = prime_index_;
    while (assert(primes[i] != static_cast<PrimeIndex>(-1)), primes[i] < s) ++i;
    return i;
  }

  template <class P>
  void HashTable<P>::create_table(PrimeIndex i) {
    prime_index_ = i;
    table_size_ = primes[prime_index_];
    table_ = reinterpret_cast<Node * *>(GC_MALLOC/*_UNCOLLECTABLE*/((table_size_+1) * sizeof(Node *)));
    table_end_ = table_ + table_size_;
    *table_end_ = reinterpret_cast<Node *>(table_end_);
  }

  template <class P>
  void HashTable<P>::init(PrimeIndex i)
  {
    size_ = 0;
    create_table(i);
    node_pool_.add_block(primes[i]);
  }

  template <class P>
  HashTable<P>::HashTable(Node * n, unsigned sz, NodePool & other)
  {
    size_ = 0;
    create_table(next_largest(sz));
    node_pool_.steal(other);
    while (n != 0) {
      Node * * put_me_here = table_ + (parms_.hash(parms_.key(n->data)) % table_size_);
      Node * tmp = n;
      n = n->next;
      tmp->next = *put_me_here;
      *put_me_here = tmp;
      ++size_;
    }
    assert(sz == size_);
    node_pool_.add_block(table_size_ - size_);
  }

  template <class P>
  template <class T>
  std::pair<typename HashTable<P>::iterator,bool> HashTable<P>::insert(const T & to_insert)
  {
    bool have;
    iterator put_me_here = find_i(parms_.key(to_insert), have);
    if (have && !parms_.is_multi) 
      return std::pair<iterator,bool>(put_me_here,false);
    Node * new_node = node_pool_.new_node();
    if (new_node == 0) {
      resize_i(prime_index_+1);
      return insert(to_insert);
    }
    new 
      (const_cast<void *>(reinterpret_cast<const void *>(&new_node->data))) 
      Value(to_insert);
    new_node->next = *put_me_here.n;
    *put_me_here.n = new_node;
    ++size_;
    return std::pair<iterator,bool>(put_me_here,true);
  }
  
  template <class P>
  void HashTable<P>::erase(iterator to_erase) 
  {
    (*to_erase.n)->data.~Value();
    Node * tmp = *to_erase.n;
    *to_erase.n = (*to_erase.n)->next;
    node_pool_.remove_node(tmp);
    --size_;
  }

  template <class P>
  typename HashTable<P>::Size HashTable<P>::erase(const Key & k)
  {
    Size num_erased = 0;
    bool irrelevant;
    Node * * first = find_i(k,irrelevant).n;
    Node * n = *first;
    while (n != 0 && parms_.equal(parms_.key(n->data), k)) {
      Node * tmp = n;
      n->data.~Value();
      n = n->next;
      node_pool_.remove_node(tmp);
      ++num_erased;
    }
    *first = n;
    size_ -= num_erased;
    return num_erased;
  }
  
  template <class P>
  typename HashTable<P>::iterator HashTable<P>::find_i(const Key & to_find, bool & have)
  {
    Size pos = parms_.hash(to_find) % table_size_;
    Node * * n = table_ + pos;
    have = false;
    while (true) {
      if (*n == 0) {
	break;
      } else if (parms_.equal(parms_.key((*n)->data),to_find)) {
	have = true;
	break;
      }
      n = &(*n)->next;
    }
    return iterator(table_ + pos, n);
  }

  template <class P>
  std::pair<typename HashTable<P>::iterator, typename HashTable<P>::iterator>
  HashTable<P>::equal_range_i(const Key & to_find, int & c)
  {
    c = 0;
    bool have;
    iterator first = find_i(to_find,have);
    if (!have)
      return std::pair<iterator,iterator>(end(),end());
    iterator last = first;
    c = 1;
    ++last;
    iterator e = end();
    while (!(last == e) && parms_.equal(parms_.key(*last), to_find)) {
      ++c;
      ++last;
    }
    return std::pair<iterator,iterator>(first,last);
  }

  template <class P>
  void HashTable<P>::del() 
  {
    for (Node * * i = table_; i != table_end_; ++i) {
      Node * n = *i;
      while (n != 0) {
	n->data.~Value();
	n = n->next;
      }
    }
    GC_FREE (table_);
    size_ = 0;
    node_pool_.clear();
    table_ = 0;
    table_size_ = 0;
    prime_index_ = 0;
  }

  template <class P>
  void HashTable<P>::resize_i(PrimeIndex new_prime_index) 
  {
    Node * * old_table = table_;
    Node * * old_end = table_end_;
    Size old_size = table_size_;
    create_table(new_prime_index);
    for (Node * * i = old_table; i != old_end; ++i) {
      Node * n = *i;
      while (n != 0) {
	Node * * put_me_here = table_ + (parms_.hash(parms_.key(n->data)) % table_size_);
	Node * tmp = n;
	n = n->next;
	tmp->next = *put_me_here;
	*put_me_here = tmp;
      }
    }
    GC_FREE(old_table);
    node_pool_.add_block(table_size_ - old_size);
  }

  template <class P>
  void HashTable<P>::copy(const HashTable & other) 
  {
    init(other.prime_index_);
    size_  = other.size_;
    parms_ = other.parms_;
    for (unsigned int i = 0; i != other.table_size_; ++i) {
      for (Node * j = other.table_[i]; j != 0; j = j->next) {
	Node * n = node_pool_.new_node();
	new 
	  (const_cast<void *>(reinterpret_cast<const void *>(&n->data))) 
	  Value(j->data);
	n->next = table_[i];
	table_[i] = n;
      }
    }
  }

  template <class P>
  void HashTable<P>::dump_stats() {
    using std::printf;
    printf("LOAD: %f  SIZE %u:  BUCKETS: %u\n", (double)size_ / (double)table_size_, size_, table_size_);
    Vector<unsigned> tally;
    Vector<unsigned> tally2;
    Vector<const Key *> uniq;
    Vector<std::pair<unsigned long, unsigned> > gather;
    for (Node * * i = table_; i != table_end_; ++i) {
      unsigned c = 0;
      Node * n = *i;
      gather.clear();
      uniq.clear();
      //printf("---\n");
      while (n) {
        ++c;
        unsigned j = 0;
        const Key & k = parms_.key(n->data);
        while (j != uniq.size() && !(*uniq[j] == k)) ++j;
        if (j == uniq.size()) {
          uniq.push_back(&k);
          j = 0;
          unsigned h = parms_.hash(k);
          while (j != gather.size() && gather[j].first != h) ++j;
          if (j == gather.size()) gather.resize(j+1);
          gather[j].first = h;
          gather[j].second++;
        }
        n = n->next;
        //printf("  %u %u\n", h, j);
      }
      if (tally.size() <= c) tally.resize(c+1);
      tally[c]++;
      for (unsigned j = 0; j != gather.size(); ++j) {
        unsigned c = gather[j].second;
        if (tally2.size() <= c) tally2.resize(c+1);
        tally2[c]++;
      }
      if (c > 16) {
        n = *i;
        while (n) {
          const Key & k = parms_.key(n->data);
          printf("?? %lu %p %p\n", parms_.hash(k), k.prod, k.str);
          n = n->next;
        }
        printf("---\n");
      }
    }
    for (unsigned i = 0; i != tally.size(); ++i)
      printf(" -- %u: %u (%f)\n", i, tally[i], (double)tally[i]/(double)table_size_);
    for (unsigned i = 0; i != tally2.size(); ++i)
      printf(" == %u: %u (%f)\n", i, tally2[i], (double)tally2[i]/(double)table_size_);
  }

  template <typename H, unsigned INIT_SZ, unsigned MAX_SZ>
  template <class T>
  std::pair<typename tiny_hash<H,INIT_SZ,MAX_SZ>::value_type *,bool> tiny_hash<H,INIT_SZ,MAX_SZ>::insert_i(const T & to_insert)
  {
    unsigned sz = 0;
    if (!first) node_pool_.add_block(INIT_SZ);
    Node * * n = &first;
    for (; *n; n = &(*n)->next) {
      if (typename H::Parms().equal(H::Parms::key((*n)->data),typename H::Parms().key(to_insert))) 
        return std::pair<value_type *,bool>(&(*n)->data, false);
      ++sz;
    }
    Node * new_node = node_pool_.new_node();
    if (new_node == 0) {
      if (sz * 2 <= MAX_SZ) {
        node_pool_.add_block(sz); // effectively doubling the size
        new_node = node_pool_.new_node();
      } else {
        hash = new H(first, sz, node_pool_);
        return insert(to_insert);
      }
    }
    new 
      (const_cast<void *>(reinterpret_cast<const void *>(&new_node->data))) 
      typename H::Value(to_insert);
    *n = new_node;
    new_node->next = NULL;
    return std::pair<value_type *,bool>(&new_node->data,true);
  }

  template <typename H, unsigned INIT_SZ, unsigned MAX_SZ>
  void tiny_hash<H,INIT_SZ,MAX_SZ>::dump_stats() {
    if (hash) {
      hash->dump_stats();
    } else {
      unsigned c = 0;
      for (Node * n = first; n; n = n->next) {
        ++c;
      }
      printf("TINY HASH: %u\n", c);
    }
  }

//}


#endif
