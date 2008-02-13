#ifndef STRING_BUF__HPP
#define STRING_BUF__HPP

#include "util.hpp"

//#include "hash_fun.hpp"
#include "parm_string.hpp"
#include "ostream.hpp"
#include "istream.hpp"

struct StringBuf : public OStream {
public:
  typedef const char * const_iterator;
  typedef char *       iterator;
  typedef unsigned     size_type;
private:
  StringObj * d;
  char * begin_;
  char * end_;
  char * storage_end_;  

  void assign_only_nonnull(const char * b, unsigned size)
  {
    d = (StringObj *)GC_MALLOC_ATOMIC(sizeof(StringObj) + size + 1);
    d->size = 0;
    begin_ = d->str;
    memmove(begin_, b, size);
    end_   = begin_ + size;
    storage_end_ = end_ + 1;
  }
  void zero()
  {
    d = 0;
    begin_ = 0;
    end_ = 0;
    storage_end_ = 0;
  }
  void assign_only(const char * b)
  {
    if (b && *b) assign_only_nonnull(b, strlen(b));
    else zero();
  }
  void assign_only(const char * b, unsigned size)
  {
    if (b && size > 0) assign_only_nonnull(b, size);
    else zero();
  }
  void reserve_i(size_t s = 0);

public:
    void reserve(size_t s)
    {
      if (storage_end_ - begin_ >= (int)s + 1) return;
      reserve_i(s);
    }

    char * begin() {return begin_;}
    char * end() {return end_;}

    const char * begin() const {return begin_;}
    const char * end()   const {return end_;}

    char * pbegin() {return begin_;}
    char * pend() {return end_;}

    const char * pbegin() const {return begin_;}
    const char * pend()   const {return end_;}

    size_t size() const {return end_ - begin_;}
    bool empty() const {return begin_ == end_;}
    size_t max_size() const {return INT_MAX;}
    size_t capacity() const {return storage_end_ ? storage_end_ - begin_ - 1 : 0;}

    void ensure_null_end() const {
      if (!begin_) const_cast<StringBuf *>(this)->reserve_i();
      *end_ = '\0';
    }

    char * data() {return begin_;}
    const char * data() const {return begin_;}

    char * data(int pos) {return begin_ + pos;}
    char * data_end() {return end_;}

    template <typename U>
    U * datap() {
      return reinterpret_cast<U * >(begin_);
    }
    template <typename U>
    U * datap(int pos) {
      return reinterpret_cast<U * >(begin_ + pos);
    }

    char & operator[] (size_t pos) {return begin_[pos];}
    char operator[] (size_t pos) const {return begin_[pos];}

    char & back() {return end_[-1];}
    char back() const {return end_[-1];}

    void clear() {end_ = begin_;}

    //
    // constructors
    //

    StringBuf() : d(0), begin_(0), end_(0), storage_end_(0) {}
    StringBuf(const char * s) {assign_only(s);}
    StringBuf(const char * s, unsigned size) {assign_only(s, size);}
    StringBuf(ParmStr s) {assign_only(s, s.size());}
    StringBuf(String s) {assign_only(s, s.size());}
    StringBuf(SubStr s) {assign_only(s, s.size());}
    StringBuf(const StringBuf & other) {assign_only(other.begin_, other.end_-other.begin_);}

    //
    // assign
    //

    void assign(const char * b, size_t size)
    {
      clear();
      if (size != 0) {
        reserve(size);
        memmove(begin_, b, size);
        end_   = begin_ + size;
      }
    }
    void assign(const char * b)
    {
      if (b) assign(b, strlen(b));
    }
    StringBuf & operator= (const char * s) {
      assign(s);
      return *this;
    }
    StringBuf & operator= (ParmStr s) {
      assign(s, s.size());
      return *this;
    }
    StringBuf & operator= (String s) {
      assign(s, s.size());
      return *this;
    }
    StringBuf & operator= (SubStr s) {
      assign(s, s.size());
      return *this;
    }
    StringBuf & operator= (const StringBuf & s) {
      assign(s.begin_, s.end_ - s.begin_);
      return *this;
    }

    //
    // append
    //

    StringBuf & append(const void * str, unsigned int sz)
    {
      reserve(size() + sz);
      if (sz > 0) memcpy(end_, str, sz);
      end_ += sz;
      return *this;
    }
    StringBuf & append(const void * d, const void * e)
    {
      append(d, (const char *)e - (const char *)d);
      return *this;
    }
    StringBuf & append(StringBuf & str, unsigned int sz)
    {
      append(str.begin_, sz);
      return *this;
    }
    StringBuf & append(const char * str)
    {
      if (!end_) reserve_i();
      for (; *str && end_ != storage_end_ - 1; ++str, ++end_)
        *end_ = *str;
      if (end_ == storage_end_ - 1) append(str, strlen(str));
      return *this;
    }
    StringBuf & append(char c)
    {
      reserve(size() + 1);
      *end_ = c;
      ++end_;
      return *this;
    }

    StringBuf & operator+= (const char * s) {
      append(s);
      return *this;
    }
    StringBuf & operator+= (char c) {
      append(c);
      return *this;
    }
    StringBuf & operator+= (ParmStr s) {
      if (s.have_size())
        append(s, s.size());
      else
        append(s);
      return *this;
    }
    StringBuf & operator+= (String s) {
      append(s, s.size());
      return *this;
    }
    StringBuf & operator+= (SubStr s) {
      append(s, s.size());
      return *this;
    }
    StringBuf & operator+= (const StringBuf & s) {
      append(s.begin_, s.end_ - s.begin_);
      return *this;
    }

    //
    // prepend
    //

    StringBuf & prepend(const char * str)
    {
      insert(0, str, strlen(str));
      return *this;
    }

    //
    //
    //

    ~StringBuf() {if (d) GC_FREE(d);}

    void swap(StringBuf & other) {
      std::swap(d, other.d);
      std::swap(begin_, other.begin_);
      std::swap(end_, other.end_);
      std::swap(storage_end_, other.storage_end_);
    }

    //
    //
    //

    int vprintf(const char * format, va_list ap);

    //
    //
    //

    void push_back(char c) {append(c);}

    void pop_back(size_t p = 1) {end_ -= p;}

    char * insert(size_t p, char c)
    {
      reserve(size() + 1);
      char * pos = begin_ + p;
      size_t to_move = end_ - pos;
      if (to_move) memmove(pos + 1, pos, to_move);
      *pos = c;
      ++end_;
      return pos;
    }
    char * insert(char * pos, char c)
    {
      return insert(pos - begin_, c);
    }
    void insert(size_t p, const char * str, size_t sz)
    {
      reserve(size() + sz);
      char * pos = begin_ + p;
      size_t to_move = end_ - pos;
      if (to_move) memmove(pos + sz, pos, to_move);
      memcpy(pos, str, sz);
      end_ += sz;
    }
    void insert(char * pos, const char * f, const char * l)
    {
      insert(pos - begin_, f, l - f);
    }

    char * erase(char * pos)
    {
      size_t to_move = end_ - pos - 1;
      if (to_move) memmove(pos, pos + 1, to_move);
      --end_;
      return pos;
    }
    char * erase(char * f, char * l)
    {
      if (l >= end_) {
        end_ = f < end_ ? f : end_;
      } else {
        size_t sz = l - f;
        memmove(f, f + sz, end_ - l);
        end_ -= sz;
      }
      return f;
    }
    void erase(size_t pos, size_t s)
    {
      erase(begin_ + pos, begin_ + pos + s);
    }

    //FIXME: Make this more efficent by rewriting the implemenation
    //       to work with raw memory rather than using vector<char>
    template <typename Itr>
    void replace(iterator start, iterator stop, Itr rstart, Itr rstop)
    {
      iterator i = erase(start,stop);
      insert(i, rstart, rstop);
    }

    void replace(size_t pos, size_t n, const char * with, size_t s)
    {
      replace(begin_ + pos, begin_ + pos + n, with, with + s);
    }
    void resize(size_t n)
    {
      reserve(n);
      end_ = begin_ + n;
    }
    void resize(size_t n, char c)
    {
      size_t old_size = size();
      reserve(n);
      end_ = begin_ + n;
      int diff = n - old_size;
      if (diff > 0) memset(begin_ + old_size, c, diff);
    }
    int alloc(int s) {
      int pos = size();
      resize(pos + s);
      return pos;
    }

    void write (char c) {append(c);}
    void write (ParmStr str) {operator+=(str);}
    void write (const void * str, unsigned int sz) {append(str,sz);}

    String freeze() {
      ensure_null_end();
      StringObj * ret = d;
      ret->size = size();
      zero();
      return ret;
    }

    StringBuf & operator << (ParmStr str) {
      append(str);
      return *this;
    }

    StringBuf & operator << (char c) {
      append(c);
      return *this;
    }
};

  inline StringBuf operator+ (ParmStr lhs, ParmStr rhs)
  {
    StringBuf tmp;
    tmp.reserve(lhs.size() + rhs.size());
    tmp += lhs;
    tmp += rhs;
    return tmp;
  }

  //inline ParmString::ParmString(const StringBuf & s) : str_(s.c_str()), size_(s.size()) {}

  class StringIStream : public IStream {
    const char * in_str;
    char         delem;
  public:
    StringIStream(ParmStr s, char d = ';')
      : IStream(d), in_str(s) {}
    bool append_line(StringBuf & str, char c);
    bool read(void * data, unsigned int size);
  };

  //template <> struct hash<String> : public HashString<String> {};

  inline bool IStream::getline(StringBuf & str, char c)
  {
    str.clear();
    return append_line(str,c);
  }

  inline bool IStream::getline(StringBuf & str)
  {
    str.clear();
    return append_line(str,delem);
  }



namespace std
{
  template<> inline void swap(StringBuf & x, StringBuf & y) {return x.swap(y);}
}



#endif
