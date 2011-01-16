#ifndef STRING_BUF__HPP
#define STRING_BUF__HPP

#include <limits.h>
#include <stdarg.h>
#include <string.h>

struct StringObj {
  unsigned size;
  char str[0];
};

// NOTE: StringBuf always leaves room for the null character at the
//       end, that is end_ < storage_end_
struct StringBuf {
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
    d = (StringObj *)zl_malloc_atomic(sizeof(StringObj) + size + 1);
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

    char * pbegin() {return begin_;}
    char * pend() {return end_;}

    size_t size() const {return end_ - begin_;}
    bool empty() const {return begin_ == end_;}
  //size_t max_size() const {return INT_MAX;}
    size_t capacity() const {return storage_end_ ? storage_end_ - begin_ - 1 : 0;}

    void ensure_null_end() const {
      if (!begin_) ((StringBuf *)this)->reserve_i();
      *end_ = '\0';
    }

    char * data() {return begin_;}

    char * data(int pos) {return begin_ + pos;}
    char * data_end() {return end_;}

    char & operator[] (size_t pos) {return begin_[pos];}

    char & back() {return end_[-1];}

    void clear() {end_ = begin_;}

    //
    // constructors
    //

    StringBuf() : d(0), begin_(0), end_(0), storage_end_(0) {}
    StringBuf(const char * s) {assign_only(s);}
    StringBuf(const char * s, unsigned size) {assign_only(s, size);}
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

    ~StringBuf() {if (d) zl_free(d);}

  //void swap(StringBuf & other) {
  //    std::swap(d, other.d);
  //    std::swap(begin_, other.begin_);
  //    std::swap(end_, other.end_);
  //    std::swap(storage_end_, other.storage_end_);
  //  }

    //
    //
    //

    int vprintf(const char * format, va_list ap);

      int printf(const char * format, ...)
    {
      va_list ap;
      va_start(ap, format);
      int res = vprintf(format, ap);
      va_end(ap);
      return res;
    }



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
    void write (const char * str) {append(str);}
    void write (const void * str, unsigned int sz) {append(str,sz);}
  
    StringObj * freeze() {
      ensure_null_end();
      StringObj * ret = d;
      ret->size = size();
      zero();
      return ret;
    }

};

StringBuf & operator << (StringBuf & buf, const char * str) {
  buf.append(str);
  return buf;
}

StringBuf & operator << (StringBuf & buf, char c) {
  buf.append(c);
  return buf;
}


//  inline StringBuf operator+ (ParmStr lhs, ParmStr rhs)
//  {
//    StringBuf tmp;
//    tmp.reserve(lhs.size() + rhs.size());
//    tmp += lhs;
//    tmp += rhs;
//    return tmp;
//  }

#undef NPOS
static const unsigned NPOS = UINT_MAX;



#endif
