#ifndef UTIL__HPP
#define UTIL__HPP

#include <assert.h>
#include <stdlib.h>

#include "gc.hpp"
#include "vector.hpp"
#include "parm_string.hpp"
#include "syntax-f.hpp"

#include "hash.hpp"
#include "iostream.hpp"

//String sample(const char * begin, const char * end, unsigned max_len);

struct StringObj {
  unsigned size;
  char str[];
};

extern const struct StringObj1 EMPTY_STRING_OBJ1;

static const StringObj * const EMPTY_STRING_OBJ =
  reinterpret_cast<const StringObj *>(&EMPTY_STRING_OBJ1);

struct SubStr;

struct String {
private:
  const StringObj * d;
public:
  typedef const char * iterator;
  typedef const char * const_iterator;

  //void dump_if_large() {
  //  if (size() > 256)
  //    printf("LARGE STR (%d): %s\n", size(), ~sample(begin(), end(), 64));
  //}

  void assign(const char * str, unsigned sz) {
    StringObj * d0 = (StringObj *)GC_MALLOC_ATOMIC(sizeof(StringObj) + sz + 1);
    d0->size = sz;
    memmove(d0->str, str, sz);
    d0->str[sz] = '\0';
    d = d0;
    //dump_if_large();
  }
  void assign(const char * str, const char * e) {
    assign(str, e - str);
  }
  inline void assign(const SubStr & str);
  void assign(const char * str) {assign(str, strlen(str));}
  String() : d(EMPTY_STRING_OBJ) {}
  String(const SubStr & str) {assign(str);}
  String(const char * str) {assign(str);}
  String(const StringObj * str) : d(str) {assert(str); /*dump_if_large();*/}
  String(const char * str, const char * e) {assign(str, e);}
  bool defined() const {return d != EMPTY_STRING_OBJ;}
  unsigned size() const {return d->size;}
  bool empty() const {return d->size == 0;}
  const char * begin() const {return d->str;}
  const char * end()   const {return d->str + d->size;}
  const char * data() const {return d->str;}
  const char * pbegin() const {return d->str;}
  const char * pend()   const {return d->str + d->size;}
  char operator[] (unsigned sz) const {return d->str[sz];}
  char operator[] (int sz)      const {return d->str[sz];}
  operator const char * () const {return d->str;}
  const char * c_str() const {return d->str;}
  const char * operator~() const {return d->str;}
};

inline ParmString::ParmString(String s) 
  : str_(s), size_(s.size()) {}

struct SubStr {
  const char * begin;
  const char * end;
  SubStr() 
    : begin(0), end(0) {}
  SubStr(const char * b, const char * e) : begin(b), end(e) {}
  SubStr(String s) : begin(s.pbegin()), end(s.pend()) {}
  //explicit SubStr(const string & s) : begin(s.c_str()), end(begin + s.size()) {}
  void clear() {begin = end = 0;}
  void assign(const char * b, const char * e) {begin = b; end = e;}
  bool empty() const {return begin == end;}
  unsigned size() const {return end - begin;}
  operator const char * () {return begin;}
  SubStr & operator=(const char * s) {begin = s; return *this;}
  String to_string() {return String(begin,end);}
};

inline void String::assign(const SubStr & str) {assign(str.begin, str.size());}

int cmp(const char * x, unsigned x_sz, const char * y, unsigned y_sz);

#define OP_CMP(T1, T2) \
  static inline bool operator==(T1 x, T2 y) {\
    if (x.size() != y.size()) return false;\
    return memcmp(x, y, x.size()) == 0;\
  }\
  static inline bool operator!=(T1 x, T2 y) {\
    if (x.size() != y.size()) return true;\
    return memcmp(x, y, x.size()) != 0;\
  }\
  static inline int cmp(T1 x, T2 y) {\
    return cmp(x, x.size(), y, y.size()); \
  }\
  static inline bool operator<(T1 x, T2 y) {\
    return cmp(x, x.size(), y, y.size()) < 0;\
  }
OP_CMP(String, String);
OP_CMP(SubStr, SubStr);
OP_CMP(String, SubStr);
OP_CMP(SubStr, String);

#define CSTR_OP_CMP(T1, T2) \
  static inline bool operator==(T1 x, T2 y) {return strcmp(x, y) == 0;}\
  static inline bool operator!=(T1 x, T2 y) {return strcmp(x, y) != 0;}\
  static inline int cmp(T1 x, T2 y) {return strcmp(x,y);} \
  static inline bool operator<(T1 x, T2 y) {return strcmp(x,y) < 0;}\
  
CSTR_OP_CMP(String, const char *);
CSTR_OP_CMP(const char *, String);

static inline bool operator==(SubStr x, const char * y) {
  int res = strncmp(x.begin, y, x.size());
  if (res != 0) return false;
  return y[x.size()] == '\0'; // we know y is as least as long as x
                              // otherwise strncmp will return
                              // non-zero
}

struct Pos {
  unsigned line;
  unsigned col;
  Pos(unsigned l, unsigned c) : line(l), col(c) {}
};

char * pos_to_str(Pos p, char * buf);

class OStream;
class SourceFile;

#define PURE __attribute__ ((pure))

//
// A SourceInfo "block" is part of a "file".  The syntax inside a
// macro call or syntax primitive is its own block.  If a piece
// of syntax contains two parts from the same block then the SourceStr
// for that syntax is considered the "bounding box" of its parts.
//
class SourceInfo {
public:
  const SourceInfo * parent;
  SourceInfo(const SourceInfo * p = NULL) : parent(p) {}
  String file_name() const;
  Pos get_pos(const char * s) const;
  unsigned size() const;
  const char * begin() const;
  const char * end() const;    
  PURE virtual const SourceFile * file() const  {return parent ? parent->file() : NULL;}
  PURE virtual const SourceInfo * block() const {return parent ? parent->block() : NULL;}
  virtual const SourceInfo * find_insertion_point(const Syntax * outer) const 
    { //printf("esi find i point (parent = %p)\n", parent);
      //dump_info(COUT, "  ");
      return parent ? parent->find_insertion_point(outer) : NULL;}
  // clone_until: clone until (and including) stop, if we are the same
  //   as stop then when cloning set the parent to new_parent
  virtual const SourceInfo * clone_until(const SourceInfo * stop, 
                                         const SourceInfo * new_parent) const {abort();}
  typedef hash_set<const SourceInfo *> AlreadySeen;
  virtual bool dump_info_self(OStream &) const = 0;
  virtual void dump_info(OStream & o, AlreadySeen & as, const char * prefix) const;
  void dump_info(OStream & o, const char * prefix="") const;
  virtual ~SourceInfo() {}
};

class SourceFile : public SourceInfo, public gc_cleanup {
private:
  String file_name_;
  char * data_;
  unsigned size_;
  Vector<const char *> lines_;
public:
  SourceFile(String file) : data_(), size_(0) {read(file);}
  SourceFile(int fd) : data_(), size_(0) {read(fd);}
  String file_name() const {return file_name_;}
  Pos get_pos(const char * s) const;
  char * get_pos_str(const char * s, char * buf) const {
    return pos_to_str(get_pos(s), buf);
  }
  unsigned size() const {return size_;}
  const char * begin() const {return data_;}
  const char * end() const {return data_ + size_;}
  ~SourceFile() {if (data_) free(data_);}
  const SourceFile * file() const {return this;}
  const SourceInfo * block() const {return this;}
  bool dump_info_self(OStream & o) const;
private:
  void read(String file);
  void read(int fd);
};

inline String SourceInfo::file_name() const {return file()->file_name();}
inline Pos SourceInfo::get_pos(const char * s) const {return file()->get_pos(s);}
inline unsigned SourceInfo::size() const {return file()->size();}
inline const char * SourceInfo::begin() const {return file()->begin();}
inline const char * SourceInfo::end() const {return file()->end();}

String add_dir_if_needed(String file, const SourceInfo * included_from);

SourceFile * new_source_file(String file, const SourceInfo * included_from = NULL);

SourceFile * new_source_file(int fd);

#undef NPOS
static const unsigned NPOS = UINT_MAX;

#endif
