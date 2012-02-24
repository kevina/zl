#ifndef SOURCE_STR__HPP
#define SOURCE_STR__HPP

#include "util.hpp"

struct Pos {
  String name;
  unsigned line;
  unsigned col;
  Pos() : line(NPOS), col(NPOS) {}
  Pos(String fn, unsigned l, unsigned c) : name(fn), line(l), col(c) {}
  bool defined() const {return !name.empty() && line != NPOS;}
  void clear() {name = String(); line = NPOS; col = NPOS;}
};

void pos_to_str(Pos p, OStream & out);

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
public: // but don't use
  struct SourceChange {const char * idx; String file_name; unsigned lineno;};
private:
  String file_name_;
  char * data_;
  unsigned size_;
  bool pp_mode; // pp = preprocess
  Vector<SourceChange> source_change;
  Vector<const char *> lines_;
public:
  bool internal;
  SourceFile(String file, bool cpm = false) : data_(), size_(0), pp_mode(cpm), internal(false) {read(file);}
  SourceFile(int fd, bool cpm = false) : data_(), size_(0), pp_mode(cpm), internal(false) {read(fd);}
  String file_name() const {return file_name_;}
  Pos get_pos(const char * s) const;
  void get_pos_str(const char * s, OStream & buf) const {
    pos_to_str(get_pos(s), buf);
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

SourceFile * new_source_file(String file, const SourceInfo * included_from = NULL, bool pp_mode = false);

SourceFile * new_source_file(int fd, bool pp_mode = false);

bool pos_str(const SourceFile * source, const char * pos,
             const char * pre, OStream & o, const char * post);

static inline bool pos_str(const SourceInfo * source, const char * pos,
                           const char * pre, OStream & o, const char * post)
{
  return pos_str(source ? source->file() : NULL, pos, pre, o, post);
}

String sample(const char * begin, const char * end, unsigned max_len = 20);
static inline String sample(String str, unsigned max_len = 20) {
  return sample(str.begin(), str.end(), max_len);
}

struct SourceStr : public SubStr {
  const SourceInfo * source;
  const SourceInfo * source_block() const {return source ? source->block() : NULL;}
  SourceStr() : source() {}
  SourceStr(const char * b, const char * e) : SubStr(b,e), source() {}
  SourceStr(String s) : SubStr(s), source() {}
  SourceStr(const SourceInfo * f) : SubStr(f->begin(), f->end()), source(f) {}
  SourceStr(const SourceInfo * f, const char * b, const char * e) 
    : SubStr(b,e), source(f) {}
  SourceStr(const SourceInfo * f, String s) 
    : SubStr(s), source(f) {}
  SourceStr(const SourceInfo * f, SubStr s) 
    : SubStr(s), source(f) {}
  SourceStr(const SourceStr & other, const char * e)
    : SubStr(other.begin, e), source(other.source) {}
  operator const char * & () {return begin;}
  operator const char * () const {return begin;}
  void clear() {
    SubStr::clear();
    source = 0;
  }
  void assign(const char * str, const SourceStr & end) {
    SubStr::assign(str, end.begin);
    source = end.source;
  }
  void adj(const SourceStr & other) {
    if (!source && other.source) source = other.source;
    if (source_block() != other.source_block()) return;
    if (!begin || begin > other.begin) begin = other.begin;
    if (!end || end < other.end) end = other.end;
  }
  SourceStr & operator=(const char * s) {begin = s; return *this;}
  bool pos_str(const char * pre, OStream & o, const char * pos) const {
    return ::pos_str(source, begin, pre, o, pos);
  }
  void sample_w_loc(OStream & o, unsigned max_len = 20) const;
};

#endif
