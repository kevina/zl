#ifndef SOURCE_STR__HPP
#define SOURCE_STR__HPP

#include "util.hpp"

class OStream;
class SourceFile;
#define PURE __attribute__ ((pure))

//
//
//

struct Pos {
  String name;
  unsigned line;
  unsigned col;
  Pos() : line(NPOS), col(NPOS) {}
  Pos(String fn, unsigned l, unsigned c) : name(fn), line(l), col(c) {}
  bool defined() const {return !name.empty() && line != NPOS;}
  void clear() {name = String(); line = NPOS; col = NPOS;}
};

//
// A SourceInfo "block" is part of a "file".  The syntax inside a
// macro call or syntax primitive is its own block.  If a piece
// of syntax contains two parts from the same block then the SourceStr
// for that syntax is considered the "bounding box" of its parts.
//

struct SourceBlock;
struct SourceFile;

struct BacktraceInfo {
  enum Action {EXPANSION_OF} const action;
  BacktraceInfo(Action a) : action(a) {}
  void dump_info(StringBuf & res, const char * prefix) const;
};

struct SourceInfo {
  const SourceBlock * block;
  const BacktraceInfo * backtrace;
  SourceInfo(const SourceBlock * b, const BacktraceInfo * bt = NULL) 
    : block(b), backtrace(bt) {}
  inline const SourceFile * file() const;
  inline const BacktraceInfo * origin() const;
};

struct SourceStr : public SubStr {
  const SourceInfo * source;
  const SourceBlock * block() const {return source ? source->block : NULL;}
  const SourceFile * file() const {return source ? source->file() : NULL;}
  SourceStr() : source() {}
  explicit SourceStr(const SourceBlock * f);
  explicit SourceStr(const SourceFile * f);
  SourceStr(const SourceInfo * f, const char * b, const char * e) 
    : SubStr(b,e), source(f) {}
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
    if (block() != other.block()) return;
    if (!begin || begin > other.begin) begin = other.begin;
    if (!end || end < other.end) end = other.end;
  }
  SourceStr & operator=(const char * s) {begin = s; return *this;}
  inline bool pos_str(const char * pre, OStream & o, const char * pos) const;
  void sample_w_loc(OStream & o, unsigned max_len = 20) const;
};

struct SourceBlock {
  const SourceFile * file;
  SourceStr box;
  //const BacktraceInfo * origin;
  SourceInfo base_info;
  SourceBlock(const SourceFile * f, const SourceStr & s) 
    : file(f), box(s), base_info(this) {}
  SourceBlock(SubStr s) 
    : file(NULL), box(NULL, s.begin, s.end), base_info(this) {}
  SourceBlock(const SourceStr & s) 
    : file(s.file()), box(s), base_info(this) {}
private:
  SourceBlock(const SourceBlock &);
  void operator=(const SourceBlock &);
};

class SourceFile : public gc_cleanup {
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
  SourceBlock base_block;
  SourceFile(String file, bool cpm = false) 
    : data_(), size_(0), pp_mode(cpm), internal(false),
      base_block(this, SourceStr())
    {read(file);}
  SourceFile(int fd, bool cpm = false) 
    : data_(), size_(0), pp_mode(cpm), internal(false),
      base_block(this, SourceStr())
    {read(fd);}
  String file_name() const {return file_name_;}
  Pos get_pos(const char * s) const;
  inline void get_pos_str(const char * s, OStream & buf) const;
  unsigned size() const {return size_;}
  const char * begin() const {return data_;}
  const char * end() const {return data_ + size_;}
  ~SourceFile() {if (data_) free(data_);}
private:
  void read(String file);
  void read(int fd);
  SourceFile(const SourceFile &);
  void operator=(const SourceFile &);
};

// XXX: Fix so block is always defined
inline const SourceFile * SourceInfo::file() const {
  return block ? block->file : NULL;
}
inline const BacktraceInfo * SourceInfo::origin() const {
  return block && block->box.source ? block->box.source->backtrace : NULL;
}

inline SourceStr::SourceStr(const SourceBlock * f) 
  : SubStr(f->box.begin, f->box.end), source(&f->base_info) {}
inline SourceStr::SourceStr(const SourceFile * f) 
  : SubStr(f->begin(), f->end()), source(&f->base_block.base_info) {}

//
//
//

void pos_to_str(Pos p, OStream & out);

String add_dir_if_needed(String file, const SourceFile * included_from);

SourceFile * new_source_file(String file, const SourceFile * included_from = NULL, bool pp_mode = false);

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

//
//
//

inline bool SourceStr::pos_str(const char * pre, OStream & o, const char * pos) const {
  return ::pos_str(source, begin, pre, o, pos);
}

inline void SourceFile::get_pos_str(const char * s, OStream & buf) const {
  pos_to_str(get_pos(s), buf);
}

//
// BacktraceInfo members
//

struct Macro;
struct ExpansionOf : public BacktraceInfo {
  ExpansionOf(const Macro * m, const SourceStr & str) 
    : BacktraceInfo(EXPANSION_OF), macro(m), call_site(str)  {}
  const Macro * macro;
  SourceStr call_site;
  inline const BacktraceInfo * parent() const {
    if (!call_site.source) return NULL; // FIXME: Should't be necessary
    return call_site.source->backtrace;
  }
};

#endif
