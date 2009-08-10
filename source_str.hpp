#ifndef SOURCE_STR__HPP
#define SOURCE_STR__HPP

bool pos_str(const SourceFile * source, const char * pos,
             const char * pre, OStream & o, const char * post,
             bool w_source = true);

static inline bool pos_str(const SourceInfo * source, const char * pos,
                           const char * pre, OStream & o, const char * post,
                           bool w_source = true) 
{
  return pos_str(source ? source->file() : NULL, pos, pre, o, post, w_source);
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
  bool end_pos_str(const char * pre, OStream & o, const char * pos) const {
    return ::pos_str(source, end, pre, o, pos, false);
  }
  void sample_w_loc(OStream & o, unsigned max_len = 20) const;
};

#endif
