#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#include "util.hpp"
#include "source_str.hpp"
#include "iostream.hpp"
#include "string_buf.hpp"
#include "hash-t.hpp"
#include "parse_common.hpp"

#include <algorithm>

//
//
//

int cmp(const char * x, unsigned x_sz, const char * y, unsigned y_sz) {
  if (x_sz == y_sz) {
      return memcmp(x, y, x_sz);
  } else if (x_sz < y_sz) {
    int res = memcmp(x, y, x_sz);
    if (res == 0) return -1;
    return res;
  } else {
    int res = memcmp(x, y, y_sz);
    if (res == 0) return 1;
    return res;
  }
}

//
//
//

// void SourceInfo::dump_info(OStream & o, AlreadySeen & as, const char * prefix) const {
//   bool seen_self = as.have(this);
//   as.insert(this);
//   StringBuf buf;
//   bool dump_parents = dump_info_self(buf);
//   String str = buf.freeze();
//   if (!str.empty()) {
//     if (seen_self) 
//       o << prefix << "[" << str << "]\n";
//     else
//       o << prefix << str << "\n";
//   }
//   if (parent && !seen_self && dump_parents)
//     parent->dump_info(o, as, prefix);
// }

// void SourceInfo::dump_info(OStream & o, const char * prefix) const {
//   AlreadySeen as;
//   dump_info(o, as, prefix);
// }

struct StringObj1 {
  unsigned size;
  char str[1];
};

const StringObj1 EMPTY_STRING_OBJ1 = {0, {'\0'}};

void pos_to_str(Pos p, OStream & buf) {
  if (p.name.empty())
    buf.write("<anon>");
  else
    buf.write(p.name);
  if (p.line == NPOS)
    ;//buf.write(":");
  else if (p.col == NPOS)
    buf.printf(":%u", p.line);
  else
    buf.printf(":%u:%u", p.line, p.col);
}

const BacktraceInfo SourceBlock::PLACEHOLDER(BacktraceInfo::NONE);

void SourceFile::read(String file) {
  file_name_ = file;
  int fd = open(file.c_str(), O_RDONLY);
  if (fd <= 0) {
    fprintf(stderr, "SourceFile::read(): Unable to open file \"%s\" for reading\n", ~file);
    abort();
  }
  read(fd);
}

void SourceFile::read(int fd) {
  static const unsigned BLOCK_SIZE = 1024*16;
  char * d = (char *)malloc(BLOCK_SIZE);
  unsigned capacity = BLOCK_SIZE;
  size_ = 0;
  ssize_t s;
  while (s = ::read(fd, d + size_, BLOCK_SIZE), s) {
    size_ += s;
    if (size_ + BLOCK_SIZE > capacity) {
        capacity *= 2;
        d = (char *)realloc(d, capacity);
    }
  }
  d = (char *)realloc(d, size_+1);
  d[size_] = '\0';
  data_ = d;
  lines_.push_back(data_);
  bool new_line = true;
  bool need_push = true;
  for (char * s = data_; *s; ++s) {
    if (new_line) {
      if (pp_mode && *s == '#') {
        if (s[1] != ' ') continue;
        SourceChange sc;
        sc.idx = s; // the idx is the start of the line
        ++s;
        sc.lineno = strtoul(s, &s, 10);
        ++s;
        assert(*s == '"');
        ++s;
        char * begin = s;
        while (*s && *s != '"') {
          if (*s == '\\') {
            ++s;
            assert(*s);
          }
          ++s;
        }
        assert(*s == '"');
        sc.file_name = String(begin,s);
        while (*s && *s != '\n') ++s;
        if (need_push)
          source_change.push_back(sc);
        else
          source_change.back() = sc;
        memset((void *)sc.idx, ' ', s - sc.idx); // blank line
        need_push = false; // if the next line is also a line control
                           // line just overwrite this one
      } else if (pp_mode) {
        need_push = true;
      }
      new_line = true;
    } 
    if (*s == '\n') {
      lines_.push_back(s+1);
      new_line = true;
    }
  }
  base_block.box = SourceStr(this);
}

struct SourceChangeLt {
  bool operator() (const SourceFile::SourceChange & x, const SourceFile::SourceChange & y) 
    {return x.idx < y.idx;}
  bool operator() (const char * x, const SourceFile::SourceChange & y) 
    {return x < y.idx;}
  bool operator() (const SourceFile::SourceChange & x, const char * y) 
    {return x.idx < y;}
};

Pos SourceFile::get_pos(const char * s) const {
  if (s < data_ || s > data_ + size_ + 1) {
    return Pos(file_name_, NPOS, NPOS);
  }
  Vector<const char *>::const_iterator l = lower_bound(lines_.begin(), lines_.end(), s);
  if (*l != s) --l;
  if (!pp_mode) return Pos(file_name_, l - lines_.begin() + 1, s - *l + 1);

  Vector<SourceChange>::const_iterator sc = lower_bound(source_change.begin(), source_change.end(), 
                                                        s, SourceChangeLt());
  if (sc->idx != s) --sc;
  Vector<const char *>::const_iterator sc_l = lower_bound(lines_.begin(), lines_.end(), sc->idx);
  if (*sc_l != sc->idx) --sc_l;
  assert(sc_l <= l);
  unsigned line_offset = l - sc_l - 1;
  return Pos(sc->file_name, sc->lineno + line_offset, NPOS);
}

String add_dir_if_needed(String file, const SourceFile * included_from) {
  if (file[0] == '/') {
    return file;
  } else {
    StringBuf buf;
    if (included_from) {
      const char * ifn = ~included_from->file_name();
      const char * slash = strrchr(ifn, '/');
      if (slash)
        buf.append(ifn, slash + 1);
      buf.append(file);
    } else {
      char buf2[1024];
      char * wd = getcwd(buf2, 1024);
      buf.append(wd);
      buf.append('/');
      buf.append(file);
    }
    return buf.freeze();
  } 
}

SourceFile * new_source_file(String file, const SourceFile * included_from, bool pp_mode) {
  return new SourceFile(add_dir_if_needed(file, included_from), pp_mode);
}

SourceFile * new_source_file(int fd, bool pp_mode) {
  return new SourceFile(fd, pp_mode);
}

