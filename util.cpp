#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#include "util.hpp"
#include "iostream.hpp"
#include "string_buf.hpp"
#include "hash-t.hpp"

#include <algorithm>

void SourceInfo::dump_info(OStream & o, AlreadySeen & as, const char * prefix) const {
  bool seen_self = as.have(this);
  as.insert(this);
  StringBuf buf;
  bool dump_parents = dump_info_self(buf);
  String str = buf.freeze();
  if (!str.empty()) {
    if (seen_self) 
      o << prefix << "[" << str << "]\n";
    else
      o << prefix << str << "\n";
  }
  if (parent && !seen_self && dump_parents)
    parent->dump_info(o, as, prefix);
}

void SourceInfo::dump_info(OStream & o, const char * prefix) const {
  AlreadySeen as;
  dump_info(o, as, prefix);
}

struct StringObj1 {
  unsigned size;
  char str[1];
};

const StringObj1 EMPTY_STRING_OBJ1 = {0, {'\0'}};

char * pos_to_str(Pos p, char * buf) {
  if (p.line == UINT_MAX)
    strcpy(buf, "<somewhere>");
  else
    snprintf(buf, 24, "%uc%u", p.line, p.col);
  return buf;
}

void SourceFile::read(String file) {
  file_name_ = file;
  int fd = open(file.c_str(), O_RDONLY);
  assert(fd >= 0);
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
  for (const char * s = data_; *s; ++s)
    if (*s == '\n') lines_.push_back(s+1);
}

Pos SourceFile::get_pos(const char * s) const {
  if (s < data_ || s > data_ + size_ + 1) {
    return Pos(UINT_MAX,0);
  }
  Vector<const char *>::const_iterator i = lower_bound(lines_.begin(), lines_.end(), s);
  if (*i != s) --i;
  return Pos(i - lines_.begin() + 1, s - *i);
}

SourceFile * new_source_file(String file) {
  return new SourceFile(file);
}

SourceFile * new_source_file(int fd) {
  return new SourceFile(fd);
}

bool SourceFile::dump_info_self(OStream & o) const {
  return false;
  //o.printf("%sin file %s\n", prefix, ~file_name_);
}

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
