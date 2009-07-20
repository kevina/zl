// This file is part of The New Aspell
// Copyright (C) 2001 by Kevin Atkinson under the GNU LGPL license
// version 2.0 or 2.1.  You should have received a copy of the LGPL
// license along with this library if you did not you can find
// it at http://www.gnu.org/.

#include <stdio.h>
#include <assert.h>

#include "iostream.hpp"

#include "asc_ctype.hpp"
#include "string_buf.hpp"
#include "fstream.hpp"

//namespace acommon {

  void FStream::open(ParmStr name, const char * mode)
  {
    assert (file_ == 0);
    file_ = fopen(name,mode);
    if (file_ == 0) {
      if (strpbrk(mode, "wa+") != 0)
	abort(); //return make_err(cant_write_file, name);
      else
	abort(); //return make_err(cant_read_file, name);
    } 
  }

  void FStream::close()
  {
    if (file_ != 0 && own_)
      fclose(file_);
    file_ = 0;
  }

  int FStream::file_no() 
  {
    return fileno(file_);
  }

  FILE * FStream::c_stream() 
  {
    return file_;
  }

  void FStream::restart()
  {
    flush();
    fseek(file_,0,SEEK_SET);
  }

  void FStream::skipws() 
  {
    int c;
    while (c = getc(file_), c != EOF && asc_isspace(c));
    ungetc(c, file_);
  }

  FStream & FStream::operator>> (StringBuf & str)
  {
    skipws();
    int c;
    str = "";
    while (c = getc(file_), c != EOF && !asc_isspace(c))
      str += static_cast<char>(c);
    ungetc(c, file_);
    return *this;
  }

  bool FStream::append_line(StringBuf & str, char d)
  {
    int c;
    c = getc(file_);
    if (c == EOF) return false;
    if (c == (int)d) return true;
    str.append(c);
    while (c = getc(file_), c != EOF && c != (int)d) 
      str.append(c);
    return true;
  }

  bool FStream::read(void * str, unsigned int n)
  {
    fread(str,1,n,file_);
    return operator bool();
  }

  void FStream::write(char c)
  {
    putc(c, file_);
  }

  void FStream::write(ParmStr str) 
  {
    fputs(str, file_);
  }

  void FStream::write(const void * str, unsigned int n)
  {
    fwrite(str,1,n,file_);
  }

  void FStream::write(FStream & f) {
    int chr;
    while ((chr = getc(f.file_)) != EOF)
      putc(chr, file_);
  }

  FStream & FStream::operator>> (unsigned int & num)
  {
    int r = fscanf(file_, " %u", &num);
    if (r != 1)
      close();
    return *this;
  }


  FStream & FStream::operator>> (int & num)
  {
    int r = fscanf(file_, " %i", &num);
    if (r != 1)
      close();
    return *this;
  }

//}
