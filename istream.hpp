// This file is part of The New Aspell
// Copyright (C) 2001 by Kevin Atkinson under the GNU LGPL license
// version 2.0 or 2.1.  You should have received a copy of the LGPL
// license along with this library if you did not you can find
// it at http://www.gnu.org/.

#ifndef ASPELL_ISTREAM__HPP
#define ASPELL_ISTREAM__HPP

#include "gc.hpp"

//namespace acommon {

  class StringBuf;

  class IStream : public gc_cleanup {
  private:
    char delem;
  public:
    IStream(char d = '\n') : delem(d) {}
    
    char delim() const {return delem;}

    // getline will read until delem
    virtual bool append_line(StringBuf &, char c) = 0;
    bool append_line(StringBuf & str) {return append_line(str, delem);}
    bool getline(StringBuf & str, char c);
    bool getline(StringBuf & str);

    virtual bool read(void *, unsigned int) = 0;

    virtual ~IStream() {}
  };
  
//}

#endif
