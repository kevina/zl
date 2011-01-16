AUTOMAKE_OPTIONS = foreign

#CXX=/opt/gcc-4.4-zl/bin/g++
AM_CXXFLAGS = -Wall -Wno-sign-compare -Wno-unused 
AM_CXXFLAGS += -I/usr/include/gc
#AM_CXXFLAGS += -I/home/kevina/gc6.8/include 
#AM_CXXFLAGS += -DNO_GC

bin_PROGRAMS=zl
bin_SCRIPTS=zlc
zl_SOURCES=ast.cpp  expand.cpp charset.cpp  main.cpp  parse.cpp  parse_op.cpp  peg.cpp util.cpp string_buf.cpp fstream.cpp type.cpp parse_decl.cpp symbol_table.cpp iostream.cpp ct_value.cpp
zl_LDADD=
#zl_LDADD+= /home/kevina/gc6.8/gc.a 
zl_LDADD+= -lgc
zl_LDADD+= -ldl
zl_DEPENDENCIES = zl.ld
zl_LDFLAGS=-Wl,--export-dynamic,--version-script=zl.ld

noinst_HEADERS=*.hpp

noinst_DATA=prelude-fct.so mangle-fct.so libc++.o

EXTRA_DIST = test/* zl.ld ops.in grammer.in grammer.ins \
  class-simple.zl class.zl prelude.zl class-simple.zlh  prelude.zlh zlc.in \
  grammer.hint libc++.cpp c++-include/* zl_h.h prelude-c++.zlh

class-simple-fct.so: zl prelude.zlh prelude.zl class-simple.zlh class-simple.zl grammer.in
	ulimit -s 1024; ./zl -C -P class-simple.zl > class-simple-fct.log
#	zls -g -fexceptions -shared -fpic -o class-simple-fct.so class-simple.zls

prelude-fct.so: zl class-simple-fct.so prelude.zlh prelude.zl class.zl grammer.in
	ulimit -s 1024; ./zl -C -P prelude.zl > prelude-fct.log
#	zls -g -fexceptions -shared -fpic -o prelude-fct.so prelude.zls

mangle-fct.so: zl zlc prelude.zlh mangle.zlp string_buf-zl.hpp string_buf-zl.cpp
	./zlc -E mangle.zlp
	ulimit -s 1024; ./zl -C -P mangle.i > mangle-fct.log
#	zls -g -fexceptions -shared -fpic -o prelude-fct.so prelude.zls mangle.zls




dist-hook:
	cp -p $(srcdir)/distextra/* $(distdir)

zlc: $(top_builddir)/config.status $(srcdir)/zlc.in
	$(top_builddir)/config.status --file=$@
	chmod +x $@

libc++.o: zl prelude-fct.so libc++.cpp c++-include/*
	./zlc -c -g -save-temps libc++.cpp > libc++.log
