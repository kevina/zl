AUTOMAKE_OPTIONS = foreign

#CXX=/opt/gcc-4.4-zl/bin/g++
AM_CXXFLAGS = -Wall -Wno-sign-compare -Wno-unused
#AM_CXXFLAGS += -I/usr/include/gc
#AM_CXXFLAGS += -I/home/kevina/gc6.8/include 
AM_CXXFLAGS += -DNO_GC

noinst_PROGRAMS=zl
noinst_SCRIPTS=zlc
zl_SOURCES=ast.cpp expand.cpp charset.cpp main.cpp parse.cpp		\
  parse_op.cpp peg.cpp util.cpp string_buf.cpp fstream.cpp type.cpp	\
  parse_decl.cpp symbol_table.cpp iostream.cpp ct_value.cpp syntax.cpp
zl_LDADD=
#zl_LDADD+= /home/kevina/gc6.8/gc.a 
zl_LDADD+= -lgc
zl_LDADD+= -ldl
zl_DEPENDENCIES = zl.ld
zl_LDFLAGS=-Wl,--export-dynamic,--version-script=zl.ld

noinst_HEADERS=*.hpp

noinst_DATA=prelude-fct.so mangle-fct.so libc++.o class-gcc_abi-fct.so

CLEANFILES=${noinst_DATA}

EXTRA_DIST = test/* \
  zl.ld ops.in zlc.in grammer.in grammer.ins grammer.hint \
  libc++.cpp c++-include/* zl_h.h string_buf-zl.cpp string_buf-zl.hpp \
  abi_info.zlh class-gcc_abi.zl class-gcc_abi.zlh \
  class-simple.zl class-simple.zlh class.zl macro_api.zlh mangle.zlh \
  prelude-c++.zlh prelude-extra.zlh prelude.zl prelude.zlh \
  mangle-orig-abi.zlp mangle.zlp

class-simple-fct.so: zl prelude.zlh prelude.zl class-simple.zlh class-simple.zl grammer.in
	ulimit -s 1024; ./zl -C -P class-simple.zl > class-simple-fct.log
#	zls -g -fexceptions -shared -fpic -o class-simple-fct.so class-simple.zls

abi_info.zlh: zl zlc abi_info.hpp
	./zlc -E abi_info.hpp -o abi_info.i
	mv abi_info.i abi_info.zlh

prelude-fct.so: zl class-simple-fct.so prelude.zlh prelude.zl class.zl grammer.in abi_info.zlh qquote.zl
	ulimit -s 1024; ./zl -C -P prelude.zl > prelude-fct.log
#	zls -g -fexceptions -shared -fpic -o prelude-fct.so prelude.zls

mangle-fct.so: zl zlc prelude.zlh mangle.zlp string_buf-zl.hpp string_buf-zl.cpp prelude-fct.so
	./zlc -E mangle.zlp -o mangle.i
	ulimit -s 1024; ./zl -C -P -pp mangle.i > mangle-fct.log
#	zls -g -fexceptions -shared -fpic -o prelude-fct.so prelude.zls mangle.zls

class-gcc_abi-fct.so: zl class-gcc_abi.zl class-simple-fct.so prelude.zlh prelude.zl class.zl grammer.in
	ulimit -s 1024; ./zl -C -P class-gcc_abi.zl > class-gcc_abi-fct.log

dist-hook:
	$(MAKE) -C test clean
	cp -p $(srcdir)/distextra/* $(distdir)

zlc: $(top_builddir)/config.status $(srcdir)/zlc.in
	$(top_builddir)/config.status --file=$@
	chmod +x $@

libc++.o: zl prelude-fct.so libc++.cpp c++-include/*
	./zlc -c -g -save-temps libc++.cpp > libc++.log

fake-install:
	$(install_sh) -d $(bindir)
	ln -f -s $(abs_srcdir)/zlc $(bindir)/zlc

fake-uninstall:
	-rm $(bindir)/zlc

