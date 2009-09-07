AUTOMAKE_OPTIONS = foreign

#CXX=/opt/gcc-4.4-zl/bin/g++
AM_CXXFLAGS = -Wall -Wno-sign-compare -Wno-unused -DNO_GC
CXXFLAGS = -g
#CXXFLAGS = -O -fno-default-inline

bin_PROGRAMS=zl
zl_SOURCES=ast.cpp  expand.cpp charset.cpp  main.cpp  parse.cpp  parse_op.cpp  peg.cpp util.cpp string_buf.cpp fstream.cpp type.cpp parse_decl.cpp symbol_table.cpp iostream.cpp ct_value.cpp
zl_LDADD=
#zl_LDADD+= /home/kevina/gc6.8/gc.a
#zl_LDADD+= -lgc
#zl_LDADD+= -lgccpp
zl_LDADD+= -ldl
zl_LDFLAGS=-Wl,--export-dynamic,--version-script=zl.ld

noinst_DATA=prelude-fct.so

class-simple-fct.so: zl prelude.zlh prelude.zl class-simple.zlh class-simple.zl
	ulimit -s 1024; ./zl -C -P class-simple.zl > class-simple-fct.log

prelude-fct.so: zl class-simple-fct.so prelude.zlh prelude.zl class.zl 
	ulimit -s 1024; ./zl -C -P prelude.zl > prelude-fct.log
