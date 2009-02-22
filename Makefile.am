AUTOMAKE_OPTIONS = foreign

CXXFLAGS = -g -Wall -Wno-sign-compare -Wno-unused -DNO_GC

bin_PROGRAMS=zl
zl_SOURCES=ast.cpp  expand.cpp charset.cpp  main.cpp  parse.cpp  parse_op.cpp  peg.cpp util.cpp string_buf.cpp fstream.cpp type.cpp parse_decl.cpp symbol_table.cpp iostream.cpp
zl_LDADD=
#zl_LDADD+= ../gc6.8/gc.a
#zl_LDADD+= -lgc
#zl_LDADD+= -lgccpp
zl_LDADD+= -ldl
zl_LDFLAGS=-Wl,--export-dynamic,--version-script=zl.ld

noinst_DATA=prelude.so

prelude.so: prelude.zlh prelude.zl zl
	./zl -p > /dev/null
