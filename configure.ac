#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(zl, 0.01)
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([main.cpp])
AC_CONFIG_HEADER([config.h])

# Checks for programs.
if test -z "$CXXFLAGS"; then
  CXXFLAGS="-g -O"
fi
AC_PROG_CXX

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL      
AC_C_CONST
AC_C_INLINE

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset strtol])

AC_CONFIG_FILES([Makefile])

AH_TEMPLATE([SOURCE_PREFIX], [])
AC_DEFINE_UNQUOTED([SOURCE_PREFIX], "`pwd`/") 

AC_OUTPUT
