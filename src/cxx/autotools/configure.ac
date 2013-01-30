AC_INIT([Healpix_cxx], [3.0])
AM_INIT_AUTOMAKE([silent-rules foreign -Werror])
LT_INIT
AC_CONFIG_MACRO_DIR([m4])

dnl
dnl By default, install the headers into a subdirectory of
dnl ${prefix}/include to avoid possible header filename collisions.
dnl
includedir="${includedir}/${PACKAGE_NAME}"

dnl
dnl Enable silent build rules if this version of Automake supports them
dnl
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_DEFUN([AX_CHECK_COMPILE_FLAG],
[AS_VAR_PUSHDEF([CACHEVAR],[ax_cv_check_[]_AC_LANG_ABBREV[]flags_$4_$1])dnl
AC_CACHE_CHECK([whether _AC_LANG compiler accepts $1], CACHEVAR, [
  ax_check_save_flags=$[]_AC_LANG_PREFIX[]FLAGS
  _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS $4 $1"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
    [AS_VAR_SET(CACHEVAR,[yes])],
    [AS_VAR_SET(CACHEVAR,[no])])
  _AC_LANG_PREFIX[]FLAGS=$ax_check_save_flags])
AS_IF([test x"AS_VAR_GET(CACHEVAR)" = xyes],
  [m4_default([$2], :)],
  [m4_default([$3], :)])
AS_VAR_POPDEF([CACHEVAR])dnl
])dnl AX_CHECK_COMPILE_FLAGS

AC_PROG_CC_C99
AC_OPENMP
CPPFLAGS="$CPPFLAGS $OPENMP_CFLAGS"
CFLAGS="$CFLAGS $OPENMP_CFLAGS"
AX_CHECK_COMPILE_FLAG([-march=native],[CC="$CC -march=native"])
AX_CHECK_COMPILE_FLAG([-fno-tree-vectorize],[CFLAGS="$CFLAGS -fno-tree-vectorize"])
AX_CHECK_COMPILE_FLAG([-ffast-math],[CFLAGS="$CFLAGS -ffast-math"])
AX_CHECK_COMPILE_FLAG([-fomit-frame-pointer],[CFLAGS="$CFLAGS -fomit-frame-pointer"])

AC_PROG_CXX
AC_LANG_PUSH([C++])
AC_OPENMP
CXXFLAGS="$CXXFLAGS $OPENMP_CFLAGS"
AX_CHECK_COMPILE_FLAG([-march=native],[CXX="$CXX -march=native"])
AX_CHECK_COMPILE_FLAG([-fno-tree-vectorize],[CXXFLAGS="$CXXFLAGS -fno-tree-vectorize"])
AX_CHECK_COMPILE_FLAG([-ffast-math],[CXXFLAGS="$CXXFLAGS -ffast-math"])
AX_CHECK_COMPILE_FLAG([-fomit-frame-pointer],[CXXFLAGS="$CXXFLAGS -fomit-frame-pointer"])
AC_LANG_POP([C++])

dnl
dnl Using pkg-config, determine the compiler flags (CFITSIO_CFLAGS)
dnl and linker flags (CFITSIO_LIBS) required to build against libcfitsio.
dnl
PKG_CHECK_MODULES([CFITSIO], [cfitsio])

AC_PROG_LIBTOOL

dnl
dnl Create pkgconfig .pc file.
dnl
AX_CREATE_PKGCONFIG_INFO(,,,,[$CFITSIO_CFLAGS])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
