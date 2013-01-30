AC_INIT([chealpix], [3.0])
AM_INIT_AUTOMAKE([silent-rules foreign -Werror])
LT_INIT
AC_CONFIG_MACRO_DIR([m4])

dnl
dnl Enable silent build rules if this version of Automake supports them
dnl
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_PROG_CC_C99

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
