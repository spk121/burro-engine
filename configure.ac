# Process this file with autoconf to produce a configure script.

# Autoconf requirements
AC_PREQ([2.69])
AC_INIT([Burro Game Engine],[0.0],[spk121@yahoo.com],[burro-engine],[https://github.com/spk121/burro-engine])
AC_COPYRIGHT([GPL3+])
AC_CONFIG_SRCDIR([engine/main.c])
AC_CONFIG_AUX_DIR([build_aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign subdir-objects])
AC_CONFIG_HEADERS([config.h])

# Checks for programs
AC_PROG_CC
AC_ARG_VAR([GUILE_SNARF], [guile snarf compilation tool])
AC_CHECK_PROGS([GUILE_SNARF],[guile-snarf],[no])
AS_IF([test "x$GUILE_SNARF" = "xno"],
      [AC_MSG_ERROR([Could not find guile-snarf tool needed for compilation])])
## FIXME: check for glib-compile-resources

# Checks for libraries
PKG_CHECK_MODULES([GTK3],[gtk+-3.0])
PKG_CHECK_MODULES([GUILE],[guile-2.2])
PKG_CHECK_MODULES([LIBPULSE],[libpulse])
PKG_CHECK_MODULES([LIBPULSE_MAINLOOP_GLIB],[libpulse-mainloop-glib])

# Checks for header files
# Checks for types
# Checks for structures
# Checks for compiler characteristics
# Checks for library functions
# Checks for system services

AC_MSG_RESULT([
        $PACKAGE $VERSION
        =====

        prefix:                 ${prefix}
        sysconfdir:             ${sysconfdir}
        libdir:                 ${libdir}
        includedir:             ${includedir}

        compiler:               ${CC}
        cflags:                 ${CFLAGS}
        ldflags:                ${LDFLAGS}
])

AC_CONFIG_FILES([Makefile
		 engine/Makefile
		 test/Makefile
		 meta/Makefile])
AC_OUTPUT
