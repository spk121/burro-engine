# Process this file with autoconf to produce a configure script.

# Autoconf requirements
AC_PREQ([2.69])
AC_INIT([Burro Engine],[0.0],
        [spk121@yahoo.com],
        [burro-engine],
        [https://github.com/spk121/burro-engine])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([foreign tar-ustar subdir-objects])

AM_SILENT_RULES([yes])

## Let's try avoiding the config.h until absolutely necessary
#AC_CONFIG_HEADERS([config.h])

# Checks for libraries
PKG_CHECK_MODULES([DEPS],[gtk+-3.0
                          gio-2.0
                          glib-2.0
                          guile-2.2
                          libpulse
                          libpulse-mainloop-glib])


# Checks for programs
AC_PROG_CC
AC_ARG_VAR([GUILE_SNARF], [guile snarf compilation tool])
AC_CHECK_PROGS([GUILE_SNARF],[guile-snarf],[no])
AS_IF([test "x$GUILE_SNARF" = "xno"],
      [AC_MSG_ERROR([Could not find guile-snarf tool needed for compilation])])

GLIB_COMPILE_RESOURCES=`$PKG_CONFIG --variable glib_compile_resources gio-2.0`
AC_SUBST(GLIB_COMPILE_RESOURCES)

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 test/Makefile])
AC_OUTPUT
