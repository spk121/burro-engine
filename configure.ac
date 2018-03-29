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
LT_INIT
AM_SILENT_RULES([yes])

GLIB_GSETTINGS

AC_CONFIG_HEADERS([config.h])

# Checks for libraries
PKG_PROG_PKG_CONFIG
PKG_CHECK_MODULES([DEPS],[gtk+-3.0
                          gio-2.0
                          glib-2.0
                          dnl guile-2.2
                          libpulse
                          libpulse-mainloop-glib])

AC_ARG_WITH([zegu],
	    [AS_HELP_STRING([--with-zegu], 
   	                    [experimental support the Zegu scripting engine])],
	    [with_libzegu=yes],
	    [with_libzegu=no])
	     
LIBZEGU=
AS_IF([test "x$with_libzegu" = xyes],
      [AC_CHECK_LIB([zegu],
                    [zegu_init],
                    [AC_SUBST([LIBZEGU], ["-lzegu"])
		     AC_DEFINE([HAVE_LIBZEGU], [1],
		               [Define if you have libzegu])],
		    [AC_MSG_FAILURE([--with-libzegu was given, but, test for libzegu failed])],
		    [-lzegu])])

################################################################
# Guile support details
GUILE_PKG([2.2])
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

AC_ARG_VAR([GUILE_SNARF], [guile snarf compilation tool])
_guile_suffix=`echo "$GUILE" | sed -e 's,^.*/guile\(.*\)$,\1,'`
AC_PATH_PROG([GUILE_SNARF],[guile-snarf$_guile_suffix])
AS_IF([test "x$GUILE_SNARF" = "xno"],
      [AC_MSG_ERROR([Could not find guile-snarf$_guile_suffix tool needed for compilation])])

# Guile has three directories that don't follow the GNU Filesystem
# Heirarchy Standards.  If one follows the GNU FHS, files get installed
# in directories in which Guile does not search for them by default.
AC_ARG_WITH([guilesitedir],
  [use the specified installation path for Guile modules],
  [case "x$withval" in
    xyes|xno) guilesitedir="";;
    *)        guilesitedir="$withval";;
   esac],
  [guilesitedir=""])

pkgdatadir="$datadir/$PACKAGE_TARNAME"
if test "x$guilesitedir" = "x"; then
  guilesitedir="$pkgdatadir"
fi
AC_SUBST([guilesitedir])

AC_ARG_WITH([guileextensiondir],
  [use the specified installation path for Guile binary shared libraries],
  [case "x$withval" in
    xyes|xno) guileextensiondir="";;
    *)        guileextensiondir="$withval";;
   esac],
  [guileextensiondir=""])

if test "x$guileextensiondir" = "x"; then
  guileextensiondir="$libdir"
fi
AC_SUBST([guileextensiondir])

################################################################
# GLIB resource compiler

GLIB_COMPILE_RESOURCES=`$PKG_CONFIG --variable glib_compile_resources gio-2.0`
AC_SUBST(GLIB_COMPILE_RESOURCES)

################################################################
# Finish up

AC_CONFIG_FILES([
 Makefile
 src/Makefile
 test/Makefile
 data/Makefile
 data/icons/Makefile
 data/icons/hicolor/Makefile
 data/icons/hicolor/48x48/Makefile
 data/icons/hicolor/48x48/apps/Makefile
 data/icons/hicolor/scalable/Makefile
 data/icons/hicolor/scalable/apps/Makefile
 data/icons/src/Makefile
 ])

AC_CONFIG_FILES([src/uninstalled-env], [chmod +x src/uninstalled-env])

AC_OUTPUT
AC_SUBST([guilesitedir])

if test "$guilesitedir" != "$GUILE_SITE"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in '${guilesitedir}'.]
   [You should probably re-run `configure' with]
   [`--with-guilesitedir=$GUILE_SITE']
   [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
fi

if test "x$GUILE_EXTENSION" != "x"; then
if test "$guileextensiondir" != "$GUILE_EXTENSION"; then
   AC_MSG_WARN([]
   [The Guile extensions will be installed in ${guileextensiondir}.]
   [You should consider re-running `configure' with]
   [`--with-guileextensiondir=$GUILE_EXTENSION'])
fi
fi

