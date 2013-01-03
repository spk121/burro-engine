AC_PREREQ(2.60)
AC_INIT([Project Burro],
        [0.0],
        [burro-devel@lonelycactus.com],
        [project-burro],
        [http://lonelycactus.com/project/burro])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([check-news foreign 1.11 -Wall -Wno-portability silent-rules tar-pax no-dist-gzip dist-xz subdir-objects])
AC_PROG_CC_STDC
AC_USE_SYSTEM_EXTENSIONS
AC_SYS_LARGEFILE
AC_CONFIG_MACRO_DIR([m4])
AM_SILENT_RULES([yes])
# AC_PREFIX_DEFAULT([/usr/local])
LT_INIT([disable-static pic-only])

AC_PROG_SED
AC_PROG_MKDIR_P
AC_CHECK_LIB([m],[cos])
PKG_CHECK_MODULES([DEPS], [glib-2.0 gio-2.0 gtk+-3.0 cairo libpulse])
PKG_CHECK_MODULES([GUILE], [guile-2.0],,
  [PKG_CHECK_MODULES([GUILE], [guile-1.8])])
GUILEVER=`guile -c "(display (version))"`
AC_MSG_NOTICE([Using guile version ${GUILEVER}])
AC_PATH_PROG([GUILE_EXE],[guile])

AC_ARG_ENABLE([logging],
        AS_HELP_STRING([--disable-logging], [disable system logging @<:@default=enabled@:>@]),
        [], enable_logging=yes)
AS_IF([test "x$enable_logging" = "xyes"], [
        AC_DEFINE(ENABLE_LOGGING, [1], [System logging.])
])

AC_ARG_ENABLE([debug],
        AS_HELP_STRING([--enable-debug], [enable debug messages @<:@default=disabled@:>@]),
        [], [enable_debug=no])
AS_IF([test "x$enable_debug" = "xyes"], [
        AC_DEFINE(ENABLE_DEBUG, [1], [Debug messages.])
])

AC_ARG_ENABLE([strict],
        AS_HELP_STRING([--enable-strict], [abort on all runtime errors @<:@default=disabled@:>@]),
        [], [enable_strict=no])
AS_IF([test "x$enable_strict" = "xyes"], [
        AC_DEFINE(ENABLE_STRICT, [1], [Debug messages.])
])

my_CFLAGS="-Wall \
-Wmissing-declarations -Wmissing-prototypes \
-Wnested-externs -Wpointer-arith \
-Wpointer-arith -Wsign-compare -Wchar-subscripts \
-Wstrict-prototypes -Wshadow \
-Wformat-security -Wtype-limits"
AC_SUBST([my_CFLAGS])
AC_CONFIG_FILES([
        Makefile
	gui/Makefile
	game/Makefile
])
#	libburro/Makefile
#	game/Makefile
# AC_CONFIG_SUBDIRS([libburro game])
AC_OUTPUT

AC_MSG_RESULT([
        $PACKAGE $VERSION
        =====

	srcdir:                 ${srcdir}

        prefix:                 ${prefix}
        sysconfdir:             ${sysconfdir}
        libdir:                 ${libdir}
        includedir:             ${includedir}

        compiler:               ${CC}
        cflags:                 ${CFLAGS}
        ldflags:                ${LDFLAGS}

        deps_cflags:            ${DEPS_CFLAGS}
        deps_libs:              ${DEPS_LIBS}

        logging:                ${enable_logging}
        debug:                  ${enable_debug}
        strict:                 ${enable_strict}

        guile:                  ${GUILE_EXE}
])

