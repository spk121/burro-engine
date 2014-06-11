AC_PREREQ(2.60)
AC_INIT([Project Burro Game],
        [0.0],
        [burro-devel@lonelycactus.com],
        [project-burro-game],
        [http://lonelycactus.com/project/burro])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([check-news foreign 1.11 -Wall -Wno-portability silent-rules tar-pax no-dist-gzip dist-xz subdir-objects])
AC_CONFIG_MACRO_DIR([m4])
AM_SILENT_RULES([yes])

AC_PATH_PROG([GUILE_EXE],[guile])

AC_ARG_ENABLE([logging],
        AS_HELP_STRING([--disable-logging], [disable system logging @<:@default=enabled@:>@]),
        [], enable_logging=yes)
AS_IF([test "x$enable_logging" = "xyes"], [
        AC_SUBST([ENABLE_LOGGING],[1])
])

AC_ARG_ENABLE([debug],
        AS_HELP_STRING([--enable-debug], [enable debug messages @<:@default=disabled@:>@]),
        [], [enable_debug=no])
AS_IF([test "x$enable_debug" = "xyes"], [
        AC_SUBST([ENABLE_DEBUG],[1])
])

AC_ARG_ENABLE([strict],
        AS_HELP_STRING([--enable-strict], [abort on all runtime errors @<:@default=disabled@:>@]),
        [], [enable_strict=no])
AS_IF([test "x$enable_strict" = "xyes"], [
        AC_SUBST([ENABLE_STRICT],[1])
])

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
        deps_cflags:            ${DEPS_CFLAGS}
        deps_libs:              ${DEPS_LIBS}

        logging:                ${enable_logging}
        debug:                  ${enable_debug}
        strict:                 ${enable_strict}
])

AC_CONFIG_FILES([
        Makefile
])
AC_OUTPUT
