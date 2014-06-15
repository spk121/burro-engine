EXTRA_DIST =
CLEANFILES =
ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}
AM_MAKEFLAGS = --no-print-directory

AM_CPPFLAGS = \
	-include $(top_builddir)/config.h \
	-DSYSCONFDIR=\""$(sysconfdir)"\" \
	-DLIBEXECDIR=\""$(libexecdir)"\" \
	-I${top_srcdir}/src/libtga

AM_CFLAGS = ${my_CFLAGS} \
	-fvisibility=hidden \
	-ffunction-sections \
	-fdata-sections

AM_LDFLAGS = \
	-Wl,--gc-sections \
	-Wl,--as-needed

SED_PROCESS = \
	$(AM_V_GEN)$(MKDIR_P) $(dir $@) && $(SED) \
	-e 's,@VERSION\@,$(VERSION),g' \
	-e 's,@prefix\@,$(prefix),g' \
	-e 's,@exec_prefix\@,$(exec_prefix),g' \
	-e 's,@libdir\@,$(libdir),g' \
	-e 's,@includedir\@,$(includedir),g' \
	< $< > $@ || rm $@

TESTS =
check_PROGRAMS =

################################################################
# LIBTGA

LIBTGA_CURRENT=1
LIBTGA_REVISION=0
LIBTGA_AGE=1

lib_LTLIBRARIES = src/libtga/libtga.la

src_libtga_libtga_la_SOURCES =\
	src/libtga/libtga-private.h \
	src/libtga/libtga.c \
	src/libtga/targa.c \
	src/libtga/color.c

EXTRA_DIST += src/libtga/libtga.sym

src_libtga_libtga_la_LDFLAGS = $(AM_LDFLAGS) \
	-version-info $(LIBTGA_CURRENT):$(LIBTGA_REVISION):$(LIBTGA_AGE)

#	-Wl,--version-script=$(top_srcdir)/src/libtga/libtga.sym

src_libtga_libtga_la_DEPENDENCIES = ${top_srcdir}/src/libtga/libtga.sym

TESTS += src/libtga/test-libtga

check_PROGRAMS += src/libtga/test-libtga
src_libtga_test_libtga_SOURCES = src/libtga/test-libtga.c
src_libtga_test_libtga_LDADD = src/libtga/libtga.la
