EXTRA_DIST =
CLEANFILES =
ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}
AM_MAKEFLAGS = --no-print-directory

dist_pkglibexec_SCRIPTS = src/game.scm
pkgguiledir = $(pkgdatadir)/src
pkgguile_DATA = src/burro-lib.scm

M4_PROCESS = \
	$(AM_V_GEN)$(MKDIR_P) $(dir $@) && m4 \
	-P \
	-D GUILE_EXE="$(GUILE_EXE)" \
	-D PKGLIBDIR="$(pkglibdir)" \
	-D PKGGUILEDIR="$(pkgguiledir)" \
	-D ENABLE_LOGGING="$(ENABLE_LOGGING)" \
	-D ENABLE_DEBUG="$(ENABLE_DEBUG)" \
	-D ENABLE_STRICT="$(ENABLE_STRICT)" \
	$< > $@ || rm $@

%.scm: %.scm.in Makefile
	$(M4_PROCESS)

EXTRA_DIST += src/game.scm.in
CLEANFILES += src/game.scm


