bin_PROGRAMS = burro-engine
pkglib_LTLIBRARIES = libburrowidget.la

################################################################
# TOP-LEVEL SOURCES

libburrowidget_la_SOURCES = \
 burro_canvas.c burro_canvas.h \
 burro_canvas_vram.c burro_canvas_vram.h \
 burro_canvas_bg.c burro_canvas_bg.h \
 burro_canvas_colors.c burro_canvas_colors.h

SRCS_X_H = \
 x/xcairo.h \
 x/xgdk.h \
 x/xgdk-pixbuf.h \
 x/xgio.h \
 x/xgtk.h \
 x/xguile.h \
 x/xpulseaudio.h

SRCS_Y_H =

SRCS_X_C   = \
 x/xcairo.c \
 x/xgdk.c \
 x/xgdk-pixbuf.c \
 x/xgio.c \
 x/xglib.c \
 x/xgtk.c \
 x/xguile.c \
 x/xpulseaudio.c

SRCS_Y_C =

DOT_X_FILES = \
 x/xcairo.x \
 x/xgdk-pixbuf.x \
 x/xgdk.x \
 x/xgio.x \
 x/xglib.x \
 x/xgtk.x \
 x/xguile.x \
 x/xpulseaudio.x \
 burro_app_win.x \
 burro_canvas.x \
 burro_canvas_vram.x \
 burro_canvas_bg.x

BUILT_SOURCES = $(DOT_X_FILES) \
 r/burro.resource.c \
 r/burro.resource.h

EXTRA_DIST = r/burro.resource.xml


r/burro.resource.c: r/burro.resource.xml r/app-menu.ui r/window.ui r/burro_preferences_dialog.ui r/gears-menu.ui
	$(GLIB_COMPILE_RESOURCES) --sourcedir=r --generate-source $<

r/burro.resource.h: r/burro.resource.xml
	$(GLIB_COMPILE_RESOURCES) --sourcedir=r --generate-header $<

CLEANFILES = $(DOT_X_FILES)

burro_engine_SOURCES = $(SRCS_X_H) \
 $(SRCS_X_C) \
 $(SRCS_Y_H) \
 $(SRCS_Y_C) \
 r/burro.resource.c r/burro.resource.h \
 burro_app.c burro_app.h \
 burro_app_win.c burro_app_win.h \
 burro_preferences_dialog.c burro_preferences_dialog.h \
 burro_lisp.c burro_lisp.h \
 burro_repl.c burro_repl.h \
 main.c

burro_engine_LDADD = $(DEPS_LIBS) $(GUILE_LIBS) -lburrowidget -lm
AM_CFLAGS = $(DEPS_CFLAGS) $(GUILE_CFLAGS)

#AM_LDFLAGS = -rdynamic

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS) $(AM_CFLAGS)

SUFFIXES = .x

.c.x:
	$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

gsettings_SCHEMAS = com.lonelycactus.burroengine.gschema.xml
EXTRA_DIST += $(gsettings_SCHEMAS)
@GSETTINGS_RULES@


################################################################
# Guile scheme libraries

dist_guilesite_DATA = burro.scm

################################################################
# Flymake support

get_cs_flags = $(foreach target,$(subst .,_,$(subst -,_,$($(2)))),$($(target)_$(1)FLAGS))
get_cs_all_flags = $(foreach type,$(2),$(call get_cs_flags,$(1),$(type)))
get_cs_compile = $(if $(subst C,,$(1)),$($(1)COMPILE),$(COMPILE))
get_cs_cmdline = $(call get_cs_compile,$(1)) $(call get_cs_all_flags,$(1),check_PROGRAMS bin_PROGRAMS lib_LTLIBRARIES) -fsyntax-only

check-syntax:
        s=$(suffix $(CHK_SOURCES));\
        if   [ "$$s" = ".c"   ]; then \
		$(call get_cs_cmdline,C)   $(CHK_SOURCES);\
        elif [ "$$s" = ".cpp" ]; then \
		$(call get_cs_cmdline,CXX) $(CHK_SOURCES);\
        else exit 1; fi

.PHONY: check-syntax
