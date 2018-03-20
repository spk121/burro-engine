bin_PROGRAMS = burro

################################################################
# TOP-LEVEL SOURCES

SRCS_X_H = \
 x/xcairo.h \
 x/xgdk.h \
 x/xgdk-pixbuf.h \
 x/xgio.h \
 x/xgtk.h \
 x/xguile.h \
 x/xpulseaudio.h

SRCS_Y_H = \
 y/audio_model.h \
 y/backdrop.h \
 y/bg.h \
 y/console.h \
 y/draw.h \
 y/ecma48.h \
 y/eng.h \
 y/guile.h \
 y/lineedit.h \
 y/lisp.h \
 y/loop.h \
 y/matrix.h \
 y/obj.h \
 y/pixbuf.h \
 y/proc.h \
 y/pulseaudio.h \
 y/rand.h \
 y/repl.h \
 y/textbox.h \
 y/vram.h \
 y/wave.h

SRCS_X_C   = \
 x/xcairo.c \
 x/xgdk.c \
 x/xgdk-pixbuf.c \
 x/xgio.c \
 x/xglib.c \
 x/xgtk.c \
 x/xguile.c \
 x/xpulseaudio.c

SRCS_Y_C = \
 y/audio_model.c \
 y/backdrop.c \
 y/bg.c \
 y/console.c \
 y/draw.c \
 y/ecma48.c \
 y/eng.c \
 y/guile.c \
 y/lineedit.c \
 y/lisp.c \
 y/loop.c \
 y/matrix.c \
 y/obj.c \
 y/pixbuf.c \
 y/pulseaudio.c \
 y/rand.c \
 y/repl.c \
 y/textbox.c \
 y/vram.c \
 y/wave.c

DOT_X_FILES = \
 x/xcairo.x \
 x/xgdk-pixbuf.x \
 x/xgdk.x \
 x/xgio.x \
 x/xglib.x \
 x/xgtk.x \
 x/xguile.x \
 x/xpulseaudio.x \
 y/audio_model.x \
 y/backdrop.x \
 y/bg.x \
 y/console.x \
 y/draw.x \
 y/ecma48.x \
 y/eng.x \
 y/guile.x \
 y/lineedit.x \
 y/lisp.x \
 y/loop.x \
 y/matrix.x \
 y/obj.x \
 y/pixbuf.x \
 y/pulseaudio.x \
 y/rand.x \
 y/repl.x \
 y/textbox.x \
 y/vram.x \
 y/wave.x

BUILT_SOURCES = $(DOT_X_FILES) \
 r/burro.resource.c \
 r/burro.resource.h

EXTRA_DIST = r/burro.resource.xml

r/burro.resource.c: r/burro.resource.xml
	glib-compile-resources --sourcedir=r --generate-source $<

r/burro.resource.h: r/burro.resource.xml
	glib-compile-resources --sourcedir=r --generate-header $<


CLEANFILES = $(DOT_X_FILES)

burro_SOURCES = $(SRCS_X_H) \
 $(SRCS_X_C) \
 $(SRCS_Y_H) \
 $(SRCS_Y_C) \
 r/burro.resource.c r/burro.resource.h \
 main.c

AM_CFLAGS = $(GTK3_CFLAGS) \
 $(GUILE_CFLAGS) \
 $(LIBPULSE_CFLAGS) \
 $(LIBPULSE_MAINLOOP_GLIB_CFLAGS)

LIBS = $(GTK3_LIBS) \
 $(GUILE_LIBS) \
 $(LIBPULSE_LIBS) \
 $(LIBPULSE_MAINLOOP_GLIB_LIBS) \
 -lm

snarfcppopts = $(DEFS) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS) $(AM_CFLAGS)

SUFFIXES = .x

.c.x:
	$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

##
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
