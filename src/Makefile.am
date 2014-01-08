EXTRA_DIST = z/swig.i
CLEANFILES =
ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}
AM_MAKEFLAGS = --no-print-directory

AM_CPPFLAGS = \
	-include $(top_builddir)/config.h \
	-DSYSCONFDIR=\""$(sysconfdir)"\" \
	-DLIBEXECDIR=\""$(libexecdir)"\" \
	-I${top_srcdir}/src \
	-Ix

AM_CFLAGS = ${my_CFLAGS} \
	${DEPS_CFLAGS} \
	${GUILE_CFLAGS} \
	-fvisibility=hidden

AM_LDFLAGS = \
	${DEPS_LIBS} \
	${GUILE_LIBS} 

bin_PROGRAMS = burro

burro_SOURCES = \
	main.c \
	r/resource.c \
	r/resource.h \
	x/xcairo.c \
	x/xcairo.h \
	x/xgdk.c \
	x/xgdk.h \
	x/xgio.c \
	x/xgio.h \
	x/xglib.c \
	x/xglib.h \
	x/xguile.c \
	x/xguile.h \
	x/xgtk.c \
	x/xgtk.h \
	y/bg.c \
	y/bg.h \
	y/draw.c \
	y/draw.h \
	y/eng.c \
	y/eng.h \
	y/io.c \
	y/io.h \
	y/loop.c \
	y/loop.h \
	y/obj.c \
	y/obj.h \
	y/proc.c \
	y/proc.h \
	y/rand.c \
	y/rand.h \
	y/tga.c \
	y/tga.h \
	z/game.c \
	z/game.h \
	z/guileapi.c

BUILT_SOURCES = \
	r/resource.c \
	r/resource.h \
	z/guileapi.c 

ORIG_RESOURCES = \
	r/cga8c.tga

BUILT_RESOURCES = \
	r/cga8c_bmp8.tga \
	r/splash_bmp16.tga

r/cga8c_bmp8.tga: r/cga8c.tga
	targatool --bmp8 cga8c.tga --output cga8c_bmp8.tga

r/resource.h: r/resource.xml $(BUILT_RESOURCES)
	glib-compile-resources --sourcedir=r --generate $< --target=$@

r/resource.c: r/resource.xml $(BUILT_RESOURCES)
	glib-compile-resources --sourcedir=r --generate $< --target=$@

z/guileapi.c: z/swig.i
	swig -guile -o $@ $<

# burro_SOURCES =\
# 	x/xcairo.c \
# 	x/xcairo.h \
# 	x/xglib.c \
# 	x/xglib.h \
# 	x/xgdk.c \
# 	x/xgdk.h \
# 	x/xgtk.c \
# 	x/xgtk.h \
# 	x/xpulseaudio.c \
# 	x/xpulseaudio.h \
# 	x/xgio.c \
# 	x/xgio.h \
# 	y/io.c \
# 	y/io.h \
# 	y/tga.c \
# 	y/tga.h \
# 	y/bg.c \
# 	y/bg.h \
# 	y/obj.h \
# 	y/obj.c \
# 	eng_audio.c \
# 	eng_audio.h \
# 	engine.c \
# 	engine_constants.h \
# 	engine.h \
# 	engine_priv.h \
# 	eng_input.h \
# 	eng_state.c \
# 	eng_state.h \
# 	eng_timers.h \
# 	eng_video.c \
# 	eng_video.h \
# 	main.c \
# 	pulseaudio.c \
# 	pulseaudio.h \
# 	socket.c \
# 	extern.c \
# 	commands.c
