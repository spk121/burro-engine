EXTRA_DIST =
CLEANFILES =
ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}
AM_MAKEFLAGS = --no-print-directory

AM_CPPFLAGS = \
	-include $(top_builddir)/config.h \
	-DSYSCONFDIR=\""$(sysconfdir)"\" \
	-DLIBEXECDIR=\""$(libexecdir)"\" \
	-I${top_srcdir}/src/libburro \
	-I${top_srcdir}/src

AM_CFLAGS = ${my_CFLAGS} \
	-fvisibility=hidden

AM_LDFLAGS =

bin_PROGRAMS = burro

burro_SOURCES =\
	dl.c \
	dl.h \
	eng_audio.c \
	eng_audio.h \
	engine.c \
	engine_constants.h \
	engine.h \
	engine_priv.h \
	eng_input.h \
	eng_state.c \
	eng_state.h \
	eng_timers.h \
	eng_video.c \
	eng_video.h \
	game.c \
	game.h \
	io.c \
	io.h \
	main.c \
	pulseaudio.c \
	pulseaudio.h
