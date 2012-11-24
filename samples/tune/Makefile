CFLAGS += -g3 -gdwarf-2 -O0 -I ../.. \
	`pkg-config glib-2.0 gio-2.0 gtk+-3.0 libpulse --cflags`
LIBS += `pkg-config glib-2.0 gio-2.0 gtk+-3.0 libpulse --libs` -lm -lmudflap

all: tones

VPATH=../..

burro.a: engine.o \
	eng_audio.o \
	eng_state.o \
	eng_video.o \
	pulseaudio.o
	ar rcs burro.a engine.o eng_audio.o eng_state.o eng_video.o pulseaudio.o
tones: main.o tga.o io.o burro.a
	gcc -g -o tones main.o tga.o io.o burro.a -fmudflap $(LIBS)
