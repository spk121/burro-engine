# Makefile -- the makefile for the gui

# Copyright 2014, Michael L. Gran

# This file is part of the Project Burro game engine.

# Project Burro is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# Project Burro is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Project Burro.  If not, see
# <http://www.gnu.org/licenses/>. */

SHELL = /bin/sh

srcdir = .

CC       = gcc --std=c11 -fdiagnostics-color -fmessage-length=78
CXX      = g++ --std=c++11 -fdiagnostics-color -fmessage-length=78

################################################################
# FLAGS

CPPFLAGS = \
	-Wall \
	-Wunused-macros \
	-Wendif-labels \
	-Wunknown-pragmas \
	`sdl2-config --cflags` \
	`pkg-config cairo --cflags` \
	`pkg-config tinyxml --cflags` \
	`pkg-config libiso9660++ --cflags` \
	`pkg-config mozjs-24 --cflags `\
	-I$(srcdir) \
	-I$(srcdir)/tmxparser/src

DEFS     = -DSDL_ASSERT_LEVEL=2

CDEBUG   = -g3 -O0

CWARN = \
    -Wall \
	-Wpointer-arith \
	-Wformat-nonliteral \
	-Winit-self \
	-Wignored-qualifiers \
	-Wwrite-strings \
	-Wconversion \
	-Werror \
	-Wno-error=conversion

    # -Wcast-align \
    # -Wmissing-declarations \
    # -Wpointer-arith \
    # -Warray-bounds -ftree-vrp \
    # -Wstrict-aliasing -fstrict-aliasing \
    # -Wstrict-overflow -fstrict-overflow \
    # -Wunreachable-code \
    # -Winvalid-pch \
	#    -Wundef \


CXXWARN = $(CWARN) -Wsign-promo

	# -Weffc++ 

CFLAGS   = $(CPPFLAGS) $(CWARN) $(CDEBUG) $(DEFS) \
	-fvisibility=hidden

    # -fvisibility=hidden \
    # -ffunction-sections \
    # -fdata-sections

CXXFLAGS = $(CFLAGS) $(CXXWARN) $(CDEBUG) $(DEFS)

LDFLAGS  = -Xlinker --Map=burro.map \
	-Wl,--gc-sections -Wl,--as-needed -Wl,--demangle

################################################################
# TOP-LEVEL SOURCES

SRCS_C   = \

SRCS_CPP = \
	backdrop.cpp \
	console.cpp \
	draw.cpp \
	ecma48_test.cpp \
	eng.cpp \
	lineedit.cpp \
	loop.cpp \
	main.cpp \
	Object.cpp \
	tmx.cpp \
	utf.cpp \
	xcairo.cpp \
	xiso9660.cpp \
	xsdl.cpp \
	Background.cpp \
	Interpreter.cpp \
	ResHandle.cpp \
	ResCache.cpp \
	ResourceFile.cpp 

SRCS_RL = \
	ecma48.rl

SRCS_JS  = \
	main.js

MAPS_TMX = \
	map1.tmx

TILESHEETS_PNG = \
	cave.png

SPRITES_ASE = \
	man.ase face.ase

CONSOLE_FONTS = \
	tools/font/8x13.bdf
################################################################
# ALL

.PHONY:  all
all:     burro burro.iso bdf-convert

################################################################
# GENERATED SOURCES

ecma48.cpp : ecma48.rl
	ragel -o $@ $<

%.h : %.bdf
	./bdf-convert $< $@

console.cpp : 8x13.h

sprites.png sprites.jsn: $(SPRITES_ASE)
	 aseprite --batch --sheet sprites.png --data sprites.jsn $(SPRITES_ASE)

################################################################
# OBJECTS

8x13.h: bdf-convert

OBJS     = \
	$(SRCS_C:.c=.o) \
	$(SRCS_CPP:.cpp=.o) \
	ecma48.o

DEPENDS = ${OBJS:.o=.d}
-include ${DEPENDS}

%.o : %.c
	$(CC) $(CFLAGS) -c $*.c -o $*.o
	$(CC) $(CPPFLAGS) -M $*.c > $*.d

%.o : %.cpp
	$(CXX) $(CXXFLAGS) -c $*.cpp -o $*.o
	$(CXX) $(CPPFLAGS) -M  $*.cpp > $*.d

burro.iso: $(SRCS_JS) $(MAPS_TMX) $(TILESHEETS_PNG) sprites.png sprites.jsn
	genisoimage -o $@ $^


################################################################
# EXECUTABLES

bdf-convert: bdf-convert.c
	$(CC) $(CWARN) -Wno-error=conversion -o $@ bdf-convert.c

LIBS     = \
	`sdl2-config --libs` \
	`pkg-config cairo --libs` \
	`pkg-config tinyxml --libs` \
	`pkg-config libiso9660++ --libs` \
	`pkg-config mozjs-24 --libs `\
	-L$(srcdir)/tmxparser/build -ltmxparser

burro:   $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

################################################################
# MINOR TARGETS

.PHONY:  clean
clean:
	-rm -f $(OBJS) $(DEPENDS) bdf-convert burro burro.iso core

.PHONY: check-syntax
check-syntax:
	$(CXX) $(CXXFLAGS) -fsyntax-only $(SRCS_CPP) ecma48.cpp

TAGS:
	etags $(SRCS_C) $(SRCS_CPP)

